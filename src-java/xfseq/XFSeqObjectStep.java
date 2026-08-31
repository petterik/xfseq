package xfseq;

import clojure.lang.AFn;
import clojure.lang.IChunk;
import clojure.lang.IChunkedSeq;
import clojure.lang.IFn;
import clojure.lang.ISeq;
import clojure.lang.LazySeq;
import clojure.lang.RT;
import clojure.lang.Reduced;
import xfseq.buffer.IXFSeqBuffer;

/**
 * Shared state machine for the retained object loop candidates.
 *
 * <p>The candidate classes deliberately retain their historical names and
 * constructors, but all of them use this implementation for state,
 * completion, and output ownership.  The shape only controls which input
 * loop is selected; it is never inferred from an arbitrary caller-supplied
 * tag.</p>
 */
class XFSeqObjectStep extends AFn {

    enum Shape {
        MIXED,
        DECHUNKED,
        CHUNKED
    }

    private static final int READY = 0;
    private static final int SINGLE_PENDING = 1;
    private static final int CHUNK_PROCESSING = 2;
    private static final int CHUNK_TAIL_PENDING = 3;

    private IXFSeqBuffer buf;
    private IFn xf;
    private Object accumulator;
    private ISeq s;
    private final Shape shape;
    private final boolean reducedAware;
    private boolean completed;

    /* A successful input batch keeps its tail pending until its output is
       demanded.  This avoids probing a lazy tail merely to return a prefix. */
    private int pending = READY;
    private ISeq pendingSingle;
    private IChunkedSeq pendingChunkSource;
    private IChunk pendingChunk;
    private int pendingChunkIndex;

    XFSeqObjectStep(IXFSeqBuffer buf, IFn xf, ISeq s,
                    Shape shape, boolean reducedAware) {
        this.buf = buf;
        this.xf = xf;
        this.accumulator = buf;
        this.s = s;
        this.shape = shape;
        this.reducedAware = reducedAware;
    }

    @Override
    public Object invoke() {
        if (completed) {
            return null;
        }

        IXFSeqBuffer buf = this.buf;
        IFn xf = this.xf;
        Object acc = this.accumulator;
        // A pending batch owns the current source node until its output is
        // consumed.  Do not call seq() on that node again before resolving
        // its tail; besides duplicate effects, that would move a tail
        // exception to the wrong realization boundary.
        ISeq c = pending == READY && this.s != null ? this.s.seq() : null;

        while (true) {
            if (pending == SINGLE_PENDING) {
                ISeq nextSource = pendingSingle.more();
                pendingSingle = null;
                pending = READY;
                this.s = nextSource;
                if (!buf.isEmpty()) {
                    return buf.toSeq(new LazySeq(this));
                }
                c = nextSource == null ? null : nextSource.seq();
                continue;
            }

            if (pending == CHUNK_TAIL_PENDING) {
                ISeq nextSource = pendingChunkSource.chunkedMore();
                pendingChunkSource = null;
                pendingChunk = null;
                pendingChunkIndex = 0;
                pending = READY;
                this.s = nextSource;
                if (!buf.isEmpty()) {
                    return buf.toSeq(new LazySeq(this));
                }
                c = nextSource == null ? null : nextSource.seq();
                continue;
            }

            if (pending == CHUNK_PROCESSING) {
                IChunk chunk = pendingChunk;
                int n = chunk.count();
                for (int i = pendingChunkIndex; i < n; i++) {
                    // Preserve the failed input position for LazySeq's
                    // retry-on-exception behavior.
                    pendingChunkIndex = i;
                    Object next = xf.invoke(acc, chunk.nth(i));
                    if (reducedAware && RT.isReduced(next)) {
                        this.accumulator = ((Reduced) next).deref();
                        clearPending();
                        // A completion exception may leave LazySeq's thunk
                        // retryable.  Do not retry the input that already
                        // returned Reduced; retry completion with its
                        // unwrapped accumulator instead.
                        this.s = null;
                        return finish();
                    }
                    acc = next;
                    this.accumulator = acc;
                    pendingChunkIndex = i + 1;
                }

                // Resolve chunkedMore only when the continuation is forced,
                // unless this batch produced no output and more input is
                // needed immediately.
                pending = CHUNK_TAIL_PENDING;
                if (!buf.isEmpty()) {
                    return buf.toSeq(new LazySeq(this));
                }
                c = resolveChunkTail();
                continue;
            }

            if (c == null) {
                return finish();
            }

            if (c instanceof IChunkedSeq) {
                if (shape == Shape.DECHUNKED) {
                    throw invalidShape("dechunked", c);
                }
                IChunkedSeq chunked = (IChunkedSeq) c;
                IChunk chunk = chunked.chunkedFirst();
                pendingChunkSource = chunked;
                pendingChunk = chunk;
                pendingChunkIndex = 0;
                pending = CHUNK_PROCESSING;
                continue;
            }

            if (shape == Shape.CHUNKED) {
                throw invalidShape("chunked", c);
            }

            ISeq item = c;
            Object next = xf.invoke(acc, item.first());
            if (reducedAware && RT.isReduced(next)) {
                this.accumulator = ((Reduced) next).deref();
                clearPending();
                // See the chunked path above: completion retry must not
                // process the terminal input a second time.
                this.s = null;
                return finish();
            }
            acc = next;
            this.accumulator = acc;

            // Keep the source tail pending while this batch is exposed.  A
            // tail exception is consequently observed by the continuation,
            // and retrying that continuation does not repeat the step.
            pendingSingle = item;
            pending = SINGLE_PENDING;
            if (!buf.isEmpty()) {
                return buf.toSeq(new LazySeq(this));
            }
            c = resolveSingleTail();
        }
    }

    private ISeq resolveSingleTail() {
        ISeq nextSource = pendingSingle.more();
        pendingSingle = null;
        pending = READY;
        this.s = nextSource;
        return nextSource == null ? null : nextSource.seq();
    }

    private ISeq resolveChunkTail() {
        ISeq nextSource = pendingChunkSource.chunkedMore();
        pendingChunkSource = null;
        pendingChunk = null;
        pendingChunkIndex = 0;
        pending = READY;
        this.s = nextSource;
        return nextSource == null ? null : nextSource.seq();
    }

    private IllegalArgumentException invalidShape(String expected, ISeq actual) {
        return new IllegalArgumentException(
                "Expected " + expected + " object source, got "
                        + actual.getClass().getName());
    }

    private void clearPending() {
        pending = READY;
        pendingSingle = null;
        pendingChunkSource = null;
        pendingChunk = null;
        pendingChunkIndex = 0;
    }

    /** Complete once, preserving the accumulator returned by the transducer. */
    private Object finish() {
        if (completed) {
            return null;
        }

        IXFSeqBuffer buf = this.buf;
        IFn xf = this.xf;
        Object completedAccumulator = xf.invoke(this.accumulator);
        this.accumulator = completedAccumulator;

        ISeq result = buf.isEmpty() ? null : buf.toTail();

        // Leave all state intact if completion or flushing throws.  LazySeq
        // retries a failed thunk, and successful forcing is already cached by
        // LazySeq itself.
        completed = true;
        this.s = null;
        this.xf = null;
        this.buf = null;
        this.accumulator = null;
        clearPending();
        return result;
    }
}
