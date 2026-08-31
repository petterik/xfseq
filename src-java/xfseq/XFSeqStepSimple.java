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
 * The mixed object sequence step used by the object-only xf-seq path.
 *
 * <p>The surrounding {@link LazySeq} supplies the once-only realization
 * boundary. This object only carries the transducer state between output
 * batches; it does not eagerly inspect the source or create a second lazy
 * realization mechanism.</p>
 */
public class XFSeqStepSimple extends AFn {

    private IXFSeqBuffer buf;
    private IFn xf;
    private Object accumulator;
    private ISeq s;
    private boolean completed;
    private static final int READY = 0;
    private static final int CHUNK_PENDING = 1;
    private static final int SINGLE_PENDING = 2;
    private int pending = READY;
    private IChunkedSeq pendingChunkSource;
    private IChunk pendingChunk;
    private int pendingChunkIndex;
    private ISeq pendingSingle;

    /**
     * Retain the historical constructor shape for direct candidate probes.
     * The buffer is the initial accumulator for an ordinary transducer.
     */
    public XFSeqStepSimple(IXFSeqBuffer buf, IFn xf, ISeq s) {
        this(buf, xf, buf, s);
    }

    XFSeqStepSimple(IXFSeqBuffer buf, IFn xf, Object accumulator, ISeq s) {
        this.buf = buf;
        this.xf = xf;
        this.accumulator = accumulator;
        this.s = s;
    }

    @Override
    public Object invoke() {
        if (completed) {
            return null;
        }

        IXFSeqBuffer buf = this.buf;
        IFn xf = this.xf;
        Object acc = this.accumulator;
        ISeq c = this.s == null ? null : this.s.seq();

        while (true) {
            if (pending == SINGLE_PENDING) {
                // A successful step whose source tail failed must not be
                // applied twice when LazySeq retries this node.
                ISeq nextSource = pendingSingle.more();
                pending = READY;
                pendingSingle = null;
                this.s = nextSource;
                if (!buf.isEmpty()) {
                    return buf.toSeq(new LazySeq(this));
                }
                c = nextSource == null ? null : nextSource.seq();
                continue;
            }

            if (c == null) {
                return finish();
            }

            if (pending != CHUNK_PENDING && c instanceof IChunkedSeq) {
                pendingChunkSource = (IChunkedSeq) c;
                pendingChunk = pendingChunkSource.chunkedFirst();
                pendingChunkIndex = 0;
                pending = CHUNK_PENDING;
            }

            if (pending == CHUNK_PENDING) {
                IChunk ch = pendingChunk;
                int n = ch.count();
                for (int i = pendingChunkIndex; i < n; i++) {
                    // Save the next input position before invoking user code;
                    // a thrown step is retried at that input, not at the
                    // beginning of an already partially buffered chunk.
                    pendingChunkIndex = i;
                    Object next = xf.invoke(acc, ch.nth(i));
                    if (RT.isReduced(next)) {
                        this.accumulator = ((Reduced) next).deref();
                        pending = READY;
                        pendingChunkSource = null;
                        pendingChunk = null;
                        // If completion throws, LazySeq retries its thunk.
                        // Keep that retry on completion rather than
                        // reprocessing the input that already returned
                        // Reduced.
                        this.s = null;
                        return finish();
                    }
                    acc = next;
                    this.accumulator = acc;
                    pendingChunkIndex = i + 1;
                }

                // Do not probe the next node until this batch's output is
                // consumed. In particular, completion is deferred for an
                // output-bearing final chunk.
                ISeq nextSource = pendingChunkSource.chunkedMore();
                pending = READY;
                pendingChunkSource = null;
                pendingChunk = null;
                pendingChunkIndex = 0;
                this.accumulator = acc;
                this.s = nextSource;
                if (!buf.isEmpty()) {
                    return buf.toSeq(new LazySeq(this));
                }
                c = nextSource == null ? null : nextSource.seq();
            } else {
                ISeq item = c;
                Object next = xf.invoke(acc, item.first());
                if (RT.isReduced(next)) {
                    this.accumulator = ((Reduced) next).deref();
                    // See the chunked path above: a completion retry must
                    // not process the terminal input a second time.
                    this.s = null;
                    return finish();
                }
                acc = next;
                this.accumulator = acc;

                // Advancing the source is part of successfully processing the
                // item. Keep it in the state before returning a continuation
                // so a later realization resumes at the following item.
                pendingSingle = item;
                pending = SINGLE_PENDING;
                ISeq nextSource = item.more();
                pendingSingle = null;
                pending = READY;
                this.s = nextSource;
                if (!buf.isEmpty()) {
                    return buf.toSeq(new LazySeq(this));
                }
                c = nextSource == null ? null : nextSource.seq();
            }
        }

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

        // A successful LazySeq realization will never invoke this step again,
        // but clear terminal references as a defensive state invariant. Do
        // this only after completion succeeds so LazySeq's retry-on-exception
        // behavior remains intact.
        completed = true;
        this.s = null;
        this.xf = null;
        this.buf = null;
        this.accumulator = null;
        return result;
    }
}
