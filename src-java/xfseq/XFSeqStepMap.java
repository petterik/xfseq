package xfseq;

import clojure.lang.AFn;
import clojure.lang.IChunk;
import clojure.lang.IChunkedSeq;
import clojure.lang.IFn;
import clojure.lang.ISeq;
import clojure.lang.LazySeq;
import clojure.lang.RT;
import clojure.lang.Reduced;
import xfseq.buffer.ObjectBuffer;

/** Map-only object step with the ordinary transformed reducing function. */
final class XFSeqStepMap extends AFn {

    private ObjectBuffer buf;
    private IFn xf;
    private Object accumulator;
    private ISeq s;
    private boolean completed;
    private boolean failed;

    XFSeqStepMap(ObjectBuffer buf, IFn xf, Object accumulator, ISeq s) {
        this.buf = buf;
        this.xf = xf;
        this.accumulator = accumulator;
        this.s = s;
    }

    @Override
    public Object invoke() {
        if (completed || failed) {
            return null;
        }

        try {
            return invokeStep();
        } catch (Throwable error) {
            // Direct unary map's lazy node is one-shot after any failure.
            failed = true;
            xf = null;
            s = null;
            buf = null;
            accumulator = null;
            throw clojure.lang.Util.sneakyThrow(error);
        }
    }

    private Object invokeStep() {
        ObjectBuffer buf = this.buf;
        IFn xf = this.xf;
        Object acc = this.accumulator;
        ISeq c = this.s == null ? null : this.s.seq();

        if (c == null) {
            return finish();
        }

        if (c instanceof IChunkedSeq) {
            IChunkedSeq chunked = (IChunkedSeq) c;
            IChunk chunk = chunked.chunkedFirst();
            int n = chunk.count();
            for (int i = 0; i < n; i++) {
                Object next = xf.invoke(acc, chunk.nth(i));
                if (RT.isReduced(next)) {
                    this.accumulator = ((Reduced) next).deref();
                    this.s = null;
                    return finish();
                }
                acc = next;
                this.accumulator = acc;
            }

            this.accumulator = acc;
            this.s = chunked.chunkedMore();
            return buf.toChunkSeq(new LazySeq(this));
        }

        ISeq item = c;
        Object next = xf.invoke(acc, item.first());
        // Map's direct lazy implementation obtains the source tail after the
        // mapper succeeds and before exposing the returned Cons node.
        ISeq nextSource = item.more();
        if (RT.isReduced(next)) {
            this.accumulator = ((Reduced) next).deref();
            this.s = null;
            return finish();
        }
        this.accumulator = next;
        this.s = nextSource;
        return buf.toSeq(new LazySeq(this));
    }

    private Object finish() {
        if (completed) {
            return null;
        }

        ObjectBuffer buf = this.buf;
        IFn xf = this.xf;
        Object completedAccumulator = xf.invoke(this.accumulator);
        this.accumulator = completedAccumulator;

        ISeq result = buf.isEmpty() ? null : buf.toTail();
        completed = true;
        this.s = null;
        this.xf = null;
        this.buf = null;
        this.accumulator = null;
        return result;
    }
}
