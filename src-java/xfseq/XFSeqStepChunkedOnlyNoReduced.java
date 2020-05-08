package xfseq;

import clojure.lang.*;
import xfseq.buffer.IXFSeqBuffer;

public class XFSeqStepChunkedOnlyNoReduced extends AFn {

    private final IXFSeqBuffer buf;
    private final IFn xf;
    private ISeq s;

    public XFSeqStepChunkedOnlyNoReduced(IXFSeqBuffer buf, IFn xf, ISeq s) {
        this.buf = buf;
        this.xf = xf;
        this.s = s;
    }

    public Object invoke() {
        IXFSeqBuffer buf = this.buf;

        for (ISeq c = this.s.seq(); c != null; c = c.seq()) {
            IChunkedSeq cs = (IChunkedSeq)c;
            IChunk ch = cs.chunkedFirst();
            for(int i = 0; i < ch.count(); i++) {
                xf.invoke(buf, ch.nth(i));
            }
            c = cs.chunkedMore();
            if (!buf.isEmpty()) {
                this.s = c;
                return buf.toSeq(new LazySeq(this));
            }
        }

        xf.invoke(buf);
        return buf.isEmpty() ? null : buf.toTail();
    }
}
