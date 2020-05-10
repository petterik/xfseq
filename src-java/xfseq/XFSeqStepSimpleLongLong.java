package xfseq;

import clojure.lang.*;
import xfseq.buffer.IXFSeqBuffer;

public class XFSeqStepSimpleLongLong extends AFn {

    private final IXFSeqBuffer buf;
    private final OLO xf;
    private ISeq s;

    public XFSeqStepSimpleLongLong(IXFSeqBuffer buf, OLO xf, ISeq s) {
        this.buf = buf;
        this.xf = xf;
        this.s = s;
    }

    public Object invoke() {
        IXFSeqBuffer buf = this.buf;
        outer:
        for (ISeq c = this.s.seq(); c != null; c = c.seq()) {
            if (c instanceof IChunkedSeq) {
                ILongChunk ch = (ILongChunk)((IChunkedSeq) c).chunkedFirst();
                for(int i = 0; i < ch.count(); i++) {
                    if(buf != xf.invokePrim(buf, ch.nthLong(i))) {
                        break outer;
                    }
                }
                c = ((IChunkedSeq) c).chunkedMore();
            } else {
                if (buf != xf.invokePrim(buf, ((ILongSeq)c).firstLong())) {
                    break;
                }
                c = c.more();
            }

            if (!buf.isEmpty()) {
                this.s = c;
                return buf.toSeq(new LazySeq(this));
            }
        }

        ((IFn)xf).invoke(buf);
        return buf.isEmpty() ? null : buf.toTail();
    }
}
