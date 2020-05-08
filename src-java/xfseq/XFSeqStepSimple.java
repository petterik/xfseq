package xfseq;

import clojure.lang.*;
import xfseq.buffer.IXFSeqBuffer;

public class XFSeqStepSimple extends AFn {

    private final IXFSeqBuffer buf;
    private final IFn xf;
    private ISeq s;

    public XFSeqStepSimple(IXFSeqBuffer buf, IFn xf, ISeq s) {
        this.buf = buf;
        this.xf = xf;
        this.s = s;
    }

    public Object invoke() {
        IXFSeqBuffer buf = this.buf;
        outer:
        for (ISeq c = this.s.seq(); c != null; c = c.seq()) {
            if (c instanceof IChunkedSeq) {
                IChunk ch = ((IChunkedSeq) c).chunkedFirst();
                for(int i = 0; i < ch.count(); i++) {
                    if(buf != xf.invoke(buf, ch.nth(i))) {
                        break outer;
                    }
                }
                c = ((IChunkedSeq) c).chunkedMore();
            } else {
                if (buf != xf.invoke(buf, c.first())) {
                    break;
                }
                c = c.more();
            }

            if (!buf.isEmpty()) {
                this.s = c;
                return buf.toSeq(new LazySeq(this));
            }
        }

        xf.invoke(buf);
        return buf.isEmpty() ? null : buf.toTail();
    }
}
