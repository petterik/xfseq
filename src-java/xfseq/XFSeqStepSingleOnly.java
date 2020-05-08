package xfseq;

import clojure.lang.*;
import xfseq.buffer.IXFSeqBuffer;

public class XFSeqStepSingleOnly extends AFn {

    private final IXFSeqBuffer buf;
    private final IFn xf;
    private ISeq s;

    public XFSeqStepSingleOnly(IXFSeqBuffer buf, IFn xf, ISeq s) {
        this.buf = buf;
        this.xf = xf;
        this.s = s;
    }

    public Object invoke() {
        IXFSeqBuffer buf = this.buf;

        for (ISeq c = this.s.seq(); c != null; c = c.seq()) {
            if (buf != xf.invoke(buf, c.first())) {
                break;
            }
            c = c.more();

            if (!buf.isEmpty()) {
                this.s = c;
                return buf.toSeq(new LazySeq(this));
            }
        }

        xf.invoke(buf);
        return buf.isEmpty() ? null : buf.toTail();
    }
}
