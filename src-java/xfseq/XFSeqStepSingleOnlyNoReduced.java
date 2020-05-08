package xfseq;

import clojure.lang.AFn;
import clojure.lang.IFn;
import clojure.lang.ISeq;
import clojure.lang.LazySeq;
import xfseq.buffer.IXFSeqBuffer;

public class XFSeqStepSingleOnlyNoReduced extends AFn {

    private final IXFSeqBuffer buf;
    private final IFn xf;
    private ISeq s;

    public XFSeqStepSingleOnlyNoReduced(IXFSeqBuffer buf, IFn xf, ISeq s) {
        this.buf = buf;
        this.xf = xf;
        this.s = s;
    }

    public Object invoke() {
        IXFSeqBuffer buf = this.buf;

        for (ISeq c = this.s.seq(); c != null; c = c.seq()) {
            xf.invoke(buf, c.first());
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
