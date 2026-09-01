package xfseq;

import clojure.lang.IFn;
import clojure.lang.ISeq;
import xfseq.buffer.IXFSeqBuffer;

/** Repaired dechunked object candidate with explicit Reduced handling. */
public class XFSeqStepSingleOnly extends XFSeqObjectStep {

    public XFSeqStepSingleOnly(IXFSeqBuffer buf, IFn xf, ISeq s) {
        super(buf, xf, s, Shape.DECHUNKED, true);
    }
}
