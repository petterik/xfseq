package xfseq;

import clojure.lang.IFn;
import clojure.lang.ISeq;
import xfseq.buffer.IXFSeqBuffer;

/** Dechunked object candidate for a structurally non-reducing transducer. */
public class XFSeqStepSingleOnlyNoReduced extends XFSeqObjectStep {

    public XFSeqStepSingleOnlyNoReduced(IXFSeqBuffer buf, IFn xf, ISeq s) {
        super(buf, xf, s, Shape.DECHUNKED, false);
    }
}
