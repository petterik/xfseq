package xfseq;

import clojure.lang.IFn;
import clojure.lang.ISeq;
import xfseq.buffer.IXFSeqBuffer;

/** Chunked object candidate for a structurally non-reducing transducer. */
public class XFSeqStepChunkedOnlyNoReduced extends XFSeqObjectStep {

    public XFSeqStepChunkedOnlyNoReduced(IXFSeqBuffer buf, IFn xf, ISeq s) {
        super(buf, xf, s, Shape.CHUNKED, false);
    }
}
