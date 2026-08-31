package xfseq;

import clojure.lang.IFn;
import clojure.lang.ISeq;
import xfseq.buffer.IXFSeqBuffer;

/** Repaired chunked object candidate with explicit Reduced handling. */
public class XFSeqStepChunkedOnly extends XFSeqObjectStep {

    public XFSeqStepChunkedOnly(IXFSeqBuffer buf, IFn xf, ISeq s) {
        super(buf, xf, s, Shape.CHUNKED, true);
    }
}
