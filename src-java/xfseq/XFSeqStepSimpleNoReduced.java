package xfseq;

import clojure.lang.IFn;
import clojure.lang.ISeq;
import xfseq.buffer.IXFSeqBuffer;

/**
 * Mixed object candidate for a structurally non-reducing transducer.
 *
 * <p>The no-reduced precondition is enforced by the test/benchmark adapter;
 * this class intentionally has no public xf-seq dispatch path.</p>
 */
public class XFSeqStepSimpleNoReduced extends XFSeqObjectStep {

    public XFSeqStepSimpleNoReduced(IXFSeqBuffer buf, IFn xf, ISeq s) {
        super(buf, xf, s, Shape.MIXED, false);
    }
}
