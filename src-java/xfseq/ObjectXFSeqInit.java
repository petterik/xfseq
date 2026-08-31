package xfseq;

import clojure.lang.AFn;
import clojure.lang.IFn;
import clojure.lang.ISeq;
import clojure.lang.RT;
import xfseq.buffer.ObjectBuffer;

/** Deferred initialization for the object-only xf-seq engine. */
public final class ObjectXFSeqInit extends AFn {

    private final IFn xform;
    private final Object coll;

    public ObjectXFSeqInit(IFn xform, Object coll) {
        this.xform = xform;
        this.coll = coll;
    }

    @Override
    public Object invoke() {
        // Source realization intentionally precedes transducer application.
        // Keeping both operations in this thunk makes construction entirely
        // lazy and preserves the observable exception order.
        ISeq source = RT.seq(coll);
        ObjectBuffer buffer = new ObjectBuffer();
        IFn reducingFn = (IFn) xform.invoke(buffer);

        if (source == null) {
            reducingFn.invoke(buffer);
            return buffer.isEmpty() ? null : buffer.toTail();
        }

        return new XFSeqStepSimple(buffer, reducingFn, buffer, source).invoke();
    }
}
