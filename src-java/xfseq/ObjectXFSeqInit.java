package xfseq;

import clojure.lang.AFn;
import clojure.lang.IFn;
import clojure.lang.ISeq;
import clojure.lang.Numbers;
import clojure.lang.RT;
import clojure.lang.Util;
import xfseq.buffer.ObjectBuffer;

/** Deferred initialization for the object-only xf-seq engine. */
public final class ObjectXFSeqInit extends AFn {

    private IFn xform;
    private Object coll;
    private final UnaryProfile profile;
    private final Object takeCount;
    private boolean failed;

    public ObjectXFSeqInit(IFn xform, Object coll) {
        this(xform, coll, UnaryProfile.GENERIC, null);
    }

    public ObjectXFSeqInit(IFn xform, Object coll, UnaryProfile profile) {
        this(xform, coll, profile, null);
    }

    public ObjectXFSeqInit(IFn xform, Object coll, UnaryProfile profile,
                           Object takeCount) {
        this.xform = xform;
        this.coll = coll;
        this.profile = profile;
        this.takeCount = takeCount;
    }

    @Override
    public Object invoke() {
        if (failed) {
            return null;
        }

        // Keep invalid-count failures retryable. Direct take's one-shot
        // closure retains its count/source state when the deferred pos? check
        // itself throws, so every force must report the same class.
        if (profile == UnaryProfile.TAKE && !Numbers.isPos(takeCount)) {
            return null;
        }

        XFSeqStepSimple step = null;
        try {
            // Source realization intentionally precedes transducer
            // application. Keeping both operations in this thunk makes
            // construction entirely lazy and preserves the observable
            // exception order.
            ISeq source = RT.seq(coll);
            ObjectBuffer buffer = new ObjectBuffer();
            IFn reducingFn = (IFn) xform.invoke(buffer);

            if (source == null) {
                reducingFn.invoke(buffer);
                return buffer.isEmpty() ? null : buffer.toTail();
            }

            step = new XFSeqStepSimple(buffer, reducingFn, buffer, source,
                                       profile);
            return step.invoke();
        } catch (Throwable error) {
            if (profile != UnaryProfile.GENERIC) {
                // Direct take's one-shot closure clears its captured count
                // after a successful step even when obtaining the final rest
                // throws.  Preserve that single retry's exception shape while
                // still dropping all source/xform state here.
                boolean takePostStepReplay = profile == UnaryProfile.TAKE
                        && step == null
                        && xform == null
                        && coll == null;
                if (!takePostStepReplay
                        && (profile != UnaryProfile.TAKE
                            || step == null
                            || step.failed())) {
                    failed = true;
                }
                xform = null;
                coll = null;
            }
            throw Util.sneakyThrow(error);
        }
    }
}
