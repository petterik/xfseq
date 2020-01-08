package xfseq;

import clojure.lang.*;
import xfseq.buffer.DoubleBuffer;
import xfseq.buffer.IXFSeqBuffer;
import xfseq.buffer.LongBuffer;
import xfseq.buffer.ObjectBuffer;

/**

 Based on the Clojure code:

 (def ^:static ^:const chunked-seq-class clojure.lang.IChunkedSeq)

 (defn ^:private ^:static xf-seq-step
   [^clojure.lang.ISeq _s ^clojure.lang.IFn xf ^clojure.lang.XFSeqDynamicBuffer2 buf]
   (if (identical? _s nil)
     (do
       (xf (.scope buf))
       (.toSeq buf nil))
     (let [_s (if (.isInstance ^Class chunked-seq-class _s)
       (let [ch (chunk-first ^clojure.lang.IChunkedSeq _s)]
         (if (identical? buf (.reduce ch xf (.scope buf (.count ch))))
           (chunk-rest ^clojure.lang.IChunkedSeq _s)
           ()))
         (if (identical? buf (xf buf (.first _s)))
           (._more _s)
           ()))]
       (.toSeq buf
         (lazy-seq
           (xf-seq-step (.seq ^clojure.lang.ISeq _s) xf buf))))))

 (def ^:static xf-seq-arr-conj!
   (fn
     ([] (clojure.lang.XFSeqDynamicBuffer2.))
     ([buf] buf)
     ([buf x]
       (.conj ^clojure.lang.XFSeqDynamicBuffer2 buf x))))

 (def ^:static xf-seq
   (fn xf-seq [xform coll]
     (lazy-seq
       (let [_s (seq coll)]
         (if _s
           (xf-seq-step _s (xform xf-seq-arr-conj!) (clojure.lang.XFSeqDynamicBuffer2.)))))))

 */
public class XFSeq {

    private static Symbol longHint = Symbol.intern(null, "long");
    private static Symbol doubleHint = Symbol.intern(null, "double");

    private static class InitXFSeq extends AFn {
        private final IFn xf;
        private final Object coll;

        InitXFSeq(IFn xf, Object coll) {
            this.xf = xf;
            this.coll = coll;
        }

        @Override
        public Object invoke() {
            Object s = RT.seq(coll);
            if (s != null) {
                // TODO: Must know what the type is which is passed to the rf.
                //       Before invoking with buf.
                //       Can the transducers return the type of the object? (arity-0?)
                Object typeHint = xf.invoke();

                IXFSeqBuffer buf;
                if (longHint.equals(typeHint)) {
                    buf = new LongBuffer();
                } else if (doubleHint.equals(typeHint)) {
                    buf = new DoubleBuffer();
                } else {
                    buf = new ObjectBuffer();
                }
                IFn xform = (IFn)xf.invoke(buf);

                // After the first call to .seq, we know if the seq is a LongSeq, DoubleSeq or just a Seq.
                XFSeqStep step;
                if (s instanceof ILongSeq) {
                    step = new XFSeqStep.LongStep(xform, (ISeq)s, buf);
                } else if (s instanceof IDoubleSeq) {
                    step = new XFSeqStep.DoubleStep(xform, (ISeq)s, buf);
                } else {
                    step = new XFSeqStep.ObjectStep(xform, (ISeq)s, buf);
                }

                s = step.invoke();
            }
            return s;
        }
    }

    public static Object create(IFn xform, Object coll) {
        return new XFSeqHead(xform, coll);
    }

    private static class XFSeqHead implements Seqable, Sequential, IReduceInit, IPending {

        private IFn xform;
        private Object coll;

        public XFSeqHead(IFn xform, Object coll) {
            this.xform = xform;
            this.coll = coll;
        }

        @Override
        public synchronized boolean isRealized() {
            return xform == null;
        }

        @Override
        public synchronized Object reduce(IFn iFn, Object o) {
            IFn xf = (IFn)xform.invoke(iFn);
            if (o instanceof XFSeqHead) {
                return ((XFSeqHead)o).reduce(xf, o);
            } else {
                // Namespace.find("clojure.core").findInternedVar(Symbol.intern("reduce")).deref()
                // TODO: Call clojure.core/reduce (?)
                // TODO: OR hook in to coll-reduce?
                return null;
            }
        }

        @Override
        public synchronized ISeq seq() {
            if (xform != null) {
                coll = new LazySeq(new InitXFSeq(xform, coll));
                xform = null;
            }

            return ((ISeq)coll).seq();
        }
    }

}
