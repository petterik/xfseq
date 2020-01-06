package xfseq;

import clojure.lang.*;

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
                // After the first call to .seq, we know if the seq is a LongSeq, DoubleSeq or just a Seq.
                s = new AXFSeqStep.ObjectSeq(xf, (ISeq)s).invoke();
            }
            return s;
        }
    }

    public static ISeq create(IFn xform, Object coll) {
        return new LazySeq(new InitXFSeq(xform, coll));
    }

}
