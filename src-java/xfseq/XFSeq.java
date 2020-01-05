package xfseq;

import clojure.lang.*;

/**

 Based on the Clojure code:

 (def ^:static ^:const chunked-seq-class clojure.lang.IChunkedSeq)

 (defn ^:private ^:static xf-seq-step
   [^clojure.lang.ISeq s ^clojure.lang.IFn xf ^clojure.lang.XFSeqDynamicBuffer2 buf]
   (if (identical? s nil)
     (do
       (xf (.scope buf))
       (.toSeq buf nil))
     (let [s (if (.isInstance ^Class chunked-seq-class s)
       (let [ch (chunk-first ^clojure.lang.IChunkedSeq s)]
         (if (identical? buf (.reduce ch xf (.scope buf (.count ch))))
           (chunk-rest ^clojure.lang.IChunkedSeq s)
           ()))
         (if (identical? buf (xf buf (.first s)))
           (.more s)
           ()))]
       (.toSeq buf
         (lazy-seq
           (xf-seq-step (.seq ^clojure.lang.ISeq s) xf buf))))))

 (def ^:static xf-seq-arr-conj!
   (fn
     ([] (clojure.lang.XFSeqDynamicBuffer2.))
     ([buf] buf)
     ([buf x]
       (.conj ^clojure.lang.XFSeqDynamicBuffer2 buf x))))

 (def ^:static xf-seq
   (fn xf-seq [xform coll]
     (lazy-seq
       (let [s (seq coll)]
         (if s
           (xf-seq-step s (xform xf-seq-arr-conj!) (clojure.lang.XFSeqDynamicBuffer2.)))))))

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
                s = new XFSeqStep(xf, (ISeq)s).invoke();
            }
            return s;
        }
    }

    public static ISeq create(IFn xform, Object coll) {
        return new LazySeq(new InitXFSeq(xform, coll));
    }

    /**
     * OLD API: Evolution of XFSeq using separate dynamic buffer object.
     */
    private static class InitLazySeq extends AFn {
        private final IFn xf;
        private final Object coll;

        InitLazySeq(IFn xf, Object coll) {
            this.xf = xf;
            this.coll = coll;
        }

        @Override
        public Object invoke() {
            Object s = RT.seq(coll);
            if (s != null) {
                XFSeqDynamicBuffer2 buf = new XFSeqDynamicBuffer2();
                IFn xform = (IFn)xf.invoke(buf);
                s = new NextStep(xform, buf, (ISeq)s).invoke();
            }
            return s;
        }
    }

    public static class NextStep extends AFn {

        private final IFn xf;
        private final XFSeqDynamicBuffer2 buf;
        private ISeq s;

        NextStep(IFn xf, XFSeqDynamicBuffer2 buf, ISeq s) {
            this.xf = xf;
            this.buf = buf;
            this.s = s;
        }

        private ISeq invokeChunked(IChunkedSeq cs) {
            IChunk ch = cs.chunkedFirst();
            buf.scope(ch.count());
            if (buf == ch.reduce(xf, buf)) {
                return cs.chunkedMore();
            } else {
                return PersistentList.EMPTY;
            }
        }

        public Object invoke() {
            ISeq c = s.seq();
            if (c == null) {
                buf.scope();
                xf.invoke(buf);
                return buf.toSeq();
            } else {
                if (c instanceof IChunkedSeq) {
                    c = invokeChunked((IChunkedSeq) c);
                } else {
                    if (buf == xf.invoke(buf, c.first())) {
                        c = c.more();
                    } else {
                        c = PersistentList.EMPTY;
                    }
                }
                s = c;
                return buf.toSeq(new LazySeq(this));
            }
        }
    }

    /**
     * OLD API w/ LazySeq
     */
    public static ISeq createStackable(IFn xform, Object coll) {
        return new LazySeq(new ConsumableInternals(xform, coll, null));
    }

    private static class ConsumableInternals extends AFn /* implements Consumable */ {

        private static final Keyword STACKABLE = Keyword.intern(null, "stackable");
        private static final Keyword CONSUMED_SEQ = Keyword.intern("clojure.lang.LazySeq$ConsumableInternals", "CONSUMED_SEQ");
        private static final Var STRICT_CONSUMABLE_SEQS = RT.var("clojure.lang.LazySeq","*strict-consumable-seqs*").setDynamic();

        static {
            STRICT_CONSUMABLE_SEQS.doReset(true);
        }

        private static boolean isStrictlyConsumable(){
            return (boolean) STRICT_CONSUMABLE_SEQS.get();
        }

        private IFn xf;
        private Object coll;
        private Object ls;

        ConsumableInternals(IFn xf, Object coll, Object ls) {
            this.xf = xf;
            this.coll = coll;
            this.ls = ls;
        }

        void ensureNotConsumed(){
            if (coll == CONSUMED_SEQ) {
                if (isStrictlyConsumable()) {
                    throw new RuntimeException("LazySeq's internals were destroyed when used as a Consumable");
                } else {
                    System.err.println("WARN: Consumed seq is being reused. Will re-run transformations.");
                    new Exception().printStackTrace();
                }
            }
        }

        void setConsumed() {
            xf = null;
            coll = CONSUMED_SEQ;
            if (isStrictlyConsumable()) {
                ls = null;
            }
        }

        public Object invoke() {
            ensureNotConsumed();
            Object ret;
            if (ls != null) {
                ret = ls;
            } else {
                ret = XFSeq.create(xf, coll);
            }
            // After it's been invoked, the LazySeq will set this "fn" to null.
            // No need to clear any fields..(?)
            // setConsumed();
            return ret;
        }

        /*
        @Override
        public IReduceInit consumable(IFn xform) {
            ensureNotConsumed();
            IReduceInit consumable = clojure.lang.RT.stackConsunables(xform, xf, coll);
            setConsumed();
            return consumable;
        }
        */

        /*
        @Override
        public ISeq stack(IFn xform) {
            // A seq is stackable when the caller doesn't pass in
            // a custom LazySeq.
            boolean stackable;
            if (ls == null) {
                stackable = true;
            } else {
                IPersistentMap meta = ((IMeta) ls).meta();
                stackable = meta != null && meta.containsKey(STACKABLE) && Boolean.TRUE.equals(meta.entryAt(STACKABLE).val());
            }

            if (stackable) {
                ensureNotConsumed();
                ISeq s = clojure.lang.RT.stackSeqs(xform, xf, coll);
                setConsumed();
                return s;
            } else {
                return null;
            }
        }
        */
    }
}
