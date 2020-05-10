(ns xfseq.core
  (:refer-clojure :exclude [map filter remove take])
  (:require
    [clojure.core :as clj.core]
    [clojure.set :as set]
    [clojure.walk :as walk]
    [xfseq.analyze :as ana]
    [xfseq.protocols :as p])
  (:import [xfseq ILongSeq IDoubleSeq LongChunkedCons LongArrayChunk DoubleChunkedCons DoubleArrayChunk]
           [clojure.lang Numbers IFn]
           [xfseq.buffer LongBuffer DoubleBuffer ObjectBuffer]))

(set! *warn-on-reflection* true)

(defn long-chunk [^longs arr ^long off ^long len]
  (let [chunk-length (min len (+ off 32))]
    (LongChunkedCons. (LongArrayChunk. arr off chunk-length)
      (when (< chunk-length len)
        (lazy-seq
          (long-chunk arr chunk-length len))))))

(extend-protocol p/ILongSeqable
  (class (long-array 0))
  (long-seq [arr]
    (let [arr (longs arr)
          len (count arr)]
      (when (pos? len)
        (long-chunk arr 0 len)))))

(defn double-chunk [^doubles arr ^long off ^long len]
  (let [chunk-length (min len (+ off 32))]
    (DoubleChunkedCons. (DoubleArrayChunk. arr off chunk-length)
      (when (< chunk-length len)
        (lazy-seq
          (double-chunk arr chunk-length len))))))

(extend-protocol p/IDoubleSeqable
  (class (double-array 0))
  (double-seq [arr]
    (let [arr (doubles arr)
          len (count arr)]
      (when (pos? len)
        (double-chunk arr 0 len)))))


;;;;;;;;;;;;;;;;;;;
;; XFSeq creation
;;

(defonce ^:private deconstructed (Object.))

;; TODO: Can we generate classes with:
;;       * type hinted arguments
;;       * Generated names, like XFSeqStep_LLOL
;; TODO: Can we get the generated class if it already exists?
;;       - No, let's just generate them all and assume they exist.
;;       - It'll be 3^4 = 81 classes (3 values, 4 inputs)
;;       - We may want to be smarter if we want to add chunked-or-not seqs.
;;         to skip the chunked check, which makes 4^4 = 256 classes.
;;         - I.e. generate on the fly.
;;       - Hmm, do they really need to be deftypes?
;;       - They really just need to be invokable?
;;         - Yes. They need to be deftypes to get the mutable hinted fields.

(defmacro gen-deftype [name args]
  (prn (clj.core/mapv meta args))
  `(deftype ~name ~args))

(comment
  (gen-deftype GenTest [^:unsynchronized-mutable ^long a])
  )

(def buffer-map {'Object xfseq.buffer.ObjectBuffer
                 'long   xfseq.buffer.LongBuffer
                 'double xfseq.buffer.DoubleBuffer})

(defn class->sym [^Class c]
  (symbol (.getName c)))

(defn gen-xfseq-name [{:keys [arity-2 input-type]}]
  (let [hint->letter (fn [hint]
                       (Character/toUpperCase (.charAt (str hint) 0)))]
    (->> [(nth (:args arity-2) 1) input-type]
      (clj.core/map hint->letter)
      (apply str "XFSeqStep_"))))

(defn- gen-xfseq-step*
  "Generates a class and returns a constructor function."
  [class-name {:keys [arity-2 input-type]}]
  (let [rf-arg-type (nth (:args arity-2) 1)

        buf-type rf-arg-type
        buf-class (get buffer-map buf-type)
        _ (when (nil? buf-class)
            (prn "buf-type: " buf-type))

        _ (prn "classes: "
            (clj.core/map (juxt identity type)
              (into (:args arity-2)
                [(:class arity-2) input-type])))

        seq-class (condp = input-type
                    'long xfseq.ILongSeq
                    'double xfseq.IDoubleSeq
                    clojure.lang.ISeq)

        chunk-type (condp = input-type
                     'long xfseq.ILongChunk
                     'double xfseq.IDoubleChunk
                     clojure.lang.IChunk)

        args [(with-meta (symbol "buf") {:tag     (class->sym buf-class)
                                         :private true})
              (with-meta (symbol "xf") {:tag (class->sym (:class arity-2))})
              (with-meta (symbol "s") {:tag                    (class->sym seq-class)
                                       :unsynchronized-mutable true})]

        xf-invoke-first (let []
                          (if (= 'Object buf-type)
                            `(~'xf ~'buf (.first ~'c))
                            (concat
                              [(if (= rf-arg-type 'Object)
                                 '.invoke
                                 '.invokePrim)
                               'xf
                               'buf]
                              [(cond->> (condp = input-type
                                          'long `(.firstLong ~'c)
                                          'double `(.firstDouble ~'c)
                                          'Object `(.first ~'c))
                                 ;; If the input type doesn't match the
                                 ;; xf input arg type, then cast it.
                                 (not= buf-type input-type)
                                 (list (condp = buf-type
                                         'long 'clojure.lang.RT/longCast
                                         'double 'clojure.lang.RT/doubleCast)))])))

        xf-invoke-chunk (if (= 'Object buf-type)
                          `(~'xf ~'buf (.nth ~'chunk ~'i))
                          (concat
                            [(if (= rf-arg-type 'Object)
                               '.invoke
                               '.invokePrim)
                             'xf
                             'buf]
                            [(cond->> (condp = input-type
                                        'long `(.nthLong ~'chunk ~'i)
                                        'double `(.nthDouble ~'chunk ~'i)
                                        'Object `(.nth ~'chunk ~'i))
                               ;; If the input type doesn't match the
                               ;; xf input arg type, then cast it.
                               (not= buf-type input-type)
                               (list (condp = buf-type
                                       'long 'clojure.lang.RT/longCast
                                       'double 'clojure.lang.RT/doubleCast)))]))


        ;; TODO: generate my own bytecode.
        invoke-body `(loop [~'c (.seq ~'s)]
                       (if (clojure.lang.Util/identical ~'c nil)
                         (do
                           (~'xf ~'buf)
                           (if (.isEmpty ~'buf)
                             nil
                             (.toTail ~'buf)))
                         (let [~(with-meta (symbol "c") {:tag (class->sym clojure.lang.ISeq)})
                               (if (instance? clojure.lang.IChunkedSeq ~'c)
                                 (let [~(with-meta (symbol "c") {:tag (class->sym clojure.lang.IChunkedSeq)}) ~'c
                                       ~(with-meta (symbol "chunk") {:tag (class->sym chunk-type)}) (.chunkedFirst ~'c)
                                       ~'n (.count ~'chunk)]
                                   (loop [~'i 0]
                                     (if (clojure.lang.Numbers/lt ~'i ~'n)
                                       (if (clojure.lang.Util/identical ~'buf ~xf-invoke-chunk)
                                         (recur (clojure.lang.Numbers/unchecked_inc ~'i))
                                         ;; reduced
                                         clojure.lang.PersistentList/EMPTY)
                                       (.chunkedMore ~'c))))
                                 (let [~(with-meta (symbol "c") {:tag (class->sym seq-class)}) ~'c]
                                   (if (clojure.lang.Util/identical ~'buf ~xf-invoke-first)
                                     (.more ~'c)
                                     clojure.lang.PersistentList/EMPTY)))]
                           (if (.isEmpty ~'buf)
                             (recur (.seq ~'c))
                             (do
                               (set! ~'s ~'c)
                               (.toSeq ~'buf (clojure.lang.LazySeq. ~'this)))))))

        body `(deftype ~(symbol class-name) ~args
                IFn
                (~'invoke [~'this]
                  ~invoke-body))]



    [body buf-class]))

(defmacro gen-xfseq-step [args]
  (let [class-name (gen-xfseq-name args)
        [body# buffer-class#] (gen-xfseq-step* class-name args)
        ctor# (symbol (str '-> class-name))]

    `(do
       ~body#

       [~class-name
        (fn [xf# coll#]
          (let [buf# (.newInstance ^Class ~buffer-class#)]
            (~ctor# buf# (xf# buf#) coll#)))])))


(comment
  (-> (gen-xfseq-step* {:arity-2    {:args  ['Object 'long]
                                     :class clojure.lang.IFn$OLO}
                        :input-type 'long})
    (doto clojure.pprint/pprint)
    (eval))

  (gen-xfseq-step {:arity-2    {:args  [Object long]
                                :class clojure.lang.IFn$OLO}
                   :input-type long})

  (macroexpand-1 '(gen-xfseq-step {:arity-2    {:args  [Object long]
                                                :class clojure.lang.IFn$OLO}
                                   :input-type long}))

  ((xfseq.core.XFSeqStep_LLO. ))

  (def test-seq (second (gen-xfseq-step {:arity-2    {:args  [Object long]
                                                      :class clojure.lang.IFn$OLO}
                                         :input-type long})))

  ((test-seq (map long-inc) (p/long-seq (long-array [1 2 3]))))

  (clojure.lang.Reflector/getField xfseq.core.XFSeqStep_LLO "s" false)



  (-> (gen-xfseq-step* {:arity-2    {:args  ['Object 'long]
                                     :class clojure.lang.IFn$OLO}
                        :input-type 'long})
    (nth 2)
    (nth 2)
    (meta))
  )

(def xfseq-classes (atom {}))

(defmacro gen-xfseq-classes []
  (let [types '[long double Object]
        class-ids (for [arity-2-arg types
                        input-type types]
                    [arity-2-arg input-type])]
    `(do
       ~@(clj.core/map
           (fn [[arity-2-arg input-type]]
             (let [klass (if (= arity-2-arg 'Object)
                           clojure.lang.IFn
                           (Class/forName
                             (format "clojure.lang.IFn$O%sO"
                               (Character/toUpperCase
                                 (.charAt (str arity-2-arg) 0)))))]
               `(let [[name# ctor#] (gen-xfseq-step
                                      {:arity-2     {:args  [~'Object ~arity-2-arg]
                                                     :class ~klass}
                                       :input-type  ~input-type})]
                  (swap! xfseq-classes assoc name# ctor#))))
           class-ids))))

(gen-xfseq-classes)

(defn- init-xfseq-step [xf coll xfseq-step-args]
  #_(prn "Using xfseq: " (gen-xfseq-name xfseq-step-args))
  (let [ctor (get @xfseq-classes (gen-xfseq-name xfseq-step-args))]
    (ctor xf coll)))

(comment
  ((init-xfseq-step (map inc) [1 2 3]
     {:arity-2    {:args  '[Object long]
                   :class clojure.lang.IFn$OLO}
      :input-type 'long}))

  ;; Outputs the Java ASM code for creating a class
  (jdk.internal.org.objectweb.asm.util.ASMifier/main
    (into-array String ["/Users/petter/Github/petterik/xfseq/classes/xfseq/core/XFSeqStep_DD.class"]))

  (do
    (jdk.internal.org.objectweb.asm.util.ASMifier/main
      (into-array String ["/Users/petter/Github/petterik/xfseq/classes/production/xfseq/xfseq/XFSeqStepSimpleLong.class"]))
    (Thread/sleep 100))

  (do
    (jdk.internal.org.objectweb.asm.util.ASMifier/main
      (into-array String ["/Users/petter/Github/petterik/xfseq/classes/production/xfseq/xfseq/XFSeqStepSimpleLongLong.class"]))
    (Thread/sleep 100))

  (import '[xfseq.gen MyOwn])
  (let [buf (ObjectBuffer.)]
    (clojure.lang.LazySeq. (MyOwn. buf ((map inc) buf) (seq [1 2 3]))))

  (=
    (let [v (into [] (range 1e6))]
      (System/gc)
      (dotimes [_ 10]
        (time
          (let [buf (ObjectBuffer.)]
            (doall
              (clojure.lang.LazySeq. (MyOwn. buf ((map identity) buf) (seq v))))
            nil)))
      (let [buf (ObjectBuffer.)]
        (clojure.lang.LazySeq. (MyOwn. buf ((map identity) buf) (seq v)))))

    (let [v (into [] (range 1e6))]
      (System/gc)
      (dotimes [_ 10]
        (time
          (do
            (doall
              (clojure.lang.LazySeq. (init-xfseq-step (map identity) (seq v)
                                       {:arity-2    {:args  '[Object Object]
                                                     :class clojure.lang.IFn}
                                        :input-type 'Object})))
            nil)))
      (clojure.lang.LazySeq. (init-xfseq-step (map identity) (seq v)
                               {:arity-2    {:args  '[Object Object]
                                             :class clojure.lang.IFn}
                                :input-type 'Object})))

    (let [v (into [] (range 1e6))]
      (System/gc)
      (dotimes [_ 10]
        (time
          (do
            (doall
              (clj.core/map identity v))
            nil)))
      (clj.core/map identity v)))

  ;; What we can do is write the optimal Java code, compile to a class, and ASMifier it.
  ;; Take the parts that we want from it and dynamically add it to the class.

  ;; The generated classes can also be deduped.
  )



(deftype InitXFSeq [xf coll]
  clojure.lang.IFn
  (invoke [this]
    (when-some [s (condp satisfies? coll
                    p/ILongSeqable (p/long-seq coll)
                    p/IDoubleSeqable (p/double-seq coll)
                    (seq coll))]
      (let [buf (case (::return-hint (meta xf))
                  long (LongBuffer.)
                  double (DoubleBuffer.)
                  Object (ObjectBuffer.)
                  ;; If there's no return hint, use a buffer
                  ;; of the same type as the incoming seq.
                  (condp instance? s
                    ILongSeq (LongBuffer.)
                    IDoubleSeq (DoubleBuffer.)
                    (ObjectBuffer.)))

            rf (xf buf)

            input-type (condp instance? s
                         ILongSeq 'long
                         IDoubleSeq 'double
                         'Object)

            ana (ana/analyze-primitive-interfaces (ana/interfaces (class rf)))

            step (init-xfseq-step xf s
                   {:arity-2    (get ana 2 {:args  '[Object Object]
                                            :class clojure.lang.IFn})
                    :input-type input-type})]
        (step)))))

(declare ensure-valid)

(deftype XFSeqHead [^:unsynchronized-mutable xf
                    ^:unsynchronized-mutable coll]
  clojure.lang.Sequential
  clojure.lang.Seqable
  (seq [this]
    (locking this
      (ensure-valid coll "Unable to create a seq from a deconstructed XFSeq")

      (when (some? xf)
        (set! coll (clojure.lang.LazySeq. (InitXFSeq. xf coll)))
        (set! xf nil))

      (seq coll)))

  clojure.lang.IPending
  (isRealized [this]
    (locking this
      (ensure-valid coll "XFSeqHead already deconstructed")
      (nil? xf)))

  p/IDeconstruct
  (deconstruct! [this]
    (locking this
      (ensure-valid coll "Unable to deconstruct XFSeq more than once")
      (when (some? xf)
        (let [ret [xf coll]]
          (set! xf nil)
          (set! coll deconstructed)
          ret)))))

(defn- ensure-valid [coll msg]
  (when (= coll deconstructed)
    (throw (ex-info msg {}))))

(defmethod print-method XFSeqHead
  [value writer]
  (print-method (seq value) writer))

(defn xf-seq
  [xf coll]
  (XFSeqHead. xf coll))

;;;;;;;;;;;;;;;;
;; Transducers
;;

(def map:xf-factory
  (ana/xf-factory*
    (ana/map:type-analyzer
      '(fn [rf f]
         (fn
           ([] (rf))
           ([acc] (rf acc))
           ([acc item]
            (rf acc (f item))))))))

(defn map
  ([f]
   ^{::return-hint (-> (class f)
                     (ana/interfaces)
                     (ana/analyze-primitive-interfaces)
                     (get-in [1 :return] 'Object))}
   (fn
     [rf]
     (map:xf-factory rf f)))
  ([f coll]
   (xf-seq (map f) coll)))

(def filter:xf-factory
  (ana/xf-factory
    '(fn [rf pred]
       (fn
         ([] (rf))
         ([acc] (rf acc))
         ([acc item]
          (if (pred item)
            (rf acc item)
            acc))))))

(defn filter
  ([pred]
   (fn
     [rf]
     (filter:xf-factory rf pred)))
  ([pred coll]
   (xf-seq (filter pred) coll)))

(def remove:xf-factory
  (ana/xf-factory
    '(fn [rf pred]
       (fn
         ([] (rf))
         ([acc] (rf acc))
         ([acc item]
          (if-not (pred item)
            (rf acc item)
            acc))))))

(defn remove
  ([pred]
   (fn
     [rf]
     (remove:xf-factory rf pred)))
  ([pred coll]
   (xf-seq (remove pred) coll)))

(def take:xf-factory
  (ana/xf-factory
    '(fn [rf n]
       (let [nv (volatile! n)]
         (fn
           ([] (rf))
           ([result] (rf result))
           ([result input]
            (let [n @nv
                  nn (vswap! nv dec)
                  result (if (pos? n)
                           (rf result input)
                           result)]
              (if (not (pos? nn))
                (ensure-reduced result)
                result))))))))

(defn take
  ([n]
   (fn [rf]
     (take:xf-factory rf n)))
  ([n coll]
   (xf-seq (take n) coll)))

;;;;;;;;;;;;;;;;
;; Consume API
;;

(defn consume
  "Consumes the XFSeq, deconstructing it to call reduce on the
   original collection.

   This allows for code to be written as:
     (->> coll (map inc) ... (consume + 0))
   that would be executed as:
     (transduce (comp (map inc) ...) + 0 coll).

   By only replacing reduce with consume at the end.

   noun: consumable; a commodity that is intended to be used up relatively quickly."
  [rf init coll]
  (if-some [[xf coll] (when (satisfies? p/IDeconstruct coll) (p/deconstruct! coll))]
    (recur (xf rf) init coll)
    ;; TODO: Needs primitive reduce?
    (let [ana (ana/analyze-primitive-interfaces (ana/interfaces (class rf)))
          ret (reduce rf init coll)]
      ;; Call 1 arity if available.
      (cond-> ret
        (some? (get ana 1))
        (rf)))))

(defn drain
  "Returns a draining version of the collection which skips intermediate structures
   when possible, rendering those intermediates unusable.

   This allows for code like:
    (let [a (map inc coll)]
      (drained (map dec a)))
   To never construct intermediate a. It also makes code using a after having
   iterated through the drained throw an exception:
     (let [a (map inc coll)]
       (count (drained (map dec a)))
       (prn a) ;; throws
     )

   However, using `a` before it's drained is valid but will not yield the
   performance improvements as it'll not be deconstructable."
  [coll]
  (loop [rf nil coll coll]
    (if-some [[xf coll] (when (satisfies? p/IDeconstruct coll) (p/deconstruct! coll))]
      (recur (if (some? rf) (xf rf) xf) coll)
      (if (some? rf)
        (xf-seq rf coll)
        coll))))


;;;;;;;;;;
;; Utils
;;

(def long-add (fn ^long [^long a ^long b] (clojure.lang.Numbers/add a b)))

(def long-inc (fn ^long [^long l] (clojure.lang.Numbers/add l 1)))

(def long-even? (fn [^long l] (zero? (clojure.lang.Numbers/and l 1))))


(comment

  (xf-seq (map (fn ^double [^double d] (clojure.lang.Numbers/add d 1.0))) #{3 2 1})

  ;; Currently returns incorrectly:
  ;; (2 3 nil)
  (map (fn ^long [^long i] (Numbers/add i (long 1))) (range (long 1e5)))

  (time (map inc [1 2 3]))
  (time (clj.core/map inc [1 2 3]))
  (time (bases (class long-inc)))
  (time (interfaces (class long-inc)))
  (let [x (interfaces (class long-inc))]
    (time (analyze-primitive-interfaces x)))

  (->> (repeat (long 1e5) 1)
    (map long-inc)
    (filter long-even?)
    (dorun)
    (time))

  (let [map clojure.core/map]
    (->> (repeat (long 1e5) 1)
      (transduce
        (comp
          (map long-inc)
          (map long-inc)
          (map long-inc)
          (map long-inc)
          (map long-inc))
        (completing
          (fn ^long [^long acc ^long i]
           (clojure.lang.Numbers/add acc i)))
        0)
      (time)))

  (let [filter clj.core/filter]
    (->> (range (long 1e5))
      (filter long-even?)
      (filter long-even?)
      (filter long-even?)
      (filter long-even?)
      (filter long-even?)
      (filter long-even?)
      (dorun)
     (time)))

  (time (dorun (take 1e6 (map long-inc (remove long-even? (map long-inc (range (long 1e6))))))))
  (let [take clojure.core/take
        map clojure.core/map
        remove clojure.core/remove]
    (time (dorun (take 1e6 (map long-inc (remove long-even? (map long-inc (range (long 1e6)))))))))

  (let [map clj.core/map]
    (let [arr (long-array (range (long 1e6)))]
     (->> arr
       (map long-inc)
       (map long-inc)
       (map long-inc)
       (map long-inc)
       (dorun)
       (time))))

  (let [map clj.core/map]
    (let [arr (long-array (range (long 1e6)))]
      (->> arr
        (map long-inc)
        (map long-inc)
        (map long-inc)
        (map long-inc)
        (drain)
        (dorun)
        (time))))

  (let [a (map inc (range 10))]
    (prn a)
    (doall (drain (map inc a)))
    )

  (reduce + 0 (map inc (range 10)))
  (let [map clj.core/map]
    (let [arr (long-array (repeat (long 1e6) 1))]
      (System/gc)
      (time (dorun (map long-inc arr)))))


  ;; Fails:
  ;; Would need to enumerate all combinations of LongStep<X>,
  ;; where X = double, long, Object. Should maybe consider code-gen.
  (map inc (p/long-seq (long-array (repeat 100 1))))

  ;; Fails:
  ;; "count not supported on this type: XFSeqHead"
  (count (map inc [1 2 3]))

  ;; TODO: Implement more transducers
  ;;       Implement primitive reduce
  ;;       Generate some of the Java code (Look at XFSeqStep$<x>Step).

  (let [map clj.core/map]
    (let [arr (long-array (repeat (long 1e6) 1))]
      (time (reduce long-add 0 (map long-inc arr)))))
  ;; clojure.core/map: "Elapsed time: 111.124331 msecs"
  ;; xfseq.core/map:   "Elapsed time: 12.930992 msecs"

  (let [arr (seq (into #{} (range 0 10)))]
    (time (reduce long-add 0 (map long-inc arr))))

  ;; Note: This can get even faster with primitive invocations
  ;; of the reducing function.



  (def ->long-long-xfseq (gen-xfseq-step {:arity-2    {:args  ['Object 'long]
                                                       :class clojure.lang.IFn$OLO}
                                          :input-type 'long}))



  (->long-long-xfseq (map long-inc) (p/long-seq (long-array (repeat (long 1e6) 1))))

  )