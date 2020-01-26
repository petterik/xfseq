(ns xfseq.core
  (:refer-clojure :exclude [map filter remove take])
  (:require
    [clojure.core :as clj.core]
    [clojure.set :as set]
    [clojure.walk :as walk])
  (:import [xfseq ILongSeq XFSeqStep$LongStep IDoubleSeq XFSeqStep$DoubleStep XFSeqStep$ObjectStep LongArrayCons DoubleArrayCons]
           [clojure.lang Numbers]
           [xfseq.buffer LongBuffer DoubleBuffer ObjectBuffer]))

(defprotocol IDeconstruct
  (deconstruct! [this]
    "Safely returns its parts if they haven't been used
     already, rendering the object unusable.

     Returns nil if they're already been used.

     This is really only meant to be used with xfseq.core/consume."))

(defprotocol ILongSeqable
  (long-seq [this]))

(defprotocol IDoubleSeqable
  (double-seq [this]))

(extend-protocol ILongSeqable
  (class (long-array 0))
  (long-seq [arr]
    (let [arr (longs arr)
          len (count arr)]
      (when (pos? len)
        (LongArrayCons. arr 0 len nil)))))

(extend-protocol IDoubleSeqable
  (class (double-array 0))
  (double-seq [arr]
    (let [arr (doubles arr)
          len (count arr)]
      (when (pos? len)
       (DoubleArrayCons. arr 0 len nil)))))

;;;;;;;;;;;;;;;;;;;
;; XFSeq creation
;;

(defonce ^:private deconstructed (Object.))

(deftype InitXFSeq [xf coll]
  clojure.lang.IFn
  (invoke [this]
    (when-some [s (condp satisfies? coll
                    ILongSeqable (long-seq coll)
                    IDoubleSeqable (double-seq coll)
                    (seq coll))]
      (let [buf (case (::return-hint (meta xf))
                  long (LongBuffer.)
                  double (DoubleBuffer.)
                  ;; If there's no return hint, use a buffer
                  ;; of the same type as the incoming seq.
                  (condp instance? s
                    ILongSeq (LongBuffer.)
                    IDoubleSeq (DoubleBuffer.)
                    (ObjectBuffer.)))

            rf (xf buf)

            step (condp instance? s
                   ILongSeq (XFSeqStep$LongStep. rf s buf)
                   IDoubleSeq (XFSeqStep$DoubleStep. rf s buf)
                   (XFSeqStep$ObjectStep. rf s buf))]
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

  IDeconstruct
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

;;;;;;;;;;;;;;;;;;;
;; Type analyzing
;;

(defn interfaces [^Class c]
  (when c
    (seq (.getInterfaces c))))

(defn analyze-primitive-interfaces*
  [interfaces]
  (into {}
    (comp
      (clj.core/filter
        (fn [^Class interface]
          (.startsWith (.getCanonicalName interface) "clojure.lang.IFn.")))
      (clj.core/map
        (fn [^Class klass]
          (let [canonical-name (.getCanonicalName klass)
                interface-name (subs canonical-name (inc (.lastIndexOf canonical-name ".")))
                letter->type-hint (fn [letter]
                                    (condp = letter
                                      \L 'long
                                      \D 'double
                                      \O 'Object
                                      'Object))]
            {:arity  (dec (count interface-name))
             :return (letter->type-hint (last interface-name))
             :class  klass
             :args   (mapv letter->type-hint (butlast interface-name))})))
      (clj.core/map (juxt :arity identity)))
    interfaces))

(def analyze-primitive-interfaces
  "Given a set of interfaces, datafies the primitive IFn interfaces
  and returns them in a map by arity."
  (memoize analyze-primitive-interfaces*))

(def type-hint->letter {'double "D"
                        'long   "L"
                        'Object "O"})

(defn hint-map->interface
  "Takes a datafied description of an interface and returns
  the interface name matching the description as a symbol."
  [{:keys [return args]}]
  (if (every? #{'Object} (cons return args))
    (symbol "clojure.lang.IFn")
    (symbol
      (format "clojure.lang.IFn$%s"
        (apply str
          (concat
            (clj.core/map type-hint->letter args)
            [(type-hint->letter return)]))))))

(defn is-primitive? [interface-name]
  (not= "clojure.lang.IFn" (name interface-name)))

(defn- f-replacement [ana-f]
  {'f (get ana-f 1 {:arity  1
                    :args   '[Object]
                    :return 'Object})})

(defn- rf-replacement [ana-rf]
  {'rf (get ana-rf 2 {:arity  2
                      :args   '[Object Object]
                      :return 'Object})})

(defn apply-type-hints
  "Given datafied rf and an xf-body, return the body with
  applied type hints from the datafication as well as
  primitive function invocations where possible.

  Takes a replacement-map with {sym type-analyze} which is
  used to replace function invocations with primitive
  invocations.

  This replacement is only applied to the xf-body with arity-2."
  ([ana-rf xf-body replacement-map]
   (let [apply-hints (fn [[arg-list & body]]
                       (let [arg-count (count arg-list)
                             typed-args (into []
                                          (map-indexed
                                            (fn [idx sym]
                                              (vary-meta sym assoc :tag (get-in ana-rf [arg-count :args idx]))))
                                          arg-list)]
                         (cons
                           (vary-meta typed-args assoc :tag (get-in ana-rf [arg-count :return]))
                           (cond->> body
                             (== 2 arg-count)
                             (walk/postwalk
                               (fn [x]
                                 (if (or (list? x) (seq? x))
                                   (if-some [[sym replacement] (find replacement-map (first x))]
                                     (let [interface-name (hint-map->interface replacement)]
                                       (assert (== (:arity replacement) (dec (count x))))
                                       (list*
                                         (if (is-primitive? interface-name)
                                           '.invokePrim
                                           '.invoke)
                                         (vary-meta sym assoc :tag interface-name)
                                         (rest x)))
                                     x)
                                   x)))))))]
     (clj.core/map
       (fn [inner]
         (walk/postwalk
           (fn [x]
             (if (and (coll? x) (= 'fn (first x)))
               (cons (first x)
                 (clj.core/map apply-hints (rest x)))
               x))
           inner))
       xf-body))))

(defn rf-type-analyzer
  "Analyzer for xf-bodies that only needs to type hint the rf."
  [xf-body]
  (fn [rf-bases]
    (let [ana-rf (analyze-primitive-interfaces rf-bases)]
      (apply-type-hints
        ana-rf
        xf-body
        (rf-replacement ana-rf)))))

(defn rf+f-type-analyzer
  "Analyzer for xf-bodies that type hints both rf and f."
  [xf-body f-sym]
  (fn [rf-bases f-bases]
    (let [ana-rf (analyze-primitive-interfaces rf-bases)]
      (apply-type-hints
        ana-rf
        xf-body
        (merge
          (rf-replacement ana-rf)
          (set/rename-keys
            (f-replacement (analyze-primitive-interfaces f-bases))
            {'f f-sym}))))))

(defn- xf-factory*
  [analyzer]
  (let [analyze+eval (memoize (comp eval analyzer))]
    ;; These args are the same number of arguments as the ones in
    ;; the xf-body
    (fn [& args]
      (let [new-xf (apply analyze+eval
                     (clj.core/map (comp interfaces class) args))]
        (apply new-xf args)))))

(defn xf-factory
  "Takes a body of an xf and returns a function that takes a
  reducing function and returns a transducer.

  The analyzing and evaluation is memoized based on the interfaces
  of the rf."
  [xf-body]
  (let [args (-> xf-body second)
        _ (assert (vector? args))
        analyzer (case (count args)
                   1 (rf-type-analyzer xf-body)
                   2 (rf+f-type-analyzer xf-body (second args)))]
    (xf-factory* analyzer)))

;;;;;;;;;;;;;;;;
;; map helpers
;;

(defn map:type-analyzer
  "Updates the applied reducing function hints with the types
  of the mapping function."
  [xf-body]
  (fn [rf-bases f-bases]
    (let [ana-f (analyze-primitive-interfaces f-bases)
          ana-rf (analyze-primitive-interfaces rf-bases)
          replacements (-> (merge
                             (f-replacement ana-f)
                             (rf-replacement ana-rf))
                         ;; setting the type of the rf's second argument to the
                         ;; type of f's return value.
                         (assoc-in ['rf :args 1] (get-in ana-f [1 :return] 'Object)))]
      (apply-type-hints
        ;; map's arity-2 arg1 should have the same type as the f's arg0.
        (assoc-in ana-rf [2 :args 1] (get-in ana-f [1 :args 0]))
        xf-body
        replacements))))

;;;;;;;;;;;;;;;;
;; Transducers
;;

(def map:xf-factory
  (xf-factory*
    (map:type-analyzer
      '(fn [rf f]
         (fn
           ([] (rf))
           ([acc] (rf acc))
           ([acc item]
            (rf acc (f item))))))))

(defn map
  ([f]
   ^{::return-hint (-> (class f)
                     (interfaces)
                     (analyze-primitive-interfaces)
                     (get-in [1 :return]))}
   (fn
     [rf]
     (map:xf-factory rf f)))
  ([f coll]
   (xf-seq (map f) coll)))

(def filter:xf-factory
  (xf-factory
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
  (xf-factory
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
  (xf-factory
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
  (if-some [[xf coll] (when (satisfies? IDeconstruct coll) (deconstruct! coll))]
    (recur (xf rf) init coll)
    ;; TODO: Needs primitive reduce?
    ;;       I.e. calling the reducing function with the .invokePrim
    ;;       methods and not invoke, as it'll box the numbers.
    (reduce rf init coll)))

;;;;;;;;;;
;; Utils
;;

(def long-add (fn ^long [^long l] (clojure.lang.Numbers/add l 1)))

(def long-even? (fn [^long l] (zero? (clojure.lang.Numbers/and l 1))))


(comment

  ;; Currently returns incorrectly:
  ;; (2 3 nil)
  (map (fn ^long [^long i] (Numbers/add i (long 1))) (range (long 1e5)))

  (time (map inc [1 2 3]))
  (time (clj.core/map inc [1 2 3]))
  (time (bases (class long-add)))
  (time (interfaces (class long-add)))
  (let [x (interfaces (class long-add))]
    (time (analyze-primitive-interfaces x)))

  (->> (repeat (long 1e5) 1)
    (map long-add)
    (filter long-even?)
    (dorun)
    (time))

  (let [map clojure.core/map]
    (->> (repeat (long 1e5) 1)
      (transduce
        (comp
          (map long-add)
          (map long-add)
          (map long-add)
          (map long-add)
          (map long-add))
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

  (time (dorun (take 1e6 (map long-add (remove long-even? (map long-add (range (long 1e6))))))))
  (let [take clojure.core/take
        map clojure.core/map
        remove clojure.core/remove]
    (time (dorun (take 1e6 (map long-add (remove long-even? (map long-add (range (long 1e6)))))))))

  (let [map clj.core/map]
    (let [arr (long-array (range (long 1e6)))]
     (->> arr
       (map long-add)
       (map long-add)
       (map long-add)
       (map long-add)
       (dorun)
       (time))))
  )