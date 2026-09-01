(ns xfseq.core
  (:refer-clojure :exclude [map filter remove take])
  (:require
    [xfseq.analyze :as ana]
    [xfseq.protocols :as p])
  (:import [xfseq LongChunkedCons LongArrayChunk DoubleChunkedCons DoubleArrayChunk]))

(set! *warn-on-reflection* true)

(defn long-chunk [^longs arr ^long off ^long len]
  (let [chunk-length (min len (+ off 32))]
    (LongChunkedCons. (LongArrayChunk. arr off chunk-length)
      (when (< chunk-length len)
        (lazy-seq
          (long-chunk arr chunk-length len))))))

^{:clj-kondo/ignore [:unresolved-protocol-method]}
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

^{:clj-kondo/ignore [:unresolved-protocol-method]}
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

(defn xf-seq
  [xf coll]
  (clojure.lang.LazySeq. (xfseq.ObjectXFSeqInit. xf coll)))

;;;;;;;;;;;;;;;;
;; Transducers
;;

;; Keep these public operations tied to the corresponding Clojure transducer
;; definitions.  The analyzer and generator remain available for the later
;; primitive experiment, but must not be part of the #1 object path.

(defn map
  ([f]
   (clojure.core/map f))
  ([f coll]
   (xf-seq (map f) coll)))

(defn filter
  ([pred]
   (clojure.core/filter pred))
  ([pred coll]
   (xf-seq (filter pred) coll)))

(defn remove
  ([pred]
   (clojure.core/remove pred))
  ([pred coll]
   (xf-seq (remove pred) coll)))

(defn take
  ([n]
   (clojure.core/take n))
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

(def long-inc (fn ^long [^long l] (clojure.lang.Numbers/add l (long 1))))

(def long-even? (fn [^long l] (zero? (clojure.lang.Numbers/and l 1))))

(def double-add (fn ^double [^double a ^double b] (clojure.lang.Numbers/add a b)))

(def double-inc (fn ^double [^double l] (clojure.lang.Numbers/add l 1.0)))

(def double-even? (fn [^double l] (long-even? (long (Math/round l)))))
