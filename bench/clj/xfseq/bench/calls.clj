(ns xfseq.bench.calls
  "AOT benchmark callers.

  This namespace is benchmark-only.  Its AOT-generated function classes are
  the narrow Java boundary used by the JMH harness; all calls to the library
  and to the candidate adapter are resolved while this namespace is compiled
  with direct-linking enabled."
  (:require [clojure.core :as core]
            [xfseq.core :as xf]
            [xfseq.phase-2-candidates :as candidates]))

(set! *warn-on-reflection* true)

(def ^:private inc-fn
  (fn ^long [^long value]
    (inc value)))

(def ^:private even-fn
  (fn [value]
    (even? value)))

(defn -source
  [^String kind size]
  (let [values (range size)]
    (case kind
      "list" (apply list values)
      "vector" (vec values)
      "subvector" (subvec (vec (range (+ size 4))) 2 (+ size 2))
      "range" (range size)
      "set" (set values)
      "array" (object-array values)
      "iterable" (java.util.ArrayList. ^java.util.Collection (vec values))
      (throw (IllegalArgumentException.
               (str "Unknown Phase 2 benchmark source: " kind))))))

(defn -xform
  [^String workload]
  (case workload
    "identity" identity
    ;; Use ordinary Clojure transducers for all public comparison rows.  The
    ;; analyzer-specialized xfseq/map reducing function requires IFn.OLO and
    ;; cannot be consumed by clojure.core/sequence's TransformerIterator;
    ;; keeping this xform generic makes every public implementation a valid,
    ;; symmetric comparison while xfseq remains the implementation under test.
    "map" (core/map inc-fn)
    "filter" (core/filter even-fn)
    "map-filter" (comp (core/map inc-fn) (core/filter even-fn))
    "five-map" (comp (core/map inc-fn)
                      (core/map inc-fn)
                      (core/map inc-fn)
                      (core/map inc-fn)
                      (core/map inc-fn))
    "take" (core/take 32)
    (throw (IllegalArgumentException.
             (str "Unknown Phase 2 benchmark workload: " workload)))))

(defn -nonReducingOperation
  [^String workload]
  (candidates/non-reducing-operation
    (case workload
      "identity" :identity
      "map" :map
      "filter" :filter
      (throw (IllegalArgumentException.
               (str "Workload is not a structurally non-reducing operation: "
                    workload))))))

(defn -publicXfSeq
  [xform source]
  (xf/xf-seq xform source))

(defn -publicSequence
  [xform source]
  (core/sequence xform source))

(defn -publicEduction
  [xform source]
  (eduction xform source))

(defn -publicTransduce
  [xform source]
  (transduce xform conj [] source))

(defn -candidate
  [^String stable-id xform source]
  (candidates/instantiate-candidate stable-id xform source))

(defn -firstValue
  [value]
  (first value))

(defn- numeric-value
  [value]
  (if (number? value)
    (long value)
    (long (hash value))))

(defn -checksum
  [value]
  (reduce (fn ^long [^long total item]
            (+ total (numeric-value item)))
          0
          value))

(defn -prefixChecksum
  [value n]
  (loop [s (seq value)
         i 0
         total 0]
    (if (or (nil? s) (= i n))
      (long total)
      (recur (next s)
             (inc i)
             (long (+ total (numeric-value (first s))))))))

(defn -vectorValue
  [value]
  (vec value))

(defn -reduceChecksum
  [value]
  (transduce identity
             (fn
               ([total] total)
               ([^long total item]
                (+ total (numeric-value item))))
             0
             value))
