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

(defn- focused-number
  [value]
  (if (number? value)
    (long value)
    (long (hash value))))

(defn- focused-heavy
  [value]
  (let [value (mod (focused-number value) 1000)]
    (long (+ (* value value) (* 31 value) 7))))

(def ^:private focused-arithmetic
  (fn [value]
    (inc (focused-number value))))

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
      "iterator" (iterator-seq
                    (.iterator ^java.lang.Iterable
                               (java.util.ArrayList.
                                 ^java.util.Collection (vec values))))
      "lazy-list" (lazy-seq (apply list values))
      "map-entries" (seq (into {} (map (fn [value] [value value]) values)))
      "sorted-map-entries"
      (seq (into (sorted-map)
                 (map (fn [value] [value value]) values)))
      "repeat" (repeat 0)
      "iterate" (iterate inc 0)
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
  (core/transduce xform
                  (fn
                    ([] [])
                    ([acc] acc)
                    ([acc item] (core/conj acc item)))
                  []
                  source))

;; Phase 3 direct unary call sites.  Keep one wrapper per core/candidate
;; function so AOT disassembly can prove the exact static call at the timed
;; boundary; the Java harness supplies the function/source objects only.
(defn -phase3Function
  [^String operation]
  (case operation
    "map" inc-fn
    "filter" even-fn
    "remove" even-fn
    "take" nil
    (throw (IllegalArgumentException.
             (str "Unknown Phase 3 unary operation: " operation)))))

(defn -phase3Xform
  [^String operation]
  (case operation
    "map" (core/map inc-fn)
    "filter" (core/filter even-fn)
    "remove" (core/remove even-fn)
    "take" (core/take 32)
    (throw (IllegalArgumentException.
             (str "Unknown Phase 3 unary operation: " operation)))))

;; Focused Phase 3 controls retain the four-key primary manifest contract but
;; expose the workload/source vocabulary required for later decision cells.
;; These values are benchmark-only and do not alter the public API.
(defn -focusedFunction
  [^String operation ^String workload size]
  (case operation
    "map" (case workload
            "identity" identity
            "arithmetic" focused-arithmetic
            "heavy" focused-heavy
            (throw (IllegalArgumentException.
                     (str "Unknown focused map workload: " workload))))
    "filter" (case workload
               "selectivity-0" (constantly false)
               "selectivity-1" (fn [value]
                                  (= 0 (mod (focused-number value)
                                            (max 1 (long size)))))
               "selectivity-50" (fn [value]
                                   (even? (focused-number value)))
               "selectivity-99" (fn [value]
                                   (not= (dec (long size))
                                         (focused-number value)))
               "selectivity-100" (constantly true)
               (throw (IllegalArgumentException.
                        (str "Unknown focused filter workload: " workload))))
    "remove" (case workload
               ;; Selectivity names describe output percentage.  `remove`
               ;; emits values for which its predicate is false, so its
               ;; predicate truth percentage is the complement.
               "selectivity-0" (constantly true)
               "selectivity-1" (fn [value]
                                  (not= (dec (long size))
                                        (focused-number value)))
               "selectivity-50" (fn [value]
                                   (even? (focused-number value)))
               "selectivity-99" (fn [value]
                                   (= (dec (long size))
                                      (focused-number value)))
               "selectivity-100" (constantly false)
               (throw (IllegalArgumentException.
                        (str "Unknown focused remove workload: " workload))))
    "take" nil
    (throw (IllegalArgumentException.
             (str "Unknown focused unary operation: " operation)))))

(defn -focusedXform
  [^String operation ^String workload size take-count]
  (let [function (-focusedFunction operation workload size)]
    (case operation
      "map" (core/map function)
      "filter" (core/filter function)
      "remove" (core/remove function)
      "take" (core/take take-count)
      (throw (IllegalArgumentException.
               (str "Unknown focused unary operation: " operation))))))

(defn -focusedNonReducingXform
  [^String operation ^String workload size]
  (let [function (-focusedFunction operation workload size)]
    (case operation
      "map" (candidates/non-reducing-operation :map function)
      "filter" (candidates/non-reducing-operation :filter function)
      ;; remove is filter with the complement predicate, matching core's
      ;; transducer while retaining the adapter's non-reducing proof token.
      "remove" (candidates/non-reducing-operation
                  :filter (complement function))
      (throw (IllegalArgumentException.
               (str "No focused non-reducing form for: " operation))))))

(defn -coreMap
  [f source]
  (core/map f source))

(defn -coreFilter
  [pred source]
  (core/filter pred source))

(defn -coreRemove
  [pred source]
  (core/remove pred source))

(defn -coreTake
  [n source]
  (core/take n source))

(defn -candidateMap
  [f source]
  (xf/map f source))

(defn -candidateFilter
  [pred source]
  (xf/filter pred source))

(defn -candidateRemove
  [pred source]
  (xf/remove pred source))

(defn -candidateTake
  [n source]
  (xf/take n source))

(defn -focusedCoreMap
  [f source]
  (core/map f source))

(defn -focusedCoreFilter
  [pred source]
  (core/filter pred source))

(defn -focusedCoreRemove
  [pred source]
  (core/remove pred source))

(defn -focusedCoreTake
  [n source]
  (core/take n source))

(defn -focusedCandidateMap
  [f source]
  (xf/map f source))

(defn -focusedCandidateFilter
  [pred source]
  (xf/filter pred source))

(defn -focusedCandidateRemove
  [pred source]
  (xf/remove pred source))

(defn -focusedCandidateTake
  [n source]
  (xf/take n source))

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
  (core/transduce
    (fn [rf] rf)
    (fn
      ([total] total)
      ([^long total item]
       (+ total (numeric-value item))))
    0
    value))

;; Direct transduce controls.  Each wrapper is a terminal operation rather
;; than a vector-producing constructor, so the Phase 3 harness can select the
;; exact sink once during setup.  The reducing functions deliberately match
;; the corresponding benchmark sink: first stops after one item, prefix stops
;; after eight, and the remaining controls produce either a checksum or the
;; complete vector.
(defn -transduceFirst
  [xform source]
  (core/transduce
    xform
    (fn
      ([] nil)
      ([value] value)
      ([_ item] (reduced item)))
    nil
    source))

(defn -transducePrefixChecksum
  [xform source]
  (second
    (core/transduce
      xform
      (fn
        ([] [0 0])
        ([state] state)
        ([[count total] item]
         (let [count (inc count)
               total (+ total (numeric-value item))]
           (if (= count 8)
             (reduced [count total])
             [count total]))))
      [0 0]
      source)))

(defn -transduceChecksum
  [xform source]
  (core/transduce
    xform
    (fn
      ([total] total)
      ([^long total item]
       (+ total (numeric-value item))))
    0
    source))

(defn -transduceVector
  [xform source]
  (core/transduce
    xform
    (fn
      ([value] value)
      ([value item] (conj value item)))
    []
    source))
