(ns xfseq.unary-oracle-test
  "Direct Clojure 1.12.5 facts used by the unary compatibility phases.

  The builders in this namespace always return a fresh source.  Later phase
  tests can therefore run the direct oracle and the candidate independently
  without accidentally sharing a consumed lazy source."
  (:require [clojure.test :refer [deftest is testing]]
            [xfseq.core :as xfseq])
  (:import [clojure.lang ASeq IChunk IChunkedSeq Seqable]
           [java.util ArrayList Collection]))

(set! *warn-on-reflection* true)

(def boundary-sizes
  [0 1 2 3 4 5 7 8 9 31 32 33 63 64 65 1000])

(def source-kinds
  [:nil :empty :list :vector :subvector :range :array :iterable :iterator
   :lazy])

(defn fresh-source
  "Build an ordered, fresh source containing `(range n)`."
  [source-kind n]
  (let [values (vec (range n))]
    (case source-kind
      :nil nil
      :empty []
      :list (apply list values)
      :vector values
      :subvector (subvec (vec (range (inc n))) 0 n)
      :range (range n)
      :array (object-array values)
      :iterable (ArrayList. ^Collection values)
      :iterator (iterator-seq
                  (.iterator ^java.lang.Iterable (ArrayList. ^Collection values)))
      :lazy (letfn [(lazy-values [remaining]
                      (lazy-seq
                        (when-some [s (seq remaining)]
                          (cons (first s) (lazy-values (rest s))))))]
              (lazy-values values))
      (throw (IllegalArgumentException.
               (str "Unknown unary oracle source: " source-kind))))))

(defn traced-source
  "A fresh Seqable that records the deferred source `seq` call."
  [events values]
  (reify Seqable
    (seq [_]
      (swap! events conj :source-seq)
      (seq values))))

(defn traced-aseq
  "A fresh dechunked Seqable recording source seq/first/rest order.

  The ordinary backing values are realized before the traced nodes are
  created, so only calls made by the direct core function appear in `events`."
  [events values]
  (letfn [(node [remaining]
            (when-some [s (seq remaining)]
              (let [value (first s)
                    tail (next s)]
                (proxy [ASeq] []
                  (first []
                    (do
                      (swap! events conj [:first value])
                      value))
                  (next []
                    (do
                      (swap! events conj :next)
                      (node tail)))))))]
    (reify Seqable
      (seq [_]
        (swap! events conj :source-seq)
        (node values)))))

(defn throwing-source
  "A fresh Seqable whose first source access throws after recording it."
  [events]
  (reify Seqable
    (seq [_]
      (swap! events conj :source-seq)
      (throw (IllegalStateException. "unary oracle source")))))

(defn throwing-function
  "Return a fresh unary function that throws for `failure-value`."
  [events operation failure-value]
  (fn [value]
    (swap! events conj [operation value])
    (if (= failure-value value)
      (throw (IllegalStateException. (str "unary oracle " operation)))
      value)))

(defn chunk-sizes
  "Return the sizes of realized chunks in a sequence, ignoring empty tails."
  [value]
  (loop [s (seq value)
         sizes []]
    (if (nil? s)
      sizes
      (if (instance? IChunkedSeq s)
        (let [chunked ^IChunkedSeq s]
          (recur (seq (.chunkedMore chunked))
                 (conj sizes (count (.chunkedFirst chunked)))))
        (recur (next s) (conj sizes 1))))))

(defn node-kinds
  "Return `:chunk` or `:cons` for every realized output node."
  [value]
  (loop [s (seq value)
         kinds []]
    (if (nil? s)
      kinds
      (if (instance? IChunkedSeq s)
        (recur (seq (.chunkedMore ^IChunkedSeq s)) (conj kinds :chunk))
        (recur (next s) (conj kinds :cons))))))

(defn direct-unary
  "Apply one direct Clojure unary collection function."
  [operation source]
  (case operation
    :map (clojure.core/map inc source)
    :filter (clojure.core/filter even? source)
    :remove (clojure.core/remove even? source)
    :take (clojure.core/take 5 source)
    (throw (IllegalArgumentException.
             (str "Unknown unary oracle operation: " operation)))))

(defn thrown-class
  "Return the class thrown by `thunk`, or nil when it succeeds."
  [thunk]
  (try
    (thunk)
    nil
    (catch Throwable error
      (class error))))

(deftest direct-unary-values-across-fresh-sources
  (doseq [source-kind source-kinds
          n boundary-sizes
          operation [:map :filter :remove :take]]
    (let [input-values (if (#{:nil :empty} source-kind)
                         []
                         (vec (range n)))
          expected (case operation
                     :map (vec (clojure.core/map inc input-values))
                     :filter (vec (clojure.core/filter even? input-values))
                     :remove (vec (clojure.core/remove even? input-values))
                     :take (vec (clojure.core/take 5 input-values)))
          actual (vec (direct-unary operation (fresh-source source-kind n)))]
      (testing (str source-kind "/" n "/" (name operation))
        (is (= expected actual))))))

(defn- transformed-rf-snapshot
  "Invoke a fresh transformed reducing function through completion.

  Unlike `transduce` with an explicit init, this deliberately exercises the
  transformed reducing function's zero-arity initializer.  It also stops on
  the first `Reduced` result and completes with the unwrapped accumulator."
  [xform values]
  (let [events (atom [])
        rf (fn
             ([]
              (swap! events conj :init)
              [])
             ([acc]
              (swap! events conj [:complete acc])
              acc)
             ([acc item]
              (swap! events conj [:step item])
              (conj acc item)))
        step (xform rf)
        init (step)
        {:keys [acc step-values reduced-flags]}
        (loop [acc init
               values (seq values)
               step-values []
               reduced-flags []]
          (if (nil? values)
            {:acc acc
             :step-values step-values
             :reduced-flags reduced-flags}
            (let [next-acc (step acc (first values))
                  reduced? (reduced? next-acc)
                  step-values (conj step-values (unreduced next-acc))
                  reduced-flags (conj reduced-flags reduced?)]
              (if reduced?
                {:acc next-acc
                 :step-values step-values
                 :reduced-flags reduced-flags}
                (recur next-acc (next values)
                       step-values reduced-flags)))))
        completion (step (unreduced acc))]
    {:init init
     :step-values step-values
     :reduced-flags reduced-flags
     :completion completion
     :events @events}))

(defn- invalid-step-class
  [xform]
  (let [step (xform (fn
                      ([acc] acc)
                      ([acc _item] acc)))]
    (thrown-class #(step [] 1 2))))

(defn- invalid-xform-class
  [xform]
  (thrown-class #(xform)))

(deftest public-transducer-arities-delegate-to-core
  (doseq [[operation ours direct]
          [[:map #(xfseq/map inc) #(clojure.core/map inc)]
           [:filter #(xfseq/filter even?) #(clojure.core/filter even?)]
           [:remove #(xfseq/remove even?) #(clojure.core/remove even?)]
           [:take #(xfseq/take 2) #(clojure.core/take 2)]]]
    (testing (name operation)
      (is (= (transformed-rf-snapshot (direct) [2 3 4])
             (transformed-rf-snapshot (ours) [2 3 4])))
      (is (= (invalid-xform-class (direct))
             (invalid-xform-class (ours)))))))

(deftest delegated-fixed-arity-transducers-reject-invalid-step-arity
  (doseq [[operation ours direct]
          [[:filter #(xfseq/filter even?) #(clojure.core/filter even?)]
           [:remove #(xfseq/remove even?) #(clojure.core/remove even?)]
           [:take #(xfseq/take 2) #(clojure.core/take 2)]]]
    (testing (name operation)
      (is (= (invalid-step-class (direct))
             (invalid-step-class (ours)))))))

(deftest delegated-map-transducer-keeps-multi-input-step
  (let [ours ((xfseq/map vector) conj)
        direct ((clojure.core/map vector) conj)]
    (doseq [inputs [[1 :a]
                    [1 :a :b]
                    [1 :a :b :c nil]]]
      (testing (pr-str inputs)
        (is (= (apply direct [] inputs)
               (apply ours [] inputs)))))))

(deftest unary-collection-surfaces-remain-unary
  (doseq [operation [xfseq/map xfseq/filter xfseq/remove xfseq/take]]
    (is (thrown? clojure.lang.ArityException
                 (operation identity [] [])))))

(deftest direct-chunk-output-facts
  (testing "map preserves input chunking"
    (doseq [n boundary-sizes]
      (let [result (clojure.core/map inc (vec (range n)))
            expected (loop [remaining n
                            sizes []]
                       (if (zero? remaining)
                         sizes
                         (let [size (min remaining 32)]
                           (recur (- remaining size) (conj sizes size)))))]
        (is (= expected (chunk-sizes result)))
        (is (= (pos? n) (instance? IChunkedSeq (seq result)))))))
  (testing "filter and remove retain non-empty input chunks, including sparse ones"
    (doseq [pass-count [0 1 2 3 4 5 31 32 33 40 63 64]]
      (let [input (vec (range 64))
            expected (cond-> []
                      (pos? pass-count) (conj (min pass-count 32))
                      (> pass-count 32) (conj (- pass-count 32)))
            filter-result (clojure.core/filter #(< % pass-count) input)
            remove-result (clojure.core/remove #(>= % pass-count) input)]
        (testing (str "pass-count=" pass-count)
          (is (= expected (chunk-sizes filter-result)))
          (is (= expected (chunk-sizes remove-result)))
          (is (= (pos? pass-count)
                 (instance? IChunkedSeq (seq filter-result))))
          (is (= (pos? pass-count)
                 (instance? IChunkedSeq (seq remove-result))))))))
  (testing "take remains an unchunked Cons chain"
    (doseq [take-count [-1 0 1 4 5 31 32 33 64]]
      (let [result (clojure.core/take take-count (vec (range 64)))
            expected-count (max 0 (min take-count 64))]
        (is (= expected-count (count result)))
        (is (every? #{:cons} (node-kinds result)))))))

(deftest direct-filter-chunk-drives-downstream-map
  (let [calls (atom [])
        result (clojure.core/map
                 (fn [value]
                   (swap! calls conj value)
                   value)
                 (clojure.core/filter #(zero? (mod % 8))
                                      (vec (range 32))))]
    (is (= [] @calls))
    (is (= 0 (first result)))
    (is (= [0 8 16 24] @calls))
    (let [chunk (clojure.core/chunk-first (seq result))]
      (is (= [0 8 16 24]
             (mapv #(.nth ^IChunk chunk %) (range (.count ^IChunk chunk))))))))

(deftest direct-dechunked-source-order
  (testing "map calls source first, mapper, then source rest"
    (let [events (atom [])
          result (clojure.core/map
                   (fn [value]
                     (swap! events conj [:map value])
                     (* 10 value))
                   (traced-aseq events [1 2]))]
      (is (= [] @events))
      (is (= 10 (first result)))
      (is (= [:source-seq [:first 1] [:map 1] :next] @events))))
  (testing "filter advances source rest before its predicate"
    (let [events (atom [])
          result (clojure.core/filter
                   (fn [value]
                     (swap! events conj [:pred value])
                     (= value 2))
                   (traced-aseq events [1 2 3]))]
      (is (= [] @events))
      (is (= 2 (first result)))
      (is (= [:source-seq [:first 1] :next [:pred 1]
              [:first 2] :next [:pred 2]]
             @events))))
  (testing "remove inherits filter's source order"
    (let [events (atom [])
          result (clojure.core/remove
                   (fn [value]
                     (swap! events conj [:pred value])
                     (= value 1))
                   (traced-aseq events [1 2]))]
      (is (= 2 (first result)))
      (is (= [:source-seq [:first 1] :next [:pred 1]
              [:first 2] :next [:pred 2]]
             @events)))))

(deftest direct-take-guard-and-advance-facts
  (testing "non-positive counts do not touch the source"
    (doseq [take-count [-1 0]]
      (let [events (atom [])
            result (clojure.core/take take-count
                                      (traced-source events [1 2]))]
        (is (nil? (seq result)))
        (is (= [] @events)))))
  (testing "invalid counts throw before source access"
    (doseq [[take-count expected-class]
            [[nil NullPointerException]
             ["bad" ClassCastException]
             [:bad ClassCastException]]]
      (let [events (atom [])
            result (clojure.core/take take-count
                                      (traced-source events []))]
        (is (= expected-class (thrown-class #(seq result))))
        (is (= [] @events)))))
  (testing "positive take advances rest after its final value only"
    (let [events (atom [])
          result (clojure.core/take 1 (traced-aseq events [1 2]))]
      (is (= 1 (first result)))
      (is (= [:source-seq [:first 1] :next] @events))
      (is (= [1] (vec result)))
      (is (= [:source-seq [:first 1] :next] @events)))))

(deftest direct-failed-lazy-nodes-are-one-shot
  (testing "a later dechunked map node does not retry its failed mapper"
    (let [events (atom [])
          result (clojure.core/map (throwing-function events :map 2)
                                   (list 1 2))]
      (is (= 1 (first result)))
      (is (= IllegalStateException (thrown-class #(next result))))
      (is (nil? (next result)))
      (is (= [[:map 1] [:map 2]] @events))))
  (testing "a later dechunked filter node does not retry its failed predicate"
    (let [events (atom [])
          pred (fn [value]
                 (swap! events conj [:pred value])
                 (cond
                   (= value 1) true
                   (= value 2) (throw (IllegalStateException. "predicate"))
                   :else false))
          result (clojure.core/filter pred (list 1 2))]
      (is (= 1 (first result)))
      (is (= IllegalStateException (thrown-class #(next result))))
      (is (nil? (next result)))
      (is (= [[:pred 1] [:pred 2]] @events))))
  (testing "a failed initial chunk node becomes an empty tail"
    (let [events (atom [])
          result (clojure.core/map (throwing-function events :map 1)
                                   (vec [0 1 2]))]
      (is (= IllegalStateException (thrown-class #(first result))))
      (is (nil? (seq result)))
      (is (= [[:map 0] [:map 1]] @events))))
  (testing "a failed source seq is not retried"
    (let [events (atom [])
          result (clojure.core/map identity (throwing-source events))]
      (is (= IllegalStateException (thrown-class #(first result))))
      (is (nil? (seq result)))
      (is (= [:source-seq] @events)))))
