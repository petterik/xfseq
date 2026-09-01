(ns xfseq.unary-oracle-test
  "Direct Clojure 1.12.5 facts used by the unary compatibility phases.

  The builders in this namespace always return a fresh source.  Later phase
  tests can therefore run the direct oracle and the candidate independently
  without accidentally sharing a consumed lazy source."
  (:require [clojure.test :refer [deftest is testing]]
            [xfseq.core :as xfseq])
  (:import [clojure.lang ASeq IChunk IChunkedSeq LazySeq Seqable]
           [java.lang.reflect Field]
           [java.util ArrayList Collection]
           [java.util.concurrent CountDownLatch]))

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

(defn throwing-node-source
  "A fresh dechunked source that throws from `first` or `next`."
  [events failure-point]
  (reify Seqable
    (seq [_]
      (swap! events conj :source-seq)
      (proxy [ASeq] []
        (first []
          (swap! events conj :first)
          (if (= failure-point :first)
            (throw (IllegalStateException. "unary oracle first"))
            1))
        (next []
          (swap! events conj :next)
          (if (= failure-point :next)
            (throw (IllegalStateException. "unary oracle next"))
            nil))))))

(defn traced-chunk
  "A fresh chunk that can fail at one indexed access."
  [events node-index values failure]
  (let [read (fn [index]
               (swap! events conj [:chunk-nth node-index index])
               (if (and (= :nth (:method failure))
                        (= node-index (:node failure))
                        (= index (:index failure)))
                 (throw (IllegalStateException. "unary oracle chunk nth"))
                 (nth values index)))]
    (reify IChunk
      (count [_]
        (count values))
      (nth [_ index]
        (read index))
      (nth [_ index not-found]
        (if (<= 0 index (dec (count values)))
          (read index)
          not-found))
      (dropFirst [_]
        (let [remaining (vec (rest values))]
          (clojure.lang.ArrayChunk.
            (object-array remaining) 0 (count remaining))))
      (reduce [_ rf init]
        (reduce rf init values)))))

(defn traced-chunked-source
  "A fresh finite chunked source with one optional failing operation.

  `failure` is nil or a map with `:node` and `:method`, plus `:index` for
  `:nth`.  Every direct and candidate run gets a new source graph."
  [events chunks failure]
  (letfn [(node [node-index]
            (let [values (nth chunks node-index)
                  chunk (traced-chunk events node-index values failure)
                  more-node (when (< (inc node-index) (count chunks))
                              (node (inc node-index)))]
              (proxy [ASeq IChunkedSeq] []
                (first []
                  (.nth ^IChunk chunk 0))
                (next []
                  more-node)
                (more []
                  (or more-node clojure.lang.PersistentList/EMPTY))
                (chunkedFirst []
                  (swap! events conj [:chunked-first node-index])
                  (if (and (= :chunked-first (:method failure))
                           (= node-index (:node failure)))
                    (throw (IllegalStateException.
                             "unary oracle chunkedFirst"))
                    chunk))
                (chunkedNext []
                  more-node)
                (chunkedMore []
                  (swap! events conj [:chunked-more node-index])
                  (if (and (= :chunked-more (:method failure))
                           (= node-index (:node failure)))
                    (throw (IllegalStateException.
                             "unary oracle chunkedMore"))
                    more-node))))) ]
    (reify Seqable
      (seq [_]
        (swap! events conj :source-seq)
        (node 0)))))

(defn throwing-function
  "Return a fresh unary function that throws for `failure-value`."
  [events operation failure-value]
  (fn [value]
    (swap! events conj [operation value])
    (if (= failure-value value)
      (throw (IllegalStateException. (str "unary oracle " operation)))
      value)))

(defn invalid-arity-function
  "Return a fresh callable whose unary invocation has an invalid arity."
  []
  (fn [_first _second]
    (throw (IllegalArgumentException. "wrong arity"))))

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

(defn candidate-unary
  "Apply the Phase 3 public unary candidate to a source."
  [operation source]
  (case operation
    :map (xfseq/map inc source)
    :filter (xfseq/filter even? source)
    :remove (xfseq/remove even? source)
    :take (xfseq/take 5 source)
    (throw (IllegalArgumentException.
             (str "Unknown unary candidate operation: " operation)))))

(defn- concurrently
  [f]
  (let [start (CountDownLatch. 1)
        values (atom [])
        errors (atom [])
        threads (repeatedly
                  2
                  #(Thread.
                     (reify Runnable
                       (run [_]
                         (try
                           (.await ^CountDownLatch start)
                           (swap! values conj (f))
                           (catch Throwable error
                             (swap! errors conj error)))))))]
    (doseq [thread threads]
      (.start ^Thread thread))
    (.countDown start)
    (doseq [thread threads]
      (.join ^Thread thread 5000))
    [@values @errors]))

(defn thrown-class
  "Return the class thrown by `thunk`, or nil when it succeeds."
  [thunk]
  (try
    (thunk)
    nil
    (catch Throwable error
      (class error))))

(defn- force-classes
  "Force one lazy node four times, recording each thrown exception class."
  [thunk]
  (vec (repeatedly 4 #(thrown-class thunk))))

(defn- private-field
  [object field-name]
  (let [^Field field (.getDeclaredField ^Class (class object) field-name)]
    (.setAccessible field true)
    (.get field object)))

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

(defn- transformed-rf-reduced-snapshot
  "Compare a transformed reducing function when its sink stops early.

  The sink returns `reduced` on the second ordinary step, so this exercises
  the transformed function's zero-arity init, ordinary steps, Reduced
  propagation, and one-arity completion directly rather than through
  `transduce` with an explicit init."
  [xform values]
  (let [events (atom [])
        rf (fn
             ([]
              (swap! events conj :init)
              [])
             ([acc]
              (swap! events conj [:complete acc])
              (conj acc :complete))
             ([acc item]
              (swap! events conj [:step item])
              (let [next-acc (conj acc item)]
                (if (= item 3)
                  (reduced next-acc)
                  next-acc))))
        step (xform rf)
        init (step)
        first-step (step init (first values))
        second-step (step first-step (second values))
        completion (step (unreduced second-step))]
    {:init init
     :step-values [(unreduced first-step) (unreduced second-step)]
     :reduced-flags [(reduced? first-step) (reduced? second-step)]
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

(deftest delegated-take-transducer-preserves-reduced-and-completion
  (is (= (transformed-rf-reduced-snapshot (clojure.core/take 2) [2 3 4])
         (transformed-rf-reduced-snapshot (xfseq/take 2) [2 3 4]))))

(deftest delegated-transducers-preserve-invalid-step-arity
  (doseq [[operation ours direct]
          [[:map #(xfseq/map inc) #(clojure.core/map inc)]
           [:filter #(xfseq/filter even?) #(clojure.core/filter even?)]
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

(deftest unary-candidate-values-match-direct-core
  (doseq [source-kind source-kinds
          n boundary-sizes
          operation [:map :filter :remove :take]]
    (let [expected (vec (direct-unary operation
                                      (fresh-source source-kind n)))
          actual (vec (candidate-unary operation
                                       (fresh-source source-kind n)))]
      (testing (str source-kind "/" n "/" (name operation))
        (is (= expected actual))))))

(deftest unary-candidate-output-shape-matches-direct-core
  (testing "map retains the direct input-chunk shape"
    (doseq [n boundary-sizes]
      (let [direct (direct-unary :map (fresh-source :vector n))
            candidate (candidate-unary :map (fresh-source :vector n))]
        (is (= (chunk-sizes direct) (chunk-sizes candidate)))
        (is (= (node-kinds direct) (node-kinds candidate))))))
  (testing "sparse filter and remove outputs stay chunked"
    (doseq [pass-count [0 1 2 3 4 5 31 32 33 40 63 64]]
      (let [input (vec (range 64))
            pred #(< % pass-count)
            direct-filter (clojure.core/filter pred input)
            candidate-filter (xfseq/filter pred (vec input))
            direct-remove (clojure.core/remove #(>= % pass-count) input)
            candidate-remove (xfseq/remove #(>= % pass-count) (vec input))]
        (testing (str "pass-count=" pass-count)
          (is (= (chunk-sizes direct-filter)
                 (chunk-sizes candidate-filter)))
          (is (= (node-kinds direct-filter)
                 (node-kinds candidate-filter)))
          (is (= (chunk-sizes direct-remove)
                 (chunk-sizes candidate-remove)))
          (is (= (node-kinds direct-remove)
                 (node-kinds candidate-remove)))))))
  (testing "take retains its direct unchunked Cons shape"
    (doseq [take-count [-1 0 1 4 5 31 32 33 64]]
      (let [input (vec (range 64))
            direct (clojure.core/take take-count input)
            candidate (xfseq/take take-count (vec input))]
        (is (= (node-kinds direct) (node-kinds candidate)))))))

(deftest unary-candidate-dechunked-order-matches-direct-core
  (testing "map invokes the mapper before source rest"
    (let [direct-events (atom [])
          candidate-events (atom [])
          direct (clojure.core/map
                   (fn [value]
                     (swap! direct-events conj [:map value])
                     (* 10 value))
                   (traced-aseq direct-events [1 2]))
          candidate (xfseq/map
                      (fn [value]
                        (swap! candidate-events conj [:map value])
                        (* 10 value))
                      (traced-aseq candidate-events [1 2]))]
      (is (= (first direct) (first candidate)))
      (is (= @direct-events @candidate-events))))
  (testing "filter and remove advance source rest before predicates"
    (doseq [operation [:filter :remove]]
      (let [direct-events (atom [])
            candidate-events (atom [])
            pred (fn [events value]
                   (swap! events conj [:pred value])
                   (= value 2))
            direct (case operation
                     :filter (clojure.core/filter #(pred direct-events %)
                                                  (traced-aseq direct-events [1 2 3]))
                     :remove (clojure.core/remove #(pred direct-events %)
                                                  (traced-aseq direct-events [1 2 3])))
            candidate (case operation
                        :filter (xfseq/filter #(pred candidate-events %)
                                              (traced-aseq candidate-events [1 2 3]))
                        :remove (xfseq/remove #(pred candidate-events %)
                                              (traced-aseq candidate-events [1 2 3])))]
        (is (= (first direct) (first candidate)))
        (is (= @direct-events @candidate-events)))))
  (testing "take advances source rest after its final value"
    (let [direct-events (atom [])
          candidate-events (atom [])
          direct (clojure.core/take 1 (traced-aseq direct-events [1 2]))
          candidate (xfseq/take 1 (traced-aseq candidate-events [1 2]))]
      (is (= (first direct) (first candidate)))
      (is (= @direct-events @candidate-events))
      (is (= [1] (vec candidate)))
      (is (= @direct-events @candidate-events)))))

(deftest unary-candidate-sparse-filter-drives-downstream-map
  (let [direct-calls (atom [])
        candidate-calls (atom [])
        direct (clojure.core/map
                 (fn [value]
                   (swap! direct-calls conj value)
                   value)
                 (clojure.core/filter #(zero? (mod % 8))
                                      (vec (range 32))))
        candidate (clojure.core/map
                    (fn [value]
                      (swap! candidate-calls conj value)
                      value)
                    (xfseq/filter #(zero? (mod % 8))
                                  (vec (range 32))))]
    (is (= [] @direct-calls))
    (is (= [] @candidate-calls))
    (is (= (first direct) (first candidate)))
    (is (= [0 8 16 24] @direct-calls))
    (is (= @direct-calls @candidate-calls))
    (is (= (chunk-sizes direct) (chunk-sizes candidate)))))

(deftest unary-candidate-take-guard-and-invalid-count-match-direct-core
  (testing "non-positive counts do not touch either source"
    (doseq [take-count [-1 0]]
      (let [direct-events (atom [])
            candidate-events (atom [])
            direct (clojure.core/take take-count
                                      (traced-source direct-events [1 2]))
            candidate (xfseq/take take-count
                                  (traced-source candidate-events [1 2]))]
        (is (= [nil nil nil nil] (force-classes #(seq direct))))
        (is (= [nil nil nil nil] (force-classes #(seq candidate))))
        (is (= [] @direct-events))
        (is (= @direct-events @candidate-events)))))
  (testing "invalid counts throw before either source"
    (doseq [[take-count expected-class]
            [[nil NullPointerException]
             ["bad" ClassCastException]
             [:bad ClassCastException]]]
      (let [direct-events (atom [])
            candidate-events (atom [])
            direct (clojure.core/take take-count
                                      (traced-source direct-events []))
            candidate (xfseq/take take-count
                                  (traced-source candidate-events []))]
        (is (= (vec (repeat 4 expected-class))
               (force-classes #(seq direct))))
        (is (= (vec (repeat 4 expected-class))
               (force-classes #(seq candidate))))
        (let [initializer (private-field candidate "fn")]
          (is (some? (private-field initializer "coll")))
          (is (some? (private-field initializer "xform"))))
        (is (= [] @direct-events))
        (is (= @direct-events @candidate-events))))))

(deftest unary-candidate-failed-nodes-are-one-shot
  (testing "later map and filter failures do not retry their lazy nodes"
    (doseq [operation [:map :filter :remove]]
      (let [direct-events (atom [])
            candidate-events (atom [])
            direct (case operation
                     :map (clojure.core/map
                            (throwing-function direct-events :map 2)
                            (list 1 2))
                     :filter (clojure.core/filter
                               (fn [value]
                                 (swap! direct-events conj [:pred value])
                                 (if (= value 2)
                                   (throw (IllegalStateException. "predicate"))
                                   true))
                               (list 1 2))
                     :remove (clojure.core/remove
                               (fn [value]
                                 (swap! direct-events conj [:pred value])
                                 (if (= value 2)
                                   (throw (IllegalStateException. "predicate"))
                                   false))
                               (list 1 2)))
            candidate (case operation
                       :map (xfseq/map
                              (throwing-function candidate-events :map 2)
                              (list 1 2))
                       :filter (xfseq/filter
                                 (fn [value]
                                   (swap! candidate-events conj [:pred value])
                                   (if (= value 2)
                                     (throw (IllegalStateException. "predicate"))
                                     true))
                                 (list 1 2))
                       :remove (xfseq/remove
                                 (fn [value]
                                   (swap! candidate-events conj [:pred value])
                                   (if (= value 2)
                                     (throw (IllegalStateException. "predicate"))
                                     false))
                                 (list 1 2)))]
        (is (= (first direct) (first candidate)))
        (is (= [IllegalStateException nil nil nil]
               (force-classes #(next direct))))
        (is (= [IllegalStateException nil nil nil]
               (force-classes #(next candidate))))
        (is (= @direct-events @candidate-events)))))
  (testing "initial chunk and source failures leave empty tails"
    (let [direct-events (atom [])
          candidate-events (atom [])
          direct (clojure.core/map
                   (throwing-function direct-events :map 1)
                   (vec [0 1 2]))
          candidate (xfseq/map
                      (throwing-function candidate-events :map 1)
                      (vec [0 1 2]))]
      (is (= [IllegalStateException nil nil nil]
             (force-classes #(first direct))))
      (is (= [IllegalStateException nil nil nil]
             (force-classes #(first candidate))))
      (is (= @direct-events @candidate-events)))
    (doseq [operation [:map :take]]
      (let [direct-events (atom [])
            candidate-events (atom [])
            direct (case operation
                     :map (clojure.core/map identity
                                            (throwing-source direct-events))
                     :take (clojure.core/take 1
                                              (throwing-source direct-events)))
            candidate (case operation
                        :map (xfseq/map identity
                                         (throwing-source candidate-events))
                        :take (xfseq/take 1
                                          (throwing-source candidate-events)))]
        (testing (str (name operation) "/source-seq")
          (is (= [IllegalStateException nil nil nil]
                 (force-classes #(first direct))))
          (is (= [IllegalStateException nil nil nil]
                 (force-classes #(first candidate))))
          (is (= @direct-events @candidate-events)))))))

(deftest unary-candidate-custom-source-node-failures-are-one-shot
  (doseq [[operation failure-point]
          [[:map :first] [:map :next]
           [:filter :first] [:filter :next]
           [:take :first]]]
    (let [direct-events (atom [])
          candidate-events (atom [])
          direct (case operation
                   :map (clojure.core/map identity
                                          (throwing-node-source direct-events
                                                                failure-point))
                   :filter (clojure.core/filter identity
                                                (throwing-node-source direct-events
                                                                      failure-point))
                   :take (clojure.core/take 1
                                            (throwing-node-source direct-events
                                                                  failure-point)))
          candidate (case operation
                      :map (xfseq/map identity
                                       (throwing-node-source candidate-events
                                                             failure-point))
                      :filter (xfseq/filter identity
                                             (throwing-node-source candidate-events
                                                                   failure-point))
                      :take (xfseq/take 1
                                        (throwing-node-source candidate-events
                                                              failure-point)))]
      (testing (str (name operation) "/" (name failure-point))
        (is (= [IllegalStateException nil nil nil]
               (force-classes #(first direct))))
        (is (= [IllegalStateException nil nil nil]
               (force-classes #(first candidate))))
        (is (= @direct-events @candidate-events))))))

(deftest unary-candidate-take-final-rest-failure-matches-direct-core
  (let [direct-events (atom [])
        candidate-events (atom [])
        direct (clojure.core/take
                 1
                 (throwing-node-source direct-events :next))
        candidate (xfseq/take
                    1
                    (throwing-node-source candidate-events :next))
        direct-errors (force-classes #(first direct))
        candidate-errors (force-classes #(first candidate))]
    (is (= [IllegalStateException NullPointerException
            NullPointerException NullPointerException]
           direct-errors))
    (is (= direct-errors candidate-errors))
    (is (= @direct-events @candidate-events))))

(deftest unary-candidate-invalid-arity-and-source-match-direct-core
  (testing "invalid mapper and predicate arities fail at the same point"
    (doseq [operation [:map :filter :remove]]
      (let [direct-events (atom [])
            candidate-events (atom [])
            direct-invalid (invalid-arity-function)
            candidate-invalid (invalid-arity-function)
            direct (case operation
                     :map (clojure.core/map direct-invalid
                                            (traced-source direct-events [1]))
                     :filter (clojure.core/filter direct-invalid
                                                  (traced-source direct-events [1]))
                     :remove (clojure.core/remove direct-invalid
                                                  (traced-source direct-events [1])))
            candidate (case operation
                        :map (xfseq/map candidate-invalid
                                         (traced-source candidate-events [1]))
                        :filter (xfseq/filter candidate-invalid
                                               (traced-source candidate-events [1]))
                        :remove (xfseq/remove candidate-invalid
                                               (traced-source candidate-events [1])))
            direct-errors (force-classes #(first direct))
            candidate-errors (force-classes #(first candidate))]
        (testing (name operation)
          (is (= [clojure.lang.ArityException nil nil nil]
                 direct-errors))
          (is (= direct-errors candidate-errors))
          (is (= @direct-events @candidate-events))))))
  (testing "a non-seqable source fails before any operation callback"
    (doseq [operation [:map :filter :remove :take]]
      (let [direct (case operation
                     :map (clojure.core/map identity (Object.))
                     :filter (clojure.core/filter identity (Object.))
                     :remove (clojure.core/remove identity (Object.))
                     :take (clojure.core/take 1 (Object.)))
            candidate (case operation
                       :map (xfseq/map identity (Object.))
                       :filter (xfseq/filter identity (Object.))
                       :remove (xfseq/remove identity (Object.))
                       :take (xfseq/take 1 (Object.)))
            direct-errors (force-classes #(first direct))
            candidate-errors (force-classes #(first candidate))]
        (testing (name operation)
          (is (= [IllegalArgumentException nil nil nil]
                 direct-errors))
          (is (= direct-errors candidate-errors)))))))

(defn- chunked-result
  [candidate? operation source]
  (case operation
    :map (if candidate?
           (xfseq/map inc source)
           (clojure.core/map inc source))
    :filter (if candidate?
              (xfseq/filter even? source)
              (clojure.core/filter even? source))
    :remove (if candidate?
              (xfseq/remove even? source)
              (clojure.core/remove even? source))))

(defn- initial-chunk-failure-snapshot
  [candidate? operation failure]
  (let [events (atom [])
        result (chunked-result
                 candidate?
                 operation
                 (traced-chunked-source events [[0 1] [2 3]] failure))
        errors (force-classes #(first result))]
    {:errors errors
     :events @events}))

(defn- later-chunk-failure-snapshot
  [candidate? operation failure]
  (let [events (atom [])
        result (chunked-result
                 candidate?
                 operation
                 (traced-chunked-source events [[0 1] [2 3]] failure))
        prefix (first result)
        tail (rest result)
        errors (force-classes #(seq tail))]
    {:prefix prefix
     :errors errors
     :events @events}))

(deftest unary-candidate-chunked-source-failures-match-direct-core
  (let [operations [:map :filter :remove]
        initial-failures [{:node 0 :method :chunked-first}
                          {:node 0 :method :nth :index 1}
                          {:node 0 :method :chunked-more}]
        later-failures [{:node 1 :method :chunked-first}
                        {:node 1 :method :nth :index 1}
                        {:node 1 :method :chunked-more}]]
    (testing "initial chunk failures"
      (doseq [operation operations
              failure initial-failures]
        (testing (str (name operation) "/" (pr-str failure))
          (is (= (initial-chunk-failure-snapshot false operation failure)
                 (initial-chunk-failure-snapshot true operation failure))))))
    (testing "later chunk failures preserve the realized prefix"
      (doseq [operation operations
              failure later-failures]
        (testing (str (name operation) "/" (pr-str failure))
          (is (= (later-chunk-failure-snapshot false operation failure)
                 (later-chunk-failure-snapshot true operation failure))))))))

(defn- surface-result
  [candidate? operation source]
  (case operation
    :map (if candidate?
           (xfseq/map inc source)
           (clojure.core/map inc source))
    :filter (if candidate?
              (xfseq/filter even? source)
              (clojure.core/filter even? source))
    :remove (if candidate?
              (xfseq/remove even? source)
              (clojure.core/remove even? source))
    :take (if candidate?
            (xfseq/take 3 source)
            (clojure.core/take 3 source))))

(defn- surface-snapshot
  [candidate? operation input]
  (let [result (surface-result candidate? operation (vec input))
        early (reduce (fn [acc value]
                        (reduced [acc value]))
                      :start
                      result)
        values (vec result)]
    {:class (class result)
     :seq? (seq? result)
     :sequential? (sequential? result)
     :first (first result)
     :next (vec (next result))
     :rest (vec (rest result))
     :nth (nth result 1 nil)
     :count (count result)
     :values values
     :into (into [] result)
     :reduce (reduce + result)
     :reduce-init (reduce + 10 result)
     :early early
     :equal-list (= result (apply list values))
     :equal-vector (= result values)
     :hash-equal (= (hash result) (hash (apply list values)))
     :printed (pr-str result)
     :iterated (vec (iterator-seq (.iterator ^java.lang.Iterable result)))
     :with-meta (meta (with-meta result {:surface operation}))}))

(deftest unary-candidate-standard-surface-matches-direct-core
  (doseq [operation [:map :filter :remove :take]
          input [[] (vec (range 6))]]
    (testing (str (name operation) "/" (if (seq input) "non-empty" "empty"))
      (is (= (surface-snapshot false operation input)
             (surface-snapshot true operation input)))))
  (doseq [operation [:map :filter :remove :take]]
    (let [direct (surface-result false operation (vec (range 6)))
          candidate (surface-result true operation (vec (range 6)))
          [direct-values direct-errors] (concurrently #(nth direct 1))
          [candidate-values candidate-errors] (concurrently #(nth candidate 1))]
      (testing (str "concurrent/" (name operation))
        (is (= (sort direct-values) (sort candidate-values)))
        (is (= [] direct-errors))
        (is (= [] candidate-errors))))))

(deftest unary-candidate-failed-nodes-release-engine-state
  (testing "a failed initializer releases its source and xform"
    (let [events (atom [])
          result (xfseq/map
                   (throwing-function events :map 0)
                   (list 0 1))
          initializer (private-field result "fn")]
      (is (= [IllegalStateException nil nil nil]
             (force-classes #(first result))))
      (is (nil? (private-field initializer "coll")))
      (is (nil? (private-field initializer "xform")))))
  (testing "a failed step releases its source, xform, buffer, and accumulator"
    (let [events (atom [])
          result (xfseq/map
                   (throwing-function events :map 2)
                   (list 1 2))]
      (is (= 1 (first result)))
      (let [tail (rest result)
            step (private-field tail "fn")]
        (is (= [IllegalStateException nil nil nil]
               (force-classes #(seq tail))))
        (doseq [field ["s" "xf" "buf" "accumulator"]]
          (testing field
            (is (nil? (private-field step field))))))))
  (testing "direct one-shot closures clear captured source state"
    (let [events (atom [])
          result (clojure.core/map
                   (throwing-function events :map 2)
                   (list 1 2))]
      (is (= 1 (first result)))
      (let [tail (rest result)
            closure (private-field tail "fn")]
        (is (= [IllegalStateException nil nil nil]
               (force-classes #(seq tail))))
        (is (nil? (private-field closure "coll")))))))

(deftest unary-candidate-surface-cache-concurrency-and-one-shot-source
  (let [calls (atom 0)
        result (xfseq/map (fn [value]
                            (swap! calls inc)
                            (* 2 value))
                          (list 1 2 3))]
    (is (instance? LazySeq result))
    (is (seq? result))
    (is (sequential? result))
    (is (= 0 @calls))
    (is (= 2 (first result)))
    (is (= 2 (first result)))
    (is (= 1 @calls))
    (is (= [2 4 6] (vec result)))
    (is (= [2 4 6] (vec result)))
    (is (= 3 @calls))
    (is (= 4 (nth result 1)))
    (is (= 3 (count result)))
    (is (= [2 4 6] (into [] result)))
    (is (= 12 (reduce + result)))
    (is (= result (list 2 4 6)))
    (is (= (hash result) (hash (list 2 4 6))))
    (is (= {:tag :candidate}
           (meta (with-meta result {:tag :candidate}))))
    (is (= "(2 4 6)" (pr-str result)))
    (is (= [2 4 6]
           (vec (iterator-seq (.iterator ^java.lang.Iterable result))))))
  (let [empty-result (xfseq/map inc [])]
    (is (instance? LazySeq empty-result))
    (is (nil? (seq empty-result)))
    (is (nil? (seq empty-result))))
  (let [calls (atom 0)
        result (xfseq/map (fn [value]
                            (swap! calls inc)
                            (* 2 value))
                          (list 1 2 3))
        [values errors] (concurrently #(nth result 1))]
    (is (= [4 4] (sort values)))
    (is (= [] errors))
    (is (= 2 @calls)))
  (let [calls (atom [])
        result (xfseq/map (fn [value]
                            (swap! calls conj value)
                            value)
                          (fresh-source :iterator 5))]
    (is (= (vec (range 5)) (vec result)))
    (is (= (vec (range 5)) (vec result)))
    (is (= (vec (range 5)) @calls))))

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
        (is (= (vec (repeat 4 expected-class))
               (force-classes #(seq result))))
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
      (is (= [IllegalStateException nil nil nil]
             (force-classes #(next result))))
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
      (is (= [IllegalStateException nil nil nil]
             (force-classes #(next result))))
      (is (= [[:pred 1] [:pred 2]] @events))))
  (testing "a failed initial chunk node becomes an empty tail"
    (let [events (atom [])
          result (clojure.core/map (throwing-function events :map 1)
                                   (vec [0 1 2]))]
      (is (= [IllegalStateException nil nil nil]
             (force-classes #(first result))))
      (is (= [[:map 0] [:map 1]] @events))))
  (testing "a failed source seq is not retried"
    (let [events (atom [])
          result (clojure.core/map identity (throwing-source events))]
      (is (= [IllegalStateException nil nil nil]
             (force-classes #(first result))))
      (is (= [:source-seq] @events)))))
