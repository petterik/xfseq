(ns xfseq.object-engine-test
  (:require [clojure.test :refer [deftest is testing]]
            [xfseq.core :as core])
  (:import [clojure.lang ArrayChunk ChunkedCons IChunk IChunkedSeq LazySeq Seqable]
           [xfseq.buffer ObjectBuffer]
           [java.lang.reflect Field]
           [java.util.concurrent CountDownLatch]))

(set! *warn-on-reflection* true)

(defn traced-source [events values]
  (reify Seqable
    (seq [_]
      (swap! events conj :source-seq)
      (seq values))))

(defn event-kind [event]
  (if (vector? event) (first event) event))

(defn chunk-sizes [value]
  (loop [s (seq value)
         sizes []]
    (if (nil? s)
      sizes
      (if (instance? IChunkedSeq s)
        (let [chunked ^IChunkedSeq s
              chunk ^IChunk (.chunkedFirst chunked)]
          (recur (seq (.chunkedMore chunked)) (conj sizes (.count chunk))))
        (recur (seq (.more ^clojure.lang.ISeq s)) (conj sizes 1))))))

(defn private-field [value field-name]
  (let [field ^Field (.getDeclaredField ^Class (class value) field-name)]
    (.setAccessible field true)
    (.get field value)))

(defn concurrently [f]
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

(defn completion-xform [events]
  (fn [rf]
    (swap! events conj :xform-apply)
    (fn
      ([acc]
       (swap! events conj [:complete acc])
       (rf acc :complete))
      ([acc item]
       (swap! events conj [:step item])
       (rf acc item)))))

(defn changing-accumulator-xform [events]
  (fn [rf]
    (let [sink (atom nil)]
      (fn
        ([acc]
         (swap! events conj [:complete acc])
         (rf (or @sink acc) :complete))
        ([acc item]
         (when-not @sink
           (reset! sink acc))
         (swap! events conj [:step acc item])
         (rf @sink item)
         {:last item})))))

(defn stopping-xform [events stop]
  (fn [rf]
    (let [sink (atom nil)]
      (fn
        ([acc]
         (swap! events conj [:complete acc])
         (rf (or @sink acc) :complete))
        ([acc item]
         (when-not @sink
           (reset! sink acc))
         (swap! events conj [:step item])
         (rf @sink item)
         (if (= stop item)
           (reduced {:terminal item})
           {:last item}))))))

(deftest values-match-sequence-across-object-sources
  (let [source-builders
        [(constantly nil)
         (constantly [])
         #(list 0 1 2 3 4)
         #(vec (range 5))
         #(subvec (vec (range 7)) 1 6)
         #(range 5)
         #(hash-set 0 1 2 3 4)
         #(object-array (range 5))
         #(iterator-seq (.iterator ^java.lang.Iterable (java.util.ArrayList. [0 1 2 3 4])))]
        xforms [(map inc)
                (filter even?)
                (comp (map inc) (filter even?))
                (comp (mapcat #(list % (* 2 %))) (take 7))]]
    (doseq [build source-builders
            xform xforms]
      (is (= (vec (sequence xform (build)))
             (vec (core/xf-seq xform (build))))))))

(deftest construction-and-completion-traces
  (testing "construction does not touch source or transducer"
    (let [events (atom [])
          result (core/xf-seq (completion-xform events)
                              (traced-source events [1 2]))]
      (is (instance? LazySeq result))
      (is (= [] @events))
      (is (= 1 (first result)))
      (is (= [:source-seq :xform-apply :step :step]
             (mapv event-kind @events)))
      (is (= [1 2 :complete] (vec result)))
      (is (= 1 (count (filter #(= :complete (event-kind %)) @events))))
      (is (= [1 2 :complete] (vec result)))
      (is (= 1 (count (filter #(= :xform-apply (event-kind %)) @events))))
      (is (= 1 (count (filter #(= :complete (event-kind %)) @events)))))))

(deftest empty-input-completes
  (let [events (atom [])
        result (core/xf-seq (completion-xform events) [])]
    (is (instance? LazySeq result))
    (is (= [] @events))
    (is (= [:complete] (vec (map event-kind (seq result)))))
    (is (= [:xform-apply :complete] (mapv event-kind @events)))
    (is (= [:complete] (vec result)))
    (is (= 1 (count (filter #(= :complete (event-kind %)) @events))))))

(deftest returned-accumulator-is-carried-forward
  (let [events (atom [])
        result (core/xf-seq (changing-accumulator-xform events) (list 1 2))
        values (vec result)
        steps (filter #(= :step (event-kind %)) @events)
        completion (some #(when (= :complete (event-kind %)) %) @events)]
    (is (= [1 2 :complete] values))
    (is (= 2 (count steps)))
    (is (not (map? (second (first steps)))))
    (is (= {:last 1} (second (second steps))))
    (is (= {:last 2} (second completion)))))

(deftest reduced-stops-before-next-input-and-completes
  (let [events (atom [])
        result (core/xf-seq (stopping-xform events 3) (vec (range 64)))
        values (vec result)
        steps (filter #(= :step (event-kind %)) @events)
        completion (some #(when (= :complete (event-kind %)) %) @events)]
    (is (= (conj (vec (range 4)) :complete) values))
    (is (= 4 (count steps)))
    (is (= [:step 3] (last steps)))
    (is (= {:terminal 3} (second completion)))
    (is (= 1 (count (filter #(= :complete (event-kind %)) @events))))
    (is (= values (vec result)))))

(deftest chunked-and-dechunked-realization
  (testing "a chunked source is processed a chunk at a time"
    (let [calls (atom 0)
          xform (fn [rf]
                  (fn
                    ([acc] (rf acc))
                    ([acc item]
                     (swap! calls inc)
                     (rf acc item))))
          result (core/xf-seq xform (vec (range 64)))]
      (is (= 0 @calls))
      (is (= 0 (first result)))
      (is (= 32 @calls))
      (is (= 31 (nth result 31)))
      (is (= 32 @calls))
      (is (= 63 (nth result 63)))
      (is (= 64 @calls))))
  (testing "a dechunked source stops after its first output"
    (let [calls (atom 0)
          xform (fn [rf]
                  (fn
                    ([acc] (rf acc))
                    ([acc item]
                     (swap! calls inc)
                     (rf acc item))))
          result (core/xf-seq xform (list 1 2 3))]
      (is (= 1 (first result)))
      (is (= 1 @calls))))
  (testing "dechunked filtering crosses rejected inputs"
    (let [calls (atom [])
          xform (fn [rf]
                  (fn
                    ([acc] (rf acc))
                    ([acc item]
                     (swap! calls conj item)
                     (if (= item 3) (rf acc item) acc))))
          result (core/xf-seq xform (list 1 2 3 4))]
      (is (= 3 (first result)))
      (is (= [1 2 3] @calls)))))

(deftest mixed-tail-source-is-safe
  (let [first-chunk (object-array [1 2])
        chunked-tail (ChunkedCons. (ArrayChunk. first-chunk 0 2)
                                   (list 3 4))
        xform (fn [rf]
                (fn
                  ([acc] (rf acc))
                  ([acc item] (rf acc item))))]
    (is (= [0 1 2 3 4]
           (vec (core/xf-seq xform (cons 0 chunked-tail)))))
    (is (= [1 2 3 4]
           (vec (core/xf-seq xform chunked-tail))))))

(deftest expansion-keeps-output-ordered-and-bounded
  (let [xform (fn [rf]
                (fn
                  ([acc] (rf acc))
                  ([acc _item]
                   (loop [acc acc
                          item 0]
                     (if (= item 1000)
                       acc
                       (recur (rf acc item) (inc item)))))))
        result (core/xf-seq xform (list :input))
        values (vec result)]
    (is (= 1000 (count values)))
    (is (= (vec (range 1000)) values))
    (is (every? #(<= % 32) (chunk-sizes result)))))

(deftest object-buffer-owns-no-exposed-working-array
  (let [buffer (ObjectBuffer.)]
    (dotimes [item 100]
      (.invoke buffer buffer item))
    (let [tail (.toTail buffer)]
      (is (= (vec (range 100)) (vec tail)))
      (is (every? #(<= % 32) (chunk-sizes tail)))
      (dotimes [item 3]
        (.invoke buffer buffer (+ 100 item)))
      (is (= (vec (range 100)) (vec tail)))
      (is (= 3 (int (private-field buffer "idx"))))
      (is (<= (alength ^objects (private-field buffer "arr")) 32))))
  (let [buffer (ObjectBuffer.)]
    (.invoke buffer buffer :value)
    (let [tail (.toTail buffer)]
      (is (= [:value] (vec tail)))
      (is (nil? (aget ^objects (private-field buffer "arr") 0))))))

(deftest standard-lazy-seq-surface-and-caching
  (let [calls (atom 0)
        result (core/xf-seq (core/map (fn [item]
                                        (swap! calls inc)
                                        (inc item)))
                            (list 1 2 3))]
    (is (instance? LazySeq result))
    (is (seq? result))
    (is (sequential? result))
    (is (= 2 (first result)))
    (is (= 2 (first result)))
    (is (= 1 @calls))
    (is (= [2 3 4] (vec result)))
    (is (= 3 @calls))
    (is (= 3 (nth result 1)))
    (is (= 3 (count result)))
    (is (= [2 3 4] (into [] result)))
    (is (= 9 (reduce + result)))
    (is (= result (list 2 3 4)))
    (is (= (hash result) (hash (list 2 3 4))))
    (is (= "(2 3 4)" (pr-str result)))
    (is (= [2 3 4]
           (vec (iterator-seq (.iterator ^java.lang.Iterable result)))))))

(deftest source-and-xform-exception-order
  (testing "source seq is observed before xform initialization"
    (let [events (atom [])
          source (reify Seqable
                   (seq [_]
                     (swap! events conj :source-seq)
                     (throw (IllegalStateException. "source"))))
          xform (fn [_]
                  (swap! events conj :xform-apply)
                  (throw (IllegalArgumentException. "xform")))
          result (core/xf-seq xform source)]
      (is (= [] @events))
      (is (thrown? IllegalStateException (seq result)))
      (is (= [:source-seq] @events))))
  (testing "xform initialization happens at first realization"
    (let [events (atom [])
          source (traced-source events [1])
          xform (fn [_]
                  (swap! events conj :xform-apply)
                  (throw (IllegalArgumentException. "xform")))
          result (core/xf-seq xform source)]
      (is (= [] @events))
      (is (thrown? IllegalArgumentException (seq result)))
      (is (= [:source-seq :xform-apply] @events))))
  (testing "a source tail exception occurs when that tail is demanded"
    (let [events (atom [])
          source (lazy-seq
                   (do
                     (swap! events conj :source-seq)
                     (cons :item
                           (lazy-seq
                             (swap! events conj :source-next)
                             (throw (IllegalStateException. "next"))))))
          xform (fn [rf]
                  (fn
                    ([acc] (rf acc))
                    ([acc item] (rf acc item))))
          result (core/xf-seq xform source)]
      (is (= [] @events))
      (is (= :item (first result)))
      (is (= [:source-seq] @events))
      (is (thrown? IllegalStateException (next result)))
      (is (= [:source-seq :source-next] @events)))))

(deftest step-and-completion-exceptions-follow-lazy-retry
  (testing "a failed dechunked step is retried at the failed item"
    (let [events (atom [])
          xform (fn [rf]
                  (fn
                    ([acc] (rf acc))
                    ([acc item]
                     (swap! events conj [:step item])
                     (if (= item 2)
                       (throw (IllegalStateException. "step"))
                       (rf acc item)))))
          result (core/xf-seq xform (list 1 2))]
      (is (= 1 (first result)))
      (is (thrown? IllegalStateException (seq (next result))))
      (is (= [[:step 1] [:step 2]] @events))
      (is (thrown? IllegalStateException (seq (next result))))
      (is (= [[:step 1] [:step 2] [:step 2]] @events))))
  (testing "a failed initial completion remains retryable"
    (let [events (atom [])
          failures (atom 0)
          xform (fn [rf]
                  (fn
                    ([acc]
                     (swap! events conj :complete)
                     (if (= 1 (swap! failures inc))
                       (throw (IllegalStateException. "complete"))
                       (rf acc :complete)))
                    ([acc item] (rf acc item))))
          result (core/xf-seq xform [])]
      (is (thrown? IllegalStateException (seq result)))
      (is (= [:complete] @events))
      (is (= [:complete] (vec result)))
      (is (= [:complete :complete] @events))))
  (testing "a failed chunk step resumes without duplicating buffered inputs"
    (let [events (atom [])
          failures (atom 0)
          xform (fn [rf]
                  (fn
                    ([acc] (rf acc))
                    ([acc item]
                     (swap! events conj item)
                     (if (and (= item 34)
                              (= 1 (swap! failures inc)))
                       (throw (IllegalStateException. "chunk step"))
                       (rf acc item)))))
          result (core/xf-seq xform (vec (range 70)))]
      (is (= 0 (first result)))
      (is (thrown? IllegalStateException (nth result 32)))
      (is (= (vec (range 32 35)) (subvec (vec @events) 32)))
      (is (= (vec (range 70)) (vec result)))
      (is (= (vec (range 32 35)) (subvec (vec @events) 32 35)))
      (is (= (vec (range 34 70)) (subvec (vec @events) 35))))))

(deftest same-node-realization-is-cached
  (let [calls (atom 0)
        result (core/xf-seq (core/map (fn [item]
                                        (swap! calls inc)
                                        item))
                            (list 1 2 3))
        [values errors] (concurrently #(first result))]
    (is (= [1 1] (sort values)))
    (is (empty? errors))
    (is (= 1 @calls))
    (let [[values errors] (concurrently #(nth result 1))]
      (is (= [2 2] (sort values)))
      (is (empty? errors)))
    (is (= 2 @calls))
    (is (= [1 2 3] (vec result)))))
