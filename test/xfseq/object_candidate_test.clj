(ns xfseq.object-candidate-test
  (:require [clojure.test :refer [deftest is testing]]
            [xfseq.phase-2-candidates :as candidates])
  (:import [clojure.lang ArrayChunk ChunkedCons IFn]
           [xfseq XFSeqStep$ObjectStep XFSeqStepChunkedOnly
            XFSeqStepChunkedOnlyNoReduced XFSeqStepSimple
            XFSeqStepSimpleNoReduced XFSeqStepSingleOnly
            XFSeqStepSingleOnlyNoReduced]))

(set! *warn-on-reflection* true)

(defn mixed-source []
  (cons 0
        (ChunkedCons. (ArrayChunk. (object-array [1 2 3]) 0 3)
                      (list 4 5))))

(defn source-for [source-mode]
  (case source-mode
    :mixed (mixed-source)
    :dechunked (list 0 1 2 3 4)
    :chunked (vec [0 1 2 3 4])))

(defn- sized-source [source-mode n]
  (case source-mode
    :dechunked (apply list (range n))
    :chunked (vec (range n))
    :mixed-dc
    (cond
      (zero? n) nil
      (= 1 n) (list 0)
      :else (cons 0
                  (ChunkedCons.
                    (ArrayChunk. (object-array (range 1 n)) 0 (dec n))
                    nil)))
    :mixed-cd
    (cond
      (zero? n) nil
      :else (ChunkedCons.
              (ArrayChunk. (object-array [0]) 0 1)
              (when (> n 1) (apply list (range 1 n)))))))

(def boundary-sizes
  [0 1 2 3 4 5 7 8 9 31 32 33 63 64 65 1000])

(defn completion-marker-xform [rf]
  (fn
    ([acc] (rf acc :complete))
    ([acc item] (rf acc item))))

(def differential-transforms
  [{:label :one-to-one
    :build (fn [] (map inc))}
   {:label :filtering
    :build (fn [] (filter even?))}
   {:label :keep
    :build (fn [] (keep (fn [value]
                          (when (even? value) (* 10 value)))))}
   {:label :early-reduction
    :build (fn [] (take 7))}
   {:label :stateful
    :build (fn [] (map-indexed (fn [index value] [index value])))}
   {:label :expanding
    :build (fn [] (mapcat (fn [value] [value (* 2 value)])))}
   {:label :completion-emitting
    :build (fn [] completion-marker-xform)}
   {:label :composed
    :build (fn []
             (comp (map inc)
                   (filter even?)
                   (mapcat (fn [value] [value (* 2 value)]))
                   (take 11)))}])

(defn completion-xform [events]
  (fn [rf]
    (fn
      ([acc]
       (swap! events conj [:complete acc])
       (rf acc :complete))
      ([acc item]
       (swap! events conj [:step item acc])
       (rf acc item)))))

(defn stopping-xform [stop]
  (fn [rf]
    (fn
      ([acc] (rf acc :complete))
      ([acc item]
       (let [next (rf acc item)]
         (if (= stop item)
           (reduced next)
           next))))))

(defn retrying-filter-xform [events]
  (let [failed? (atom false)]
    (fn [rf]
      (fn
        ([acc] (rf acc :complete))
        ([acc item]
         (swap! events conj item)
         (if (< item 2)
           acc
           (if (and (= item 2) (compare-and-set! failed? false true))
             (throw (IllegalStateException. "candidate step"))
             (rf acc item))))))))

(defn reduced-completion-retry-xform [events]
  (let [completion-failures (atom 0)]
    (fn [rf]
      (fn
        ([acc]
         (swap! events conj :complete)
         (if (= 1 (swap! completion-failures inc))
           (throw (IllegalStateException. "candidate completion"))
           (rf acc :complete)))
        ([acc item]
         (swap! events conj [:step item])
         (if (< item 2)
           acc
           (let [next (rf acc item)]
             (if (= item 2)
               (reduced next)
               next))))))))

(defn expanding-xform [rf]
  (fn
    ([acc] (rf acc))
    ([acc item]
     (loop [acc acc
            n 0]
       (if (= n 1000)
         acc
         (recur (rf acc [item n]) (inc n)))))))

(defn values-from [step]
  (vec (.invoke ^IFn step)))

(defn sequence-from [step]
  (.invoke ^IFn step))

(defn chunk-sizes [value]
  (loop [s (seq value)
         sizes []]
    (if (nil? s)
      sizes
      (if (instance? clojure.lang.IChunkedSeq s)
        (let [chunked ^clojure.lang.IChunkedSeq s
              chunk (.chunkedFirst chunked)]
          (recur (seq (.chunkedMore chunked))
                 (conj sizes (.count chunk))))
        (recur (seq (.more ^clojure.lang.ISeq s))
               (conj sizes 1))))))

(def reduced-aware-ids
  (->> candidates/object-candidate-registry
       (filter :reduced-aware?)
       (mapv :stable-id)))

(def non-reduced-ids
  (->> candidates/object-candidate-registry
       (remove :reduced-aware?)
       (mapv :stable-id)))

(defn- candidate-source-modes [{:keys [source-mode]}]
  (if (= :mixed source-mode)
    [:dechunked :chunked :mixed-dc :mixed-cd]
    [source-mode]))

(defn- oracle-values [transform source-mode n]
  {:sequence (vec (sequence ((:build transform))
                             (sized-source source-mode n)))
   :transduced (transduce ((:build transform)) conj []
                          (sized-source source-mode n))})

(deftest registry-has-explicit-v2-identities
  (is (= 7 (count candidates/object-candidate-registry)))
  (is (= 7 (count (set (candidates/candidate-ids)))))
  (is (every? #(re-find #"-v2$" %) (candidates/candidate-ids)))
  (is (= 7 (count candidates/old-to-new-id)))
  (doseq [{:keys [stable-id historical-id]} candidates/object-candidate-registry]
    (is (= stable-id (get candidates/old-to-new-id historical-id)))
    (is (not= historical-id stable-id))))

(deftest reduced-aware-candidates-differentially-match-fresh-oracles
  (doseq [candidate candidates/object-candidate-registry
          :when (:reduced-aware? candidate)
          source-mode (candidate-source-modes candidate)
          n boundary-sizes
          transform differential-transforms]
    (let [stable-id (:stable-id candidate)
          label (:label transform)
          {:keys [sequence transduced]}
          (oracle-values transform source-mode n)
          result (sequence-from
                   (candidates/instantiate-candidate
                     stable-id
                     ((:build transform))
                     (sized-source source-mode n)))]
      (testing (str stable-id "/" source-mode "/" n "/" (name label))
        (is (= sequence transduced))
        (is (= sequence (vec result)))))))

(deftest mixed-candidates-exercise-both-tail-directions
  (doseq [{:keys [stable-id reduced-aware?] :as candidate}
          candidates/object-candidate-registry
          :when (= :mixed (:source-mode candidate))
          source-mode [:mixed-dc :mixed-cd]]
    (let [xform (if reduced-aware?
                  (fn [rf]
                    (fn
                      ([acc] (rf acc))
                      ([acc item] (rf acc item))))
                  (candidates/non-reducing-operation :identity))
          result (sequence-from
                   (candidates/instantiate-candidate
                     stable-id xform (sized-source source-mode 65)))]
      (testing (str stable-id "/" source-mode)
        (is (= (vec (range 65)) (vec result)))
        (is (every? #(<= % 32) (chunk-sizes result)))))))

(deftest reduced-aware-candidates-share-the-contract
  (doseq [{:keys [stable-id source-mode]} candidates/object-candidate-registry
          :when (get-in candidates/candidate-by-id [stable-id :reduced-aware?])]
    (testing stable-id
      (let [events (atom [])
            step (candidates/instantiate-candidate
                   stable-id
                   (completion-xform events)
                   (source-for source-mode))]
        (is (= (conj (vec (source-for source-mode)) :complete)
               (values-from step)))
        (is (= 1 (count (filter #(= :complete (first %)) @events))))
        (is (= (count (source-for source-mode))
               (count (filter #(= :step (first %)) @events))))))))

(deftest every-candidate-completes-empty-input
  (doseq [{:keys [stable-id reduced-aware?]}
          candidates/object-candidate-registry]
    (testing stable-id
      (let [events (atom [])
            xform (if reduced-aware?
                    (completion-xform events)
                    (candidates/non-reducing-operation
                      :changing-accumulator))
            step (candidates/instantiate-candidate stable-id xform nil)]
        (is (= [:complete] (values-from step)))
        ;; The no-reduced adapter operation has the same completion contract;
        ;; all candidates must flush its completion value on an empty source.
        (if reduced-aware?
          (is (= 1 (count (filter #(= :complete (first %)) @events))))
          (is (= [] @events)))))))

(deftest reduced-aware-candidates-preserve-accumulator-and-reduction
  (doseq [{:keys [stable-id source-mode]} candidates/object-candidate-registry
          :when (get-in candidates/candidate-by-id [stable-id :reduced-aware?])]
    (testing stable-id
      (let [step (candidates/instantiate-candidate
                   stable-id
                   (stopping-xform 2)
                   (source-for source-mode))]
        (is (= [0 1 2 :complete] (values-from step)))))))

(deftest every-candidate-carries-ordinary-accumulators
  (doseq [{:keys [stable-id source-mode reduced-aware?]}
          candidates/object-candidate-registry]
    (testing stable-id
      (let [source-values (vec (source-for source-mode))
            xform (if reduced-aware?
                    (fn [rf]
                      (fn
                        ([acc] (rf acc :complete))
                        ([acc item]
                         (rf acc
                             [(when (map? acc) (:last acc)) item])
                         (assoc {} :last item))))
                    (candidates/non-reducing-operation
                      :changing-accumulator))
            step (candidates/instantiate-candidate
                   stable-id xform (source-for source-mode))]
        (is (= (conj
                 (mapv (fn [[index item]]
                         [(when (pos? index)
                            (nth source-values (dec index)))
                          item])
                       (map-indexed vector source-values))
                 :complete)
               (values-from step)))))))

(deftest every-reduced-aware-candidate-retries-the-failed-input
  (doseq [{:keys [stable-id source-mode]}
          candidates/object-candidate-registry
          :when (get-in candidates/candidate-by-id [stable-id :reduced-aware?])]
    (testing stable-id
      (let [events (atom [])
            step (candidates/instantiate-candidate
                   stable-id
                   (retrying-filter-xform events)
                   (source-for source-mode))]
        (is (thrown? IllegalStateException (.invoke ^IFn step)))
        (let [source-values (vec (source-for source-mode))]
          (is (= (conj (subvec source-values 2) :complete)
                 (values-from step)))
          (is (= (concat (take 2 source-values)
                         [2]
                         (subvec source-values 2))
                 @events)))))))

(deftest reduced-completion-retry-does-not-repeat-terminal-step
  (doseq [{:keys [stable-id source-mode]}
          candidates/object-candidate-registry
          :when (get-in candidates/candidate-by-id [stable-id :reduced-aware?])]
    (testing stable-id
      (let [events (atom [])
            step (candidates/instantiate-candidate
                   stable-id
                   (reduced-completion-retry-xform events)
                   (source-for source-mode))]
        (is (thrown? IllegalStateException (.invoke ^IFn step)))
        (is (= [2 :complete] (values-from step)))
        (is (= [[:step 0] [:step 1] [:step 2] :complete :complete]
               @events))))))

(deftest every-candidate-terminal-invocation-is-idempotent
  (doseq [{:keys [stable-id source-mode reduced-aware?]}
          candidates/object-candidate-registry]
    (let [source-mode (if (= :mixed source-mode) :mixed-dc source-mode)
          events (atom [])
          xform (if reduced-aware?
                  (completion-xform events)
                  (candidates/non-reducing-operation :completion events))
          step (candidates/instantiate-candidate
                 stable-id xform (sized-source source-mode 5))
          values (values-from step)
          completion? (fn [event]
                        (or (= :complete event)
                            (and (vector? event)
                                 (= :complete (first event)))))
          step? (fn [event]
                  (and (vector? event) (= :step (first event))))]
      (testing (str stable-id "/" source-mode)
        (is (= [0 1 2 3 4 :complete] values))
        (is (nil? (.invoke ^IFn step)))
        (is (= 1 (count (filter completion? @events))))
        (is (= 5 (count (filter step? @events))))))))

(deftest every-candidate-keeps-expanded-output-bounded
  (doseq [{:keys [stable-id source-mode]}
          candidates/object-candidate-registry]
    (testing stable-id
      (let [reduced-aware? (get-in candidates/candidate-by-id
                                   [stable-id :reduced-aware?])
            xform (if reduced-aware?
                    expanding-xform
                    (candidates/non-reducing-operation :identity))
            source (if reduced-aware?
                     (case source-mode
                       :chunked [:input]
                       (list :input))
                     (source-for source-mode))
            step (candidates/instantiate-candidate stable-id xform source)
            result (sequence-from step)
            values (vec result)]
        (if reduced-aware?
          (do
            (is (= 1000 (count values)))
            (is (= (vec (map (fn [n] [:input n]) (range 1000))) values))
            (is (every? #(<= % 32) (chunk-sizes result))))
          (is (= (vec (source-for source-mode)) values)))))))

(deftest no-reduced-candidates-expand-output-order-and-bounds
  (let [expected
        (vec (mapcat (fn [item]
                       [[item :left] [item :right]])
                     (range 1000)))]
    (doseq [{:keys [stable-id source-mode] :as candidate}
            candidates/object-candidate-registry
            :when (not (:reduced-aware? candidate))]
      (let [source-mode (if (= :mixed source-mode) :mixed-cd source-mode)
            result (sequence-from
                     (candidates/instantiate-candidate
                       stable-id
                       (candidates/non-reducing-operation :expanding)
                       (sized-source source-mode 1000)))
            values (vec result)]
        (testing (str stable-id "/" source-mode)
          (is (= expected values))
          (is (= 2000 (count values)))
          (is (every? #(<= % 32) (chunk-sizes result))))))))

(deftest no-reduced-candidates-require-proved-operation
  (doseq [{:keys [stable-id source-mode]} candidates/object-candidate-registry
          :when (not (get-in candidates/candidate-by-id [stable-id :reduced-aware?]))]
    (testing stable-id
      (is (thrown? IllegalArgumentException
                   (candidates/instantiate-candidate
                     stable-id
                     (fn [rf] rf)
                     (source-for source-mode))))
      (is (= (mapv inc (source-for source-mode))
             (values-from
               (candidates/instantiate-candidate
                 stable-id
                 (candidates/non-reducing-operation :map inc)
                 (source-for source-mode)))))
      (is (thrown? IllegalArgumentException
                   (candidates/instantiate-candidate
                     stable-id
                     (candidates/->NonReducingOperation
                       :identity identity (fn [rf] rf) (Object.))
                     (source-for source-mode)))))))

(deftest specialized-adapters-reject-invalid-source-before-xform
  (let [events (atom [])
        xform (fn [rf]
                (swap! events conj :xform-apply)
                rf)]
    (doseq [stable-id ["java-dechunked-object-reduced-aware-v2"
                       "java-dechunked-object-nonreducing-v2"]]
      (is (thrown? IllegalArgumentException
                   (candidates/instantiate-candidate stable-id xform (vec [1 2])))))
    (is (= [] @events)))
  (let [mixed (mixed-source)]
    (doseq [stable-id ["java-dechunked-object-reduced-aware-v2"
                       "java-dechunked-object-nonreducing-v2"]]
      (is (thrown? IllegalArgumentException
                   (candidates/instantiate-candidate
                     stable-id
                     (completion-xform (atom []))
                     mixed))))
    (doseq [stable-id ["java-chunked-object-reduced-aware-v2"
                       "java-chunked-object-nonreducing-v2"]]
      (is (thrown? IllegalArgumentException
                   (candidates/instantiate-candidate
                     stable-id
                     (completion-xform (atom []))
                     (list 1 2)))))))

(deftest candidate-class-identities-remain-visible
  (let [class-by-id
        {"java-polymorphic-object-reduced-aware-v2" XFSeqStep$ObjectStep
         "java-mixed-object-reduced-aware-v2" XFSeqStepSimple
         "java-mixed-object-nonreducing-v2" XFSeqStepSimpleNoReduced
         "java-dechunked-object-reduced-aware-v2" XFSeqStepSingleOnly
         "java-dechunked-object-nonreducing-v2" XFSeqStepSingleOnlyNoReduced
         "java-chunked-object-reduced-aware-v2" XFSeqStepChunkedOnly
         "java-chunked-object-nonreducing-v2" XFSeqStepChunkedOnlyNoReduced}]
    (doseq [{:keys [stable-id source-mode]} candidates/object-candidate-registry]
      (testing stable-id
        (is (instance? (get class-by-id stable-id)
                       (candidates/instantiate-candidate
                         stable-id
                         (if (get-in candidates/candidate-by-id
                                    [stable-id :reduced-aware?])
                           (completion-xform (atom []))
                           (candidates/non-reducing-operation :identity))
                         (source-for source-mode))))))))
