(ns xfseq.phase-2-candidates
  "Test/benchmark-only adapters for the repaired Phase 2 object candidates.

  These adapters are deliberately outside the production source path.  They
  validate source shape before constructing a specialized candidate and make
  the non-reducing proof an explicit value produced by this namespace rather
  than caller metadata."
  (:require [xfseq.analyze :as ana])
  (:import [clojure.lang IChunkedSeq ISeq]
           [xfseq XFSeqStep$ObjectStep XFSeqStepChunkedOnly
            XFSeqStepChunkedOnlyNoReduced XFSeqStepSimple
            XFSeqStepSimpleNoReduced XFSeqStepSingleOnly
            XFSeqStepSingleOnlyNoReduced]
           [xfseq.buffer ObjectBuffer]))

(set! *warn-on-reflection* true)

;; The public #1 functions now delegate their transducer arities to core.  The
;; generated primitive path is retained as historical research code, so its
;; old analyzer-shaped map xform remains available only to tests that exercise
;; that path.  It is intentionally not reachable from xfseq.core/map.
(def ^:private historical-map-xf-factory
  (ana/xf-factory*
    (ana/map:type-analyzer
      '(fn [rf f]
         (fn
           ([] (rf))
           ([acc] (rf acc))
           ([acc item]
            (rf acc (f item))))))))

(defn historical-map-xform
  "Return the analyzer-backed map xform for the preserved generator path."
  [f]
  (with-meta
    (fn [rf]
      (historical-map-xf-factory rf f))
    {:xfseq.core/no-reduced? true
     :xfseq.core/return-hint
     (-> (class f)
         (ana/interfaces)
         (ana/analyze-primitive-interfaces)
         (get-in [1 :return] 'Object))}))

(def object-candidate-registry
  "Stable repaired IDs.  Historical IDs are retained as values, not reused."
  [{:stable-id "java-polymorphic-object-reduced-aware-v2"
    :historical-id "java-polymorphic-object-identity-stop"
    :source-class "xfseq.XFSeqStep$ObjectStep"
    :constructor :polymorphic
    :source-mode :mixed
    :reduced-aware? true}
   {:stable-id "java-mixed-object-reduced-aware-v2"
    :historical-id "java-mixed-object-identity-stop"
    :source-class "xfseq.XFSeqStepSimple"
    :constructor :mixed
    :source-mode :mixed
    :reduced-aware? true}
   {:stable-id "java-mixed-object-nonreducing-v2"
    :historical-id "java-mixed-object-no-stop"
    :source-class "xfseq.XFSeqStepSimpleNoReduced"
    :constructor :mixed-no-reduced
    :source-mode :mixed
    :reduced-aware? false
    :requires-non-reducing-proof? true}
   {:stable-id "java-dechunked-object-reduced-aware-v2"
    :historical-id "java-dechunked-object-identity-stop"
    :source-class "xfseq.XFSeqStepSingleOnly"
    :constructor :dechunked
    :source-mode :dechunked
    :reduced-aware? true}
   {:stable-id "java-dechunked-object-nonreducing-v2"
    :historical-id "java-dechunked-object-no-stop"
    :source-class "xfseq.XFSeqStepSingleOnlyNoReduced"
    :constructor :dechunked-no-reduced
    :source-mode :dechunked
    :reduced-aware? false
    :requires-non-reducing-proof? true}
   {:stable-id "java-chunked-object-reduced-aware-v2"
    :historical-id "java-chunked-object-identity-stop"
    :source-class "xfseq.XFSeqStepChunkedOnly"
    :constructor :chunked
    :source-mode :chunked
    :reduced-aware? true}
   {:stable-id "java-chunked-object-nonreducing-v2"
    :historical-id "java-chunked-object-no-stop"
    :source-class "xfseq.XFSeqStepChunkedOnlyNoReduced"
    :constructor :chunked-no-reduced
    :source-mode :chunked
    :reduced-aware? false
    :requires-non-reducing-proof? true}])

(def old-to-new-id
  "Explicit Phase 0 to repaired-candidate mapping."
  (into {}
    (map (juxt :historical-id :stable-id) object-candidate-registry)))

(def candidate-by-id
  (into {} (map (juxt :stable-id identity) object-candidate-registry)))

(def ^:private non-reducing-proof-token (Object.))

(defrecord NonReducingOperation [operation f xform proof-token])

(defn- passthrough-xform [rf]
  (fn
    ([acc] (rf acc))
    ([acc item] (rf acc item))))

(defn- mapped-xform [rf f]
  (fn
    ([acc] (rf acc))
    ([acc item] (rf acc (f item)))))

(defn- filtered-xform [rf pred]
  (fn
    ([acc] (rf acc))
    ([acc item] (if (pred item) (rf acc item) acc))))

(defn- changing-accumulator-xform [rf]
  (fn
    ([acc] (rf acc :complete))
    ([acc item]
     (rf acc [(when (map? acc) (:last acc)) item])
     (assoc {} :last item))))

(defn- expanding-xform [rf]
  (fn
    ([acc] (rf acc))
    ([acc item]
     (let [acc (rf acc [item :left])]
       (rf acc [item :right])))))

(defn- completion-counting-xform [events]
  (fn [rf]
    (fn
      ([acc]
       (swap! events conj :complete)
       (rf acc :complete))
      ([acc item]
       (swap! events conj [:step item])
       (rf acc item)))))

(defn non-reducing-operation
  "Create one of the adapter's structurally non-reducing transforms.

  The adapter owns these implementations and applies them only to its own
  ObjectBuffer.  There is intentionally no general `mark-non-reducing`
  escape hatch for arbitrary caller transducers."
  ([operation]
   (non-reducing-operation operation identity))
  ([operation f]
   (let [xform
         (case operation
           :identity passthrough-xform
           :map (fn [rf] (mapped-xform rf f))
           :filter (fn [rf] (filtered-xform rf f))
           :changing-accumulator changing-accumulator-xform
           :expanding expanding-xform
           :completion (completion-counting-xform f)
           (throw (IllegalArgumentException.
                    (str "Unsupported non-reducing operation: " operation))))]
     (->NonReducingOperation operation f xform non-reducing-proof-token))))

(defn- source-shape-error [expected source]
  (IllegalArgumentException.
    (str "Expected " (name expected) " object source, got "
         (.getName ^Class (class source)))))

(defn- validate-source-shape!
  "Walk a finite test/benchmark source and reject specialized shape errors.

  The walk is intentionally an adapter concern.  Production `xf-seq` never
  calls this function and therefore never eagerly classifies a source."
  [shape source]
  (if (= :mixed shape)
    source
    (do
      (loop [s (seq source)]
        (when s
          (let [chunked? (instance? IChunkedSeq s)]
            (case shape
              :dechunked (when chunked?
                           (throw (source-shape-error shape s)))
              :chunked (when-not chunked?
                         (throw (source-shape-error shape s)))
              :mixed nil)
            (let [next (if chunked?
                         (.chunkedMore ^IChunkedSeq s)
                         (.more ^ISeq s))]
              (recur (when next (.seq ^ISeq next)))))))
      source)))

(defn- candidate! [stable-id]
  (or (get candidate-by-id stable-id)
      (throw (IllegalArgumentException.
               (str "Unknown Phase 2 object candidate: " stable-id)))))

(defn- reducing-function [candidate xform buffer]
  (when (and (:requires-non-reducing-proof? candidate)
             (not (and (instance? NonReducingOperation xform)
                       (identical? non-reducing-proof-token
                                   (:proof-token ^NonReducingOperation xform)))))
    (throw (IllegalArgumentException.
             (str "Candidate " (:stable-id candidate)
                  " requires a structurally non-reducing adapter operation"))))
  ((if (and (instance? NonReducingOperation xform)
            (identical? non-reducing-proof-token
                        (:proof-token ^NonReducingOperation xform)))
     (:xform ^NonReducingOperation xform)
     xform)
   buffer))

(defn- construct [constructor buffer reducing-fn source]
  (case constructor
    :polymorphic (XFSeqStep$ObjectStep. reducing-fn source buffer)
    :mixed (XFSeqStepSimple. buffer reducing-fn source)
    :mixed-no-reduced (XFSeqStepSimpleNoReduced. buffer reducing-fn source)
    :dechunked (XFSeqStepSingleOnly. buffer reducing-fn source)
    :dechunked-no-reduced
    (XFSeqStepSingleOnlyNoReduced. buffer reducing-fn source)
    :chunked (XFSeqStepChunkedOnly. buffer reducing-fn source)
    :chunked-no-reduced
    (XFSeqStepChunkedOnlyNoReduced. buffer reducing-fn source)))

(defn instantiate-candidate
  "Instantiate a repaired object candidate for a fresh source fixture.

  Specialized source validation happens before the candidate can cast a node;
  no-reduced candidates additionally require a NonReducingOperation created by
  `non-reducing-operation`."
  [stable-id xform source]
  (let [{:keys [constructor source-mode] :as candidate} (candidate! stable-id)
        source (seq source)]
    (validate-source-shape! source-mode source)
    (let [buffer (ObjectBuffer.)
          reducing-fn (reducing-function candidate xform buffer)]
      (construct constructor buffer reducing-fn source))))

(defn candidate-ids []
  (mapv :stable-id object-candidate-registry))
