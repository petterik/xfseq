(ns xfseq.core
  (:refer-clojure :exclude [map])
  (:require
    [clojure.core :as clj.core]
    [clojure.walk :as walk])
  (:import [xfseq ILongSeq XFSeqStep$LongStep IDoubleSeq XFSeqStep$DoubleStep XFSeqStep$ObjectStep]
           [clojure.lang Numbers]
           [xfseq.buffer LongBuffer DoubleBuffer ObjectBuffer]))

(defprotocol IDeconstruct
  (deconstruct! [this]
    "Safely returns its parts if they haven't been used
     already, rendering the object unusable.

     Returns nil if they're already been used.

     This is really only meant to be used with xfseq.core/consume."))

;;;;;;;;;;;;;;;;;;;
;; XFSeq creation
;;

(defonce ^:private deconstructed (Object.))

(deftype InitXFSeq [xf coll]
  clojure.lang.IFn
  (invoke [this]
    (when-some [s (seq coll)]
      (let [buf (case (::return-hint (meta xf))
                  long (LongBuffer.)
                  double (DoubleBuffer.)
                  (ObjectBuffer.))

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

(defn- analyze-primitive-interface [x-bases]
  (into {}
    (comp
      (clj.core/keep #(let [cn (.getCanonicalName ^Class %)]
                        (when (.startsWith cn "clojure.lang.IFn.")
                          [% cn])))
      (clj.core/map
        (fn [[^Class klass ^String canonical-name]]
          (let [interface-name (subs canonical-name (inc (.lastIndexOf canonical-name ".")))
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
    x-bases))

(def type-hint->letter {'double "D"
                        'long   "L"
                        'Object "O"})

(defn hint-map->interface [{:keys [return args] :as m}]
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

(defn apply-type-hints [types replacement-map xf-body]
  ;; TODO: Apply the type-hints to an xf-body.
  ;;       this will be used for all(?) xf's.
  (let [apply-hints (fn [[arg-list & body]]
                      (let [arg-count (count arg-list)
                            typed-args (into []
                                         (map-indexed
                                           (fn [idx sym]
                                             (vary-meta sym assoc :tag (get-in types [arg-count :args idx]))))
                                         arg-list)]
                        (cons
                          (vary-meta typed-args assoc :tag (get-in types [arg-count :return]))
                          (cond->> body
                            (== 2 arg-count)
                            (walk/postwalk
                              (fn [x]
                                (if (or (list? x) (seq? x))
                                  (if-some [[sym replacement] (find replacement-map (first x))]
                                    (let [interface-name (hint-map->interface replacement)]
                                      (assert (== (:arity replacement) (dec (count x))))
                                      ;; TODO: Invent what goes in a replacement map.
                                      ;; TODO: The idea is that both rf and f needs to be replaced with .invokePrim and
                                      ;;       their correct typed arguments and clojure.lang.IFn$<> interfaces.
                                      (list*
                                        (if (is-primitive? interface-name)
                                          '.invokePrim
                                          '.invoke)
                                        (vary-meta sym assoc :tag interface-name)
                                        (rest x)))
                                    x)
                                  x)))))))]
    (->> xf-body
      (clj.core/map
        (fn [inner]
          (if (or (list? inner) (seq? inner))
            (clj.core/map
              (fn [x]
                (if (coll? x)
                  (apply-hints x)
                  x))
              inner)
            inner))))))

(defn map:type-analyzer [xf-body]
  (fn [f-bases rf-bases]
    (let [ana-f (analyze-primitive-interface f-bases)
          ana-rf (analyze-primitive-interface rf-bases)
          replacements {'f  (get ana-f 1 {:arity  1
                                          :args   '[Object]
                                          :return 'Object})
                        'rf (-> ana-rf
                              (get 2 {:arity  2
                                      :args   '[Object Object]
                                      :return 'Object})
                              (assoc-in [:args 1] (get-in ana-f [1 :return] 'Object)))}]
      (apply-type-hints
        ;; map's arity-2 arg1 should have the same type as the f's arg0.
        (assoc-in ana-rf [2 :args 1] (get-in ana-f [1 :args 0]))
        replacements
        xf-body))))

(def map:xf-analyzer
  (map:type-analyzer
    '(fn [f rf]
       (fn
         ([] (rf))
         ([acc] (rf acc))
         ([acc item]
          (rf acc (f item)))))))

(def map:eval-type-hinted-xf*
  (memoize
    (comp
      eval
      map:xf-analyzer)))

(defn map:eval-type-hinted-xf [f rf]
  (let [new-fn (map:eval-type-hinted-xf*
                 (bases (class f))
                 (bases (class rf)))]
    (new-fn f rf)))

;;;;;;;;;;;;;;;;
;; Transducers
;;

(defn map
  ([f]
   ^{::return-hint (get-in (analyze-primitive-interface (bases (class f))) [1 :return])}
   (fn
     ([rf]
      ;; TODO: Look at how scepter creates functions on the fly
      (map:eval-type-hinted-xf f rf))))
  ([f coll]
   (xf-seq (map f) coll)))

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

(comment

  ;; Currently returns incorrectly:
  ;; (2 3 nil)
  (map (fn ^long [^long i] (Numbers/add i (long 1))) (range (long 1e5)))

  (def long-add (fn ^long [^long l] (clojure.lang.Numbers/add l 1)))

  (->> (repeat (long 1e5) 1)
    (map long-add)
    (map long-add)
    (map long-add)
    (map long-add)
    (map long-add)
    (consume (fn ^long [^long acc ^long i]
               (clojure.lang.Numbers/add acc i))
      0)
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
  )