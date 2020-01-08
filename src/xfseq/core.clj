(ns xfseq.core
  (:refer-clojure :exclude [map])
  (:require
    [clojure.core :as clj.core]
    [clojure.walk :as walk])
  (:import [xfseq XFSeq]
           [clojure.lang Numbers]))

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

(defn xf-seq
  [xf coll]
  (XFSeq/create xf coll))

(defn map
  ([f]
   (fn
     ([]
      (get-in (analyze-primitive-interface (bases (class f))) [1 :return]))
     ([rf]
      ;; TODO: Look at how scepter creates functions on the fly
      (map:eval-type-hinted-xf f rf))))
  ([f coll]
   (xf-seq (map f) coll)))

(comment

  ;; Currently returns incorrectly:
  ;; (2 3 nil)
  (map (fn ^long [^long i] (Numbers/add i (long 1))) (range (long 1e5)))

  )