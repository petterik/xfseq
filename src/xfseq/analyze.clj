(ns xfseq.analyze
  (:require
    [clojure.set :as set]
    [clojure.walk :as walk]))

;;;;;;;;;;;;;;;;;;;
;; Type analyzing
;;

(defn interfaces [^Class c]
  (when c
    (seq (.getInterfaces c))))

(defn analyze-primitive-interfaces*
  [interfaces]
  (into {}
    (comp
      (filter
        (fn [^Class interface]
          (.startsWith (.getCanonicalName interface) "clojure.lang.IFn.")))
      (map
        (fn [^Class klass]
          (let [canonical-name (.getCanonicalName klass)
                interface-name (subs canonical-name (inc (.lastIndexOf canonical-name ".")))
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
      (map (juxt :arity identity)))
    interfaces))

(def analyze-primitive-interfaces
  "Given a set of interfaces, datafies the primitive IFn interfaces
  and returns them in a map by arity."
  (memoize analyze-primitive-interfaces*))

(def type-hint->letter {'double "D"
                        'long   "L"
                        'Object "O"})

(defn hint-map->interface
  "Takes a datafied description of an interface and returns
  the interface name matching the description as a symbol."
  [{:keys [return args]}]
  (if (every? #{'Object} (cons return args))
    (symbol "clojure.lang.IFn")
    (symbol
      (format "clojure.lang.IFn$%s"
        (apply str
          (concat
            (map type-hint->letter args)
            [(type-hint->letter return)]))))))

(defn is-primitive? [interface-name]
  (not= "clojure.lang.IFn" (name interface-name)))

(defn- f-replacement [ana-f]
  {'f (get ana-f 1 {:arity  1
                    :args   '[Object]
                    :return 'Object})})

(defn- rf-replacement [ana-rf]
  {'rf (get ana-rf 2 {:arity  2
                      :args   '[Object Object]
                      :return 'Object})})

(defn apply-type-hints
  "Given datafied rf and an xf-body, return the body with
  applied type hints from the datafication as well as
  primitive function invocations where possible.

  Takes a replacement-map with {sym type-analyze} which is
  used to replace function invocations with primitive
  invocations.

  This replacement is only applied to the xf-body with arity-2."
  ([ana-rf xf-body replacement-map]
   (let [apply-hints (fn [[arg-list & body]]
                       (let [arg-count (count arg-list)
                             typed-args (into []
                                          (map-indexed
                                            (fn [idx sym]
                                              (vary-meta sym assoc :tag (get-in ana-rf [arg-count :args idx]))))
                                          arg-list)]
                         (cons
                           (vary-meta typed-args assoc :tag (get-in ana-rf [arg-count :return]))
                           (cond->> body
                             (== 2 arg-count)
                             (walk/postwalk
                               (fn [x]
                                 (if (or (list? x) (seq? x))
                                   (if-some [[sym replacement] (find replacement-map (first x))]
                                     (let [interface-name (hint-map->interface replacement)]
                                       (assert (== (:arity replacement) (dec (count x))))
                                       (list*
                                         (if (is-primitive? interface-name)
                                           '.invokePrim
                                           '.invoke)
                                         (vary-meta sym assoc :tag interface-name)
                                         (rest x)))
                                     x)
                                   x)))))))]
     (map
       (fn [inner]
         (walk/postwalk
           (fn [x]
             (if (and (coll? x) (= 'fn (first x)))
               (cons (first x)
                 (map apply-hints (rest x)))
               x))
           inner))
       xf-body))))

(defn rf-type-analyzer
  "Analyzer for xf-bodies that only needs to type hint the rf."
  [xf-body]
  (fn [rf-bases]
    (let [ana-rf (analyze-primitive-interfaces rf-bases)]
      (apply-type-hints
        ana-rf
        xf-body
        (rf-replacement ana-rf)))))

(defn rf+f-type-analyzer
  "Analyzer for xf-bodies that type hints both rf and f."
  [xf-body f-sym]
  (fn [rf-bases f-bases]
    (let [ana-rf (analyze-primitive-interfaces rf-bases)]
      (apply-type-hints
        ana-rf
        xf-body
        (merge
          (rf-replacement ana-rf)
          (set/rename-keys
            (f-replacement (analyze-primitive-interfaces f-bases))
            {'f f-sym}))))))

(defn xf-factory*
  [analyzer]
  (let [analyze+eval (memoize (comp eval analyzer))]
    ;; These args are the same number of arguments as the ones in
    ;; the xf-body
    (fn [& args]
      (let [new-xf (apply analyze+eval
                     (map (comp interfaces class) args))]
        (apply new-xf args)))))

(defn xf-factory
  "Takes a body of an xf and returns a function that takes a
  reducing function and returns a transducer.

  The analyzing and evaluation is memoized based on the interfaces
  of the rf."
  [xf-body]
  (let [args (-> xf-body second)
        _ (assert (vector? args))
        analyzer (case (count args)
                   1 (rf-type-analyzer xf-body)
                   2 (rf+f-type-analyzer xf-body (second args)))]
    (xf-factory* analyzer)))


;;;;;;;;;;;;;;;;
;; map helper
;;

(defn map:type-analyzer
  "Updates the applied reducing function hints with the types
  of the mapping function."
  [xf-body]
  (fn [rf-bases f-bases]
    (let [ana-f (analyze-primitive-interfaces f-bases)
          ana-rf (analyze-primitive-interfaces rf-bases)
          replacements (-> (merge
                             (f-replacement ana-f)
                             (rf-replacement ana-rf))
                         ;; setting the type of the rf's second argument to the
                         ;; type of f's return value.
                         (assoc-in ['rf :args 1] (get-in ana-f [1 :return] 'Object)))]
      (apply-type-hints
        ;; map's arity-2 arg1 should have the same type as the f's arg0.
        (assoc-in ana-rf [2 :args 1] (get-in ana-f [1 :args 0]))
        xf-body
        replacements))))

