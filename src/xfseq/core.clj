(ns xfseq.core
  (:refer-clojure :exclude [map])
  (:require
    [clojure.core :as clj.core]
    [clojure.walk :as walk])
  (:import [xfseq XFSeq]
           [clojure.lang Numbers]))

(defn xf-seq
  [xf coll]
  (XFSeq/create xf coll))

(defn- analyze-primitive-interface [x]
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
    ;; TODO: Get the union from the set of all the possible classes instead of
    ;;       iterating through all the supers.
    (supers (class x))))

(comment
  (fn [^clojure.lang.IFn$LL f]
    (let [^clojure.lang.IFn$LL f f]
      (fn [rf]
        (fn
          ([] (rf))
          ([acc] (rf acc))
          ([acc ^long item]
           (rf acc (f item)))))))
  )

(def type-hint->letter {'double "D"
                        'long   "L"
                        'Object "O"})

(defn hint-map->interface [{:keys [return args] :as m}]
  (doto
    (if (every? #{'Object} (cons return args))
      (symbol "clojure.lang.IFn")
      (symbol
        (format "clojure.lang.IFn$%s"
          (apply str
            (concat
              (clj.core/map type-hint->letter args)
              [(type-hint->letter return)])))))
    (prn m)))

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
    #_(->> xf-body
      (clj.core/map
        (fn [x]
          (if (coll? x)
            (apply-hints x)
            x))))
    ;; With wrap.
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
(comment
  (re-find #"\$O+$" "foo$OOO")
  )

(defn map:analyze-types [f rf xf-body]
  (let [ana-f (analyze-primitive-interface f)
        ana-rf (analyze-primitive-interface rf)
        replacements {'f  (get ana-f 1 {:arity  1
                                        :args   '[Object]
                                        :return 'Object})
                      'rf (-> ana-rf
                            (get 2 {:arity  2
                                    :args   '[Object Object]
                                    :return  'Object})
                            (assoc-in [:args 1] (get-in ana-f [1 :return] 'Object)))}]
    (apply-type-hints
      ;; map's arity-2 arg1 should have the same type as the f's arg0.
      (assoc-in ana-rf [2 :args 1] (get-in ana-f [1 :args 0]))
      replacements
      xf-body)))

(defmacro map:with-types [xf-body]
  `(let [body# ~xf-body]
     (fn [f# rf#]
       ((eval (map:analyze-types f# rf# body#)) f# rf#))))

(comment
  (analyze-primitive-interface (fn ^long [^long i] i))
  (let [f (fn ^long [^long i] i)
        rf (fn
             (^double [^double a] a)
             (^double
              [^double a ^long b] (Numbers/divide (Numbers/add a b) 2.0)))]
    #_(map:analyze-types f rf
      '(fn
         ([] (rf))
         ([acc] (rf acc))
         ([acc item]
          (rf acc (f item)))))
    (macroexpand-1 '(map:with-types f rf
                      (fn
                         ([] (rf))
                         ([acc] (rf acc))
                         ([acc item]
                          (rf acc (f item)))))))
  )

#_(defn map
  ([f]
   (let [ret-type (cond
                    (or
                      (instance? IFn$LL f)
                      (instance? IFn$OL f)
                      (instance? IFn$DL f))
                    Long
                    (or
                      (instance? IFn$DD f)
                      (instance? IFn$OD f)
                      (instance? IFn$LD f))
                    Double)]
     (fn
       ([]
        ret-type)
       ([rf]
        ;; TODO: Generate all of this type hinted code.
        (or
          (cond
            (and
              (= Long ret-type)
              ;; TODO: Support the other primitive function interfaces
              ;; (instance? IFn$LLO rf)
              ;; (instance? IFn$OLL rf)
              ;; (instance? IFn$LLL rf)
              (instance? IFn$OLO rf))
            (cond
              (instance? IFn$LL f)
              (fn
                ([] (rf))
                ([acc] (rf acc))
                ([acc ^long item]
                 (.invokePrim ^IFn$OLO rf acc (.invokePrim ^IFn$LL f item))))

              (instance? IFn$OL f)
              (fn
                ([] (rf))
                ([acc] (rf acc))
                ([acc item]
                 (.invokePrim ^IFn$OLO rf acc (.invokePrim ^IFn$OL f item))))

              (instance? IFn$DL f)
              (fn
                ([] (rf))
                ([acc] (rf acc))
                ([acc ^double item]
                 (.invokePrim ^IFn$OLO rf acc (.invokePrim ^IFn$DL f item)))))

            (and
              (= Double ret-type)
              ;; TODO: Support the other primitive function interfaces
              ;; (instance? IFn$DDO rf)
              ;; (instance? IFn$ODD rf)
              ;; (instance? IFn$DDD rf)
              (instance? IFn$ODO rf))
            (cond
              (instance? IFn$DD f)
              (fn
                ([] (rf))
                ([acc] (rf acc))
                ([acc ^double item]
                 (.invokePrim ^IFn$ODO rf acc (.invokePrim ^IFn$DD f item))))

              (instance? IFn$OD f)
              (fn
                ([] (rf))
                ([acc] (rf acc))
                ([acc item]
                 (.invokePrim ^IFn$ODO rf acc (.invokePrim ^IFn$OD f item))))

              (instance? IFn$LD f)
              (fn
                ([] (rf))
                ([acc] (rf acc))
                ([acc ^long item]
                 (.invokePrim ^IFn$ODO rf acc (.invokePrim ^IFn$LD f item))))))
          (fn
            ([] (rf))
            ([acc] (rf acc))
            ([acc item]
             (rf acc (f item)))))))))
  ([f coll]
   (xf-seq (map f) coll)))

(defn map
  ([f]
   (fn
     ([]
      (get-in (analyze-primitive-interface f) [1 :return]))
     ([rf]
      ;; TODO: Look at how scepter creates functions on the fly
      ((eval (map:analyze-types f rf
               '(fn [f rf]
                  (fn
                    ([] (rf))
                    ([acc] (rf acc))
                    ([acc item]
                     (rf acc (f item))))))) f rf))))
  ([f coll]
   (xf-seq (map f) coll)))

(comment

  ;; Currently returns incorrectly:
  ;; (2 3 nil)
  (map (fn ^long [^long i] (Numbers/add i 1)) [1 2])

  ;; But we're very close.

  )