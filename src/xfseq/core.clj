(ns xfseq.core
  (:refer-clojure :exclude [map])
  (:import [xfseq XFSeq]
           [clojure.lang IFn$OL IFn$LL IFn$DL IFn$LD IFn$OD IFn$DD IFn$OLO IFn$ODO]))

(defn xf-seq
  [xf coll]
  (XFSeq/create xf coll))

(defn map
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
               (.invokePrim ^IFn$OLO rf acc (.invokePrim ^IFn$DL f item))))
            :else
            (fn
              ([] (rf))
              ([acc] (rf acc))
              ([acc item]
               (rf acc (f item)))))

          (and
            (= Long ret-type)
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
               (.invokePrim ^IFn$ODO rf acc (.invokePrim ^IFn$LD f item))))
            :else
            (fn
              ([] (rf))
              ([acc] (rf acc))
              ([acc item]
               (rf acc (f item)))))

          :else
          (fn
            ([] (rf))
            ([acc] (rf acc))
            ([acc item]
             (rf acc (f item)))))))))
  ([f coll]
   (xf-seq (map f) coll)))
