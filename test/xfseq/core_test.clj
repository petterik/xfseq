(ns xfseq.core-test
  (:require [clojure.test :as test :refer [deftest is are testing]]
            [xfseq.core :as core]
            [xfseq.gen :as gen]))

(def size (long 1e3))

(defn dechunk [s]
  (when-some [[x & r] (seq s)]
    (cons x (dechunk r))))

(comment

  (gen/xf-seq (map inc) (repeat size (Long. 2))))

(deftest seq-test
  ;; Objects
  (let [objs (repeat size (Long. 2))
        v-objs (vec objs)

        rang (range 0 (long size))
        v-rang (vec rang)
        s-rang (set rang)

        arr (object-array objs)
        l-arr (long-array size)
        d-arr (double-array size)

        ;; Not sure what this tests more than what objs (repeat) does.
        dc-objs (dechunk v-objs)]

    (testing "Basic sequence functionality"
      (are [s]
        (=
          (sequence (map inc) s)
          (core/xf-seq (map inc) s)
          (gen/xf-seq (map inc) s))
        objs v-objs rang v-rang s-rang arr #_l-arr #_d-arr dc-objs))

    #_(let [even? (comp even? int)]
      (testing "Filtering"
        (are [s]
          (=
            (sequence (filter even?) s)
            (core/xf-seq (filter even?) s)
            (gen/xf-seq (filter even?) s))
          objs v-objs rang v-rang s-rang arr l-arr d-arr dc-objs)))

    #_(testing "Reduced"
      (is (< 40 size))
      (are [s]
        (=
          (sequence (take 40) s)
          (core/xf-seq (take 40) s)
          (gen/xf-seq (take 40) s))
        objs v-objs rang v-rang s-rang arr l-arr d-arr dc-objs))

    #_(testing "Long primitives"
      (are [s]
        (=
          (sequence (map core/long-inc) s)
          (core/xf-seq (core/map core/long-inc) s)
          (gen/xf-seq (core/map core/long-inc) s))
        objs v-objs rang v-rang s-rang arr l-arr d-arr dc-objs))
    ))
