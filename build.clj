(ns build
  (:refer-clojure :exclude [test])
  (:require [clojure.tools.build.api :as b]))

(def class-dir "target/classes")
(def basis (delay (b/create-basis {:project "deps.edn"})))

(defn clean [_]
  (b/delete {:path "target"}))

(defn javac [_]
  (b/javac {:basis @basis
            :src-dirs ["src-java"]
            :class-dir class-dir
            :javac-opts ["--release" "8" "-Xlint:-options"]}))

(defn process! [command-args]
  (let [{:keys [exit]} (b/process {:command-args command-args
                                    :out :inherit
                                    :err :inherit})]
    (when-not (zero? exit)
      (throw (ex-info "Child process failed"
                      {:command-args command-args
                       :exit exit})))))

(defn test [_]
  (process! ["clojure" "-Srepro" "-M:test"]))

(defn lint [_]
  (process! ["clojure" "-Srepro" "-M:lint" "-m" "xfseq.phase-1-lint"]))

(defn reflection [_]
  (process! ["clojure" "-Srepro" "-M:reflection" "-m"
             "xfseq.phase-1-reflection"]))

(defn check [_]
  (clean nil)
  (javac nil)
  (lint nil)
  (reflection nil)
  (test nil))
