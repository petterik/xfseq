(ns xfseq.bench.runner
  "Command-line helpers used by the build tasks.

  JMH writes its own JSON, so this namespace reserves durable paths, merges
  independently executed smoke groups, validates the result shape, and writes
  environment metadata with CREATE_NEW semantics."
  (:require [clojure.string :as str]
            [xfseq.bench.registry :as registry]))

(set! *warn-on-reflection* true)

(defn- parse-commands
  [value]
  (read-string value))

(defn -main
  [& args]
  (try
    (case (first args)
    "validate"
    (println (pr-str (registry/validate-result! (second args))))

    "validate-smoke"
    (println (pr-str (registry/validate-smoke! (second args))))

    "validate-phase3-smoke"
    (println (pr-str (registry/validate-phase3-smoke! (second args))))

    "manifest"
    (let [manifest (registry/read-manifest (second args))]
      (println (pr-str {:path (:path manifest)
                        :profile (:profile manifest)
                        :cells (count (:cells manifest))
                        :identities (count (registry/manifest-identities manifest))})))

    "validate-manifest"
    (let [[_ result manifest profile] args]
      (println (pr-str
                 (registry/validate-manifest!
                   result manifest (keyword (or profile "decision"))))))

    "merge"
    (println (pr-str (registry/merge-results! (second args) (drop 2 args))))

    "merge-smoke"
    (println (pr-str (registry/merge-smoke-results!
                       (second args)
                       (drop 2 args))))

    "merge-manifest"
    (let [[_ target manifest profile & inputs] args]
      (println (pr-str
                 (registry/merge-manifest-results!
                   target inputs manifest (keyword profile)))))

    "environment"
    (let [[phase target profile run-id result jar commands]
          (case (count args)
            6 (let [[_ target profile result jar commands] args]
                [:phase2 target profile nil result jar commands])
            7 (let [[_ target profile run-id result jar commands] args]
                [:phase2 target profile (when-not (str/blank? run-id)
                                         run-id)
                 result jar commands])
            8 (let [[_ phase target profile run-id result jar commands] args]
                [(keyword phase) target profile
                 (when-not (str/blank? run-id) run-id)
                 result jar commands])
            (throw (IllegalArgumentException.
                     "environment expects target, profile, result, jar, commands, optionally run-id, or phase target ...")))
          value (registry/environment phase
                                      (keyword profile)
                                      run-id
                                      result
                                      jar
                                      (parse-commands commands))]
      (registry/write-edn-new! target value)
      (println (pr-str {:path target
                        :sha256 (registry/sha256-file target)})))

      (throw (IllegalArgumentException.
               (str "Unknown benchmark runner command: " (first args)))))
    (finally
      ;; clojure.java.shell/sh uses agent-backed stream readers.  The work is
      ;; complete when each command returns, but those pools otherwise keep
      ;; this short-lived CLI process alive after its output has been emitted.
      (shutdown-agents))))
