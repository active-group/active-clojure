(ns active.clojure.condition-hooks
  (:require [active.clojure.condition :as c]
            [clojure.main :as main]
            [clojure.test :refer :all]
            [clojure.stacktrace :as stack]))

(defn repl-caught
  [& [e]]
  (let [e (or e *e)]
    (if (c/condition? e)
      (c/print-condition e *err*)
      (main/repl-caught e))))

;; This trick (if it is one) stolen from humane-test-output

(defonce activation-body
  (delay
   (defmethod report :error [m]
     [m]
     (with-test-out
       (inc-report-counter :error)
       (println "\nERROR in" (testing-vars-str m))
       (when (seq *testing-contexts*) (println (testing-contexts-str)))
       (when-let [message (:message m)] (println message))
       (println "expected:" (pr-str (:expected m)))
       (print "  actual: ")
       (let [actual (:actual m)]
         (cond
          (c/condition? actual) (c/print-condition actual *out*)

          (instance? Throwable actual)
          (stack/print-cause-trace actual *stack-trace-depth*)

          :else (prn actual)))))))

(defn activate-clojure-test! []
  @activation-body)
