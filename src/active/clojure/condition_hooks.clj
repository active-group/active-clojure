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

(defn root-cause
  "Returns the initial cause of an exception or error by peeling off all of
  its wrappers"
  [^Throwable t]
  (loop [cause t]
    (if (and (instance? clojure.lang.Compiler$CompilerException cause)
             (not= (.source ^clojure.lang.Compiler$CompilerException cause) "NO_SOURCE_FILE"))
      (.getCause cause)) ; one level up ...
      (if-let [cause (.getCause cause)]
        (recur cause)
        cause)))

(defn print-stack-trace-of
  [^Throwable exc]
  (let [st (.getStackTrace exc)]
    (if-let [e (first st)]
      (clojure.stacktrace/print-trace-element e)
      (print "[empty stack trace]"))
    (newline)
    (doseq [e (rest st)]
      (print " ")
      (clojure.stacktrace/print-trace-element e)
      (newline))))

(defn install-default-uncaught-exception-handler!
  "Install an exception handler of last resort that will print a stack trace."
  []
  (Thread/setDefaultUncaughtExceptionHandler
   (reify Thread$UncaughtExceptionHandler
     (uncaughtException [this t e0]
       (let [e (root-cause e0)]
         (if (c/condition? e)
           (do
             (c/print-condition e *err*)
             (.flush ^java.io.PrintWriter *err*))
           (binding [*out* *err*]
             (println (.getSimpleName (class e)) (.getMessage ^Exception e))
             (print-stack-trace-of e)
             (.flush ^java.io.PrintWriter *err*))))))))
