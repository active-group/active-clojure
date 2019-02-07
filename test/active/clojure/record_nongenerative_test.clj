(ns active.clojure.record-nongenerative-test
  (:require [active.clojure.record :refer (define-record-type)]
            [clojure.test :refer :all]))

;;; Here the same definition is written as in the ``record-test`` namespace.
;;; Should fail because of other namespace
;; Helper macro that allows to test a macro.
(defmacro throws-exception?
  [form]
  (try
    (eval form)
    (catch Exception e
      (= "This record type definition already exists with different arguments."
         (.getMessage e)))))

(deftest nongenerative-record-test
  (is (throws-exception?
       (define-record-type NonGenerativeRecord
         {:nongenerative "NonGenerativeRecord"}
         (make-non-generative-record field)
         non-generative-recod?
         [field non-generative-record-field]))))
