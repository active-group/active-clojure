(ns active.clojure.record-nongenerative-test
  (:require [active.clojure.record :refer (define-record-type)]))

(define-record-type NonGROtherNS
  {:nongenerative "NonGROtherNS"}
  (make-ngrons field)
  ngrons?
  [field ngrons-field])
