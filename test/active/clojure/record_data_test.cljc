(ns active.clojure.record-data-test
  (:require [active.clojure.record :refer (define-record-type)]))

(define-record-type IntInt
  (make-int-int fst snd)
  int-int?
  [^{:spec int?} fst int-int-fst
   ^{:spec int?} snd int-int-snd])

(define-record-type Container
  (make-container value)
  container?
  [^{:spec ::IntInt} value container-value])
