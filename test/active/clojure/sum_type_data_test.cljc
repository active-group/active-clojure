(ns active.clojure.sum-type-data-test
  (:require #?(:clj [active.clojure.record :refer (define-record-type)])
            #?(:clj [active.clojure.sum-type :refer (define-sum-type match)])
            ))


(define-record-type Circle
  (make-circle radius) circle?
  [radius circle-radius])

(define-record-type Square
  (make-square height width) square?
  [height square-height
   width square-width])


(define-sum-type Forms forms? [circle? square?])



