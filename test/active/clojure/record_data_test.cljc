(ns active.clojure.record-data-test
  (:require #?(:clj [active.clojure.clj.record :refer [define-record-type]]
               :cljs [active.clojure.cljs.record :refer-macros [define-record-type]])))

(define-record-type IntInt
  {:spec ::IntInt}
  (make-int-int fst snd)
  int-int?
  [^{:spec int?} fst int-int-fst
   ^{:spec int?} snd int-int-snd])

(define-record-type Container
  {:spec ::Container}
  (make-container value)
  container?
  [^{:spec ::IntInt} value container-value])
