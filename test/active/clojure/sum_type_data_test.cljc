(ns active.clojure.sum-type-data-test
  (:require #?(:clj [active.clojure.record :as record])
            #?(:cljs [active.clojure.cljs.record :as record :include-macros true])))


(record/define-record-type circle
  #?(:clj {:java-class? false}
     :cljs {:rtd-record? true})
  (make-circle radius) circle?
  [radius circle-radius])

(record/define-record-type square
  #?(:clj {:java-class? false}
     :cljs {:rtd-record? true})
  (make-square height width) square?
  [height square-height
   width square-width])
