(ns active.clojure.record-nongenerative-test
  (:require #?(:clj [active.clojure.record :refer [define-record-type]])
            #?(:cljs [active.clojure.cljs.record]))
  #?(:cljs
     (:require-macros [active.clojure.cljs.record :refer [define-record-type]])))

(define-record-type NonGROtherNS
  {:nongenerative "NonGROtherNS"}
  (make-ngrons field)
  ngrons?
  [field ngrons-field])
