(ns active.clojure.clj.record
  (:require [active.clojure.record-helper :as r-help]
            [active.clojure.clj.record-clj-internals :refer [emit-java-record-definition
                                                             emit-own-record-definition]]))

(defmacro define-record-type
  [?type ?second & ?params]
  ;; Only emit a new record-definition, when there isn't already one
  ;; (only, when :nongenerative option is truthy)
  (when-let [[type options constructor constructor-args predicate field-triples opt+specs]
             (r-help/prepare-arguments! &form *ns* ?type ?second ?params)]
    (if (= false (:java-class? options))
      (emit-own-record-definition type options constructor constructor-args
                                  predicate field-triples opt+specs)
      (emit-java-record-definition type options constructor constructor-args
                                   predicate field-triples opt+specs))))


(defmacro record-type-rtd
   [rt]
   `(~rt :rtd))

(defmacro record-type-meta
  [rt]
  `(~rt :meta))

;; (defn predicate->record-meta [predicate]
;;   ;; Expects a namespace resolved predicate
;;   ;; if the predicate meta contains UnresolvedRecordMeta it returns a RecordMeta
;;   ;; record with resolved values. Else nil.
;;   (:meta (meta predicate)))

;; (defn record-type-predicate? [foo]
;;   (instance? RecordMeta (predicate->record-meta foo)))