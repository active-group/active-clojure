(ns active.clojure.record
  (:require [active.clojure.record-helper :as r-help]
            [active.clojure.record-clj-internals :refer [emit-java-record-definition]]))

;; TODO: clojure.lang.IEditableCollection would be nice

(defmacro define-record-type
  [?type ?second & ?params]
  ;; Only emit a new record-definition, when there isn't already one
  ;; (only, when :nongenerative option is truthy)
  (when-let [[type options constructor constructor-args predicate field-triples opt+specs]
             (r-help/prepare-arguments! &form *ns* ?type ?second ?params)]
    (if (false? (:java-class? options))
      (r-help/emit-own-record-definition type options constructor constructor-args
        predicate field-triples opt+specs)
      (emit-java-record-definition type options constructor constructor-args
        predicate field-triples opt+specs))))


(defmacro ^:no-doc record-type-rtd
   [rt]
   `(~rt :rtd))

(defmacro ^:no-doc record-type-meta
  [rt]
  `(~rt :meta))

;; (defn predicate->record-meta [predicate]
;;   ;; Expects a namespace resolved predicate
;;   ;; if the predicate meta contains UnresolvedRecordMeta it returns a RecordMeta
;;   ;; record with resolved values. Else nil.
;;   (:meta (meta predicate)))

;; (defn record-type-predicate? [foo]
;;   (instance? RecordMeta (predicate->record-meta foo)))

(defmacro define-singleton-type
  "Defines a record type without fields. Instead of a constructor, the single value of this type is bound to `var-name`."
  [type-name var-name & [predicate-name]]
  (let [ctor (gensym "ctor")]
    `(do (active.clojure.record/define-record-type ~type-name {:rtd-record? true}
           (~ctor)
           ~(or predicate-name (gensym "predicate"))
           [])
         (def ~var-name (~ctor)))))
