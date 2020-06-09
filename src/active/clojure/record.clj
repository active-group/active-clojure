(ns active.clojure.record
  (:require [active.clojure.record-helper :as r-help]
            [active.clojure.record-clj-internals :refer [emit-java-record-definition]]
            [active.clojure.record-runtime :as runtime]))

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


(defmacro record-type-rtd
   [rt]
   `(~rt :rtd))

(defmacro record-type-meta
  [rt]
  `(~rt :meta))

#?(:clj
   (defn get-field-tuples-from-record
     "Returns field-tuples of record.
  Currently only works with Clojure."
     [record]
     (map (fn [[field accessor]]
            [(symbol field) (eval (symbol accessor))])
          (:field-tuples (eval (:name (runtime/record-rtd record)))))))

#?(:clj
   (defn get-type-from-record
     "Returns type of record.
  Currently only works with Clojure."
     [record]
     (:name (runtime/record-rtd record))))

#?(:clj
   (defn rtd-record?
     "Returns true, if the thing is an rtd-record."
     [something]
     (runtime/record? something)))

;; (defn predicate->record-meta [predicate]
;;   ;; Expects a namespace resolved predicate
;;   ;; if the predicate meta contains UnresolvedRecordMeta it returns a RecordMeta
;;   ;; record with resolved values. Else nil.
;;   (:meta (meta predicate)))

;; (defn record-type-predicate? [foo]
;;   (instance? RecordMeta (predicate->record-meta foo)))
