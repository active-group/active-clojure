(ns active.clojure.cljs.record
  (:require [active.clojure.record-helper :as r-help]
            [active.clojure.record-runtime :as record-runtime]
            #?(:clj [active.clojure.cljs.record-cljs-internals :refer [emit-javascript-record-definition]])
            ))

#?(:clj
   (defmacro define-record-type
     [?type ?second & ?params]
     ;; Only emit a new record-definition, when there isn't already one
     ;; (only, when :nongenerative option is truthy)
     (when-let [[type options constructor constructor-args predicate field-triples opt+specs]
                (r-help/prepare-arguments! &form *ns* ?type ?second ?params)]
       ;; Note: java-class? used to be only for clj; now an alternative option.
       (if (or (:rtd-record? options) (false? (:java-class? options)))
         (r-help/emit-own-record-definition type options constructor constructor-args
                                     predicate field-triples opt+specs)
         (emit-javascript-record-definition &env type options constructor constructor-args
                                            predicate field-triples opt+specs)))))


#?(:clj
   (defmacro define-singleton-type
     "Defines a record type without fields. Instead of a constructor, the single value of this type is bound to `var-name`."
     [type-name var-name & [predicate-name]]
     (let [ctor (gensym "ctor")]
       `(do (active.clojure.cljs.record/define-record-type ~type-name {:rtd-record? true}
              (~ctor)
              ~(or predicate-name (gensym "predicate"))
              [])
            (def ~var-name (~ctor))))))

(defn record-type
  "Returns the record type of the given record value.

  ```
  (define-record-type Foo foo? (make-foo a))
  
  (= Foo (record-type (make-foo :b)))
  ```
  "
  [v]
  (if (record-runtime/record? v)
    (record-runtime/record-rtd v)
    (type v)))
