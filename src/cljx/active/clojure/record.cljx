(ns active.clojure.record
  (:require [active.clojure.condition :refer (error)]))

;; Only needed in ClojureScript, does nothing in Clojure
(defn check-type
  [type rec accessor]
  #+clj
  (do)
  #+cljs
  (when-not (instance? type rec) 
    (throw (error accessor "Wrong record type passed to accessor." rec type))))

(defmacro define-record-type
  [?type ?constructor-call ?predicate & ?field-specs]
  (when-not (and (list? ?constructor-call)
                 (not (empty? ?constructor-call)))
    (throw (IllegalArgumentException. (str "constructor call must be a list in " *ns* " " (meta &form)))))
  (let [?constructor (first ?constructor-call)
        ?constructor-args (rest ?constructor-call)
        ?constructor-args-set (set ?constructor-args)]
    `(do
       (defrecord ~?type
           [~@(map first ?field-specs)])
       (defn ~?predicate
         ~(str "Is object a " ?type " record?")
         [x#]
         (instance? ~?type x#))
       (defn ~?constructor
         ~(str "Construct a " ?type "record.") ;; FIXME: arg docs
         [~@?constructor-args]
         (new ~?type
              ~@(map (fn [[?field _]]
                       (if (contains? ?constructor-args-set ?field)
                         `~?field
                         `nil))
                     ?field-specs)))
       ~@(map (fn [[?field ?accessor]]
                (let [?rec (with-meta `rec# {:tag ?type})]
                  `(defn ~?accessor
                     ~(str "Access the " ?field " field from a " ?type " record.")
                     [~?rec]
                     (check-type ~?type ~?rec ~?accessor)
                     (. ~?rec ~(symbol (str "-" ?field))))))
              ?field-specs))))


