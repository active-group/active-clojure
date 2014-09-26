(ns active.clojure.record)

(defmacro define-record-type
  [?type ?constructor-call ?predicate & ?field-specs]
  (when-not (and (list? ?constructor-call)
                 (not (empty? ?constructor-call)))
    (throw (IllegalArgumentException. (str "constructor call must be a list"))))
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
                     (. ~?rec ~?field))))
              ?field-specs))))


