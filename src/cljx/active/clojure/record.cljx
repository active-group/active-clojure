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
  [?type ?constructor-call ?predicate ?field-specs & ?opt+specs]
  (when-not (and (list? ?constructor-call)
                 (not (empty? ?constructor-call)))
    (throw (IllegalArgumentException. (str "constructor call must be a list in " *ns* " " (meta &form)))))
  (when-not (vector? ?field-specs)
    (throw (IllegalArgumentException. (str "field specs must be a vector in " *ns* " " (meta &form)))))
  (when-not (even? (count (remove seq? ?field-specs)))
    (throw (IllegalArgumentException. (str "odd number of elements in field specs in " *ns* " " (meta &form)))))
  (when-not (every? true? (map #(= 3 (count %1)) (filter seq? ?field-specs)))
    (throw (IllegalArgumentException. (str "wrong number of elements in field specs with lense in " *ns* " " (meta &form)))))
  (let [?field-pairs (partition 2 (flatten (map #(if (seq? %1) (butlast %1) %1) ?field-specs)))
        ?field-pairs-lense (filter seq? ?field-specs)
        ?constructor (first ?constructor-call)
        ?constructor-args (rest ?constructor-call)
        ?constructor-args-set (set ?constructor-args)]
    `(do
       (defrecord ~?type
           [~@(map first ?field-pairs)]
         ~@?opt+specs)
       (defn ~?predicate
         ~(str "Is object a " ?type " record?")
         [x#]
         (instance? ~?type x#))
       (defn ~?constructor
         ~(str "Construct a `" ?type "` record.") ;; FIXME: arg docs
         [~@?constructor-args]
         (new ~?type
              ~@(map (fn [[?field _]]
                       (if (contains? ?constructor-args-set ?field)
                         `~?field
                         `nil))
                     ?field-pairs)))
       ~@(map (fn [[?field ?accessor]]
                (let [?rec (with-meta `rec# {:tag ?type})]
                  `(defn ~?accessor
                     ~(str "Access the `" ?field "` field from a `" ?type "` record.")
                     [~?rec]
                     (check-type ~?type ~?rec ~?accessor)
                     (. ~?rec ~(symbol (str "-" ?field))))))
              ?field-pairs)
       ~@(map (fn [[?field ?accessor ?lens]]
                (let [?data `data#
                      ?v `v#]
                  `(def ~?lens
                     ~(str "Lens for the `" ?field "` field from a `" ?type "` record.")
                     (active.clojure.lens/lens ~?accessor
                                               (fn [~?data ~?v] 
                                                 (~?constructor ~@(map 
                                                                   (fn [[?shove-field ?shove-accessor]]
                                                                     (if (= ?field ?shove-field)
                                                                       ?v 
                                                                       `(~?shove-accessor ~?data)))
                                                                   ?field-pairs)))))))
              ?field-pairs-lense))))



