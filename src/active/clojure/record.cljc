(ns active.clojure.record
  (:require [active.clojure.condition :refer (error)]))

;; Only needed in ClojureScript, does nothing in Clojure
(defn check-type
  [type rec accessor]
  #?(:clj (do))
  #?(:cljs
     (when-not (instance? type rec) 
       (throw (error accessor "Wrong record type passed to accessor." rec type)))))

#?(:clj
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
    (throw (IllegalArgumentException. (str "wrong number of elements in field specs with lens in " *ns* " " (meta &form)))))

  (let [?field-pairs (partition 2 (flatten (map #(if (seq? %1) (butlast %1) %1) ?field-specs)))
        ?field-pairs-lense (filter seq? ?field-specs)
        ?constructor (first ?constructor-call)
        ?constructor-args (rest ?constructor-call)
        ?constructor-args-set (set ?constructor-args)
        document (fn [n doc]
                   (vary-meta n
                              (fn [m]
                                (if (contains? m :doc)
                                  m
                                  (assoc m :doc doc)))))
        document-with-arglist (fn [n arglist doc]
                                (vary-meta n
                                           (fn [m]
                                             (let [m (if (contains? m :doc)
                                                       m
                                                       (assoc m :doc doc))]
                                               (if (contains? m :arglists)
                                                 m
                                                 (assoc m :arglists `'(~arglist)))))))]
                             

    (let [?field-names (map first ?field-pairs)]
      (let [?field-names-set (set ?field-names)]
        (doseq [?constructor-arg ?constructor-args]
          (when-not (contains? ?field-names-set ?constructor-arg)
            (throw (IllegalArgumentException. (str "constructor argument " ?constructor-arg " is not a field in " *ns* " " (meta &form)))))))


      `(do
         (defrecord ~?type
             [~@(map first ?field-pairs)]
           ~@?opt+specs)
         (def ~(document-with-arglist ?predicate '[thing] (str "Is object a [[" ?type "]] record?"))
           (fn [x#]
             (instance? ~?type x#)))
         (def ~(document ?constructor (str "Construct a [[" ?type "]] record."))
           (fn [~@?constructor-args]
             (new ~?type
                  ~@(map (fn [[?field _]]
                           (if (contains? ?constructor-args-set ?field)
                             `~?field
                             `nil))
                         ?field-pairs))))
         ~@(map (fn [[?field ?accessor]]
                  (let [?rec (with-meta `rec# {:tag ?type})]
                    `(def ~(document-with-arglist ?accessor (vector ?type)  (str "Access the `" ?field "` field from a [[" ?type "]] record."))
                       (fn [~?rec]
                         (check-type ~?type ~?rec ~?accessor)
                         (. ~?rec ~(symbol (str "-" ?field)))))))
                ?field-pairs)
         ~@(map (fn [[?field ?accessor ?lens]]
                  (let [?data `data#
                        ?v `v#]
                    `(def ~(document-with-arglist ?lens (vector ?type 'val) (str "Lens for the `" ?field "` field from a [[" ?type "]] record."))
                       (active.clojure.lens/lens ~?accessor
                                                 (fn [~?data ~?v] 
                                                   (~?constructor ~@(map 
                                                                     (fn [[?shove-field ?shove-accessor]]
                                                                       (if (= ?field ?shove-field)
                                                                         ?v 
                                                                         `(~?shove-accessor ~?data)))
                                                                     ?field-pairs)))))))
                ?field-pairs-lense))))))



