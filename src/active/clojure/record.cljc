(ns active.clojure.record
  #?@
  (:clj
   [(:require
     [active.clojure.lens :as lens]
     [active.clojure.macro :refer [if-cljs]])]
   :cljs
   [(:require
     [active.clojure.lens :as lens]
    (:require-macros [active.clojure.macro :refer [if-cljs]])]))

;; Only needed in ClojureScript, does nothing in Clojure
(defn check-type
  [type rec]
  #?(:clj (do))
  #?(:cljs
     (when-not (instance? type rec)
       (throw (js/Error. (str "Wrong record type passed to accessor." rec type))))))


(defrecord RecordMeta
    ;; unresolved store for record related symbols. May not leak outside this
    ;; namespace. Contains ns to allow post-macro qualification; see `record-meta` function.
    [predicate constructor ordered-accessors])

(defmacro resolve*
  [& args]
  `(if-cljs nil (resolve ~@args)))

(defmacro intern*
  [& args]
  `(if-cljs nil (intern ~@args)))

(defmacro make-record-meta
  [?predicate ?constructor ?constructor-args ?field-triples]
  #?(:cljs nil) ;; we don't return meta in cljs
  #?(:clj
  (do
    ;; we need to internalize symbols to ns resolve them
    (intern* *ns* ?predicate)
    (intern* *ns* ?constructor)
    (->RecordMeta
     (resolve* ?predicate) (resolve* ?constructor)
     (mapv (fn [constr]
             (let [accessor (second (first (filter #(= (first %) constr) ?field-triples)))]
               (intern* *ns* accessor)
               (resolve* accessor)))
           ?constructor-args)))))

#?(:clj
(defmacro define-record-type
  "Attach doc properties to the type and the field names to get reasonable docstrings."
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

  (let [?field-triples (loop [specs (seq ?field-specs)
                              triples '()]
                         (if (empty? specs)
                           (reverse triples)
                           (let [spec (first specs)]

                             (cond
                              (list? spec)
                              (do
                                (when-not (and (= 3 (count spec))
                                               (every? symbol spec))
                                  (IllegalArgumentException. (str "invalid field spec " spec " in " *ns* " " (meta &form))))
                                (recur (rest specs) (list* spec triples)))

                              (symbol? spec)
                              (do
                                (when (empty? (rest specs))
                                  (throw (IllegalArgumentException. (str "incomplete field spec for " spec " in " *ns* " " (meta &form)))))
                                (when-not (symbol? (fnext specs))
                                  (throw (IllegalArgumentException. (str "invalid accessor " (fnext specs) " for " spec " in " *ns* " " (meta &form)))))
                                (recur (nnext specs)
                                       (list* [spec (fnext specs) nil] triples)))

                              :else
                              (throw (IllegalArgumentException. (str "invalid field spec " spec " in " *ns* " " (meta &form))))))))

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
                                                 (assoc m :arglists `'(~arglist)))))))
        name-doc (fn [field]
                    (if-let [doc (:doc (meta field))]
                      (str " (" doc ")")
                      ""))

        ?field-names (map first ?field-triples)
        reference (fn [name]
                    (str "[[" (ns-name *ns*) "/" name "]]"))
        ?docref (str "See " (reference ?constructor) ".")

        record-meta `(make-record-meta ~?predicate ~?constructor ~?constructor-args ~?field-triples)]

    (let [?field-names-set (set ?field-names)]
      (doseq [?constructor-arg ?constructor-args]
        (when-not (contains? ?field-names-set ?constructor-arg)
          (throw (IllegalArgumentException. (str "constructor argument " ?constructor-arg " is not a field in " *ns* " " (meta &form)))))))

    `(do
       (defrecord ~?type
           [~@(map first ?field-triples)]
         ~@?opt+specs)

       (def ~(vary-meta (document-with-arglist ?predicate '[thing] (str "Is object a `" ?type "` record? " ?docref))
                        assoc :meta record-meta)
         (fn [x#]
           (instance? ~?type x#)))
       (def ~(document-with-arglist ?constructor
                                 (vec ?constructor-args)
                                 (str "Construct a `" ?type "`"
                                      (name-doc ?type)
                                      " record.\n"
                                      (apply str
                                             (map (fn [[?field ?accessor ?lens]]
                                                    (str "\n`" ?field "`" (name-doc ?field) ": access via " (reference ?accessor)
                                                         (if ?lens
                                                           (str ", lens " (reference ?lens))
                                                           "")))
                                                  ?field-triples))))
         (fn [~@?constructor-args]
           (new ~?type
                ~@(map (fn [[?field _]]
                         (if (contains? ?constructor-args-set ?field)
                           `~?field
                           `nil))
                       ?field-triples))))
       (declare ~@(map (fn [[?field ?accessor ?lens]] ?accessor) ?field-triples))
       ~@(mapcat (fn [[?field ?accessor ?lens]]
                   (let [?rec (with-meta `rec# {:tag ?type})]
                     `((def ~(document-with-arglist ?accessor (vector ?type)  (str "Access `" ?field "` field"
                                                                                   (name-doc ?field)
                                                                                   " from a [[" ?type "]] record. " ?docref))
                         (fn [~?rec]
                           (check-type ~?type ~?rec)
                           (. ~?rec ~(symbol (str "-" ?field)))))
                       ~@(if ?lens
                           (let [?data `data#
                                 ?v `v#]
                             `((def ~(document ?lens (str "Lens for the `" ?field "` field"
                                                          (name-doc ?field)
                                                          " from a [[" ?type "]] record." ?docref))
                                 (lens/lens ~?accessor
                                            (fn [~?data ~?v]
                                              (~?constructor ~@(map
                                                                (fn [[?shove-field ?shove-accessor]]
                                                                  (if (= ?field ?shove-field)
                                                                    ?v
                                                                    `(~?shove-accessor ~?data)))
                                                                ?field-triples)))))))
                           '()))))
                 ?field-triples)))))


(defn predicate->record-meta [predicate]
  ;; Expects a namespace resolved predicate
  ;; if the predicate meta contains UnresolvedRecordMeta it returns a RecordMeta
  ;; record with resolved values. Else nil.
  (:meta (meta predicate)))

(defn record-type-predicate? [foo]
  (instance? RecordMeta (predicate->record-meta foo)))
