(ns ^{:doc "A re-implementation of `active.clojure.record` that makes use of
Clojure's new spec library. Define records the same ways as in the old
implemenation or use the new syntax to automatically generate specs.
If a field has no explicit spec, defaults to `any?`."}
    active.clojure.record-spec
  (:require [clojure.spec.alpha :as s]
            [active.clojure.condition :as c]
            [clojure.spec.gen.alpha :as gen]))

;; Only needed in ClojureScript, does nothing in Clojure
(defn check-type
  [type rec accessor]
  #?(:clj (do))
  #?(:cljs
     (when-not (instance? type rec)
       (throw (js/Error. (str "Wrong record type passed to accessor." rec type))))))


(defn specs-vec
  [spec &form]
  (if (even? (count spec))
    (throw (IllegalArgumentException. (str "invalid field spec " spec " in " *ns* " " (meta &form))))
    (let [[field accessor & mods] spec
          mod-m (into {} (map vec (partition 2 mods)))]
      [field accessor (if-let [spec (:spec (meta field))]
                        (assoc mod-m :spec spec)
                        (assoc mod-m :spec any?))])))


(defn field-triples
  "Takes a vector of specs and the `&form` variable and returns a vector of triples.
  The specs are in one of two forms:
  - a list, containing the field name (symbol), the accessor's name (symobl) and
    optionally a key (`:spec|:lens`) followed by either the spec or the lens'
    name
  - two consecutive symbols (the field name (symbol) and the accessor's name
    (symbol).
  The `&form` is only used for reporting errors."
  [specs &form]
  (loop [specs (seq specs)
         triples []]
    (if (empty? specs)
      triples
      (let [spec (first specs)]
        (cond
          (list? spec)
          (recur (rest specs) (conj triples (specs-vec spec &form)))

          (symbol? spec)
          (do
            (when (empty? (rest specs))
              (throw (IllegalArgumentException. (str "incomplete field spec for " spec " in " *ns* " " (meta &form)))))
            (when-not (symbol? (fnext specs))
              (throw (IllegalArgumentException. (str "invalid accessor " (fnext specs) " for " spec " in " *ns* " " (meta &form)))))
            (recur (nnext specs)
                   (conj triples [spec (fnext specs) {:spec (or (:spec (meta spec)) any?)}])))
          :else
          (throw (IllegalArgumentException. (str "invalid field spec " spec " in " *ns* " " (meta &form)))))))))


(defn ns-keyword
  "Takes a symbol or string `the-name-sym` and returns a namespaces keyword
  based on that symbol.

  Example: `(ns-keyword 'foo) => :calling.name.space/foo`"
  [the-name-sym]
  (if the-name-sym
    (keyword (str (ns-name *ns*)) (str the-name-sym))
    (c/assertion-violation `ns-keyword "argument must not be nil" the-name-sym)))


(defn define-spec-form
  "Takes the name of a type and a predicate and returns the form for defining
  the spec for this name using the predicate.

  Example:

  ```
  (define-spec-form 'foo bar?)
  => (clojure.spec.alpha/def :calling.name.space/foo bar?)
  ```"
  [the-name the-predicate]
  (let [ns-key (ns-keyword the-name)]
    `(s/def ~ns-key ~the-predicate)))


(defn define-type-spec-form
  "Takes a symbol `the-name` and a seq of symbols `the-keys` and returns a spec
  form based on those values.

  Example:
  ```
  (define-type-spec-form 'foo ['bar 'baz])
  => (clojure.spec.alpha/def :calling.name.space/foo
                             (s/and
                               predicate
                               (clojure.spec.alpha/keys :req-un
                                                        [:calling.name.space/bar
                                                         :calling.name.space/baz])))
  ```"
  [the-name constructor predicate the-keys]
  (let [ns-key (ns-keyword the-name)
        ks (mapv ns-keyword the-keys)]
    `(s/def ~ns-key
       (s/spec (s/and ~predicate (s/keys :req-un ~ks))
               :gen (fn []
                      (->> (s/gen (s/keys :req-un ~ks))
                           (gen/fmap (fn [ks#] (apply ~constructor
                                                     (vals ks#))))))))))


(defn define-constructor-spec-form
  "Takes the name of a constructor `the-name`, a seq of arguments and a list of
  specs and returns a spec-form for the constructor.

  Example:
  ```
  (define-constructor-spec-form 'make-foo 'foo ['bar 'baz] ['foo-bar 'foo-baz])
  => (clojure.spec.alpha/fdef make-foo
                              :args (clojure.spec.alpha/cat :bar ::foo-bar :baz ::foo-baz)
                              :ret ::foo)
  ```"
  [the-name the-ret-type the-args-list the-specs-list]
  (let [key-args-list (map keyword the-args-list)
        ns-spec-list (map ns-keyword the-specs-list)
        the-args-entry (apply concat (map (fn [l r] [l r]) key-args-list ns-spec-list))]
    `(s/fdef ~the-name
             :args (s/cat ~@the-args-entry)
             :ret ~(ns-keyword the-ret-type))))


(defn define-accessor-spec-form
  "Takes the name of a constructor `the-name`, a seq of arguments and a list of
  specs and returns a spec-form for the constructor.

  Example:
  ```
  (define-accessor-spec-form 'make-foo ['bar 'baz] ['foo-bar 'foo-baz])
  => (clojure.spec.alpha/fdef make-foo
                              :args (clojure.spec.alpha/cat :bar ::foo-bar :baz ::foo-baz)
                              :ret ::foo)
  ```"
  [the-accessor-name the-type-name]
  `(s/fdef ~the-accessor-name
           :args (s/cat ~(keyword the-type-name) ~(ns-keyword the-type-name))
           :res ~boolean?))


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
  (when-not (every? true? (map #(= 3 (count %)) (filter seq? ?field-specs)))
    (throw (IllegalArgumentException. (str "wrong number of elements in field specs with lens in " *ns* " " (meta &form)))))

  (let [?field-triples (field-triples ?field-specs &form)
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
        ?docref (str "See " (reference ?constructor) ".")]
    (let [?field-names-set (set ?field-names)]
      (doseq [?constructor-arg ?constructor-args]
        (when-not (contains? ?field-names-set ?constructor-arg)
          (throw (IllegalArgumentException. (str "constructor argument " ?constructor-arg " is not a field in " *ns* " " (meta &form)))))))


    `(do
       (defrecord ~?type
           [~@(map #(->> % first (str ?type "-") symbol) ?field-triples)]
         ~@?opt+specs)
       (def ~(document-with-arglist ?predicate '[thing] (str "Is object a `" ?type "` record? " ?docref))
         (fn [x#]
           (instance? ~?type x#)))
       (def ~(document-with-arglist ?constructor 
                                    (vec ?constructor-args)
                                    (str "Construct a `" ?type "`"
                                         (name-doc ?type)
                                         " record.\n"
                                         (apply str
                                                (map (fn [[?field ?accessor ?lens+spec]]
                                                       (str "\n`" ?field "`" (name-doc ?field) ": access via " (reference ?accessor)
                                                            (if (:lens ?lens+spec)
                                                              (str ", lens " (reference (:lens ?lens+spec)))
                                                              "")))
                                                     ?field-triples))))
         (fn [~@?constructor-args]
           (new ~?type
                ~@(map (fn [[?field _]]
                         (if (contains? ?constructor-args-set ?field)
                           `~?field
                           `nil))
                       ?field-triples))))
       (declare ~@(map (fn [[?field ?accessor ?lens+spec]] ?accessor) ?field-triples))
       ~@(mapcat (fn [[?field ?accessor ?lens+spec]]
                   (let [?rec (with-meta `rec# {:tag ?type})]
                     `((def ~(document-with-arglist ?accessor (vector ?type)  (str "Access `" ?field "` field"
                                                                                   (name-doc ?field)
                                                                                   " from a [[" ?type "]] record. " ?docref))
                         (fn [~?rec]
                           (check-type ~?type ~?rec ~?accessor)
                           (. ~?rec ~(symbol (str ?type "-" ?field)))))
                       ~@(when-let [?lens (:lens ?lens+spec)]
                           (let [?data `data#
                                 ?v `v#]
                             `((def ~(document ?lens (str "Lens for the `" ?field "` field"
                                                          (name-doc ?field)
                                                          " from a [[" ?type "]] record." ?docref))
                                 (active.clojure.lens/lens ~?accessor
                                                           (fn [~?data ~?v]
                                                             (~?constructor ~@(map
                                                                               (fn [[?shove-field ?shove-accessor]]
                                                                                 (if (= ?field ?shove-field)
                                                                                   ?v
                                                                                   `(~?shove-accessor ~?data)))
                                                                               ?field-triples))))))))
                       ~@(when-let [?spec (:spec ?lens+spec)]))))
                 ?field-triples)
       ;; type-spec
       ~(define-type-spec-form ?type ?constructor ?predicate (map second (filter (fn [[_ _ l+s]] (:spec l+s)) ?field-triples)))
       ~(define-constructor-spec-form (first ?constructor-call) ?type (map first ?field-triples) (map second ?field-triples))
       ;; field-specs
       ~@(for [[_field ?accessor ?lens+spec] ?field-triples :when (:spec ?lens+spec)]
           (define-spec-form ?accessor (:spec ?lens+spec)))
       ~@(for [[_field ?accessor ?lens+spec] ?field-triples :when (:spec ?lens+spec)]
           (define-accessor-spec-form ?accessor ?type))))))
