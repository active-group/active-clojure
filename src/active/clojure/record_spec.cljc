(ns active.clojure.record-spec
  "A re-implementation of `active.clojure.record` that makes use of
  Clojure's new spec library. Define records the same ways as in the old
  implemenation or use the new syntax to automatically generate specs.
  If a field has no explicit spec, defaults to `any?`."
  #?@
   (:clj
    [(:require
      [active.clojure.condition :as c]
      [active.clojure.lens :as lens]
      [clojure.spec.alpha :as s]
      [clojure.spec.gen.alpha :as gen]
      [active.clojure.macro :refer (if-cljs)])]
    :cljs
    [(:require
      [active.clojure.condition :as c]
      [active.clojure.lens :as lens]
      [cljs.spec.alpha :as s :include-macros true]
      [cljs.spec.gen.alpha :as gen :include-macros true])
     (:require-macros
      [active.clojure.macro :refer (if-cljs)])]))

(defn throw-illegal-argument-exception
  [msg]
  (c/assertion-violation `throw-illegal-argument-exception "Illegal argument" msg))

;; Only needed in ClojureScript, does nothing in Clojure
(defn check-type
  [type rec accessor]
  #?(:clj (do))
  #?(:cljs
     (when-not (instance? type rec)
       (throw (js/Error. (str "Wrong record type passed to accessor." rec type))))))

(defn ns-keyword
  "Takes a symbol or string `the-name-sym` and returns a namespaces keyword
  based on that symbol.

  Example: `(ns-keyword 'foo) => :calling.name.space/foo`"
  [the-name-sym]
  (if the-name-sym
    (keyword (str (ns-name *ns*)) (str the-name-sym))
    (c/assertion-violation `ns-keyword "argument must not be nil" the-name-sym)))


(defmacro s-def
  [& args]
  `(if-cljs (cljs.spec.alpha/def ~@args)
            (clojure.spec.alpha/def ~@args)))

(s-def :active.clojure.record-spec/pass (constantly true))

(defmacro s-fdef
  [& args]
  `(if-cljs (cljs.spec.alpha/fdef ~@args)
            (clojure.spec.alpha/fdef ~@args)))

(defmacro s-and
  [& args]
  `(if-cljs (cljs.spec.alpha/and ~@args)
            (clojure.spec.alpha/and ~@args)))

(defmacro s-cat
  [& args]
  `(if-cljs (cljs.spec.alpha/cat ~@args)
            (clojure.spec.alpha/cat ~@args)))

(defmacro s-spec
  [& args]
  `(if-cljs (cljs.spec.alpha/spec ~@args)
            (clojure.spec.alpha/spec ~@args)))

(defmacro s-keys
  [& args]
  `(if-cljs (cljs.spec.alpha/keys ~@args)
            (clojure.spec.alpha/keys ~@args)))

(defmacro s-gen
  [& args]
  `(if-cljs (cljs.spec.alpha/gen ~@args)
            (clojure.spec.alpha/gen ~@args)))

(defmacro s-fmap
  [& args]
  `(if-cljs (cljs.spec.gen.alpha/fmap ~@args)
            (clojure.spec.gen.alpha/fmap ~@args)))

#?(:clj
(defmacro define-record-type
  "Attach doc properties to the type and the field names to get reasonable docstrings."
  [?type ?constructor-call ?predicate ?field-specs & ?opt+specs]
  (when-not (and (list? ?constructor-call)
                 (not (empty? ?constructor-call)))
    (throw-illegal-argument-exception (str "constructor call must be a list in " *ns* " " (meta &form))))
  (when-not (vector? ?field-specs)
    (throw-illegal-argument-exception (str "field specs must be a vector in " *ns* " " (meta &form))))
  (when-not (even? (count (remove seq? ?field-specs)))
    (throw-illegal-argument-exception (str "odd number of elements in field specs in " *ns* " " (meta &form))))
  (when-not (every? true? (map #(= 3 (count %)) (filter seq? ?field-specs)))
    (throw-illegal-argument-exception (str "wrong number of elements in field specs with lens in " *ns* " " (meta &form))))

  (letfn [(define-type-spec-form
            ;; "Takes a symbol `the-name` and a seq of symbols `the-keys` and returns a spec
            ;; form based on those values.

            ;; Example:
            ;; ```
            ;; (define-type-spec-form 'foo ['bar 'baz])
            ;; => (clojure.spec.alpha/def :calling.name.space/foo
            ;;                            (s/and
            ;;                              predicate
            ;;                              (clojure.spec.alpha/keys :req-un
            ;;                                                       [:calling.name.space/bar
            ;;                                                        :calling.name.space/baz])))
            ;; ```"
            [the-name constructor predicate the-keys]
            (let [ns-key (ns-keyword the-name)
                  ks (mapv ns-keyword the-keys)]
              `(s-def ~ns-key
                (s-spec (s-and ~predicate (s-keys :req-un ~ks))
                 :gen (fn []
                        (->> (s-gen (s-keys :req-un ~ks))
                             (s-fmap (fn [ks#] (apply ~constructor
                                                      (vals ks#))))))))))
          (define-constructor-spec-form
            ;; "Takes the name of a constructor `the-name`, a seq of arguments and a list of
            ;; specs and returns a spec-form for the constructor.

            ;; Example:
            ;; ```
            ;; (define-constructor-spec-form 'make-foo 'foo ['bar 'baz] ['foo-bar 'foo-baz])
            ;; => (clojure.spec.alpha/fdef make-foo
            ;;                             :args (clojure.spec.alpha/cat :bar ::foo-bar :baz ::foo-baz)
            ;;                             :ret ::foo)
            ;; ```"
            [the-name the-ret-type the-args-list the-specs-list]
            (let [key-args-list (map keyword the-args-list)
                  ns-spec-list (map ns-keyword the-specs-list)
                  the-args-entry (apply concat (map (fn [l r] [l r]) key-args-list ns-spec-list))]
              `(s-fdef ~the-name
                :args (s-cat ~@the-args-entry)
                :ret ~(ns-keyword the-ret-type))))
          (define-accessor-spec-form
            ;; "Takes the name of an accessor `the-accessor-name`, a seq of arguments and a list of
            ;; specs and returns a spec-form for the constructor.

            ;; Example:
            ;; ```
            ;; (define-accessor-spec-form 'foo-bar 'type 'field-type)
            ;; => (clojure.spec.alpha/fdef foo-bar
            ;;                             :args (clojure.spec.alpha/cat :foo ::foo)
            ;;                             :ret ::field-type)
            ;; ```"
            [the-accessor-name the-type-name the-field-key]
            `(s-fdef ~the-accessor-name
              :args (s-cat ~(keyword the-type-name) ~(ns-keyword the-type-name))
              :ret ~(ns-keyword the-field-key)))
          (define-spec-form
            ;; "Takes the name of a type and a predicate and returns the form for defining
            ;; the spec for this name using the predicate.

            ;; Example:

            ;; ```
            ;; (define-spec-form 'foo bar?)
            ;; => (clojure.spec.alpha/def :calling.name.space/foo bar?)
            ;; ```"
            [the-name the-predicate]
            (let [ns-key (ns-keyword the-name)]
              `(s-def ~ns-key ~the-predicate)))]
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
          ?docref (str "See " (reference ?constructor) ".")]
      (let [?field-names-set (set ?field-names)]
        (doseq [?constructor-arg ?constructor-args]
          (when-not (contains? ?field-names-set ?constructor-arg)
            (throw-illegal-argument-exception (str "constructor argument " ?constructor-arg " is not a field in " *ns* " " (meta &form))))))
      `(do
         (defrecord ~?type
             [~@(map first ?field-triples)]
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
                             (check-type ~?type ~?rec ~?accessor)
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
                                                                  ?field-triples)))))))))))
                   ?field-triples)
         ;; specs
         ~@(for [[?field _accessor _lens] ?field-triples]
             (define-spec-form ?field (or (:spec (meta ?field)) '(constantly true))))
         ~(define-type-spec-form ?type ?constructor ?predicate (map first ?field-triples))
         ~(define-constructor-spec-form ?constructor ?type ?constructor-args ?constructor-args)
         ~@(for [[?field ?accessor _lens] ?field-triples]
             (define-accessor-spec-form ?accessor ?type ?field)))))))
