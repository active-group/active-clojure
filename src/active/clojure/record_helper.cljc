(ns active.clojure.record-helper
  (:require [active.clojure.lens :as lens]
            [active.clojure.condition :as c]
            [clojure.spec.alpha :as spec]
            [active.clojure.record-runtime :as rrun]))

;;; Nongenerative stuff
;; Maps from nongenerative-id to {:ns *ns* :form define-record-form}
#?(:clj
   (defonce global-record-type-registry (atom {})))

#?(:clj
   (defn remove-record-type
     [nongenerative-id]
     (swap! global-record-type-registry
            (fn [old-reg] (dissoc old-reg nongenerative-id)))))


#?(:clj
   (defn throw-illegal-argument-exception
     [msg]
     (c/assertion-violation `throw-illegal-argument-exception "Illegal argument" msg)))


#?(:clj
   (defn prepare-arguments!
     "Checks validity of arguments and prepares them for `define-record-type` call.
  Returns vector of arguments:
  [type options constructor constructor-args predicate field-triples opt+specs].

  If :nongenerative option is truthy, the given nongenerative-id is registered in
  the global-record-type-registry. If this id already exists, but the definitions
  are different, an error is thrown. Otherwise `nil` is returned."
     [form ns ?type ?second ?params]
     (let [?options          (when (map? ?second) ?second)
           ?constructor-call (if ?options (first ?params) ?second)
           ?predicate        (if ?options (second ?params) (first ?params))
           ?field-specs      (if ?options (nth ?params 2) (second ?params))
           ?opt+specs        (if ?options (drop 3 ?params) (drop 2 ?params))]
       (when-not (or (and (list? ?constructor-call)
                       (not (empty? ?constructor-call)))
                   (symbol? ?constructor-call))
         (throw (throw-illegal-argument-exception (str "constructor call must be a list in " ns " " (meta form)))))
       (when-not (vector? ?field-specs)
         (throw (throw-illegal-argument-exception (str "field specs must be a vector in " ns " " (meta form)))))
       (when-not (even? (count (remove seq? ?field-specs)))
         (throw (throw-illegal-argument-exception (str "odd number of elements in field specs in " ns " " (meta form)))))
       (when-not (every? true? (map #(= 3 (count %1)) (filter seq? ?field-specs)))
         (throw (throw-illegal-argument-exception (str "wrong number of elements in field specs with lens in " ns " " (meta form)))))
       (let [field-tuples (loop [specs   (seq ?field-specs)
                                 triples '()]
                            (if (empty? specs)
                              (reverse triples)
                              (let [spec (first specs)]
                                (cond
                                  (not (symbol? spec))
                                  (throw (throw-illegal-argument-exception (str "invalid field spec " spec " in " ns " " (meta form))))

                                  (empty? (rest specs))
                                  (throw (throw-illegal-argument-exception (str "incomplete field spec for " spec " in " ns " " (meta form))))

                                  (not (symbol? (fnext specs)))
                                  (throw (throw-illegal-argument-exception (str "invalid accessor " (fnext specs) " for " spec " in " ns " " (meta form))))

                                  :default
                                  (recur (nnext specs)
                                    (list* [spec (fnext specs) nil] triples))))))

             [?constructor & ?constructor-args] (cond
                                                  (list? ?constructor-call)
                                                  ?constructor-call

                                                  (symbol? ?constructor-call)
                                                  (concat [?constructor-call]
                                                    (map first field-tuples)))
             ;; Rename for nongenerative test
             new-ns   ns
             new-form form]
       ;;; Check nongenerative option
         (if-let [non-g-id (:nongenerative ?options)]
           ;; nongenerative
           (if-let [{:keys [ns form]} (get @global-record-type-registry non-g-id)]
             (if (and (= ns new-ns) (= form new-form))
               ;; non-g-id exists, but definitions are the same
               nil
               (throw (Exception. "This record type definition already exists with different arguments.")))
             ;; nongenerative, but id doesn't exist. Register id and return arguments.
             (let [non-g-id (if (= true non-g-id) (str new-ns "/" ?type) non-g-id)] ; default non-g-id when key is `true`
               (swap! global-record-type-registry
                 (fn [old-reg] (assoc old-reg non-g-id {:ns new-ns :form new-form})))
               [?type ?options ?constructor ?constructor-args ?predicate field-tuples ?opt+specs]))
           ;; generative, just return arguments.
           [?type ?options ?constructor ?constructor-args ?predicate field-tuples ?opt+specs])))
     ))


;;; record_*_internals helpers
#?(:clj
   (defn report-lens-deprecation [type]
     (println (str "active.clojure.record WARNING for record-type `" type
                   "`: the explicit definition of lenses is deprecated in favor of regular "
                   "accessors already being lenses"))))

#?(:clj
   (defn reference
     [name]
     (str "[[" (ns-name *ns*) "/" name "]]")))

#?(:clj
   (defn name-spec
     [field]
     (or (:spec (meta field))
         `any?)))

#?(:clj
   (defn name-doc
     [field]
     (if-let [doc (:doc (meta field))]
       (str " (" doc ")")
       "")))

#?(:clj
   (defn document-with-arglist
     [n arglist doc]
     (vary-meta n
                (fn [m]
                  (let [m (if (contains? m :doc)
                            m
                            (assoc m :doc doc))]
                    (if (contains? m :arglists)
                      m
                      (assoc m :arglists `'(~arglist))))))))

#?(:clj
   (defn add-meta
     [sym meta-info]
     (vary-meta sym (fn [m] (merge meta-info m)))))

#?(:clj
   (defn validate-fields
     ""
     [fields name]
     (let [specials '#{__meta __hash __hasheq __extmap}]
       (when (some specials fields)
         (throw (AssertionError. (str "The names in " specials " cannot be used as field names for types or records.")))))))

;;; Helper functions for emit-*-record-defintion
#?(:clj
   (defn add-predicate-doc [type predicate docref]
     (document-with-arglist predicate '[thing] (str "Is object a `" type "` record? " docref))))


#?(:clj
   (defn add-constructor-doc [constructor constructor-args type field-triples]
     (document-with-arglist
      constructor
      (vec constructor-args)
      (str "Construct a `" type "`"
           (name-doc type)
           " record.\n"
           (apply str
                  (map (fn [[?field ?accessor ?lens]]
                         (str "\n`" ?field "`" (name-doc ?field) ": access via " (reference ?accessor)
                              (if ?lens
                                (str ", lens " (reference ?lens))
                                "")))
                       field-triples))))))

#?(:clj
   (defn add-accessor-doc [accessor type field docref]
     (document-with-arglist accessor
                            (vector type)
                            (str "Lens for the `" field "` field"
                                 (name-doc field)
                                 " from a [[" type "]] record. " docref))))

#?(:clj
   (defn add-spec-code [spec-name predicate field-triples constructor-args constructor]
     `(do
        ;; Spec for a record type
        (spec/def ~spec-name
          (spec/and ~predicate
                    ~@(map (fn [[?field ?accessor _]]
                             `#(spec/valid? ~(name-spec ?field) (~?accessor %)))
                           field-triples)))
        ;; Spec for constructor function
        ~(let [c-specs (mapcat (fn [constructor-arg]
                                 (let [field (first (filter #(= constructor-arg %)
                                                            (map first field-triples)))]
                                   [(keyword constructor-arg) (name-spec field)]))
                               constructor-args)]
           `(spec/fdef ~constructor
              :args (spec/cat ~@c-specs)
              :ret ~spec-name)))))

#?(:clj
   (defn fn-get-accessor-from-field-triple
     [type docref constructor field-triples meta-info]
     (fn [[field accessor lens]]
       (let [?rec (with-meta `rec# {:tag type})
             ?data `data#
             ?v `v#]
         `[(def ~(add-meta (add-accessor-doc accessor type field docref) meta-info)
             (lens/lens (fn [~?rec]
                          (. ~?rec ~(symbol (str "-" field))))
                        (fn [~?data ~?v]
                          ;; can't be ~constructor because constructor may take fewer arguments
                          (new ~type ~@(map
                                        (fn [[?shove-field ?shove-accessor]]
                                          (if (= field ?shove-field)
                                            ?v
                                            `(~?shove-accessor ~?data)))
                                        field-triples)))))
           ~(when lens
              (report-lens-deprecation type)
              `(def ~lens ~accessor))
           ]))))

#?(:clj
   (defn fn-get-accessor-from-field-triple-no-java-class
     [type docref constructor field-triples fields rtd-symbol meta-info]
     (fn [[field accessor lens]]
       (let [?rec `rec#
             ?data `data#
             ?v `v#]
         `[(def ~(add-meta (add-accessor-doc accessor type field docref) meta-info)
             (lens/lens (fn [~?rec]
                          ;; Get index of field, at commpile time
                          ~(let [field-index-map (into {} (map-indexed (fn [i f] [f i]) fields))
                                 i (field-index-map field)]
                             `(rrun/record-get ~rtd-symbol ~?rec ~i)))
                        (fn [~?data ~?v]
                          ;; can't be ~constructor because constructor may take fewer arguments
                          (rrun/make-record ~rtd-symbol
                                            ~@(map (fn [[?shove-field ?shove-accessor]]
                                                     (if (= field ?shove-field)
                                                       ?v
                                                       `(~?shove-accessor ~?data)))
                                                   field-triples)))))
           ~(when lens
              (report-lens-deprecation type)
              `(def ~lens ~accessor))
           ]))))
;;; End of Helper functions
