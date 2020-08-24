(ns ^:no-doc active.clojure.record-helper
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
     (c/assertion-violation `throw-illegal-argument-exception
                            (str "Illegal argument: " msg)
                            msg)))


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
           ?field-tuples     (if ?options (nth ?params 2) (second ?params))
           ?opt+specs        (if ?options (drop 3 ?params) (drop 2 ?params))]

       (when-not (or (and (list? ?constructor-call)
                       (not (empty? ?constructor-call)))
                   (symbol? ?constructor-call))
         (throw (throw-illegal-argument-exception (str "constructor call must be a list in " ns " " (meta form)))))

       (when-not (vector? ?field-tuples)
         (throw (throw-illegal-argument-exception (str "field tuples must be a vector in " ns " " (meta form)))))

       (when-not (even? (count (remove seq? ?field-tuples)))
         (throw (throw-illegal-argument-exception (str "odd number of elements in field tuples in " ns " " (meta form)))))

       (when-not (every? true? (map #(= 3 (count %1)) (filter seq? ?field-tuples)))
         (throw (throw-illegal-argument-exception (str "wrong number of elements in field tuples with lens in " ns " " (meta form)))))

       (let [field-tuples (loop [?tuples   (seq ?field-tuples)
                                 acc '()]
                            (if (empty? ?tuples)
                              (reverse acc)

                              (let [tuple (first ?tuples)]
                                (cond
                                  (not (symbol? tuple))
                                  (throw (throw-illegal-argument-exception (str "invalid field tuple " tuple " in " ns " " (meta form))))

                                  (empty? (rest ?tuples))
                                  (throw (throw-illegal-argument-exception (str "incomplete field tuple for " tuple " in " ns " " (meta form))))

                                  (not (symbol? (fnext ?tuples)))
                                  (throw (throw-illegal-argument-exception (str "invalid accessor " (fnext ?tuples) " for " tuple " in " ns " " (meta form))))

                                  :default
                                  (recur (nnext ?tuples)
                                    (list* [tuple (fnext ?tuples)] acc))))))

             [?constructor & ?constructor-args] (cond
                                                  (list? ?constructor-call)
                                                  ?constructor-call

                                                  (symbol? ?constructor-call)
                                                  (concat [?constructor-call]
                                                    (map first field-tuples)))
             ;; Rename for nongenerative test
             new-ns   ns
             new-form form]

         ;;; Check if constructor-args are in field-names
         (let [?field-names-set (set (map first field-tuples))]
           (doseq [?constructor-arg ?constructor-args]
             (when-not (contains? ?field-names-set ?constructor-arg)
               (throw (throw-illegal-argument-exception (str "Constructor argument `" ?constructor-arg "` is not a field in " *ns* " " (meta form)))))))

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
   (defn validate-fields!
     "Checks if magics are used in field-names, throws if present"
     [fields]
     (let [specials '#{__meta __hash __hasheq __extmap}]
       (when (some specials fields)
         (throw (AssertionError. (str "The names in " specials " cannot be used as field names for types or records.")))))))

;;; Helper functions for emit-*-record-defintion
#?(:clj
   (defn add-predicate-doc [type predicate docref]
     (document-with-arglist predicate '[thing] (str "Is object a `" type "` record? " docref))))


#?(:clj
   (defn add-constructor-doc [constructor constructor-args type field-tuples]
     (document-with-arglist
       constructor
       (vec constructor-args)
       (str "Construct a `" type "`"
         (name-doc type)
         " record.\n"
         (apply str
           (map (fn [[?field ?accessor]]
                  (str "\n`" ?field "`" (name-doc ?field) ": access via " (reference ?accessor)))
             field-tuples))))))

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
   (defn make-get-accessor-from-field-tuple-fn
     "Creating helper function for rtd-record generation"
     [type docref constructor field-tuples fields rtd-symbol meta-info]
     (fn [[field accessor]]
       (let [?rec  `rec#
             ?data `data#
             ?v    `v#]
         `(do
            (def ~(add-meta (add-accessor-doc accessor type field docref) meta-info)
              (lens/lens (fn [~?rec]
                          ;; Get index of field, at commpile time
                          ~(let [field-index-map (into {} (map-indexed (fn [i f] [f i]) fields))
                                 i               (field-index-map field)]
                             `(println ~i)
                             `(rrun/record-get ~rtd-symbol ~?rec ~i)))
               (fn [~?data ~?v]
                 ;; can't be ~constructor because constructor may take fewer arguments
                 (rrun/make-record ~rtd-symbol
                   ~@(map (fn [[shove-field shove-accessor]]
                            (if (= field shove-field)
                              ?v
                              `(~shove-accessor ~?data)))
                       field-tuples))))))
         ))))



#?(:clj
   (defn define-record-type-descriptor [meta-data type fields rtd-symbol]
     (let [meta+doc           (merge meta-data {:doc (str "record-type-descriptor for type " type)})
           record-type-symbol (symbol (str (ns-name *ns*)) (str type))
           record-fields      (mapv rrun/make-record-field fields)]
       `(def ~(add-meta rtd-symbol meta+doc)
          (rrun/make-record-type-descriptor '~record-type-symbol nil '~record-fields)))))

(def record-identifier ::record)

#?(:clj
   (defn define-type-function [meta-data type rtd-symbol predicate constructor args field-tuples]
     (let [sym-fn (fn [a] (str *ns* "/" a))
           field-tuples-sym (mapv (fn [[name accessor]] [(str name) (sym-fn accessor)]) field-tuples)
           additional-meta {:meta         (meta type)
                            :t            record-identifier
                            :rtd          (sym-fn rtd-symbol)
                            :predicate    (sym-fn predicate)
                            :constructor  (sym-fn constructor)
                            :args         (mapv str args)
                            :field-tuples field-tuples-sym
                            :ns           (str *ns*)}]

       `(def ~(add-meta type (merge meta-data additional-meta))
          ~additional-meta))))


#?(:clj


   (defn define-constructor-rtd
     "Defines a constructor based on a record-constructor-fn. This function takes one argument, a list of field symbols."
     [type make-record constructor-symbol constructor-args-symbols field-tuples meta-data]
     (let [sym-with-meta+doc (-> constructor-symbol
                                 (add-constructor-doc constructor-args-symbols type field-tuples)
                                 (add-meta meta-data))]

       `(def ~sym-with-meta+doc
          ~(if (> (count constructor-args-symbols) 20)
             `(fn [~'& many-args#]
                (when (not= ~(count constructor-args-symbols)
                            (count many-args#))
                  (throw (Exception. (str ~constructor-symbol " takes "
                                          ~(count constructor-args-symbols)
                                          " arguments. Got: "
                                          (count many-args#) "."))))
                (apply ~make-record
                       many-args#))
             `(fn [~@constructor-args-symbols]
                (~make-record
                 ~@(map (fn [[field _]]
                          (if (contains? (set constructor-args-symbols) field)
                            `~field
                            nil))
                        field-tuples))))))))




#_(defn define-accessors [type internal-constructor ?docref constructor-symbol field-tuples fields-symbols rtd-symbol meta-data]
    (map
      (get-accessor-from-field-tuple-no-java-class type
        internal-constructor ?docref constructor-symbol field-tuples fields-symbols rtd-symbol meta-data)
      field-tuples))

#?(:clj
   (defn emit-own-record-definition [type options constructor constructor-args predicate field-tuples opt+specs]

     (let [?docref    (str "See " (reference constructor) ".")
           fields     (mapv first field-tuples)
           _          (validate-fields! fields)
           rtd-symbol (gensym (str type "-rtd-gensym-"))
           meta-data  (meta type)

           field-triple->accessor (make-get-accessor-from-field-tuple-fn
                                    type ?docref constructor field-tuples fields rtd-symbol meta-data)]


       `(do
          (declare
            ~@(map second field-tuples)
            ~@(map first field-tuples))

          ~(define-record-type-descriptor meta-data type fields rtd-symbol)


          ;; Predicate
          (def ~(add-meta (add-predicate-doc type predicate ?docref) meta-data)
            (fn [x#] (rrun/record-of-type? x# ~rtd-symbol)))

          ;; Constructor
          ;; We are defining a anonymous function for the define constructor function.
          ;; Since this function cannot be constructed in clj, we need to it at runtime.
          ;; To make the symbol `a` known to both, we define it before-hand in macro expansion
          ~(let [a (gensym)]
           `(let [~a (fn [& x#] (apply rrun/make-record ~rtd-symbol x#))]
              ~(define-constructor-rtd type
                 a constructor constructor-args field-tuples meta-data)))

          ;; Accessors
          ~@(map field-triple->accessor field-tuples)

          ;; Specs
          ~(when-let [spec-name (:spec options)]
             (add-spec-code spec-name predicate field-tuples constructor-args constructor))

          ~(define-type-function meta-data type rtd-symbol predicate constructor constructor-args field-tuples)
          ))))
