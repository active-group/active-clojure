(ns active.clojure.clj.record-clj-internals
  (:require [active.clojure.condition :as c]
            [active.clojure.lens :as lens]
            [clojure.spec.alpha :as spec])
  (:import clojure.lang.IPersistentMap
           clojure.lang.RT
           java.lang.IllegalArgumentException))

;;;; Helper functions
(defn report-lens-deprecation [type]
  (println (str "active.clojure.record WARNING for record-type `" type
                "`: the explicit definition of lenses is deprecated in favor of regular "
                "accessors already being lenses")))

(defrecord RecordMeta
    ;; unresolved store for record related symbols. May not leak outside this
    ;; namespace. Contains ns to allow post-macro qualification; see `record-meta` function.
    [predicate constructor ordered-accessors])

(defmacro resolve*
  [& args]
  `(resolve ~@args))

(defmacro intern*
  [& args]
  `(intern ~@args))

(defmacro make-record-meta
  [?predicate ?constructor ?constructor-args ?field-triples]
  ;; we need to internalize symbols to ns resolve them
  (intern* *ns* ?predicate)
  (intern* *ns* ?constructor)
  (->RecordMeta
   (resolve* ?predicate) (resolve* ?constructor)
   (mapv (fn [constr]
           (let [accessor (second (first (filter #(= (first %) constr) ?field-triples)))]
             (intern* *ns* accessor)
             (resolve* accessor)))
         ?constructor-args)))

(defn- document
  [n doc]
  (vary-meta n
             (fn [m]
               (if (contains? m :doc)
                 m
                 (assoc m :doc doc)))))

(defn- document-with-arglist
  [n arglist doc]
  (vary-meta n
             (fn [m]
               (let [m (if (contains? m :doc)
                         m
                         (assoc m :doc doc))]
                 (if (contains? m :arglists)
                   m
                   (assoc m :arglists `'(~arglist)))))))

(defn- name-doc
  [field]
  (if-let [doc (:doc (meta field))]
    (str " (" doc ")")
    ""))

(defn- name-spec
  [field]
  (or (:spec (meta field))
      'any?))

(defn- reference
  [name]
  (str "[[" (ns-name *ns*) "/" name "]]"))

(defn ns-keyword
  "Takes a symbol or string `the-name-sym` and returns a namespaced keyword
  based on that symbol.

  Example: `(ns-keyword 'foo) => :calling.name.space/foo`"
  [the-name-sym]
  (if the-name-sym
    (keyword (str (ns-name *ns*)) (str the-name-sym))
    (c/assertion-violation `ns-keyword "argument must not be nil" the-name-sym)))


;;;; Clojure defrecord interns
(defn ^{:private true}
   maybe-destructured
   [params body]
   (if (every? symbol? params)
     (cons params body)
     (loop [params params
            new-params (with-meta [] (meta params))
            lets []]
       (if params
         (if (symbol? (first params))
           (recur (next params) (conj new-params (first params)) lets)
           (let [gparam (gensym "p__")]
             (recur (next params) (conj new-params gparam)
                    (-> lets (conj (first params)) (conj gparam)))))
         `(~new-params
           (let ~lets
             ~@body))))))

(defn- parse-opts [s]
   (loop [opts {} [k v & rs :as s] s]
     (if (keyword? k)
       (recur (assoc opts k v) rs)
       [opts s])))

(defn- parse-impls [specs]
   (loop [ret {} s specs]
     (if (seq s)
       (recur (assoc ret (first s) (take-while seq? (next s)))
              (drop-while seq? (next s)))
       ret)))

(defn- parse-opts+specs [opts+specs]
   (let [[opts specs] (parse-opts opts+specs)
         impls (parse-impls specs)
         interfaces (-> (map #(if (var? (resolve %))
                                (:on (deref (resolve %)))
                                %)
                             (keys impls))
                        set
                        (disj 'Object 'java.lang.Object)
                        vec)
         methods (map (fn [[name params & body]]
                        (cons name (maybe-destructured params body)))
                      (apply concat (vals impls)))]
     (when-let [bad-opts (seq (remove #{:no-print :load-ns} (keys opts)))]
       (let [^String msg (apply print-str "Unsupported option(s) -" bad-opts)]
         (throw (IllegalArgumentException. msg))))
     [interfaces methods opts]))

(defn- imap-cons
   [^IPersistentMap this o]
   (cond
     (map-entry? o)
     (let [^java.util.Map$Entry pair o]
       (.assoc this (.getKey pair) (.getValue pair)))
     (instance? clojure.lang.IPersistentVector o)
     (let [^clojure.lang.IPersistentVector vec o]
       (.assoc this (.nth vec 0) (.nth vec 1)))
     :else (loop [this this
                  o o]
             (if (seq o)
               (let [^java.util.Map$Entry pair (first o)]
                 (recur (.assoc this (.getKey pair) (.getValue pair)) (rest o)))
               this))))

(defn- emit-defrecord
   "Do not use this directly - use defrecord"
   {:added "1.2"}
   [tagname cname fields interfaces methods opts]
   (let [classname (with-meta (symbol (str (namespace-munge *ns*) "." cname)) (meta cname))
         interfaces (vec interfaces)
         interface-set (set (map resolve interfaces))
         methodname-set (set (map first methods))
         hinted-fields fields
         fields (vec (map #(with-meta % nil) fields))
         base-fields fields
         fields (conj fields '__meta '__extmap
                      '^:unsynchronized-mutable __hash
                      '^:unsynchronized-mutable __hasheq)
         type-hash (hash classname)]
     (when (some #{:volatile-mutable :unsynchronized-mutable} (mapcat (comp keys meta) hinted-fields))
       (throw (IllegalArgumentException. ":volatile-mutable or :unsynchronized-mutable not supported for record fields")))
     (let [gs (gensym)]
       (letfn
           [(irecord [[i m]]
              [(conj i 'clojure.lang.IRecord)
               m])
            (eqhash [[i m]]
              [(conj i 'clojure.lang.IHashEq)
               (conj m
                     `(hasheq [this#] (let [hq# ~'__hasheq]
                                        (if (zero? hq#)
                                          (let [h# (int (bit-xor ~type-hash (clojure.lang.APersistentMap/mapHasheq this#)))]
                                            (set! ~'__hasheq h#)
                                            h#)
                                          hq#)))
                     `(hashCode [this#] (let [hash# ~'__hash]
                                          (if (zero? hash#)
                                            (let [h# (clojure.lang.APersistentMap/mapHash this#)]
                                              (set! ~'__hash h#)
                                              h#)
                                            hash#)))
                     `(equals [this# ~gs] (clojure.lang.APersistentMap/mapEquals this# ~gs)))])
            (iobj [[i m]]
              [(conj i 'clojure.lang.IObj)
               (conj m `(meta [this#] ~'__meta)
                     `(withMeta [this# ~gs] (new ~tagname ~@(replace {'__meta gs} fields))))])
            (ilookup [[i m]]
              [(conj i 'clojure.lang.ILookup 'clojure.lang.IKeywordLookup)
               (conj m `(valAt [this# k#] (.valAt this# k# nil))
                     `(valAt [this# k# else#]
                             (case k# ~@(mapcat (fn [fld] [(keyword fld) fld])
                                                base-fields)
                                   (get ~'__extmap k# else#)))
                     `(getLookupThunk [this# k#]
                                      (let [~'gclass (class this#)]
                                        (case k#
                                          ~@(let [hinted-target (with-meta 'gtarget {:tag tagname})]
                                              (mapcat
                                               (fn [fld]
                                                 [(keyword fld)
                                                  `(reify clojure.lang.ILookupThunk
                                                     (get [~'thunk ~'gtarget]
                                                       (if (identical? (class ~'gtarget) ~'gclass)
                                                         (. ~hinted-target ~(symbol (str "-" fld)))
                                                         ~'thunk)))])
                                               base-fields))
                                          nil))))])
            (imap [[i m]]
              [(conj i 'clojure.lang.IPersistentMap)
               (conj m
                     `(count [this#] (+ ~(count base-fields) (count ~'__extmap)))
                     `(empty [this#] (throw (UnsupportedOperationException. (str "Can't create empty: " ~(str classname)))))
                     `(cons [this# e#] ((var imap-cons) this# e#))
                     `(equiv [this# ~gs]
                             (boolean
                              (or (identical? this# ~gs)
                                  (when (identical? (class this#) (class ~gs))
                                    (let [~gs ~(with-meta gs {:tag tagname})]
                                      (and  ~@(map (fn [fld] `(= ~fld (. ~gs ~(symbol (str "-" fld))))) base-fields)
                                            (= ~'__extmap (. ~gs ~'__extmap))))))))
                     `(containsKey [this# k#] (not (identical? this# (.valAt this# k# this#))))
                     `(entryAt [this# k#] (let [v# (.valAt this# k# this#)]
                                            (when-not (identical? this# v#)
                                              (clojure.lang.MapEntry/create k# v#))))
                     `(seq [this#] (seq (concat [~@(map #(list `clojure.lang.MapEntry/create (keyword %) %) base-fields)]
                                                ~'__extmap)))
                     `(iterator [~gs]
                                (clojure.lang.RecordIterator. ~gs [~@(map keyword base-fields)] (RT/iter ~'__extmap)))
                     `(assoc [this# k# ~gs]
                             (condp identical? k#
                               ~@(mapcat (fn [fld]
                                           [(keyword fld) (list* `new tagname (replace {fld gs} (remove '#{__hash __hasheq} fields)))])
                                         base-fields)
                               (new ~tagname ~@(remove '#{__extmap __hash __hasheq} fields) (assoc ~'__extmap k# ~gs))))
                     `(without [this# k#] (if (contains? #{~@(map keyword base-fields)} k#)
                                            (dissoc (with-meta (into {} this#) ~'__meta) k#)
                                            (new ~tagname ~@(remove '#{__extmap __hash __hasheq} fields)
                                                 (not-empty (dissoc ~'__extmap k#))))))])
            (ijavamap [[i m]]
              [(conj i 'java.util.Map 'java.io.Serializable)
               (conj m
                     `(size [this#] (.count this#))
                     `(isEmpty [this#] (= 0 (.count this#)))
                     `(containsValue [this# v#] (boolean (some #{v#} (vals this#))))
                     `(get [this# k#] (.valAt this# k#))
                     `(put [this# k# v#] (throw (UnsupportedOperationException.)))
                     `(remove [this# k#] (throw (UnsupportedOperationException.)))
                     `(putAll [this# m#] (throw (UnsupportedOperationException.)))
                     `(clear [this#] (throw (UnsupportedOperationException.)))
                     `(keySet [this#] (set (keys this#)))
                     `(values [this#] (vals this#))
                     `(entrySet [this#] (set this#)))])
            (maybe-add-eqhash [[i m]]
              (if (some #{'clojure.lang.IHashEq} i)
                [i m]
                (eqhash [i m])))
            (maybe-add-imap [[i m]]
              (if (= true (:no-map-protocol? opts))
                [i m]
                (imap [i m])))
            (maybe-add-ijavamap [[i m]]
              (if (= true (:no-map-protocol? opts))
                [i m]
                (ijavamap [i m])))]
         (let [[i m] (-> [interfaces methods] irecord maybe-add-eqhash iobj ilookup maybe-add-imap maybe-add-ijavamap)]
           `(deftype* ~(symbol (name (ns-name *ns*)) (name tagname)) ~classname
              ~(conj hinted-fields '__meta '__extmap
                     '^int ^:unsynchronized-mutable __hash
                     '^int ^:unsynchronized-mutable __hasheq)
              :implements ~(vec i)
              ~@(mapcat identity opts)
              ~@m))))))

(defn- build-positional-factory
   "Used to build a positional factory for a given type/record.  Because of the
  limitation of 20 arguments to Clojure functions, this factory needs to be
  constructed to deal with more arguments.  It does this by building a straight
  forward type/record ctor call in the <=20 case, and a call to the same
  ctor pulling the extra args out of the & overage parameter.  Finally, the
  arity is constrained to the number of expected fields and an ArityException
  will be thrown at runtime if the actual arg count does not match."
   [nom classname fields]
   (let [fn-name (symbol (str '-> nom))
         [field-args over] (split-at 20 fields)
         field-count (count fields)
         arg-count (count field-args)
         over-count (count over)
         docstring (str "Positional factory function for class " classname ".")]
     `(defn ~fn-name
        ~docstring
        [~@field-args ~@(if (seq over) '[& overage] [])]
        ~(if (seq over)
           `(if (= (count ~'overage) ~over-count)
              (new ~classname
                   ~@field-args
                   ~@(for [i (range 0 (count over))]
                       (list `nth 'overage i)))
              (throw (clojure.lang.ArityException. (+ ~arg-count (count ~'overage)) (name '~fn-name))))
           `(new ~classname ~@field-args)))))

(defn- validate-fields
   ""
   [fields name]
   (let [specials '#{__meta __hash __hasheq __extmap}]
     (when (some specials fields)
       (throw (AssertionError. (str "The names in " specials " cannot be used as field names for types or records."))))))

;;;; END OF Clojure defrecord interns


(defn emit-java-record-definition
   [?type ?options ?constructor ?constructor-args ?predicate ?field-triples ?opt+specs]
   (let [?docref (str "See " (reference ?constructor) ".")
         record-meta `(make-record-meta ~?predicate ~?constructor ~?constructor-args ~?field-triples)
         ?constructor-args-set (set ?constructor-args)]
     `(do
        (declare ~@(map (fn [[?field ?accessor ?lens]] ?accessor) ?field-triples))
        ~(let [fields (mapv first ?field-triples)]
           (validate-fields fields ?type)
           (let [[interfaces methods opts] (parse-opts+specs ?opt+specs)
                 opts (merge opts ?options)
                 ns-part (namespace-munge *ns*)
                 classname (symbol (str ns-part "." ?type))
                 hinted-fields fields
                 fields (vec (map #(with-meta % nil) fields))]
             `(let []
                (declare ~(symbol (str  '-> ?type)))
                (declare ~(symbol (str 'map-> ?type)))
                ~(emit-defrecord ?type ?type (vec hinted-fields) (vec interfaces) methods opts)
                (import ~classname)
                (when-not (:no-arrow-constructor? ~?options)
                  ~(build-positional-factory ?type classname fields))
                (defn ~(symbol (str 'map-> ?type))
                  ~(str "Factory function for class " classname ", taking a map of keywords to field values.")
                  ([m#] (~(symbol (str classname "/create"))
                         (if (instance? clojure.lang.MapEquivalence m#) m# (into {} m#)))))
                ~classname)))

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
        ~@(mapcat (fn [[?field ?accessor ?lens]]
                    (let [?rec (with-meta `rec# {:tag ?type})
                          ?data `data#
                          ?v `v#]
                      `((def ~(document-with-arglist
                               ?accessor
                               (vector ?type)
                               (str "Lens for the `" ?field "` field"
                                    (name-doc ?field)
                                    " from a [[" ?type "]] record. " ?docref))
                          (lens/lens (fn [~?rec]
                                       ;; (check-type ~?type ~?rec) ; Not needed anymore
                                       (. ~?rec ~(symbol (str "-" ?field))))
                                     (fn [~?data ~?v]
                                       (~?constructor ~@(map
                                                         (fn [[?shove-field ?shove-accessor]]
                                                           (if (= ?field ?shove-field)
                                                             ?v
                                                             `(~?shove-accessor ~?data)))
                                                         ?field-triples)))))
                        ~(when ?lens
                           (report-lens-deprecation ?type)
                           `(def ~?lens ~?accessor))
                        )))
                  ?field-triples)
        ;; Specs
        ~(when-let [spec-name (:spec ?options)]
           `(do
              ;; Spec for a record type
              (spec/def ~spec-name
                (spec/and ~?predicate
                          ~@(map (fn [[?field ?accessor _]]
                                   `#(spec/valid? ~(name-spec ?field) (~?accessor %)))
                                 ?field-triples)))
              ;; Spec for constructor function
              ~(let [c-specs (mapcat (fn [constructor-arg]
                                       (let [field (first (filter #(= constructor-arg %)
                                                                  (map first ?field-triples)))]
                                         [(keyword constructor-arg) (name-spec field)]))
                                     ?constructor-args)]
                 `(spec/fdef ~?constructor
                    :args (spec/cat ~@c-specs)
                    :ret ~spec-name))))
        ;; When `no-map-protocol?` option is given, we have to provide a print-method implementation
        ~(when (:no-map-protocol? ?options)
           (let [w (vary-meta `w# assoc :tag 'java.io.Writer)
                 v `w#]
             `(defmethod print-method ~?type [~v ~w]
                (.write ~w (str ~(str "#" *ns* "." ?type)
                                (into {} ~(mapv (fn [[?field ?accessor _]]
                                                  `(vector ~(keyword ?field) (~?accessor ~v)))
                                                ?field-triples))))))))))
