(ns active.clojure.record
  #?@
  (:clj
   [(:require
     [active.clojure.lens :as lens]
     [active.clojure.condition :as c]
     [active.clojure.macro :refer [if-cljs]])
    (:import clojure.lang.IPersistentMap)
    (:import clojure.lang.RT)
    (:import java.lang.IllegalArgumentException)
    ]
   :cljs
   [(:require
     [active.clojure.lens :as lens]
     [active.clojure.condition :as c])
    (:require-macros [active.clojure.macro :refer [if-cljs]])]))

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
      ]
     (let [[i m] (-> [interfaces methods] irecord eqhash iobj ilookup imap ijavamap)]
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
  (when-not (vector? fields)
    (throw (AssertionError. "No fields vector given.")))
  (let [specials '#{__meta __hash __hasheq __extmap}]
    (when (some specials fields)
      (throw (AssertionError. (str "The names in " specials " cannot be used as field names for types or records.")))))
  (let [non-syms (remove symbol? fields)]
    (when (seq non-syms)
      (throw (clojure.lang.Compiler$CompilerException.
              *file*
              (.deref clojure.lang.Compiler/LINE)
              (.deref clojure.lang.Compiler/COLUMN)
              (AssertionError.
               (str "defrecord and deftype fields must be symbols, "
                    *ns* "." name " had: "
                    (apply str (interpose ", " non-syms)))))))))

(defn throw-illegal-argument-exception
  [msg]
  (c/assertion-violation `throw-illegal-argument-exception "Illegal argument" msg))

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
    (throw (throw-illegal-argument-exception (str "constructor call must be a list in " *ns* " " (meta &form)))))
  (when-not (vector? ?field-specs)
    (throw (throw-illegal-argument-exception (str "field specs must be a vector in " *ns* " " (meta &form)))))
  (when-not (even? (count (remove seq? ?field-specs)))
    (throw (throw-illegal-argument-exception (str "odd number of elements in field specs in " *ns* " " (meta &form)))))
  (when-not (every? true? (map #(= 3 (count %1)) (filter seq? ?field-specs)))
    (throw (throw-illegal-argument-exception (str "wrong number of elements in field specs with lens in " *ns* " " (meta &form)))))

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
                                  (throw-illegal-argument-exception (str "invalid field spec " spec " in " *ns* " " (meta &form))))
                                (recur (rest specs) (list* spec triples)))

                              (symbol? spec)
                              (do
                                (when (empty? (rest specs))
                                  (throw (throw-illegal-argument-exception (str "incomplete field spec for " spec " in " *ns* " " (meta &form)))))
                                (when-not (symbol? (fnext specs))
                                  (throw (throw-illegal-argument-exception (str "invalid accessor " (fnext specs) " for " spec " in " *ns* " " (meta &form)))))
                                (recur (nnext specs)
                                       (list* [spec (fnext specs) nil] triples)))

                              :else
                              (throw (throw-illegal-argument-exception (str "invalid field spec " spec " in " *ns* " " (meta &form))))))))

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
          (throw (throw-illegal-argument-exception (str "constructor argument " ?constructor-arg " is not a field in " *ns* " " (meta &form)))))))

    `(do
       ~(let [name ?type
              fields (mapv first ?field-triples)
              opts+specs ?opt+specs]
          (validate-fields fields name)
          (let [gname name
                [interfaces methods opts] (parse-opts+specs opts+specs)
                ns-part (namespace-munge *ns*)
                classname (symbol (str ns-part "." gname))
                hinted-fields fields
                fields (vec (map #(with-meta % nil) fields))]
            `(let []
               (declare ~(symbol (str  '-> gname)))
               (declare ~(symbol (str 'map-> gname)))
               ~(emit-defrecord name gname (vec hinted-fields) (vec interfaces) methods opts)
               (import ~classname)
               ~(build-positional-factory gname classname fields)
               (defn ~(symbol (str 'map-> gname))
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
