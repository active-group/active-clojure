(ns active.clojure.cljs.record-cljs-internals
  (:require [active.clojure.lens :as lens]))



(defn- validate-fields
  [case name fields]
  (when-not (vector? fields)
    #?(:cljs (throw (js/Error. (str case " " name ", no fields vector given."))))))

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
      any?))

(defn- reference
  [name]
  (str "[[" (ns-name *ns*) "/" name "]]"))


(defn report-lens-deprecation [type]
  (println (str "active.clojure.record WARNING for record-type `" type
                "`: the explicit definition of lenses is deprecated in favor of regular "
                "accessors already being lenses")))

;;;; CLJS internals

(defn- build-map-factory [rsym rname fields]
  (let [fn-name (with-meta (symbol (str 'map-> rsym))
                       (assoc (meta rsym) :factory :map))
             docstring (str "Factory function for " rname ", taking a map of keywords to field values.")
             ms (gensym)
             ks (map keyword fields)
             getters (map (fn [k] `(~k ~ms)) ks)]
    `(defn ~fn-name ~docstring [~ms]
       (let [extmap# (cond->> (dissoc ~ms ~@ks)
                       (record? ~ms) (into {}))]
         (new ~rname ~@getters nil (not-empty extmap#) nil)))))

(defn- build-positional-factory
  [rsym rname fields]
  (let [fn-name (with-meta (symbol (str '-> rsym))
                       (assoc (meta rsym) :factory :positional))
             docstring (str "Positional factory function for " rname ".")
             field-values (if (-> rsym meta :internal-ctor) (conj fields nil nil nil) fields)]
    `(defn ~fn-name
       ~docstring
       [~@fields]
       (new ~rname ~@field-values))))

(defn- annotate-specs [annots v [f sigs]]
  (conj v
        (vary-meta (cons f (map #(cons (second %) (nnext %)) sigs))
                   merge annots)))

(defn dt->et
  ([type specs fields]
   (dt->et type specs fields false))
  ([type specs fields inline]
   (let [annots {:cljs.analyzer/type type
                 :cljs.analyzer/protocol-impl true
                 :cljs.analyzer/protocol-inline inline}]
     ;; loop
     (loop [ret [] specs specs]
       (if (seq specs)
         ;; let
         (let [p     (first specs)
               ret   (-> (conj ret p)
                         (into (reduce (partial annotate-specs annots) []
                                       (group-by first (take-while seq? (next specs))))))
               specs (drop-while seq? (next specs))]
           (recur ret specs))
         ret)))))

(defn- collect-protocols [impls env]
  (->> impls
       (filter symbol?)
       (map #(:name (cljs.analyzer/resolve-var (dissoc env :locals) %)))
       (into #{})))

(def fast-path-protocols
  "protocol fqn -> [partition number, bit]"
  (zipmap (map #(symbol "cljs.core" (str %))
               '[IFn ICounted IEmptyableCollection ICollection IIndexed ASeq ISeq INext
                 ILookup IAssociative IMap IMapEntry ISet IStack IVector IDeref
                 IDerefWithTimeout IMeta IWithMeta IReduce IKVReduce IEquiv IHash
                 ISeqable ISequential IList IRecord IReversible ISorted IPrintWithWriter IWriter
                 IPrintWithWriter IPending IWatchable IEditableCollection ITransientCollection
                 ITransientAssociative ITransientMap ITransientVector ITransientSet
                 IMultiFn IChunkedSeq IChunkedNext IComparable INamed ICloneable IAtom
                 IReset ISwap IIterable])
          (iterate (fn [[p b]]
                     (if (== 2147483648 b)
                       [(inc p) 1]
                       [p #?(:clj  (bit-shift-left b 1)
                             :cljs (* 2 b))]))
                   [0 1])))

(def fast-path-protocol-partitions-count
  "total number of partitions"
  (let [c (count fast-path-protocols)
             m (mod c 32)]
    (if (zero? m)
      (quot c 32)
      (inc (quot c 32)))))

(defn- resolve-var [env sym]
  (let [ret (:name (cljs.analyzer/resolve-var env sym))]
    (assert ret (str "Can't resolve: " sym))
    ret))


(defn- ->impl-map [impls]
  (loop [ret {} s impls]
    (if (seq s)
      (recur (assoc ret (first s) (take-while seq? (next s)))
             (drop-while seq? (next s)))
      ret)))

(defn- prepare-protocol-masks [env impls]
  (let [resolve  (partial resolve-var env)
             impl-map (->impl-map impls)
             fpp-pbs  (seq
                        (keep fast-path-protocols
                          (map resolve
                            (keys impl-map))))]
    (if fpp-pbs
      (let [fpps  (into #{}
                         (filter (partial contains? fast-path-protocols)
                           (map resolve (keys impl-map))))
                 parts (as-> (group-by first fpp-pbs) parts
                         (into {}
                           (map (juxt key (comp (partial map peek) val))
                             parts))
                         (into {}
                           (map (juxt key (comp (partial reduce bit-or) val))
                             parts)))]
        [fpps (reduce (fn [ps p] (update-in ps [p] (fnil identity 0)))
                parts
                (range fast-path-protocol-partitions-count))]))))

(defn- to-property [sym]
  (symbol (str "-" sym)))

(defn- emit-defrecord
  "Do not use this directly - use defrecord"
  [env tagname rname fields impls]
  (let [hinted-fields fields
        fields (vec (map #(with-meta % nil) fields))
        base-fields fields
        pr-open (str "#" #?(:clj  (.getNamespace rname)
                            :cljs (namespace rname))
                     "." #?(:clj  (.getName rname)
                            :cljs (name rname))
                     "{")
        fields (conj fields '__meta '__extmap (with-meta '__hash {:mutable true}))]
    (let [gs (gensym)
          ksym (gensym "k")
          impls (concat
                 impls
                 ['IRecord
                  'ICloneable
                  `(~'-clone [this#] (new ~tagname ~@fields))
                  'IHash
                  `(~'-hash [this#]
                    (caching-hash this#
                                  (fn [coll#]
                                    (bit-xor
                                     ~(hash (-> rname cljs.compiler/munge str))
                                     (hash-unordered-coll coll#)))
                                  ~'__hash))
                  'IEquiv
                  (let [this (gensym 'this) other (gensym 'other)]
                    `(~'-equiv [~this ~other]
                      (and (some? ~other)
                           (identical? (.-constructor ~this)
                                       (.-constructor ~other))
                           ~@(map (fn [field]
                                    `(= (.. ~this ~(to-property field))
                                        (.. ~(with-meta other {:tag tagname}) ~(to-property field))))
                                  base-fields)
                           (= (.-__extmap ~this)
                              (.-__extmap ~(with-meta other {:tag tagname}))))))
                  'IMeta
                  `(~'-meta [this#] ~'__meta)
                  'IWithMeta
                  `(~'-with-meta [this# ~gs] (new ~tagname ~@(replace {'__meta gs} fields)))
                  'ILookup
                  `(~'-lookup [this# k#] (-lookup this# k# nil))
                  `(~'-lookup [this# ~ksym else#]
                    (case ~ksym
                      ~@(mapcat (fn [f] [(keyword f) f]) base-fields)
                      (cljs.get ~'__extmap ~ksym else#)))
                  'ICounted
                  `(~'-count [this#] (+ ~(count base-fields) (count ~'__extmap)))
                  'ICollection
                  `(~'-conj [this# entry#]
                    (if (vector? entry#)
                      (-assoc this# (-nth entry# 0) (-nth entry# 1))
                      (reduce -conj
                              this#
                              entry#)))
                  'IAssociative
                  `(~'-assoc [this# k# ~gs]
                    (condp keyword-identical? k#
                      ~@(mapcat (fn [fld]
                                  [(keyword fld) (list* `new tagname (replace {fld gs '__hash nil} fields))])
                                base-fields)
                      (new ~tagname ~@(remove #{'__extmap '__hash} fields) (assoc ~'__extmap k# ~gs) nil)))
                  'IMap
                  `(~'-dissoc [this# k#] (if (contains? #{~@(map keyword base-fields)} k#)
                                           (dissoc (-with-meta (into {} this#) ~'__meta) k#)
                                           (new ~tagname ~@(remove #{'__extmap '__hash} fields)
                                                (not-empty (dissoc ~'__extmap k#))
                                                nil)))
                  'ISeqable
                  `(~'-seq [this#] (seq (concat [~@(map #(list 'cljs.MapEntry. (keyword %) % nil) base-fields)]
                                                ~'__extmap)))

                  'IIterable
                  `(~'-iterator [~gs]
                    (RecordIter. 0 ~gs ~(count base-fields) [~@(map keyword base-fields)] (if ~'__extmap
                                                                                            (-iterator ~'__extmap)
                                                                                            (nil-iter))))

                  'IPrintWithWriter
                  `(~'-pr-writer [this# writer# opts#]
                    (let [pr-pair# (fn [keyval#] (pr-sequential-writer writer# (~'js* "cljs.core.pr_writer") "" " " "" opts# keyval#))]
                      (pr-sequential-writer
                       writer# pr-pair# ~pr-open ", " "}" opts#
                       (concat [~@(map #(list `vector (keyword %) %) base-fields)]
                               ~'__extmap))))
                  'IKVReduce
                  `(~'-kv-reduce [this# f# init#]
                    (reduce (fn [ret# [k# v#]] (f# ret# k# v#)) init# this#))
                  ])
          [fpps pmasks] (prepare-protocol-masks env impls)
          protocols (collect-protocols impls env)
          tagname (vary-meta tagname assoc
                             :protocols protocols
                             :skip-protocol-flag fpps)]
      `(do
         (~'defrecord* ~tagname ~hinted-fields ~pmasks
          (extend-type ~tagname ~@(dt->et tagname impls fields true)))))))


(defn emit-javascript-record-definition
  [env ?type ?options ?constructor ?constructor-args ?predicate ?field-triples ?opt+specs]
  (let [?docref               (str "See " (reference ?constructor) ".")
        ?constructor-args-set (set ?constructor-args)
        fields                (mapv first ?field-triples)]
    (validate-fields "defrecord" ?type fields)
    `(do
       ;; direct use of `defrecord` - to be replaced in the future
       (defrecord ~?type [~@fields] ~@?opt+specs)

       (def ~?predicate (fn [x#] (instance? ~?type x#)))
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
                                      (when-not (instance? ~?type ~?rec)
                                        (throw (js/Error. ~(str "Wrong record type (" ?rec ") passed to accessor ("
                                                                ?accessor ")."))))
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
                 ?field-triples))))
