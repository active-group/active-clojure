(ns active.clojure.cljs.record-cljs-internals
  (:require [active.clojure.lens :as lens]
            [cljs.compiler :as comp]
            #?(:cljs [cljs.core :as core])
            #?(:cljs [cljs.analyzer :as ana]))
  #?(:cljs (:require-macros [cljs.core :as core])))

#?(:clj (alias 'core 'clojure.core))


;;;; CLJS internals

(core/defn- validate-fields
  [case name fields]
  (core/when-not (vector? fields)
    (throw
     #?(:clj (AssertionError. (core/str case " " name ", no fields vector given."))
        :cljs (js/Error. (core/str case " " name ", no fields vector given."))))))

(core/defn- build-map-factory [rsym rname fields]
  (core/let [fn-name (with-meta (symbol (core/str 'map-> rsym))
                       (assoc (meta rsym) :factory :map))
             ms (gensym)
             ks (map keyword fields)
             getters (map (core/fn [k] `(~k ~ms)) ks)]
    `(defn ~fn-name [~ms]
       (new ~rname ~@getters nil (not-empty (dissoc ~ms ~@ks)) nil))))

(core/defn- build-positional-factory
  [rsym rname fields]
  (core/let [fn-name (with-meta (symbol (core/str '-> rsym))
                       (assoc (meta rsym) :factory :positional))
             field-values (if (core/-> rsym meta :internal-ctor) (conj fields nil nil nil) fields)]
    `(defn ~fn-name
       [~@fields]
       (new ~rname ~@field-values))))

(core/defn- annotate-specs [annots v [f sigs]]
  (conj v
        (vary-meta (cons f (map #(cons (second %) (nnext %)) sigs))
                   merge annots)))

(core/defn dt->et
  ([type specs fields]
   (dt->et type specs fields false))
  ([type specs fields inline]
   (core/let [annots {:cljs.analyzer/type type
                      :cljs.analyzer/protocol-impl true
                      :cljs.analyzer/protocol-inline inline}]
     (core/loop [ret [] specs specs]
       (if (seq specs)
         (core/let [p     (first specs)
                    ret   (core/-> (conj ret p)
                                   (into (reduce (partial annotate-specs annots) []
                                                 (group-by first (take-while seq? (next specs))))))
                    specs (drop-while seq? (next specs))]
           (recur ret specs))
         ret)))))

(core/defn- collect-protocols [impls env]
  (core/->> impls
            (filter core/symbol?)
            (map #(:name (cljs.analyzer/resolve-var (dissoc env :locals) %)))
            (into #{})))

(def fast-path-protocols
  "protocol fqn -> [partition number, bit]"
  (zipmap (map #(symbol "cljs.core" (core/str %))
               '[IFn ICounted IEmptyableCollection ICollection IIndexed ASeq ISeq INext
                 ILookup IAssociative IMap IMapEntry ISet IStack IVector IDeref
                 IDerefWithTimeout IMeta IWithMeta IReduce IKVReduce IEquiv IHash
                 ISeqable ISequential IList IRecord IReversible ISorted IPrintWithWriter IWriter
                 IPrintWithWriter IPending IWatchable IEditableCollection ITransientCollection
                 ITransientAssociative ITransientMap ITransientVector ITransientSet
                 IMultiFn IChunkedSeq IChunkedNext IComparable INamed ICloneable IAtom
                 IReset ISwap IIterable])
          (iterate (core/fn [[p b]]
                     (if (core/== 2147483648 b)
                       [(core/inc p) 1]
                       [p #?(:clj  (core/bit-shift-left b 1)
                             :cljs (core/* 2 b))]))
                   [0 1])))

(def fast-path-protocol-partitions-count
  "total number of partitions"
  (core/let [c (count fast-path-protocols)
             m (core/mod c 32)]
    (if (core/zero? m)
      (core/quot c 32)
      (core/inc (core/quot c 32)))))

(core/defn- resolve-var [env sym]
  (core/let [ret (:name (cljs.analyzer/resolve-var env sym))]
    (core/assert ret (core/str "Can't resolve: " sym))
    ret))


(core/defn- ->impl-map [impls]
  (core/loop [ret {} s impls]
    (if (seq s)
      (recur (assoc ret (first s) (take-while seq? (next s)))
             (drop-while seq? (next s)))
      ret)))

(core/defn- prepare-protocol-masks [env impls]
  (core/let [resolve  (partial resolve-var env)
             impl-map (->impl-map impls)
             fpp-pbs  (seq
                        (keep fast-path-protocols
                          (map resolve
                            (keys impl-map))))]
    (if fpp-pbs
      (core/let [fpps  (into #{}
                         (filter (partial contains? fast-path-protocols)
                           (map resolve (keys impl-map))))
                 parts (core/as-> (group-by first fpp-pbs) parts
                         (into {}
                           (map (juxt key (comp (partial map peek) val))
                             parts))
                         (into {}
                           (map (juxt key (comp (partial reduce core/bit-or) val))
                             parts)))]
        [fpps (reduce (core/fn [ps p] (update-in ps [p] (core/fnil identity 0)))
                parts
                (range fast-path-protocol-partitions-count))]))))

(core/defn- to-property [sym]
  (symbol (core/str "-" sym)))

(core/defn- emit-defrecord
  "Do not use this directly - use defrecord"
  [env tagname rname fields impls]
  (core/let [hinted-fields fields
             fields (vec (map #(with-meta % nil) fields))
             base-fields fields
             pr-open (core/str "#" #?(:clj  (.getNamespace rname)
                                      :cljs (namespace rname))
                               "." #?(:clj  (.getName rname)
                                      :cljs (name rname))
                               "{")
             fields (conj fields '__meta '__extmap (with-meta '__hash {:mutable true}))]
    (core/let [gs (gensym)
               ksym (gensym "k")
               impls (concat
                      impls
                      ['IRecord
                       'ICloneable
                       `(~'-clone [this#] (new ~tagname ~@fields))
                       'IHash
                       `(~'-hash [this#]
                         (cljs.core/caching-hash this#
                                                 (fn [coll#]
                                                   (bit-xor
                                                    ~(hash (core/-> rname comp/munge core/str))
                                                    (hash-unordered-coll coll#)))
                                                 ~'__hash))
                       'IEquiv
                       (core/let [this (gensym 'this) other (gensym 'other)]
                         `(~'-equiv [~this ~other]
                           (and (some? ~other)
                                (identical? (.-constructor ~this)
                                            (.-constructor ~other))
                                ~@(map (core/fn [field]
                                         `(= (.. ~this ~(to-property field))
                                             (.. ~other ~(to-property field))))
                                       base-fields)
                                (= (.-__extmap ~this)
                                   (.-__extmap ~other)))))
                       'IMeta
                       `(~'-meta [this#] ~'__meta)
                       'IWithMeta
                       `(~'-with-meta [this# ~gs] (new ~tagname ~@(replace {'__meta gs} fields)))
                       'ILookup
                       `(~'-lookup [this# k#] (cljs.core/-lookup this# k# nil))
                       `(~'-lookup [this# ~ksym else#]
                         (case ~ksym
                           ~@(mapcat (core/fn [f] [(keyword f) f]) base-fields)
                           (cljs.core/get ~'__extmap ~ksym else#)))
                       'ICounted
                       `(~'-count [this#] (+ ~(count base-fields) (count ~'__extmap)))
                       'ICollection
                       `(~'-conj [this# entry#]
                         (if (vector? entry#)
                           (cljs.core/-assoc this# (cljs.core/-nth entry# 0) (cljs.core/-nth entry# 1))
                           (reduce cljs.core/-conj
                                   this#
                                   entry#)))
                       'IAssociative
                       `(~'-assoc [this# k# ~gs]
                         (condp cljs.core/keyword-identical? k#
                           ~@(mapcat (core/fn [fld]
                                       [(keyword fld) (list* `new tagname (replace {fld gs '__hash nil} fields))])
                                     base-fields)
                           (new ~tagname ~@(remove #{'__extmap '__hash} fields) (assoc ~'__extmap k# ~gs) nil)))
                       'IMap
                       `(~'-dissoc [this# k#] (if (contains? #{~@(map keyword base-fields)} k#)
                                                (dissoc (cljs.core/-with-meta (into {} this#) ~'__meta) k#)
                                                (new ~tagname ~@(remove #{'__extmap '__hash} fields)
                                                     (not-empty (dissoc ~'__extmap k#))
                                                     nil)))
                       'ISeqable
                       `(~'-seq [this#] (seq (concat [~@(map #(core/list `vector (keyword %) %) base-fields)]
                                                     ~'__extmap)))

                       'IIterable
                       `(~'-iterator [~gs]
                         (RecordIter. 0 ~gs ~(count base-fields) [~@(map keyword base-fields)]
                                      (if ~'__extmap
                                        (cljs.core/-iterator ~'__extmap)
                                        (core/nil-iter))))

                       'IPrintWithWriter
                       `(~'-pr-writer [this# writer# opts#]
                         (let [pr-pair# (fn [keyval#] (cljs.core/pr-sequential-writer writer# cljs.core/pr-writer "" " " "" opts# keyval#))]
                           (cljs.core/pr-sequential-writer
                            writer# pr-pair# ~pr-open ", " "}" opts#
                            (concat [~@(map #(core/list `vector (keyword %) %) base-fields)]
                                    ~'__extmap))))
                       ])
               [fpps pmasks] (prepare-protocol-masks env impls)
               protocols (collect-protocols impls env)
               tagname (vary-meta tagname assoc
                         :protocols protocols
                         :skip-protocol-flag fpps)]
      `(do
         (~'defrecord* ~tagname ~hinted-fields ~pmasks
           (extend-type ~tagname ~@(dt->et tagname impls fields true)))))))


;;;; END CLJS internals

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


(defn emit-javascript-record-definition
  [env ?type ?options ?constructor ?constructor-args ?predicate ?field-triples ?opt+specs]
  (let [?docref               (str "See " (reference ?constructor) ".")
        ?constructor-args-set (set ?constructor-args)
        fields                (mapv first ?field-triples)
        rsym                  (vary-meta ?type assoc :internal-ctor true)
        r                     (vary-meta
                               (:name (cljs.analyzer/resolve-var (dissoc env :locals) rsym))
                               assoc :internal-ctor true)]
    (validate-fields "defrecord" ?type fields)
    `(do
       ;; direct use of `defrecord` - to be replaced in the future
       ;; (defrecord ~?type ~fields ~@?opt+specs)

       ~(emit-defrecord env rsym r fields ?opt+specs)
       (set! (.-getBasis ~r) (fn [] '[~@fields]))
       (set! (.-cljs$lang$type ~r) true)
       (set! (.-cljs$lang$ctorPrSeq ~r) (fn [this#] (list ~(str r))))
       (set! (.-cljs$lang$ctorPrWriter ~r) (fn [this# writer#] (cljs.core/-write writer# ~(str r))))
       ~(build-positional-factory rsym r fields)
       ~(build-map-factory rsym r fields)
       ~r

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
