(ns ^:no-doc active.clojure.cljs.record-cljs-internals
  (:require [active.clojure.lens :as lens]
            [cljs.compiler :as comp]
            #?(:cljs [cljs.core :as core])
            #?(:cljs [cljs.analyzer :as ana])
            #?(:cljs [cljs.spec.alpha])
            [active.clojure.record-helper :as r-help])
  #?(:cljs (:require-macros [cljs.core :as core]
                            [active.clojure.record-helper :as r-help])))

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
             docstring (core/str "Factory function for " rname ", taking a map of keywords to field values.")
             ms (gensym)
             ks (map keyword fields)
             getters (map (core/fn [k] `(~k ~ms)) ks)]
    `(defn ~fn-name ~docstring [~ms]
       (let [extmap# (cond->> (dissoc ~ms ~@ks)
                       (record? ~ms) (into {}))]
         (new ~rname ~@getters nil (not-empty extmap#) nil)))))

(core/defn- build-positional-factory
  [rsym rname fields]
  (core/let [fn-name (with-meta (symbol (core/str '-> rsym))
                       (assoc (meta rsym) :factory :positional))
             docstring (core/str "Positional factory function for " rname ".")
             field-values (if (core/-> rsym meta :internal-ctor) (conj fields nil nil nil) fields)]
    `(defn ~fn-name
       ~docstring
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

(defn- override-default-methods
  [default-interfaces+methods provided-interfaces+methods]
  (into {}
        (for [[i ms] default-interfaces+methods]
          (if-let [new-methods (get provided-interfaces+methods i)]
            [i
             ;; Remove methods that are provided and concat the provided ones
             (concat (remove (fn [[n & rest]]
                               (some #(= n %)
                                     (map first new-methods)))
                             ms)
                     new-methods)]
            [i ms]))))

(defn- add-provided-interfaces+methods
  [default-interfaces+methods provided-interfaces+methods]
  (merge default-interfaces+methods
         (into {}
               (remove (fn [[i ms]]
                         (get default-interfaces+methods i))
                       provided-interfaces+methods))))

(core/defn- emit-defrecord
  "Do not use this directly - use defrecord"
  [options env tagname rname fields impls]
  (core/let [hinted-fields fields
             fields (vec (map #(with-meta % nil) fields))
             base-fields fields
             pr-open (core/str "#" (namespace rname)
                               "." (name rname)
                               "{")
             fields (conj fields '__meta '__extmap (with-meta '__hash {:mutable true}))]
    (core/let [gs (gensym)
               ksym (gensym "k")
               default-interfaces+methods
               {'IEquiv
                [(core/let [this (gensym 'this) other (with-meta (gensym 'other) {:tag '`~rname})]
                    `(~'-equiv [~this ~other]
                      (and (some? ~other)
                           (identical? (.-constructor ~this)
                                       (.-constructor ~other))
                           ~@(map (core/fn [field]
                                    `(= (.. ~this ~(to-property field))
                                        (.. ~other ~(to-property field))))
                                  base-fields)
                           (= (.-__extmap ~this)
                              (.-__extmap ~other)))))]

                'IAssociative
                [`(~'-assoc [this# k# ~gs]
                   (condp cljs.core/keyword-identical? k#
                     ~@(mapcat (core/fn [fld]
                                 [(keyword fld) (list* `new tagname (replace {fld gs '__hash nil} fields))])
                               base-fields)
                     (new ~tagname ~@(remove #{'__extmap '__hash} fields) (assoc ~'__extmap k# ~gs) nil)))]

                'IMap
                [`(~'-dissoc [this# k#] (if (contains? #{~@(map keyword base-fields)} k#)
                                          (dissoc (cljs.core/-with-meta (into {} this#) ~'__meta) k#)
                                          (new ~tagname ~@(remove #{'__extmap '__hash} fields)
                                               (not-empty (dissoc ~'__extmap k#))
                                               nil)))]

                'IRecord []

                'ICloneable
                [`(~'-clone [this#] (new ~tagname ~@fields))]

                'IHash
                [`(~'-hash [this#]
                   (cljs.core/caching-hash this#
                                           (fn [coll#]
                                             (bit-xor
                                              ~(hash (core/-> rname comp/munge core/str))
                                              (hash-unordered-coll coll#)))
                                           ~'__hash))]

                'IMeta
                [`(~'-meta [this#] ~'__meta)]

                'IWithMeta
                [`(~'-with-meta [this# ~gs] (new ~tagname ~@(replace {'__meta gs} fields)))]

                'ILookup
                [`(~'-lookup [this# k#] (cljs.core/-lookup this# k# nil))
                 `(~'-lookup [this# ~ksym else#]
                   (case ~ksym
                     ~@(mapcat (core/fn [f] [(keyword f) f]) base-fields)
                     (cljs.core/get ~'__extmap ~ksym else#)))]

                'ICounted
                [`(~'-count [this#] (+ ~(count base-fields) (count ~'__extmap)))]

                'ICollection
                [`(~'-conj [this# entry#]
                   (if (vector? entry#)
                     (cljs.core/-assoc this# (cljs.core/-nth entry# 0) (cljs.core/-nth entry# 1))
                     (reduce cljs.core/-conj
                             this#
                             entry#)))]

                'ISeqable
                [`(~'-seq [this#] (seq (concat [~@(map #(core/list 'cljs.core/MapEntry. (keyword %) % nil) base-fields)]
                                               ~'__extmap)))]

                'IIterable
                [`(~'-iterator [~gs]
                   (RecordIter. 0 ~gs ~(count base-fields) [~@(map keyword base-fields)]
                                (if ~'__extmap
                                  (cljs.core/-iterator ~'__extmap)
                                  (core/nil-iter))))]

                'IPrintWithWriter
                [`(~'-pr-writer [this# writer# opts#]
                   (let [pr-pair# (fn [keyval#] (cljs.core/pr-sequential-writer writer# cljs.core/pr-writer "" " " "" opts# keyval#))]
                     (cljs.core/pr-sequential-writer
                      writer# pr-pair# ~pr-open ", " "}" opts#
                      (concat [~@(map #(core/list `vector (keyword %) %) base-fields)]
                              ~'__extmap))))]
                }

               new-interfaces+methods
               (-> (override-default-methods default-interfaces+methods impls)
                   (add-provided-interfaces+methods impls)
                   ;; Remove not wanted interfaces
                   ((fn [i+m] (apply dissoc i+m (concat (when (= false (:map-protocol? options))
                                                          ['IMap 'IAssociative])
                                                        (:remove-interfaces options))))))

               ;; Convert the map structure to conform to 'extend-type's specification
               new-impls
               (apply concat (for [[i ms] new-interfaces+methods]
                               (concat [i] ms)))

               [fpps pmasks] (prepare-protocol-masks env new-impls)
               protocols (collect-protocols new-impls env)
               tagname (vary-meta tagname assoc
                         :protocols protocols
                         :skip-protocol-flag fpps)]
      `(do
         (~'defrecord* ~tagname ~hinted-fields ~pmasks
           (extend-type ~tagname ~@(dt->et tagname new-impls fields true)))))))


;;;; END CLJS internals


;;; Helper functions for emit-*-record-defintion

#?(:clj
   (defn add-spec-code [spec-name predicate field-triples constructor-args constructor]
     `(do
        ;; Spec for a record type
        (cljs.spec.alpha/def ~spec-name
          (cljs.spec.alpha/and ~predicate
                               ~@(map (fn [[?field ?accessor _]]
                                        `#(cljs.spec.alpha/valid? ~(r-help/name-spec ?field) (~?accessor %)))
                                      field-triples)))
        ;; Spec for constructor function
        ~(let [c-specs (mapcat (fn [constructor-arg]
                                 (let [field (first (filter #(= constructor-arg %)
                                                            (map first field-triples)))]
                                   [(keyword constructor-arg) (r-help/name-spec field)]))
                               constructor-args)]
           `(cljs.spec.alpha/fdef ~constructor
              :args (cljs.spec.alpha/cat ~@c-specs)
              :ret ~spec-name)))))

#?(:clj
   (defn fn-get-accessor-from-field-triple
     [type docref constructor field-triples meta-data]
     (fn [[field accessor lens]]
       (let [rec (with-meta `rec# {:tag type})
             data `data#
             v `v#]
         ;; Note that a two-arity function like this is a lens as defined by active.clojure.lens.
         `[(defn ~(r-help/add-meta (r-help/add-accessor-doc accessor type field docref) meta-data)
             ([~rec]
              (when-not (instance? ~type ~rec)
                (throw (js/Error. ~(str "Wrong record type (" rec ") passed to accessor ("
                                        accessor ")."))))
              (. ~rec ~(symbol (str "-" field))))
             ([~data ~v]
              ;; can't be ~constructor because constructor may take fewer arguments
              (new ~type ~@(map
                            (fn [[shove-field shove-accessor]]
                              (if (= field shove-field)
                                v
                                `(~shove-accessor ~data)))
                            field-triples))))
           ~(when lens
              (r-help/report-lens-deprecation type)
              `(def ~lens ~accessor))
           ]))))

;;; End of Helper functions

;;; Emit-*-record-definitions
#?(:clj
   (defn emit-javascript-record-definition
     [env type options constructor constructor-args predicate field-triples opt+specs]
     (let [docref               (str "See " (r-help/reference constructor) ".")
           constructor-args-set (set constructor-args)
           fields                (mapv first field-triples)
           rsym                  (vary-meta type assoc :internal-ctor true)
           r                     (vary-meta
                                  (:name (cljs.analyzer/resolve-var (dissoc env :locals) rsym))
                                  assoc :internal-ctor true)
           meta-data (meta type)]
       (validate-fields "defrecord" type fields)
       `(do
          ~(when-let [projection-lens (:projection-lens options)]
             `(declare ~projection-lens))
          ;; direct use of `defrecord` - to be replaced in the future
          ;; (defrecord ~type ~fields ~@opt+specs)
          (declare ~@(map (fn [[field accessor lens]] accessor) field-triples))
          ~(emit-defrecord options env rsym r fields (->impl-map opt+specs))
          (set! (.-getBasis ~r) (fn [] '[~@fields]))
          (set! (.-cljs$lang$type ~r) true)
          (set! (.-cljs$lang$ctorPrSeq ~r) (fn [this#] (list ~(str r))))
          (set! (.-cljs$lang$ctorPrWriter ~r) (fn [this# writer#] (cljs.core/-write writer# ~(str r))))

          ;; Create arrow constructor
          (when-not (= false (:arrow-constructor? ~options))
            ~(build-positional-factory rsym r fields))
          ~(build-map-factory rsym r fields)

          ;; Predicate
          (def ~(r-help/add-meta (r-help/add-predicate-doc type predicate docref) meta-data)
            (fn [x#] (instance? ~type x#)))

          ;; Constructor
          (def ~(r-help/add-meta (r-help/add-constructor-doc constructor constructor-args type field-triples) meta-data)
            (fn [~@constructor-args]
              (new ~type
                   ~@(map (fn [[field _]]
                            (if (contains? constructor-args-set field)
                              `~field
                              `nil))
                          field-triples))))
          (declare ~@(map (fn [[field accessor lens]] accessor) field-triples))
          ~@(mapcat (fn-get-accessor-from-field-triple type docref constructor field-triples meta-data)
                    field-triples)

          ;; Specs
          ~(when-let [spec-name (:spec options)]
             `(do
                ;; Spec for a record type
                (cljs.spec.alpha/def ~spec-name
                  (cljs.spec.alpha/and ~predicate
                                       ~@(map (fn [[field accessor _]]
                                                `#(cljs.spec.alpha/valid? ~(r-help/name-spec field) (~accessor %)))
                                              field-triples)))
                ;; Spec for constructor function
                ~(let [c-specs (mapcat (fn [constructor-arg]
                                         (let [field (first (filter #(= constructor-arg %)
                                                                    (map first field-triples)))]
                                           [(keyword constructor-arg) (r-help/name-spec field)]))
                                       constructor-args)]
                   `(cljs.spec.alpha/fdef ~constructor
                      :args (cljs.spec.alpha/cat ~@c-specs)
                      :ret ~spec-name))))

          ;; Projection lens
          ~(when-let [projection-lens (:projection-lens options)]
           `(def ~(vary-meta (symbol projection-lens) (fn [m] (merge meta-data m)))
              (apply r-help/into-record-projection-lens ~constructor ~(mapv second field-triples))))
          ~r))))
