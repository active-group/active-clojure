(ns active.clojure.lens
  "Lenses should obey the following laws:

  GetPut, or YankShove:

      (= (yank (shove data my-lens val)
               my-lens)
         val)

  Meaning: you get back what you put in.

  PutGet, or ShoveYank:

      (= (shove data
                my-lens
                (yank data my-lens))
         data)

  Meaning: putting back what you got does not change anything.

  PutPut, or ShoveShove:

        (= (shove data my-lens val-1)
           (shove (shove data my-lens val-2) my-lens val-1))

  Meaning: second shove wins, or shoving once is the same as shoving twice.

  A lens that satisfies these three laws is usually called \"very well-behaved\".
  See also `active.clojure.lens-test/lens-laws-hold`."
  (:require [active.clojure.functions :as f]
            [clojure.set :as set])
  (:refer-clojure :exclude [merge first second]))

(defn yank
  "Yank a value from the given data value, as defined by the given
   lens."
  [data lens]
  (lens data))

(defn shove
  "Shove a new value v into the given data value, as defined by the
   given lens, and return the updated data structure."
  [data lens v]
  (if (keyword? lens)
    (assoc data lens v)
    (lens data v)))

#?(:clj
   (defn- throw-invalid-number-of-arguments-error [n]
     (let [error-msg (str "invalid number of arguments (" n ") to lens")]
       (throw (java.lang.IllegalArgumentException. error-msg)))))

(defrecord ExplicitLens
    ^{:private true}
  [yanker shover args]
  #?@(:clj [clojure.lang.IFn
            (invoke [this data] (apply yanker data args))
            (invoke [this data v] (apply shover data v args))
            (applyTo [this apply-args]
                     (let [apply-args (object-array apply-args)]
                       (case (count apply-args)
                         1 (apply yanker (aget apply-args 0) args)
                         2 (apply shover (aget apply-args 0) (aget apply-args 1) args)
                         (throw-invalid-number-of-arguments-error (count apply-args)))))]
      :cljs [IFn
             (-invoke [this data] (apply yanker data args))
             (-invoke [this data v] (apply shover data v args))]))

(defrecord ExplicitLensWithoutArgs
    ^{:private true}
    [yanker shover]
  #?@(:clj [clojure.lang.IFn
            (invoke [this data] (yanker data))
            (invoke [this data v] (shover data v))
            (applyTo [this apply-args]
                     (let [apply-args (object-array apply-args)]
                       (case (count apply-args)
                         1 (yanker (aget apply-args 0))
                         2 (shover (aget apply-args 0) (aget apply-args 1))
                         (throw-invalid-number-of-arguments-error (count apply-args)))))]
      :cljs [IFn
             (-invoke [this data] (yanker data))
             (-invoke [this data v] (shover data v))]))

(defn lens
  "Returns a new lens defined by the given yanker function, which
  takes a data structure and must return the focused value, and the
  given shover function which takes a data structure and the new value
  in the focus. Any additional arguments are passed unchanged to the yank
  and shove functions."
  ([yanker shover]
   (ExplicitLensWithoutArgs. yanker shover))
  ([yanker shover & args]
   (ExplicitLens. yanker shover args)))

(defn- xmap-yank [data f g & args]
  (apply f data args))

(defn- xmap-shove [data v f g & args]
  (apply g v args))

(defn overhaul
  "Updates data using a lens.  The new value will be determined by
  applying `f` to the old value and any other supplied arguments."
  ([data lens f]
   (shove data lens (f (yank data lens))))
  ([data lens f & args]
   (shove data lens (apply f (yank data lens) args))))

(defn xmap
  "Returns a \"view lens\", that transforms a whole data structure
   to something else (f) and back (g)."
  [f g & args]
  (apply lens xmap-yank xmap-shove f g args))

(def
  ^{:doc "Identity lens, that just show a data structure as it is.
          It's also the neutral element of lens concatenation
          `reacl.lens/>>`."}
  id (xmap identity identity))

(defn- keyword-shove [data val key]
  (assoc data key val))

(defn- keyword-lens [kw]
  (lens get
        keyword-shove
        kw))

(defn- lift-lens [my-lens]
  (if (keyword? my-lens)
    (keyword-lens my-lens)
    my-lens))

(defn- comb-yank [data lenses]
  (reduce (fn [data my-lens]
            (my-lens data))
          data
          lenses))

(defn- comb-shove [data val lenses]
  (let [lens-1 (clojure.core/first lenses)
        remaining (rest lenses)]
    (if (empty? remaining)
      (lens-1 data val)
      (lens-1 data
              (comb-shove (lens-1 data)
                          val
                          remaining)))))

(defn >>
  "Returns a concatenation of lenses, so that the combination shows the
   value of the last one, in a data structure that the first one is put
   over."
  [& lenses]
  (let [non-trivial-lenses (remove #{id} lenses)]
    (if (empty? (rest non-trivial-lenses))
      (or (clojure.core/first non-trivial-lenses)
          id)
      (lens comb-yank comb-shove (mapv lift-lens non-trivial-lenses)))))

(defn- or-else-yank [dflt data]
  (if (nil? data) dflt data))

(defn- or-else-shove [dflt large small]
  (if (and (nil? large)
           (= dflt small))
    nil
    small))

(defn or-else
  "Returns a lens that replaces `nil` with `dflt` on yank, and keeps `nil`
  if `dflt` is shoved. Other values are not modified.

  Unlike [[default]], shove replaces values other than `nil` with
  `dflt` when `dflt` is shoved, and is thus \"well-behaved\".

  Do not shove `nil` with the returned lens."
  [dflt]
  (lens (f/partial or-else-yank dflt)
        (f/partial or-else-shove dflt)))

(def ^:private default-yank or-else-yank)

(defn- default-shove [dflt large small]
  (if (= dflt small)
    nil
    small))

(defn default
  "Returns a lens that replaces `nil` with `dflt` on yank, and `dflt`
  with `nil` on shove. Other values are not modified.

  Unlike [[or-else]], shove always returns `nil` when `dflt` is
  shoved, and is thus not \"well-behaved\".

  Do not shove `nil` with the returned lens."
  [dflt]
  (lens (f/partial default-yank dflt)
        (f/partial default-shove dflt)))

(defn- consx [v coll]
  (if (and (nil? v) (empty? coll))
    coll
    (cons v coll)))

(def
  ^{:doc "A lens focusing on the first element in a collection. It
  yanks nil if the collection is empty, and will not insert nil into an empty collection."}
  head
  (lens clojure.core/first
        #(consx %2 (rest %1))))

(def
  ^{:doc "A lens focusing on the first element in a non-empty
  collection. Behaviour on an empty collection is undefined."}
  nel-head
  (lens clojure.core/first
        #(cons %2 (rest %1))))

(def
  ^{:doc "A lens focusing on the all but the first element in a collection.
  Note that nil will be prepended when shoving into an empty collection."}
  tail
  (lens rest
        #(consx (clojure.core/first %1) %2)))

(def
  ^{:doc "A lens focusing on the all but the first element in a non-empty collection.
  Behaviour on an empty collection is undefined."}
  nel-tail
  (lens rest
        #(cons (clojure.core/first %1) %2)))

(let [pos-get (fn [data n]
                (clojure.core/first (drop n data)))
      pos-set (fn [data v n]
                (let [[front back] (split-at n data)
                      ff (take n front)]
                  (concat ff (repeat (- n (count ff)) nil) (list v) (rest back))))]
  (defn pos
    "A lens over the nth element in a sequence. Note that when shoving a
  new value `nil`s may be added before the given position, if the collection is smaller."
    [n]
    (assert (number? n))
    (assert (>= n 0))
    (lens pos-get
          pos-set
          n)))

(def ^{:doc "A lens that views a sequence as a set."}
  as-set
  (lens set
        ; this is needed to abide the second lens law
        #(if (= (set %1) %2)
           %1
           (seq %2))))

(defn- contains-shove [data mem? v]
  (if mem?
    (conj data v)
    (disj data v)))

(defn contains
  "Returns a lens showing the membership of the given value in a set."
  [v]
  (lens contains?
        contains-shove
        v))

(def ^{:doc "A lens that views a sequence of pairs as a map."}
  as-map
  (xmap #(into {} %) seq))

(defn- member-shove [data v key not-found]
  (if (= v not-found)
    (dissoc data key)
    (assoc data key v)))

(defn member
  "Returns a lens showing the value mapped to the given key in a map,
  not-found or nil if key is not present. Note that when not-found (or
  nil) is shoved into the map, the association is removed."
  [key & [not-found]]
  (lens get
        member-shove
        key
        not-found))

(def ^{:doc "A trivial lens (NOT well-behaved) that just shows nil over anything, and does never change anything."}
  void
  (lens (constantly nil) (fn [data _] data)))

(defn- is-shove [data is? cmp]
  (if is?
    cmp
    (if (= data cmp)
      nil
      data)))

(defn is
  "Returns a lens showing if a data structure equals the non-nil value v."
  [v]
  (assert (not (nil? v)))
  (lens =
        is-shove
        v))

(defn- mult-yank [data lenses]
  (map yank
       data lenses))

(defn- mult-shove [data v lenses]
  (map shove
       data lenses v))

(defn **
  "Return the product of several lenses, which means that each lens is
  held over an element of a collection in the order they appear in the
  argument list.  Note that the resulting collection is a lazy sequence
  (since it is constructed with `map`)."
  [& lenses]
  (lens mult-yank
        mult-shove
        lenses))

(defn- plus-yank [data lenses]
  (map yank
       (repeat data)
       lenses))

(defn- plus-shove [data v lenses]
  (reduce (fn [data [l v]] (shove data l v))
          data
          (map vector lenses v)))

(defn ++
  "Returns a lens over some data structure that shows a sequence of
  elements that each of the given lenses show on that. Note that the
  behaviour is undefined if those lenses do not show distinct parts
  of the data structure."
  [& lenses]
  (lens plus-yank
        plus-shove
        lenses))

(defn mapl
  "Returns a lens that maps a given lens over a collection."
  [l]
  (lens (fn mapl-yank
          [data]
          (map yank
               data (repeat l)))
        (fn mapl-shove
          [data v]
          (map shove
               (concat data (repeat nil)) (repeat l) v))))

(defn mapl-kv
  "Returns a lens that maps the given key-lens and val-lens over a key-value map
  and returns a key-value map.  Note that this special lens is needed to since
  using `mapl` together with `**` and `as-map` to achieve something similar is
  hard, due to the fact that `**` returns a lazy list and `as-map` (or
  rather `(into {} ...)` expects a two-element vector to be equivalent to a
  `clojure.lang.MapEntry`).  Something like this would work, but this is way to
  complicated and obscure:

    (>> (invert as-map) (mapl (xmap vec vec))
        (mapl (invert (invert (** key-lens value-lens) (first (seq {nil nil})))))
        (mapl (xmap vec vec)) as-map)

  Using a specialization of `**` to return vectors or even `MapEntry`s would
  improve the above chain, but these specialized versions would only make sense
  for mapping maps, so let's just provide that lens."
  [key-lens val-lens]
  (lens (fn mapl-kv-yank
               [data]
               (reduce-kv (fn [m k v] (assoc m
                                             (yank k key-lens)
                                             (yank v val-lens)))
                          (empty data) data))
             (fn mapl-kv-shove
               [data shove-v]
               (reduce-kv (fn [m k v] (assoc m
                                             (shove nil key-lens k)
                                             (shove nil val-lens v)))
                          (empty data) shove-v))))

(defn- at-index-shove [coll v n]
  (if (associative? coll)
    ;; hitting vectors and map-entries
    (assoc coll n v)
    (let [[front back] (split-at n coll)]
      (let [s (concat front
                      (list v)
                      (rest back))]
        (if (list? coll)
          (apply list s)
          (if (seq? coll)
            s
            (into (empty coll) s)))))))

(defn at-index
  "Returns a lens that focuses on the value at index n in a collection.
  The sequence must have >= n elements. Preserves the collection type when shoving."
  [n]
  (lens nth
        at-index-shove
        n))

(def ^{:doc "A lens over the first element in a collection. Equivent to [[at-index]] of 0."}
  first
  (at-index 0))

(def ^{:doc "A lens over the second element in a collection. Equivent to [[at-index]] of 1."}
  second
  (at-index 1))

(letfn [(shove-1 [struct ns keep]
          (let [skeys (keys struct)]
            [(reduce (fn [res k]
                       (if (contains? ns k)
                         (assoc res k (get ns k))
                         (if (contains? keep k)
                           res
                           (dissoc res k))))
                     struct
                     skeys)
             (select-keys ns (set/difference (set (keys ns)) (set skeys)))]))]
  (def ^{:doc "A lens over a sequence of maps or records, that yields
a merged map of all of them. If maps or records have fields of the
same name, the value in right most map is used and updated on a
change. If an update contains new keys, they are put in the left-most
map. If an update misses keys, those fields are removed on the
right-most element where they were before."}  merge
    (fn
      ([structs]
       (apply clojure.core/merge structs))
      ([structs ns]
       (if (empty? structs)
         structs ;; or maybe that should be an error?
         (let [[result remain keep] (reduce (fn [[result remain keep] next-struct]
                                              (let [[next-res next-remain] (shove-1 next-struct remain keep)]
                                                [(cons next-res result)
                                                 next-remain
                                                 ;; structs to the left can keep the keys that were 'shadowed' by this.
                                                 (set/union keep (set (keys next-struct)))]))
                                            [nil ns #{}]
                                            (reverse structs))]
           (->> (cons (clojure.core/merge (clojure.core/first result) remain)
                      (rest result))
                (into (empty structs)))))))))

(let [projection-yank
      (fn
        [empty fields in]
        (reduce (fn [r [f lens]]
                  (shove r f (yank in lens)))
                empty
                fields))
      projection-shove
      (fn [fields out v]
        (reduce (fn [r [f lens]]
                  (shove r lens (yank v f)))
                out
                fields))]
  (defn projection
    "A lens that projects multiple derived values into a new value,
  with `empty` being an initial new value, and `fields` a map or
  sequence of tuples, of a lens on the new value and lens over the
  'outer' value the lens is used on.

  As an example, this can be used to map between two record types like this:

```
  (projection (make-inner-record nil)
              {inner-record-field outer-record-field})
```

  The returned lens can then be used on a value of type
  'outer-record', to see it as a value of type 'inner-record'."
    [empty fields]
    (lens (f/partial projection-yank empty fields)
          (f/partial projection-shove fields))))

(defn project
  "A lens that projects multiple derived values into a new value,
  with an optional `empty` being an initial new value, and `fields` a map or
  sequence of tuples, of a lens on the new value and lens over the
  'outer' value the lens is used on.

  The optional `empty` will default to `nil`, so you can nil-pun your
  way to blissful oblivion:
```
  (projection {(at-index 0) :a
               (at-index 1) :b})
```
  "
  [fields & [empty]]
  (projection empty fields))

(let [invert-yank (fn invert-yank [l empty data]
                    (shove empty l data))
      invert-shove (fn invert-shove [l _data v]
                     (yank v l))]
  (defn invert
    "A lens that inverts another `lens`.  Optional argument `empty` is the initial
  new value that `lens` focusses on, which defaults to `nil`."
    [l & [empty]]
    (lens (f/partial invert-yank l empty)
          (f/partial invert-shove l))))

#_(defn ray
  "A lens that projects a value of `from-lens` into `to-lens`.  Optional argmuent
  `to-empty` is the initial new value that `to-lens` focusses on."
  [from-lens to-lens & [to-empty]]
  ;; Note: identical:
  (>> from-lens (invert to-lens to-empty))
  #_(projection to-empty {to-lens from-lens}))


(defn pattern
  "A lens over any value yielding to a map or a vector, depending on the given pattern.

  For example

```
  (pattern {:bar (at-index 0) :foo (at-index 2)})
```

  will focus on the first and third elements of a sequence, as a map with the given keys.

  Similarly, you can create a lens that yield a vector, given some fields of a map:

```
  (pattern [:bar :foo])
```

"
  [p]
  (cond
    ;; TODO: optimize projection by using transient ? (generally, make a :keyword use assoc! on transients?
    (map? p) (projection {} p)
    (vector? p) (projection [] (map-indexed (fn [idx f]
                                              [(at-index idx) f])
                                            p))
    :else (assert false "Pattern must be a map or a vector.")))

(defn surjection
  "A lens that focuses on many values in a data structure that should be identical.
  This is useful for invariants in data structures."
  [& lenses]
  (projection nil (mapv (fn [l] [id l]) lenses)))

(let [either-yank (fn [yank? then-lens else-lens d]
                    (yank d (if (yank? d)
                              then-lens
                              else-lens)))
      either-shove (fn [shove? then-lens else-lens d v]
                     (shove d (if (shove? d v)
                                then-lens
                                else-lens)
                            v))]
  (defn either
    "A lens that behaves like `then-lens` if `(yank? data)` is true when
  yanking, resp. `(shove? data value)` when shoving, and behaves like
  `else-lens` otherwise."
    [yank? shove? then-lens else-lens]
    (lens (f/partial either-yank yank? then-lens else-lens)
          (f/partial either-shove shove? then-lens else-lens))))

(defn conditional
  "A lens similar to [[either]] that allows to specify multiple variants
  at once. The last argument must a single default lens, which is used
  when no predicate matches:

  ```
  (conditional
    [yank? shove? lens]
    ...
    default)
  =>
  (either yank? shove? lens default)
  ```
  "
  [clause & clauses]
  (if (empty? clauses)
    (do (assert (not (vector? clause)) "Last element must be a default lens.")
        clause)
    (do (assert (= 3 (count clause)) clause)
        (let [[yank? shove? lens] clause]
          (assert (some? yank?))
          (assert (some? shove?))
          (assert (some? lens))
          (either yank? shove? lens
                  (apply conditional clauses))))))

(let [lift-shove? (fn [inner? data v]
                   (inner? v))
      lift-shoves? (fn [c]
                    (assert (vector? c) c)
                    (let [[outer? inner? lens] c]
                      (assert (some? inner?))
                      [outer? (f/partial lift-shove? inner?) lens]))
      throw-lens
      (lens (fn [data]
              (throw (ex-info (str "No predicate matched data: " (pr-str data)) {:data data})))
            (fn [data v]
              (throw (ex-info (str "No predicate matched value: " (pr-str v)) {:value v}))))]
  (defn union
    "A lens that combines multiple lenses depending on the values yanked or
  shoved from. Predicates on the values being yanked from or shoved to
  the lens must be given:

  ```
  (union [d-1? v-1? lens-1] ...)
  ```

  i.e. `lens-1` is used when yanking, if `d-1?` is true for the data
  being yanked from, and `lens-1` is used when shoving, if `v-1?` is
  true for the value being shoved though the lens. In each case the
  next triple are tried if the predicates do not hold. If none
  matches, an exception is thrown. You can provide an alternative
  fallback, by adding a clause that matches any value at the end."
    [& clauses]
    (apply conditional (concat (map lift-shoves? clauses)
                               [throw-lens]))))

(let [has-idx? (fn [idx vector]
                 (and (vector? vector) (some? (get vector idx))))

      union-vector-element-yank (fn [l idx empty-v data]
                                  ;; create a vector, placing the yanked value into it at idx
                                  (assoc empty-v idx (yank data l)))
      union-vector-element-shove (fn [l idx data v]
                                   ;; take value from index in vector, then shove it into data with l
                                   (shove data l (get v idx)))
      union-vector-element (fn [l idx empty-v]
                             ;; focus on position idx in a vector, 
                             (lens (f/partial union-vector-element-yank l idx empty-v)
                                   (f/partial union-vector-element-shove l idx)))
      ]
  (defn union-vector
    "A lens that combines multiple lenses depending on the values yanked,
  into a vector corresponding in length to the number of clauses:

  ```
  (union-vector [d-1? lens-1] [d-2? lens-2] ... fallback-lens)
  ```
  
  i.e. `lens-1` is used when yanking, if `d-1?` is true for the data
  being yanked from, and otherwise `lens-2` if `d-2?` holds. Yanking
  then results in a vector with all items `nil` except one, which
  determines which lens to use when shoving. That also means none of
  the given lenses should yank to `nil`!

  An optional fallback lens can be given as the last argument, which
  is applied to the 'raw data' directly. If none is given, and
  exception is thrown in that case."
    [& clauses]
    ;; Note: must not project to nil
    (let [size (let [c (count clauses)]
                 (if (not (vector? (last clauses)))
                   (dec c)
                   c))
          empty-v (vec (repeat size nil))]
      (apply union (map-indexed (fn [idx clause]
                                  (if (vector? clause)
                                    (do (assert (= 2 (count clause)))
                                        (let [[outer? lens] clause]
                                          [outer?
                                           (f/partial has-idx? idx)
                                           (union-vector-element lens idx empty-v)
                                           ]))
                                    ;; default lens, only in last position.
                                    (do (assert (= idx size))
                                        (let [dflt clause]
                                          [(f/constantly true) (f/constantly true) dflt]))))
                                clauses)))))

(let [tuple? (fn [v]
               (and (seq v) (= 2 (count v))))
      has-tag? (fn [tag v]
                 (and (tuple? v) (= tag (first v))))]
  (defn union-tagged
    "A lens that associates different kinds of values with a tag on
  yanking, and optionally adds different lenses to that:

  ```
  (union-tagged [d-1? tag-1 lens-1] [d-2? tag-2 lens-2] ... fallback-lens)
  ```

  will yank a value where `d-1?` is true to a tuple `[tag-1 v]` where
  `v` is the result of yanking `lens-1` to the data. The lens defaults
  to [[id]]. When shoving, that tag determines which lens to apply.

  A optional fallback lens can be given as the last argument, which is
  used on the 'raw data' when either none of the predicates hold on
  yanking, or none if the tags match on shoving. If none is given, an
  exception is thrown in that case."
    [& clauses]
    (let [size (count clauses)]
      (apply union (map-indexed (fn [idx clause]
                                  (if (vector? clause)
                                    (do (assert (<= 2 (count clause) 3))
                                        (let [[outer? tag & [lens]] clause]
                                          [outer?
                                           (f/partial has-tag? tag)
                                           (++ (>> void (default tag)) (or lens id))]))
                                    ;; default lens, only in last position.
                                    (do (assert (= idx (dec size)))
                                        (let [dflt clause]
                                          [(f/constantly true) (f/constantly true) dflt]))))
                                clauses)))))

(defn defer
  "A lens that defers evaluation of the given lens by lifting
  [[clojure.core/deref]] into lenses.

  Argument `var-lens` must be a [[clojure.core/Var]] with a lens
  as its value."
  [var-lens]
  (assert (var? var-lens))
  (lens (fn [data]
          (yank data (deref var-lens)))
        (fn [data v]
          (shove data (deref var-lens) v))))
