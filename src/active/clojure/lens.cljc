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

(defn- default-yank [data dflt]
  (if (nil? data) dflt data))

(defn- default-shove [v dflt]
  (if (= dflt v) nil v))

(defn default
  "Returns a lens that shows nil as the given default value, but does
  not change any other value. Note that this lens changed the type of
  the underlying data from `Maybe a` to `a` and thus you must not
  shove `nil` into it."
  [dflt]
  (xmap default-yank default-shove dflt))

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

(def ^{:doc "A trivial lens that just shows nil over anything, and does never change anything."}
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
  argument list."
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
                                                 (set/union keep (set (keys next-struct)))
                                                 ]))
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
      (fn [empty fields out v]
        (reduce (fn [r [f lens]]
                  (shove r lens (yank v f)))
                out
                fields))]
  (defn projection
    "A lens that projects multiple derived values into a new value,
  with `empty` being an initial new value, and `fields` a map or
  sequence of tuples, of a lens on the new value and lens over the
  'outer' value the lens is used on.

  As an example, this can be used to map between to record types like this:

```
  (projection (make-inner-record nil)
              {inner-record-field outer-record-field})
```

  The returned lens can then be used on a value of type
  'outer-record', to see it as a value of type 'inner-record'."
    [empty fields]
    (lens (f/partial projection-yank empty fields)
          (f/partial projection-shove empty fields))))

(defn ray
  "A lens that projects a value of `from-lens` into `to-lens`.  Optional argmuent
  `to-empty` is the initial new value that `to-lens` focusses on."
  [from-lens to-lens & [to-empty]]
  (projection to-empty {to-lens from-lens}))

(defn invert
  "A lens that inverts another `lens`.  Optional argument `empty` is the initial
  new value that `lens` focusses on.  `lens` can be a [[clojure.core/Var]] to
  delay evaluation of `lens`, which may be needed for mutually recursive
  definitions."
  [l & [empty]]
  (lens (fn [data]
          (shove empty l data))
        (fn [_data v]
          (yank v l))))

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

(defn alt
  "A lens to focus on mixed data and sum types.  Accepts a list of `alternatives`,
  where an alternative is a pair of a predicate to a lens that can focus data in
  a value that matches the given predicate."
  [& alternatives]
  (let [alt-lens
        (fn [data]
          (some (fn [[p? l]] (when (p? data) l)) alternatives))]
    (lens (fn [data]
            (yank data (alt-lens data)))
          (fn [data v]
            (shove data (alt-lens data) v)))))

(defn alt->edn
  "A projection lens on mixed data and sum types.  Accepts a list of `alternatives`,
  where an alternative is a pair of a predicate to a lens that can focus data in
  a value that matches the given predicate.

  Uses vectors in EDN representation with positional values to match to the
  types."
  [& alternatives]
  (let [empty (mapv (constantly nil) alternatives)
        yank-vec-alternatives
        (map-indexed (fn [idx [p? l]]
                       [p? (projection empty
                                       [[(at-index idx) l]])])
                     alternatives)
        yank-vec
        (apply alt yank-vec-alternatives)
        shove-vec-alternatives
        (fn [edn]
          (map-indexed (fn [idx [v l]]
                         [(constantly (some? v))
                          (projection empty
                                      [[(at-index idx) l]])])
                       (mapv (fn [[_p? l] v] [v l]) alternatives edn)))
        shove-vec
        (fn [v]
          (apply alt (shove-vec-alternatives v)))]
    (lens (fn [data]
            (yank data yank-vec))
          (fn [data edn]
            (shove data (shove-vec edn) edn)))))

(defn defer
  "A lens that defers evaluation of the given lens by lifting
  [[clojure.core/deref]] into lenses.

  Argument `var-lens` must be a [[clojure.core/Var]] with a lens
  as its value."
  [var-lens]
  (assert (var? var-lens))
  (lens (fn [data]
          (yank (deref var-lens) data))
        (fn [data v]
          (shove data (deref var-lens) v))))
