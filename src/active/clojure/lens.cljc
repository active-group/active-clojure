(ns active.clojure.lens)

;; TODO document lens laws

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

(defrecord ExplicitLens
    ^{:private true}
  [yanker shover args]
  #?@(:clj [clojure.lang.IFn
            (invoke [this data] (apply yanker data args))
            (invoke [this data v] (apply shover data v args))
            (applyTo [this args]
                     (let [args (object-array args)]
                       (case (count args)
                         1 (yanker (aget args 0))
                         2 (shover (aget args 0) (aget args 1))
                         (throw #?(:clj (java.lang.IllegalArgumentException. (str "invalid number of arguments (" (count args) ") to lens")))
                                #?(:cljs (str "invalid number of arguments (" (count args) ") to lens"))))))]
      :cljs [IFn
             (-invoke [this data] (apply yanker data args))
             (-invoke [this data v] (apply shover data v args))]))

(defn lens
  "Returns a new lens defined by the given yanker function, which
  takes a data structure and must return the focused value, and the
  given shover function which takes a data structure and the new value
  in the focus. Any additional arguments are passed unchanged to the yank
  and shove functions."
  [yank shove & args]
  (ExplicitLens. yank shove args))

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
          reacl.lens/>>."}
  id (xmap identity identity))

(defn- comb-yank [data l1 l2]
  (yank (yank data l1) l2))
(defn- comb-shove [data v l1 l2]
  (shove data l1 (shove (yank data l1) l2 v)))

(defn- >>2
  [l1 l2]
  (lens comb-yank comb-shove l1 l2))

(defn >>
  "Returns a concatenation of two or more lenses, so that the combination shows the
   value of the last one, in a data structure that the first one is put
   over."
  [l1 & lmore]
  (loop [res l1
         lmore lmore]
    (if (empty? lmore)
      res
      (recur (>>2 res (first lmore)) (rest lmore)))))

(defn- default-yank [data dflt]
  (if (nil? data) dflt data))
(defn- default-shove [v dflt]
  (if (= dflt v) nil v))

(defn default
  "Returns a lens that shows nil as the given default value, but does not change any other value."
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
  (lens #(first %)
        #(consx %2 (rest %1))))

(def
  ^{:doc
  "A lens focusing on the first element in a non-empty
  collection. Behaviour on an empty collection is undefined."}
  nel-head
  (lens #(first %)
        #(cons %2 (rest %1))))

(def
  ^{:doc "A lens focusing on the all but the first element in a collection.
  Note that nil will be prepended when shoving into an empty collection."}
  tail
  (lens #(rest %)
        #(consx (first %1) %2)))

(def
  ^{:doc "A lens focusing on the all but the first element in a non-empty collection.
  Behaviour on an empty collection is undefined."}
  nel-tail
  (lens #(rest %)
        #(cons (first %1) %2)))

(defn pos
  "A lens over the nth element in a collection. Note that when shoving a
  new value nils may be added before the given position, if the the collection is smaller."
  [n]
  (assert (number? n))
  (assert (>= n 0))
  ;; there are probably more efficient implementations:
  (if (= n 0)
    head
    (>> tail (pos (- n 1)))))

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
        key not-found))

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

;; not very general:
;; (defn repeated
;;   [n]
;;   (lens #(take n (repeat %))
;;         (fn [data v]
;;           (or (some #(not (= % data))
;;                     v)
;;               data))))

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
  behaviour is undefined if those lenses do not show distrinct parts
  of the data structure."
  [& lenses]
  (lens plus-yank
        plus-shove
        lenses))

(defn- at-index-shove [coll v n]
  (let [[front back] (split-at n coll)]
    (concat front
            [v]
            (rest back))))

(defn at-index
  "Returns a lens that focuses on the value at position n in a sequence.
  The sequence must have >= n elements."
  [n]
  (lens nth
        at-index-shove
        n))
