(ns active.clojure.struct
  (:require [active.clojure.struct.key :as key]
            [active.clojure.struct.closed-struct :as closed-struct]
            [active.clojure.struct.closed-struct-map :as closed-struct-map]
            [active.clojure.lens :as lens])
  (:refer-clojure :exclude [struct-map instance? satisfies?
                            set-validator!]))

;; Note: there is no positional constructor on purpose; although they
;; can be handy for small structs, they quickly become hard to read
;; and hard to refactor when getting larger. And defining a positional
;; constructor for small structs is much easier than the other way
;; round.

(defmacro ^:no-doc def-key [name]
  `(def ~name (key/make (symbol ~(str *ns*) ~(str name)))))

(defmacro def-struct
  "Defines a struct and its keys:

  ```
  (def-struct T [field-1 ...])
  ```
  "
  [t fields]
  `(do
     ~@(for [f# fields]
         `(def-key ~f#))
     
     (def ~t (closed-struct/create ~fields))

     ~(doseq [f# fields]
        `(key/optimize-for! ~f# ~t))
     
     ~t))

(defn struct-map
  "Returns a new struct map with the keys of the struct. All keys of the
  stuct must be given.

  ```
  (def-struct T [field-1 ...])
  (struct-map T field-1 42 ...)
  ```
  "
  [struct & keys-vals]
  ;; TODO: reject the same key given twice? Or offer that explicitly as an easy way to specify default values?
  ;; TODO: hash-map are quite complex macros in cljs - check that out.
  (closed-struct-map/build-map struct keys-vals))

;; TODO: construct from map; either arity 1 of struct-map, or (also) IFn on struct, or separate?

(defn ^{:no-doc true
        :doc "Replace validator of struct. Unsynchronized side effect; use only if you know what you are doing."}
  set-validator! [struct validator]
  ;; Note: the validator is not an argument to 'def-struct', because
  ;; you usually want to use the defined keys in the validator
  ;; implementation; that would make for a weird macro.
  (closed-struct/set-validator! struct validator))

(defn struct?
  "Tests if v is a struct defined by [[def-struct]]."
  [v]
  (closed-struct/closed-struct? v))

(defn instance?
  "Tests if `v` is a struct map created from the given `struct`."
  [struct v]
  (closed-struct-map/instance? struct v))

(defn satisfies?
  "Tests if `v` is a map and contains at least the keys defined for `struct`."
  [struct v]
  ;; Note: also checks the validity, if a validator is defined for struct.
  (closed-struct-map/satisfies? struct v))

(let [from-struct-1 (fn [v struct field-lens-map _]
                      (reduce-kv (fn [r f l]
                                   (lens/shove r l (f v)))
                                 {}
                                 field-lens-map))
      from-struct-2 (fn [v struct field-keyword-map _]
                      (-> (reduce-kv (fn [r f k]
                                       (assoc! r k (f v)))
                                     (transient {})
                                     field-keyword-map)
                          (persistent!)))
      to-struct (fn [v struct _ setter-lens-map]
                  (-> (reduce-kv (fn [r setter l]
                                   (setter r (lens/yank v l)))
                                 (closed-struct-map/unvalidated-empty-transient struct)
                                 setter-lens-map)
                      (persistent!)))]
  (defn map-projection "Returns a lens that projects between a struct-map of the given `struct` and a
  hash-map.

  ```
  (def-struct T [foo])

  (def p (map-projection {foo (lens/>> :foo :bar)}))
  
  (= (lens/yank (struct-map T foo 42) p)
     {:foo {:bar 42}})
  
  (= (lens/shove nil p {:foo {:bar 42}})
     (struct-map T foo 42))
  ```
  " [struct field-lens-map] ;; or maybe this should be in the lens package?
    ;; TODO: an optional 'error handler' that informs about which field was transformed when an exception occurred?
    (assert (= (closed-struct/keyset struct) (set (keys field-lens-map))) "All keys of the struct must be given.")
    (lens/xmap (if (every? keyword? (vals field-lens-map)) from-struct-2 from-struct-1)
               to-struct
               struct
               field-lens-map
               (into {} (map (fn [[f l]]
                               [(closed-struct-map/transient-setter struct f)
                                l])
                             field-lens-map)))))
