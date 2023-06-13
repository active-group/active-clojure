(ns active.clojure.struct
  (:require [active.clojure.struct.key :as key]
            [active.clojure.struct.closed-struct :as closed-struct]
            [active.clojure.struct.closed-struct-map :as closed-struct-map])
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
