(ns active.clojure.struct
  (:require [active.clojure.struct.key :as key]
            [active.clojure.struct.closed-struct-map :as closed-struct-map])
  (:refer-clojure :exclude [struct-map instance? satisfies?
                            set-validator!]))

(defmacro ^:no-doc def-key [name]
  `(def ~name (key/make (symbol ~(str *ns*) ~(str name)))))

(defmacro def-struct [t fields]
  ;; TOOO: proper compile-time errors:
  (assert (symbol? t))
  (assert (every? symbol? fields))
  `(do
     ~@(for [f# fields]
         `(def-key ~f#))
     
     (def ~t (closed-struct-map/create-closed-struct ~fields))

     ~(doseq [k# fields]
        `(key/optimize-for! ~k# ~t))
     
     ~t))

(defn struct-map
  "Returns a new struct map with the keys of the struct. All keys of the
  stuct must be given."
  [struct & keys-vals]
  (closed-struct-map/build-map struct keys-vals))

;; TODO: construct from map; either arity 1 of struct-map, or (also) IFn on struct, or separate?

(defn ^{:no-doc true
        :doc "Replace validator of struct. Unsynchronized side effect; use only if you know what you are doing."}
  set-validator! [struct validator]
  (closed-struct-map/set-closed-struct-validator! struct validator))

(defn struct?
  "Tests if v is a struct defined by [[def-struct]]."
  [v]
  (closed-struct-map/closed-struct? v))

(defn instance?
  "Tests if `v` is a struct map created from the given `struct`."
  [struct v]
  (closed-struct-map/instance? struct v))

(defn satisfies?
  "Tests if `v` is a map and contains at least the keys defined for `struct`."
  [struct v]
  ;; Note: also checks the validity, if a validator is defined for struct.
  (closed-struct-map/satisfies? struct v))
