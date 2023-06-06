(ns active.clojure.struct
  (:require [clojure.set :as set]
            [active.clojure.struct.key :as key]
            [active.clojure.struct.closed-struct-map :as closed-struct-map])
  (:refer-clojure :exclude [struct-map instance? satisfies?]))

(defmacro ^:no-doc def-key [name]
  `(def ~name (key/make (symbol ~(str *ns*) ~(str name)))))

(defmacro def-struct [t fields]
  ;; TOOO: proper compile-time errors:
  (assert (symbol? t))
  (assert (every? symbol? fields))
  `(do
     ~@(for [f# fields]
         `(def-key ~f#))
     
     (def ~t (closed-struct-map/create-closed-struct [~@fields]))

     ~(doseq [k# fields]
        `(key/optimize-for! ~k# ~t))
     ~t))

(defn struct-map
  "Returns a new struct map with the keys of the struct. All keys of the
  stuct must be given."
  [struct & keys-vals]
  (closed-struct-map/build-map struct keys-vals))

;; TODO: construct from map; either arity 1 of struct-map, or IFn on struct, or separate?

(defn struct?
  "Tests if v is a struct defined by [[def-struct]]."
  [v]
  (closed-struct-map/closed-struct? v))

(defn instance?
  "Tests if `v` is a map and contains exactly the keys defined for `struct`."
  [struct v]
  (and (map? v)
       (or (closed-struct-map/instance? struct v)
           (= (closed-struct-map/closed-struct-keyset struct)
              (set (keys v))))))

(defn satisfies?
  "Tests if `v` is a map and contains at least the keys defined for `struct`."
  [struct v]
  ;; actual instance, or map with at least those keys?
  (and (map? v)
       (or (closed-struct-map/instance? struct v)
           (empty? (set/difference (closed-struct-map/closed-struct-keyset struct)
                                   (set (keys v)))))))
