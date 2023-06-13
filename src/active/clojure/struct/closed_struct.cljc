(ns active.clojure.struct.closed-struct
  (:require [active.clojure.struct.validator :as v])
  (:refer-clojure :exclude [set-validator!]
                  :rename {contains? clj-contains?
                           keys clj-keys}))

(defprotocol ^:private IClosedStruct
  (-get-validator [this])
  (-set-validator [this validator]))

(deftype ^:private ClosedStruct [keys index-map  ^:unsynchronized-mutable validator]
  IClosedStruct
  (-get-validator [this]
    (.-validator this))
  (-set-validator [this validator]
    (set! (.-validator this) validator))
  #?@(:clj

      [clojure.lang.IHashEq
       (hasheq [this]
               (hash index-map))
       Object
       (equals [this other]
               (if (instance? ClosedStruct other)
                 ;; OPT: faster to compare only the keys?
                 (= index-map (.-index-map other))
                 false))]))

(defn closed-struct? [v]
  (instance? ClosedStruct v))

(defn- validator [^ClosedStruct t]
  (-get-validator t))

(defn set-validator! [^ClosedStruct t validator]
  (assert (closed-struct? t)) ;; TODO: nice exception?
  (-set-validator t validator))

(defn validate [m ^ClosedStruct t changed-keys changed-values]
  (when-let [v (-get-validator t)]
    (v/validate! v m changed-keys changed-values))
  m)

(defn valid? [t m]
  (if-let [validator (validator t)]
    (v/valid? validator m)
    true))

(defn create [fields]
  (ClosedStruct. (vec fields)
                 ;; OPT: is this actually the fastest way to create the index-map:
                 (loop [idx 0
                        r (transient {})
                        fs fields]
                   (if-not (empty? fs)
                     (recur (inc idx)
                            (assoc! r (first fs) idx)
                            (rest fs))
                     (persistent! r)))
                 nil))

(defn size [^ClosedStruct t]
  (count (.-keys t)))

(defn contains? [^ClosedStruct t key]
  (clj-contains? (.-index-map t) key))

(defn maybe-index-of [^ClosedStruct t key default]
  (get (.-index-map t) key default))

(defn index-of [t key]
  (let [idx (maybe-index-of t key ::not-found)]
    (if (= idx ::not-found)
      (throw (ex-info "Key not in struct" {:key key
                                           :x (.-index-map t)
                                           :available (.-keys t)}))
      idx)))

(defn keys "Collection of keys in order given in [[create]]." [^ClosedStruct t]
  (.-keys t))

(defn keyset [t]
  ;; OPT: maybe cache that in closed-struct
  (set (keys t)))
