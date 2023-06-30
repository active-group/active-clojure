(ns ^:no-doc active.clojure.struct.closed-struct
  (:require [active.clojure.struct.validator :as v])
  (:refer-clojure :exclude [set-validator! #?@(:cljs [contains? keys])]
                  :rename {contains? clj-contains?
                           keys clj-keys}))

(defprotocol ^:private IClosedStruct
  (-get-validator [this])
  (-set-validator [this validator]))

(deftype ^:private ClosedStruct [keys index-map  ^:unsynchronized-mutable validator extended-struct]
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

#?(:clj
   ;; TODO: tune that a bit, add namespace, deduplicate impls.
   
   (defmethod print-method ClosedStruct [s ^java.io.Writer writer]
     (.write writer "#Struct{")
     (when-let [k (first (.-keys s))]
       (print-method k writer))
     (doseq [k (rest (.-keys s))]
       (.write writer ", ")
       (print-method k writer))
     (.write writer "}"))

   :cljs
   (extend-protocol IPrintWithWriter
     ClosedStruct
     (-pr-writer [s writer _]
       (write-all writer "#Struct{")
       (doseq [k (interpose ", " (.-keys s))]
         (write-all writer k))
       (write-all writer "}"))))

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

(defn validate-single! [^ClosedStruct t changed-key changed-value]
  (when-let [v (-get-validator t)]
    (v/validate-single! v changed-key changed-value)))

(defn validate-map-only [m ^ClosedStruct t]
  (when-let [v (-get-validator t)]
    (v/validate-map-only! v m))
  m)

(defn valid? [t m]
  (if-let [validator (validator t)]
    (v/valid? validator m)
    true))

(defn create [fields extended-struct]
  ;; Note: keys of the extended struct must come first! (for optimizations to work)
  (let [all-fields (vec (concat (when (some? extended-struct) (.-keys extended-struct)) fields))]
    (ClosedStruct. all-fields
                   (loop [idx 0
                          r (transient {})
                          fs all-fields]
                     (if-not (empty? fs)
                       (recur (inc idx)
                              (assoc! r (first fs) idx)
                              (rest fs))
                       (persistent! r)))
                   nil
                   extended-struct)))

(defn size [^ClosedStruct t]
  (count (.-keys t)))

(defn contains? [^ClosedStruct t key]
  (clj-contains? (.-index-map t) key))

(defn maybe-index-of [^ClosedStruct t key default]
  (get (.-index-map t) key default))

(let [not-found #?(:clj (Object.) :cljs #js {})]
  (defn index-of [t key]
    (let [idx (maybe-index-of t key not-found)]
      (if (identical? idx not-found)
        (throw (ex-info "Key not in struct" {:key key
                                             :available (.-keys t)}))
        idx))))

(defn keys "Vector of keys in order given in [[create]]." [^ClosedStruct t]
  (.-keys t))

(defn keyset [t]
  ;; OPT: maybe cache that in closed-struct
  (set (keys t)))

(defn extended-struct "The struct that t extends, or nil." [t]
  (.-extended-struct t))
