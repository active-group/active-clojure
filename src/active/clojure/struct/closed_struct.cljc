(ns ^:no-doc active.clojure.struct.closed-struct
  (:require [active.clojure.struct.validator :as v])
  (:refer-clojure :exclude [set-validator! #?@(:cljs [contains? keys])]
                  :rename {contains? clj-contains?
                           keys clj-keys}))

(defprotocol ^:private IClosedStruct
  (-get-validator [this])
  (-set-validator [this validator]))

(deftype ^:private ClosedStruct [keys keyset index-map  ^:unsynchronized-mutable validator extended-struct]
  ;; Note: 'keys' is in original order; keyset the same as a set.
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

(defn set-validator! [^ClosedStruct t validator]
  (assert (closed-struct? t)) ;; TODO: nice exception?
  (-set-validator t validator))

(defn- maybe-validate! [m ^ClosedStruct t changed-keys changed-values]
  ;; validate map, if the struct has any of the changed-keys
  (when-let [v (-get-validator t)]
    ;; OPT: maybe validators should be able to say that they are 'inactive', so that we don't have to do this:
    (when-let [red (->> (map (fn [k v]
                               (when (clj-contains? (.-keyset t) k)
                                 [k v]))
                             changed-keys changed-values)
                        (remove nil?)
                        (not-empty))]
      (let [changed-keys (map first red)
            changed-values (map second red)]
        (when-let [s (.-extended-struct t)]
          (maybe-validate! m s changed-keys changed-values))
        
        (v/validate! v m changed-keys changed-values)))))

(defn validate [m ^ClosedStruct t changed-keys changed-values]
  ;; validator of extended struct, only if its fields changed:
  (when-let [s (.-extended-struct t)]
    (maybe-validate! m s changed-keys changed-values))
  ;; but this one always:
  (when-let [v (-get-validator t)]
    (v/validate! v m changed-keys changed-values))
  m)

(defn validate-single! [^ClosedStruct t changed-key changed-value]
  (when-let [s (.-extended-struct t)]
    (when (clj-contains? (.-keyset s) changed-key)
      (validate-single! s changed-key changed-value)))
  (when-let [v (-get-validator t)]
    (v/validate-single! v changed-key changed-value)))

(defn validate-map-only [m ^ClosedStruct t]
  (when-let [s (.-extended-struct t)]
    (validate-map-only m s))
  (when-let [v (-get-validator t)]
    (v/validate-map-only! v m))
  m)

(defn valid? [^ClosedStruct t m]
  (and (if-let [s (.-extended-struct t)]
         (valid? s m)
         true)
       (if-let [validator (-get-validator t)]
           (v/valid? validator m)
           true)))

(defn extended-struct [^ClosedStruct t]
  (.-extended-struct t))

(defn create [fields extended-struct]
  ;; Note: keys of the extended struct must come first! (for optimizations to work)
  (let [all-fields (vec (concat (when (some? extended-struct) (.-keys extended-struct)) fields))]
    (ClosedStruct. all-fields
                   (set all-fields)
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
  (.-keyset t))

(defn extended-struct "The struct that t extends, or nil." [t]
  (.-extended-struct t))
