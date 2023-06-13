(ns active.clojure.struct.closed-struct-map
  (:require [active.clojure.struct.closed-struct :as closed-struct]
            [active.clojure.struct.closed-struct-data :as data])
  #?(:clj (:import (clojure.lang Util)))
  (:refer-clojure :rename {instance? clj-instance?
                           satisfies? cljs-satisfies?}
                  :exclude [accessor struct-map struct create-struct]))


#?(:clj
   (defn- map-cons-o [f o]
     (condp clj-instance? o
       java.util.Map$Entry
       (let [^java.util.Map$Entry e o]
         (list (f (.getKey e) (.getValue e))))

       clojure.lang.IPersistentVector
       (let [^clojure.lang.IPersistentVector v o]
         ;; should be tuple [k v]
         (when (not= (count v) 2)
           ;; same as APersistentMap does:
           (throw (IllegalArgumentException. "Vector arg to map conj must be a pair")))
         (list (f (.nth v 0) (.nth v 1))))

       ;; else: clojure.lang.IPersistentMap
       (let [^clojure.lang.IPersistentMap m o]
         (map (fn [^java.util.Map$Entry e]
                (f (.getKey e) (.getValue e)))
              (.seq m))))))

;; TODO: other exceptions are probably more helpful: (also synchronize with exceptions thrown in struct/index-of)
(defn- cannot-add [key]
  (Util/runtimeException "Key already present"))

(defn- cannot-remove [key]
  (Util/runtimeException "Can't remove struct key"))

(declare create)


(defn- ensure-editable! [owner]
  (when-not owner
    ;; same error and message as clojure
    (throw (java.lang.IllegalAccessError. "Transient used after persistent! call"))))

(defn- unchecked-assoc! [struct data key val]
  ;; unchecked = without ensure-editable!
  (closed-struct/validate-single! struct key val)
  (data/mutate! struct data key val))

(deftype ^:private TransientClosedStructMap [struct data ^:unsynchronized-mutable owner]
  ;; Note: does 'single field' validation immediately, but full map validation only on persistence.

  #?@(:clj
      ;; clojure.lang.ATransientMap does not expose any helpers :-/
      ;; TODO: IFn?
      [clojure.lang.ITransientMap
       clojure.lang.Counted
       clojure.lang.ITransientAssociative
       
       (assoc [this key val]
              (ensure-editable! owner)
              (unchecked-assoc! struct data key val)
              this)
       (without [this key]
                (ensure-editable! owner)
                (throw (cannot-remove key)))
       (count [this]
              (ensure-editable! owner)
              (closed-struct/size struct))
       (conj [this val]
             (ensure-editable! owner)
             (condp clj-instance? val
               java.util.Map$Entry
               (let [^java.util.Map$Entry e val]
                 (unchecked-assoc! struct data (.getKey e) (.getValue e)))

               clojure.lang.IPersistentVector
               (let [^clojure.lang.IPersistentVector v val]
                 (when (not= 2 (.count v))
                   (java.lang.IllegalArgumentException. "Vector arg to map conj must be a pair"))
                 (unchecked-assoc! struct data (.nth v 0) (.nth v 1)))

               ;; else, assume sequence of Map.Entry
               (doseq [o (seq val)]
                 (let [^java.util.Map$Entry e o]
                   (unchecked-assoc! struct data (.getKey e) (.getValue e)))))
             this)
       (valAt [this key]
              (ensure-editable! owner)
              (data/access struct data key))
       (valAt [this key not-found]
              (ensure-editable! owner)
              (data/access-with-default struct data key not-found))
       
       (persistent [this]
                   (ensure-editable! owner)
                   ;; do full map validation here.
                   (set! (.-owner this) false)
                   ;; transient -> persistent looses meta.
                   (-> (create struct data nil)
                       (closed-struct/validate-map-only struct)))
       ]))

(deftype ^:private PersistentClosedStructMap [struct data ^int ^:unsynchronized-mutable _hasheq ^int ^:unsynchronized-mutable _hash _meta]

  ;; TODO: Ifn taking key.
  #?@(:clj
      ;; Note: if we used gen-class instead of deftype, we could
      ;; extend APersistentMap; but using gen-class is also not easy
      ;; in terms of the interop.
      [java.lang.Iterable
       
       java.util.Map
       (size [this] (closed-struct/size struct))
       (isEmpty [this] (zero? (closed-struct/size struct)))
       (containsKey [this k] (closed-struct/contains? struct k))
       (keySet [this] ;; Java api, and a Java Set
               (closed-struct/keyset struct))
       (getOrDefault [this key default]
                     (data/access-with-default struct data key default))
       (get [this key]
            ;; Note: this is called by clojure to compare other maps
            ;; to this; but maybe in more situations.
            
            ;; Not sure if we should/could throw here too, if a key is
            ;; not in map, or stick to the java.Map contract by
            ;; returning null.
            (.getOrDefault this key nil))

       clojure.lang.IObj
       clojure.lang.IMeta
       (withMeta [this meta]
                 (create struct data meta))
       (meta [this]
             _meta)

       clojure.lang.IHashEq
       (hasheq [this] ;; called by (hash x)
               (when (= 0 (.-_hasheq this))
                 (set! (.-_hasheq this) (clojure.lang.APersistentMap/mapHasheq this)))
               (.-_hasheq this))
       (hashCode [this] ;; Java's hashCode
                 (when (= 0 ^int (.-_hash this))
                   (set! (.-_hash this) (clojure.lang.APersistentMap/mapHash this)))
                 (.-_hash this))

       ;; MapEquivalence marks that other maps should try to compare with this.
       clojure.lang.MapEquivalence

       clojure.lang.IPersistentMap
       (equiv [this other]
              (condp clj-instance? other
                PersistentClosedStructMap
                (and (= struct
                        (.-struct other))
                     (data/equiv data (.-data other)))

                clojure.lang.IPersistentMap
                ;; let other map implementations do the work.
                (.equiv ^clojure.lang.IPersistentMap other this)

                ;; else java maps, or anything else.
                (clojure.lang.APersistentMap/mapEquals this other)))
       (equals [this other] ;; Java's equals
               (clojure.lang.APersistentMap/mapEquals this other))

       (cons [this o]
             ;; Note: 'into' uses this if IEditableCollection is not implemented (and that otherwise)
             (let [changed-keys-vals (map-cons-o list o)
                   
                   changed-keys (map first changed-keys-vals)
                   changed-vals (map second changed-keys-vals)]
               
               (-> (create struct (reduce (fn [data [k v]]
                                            (data/mutate! struct data k v))
                                          (data/copy data)
                                          changed-keys-vals)
                           _meta)
                   (closed-struct/validate struct changed-keys changed-vals))))

       (seq [this] ;; seq of MapEntry
            (iterator-seq (.iterator this)))

       (iterator [this] ;; iterator over MapEntry
                 (data/java-iterator struct data))

       (assoc [this key val]
              (-> (create struct (data/mutate! struct (data/copy data) key val) _meta)
                  (closed-struct/validate struct (list key) (list val))))
       (assocEx [this key val]
                ;; assocEx is 'assoc if not set yet' - all our keys are always set.
                (throw (cannot-add key)))
       (without [this key]
                (throw (cannot-remove key)))
       (count [this]
              (closed-struct/size struct))
  
       (valAt [this key]
              (data/access struct data key))
       (valAt [this key not-found]
              (data/access-with-default struct data key not-found))
       (empty [this]
              ;; OPT: 'memoize' the empty val in struct? we can't create it before hand because of the validation :-/
              (-> (create struct (data/create struct) _meta)
                  ;; potentially all keys have changed, to nil
                  (closed-struct/validate struct (closed-struct/keys struct) (repeat (closed-struct/size struct) nil))))

       clojure.lang.IEditableCollection
       (asTransient [this]
                    (TransientClosedStructMap. struct (data/copy data) true))
       ]))

(defn- create [struct data meta]
  (PersistentClosedStructMap. struct data 0 0 meta))


#?(#_:clj
   ;; seems already implemented for all IPersistentMap
   #_(defmethod print-method PersistentClosedStructMap [^PersistentClosedStructMap s ^java.io.Writer writer]
     (print-method (into {} s) writer))

   #_:cljs
   #_(extend-protocol IPrintWithWriter
     PersistentClosedStructMap
     (-pr-writer [^PersistentClosedStructMap s writer x]
       ;; OPT: direct access to map printer? reimplement?
       (-pr-writer (into {} s) writer x))))

(defn instance? [t v]
  (assert (closed-struct/closed-struct? t)) ;; TODO: exception?
  (and (clj-instance? PersistentClosedStructMap v)
       (= (.-struct v)
          t)))

(defn accessor [t key]
  (let [data-f (data/accessor t key)]
    (fn [m]
      (assert (instance? t m)) ;; TODO: exception
      (data-f (.-data m))
      )))

(defn setter [t key]
  (let [data-f (data/mutator t key)
        key-list (list key)]
    (fn [m val]
      (assert (instance? t m)) ;; TODO: exception
      (-> (create t (data-f (data/copy (.-data m)) val) (meta m))
          (closed-struct/validate t key-list (list val))))))

(defn satisfies? [t v]
  (or (instance? t v)
      (and (map? v)
           (and (every? #(contains? v %) (closed-struct/keys t))
                ;; TODO: pass 'v' or (select-keys ... v) to validator?
                (closed-struct/valid? t v)))))

(defn- build-map* [struct key-val-pairs]
  ;; TODO: fail if not all keys given, or not? (records allowed that to some extend - via less fields in ctor)
  (assert (closed-struct/closed-struct? struct)) ;; TODO: exception
  
  ;; OPT: there should be more efficient ways to contruct it...? But for clj/struct-map we would to know the orginal order :-/
  ;; TODO: use transient, resp. not that many copies.
  (reduce (fn [res [k v]]
            (assoc res k v))
          (create struct (data/create struct) nil)
          key-val-pairs))

(defn build-map [struct keys-vals]
  (assert (even? (count keys-vals))) ;; TODO: exception
  (build-map* struct (partition 2 keys-vals)))

(defn from-map [struct m]
  (build-map* struct (seq m)))
