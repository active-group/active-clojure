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

(declare create)

(deftype ^:private PersistentClosedStructMap [struct data ^int ^:unsynchronized-mutable _hasheq ^int ^:unsynchronized-mutable _hash]

  ;; TODO: Ifn taking key. TODO: transient, TODO: with-meta
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
                                          changed-keys-vals))
                   (closed-struct/validate struct changed-keys changed-vals))))

       (seq [this] ;; seq of MapEntry
            (iterator-seq (.iterator this)))

       (iterator [this] ;; iterator over MapEntry
                 (data/java-iterator struct data))

       (assoc [this key val]
              (-> (create struct (data/mutate! struct (data/copy data) key val))
                  (closed-struct/validate struct (list key) (list val))))
       (assocEx [this key val]
                ;; assocEx is 'assoc if not set yet' - all our keys are always set.
                (throw (Util/runtimeException "Key already present")))
       (without [this key]
                (throw (Util/runtimeException "Can't remove struct key")))
       (count [this]
              (closed-struct/size struct))
  
       (valAt [this key]
              (data/access struct data key))
       (valAt [this key not-found]
              (data/access-with-default struct data key not-found))
       (empty [this]
              ;; OPT: 'memoize' the empty val in struct? we can't create it before hand because of the validation :-/
              (-> (create struct (data/create struct))
                  ;; potentially all keys have changed, to nil
                  (closed-struct/validate struct (closed-struct/keys struct) (repeat (closed-struct/size struct) nil))))
       ]))

(defn- create [struct data]
  (PersistentClosedStructMap. struct data 0 0))


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
      (-> (create t (data-f (data/copy (.-data m)) val))
          (closed-struct/validate t key-list (list val))))))

(defn satisfies? [t v]
  (or (instance? t v)
      (and (map? v)
           (and (every? #(contains? v %) (closed-struct/keys t))
                ;; TODO: pass 'v' or (select-keys ... v) to validator?
                (closed-struct/valid? t v)))))

(defn- build-map* [struct key-val-pairs]
  ;; TODO: fail is not all keys given, or not? (records allowed that to some extend - via less fields in ctor)
  (assert (closed-struct/closed-struct? struct)) ;; TODO: exception
  
  ;; OPT: there should be more efficient ways to contruct it...? But for clj/struct-map we would to know the orginal order :-/
  ;; TODO: use transient, resp. not that many copies.
  (reduce (fn [res [k v]]
            (assoc res k v))
          (create struct (data/create struct))
          key-val-pairs))

(defn build-map [struct keys-vals]
  (assert (even? (count keys-vals))) ;; TODO: exception
  (build-map* struct (partition 2 keys-vals)))

(defn from-map [struct m]
  (build-map* struct (seq m)))
