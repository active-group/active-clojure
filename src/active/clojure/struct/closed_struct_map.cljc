(ns active.clojure.struct.closed-struct-map
  (:require [active.clojure.struct.validator :as v])
  #?(:clj (:import (clojure.lang Util)))
  (:refer-clojure :rename {instance? clj-instance?
                           satisfies? cljs-satisfies?
                           accessor open-struct-accessor
                           struct open-struct-map
                           create-struct create-open-struct}))

(defprotocol ^:private IClosedStruct
  (-get-validator [this])
  (-set-validator [this validator]))

(deftype ^:private ClosedStruct [field-set open-struct ^:unsynchronized-mutable validator]
  IClosedStruct
  (-get-validator [this]
    (.-validator this))
  (-set-validator [this validator]
    (set! (.-validator this) validator))
  #?@(:clj

      [clojure.lang.IHashEq
       (hasheq [this]
               (hash field-set))
       Object
       (equals [this other]
               (if (clj-instance? ClosedStruct other)
                 (= field-set (.-field-set other))
                 false))]))

(defn closed-struct? [v]
  (clj-instance? ClosedStruct v))

(defn closed-struct-keyset [^ClosedStruct t]
  (assert (closed-struct? t)) ;; TODO: exception?
  (.-field-set t))

(defn closed-struct-validator [^ClosedStruct t]
  (assert (closed-struct? t)) ;; TODO: exception?
  (-get-validator t))

(defn set-closed-struct-validator! [^ClosedStruct t validator]
  (-set-validator t validator))

(defn- validate [^clojure.lang.PersistentStructMap m ^ClosedStruct t changed-keys changed-values]
  ;; TODO: pass the PersistentClosedStructMap instead of the PersistentStructMap?
  (when-let [v (-get-validator t)]
    (v/validate! v m changed-keys changed-values))
  m)

(defn create-closed-struct [fields]
  (ClosedStruct. (set fields)
                 (apply create-open-struct fields)
                 nil))

(deftype ^:private PersistentClosedStructMap [struct ^clojure.lang.PersistentStructMap m]

  ;; TODO: Ifn taking key.
  #?@(:clj
      ;; Note: if we used gen-class instead of deftype, we could
      ;; extend APersistentMap; but using gen-class is also not easy
      ;; in terms of the interop.
      [java.lang.Iterable
       
       java.util.Map
       (size [this] (.size m))
       (isEmpty [this] (.isEmpty m))
       (containsKey [this k] (.containsKey m k))
       (keySet [this] (.keySet m))
       (getOrDefault [this key default]
                     (get this key default))
       (get [this key]
            ;; Note: this is called by clojure to compare other maps
            ;; to this; but maybe in more situations.
            
            ;; Not sure if we should/could throw here too, if a key is
            ;; not in map, or stick to the java.Map contract by
            ;; returning null.
            (get this key nil))

       clojure.lang.IHashEq
       (hasheq [this]
               (.hasheq m))
       
       clojure.lang.MapEquivalence

       clojure.lang.IPersistentMap
       (equiv [this other]
              (if (clj-instance? PersistentClosedStructMap other)
                (.equiv m (.-m other))
                (= other m)))

       (seq [this]
            (.seq m))

       (cons [this o]
             (let [check-key! (fn [k]
                                (when-not (.containsKey m k)
                                  (throw (Util/runtimeException "Not a key of struct"))))

                   ;; OPT: collecting the changes if we have no validator is superflous (do it lazily?)
                   [changed-keys changed-values]
                   (condp clj-instance? o
                     java.util.Map$Entry
                     (let [^java.util.Map$Entry e o]
                       (check-key! (.getKey e))
                       [(list (.getKey e))
                        (list (.getValue e))])

                     clojure.lang.IPersistentVector
                     (let [^clojure.lang.IPersistentVector v o]
                       (when (>= (count v) 1) ;; should be tuple [k v]
                         (check-key! (.nth v 0))
                         [(list (.nth v 0))
                          (list (.nth v 1))]))

                     ;; else: clojure.lang.IPersistentMap
                     (reduce (fn [ks-vs ^java.util.Map$Entry x]
                               (check-key! (.getKey x))
                               [(conj (first ks-vs) (.getKey x))
                                (conj (second ks-vs) (.getValue x))])
                             [[] []]
                             (seq o)))
                   new-m (-> (.cons m o)
                             (validate struct changed-keys changed-values))]
               (PersistentClosedStructMap. struct new-m)))

       (iterator [this]
                 (.iterator m))

       (assoc [this key val]
              (if (contains? m key)
                (PersistentClosedStructMap. struct (-> (.assoc m key val)
                                                       (validate struct (list key) (list val))))
                (throw (Util/runtimeException "Not a key of struct"))))
       (assocEx [this key val]
                ;; assocEx is 'assoc if not set yet' - all our keys are always set.
                (throw (Util/runtimeException "Key already present")))
       (without [this key]
                (throw (Util/runtimeException "Can't remove struct key")))
       (count [this]
              (.count m))
  
       (valAt [this key]
              (let [v (.valAt m key ::not-found)]
                (if (= v ::not-found)
                  (throw (Util/runtimeException "Not a key of struct"))
                  v)))
       (valAt [this key not-found]
              (.valAt m key not-found))
       (empty [this]
              (PersistentClosedStructMap. struct (-> (.empty m)
                                                     ;; potentially all keys have changed, to nil
                                                     (validate struct (keys m) (repeat (count m) nil)))))
       ]))

#?(:clj
   (defmethod print-method PersistentClosedStructMap [^PersistentClosedStructMap s ^java.io.Writer writer]
     (print-method (.-m s) writer))

   :cljs
   (extend-protocol IPrintWithWriter
     PersistentClosedStructMap
     (-pr-writer [^PersistentClosedStructMap s writer x]
       (-pr-writer (.-m s) writer x))))

(defn accessor [^ClosedStruct t key]
  (let [a (open-struct-accessor (.-open-struct t) key)]
    (fn [^PersistentClosedStructMap m]
      (assert (clj-instance? PersistentClosedStructMap m)) ;; TODO: exception
      (a (.-struct m)))))

(defn setter [^ClosedStruct t key]
  (assert (contains? (closed-struct-keyset t) key)) ;; TODO: exception
  ;; TODO: any way to optimize it? Probably not while basing impl on clojure/struct-map
  (fn [m v]
    (assert (clj-instance? PersistentClosedStructMap m)) ;; TODO: exception
    (assoc m key v)))

(defn instance? [^ClosedStruct t v]
  (and (clj-instance? PersistentClosedStructMap v)
       (= (.-struct v)
          t)))

(defn satisfies? [^ClosedStruct t v]
  (or (instance? t v)
      (and (map? v)
           (let [struct-keys (closed-struct-keyset t)]
             (and (every? #(contains? v %) struct-keys)
                  (if-let [validator (closed-struct-validator t)]
                    (v/valid? validator v)
                    true))))))

(defn- build-map* [struct key-val-pairs]
  ;; TODO: fail is not all keys given, or not? (records allowed that to some extend - via less fields in ctor)
  (assert (closed-struct? struct)) ;; TODO: exception
  (let [open-struct (.-open-struct struct)
        empty-m (open-struct-map open-struct)]
    ;; OPT: there should be more efficient ways to contruct it...? But for clj/struct-map we would to know the orginal order :-/
    ;; TODO: use transient
    (reduce (fn [res [k v]]
              (assoc res k v))
            (PersistentClosedStructMap. struct empty-m)
            key-val-pairs)))

(defn build-map [struct keys-vals]
  (assert (even? (count keys-vals))) ;; TODO: exception
  (build-map* struct (partition 2 keys-vals)))

(defn from-map [struct m]
  (build-map* struct (seq m)))
