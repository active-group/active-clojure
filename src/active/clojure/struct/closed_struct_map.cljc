(ns active.clojure.struct.closed-struct-map
  #?(:clj (:import (clojure.lang Util)))
  (:refer-clojure :rename {instance? clj-instance?
                           accessor open-struct-accessor
                           struct open-struct-map
                           create-struct create-open-struct}))

(deftype ^:private ClosedStruct [field-set open-struct]

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
                                  (throw (Util/runtimeException "Not a key of struct"))))]
               (condp clj-instance? o
                 java.util.Map$Entry
                 (check-key! (.getKey ^java.util.Map$Entry o))

                 clojure.lang.IPersistentVector
                 (when (>= (count o) 1) ;; should be tuple [k v]
                   (check-key! (.nth ^clojure.lang.IPersistentVector o 0)))

                 ;; else: clojure.lang.IPersistentMap
                 (doseq [^java.util.Map$Entry x (seq o)]
                   (check-key! (.getKey x)))))
             (PersistentClosedStructMap. struct (.cons m o)))

       (iterator [this]
                 (.iterator m))

       (assoc [this key val]
              (if (contains? m key)
                (PersistentClosedStructMap. struct (.assoc m key val))
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
              (PersistentClosedStructMap. struct (.empty m)))
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
  ;; TODO: exception if key not in struct
  ;; TODO: any way to optimize it?
  (fn [m v]
    (assoc m key v)))

(defn instance? [^ClosedStruct t v]
  (and (clj-instance? PersistentClosedStructMap v)
       (= (.-struct v)
          t)))

(defn create-struct [fields]
  (ClosedStruct. (set fields) (apply create-open-struct fields)))

(defn build-map [struct keys-vals]
  (assert (closed-struct? struct)) ;; TODO: exception
  ;; TODO: check all keys are included, but not more; even args
  (let [open-struct (.-open-struct struct)
        m (apply open-struct-map open-struct (map second (partition 2 keys-vals)))]
    (PersistentClosedStructMap. struct m)))

(defn from-map [struct m]
  (assert (closed-struct? struct)) ;; TODO: exception
  ;; TODO: check all keys are included, but not more
  (let [open-struct (.-open-struct struct)
        m (apply open-struct-map open-struct (vals m))]
    (PersistentClosedStructMap. struct m)))
