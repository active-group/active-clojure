(ns ^:no-doc active.clojure.record-runtime
  (:refer-clojure :exclude [record?])
  #?(:cljs (:require-macros [active.clojure.record-runtime :refer [really-make-record record-check-rtd!]]))
  #?(:cljs (:require [goog.array])))

(defrecord RecordField [name])

(defn make-record-field
  [name]
  (RecordField. name))

(defrecord RecordTypeDescriptor [name uid fields])

(defn make-record-type-descriptor
  [name uid fields]
  (RecordTypeDescriptor. name uid (vec fields)))

(defn record-type-descriptor?
  [x]
  (instance? RecordTypeDescriptor x))

(defn record-type-descriptor-field-index
  [^RecordTypeDescriptor rtd name]
  (loop [i 0
         fields (.-fields rtd)]
    (if (empty? fields)
      ;; FIXME: more expressive exception
      (throw #?(:clj (new Error (str "field " name "not found in rtd " (.-name rtd)))
                :cljs (js/Error. (str "field " name "not found in rtd " (.-name rtd)))))
      (let [^RecordField field (first fields)]
        (if (= (.-name field) name)
          i
          (recur (inc i)
                 (rest fields)))))))

(defn ^:no-doc rtd= [this-rtd other-rtd]
  ;; Note: this assures that record-type definitions can be reloaded and still 'work' (non-generative)
  #?(:clj (.equals ^RecordTypeDescriptor this-rtd ^RecordTypeDescriptor other-rtd))
  #?(:cljs (= this-rtd other-rtd)))

(deftype Record
    [^RecordTypeDescriptor rtd
     ^{:tag #?(:clj "[Ljava.lang.Object;"
               :cljs "js/Object")} slots]
  #?@(:clj
      [java.lang.Object
       (equals [this other]
               (if (instance? Record other)
                 (let [this-rtd ^RecordTypeDescriptor (.rtd this)
                       this-slots ^{:tag "[Ljava.lang.Object;"} (.slots ^Record this)
                       other-rtd ^RecordTypeDescriptor (.rtd ^Record other)
                       other-slots ^{:tag "[Ljava.lang.Object;"} (.slots ^Record other)]
                   (and (rtd= this-rtd other-rtd)
                        (every? true? (map = this-slots other-slots))))
                 false)) ; must be `false`, `nil` is no Java value
       
       (hashCode [this]
                 (hash-combine (hash (.rtd this))
                               (hash (seq (.slots this)))))
       ;; deftype types implement this by default, it seems; better override it:
       clojure.lang.IHashEq
       (hasheq [this]
               (.hashCode this))]
      :cljs
      [IEquiv
       (-equiv [this other]
               (if (instance? Record other)
                 (let [this-rtd ^RecordTypeDescriptor (.-rtd this)
                       this-slots (.-slots this)
                       other-rtd ^RecordTypeDescriptor (.-rtd other)
                       other-slots (.-slots other)]
                   (and (= ^RecordTypeDescriptor this-rtd ^RecordTypeDescriptor other-rtd)
                        (.equals goog.array
                                 this-slots
                                 other-slots
                                 -equiv)))
                 false))
       IHash
       (-hash [this]
              (hash-combine (hash (.-rtd this))
                            (hash (array-seq (.-slots this)))))])
  Object
  (toString [^Record this]
    (let [rtd        (.-rtd this)
          rtd-fields (map (comp keyword :name) (seq (:fields rtd)))
          rtd-name   (:name rtd)
          slots      (seq (.-slots this))]
      (str rtd-name
           (into {} (mapv vector rtd-fields slots))))))

;; overriding default printing
#?(:clj
   (defmethod clojure.core/print-method Record [^Record rec ^java.io.Writer writer]
     (.write writer (str rec)))
   :cljs
   (extend-protocol IPrintWithWriter
     Record
     (-pr-writer [rec writer _]
       (write-all writer (str rec)))))

(defmacro really-make-record
  [rtd & vs]
  (let [a `a#]
    `(let [~a (object-array ~(count vs))]
       ~@(map-indexed (fn [i v]
                        `(aset ~a ~i ~v))
                      vs)
       (Record. ~rtd ~a))))

(defn make-record
  ([^RecordTypeDescriptor rtd]
   (Record. rtd (object-array 0)))
  ([^RecordTypeDescriptor rtd v0]
   (really-make-record rtd v0))
  ([^RecordTypeDescriptor rtd v0 v1]
   (really-make-record rtd v0 v1))
  ([^RecordTypeDescriptor rtd v0 v1 v2]
   (really-make-record rtd v0 v1 v2))
  ([^RecordTypeDescriptor rtd v0 v1 v2 v3]
   (really-make-record rtd v0 v1 v2 v3))
  ([^RecordTypeDescriptor rtd v0 v1 v2 v3 v4]
   (really-make-record rtd v0 v1 v2 v3 v4))
  ([^RecordTypeDescriptor rtd v0 v1 v2 v3 v4 v5]
   (really-make-record rtd v0 v1 v2 v3 v4 v5))
  ([^RecordTypeDescriptor rtd v0 v1 v2 v3 v4 v5 v6]
   (really-make-record rtd v0 v1 v2 v3 v4 v5 v6))
  ([^RecordTypeDescriptor rtd v0 v1 v2 v3 v4 v5 v6 v7]
   (really-make-record rtd v0 v1 v2 v3 v4 v5 v6 v7))
  ([^RecordTypeDescriptor rtd v0 v1 v2 v3 v4 v5 v6 v7 v8]
   (really-make-record rtd v0 v1 v2 v3 v4 v5 v6 v7 v8))
  ([^RecordTypeDescriptor rtd v0 v1 v2 v3 v4 v5 v6 v7 v8 v9]
   (really-make-record rtd v0 v1 v2 v3 v4 v5 v6 v7 v8 v9))
  ;; FIXME: more
  ([^RecordTypeDescriptor rtd v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 & vs]
   (let [a (object-array (+ 11 (count vs)))]
     (aset a 0 v0)
     (aset a 1 v1)
     (aset a 2 v2)
     (aset a 3 v3)
     (aset a 4 v4)
     (aset a 5 v5)
     (aset a 6 v6)
     (aset a 7 v7)
     (aset a 8 v8)
     (aset a 9 v9)
     (aset a 10 v10)
     (loop [vs vs
            i 11]
       (when-not (empty? vs)
         (aset a i (first vs))
         (recur (rest vs) (inc i))))
     (Record. rtd a))))

(defn record?
  [x]
  (instance? Record x))

(defn record-rtd
  [^Record r]
  (.-rtd r))

(defn record-of-type?
  [r ^RecordTypeDescriptor rtd]
  (and (record? r)
       (rtd= rtd (.-rtd ^Record r))))

(defn record-type-predicate [^RecordTypeDescriptor rtd]
  (fn [x]
    (record-of-type? x rtd)))

; assumes that ?r, ?rtd are identifiers alerady
(defmacro record-check-rtd!
  "Checks, if a given element is an rtd-record and if that's the case,
  if it has the same rtd as the given.

  Note: To prevent reflection warnings, we had to add the record symbol,
  which has a type hint attached.
  The outer if condition is needed, because primitive types can't be type hinted."
  [^RecordTypeDescriptor ?rtd ^Record ?r]
  (let [error (if (:ns &env)
                `(js/Error. (str "Not a record of the correct type [[" (:name ~?rtd) "]]" ":" (pr-str ~?r)))
                `(new Error (str "Not a record of the correct type [[" (:name ~?rtd) "]]" ":" (pr-str ~?r))))
        record (vary-meta (gensym) assoc :tag `Record)]
    (if (or (symbol? ?r) (list? ?r))
      `(do
         (when-not (and (record? ~?r)
                        (let [~record ~?r]
                          (rtd= ~?rtd (.-rtd ~record))))
           (throw ~error)))
      `(throw ~error))))

(defn record-get
  [^RecordTypeDescriptor rtd ^Record r ^long index]
  (record-check-rtd! rtd r)
  (aget ^{:tag "[Ljava.lang.Object;"} (.-slots r) index))

(defn record-update
  [^RecordTypeDescriptor rtd ^Record r ^long index v]
  (record-check-rtd! rtd r)
  (let [slots
        (aclone ^{:tag "[Ljava.lang.Object;"} (.-slots r))]
    (aset slots index v)
    (Record. (.-rtd r) slots)))

;; FIXME: lens
;; FIXME: serialization
