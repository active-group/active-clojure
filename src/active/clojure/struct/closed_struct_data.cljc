(ns ^:no-doc active.clojure.struct.closed-struct-data
  "Mutable data structure backing a closed-struct-map."
  (:require [active.clojure.struct.closed-struct :as closed-struct]))

;; TODO: maybe this should do field validation?

(defn create [struct]
  (object-array (closed-struct/size struct)))

(defn accessor [struct key]
  (let [index (closed-struct/index-of struct key)]
    (fn [^objects data]
      (aget data index))))

(defn access [struct data key]
  (let [index (closed-struct/index-of struct key)]
    (aget data index)))

(defn access-with-default [struct data key default]
  (let [index (closed-struct/maybe-index-of struct key ::not-found)]
    (if (= index ::not-found)
      default
      (aget data index))))

(defn mutator [struct key]
  (let [index (closed-struct/index-of struct key)]
    (fn [^objects data value]
      (aset data index value)
      data)))

(defn mutate! [struct data key value]
  (let [index (closed-struct/index-of struct key)]
    (aset data index value)
    data))

(defn copy [data]
  (aclone data))

(defn equiv [^objects v1 ^objects v2]
  (reduce (fn [r idx]
            (if-not r
              (reduced r)
              (= (aget v1 idx)
                 (aget v2 idx))))
          (= (alength v1) (alength v2))
          (range (alength v1))))

#?(:clj
   (defn java-iterator [struct data]
     (let [ks (closed-struct/keys struct)
           idxs (range (count ks))]
       (let [^java.util.Iterator base (.iterator idxs)]
         (reify java.util.Iterator
           (hasNext [this]
             (.hasNext base))
           (next [this]
             (let [idx (.next base)]
               (clojure.lang.MapEntry. (get ks idx) (aget data idx))))
           (remove [this]
             (throw (java.lang.UnsupportedOperationException.))))))))
