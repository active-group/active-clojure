(ns ^:no-doc active.clojure.struct.closed-struct-data
  "Mutable data structure backing a closed-struct-map."
  (:require [active.clojure.struct.closed-struct :as closed-struct]))

;; TODO: maybe this should do field validation?

(defn create [struct]
  (object-array (closed-struct/size struct)))

(defn unsafe-access [data index]
  (aget data index))

(defn access [struct data key]
  (unsafe-access data
                 (closed-struct/index-of struct key)))

(let [not-found #?(:clj (Object.) :cljs #js {})]
  (defn access-with-default [struct data key default]
    (let [index (closed-struct/maybe-index-of struct key not-found)]
      (if (identical? index not-found)
        default
        (aget data index)))))

(defn kv-reduce [struct data f init]
  (let [keys (closed-struct/keys struct)
        len (closed-struct/size struct)]
    (loop [res init
           idx 0]
      (if (< idx len)
        (let [res (f res (nth keys idx) (aget data idx))]
          (if (reduced? res)
            @res
            (recur res (inc idx))))
        res))))

(defn mutate! [struct data key value]
  (let [index (closed-struct/index-of struct key)]
    (aset data index value)
    data))

(defn copy [data]
  (aclone data))

(defn equiv [^objects v1 ^objects v2]
  ;; OPT: nothing built in clojure for this?
  (reduce (fn [r idx]
            (if-not r
              (reduced r)
              (= (aget v1 idx)
                 (aget v2 idx))))
          (= (alength v1) (alength v2))
          (range (alength v1))))

(defn indices [struct]
  (range (closed-struct/size struct)))

#?(:clj
   (defn java-iterator [struct data]
     (let [ks (closed-struct/keys struct)
           idxs (indices struct)]
       (let [^java.util.Iterator base (.iterator idxs)]
         (reify java.util.Iterator
           (hasNext [this]
             (.hasNext base))
           (next [this]
             (let [idx (.next base)]
               (clojure.lang.MapEntry. (nth ks idx) (unsafe-access data idx))))
           (remove [this]
             (throw (java.lang.UnsupportedOperationException.)))))))

   :cljs
   (defn js-iterator [struct data]
     (let [ks (closed-struct/keys struct)
           idxs (indices struct)]
       (let [base (-iterator idxs)]
         (reify Object
           (hasNext [_] (.hasNext ^js base))
           (next [_]
             (let [idx (.next ^js base)]
               (MapEntry. (nth ks idx) (unsafe-access data idx) nil)))))))
   )

#?(:cljs
   (defn js-seq [struct data]
     ;; OPT: special implementation needed? (but that's a lot: https://github.com/clojure/clojurescript/blob/219951678b16575ec29bb9b88047356cf1437aec/src/main/cljs/cljs/core.cljs#L6808)
     (map (fn [key idx]
            (MapEntry. key (unsafe-access data idx) nil))
          (closed-struct/keys struct)
          (indices struct))))
