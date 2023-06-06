(ns ^:no-doc active.clojure.struct.key
  (:require [active.clojure.struct.closed-struct-map :as closed-struct-map])
  (:refer-clojure :exclude (set)))

(defprotocol ^:private IKey
  (-optimize! [this accessor setter]))

(defn- simple-access [m k]
  (get m k))

(defn- simple-set [m k v]
  (assoc m k v))

(defn- access [m k optimized]
  (if optimized (optimized m) (simple-access m k)))

(defn- set [m k v optimized]
  (if optimized (optimized m v) (simple-set m k v)))

(deftype ^:private Key [^clojure.lang.Symbol sym ^:unsynchronized-mutable accessor ^:unsynchronized-mutable setter]
  IKey
  (-optimize! [this accessor setter]
    (set! (.-accessor this) accessor)
    (set! (.-setter this) setter))
  
  #?@(:clj
      [clojure.lang.IHashEq
       (hasheq [this]
               (.hasheq sym))

       ;; TODO: withMeta meta, clojure.lang.IObj
       ;; TODO? java.lang.Comparable

       Object
       (hashCode [this]
                 (.hashCode sym))
       
       (equals [this other]
               ;; TODO: this con't make it symmetrical (= x y) (= y x) - how does that work?
               (if (instance? Key other)
                 (= sym (.-sym other))
                 false))])

  #?@(:clj
      [clojure.lang.IFn
       (invoke [this m] (access m this accessor))
       (invoke [this m v] (set m this v setter))]

      :cljs
      [IFn
       (-invoke [this m] (access m this accessor))
       (-invoke [this m v] (set m this v setter))]))

#?(:clj
   ;; add ~ to the namespaced symbol, so that quasiquoting the output
   ;; is an expression yielding the value.
   
   (defmethod print-method Key [sk ^java.io.Writer writer]
     (.write writer "~")
     (.write writer (name (.-sym sk))))

   :cljs
   (extend-protocol IPrintWithWriter
     Key
     (-pr-writer [sk writer _]
       (write-all writer "~")
       (write-all writer (name (.-sym sk))))))

(defn make [sym]
  (Key. sym nil nil))

(defn- optimized-accessor [struct key]
  (let [f (closed-struct-map/accessor struct key)]
    (fn [m]
      (if (closed-struct-map/instance? struct m)
        (f m)
        (simple-access m key)))))

(defn- optimized-setter [struct key]
  (let [f (closed-struct-map/setter struct key)]
    (fn [m v]
      (if (closed-struct-map/instance? struct m)
        (f m v)
        (simple-set m key v)))))

(defn optimize-for! [^Key key struct]
  ;; Note: should only be called once and immediately after construction/during definition;
  ;; therefor it's ok to just use :unsynchronized-mutable fields.
  (-optimize! key
              (optimized-accessor struct key)
              (optimized-setter struct key)))
