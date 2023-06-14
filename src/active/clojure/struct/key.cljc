(ns ^:no-doc active.clojure.struct.key
  (:require [active.clojure.struct.closed-struct :as closed-struct])
  (:refer-clojure :exclude (set)))

(defprotocol ^:private IKey
  (-optimize-for! [this struct])
  (-optimized-for? [this struct] "Returns the index in struct or nil."))

(deftype ^:private Key [^clojure.lang.Symbol sym ^:unsynchronized-mutable struct ^:unsynchronized-mutable index]
  IKey
  (-optimize-for! [this struct]
    (let [idx (closed-struct/index-of struct this)]
      (set! (.-struct this) struct)
      (set! (.-index this) index)))
  (-optimized-for? [this s]
    (if (and (some? struct)
             (= s struct))
      index
      nil))
  
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
               (if (instance? Key other)
                 (= sym (.-sym other))
                 false))])

  #?@(:clj
      ;; Note: the struct-map implementation of get and assoc will look for 'struct' and 'index', and use it if set.
      [clojure.lang.IFn
       (invoke [this m] (get m this))
       (invoke [this m v] (assoc m this v))]

      :cljs
      [IFn
       (-invoke [this m] (get m this))
       (-invoke [this m v] (assoc m this v))]))

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

(defn optimize-for! [^Key key struct]
  ;; Note: should only be called once and immediately after construction/during definition;
  ;; therefor it's ok to just use :unsynchronized-mutable fields.
  (-optimize-for! key struct))

(defn optimized-for? [key struct]
  (and (instance? Key key)
       (-optimized-for? ^Key key struct)))
