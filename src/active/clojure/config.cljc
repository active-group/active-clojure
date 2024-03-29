(ns active.clojure.config
  "Application configuration via a big map.

A configuration is a nested key-value map.

It contains top-level settings and sections of qualified settings, like so:

    {:top-level-setting-key 'foo
     {:section-key
      {:section-setting 'bar}}}

Additionally, a config contains *profiles* with additional
settings that can be mixed in, like so:

    {:top-level-setting-key 'foo
     {:section-key
      {:section-setting 'bar}}
     {:profiles
      {:dev
       {:top-level-setting-key 'bar
        {:section-key
         {:section-setting 'baz}}}}}}

Each profile has the same format as the top-level configuration itself
  (sans the `:profiles` key)."
  (:refer-clojure :exclude [boolean?])
  #?@
   (:clj
    [(:require
      [active.clojure.condition :as c]
      [active.clojure.record :refer :all]
      [active.clojure.lens :as lens]
      [clojure.set :as set])
     (:import java.net.URL)]
    :cljs
    [(:require [active.clojure.condition :as c]
               active.clojure.cljs.record
               [active.clojure.lens :as lens]
               [clojure.set :as set])
     (:require-macros
      [active.clojure.cljs.record :refer [define-record-type]])]))

;; TODO
;; - provide better support for reaching inside of collection ranges

(define-record-type
  ^{:doc "Description of a range of values."}
  ValueRange ; used to be called Range, but conflicts with cljs.core/->Range
  (^{:doc "Make a [[Range]] range object.
  - `description' is a human-readable string
  - `completer' is a function that accepts a range, a path, and a value,
     and either returns a \"completed\" value that takes defaults etc. into account,
     or a [[RangeError]] object.
  - `reduce` is a function accepting a range, a path, a function, an initial result,
     and a value, reducing collection values similar to clojure [[reduce]], calling
     `(f range path res v)` on all scalar values.
  - `diff` is a function accepting two values that returns a sequence of `[path v1 v2]`, see
    diff functions below"
     }
   really-make-range description completer reduce diff)
  range?
  [description range-description
   ;; takes the range, a key that says where the value was found, and the value
   ;; returns either a "completed" value or a range error
   completer range-completer
   reduce range-reduce
   diff range-diff])

(defn make-range
  ([description completer reduce]
   (make-range description completer reduce (fn [a b] [[nil a b]])))
  ([description completer reduce diff]
   (really-make-range description completer reduce diff)))

(define-record-type
  ^{:doc "Description of an error that occurred during range checking"}
  RangeError
  (^{:doc "Make a a [[RangeError]] object describing an error from range checking.
  - `range` is the range that caused the error
  - `path` is the path in the configuration that describes where the error is.
  - `value` is the value that was wrong.

  `range` can be `nil' if `key` does not appear in the schema."}
   make-range-error range path value)
  range-error?
  [range range-error-range
   path range-error-path
   value range-error-value]
  Object
  (toString [^RangeError this]
     (str "Range error at path " (vec (range-error-path this)) ": "
          "value " (pr-str (range-error-value this)) " is not in range "
          (range-description (range-error-range this)))))

(defn scalar-range-reduce
  [completer]
  (fn [range path f res val]
    (let [v' (completer range path val)]
      (c/assert (not (range-error? v')) (pr-str v'))
      (f range path res v'))))

(defn make-scalar-range
  "Make a range for unstructured, non-collection ranges."
  ([description completer]
   (make-range description completer (scalar-range-reduce completer)))
  ([description completer diff]
   (make-range description completer (scalar-range-reduce completer) diff)))

(defn any-value-range
  "Range for any value at all."
  [dflt]
  (make-scalar-range "any value at all"
                     (fn [range path val]
                       (if (nil? val)
                         dflt
                         val))))

(def non-nil-range
  "Range for any non-nil value."
  (make-scalar-range "non-nil value"
                     (fn [range path val]
                       (if (some? val)
                         val
                         (make-range-error range path val)))))

(defn predicate-range
  "Range specified by a simple predicate."
  [desc pred dflt]
  (make-scalar-range desc
                     (fn [range path val]
                       (cond
                         (nil? val) dflt
                         (pred val) val
                         :else (make-range-error range path val)))))

(defn boolean?
  "Check if a value is a boolean."
  [x]
  (or (= x true)
      (= x false)))

(defn boolean-range
  "Range for a boolean, with explicit default."
  [dflt]
  (predicate-range
   "true or false" boolean? dflt))

(defn optional-range
  "Range for something that may be in an underlying range or `nil`."
  [range]
  (make-range (str (range-description range) " (optional)")
              (fn [this-range path val]
                (if (nil? val)
                  nil
                  ((range-completer range) range path val)))
              (fn [this-range path f res val]
                (if (nil? val)
                  (f range path res nil) ;; or just res?
                  ((range-reduce range) range path f
                   res val)))
              (fn [a b] ((range-diff range) a b))))

(defn optional-default-range
  "Range for something that may be in an underlying range. If it is nil, then `dflt` is used, which must be in the underlying range too."
  [range dflt]
  (make-range (str (range-description range) " (optional with default)")
              (fn [this-range path val]
                ((range-completer range) range path
                 (if (nil? val)
                   dflt
                   val)))
              (fn [this-range path f res val]
                ((range-reduce range) range path f
                 res (if (nil? val)
                       dflt
                       val)))
              (fn [a b] ((range-diff range) a b))))

(defn integer-between-range
  "Range for an integer from a specified range, with explicit default."
  [min max dflt]
  (make-scalar-range (str "integer between " min " and " max)
                     (fn [range path val]
                       (cond
                         (nil? val) dflt

                         (and (integer? val)
                              (>= val min)
                              (<= val max))
                         val

                         :else (make-range-error range path val)))))

(def keyword-range
  (make-scalar-range "keyword"
                     (fn [range path val]
                       (if (keyword? val)
                         val
                         (make-range-error range path val)))))

(defn default-string-range
  [dflt]
  "Range for an abitrary string, default is `dflt`."
  (make-scalar-range "string"
                     (fn [range path val]
                       (cond
                         (nil? val) dflt
                         (string? val) val
                         :else (make-range-error range path val)))))

(def string-range
  "Range for an abitrary string, default is empty string."
  (default-string-range ""))

(defn nonempty-string-range
  "Range for a non-empty string with optional max length."
  [& [max-length]]
  (make-scalar-range (str "non-empty string" (if (some? max-length)
                                               (str " with maximum length of " max-length)
                                               ""))
                     (fn [range path val]
                       (if (and (string? val) (not (empty? val))
                                (if (some? max-length) (<= (count val) max-length) true))
                         val
                         (make-range-error range path val)))))

(defn max-string-range [max-length]
  "Range for an arbitrary string with maximum length."
  (make-scalar-range (str "Arbitrary string with maximum length of " max-length)
                     (fn [range path val]
                       (if (and (string? val)
                                (<= (count val) max-length))
                         val
                         (make-range-error range path val)))))

(defn any-range
  "Range that satisfies one of the ranges, tried from left to right."
  [& rs]
  (make-range (apply str "any:" (map #(str " <" (range-description %) ">") rs))
              (fn [range path val]
                (loop [rs rs]
                  (if (empty? rs)
                    (make-range-error range path val)
                    (let [res ((range-completer (first rs)) (first rs) path val)]
                      (if (range-error? res)
                        (recur (rest rs))
                        res)))))
              (fn [range path f res val]
                (loop [rs rs]
                  (if (empty? rs)
                    (c/assert false)
                    (let [this-range (first rs)
                          v ((range-completer this-range) this-range path val)]
                      (if (range-error? v)
                        (recur (rest rs))
                        ((range-reduce this-range) this-range path f res v))))))))

(defn one-of-range
  "Range for one of a set of values, with explicit default."
  [vals dflt]
  (make-scalar-range (apply str "one of:"
                            (map #(str " " %) vals))
                     (let [s (set vals)]
                       (fn [range path val]
                         (cond
                           (nil? val) dflt

                           (contains? s val) val

                           :else (make-range-error range path val))))))

(defn or-dot-range
  "Given range `r` is required in the first line,
the remainder of the lines the field holds \".\"."
  [r]
  (any-range (one-of-range #{"."} nil) r))

(defn one-of-range-custom-compare
  "Range for one of a set of values, with custom compare function,
  with explicit default."
  [vals dflt compare-fn]
  (make-scalar-range (apply str "one of:"
                            (map #(str " " %) vals))
                     (let [s (set vals)]
                       (fn [range path val]
                         (cond
                           (nil? val) dflt

                           (some #(compare-fn val %) s) val

                           :else (make-range-error range path val))))))

;; Argl ...
(defn sequable?
  "Test if something can be coerced to a seq."
  [thing]
  (try
    (seq thing)
    true
    (catch #?(:clj Throwable) #?(:cljs js/Error) e
           false)))

(defn sequence-of-range
  "Range for a sequence of values of an underlying range."
  [range]
  (make-range (str "sequence of " (range-description range))
              (let [complete (range-completer range)]
                (fn [this-range path val]
                  (cond
                   (nil? val) []

                   (not (sequable? val)) (make-range-error this-range path val)

                   :else
                   (loop [i 0
                          vals (seq val)
                          ret []]
                     (if vals
                       (let [res (complete range (conj path i) (first vals))]
                         (if (range-error? res)
                           res
                           (recur (inc i)
                                  (next vals)
                                  (conj ret res))))
                       ret)))))
              (fn [this-range path f res val]
                (let [v ((range-completer this-range) this-range path val)]
                  (c/assert (not (range-error? v)) (pr-str v))
                  (reduce (fn [res [i x]]
                            ((range-reduce range) range (conj path i) f res x))
                          res
                          (map-indexed vector v))))))

(defn range-map
  "Range constructed by transforming values matching an existing range."
  [descr range f & args]
  (make-range descr
              (let [complete (range-completer range)]
                (fn [this-range path val]
                  (let [res (complete this-range path val)]
                    (if (range-error? res)
                      res
                      (apply f res args)))))
              (range-reduce range) ;; ?? TODO correct?
              ))

(defn set-of-range
  "Range for a set of values of an underlying range."
  [range]
  (range-map (str "set of " (range-description range))
             (sequence-of-range range)
             set))

(defn tuple-of-range
  "Range for a sequence of mixed underlying ranges."
  [& rs]
  (make-range (apply str "tuple of: " (interpose ", " (map range-description rs)))
              (fn [this-range path val]
                (cond
                  (sequable? val)
                  (let [res (map-indexed (fn [i [v range]]
                                           ((range-completer range) range (conj path i) v))
                                         (map vector
                                              (seq val)
                                              rs))]
                    (or (some #(and (range-error? %) %) res)
                        (vec res)))

                  :else
                  (make-range-error this-range path val)))
              (fn [this-range path f res val]
                (let [v ((range-completer this-range) this-range path val)]
                  (c/assert (not (range-error? v)) (pr-str v))
                  (reduce (fn [res [i [v range]]]
                            ((range-reduce range) range (conj path i)
                             f res v))
                          res
                          (map-indexed vector (map vector v rs)))))
              ))

(defn map-of-ranges-diff-fn
  ;; diff: keys of map-of-ranges are added to path and the values
  ;; then are diffed themselves
  [range]
  (fn [a b]
    (let [keys-in-both
          (set/union (set (keys a)) (set (keys b)))]
      (remove nil? (mapcat (fn [k] (let [v-a (get a k)
                                         v-b (get b k)]
                                     (if (not= v-a v-b)
                                       (map (fn [[p' v1' v2']]
                                              [(concat [k] p') v1' v2'])
                                            ((range-diff range) (get a k) (get b k)))
                                       nil)))
                           keys-in-both)))))

(defn map-of-range
  "Range for a map with keys and values of underlying ranges, respectively."
  [key-range val-range]
  (make-range (str "map from " (range-description key-range) " to " (range-description val-range))
              (let [complete-val (range-completer val-range)
                    complete-key (range-completer key-range)]
                (fn [this-range ky vl] ; we need the key & val functions
                  (cond
                   (nil? vl) {}

                   (map? vl)
                   (loop [kvs (seq vl)
                          ret {}]
                     (if kvs
                       (let [kv (first kvs)
                             p (conj ky (key kv))
                             k (complete-key key-range p (key kv))
                             v (complete-val val-range p (val kv))]
                         (cond
                          (range-error? k) k
                          (range-error? v) v
                          :else (recur (next kvs)
                                       (assoc ret k v))))
                       ret))

                   :else (make-range-error this-range ky vl))))
              (fn [this-range path f res val]
                (let [v ((range-completer this-range) this-range path val)]
                  (c/assert (not (range-error? v)) (pr-str v))
                  (reduce (fn [res [k v]]
                            ((range-reduce val-range) val-range (conj path k) f
                             ((range-reduce key-range) key-range path f res k)
                             v))
                          res
                          v)))
              (map-of-ranges-diff-fn val-range)))

#?(:clj (def slurpable-range
          "Range for something that may be passed to [[slurp]]."
          (make-scalar-range "Slurpable"
                             (fn [range path val]
                               (cond
                                 (instance? URL val) val
                                 (string? val) val
                                 ;; FIXME: more cases?
                                 :else (make-range-error range path val))))))

;; Schemas

(define-record-type
  ^{:doc "Named setting within a config."}
  Setting
  (^{:doc "Make a named schema [[Setting]] object.
  - `key` is a keyword naming the setting
  - `description` is a human-readable description object
  - `range` is a [[Range]] for the admissible values of the setting
  - `inherit?` says whether the setting values may be inherited from a surrounding section"}
   make-setting key description range inherit?)
  setting?
  [key setting-key
   description setting-description
   range setting-range
   inherit? setting-inherit?])

(defn setting
  "Construct a setting.
  - `key` is a keyword naming the setting
  - `description` is a human-readable description object
  - `range` is a [[Range]] for the admissible values of the setting
  - `inherit?` says whether the setting values may be inherited from a surrounding section"
  [key description range & {:keys [inherit?]}]
  {:pre [(string? description)]}
  (c/assert (not (identical? key :profiles)) "a setting can't be called :profiles")
  (make-setting key
                description
                range
                (if inherit? true false)))

(defn setting-default-value
  "Compute the default value for a setting."
  [setting]
  (let [range (setting-range setting)
        val ((range-completer range) range
             [(setting-key setting)]
             nil)]
    (if (range-error? val)
      (c/error `setting-default-value
               (str "setting " (vec (range-error-path val)) " is missing in configuration map")
               val)
      val)))

(define-record-type
  ^{:doc "Section within a config with settings of its own."}
  Section
  (make-section key schema inherit?)
  section?
  [^{doc "keyword naming the section"}
   key section-key

   ^{doc "sub-schema describing the section's format"}
   schema section-schema

   ^{doc "whether this section inherits from outer levels"}
   inherit? section-inherit?])

(defn section
  "Make a section within a config with settings of its own."
  [key schema & {:keys [inherit?]}]
  (c/assert (not (identical? key :profiles)) "a section can't be called :profiles")
  (make-section key schema (if inherit? true false)))

(define-record-type
  ^{:doc "A schema describes a map-structured config format, and can be used for
  validation and completion."}
  Schema
  (^{:doc "Make a [[Schema]] object describing a config format.
  *For internal use;* you should use [[schema]].

  - `description` is a human-readable description
  - `settings` is a collection of [[Setting]]s
  - `settings-map` is a map from setting keys to settings
  - `sections` is a collection of [[Section]]s
  - `sections-map` is a map from section keys to sections"}
   make-map-schema description settings settings-map sections sections-map)
  map-schema?
  [description map-schema-description
   settings map-schema-settings
   settings-map map-schema-settings-map
   sections map-schema-sections
   sections-map map-schema-sections-map])

(define-record-type
  ^{:doc "A sequence schema describes a sequence config format."}
  SequenceSchema
  (make-sequence-schema description element-schema non-empty?)
  sequence-schema?
  [description sequence-schema-description
   element-schema sequence-schema-element-schema
   non-empty? sequence-schema-non-empty?])

(defn sequence-schema
  [desc el-schema]
  (make-sequence-schema desc el-schema false))

(defn nonempty-sequence-schema
  [desc el-schema]
  (make-sequence-schema desc el-schema true))

(declare normalize&check-config-object)

(declare schema-range)

(defn- schema-reduce
  ;; FIXME: docstring
  [schema range path f res val]
  (cond
    (map-schema? schema)
    (let [cmap ((range-completer range) range path val)
          settings (map-schema-settings-map schema)
          sections (map-schema-sections-map schema)]
      (c/assert (not (range-error? cmap)) (pr-str cmap))
      (reduce (fn [res [k v]]
                (if-let [setting (get settings k)]
                  ((range-reduce (setting-range setting))
                   (setting-range setting)
                   (conj path (setting-key setting))
                   f res v)
                  ;; if not a setting, it must be a section:
                  (let [section (get sections k)
                        schema (section-schema section)]
                    (schema-reduce schema
                                   (schema-range schema)
                                   (conj path (section-key section))
                                   f res v))))
              res
              cmap))

    ;; FIXME: test for this case
    (sequence-schema? schema)
    (let [v ((range-completer range) range path val)
          element-schema (sequence-schema-element-schema schema)
          element-range (schema-range element-schema)]
      (c/assert (not (range-error? v)) (pr-str v))
      (reduce (fn [res [i x]]
                (schema-reduce element-schema
                               element-range
                               (conj path i) f res x))
              res
              (map-indexed vector v)))))

(defn schema-range
  "Range for a configuration object matching a schema."
  [schema]
  (cond
    (map-schema? schema)
    (make-range (map-schema-description schema)
                (fn [range path val]
                  (cond
                    (nil? val) (normalize&check-config-object schema [] {})
                    (map? val) (normalize&check-config-object schema [] val)
                    :else (make-range-error range path val)))
                (partial schema-reduce schema))

    (sequence-schema? schema)
    (sequence-of-range (schema-range (sequence-schema-element-schema schema)))))

; Note that a global setting which can be overridden locally needs to
; be listed both at the top level and within the section.

; FIXME: check that they're both identical

(defn schema
  "Construct a map schema.

  - `description` is a human-readable description
  - `element-list' is a list of the [[Setting]]s and [[Section]]s of the schema"
  [description & element-list]
  {:pre [(string? description)]}
  ;; FIXME: should make sure there are no duplicates
  (let [settings (filter setting? element-list)
        sections (filter section? element-list)]
    (make-map-schema
     description
     (set settings)
     (zipmap (map setting-key settings) settings)
     (set sections)
     (zipmap (map section-key sections) sections))))

(defn- merge-config-objects-sans-profiles
  "Merge two configs into one, with the latter taking precedence.

  This helper assumes there are no profiles."
  [schema path c1 c2]
  (cond
    (map-schema? schema)
    (do
      (when-not (map? c1)
        (c/error `merge-config-objects-sans-profiles
                 (str "configuration at " path " is not a map: " c1)
                 path c1))
      (when-not (map? c2)
        (c/error `merge-config-objects-sans-profiles
                 (str "configuration at " path " is not a map: " c2)
                 path c2))
      (let [sections-map (map-schema-sections-map schema)
            settings-map (map-schema-settings-map schema)]
        (loop [c {}
               all-keys (seq (set/union (set (keys c1))
                                        (set (keys c2))))]
          (if all-keys
            (let [key (first all-keys)
                  val1 (get c1 key)
                  val2 (get c2 key)]
              (if (contains? settings-map key)
                (recur (assoc c
                              ;; that `nil` is a valid value
                              key
                              (if (contains? c2 key)
                                val2
                                val1))
                       (next all-keys))
                (if-let [section (get sections-map key)]
                  (recur (assoc c key
                                (merge-config-objects-sans-profiles (section-schema section) (conj (vec path) key) (or val1 {}) (or val2 {})))
                         (next all-keys))
                  (c/error `merge-config-objects-sans-profiles
                           (str "unknown path " (vec (conj path key)) " in config")
                           (conj path key) (if (contains? c1 key) val1 val2) nil (if (contains? c1 key) c1 c2)))))
            c))))

    (sequence-schema? schema)
    (do
      (when-not (sequable? c1)
        (c/error `merge-config-objects-sans-profiles
                 (str "configuration at " path " is not a sequence: " (pr-str c1))
                 path c1))
      (when-not (sequable? c2)
        (c/error `merge-config-objects-sans-profiles
                 (str "configuration at " path " is not a sequence: " (pr-str c2))
                 path c2))

      (concat c1 c2))))

(defn merge-config-objects
  "Merge several config maps into one, with the latter taking precedence."
  ([schema c] c)
  ([schema c1 c2]
     (let [profiles-1 (get c1 :profiles)
           profiles-2 (get c2 :profiles)]
       (assoc (merge-config-objects-sans-profiles schema []
                                               (dissoc c1 :profiles)
                                               (dissoc c2 :profiles))
         :profiles (merge profiles-1 profiles-2))))
  ([schema c1 c2 & cs] ; Clojure won't let us do [schema c1 & cs]
     (reduce (partial merge-config-objects schema) (concat [c1 c2] cs))))

(defn- apply-profiles
  "Apply named profiles within a config map."
  [schema config-object profile-names]
  (if-let [profile-map (:profiles config-object)]
    (let [config-object (dissoc config-object :profiles)
          profiles (map (fn [n]
                          (or (profile-map n)
                              (c/error `apply-profiles "profile does not exist" n)))
                        profile-names)]
      (reduce (partial merge-config-objects-sans-profiles schema []) config-object profiles))
    config-object))

(declare normalize&check-config-object-internal)

(defn complete-section
  [inherited-map path section]
  (let [key (section-key section)]
    (if-let [entry (get inherited-map key)]
      {key entry}
      (let [res
            (normalize&check-config-object-internal (section-schema section) {}
                                                    inherited-map (concat path [key]))]
        (c/assert (not (range-error? res)) (pr-str res))
        {key res}))))

(defn complete-settings
  [inherited-map settings-map]
  (zipmap (keys settings-map)
          (map (fn [setting]
                 (or (get inherited-map (setting-key setting))
                     (setting-default-value setting)))
               (vals settings-map))))

(defn check-section
  [section val inherited-map path]
  (normalize&check-config-object-internal (section-schema section)
                                          val
                                          inherited-map
                                          (conj (vec path) (section-key section))))

(defn- normalize&check-config-object-internal
  [schema config inherited-map path]
  (letfn []
    (cond
      (map-schema? schema)
      (if (and (not (map? config))
               (not (nil? config)))
        (make-range-error :not-a-map path config)
        (let [sections-map (map-schema-sections-map schema)
              res
              ;; go through the settings first, as we need to collect
              ;; the inherited settings
              (loop [entries (seq config)
                     c {}
                     inherited-map inherited-map
                     settings-map (map-schema-settings-map schema)]
                (if entries
                  (let [[key val] (first entries)]
                    (if-let [setting (get settings-map key)]
                      (let [range (setting-range setting)
                            res ((range-completer range) range (concat path [key]) val)]
                        (if (range-error? res)
                          res
                          (recur (next entries)
                                 (assoc c key res)
                                 (if (setting-inherit? setting)
                                   (assoc inherited-map key val)
                                   inherited-map)
                                 (dissoc settings-map key))))
                      (if (contains? sections-map key) ; do sections later
                        (recur (next entries) c inherited-map settings-map)
                        (make-range-error :unknown-section (concat path [key]) val))))
                  [(merge c (complete-settings inherited-map settings-map))
                   inherited-map settings-map]))]
          (if (range-error? res)
            res
            (let [[c inherited-map settings-map] res]
              ;; now go through the sections
              (loop [entries (seq config)
                     c c
                     inherited-map inherited-map
                     sections-map sections-map]
                (if entries
                  (let [[key val] (first entries)]
                    (if-let [section (get sections-map key)]
                      (let [res (check-section section val inherited-map path)]
                        (if (range-error? res)
                          res
                          (recur (next entries)
                                 (assoc c key res)
                                 (if (section-inherit? section)
                                   (assoc inherited-map key res)
                                   inherited-map)
                                 (dissoc sections-map key))))
                      (recur (next entries) c inherited-map sections-map)))
                  (apply merge c
                         (map (partial complete-section inherited-map path)
                              (vals sections-map)))))))))

      (sequence-schema? schema)
      (let [el-schema (sequence-schema-element-schema schema)]
        (loop [idx 0
               els config   ; FIXME: now misnamed
               res (transient [])]
          (if (empty? els)
            (let [result (persistent! res)]
              (if (and (sequence-schema-non-empty? schema) (empty? result))
                (make-range-error :nonempty-sequence-must-not-be-empty path result)
                result))
            (let [r (normalize&check-config-object-internal el-schema
                                                            (first els)
                                                            inherited-map
                                                            (concat path [idx]))]
              (if (range-error? r)
                r
                (recur (+ 1 idx)
                       (rest els)
                       (conj! res r))))))))))

(defn normalize&check-config-object
  "Normalize and check the validity of a configuration object.

  In  the result, every setting has an associated value."
  ([schema config]
   (normalize&check-config-object schema [] config {} []))

  ([schema profile-names config]
   (normalize&check-config-object schema profile-names config {} []))

  ([schema profile-names config inherited-map path]
   (let [config (apply-profiles schema config profile-names)]
     (normalize&check-config-object-internal schema config inherited-map path))))

(define-record-type ^{:doc "Validated and expanded configuration object."}
  Configuration
  (really-make-configuration object schema)
  configuration?
  [object configuration-object
   schema configuration-schema])

(defn make-configuration
  "Make a configuration from a map."
  ([schema config-object]
   (make-configuration schema [] config-object))
  ([schema profile-names config-object]
   (let [res (normalize&check-config-object schema profile-names config-object)]
     (if (range-error? res)
       (c/error `make-configuration
                (str "range error at path "
                     (vec (range-error-path res))
                     " in configuration map: "
                     "value "
                     (pr-str (range-error-value res))
                     " should be in range "
                     (let [r (range-error-range res)]
                       (if (range? r)
                         (range-description r)
                        ;; FIXME: should look at this
                         (pr-str r)))
                     " but isn't")
                res)
       (really-make-configuration res schema)))))

(defn diff-setting-values
  "Returns sequence of triples `[path-vector version-1 version-2]` of settings that differ.
   Handles some ranges more intelligently."
  [setting path v1 v2]
  (map (fn [[p' v1' v2']]
         [(concat path p') v1' v2'])
       ((range-diff (setting-range setting)) v1 v2)))

(defn diff-configuration-objects
  "Returns sequence of triples `[path-vector version-1 version-2]` of settings that differ.

  The config objects must be validated and completed."
  [schema config-object-1 config-object-2]
  (cond
    (map-schema? schema)
    (concat (filter identity
                    (mapcat
                     (fn [[key setting]]
                       (let [v1 (get config-object-1 key)
                             v2 (get config-object-2 key)]
                         (if (not= v1 v2)
                           (diff-setting-values setting [key] v1 v2)
                           [])))
                     (map-schema-settings-map schema)))
            (mapcat (fn [[key section]]
                      (map (fn [[path v1 v2]]
                             [(vec (cons key path)) v1 v2])
                           (diff-configuration-objects (section-schema section)
                                                       (get config-object-1 key) (get config-object-2 key))))
                    (map-schema-sections-map schema)))

    (sequence-schema? schema)
    (let [count1 (count config-object-1)
          count2 (count config-object-2)
          common-count (min count1 count2)
          el-schema (sequence-schema-element-schema schema)
          triples-common (mapcat (fn [idx el1 el2]
                                   (map (fn [[path v1 v2]]
                                          [(vec (cons idx path)) v1 v2])
                                        (diff-configuration-objects el-schema el1 el2)))
                                 (range)
                                 (take common-count config-object-1)
                                 (take common-count config-object-2))
          triples-1 (if (> count1 count2)
                      (map (fn [idx v]
                             [[(+ idx count2)] v nil])
                           (range) (drop common-count config-object-1))
                      [])
          triples-2 (if (> count2 count1)
                      (map (fn [idx v]
                             [[(+ idx count1)] nil v])
                           (range) (drop common-count config-object-2))
                      [])]
      (concat triples-common triples-1 triples-2))))


(defn diff-configurations
  "Returns sequence of triples [path-vectors version-1 version-2] of settings that differ."
  [schema config-1 config-2]
  (diff-configuration-objects schema
                              (configuration-object config-1)
                              (configuration-object config-2)))

(defn- sequence-schema-index?
  [thing]
  (and (number? thing) (>= thing 0)))

(defn- section-key-or-index
  [thing]
  (if (sequence-schema-index? thing)
    thing
    (section-key thing)))

(defn- access-section
  "Access the settings of a section.
  - `sections` is a list of sections and indices into sequence schemas
  - `on-last` is a function to be applied the config object at the end of the path"
  [config sections on-last]
  (letfn [(recurse [sections cf path]
            (if (empty? sections)
              (on-last cf)
              (let [sec (first sections)
                    sec-key (section-key-or-index sec)
                    sec-cf (get cf sec-key ::section-not-found)]
                (case sec-cf
                  ::section-not-found
                  (c/assertion-violation `access-section
                                         (str (if (sequence-schema-index? sec-key)
                                                "index " "section ")
                                              sec-key " not found at path " path  ": " cf)
                                         sec path config)
                  (if (sequence-schema-index? sec-key)
                    (recurse (rest sections) sec-cf (conj path sec-key))
                  (letfn [(schemarec [sections schema cf path]
                            (cond
                              (map-schema? schema)
                              (recurse sections cf path)

                              (sequence-schema? schema)
                              (if (sequence-schema-index? (first sections))
                                (recurse sections cf path)
                                (map-indexed
                                  (fn [idx subcf]
                                    (schemarec sections (sequence-schema-element-schema schema)
                                               subcf (conj path idx)))
                                  cf))))]
                    (schemarec (rest sections) (section-schema sec) sec-cf (conj path sec-key))))))))]
    (recurse sections (configuration-object config) [])))

(defn- setting-or-section-key
  [sos]
  (cond
    (setting? sos) (setting-key sos)
    (section? sos) (section-key sos)))

(defn- setting-or-section-key-or-index
  [thing]
  (if (sequence-schema-index? thing)
    thing
    (setting-or-section-key thing)))

(defn access
  "Access the value of a setting or map of a section.

  Note that the setting comes first, followed by the access path.

  - `setting-or-section` is either a setting, section or an index into a sequence schema.
  - `sections` is a list of sections and indices into sequence schemas"
  [config setting-or-section & sections]
  (access-section config sections
                  (fn [cf]
                    (let [key (setting-or-section-key-or-index setting-or-section)
                          val (get cf key ::setting-not-found)]
                      (if (= val ::setting-not-found)
                        (let [path (map section-key-or-index sections)]
                          (c/assertion-violation `access
                                                 (str (if (sequence-schema-index? key)
                                                        "index " "setting ") key
                                                      " not found at path " (vec path) ": " cf)
                                                 path setting-or-section config))
                        val)))))

(defn section-subconfig
  "Extract a section from a config as a config."
  [config & sections]
  (access-section config sections
                  (fn [cf]
                    (really-make-configuration cf
                                               (section-schema (last sections))))))

(defn sections->lens
  [sections]
  (letfn [(recurse [sections lens]
            (if (empty? sections)
              lens
              (let [section-or-index (first sections)]
                (cond
                  (sequence-schema-index? section-or-index)
                  (recurse (rest sections) (lens/>> lens (lens/at-index section-or-index)))

                  (setting? section-or-index)
                  (recurse (rest sections) (lens/>> lens (setting-key section-or-index)))

                  :else
                  (let [schema (section-schema section-or-index)]
                    (letfn [(schemarec [schema sections lens]
                              (cond
                                (map-schema? schema)
                                (recurse sections lens)

                                (sequence-schema? schema)
                                (if (sequence-schema-index? (first sections))
                                  (recurse sections lens)
                                  (lens/>> lens (lens/mapl (schemarec (sequence-schema-element-schema schema) sections lens/id))))))]
                      (schemarec schema (rest sections) (lens/>> lens (section-key section-or-index)))))))))]
    (recurse sections lens/id)))

(defn access-lens
  "A lens focussing on the value of a setting or map of a section.
  Both the yanker and the shover use [[access]] to check the validity of the
  structure of the given configuration object and to signal errors otherwise.
  Additionally, the shover completes the given value according to the range or
  schema.

  Note that the setting comes first, followed by the access path.

  - `setting-or-section` is either a setting, section or an index into a sequence schema.
  - `sections` is a list of sections and indices into sequence schemas"
  [setting-or-section & sections]
  (let [lens (lens/>> configuration-object (sections->lens (conj (vec sections) setting-or-section)))]
    (lens/lens (fn access-yanker [config]
                 (apply access config setting-or-section sections)
                 (lens config))
               (fn access-shover [config v]
                 (apply access config setting-or-section sections)
                 (let [changed-config (lens config v)]
                   (make-configuration (configuration-schema changed-config) [] (configuration-object changed-config)))))))

(defn section-subconfig-lens
  [& sections]
  (assert (last sections) (section? (last sections)))
  (lens/>> (apply access-lens sections)
           (lens/xmap #(make-configuration (section-schema (last sections)) [] %)
                      configuration-object)))

(defn sequence-schema-subconfig-lens
  [& sections]
  (assert (and (last sections) (section? (last sections)) (sequence-schema? (section-schema (last sections)))))
  (lens/>> (apply access-lens sections)
           (lens/mapl
            (lens/xmap #(make-configuration (sequence-schema-element-schema (section-schema (last sections))) [] %)
                       configuration-object))))
