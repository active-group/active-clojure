(ns active.clojure.pattern
  (:require #?(:clj  [active.clojure.record :refer [define-record-type]]
               :cljs [active.clojure.cljs.record :refer-macros [define-record-type]])
            [active.clojure.condition :as c]
            [active.clojure.lens :as lens]

            [clojure.core.match :as match]
            [clojure.spec.alpha :as s]))

(define-record-type Pattern
  (make-pattern name clauses) pattern?
  [name pattern-name
   clauses pattern-clauses])

(defn pattern
  "Takes a name and some `clauses` and returns a [[Pattern]]."
  [name & clauses]
  (make-pattern name clauses))

;;    We differentiate between three kinds of matchers:
;; 1. Constant: Match on exactly one value. The value must be equatable.
;; 2. Regex: Match on a regex. The value must be a String.
;; 3. Existence: Match on the existence. That is, not nil.
;; 4. Options: Match on any of a selection of values.

(define-record-type
  ^{:doc "A matcher that matches on exactly one value."}
  ConstantMatcher
  (make-constant-matcher value)
  constant-matcher?
  [value constant-matcher-value])

(defn match-const
  [value]
  (make-constant-matcher value))

(define-record-type
  ^{:doc "A matcher that matches against a regex."}
  RegexMatcher
  (make-regex-matcher regex)
  regex-matcher?
  [regex regex-matcher-regex])

(defn match-regex
  [regex]
  (make-regex-matcher regex))

(define-record-type
  ^{:doc "A matcher that matches on the existence of a value. The value must be anything but `nil`."}
  ExistenceMatcher
  (make-existence-matcher)
  existence-matcher?
  [])

(def the-existence-matcher "Singleton matcher. Match the existence of a vale."
  (make-existence-matcher))

(define-record-type
  ^{:doc "A matcher that matches on one of several options."}
  OptionsMatcher
  (make-options-matcher options)
  options-matcher?
  ;; TODO Perhaps this should allow for matchers recursively?
  [options options-matcher-options])

(def matcher? (some-fn constant-matcher? regex-matcher? existence-matcher? options-matcher?))

(defn matcher->value
  "Takes a `matcher` and returns the value/s it matches on.
  `::not-nil` symbolizes the existence matcher."
  [matcher]
  (cond
    (constant-matcher? matcher)
    (constant-matcher-value matcher)

    (regex-matcher? matcher)
    (regex-matcher-regex matcher)

    (options-matcher? matcher)
    (options-matcher-options matcher)

    (existence-matcher? matcher)  ; matches on everything.
    ::not-nil))

;;    There are four kinds of clauses
;; 1. Match a key in a map to some specific value.
;; 2. Match the value at a path in a map to a specific value.
;; 3. Assert the existence of a value in a map for a key.
;; 4. Assert the existence of a value in a map for a path.
;; 5. An optional clause that contains a regular clause and makes the match optional.

;; 1
(define-record-type
  ^{:doc "A clause that matches the value of `key` of a map using `matcher`. When evaluated, binds it's result to `binding`."}
  KeyMatchesClause
  (make-key-matches-clause key matcher binding)
  key-matches-clause?
  [key key-matches-clause-key
   matcher key-matches-clause-matcher
   binding key-matches-clause-binding])

(def key->sym (comp symbol name))

(defn key-matches-clause
  "Returns a clause that matches a `key` with a certain `matcher`, binding the
  match to a symbol based on `key`."
  [key matcher]
  {:pre [(matcher? matcher)]}
  (make-key-matches-clause key matcher (key->sym key)))

;; 2.
(define-record-type
  ^{:doc "A clause that matches the value of a map at the `path` using a `matcher`. When evaluated, binds it's result to `binding`."}
  PathMatchesClause
  (make-path-matches-clause path matcher binding)
  path-matches-clause?
  [path path-matches-clause-path
   matcher path-matches-clause-matcher
   binding path-matches-clause-binding])

(def path? "Is something a valid path?" (some-fn list? vector?))

(defn path-matches-clause
  [path matcher]
  {:pre [(and (path? path) (matcher? matcher))]}
  (make-path-matches-clause path matcher ((comp key->sym last) path)))

;; 3.
(define-record-type KeyExistsClause
  ^{:doc "A clause that asserts the existence of a non-nil value in a map at the `key`. When evaluated, binds it's result to `binding`."}
  (make-key-exists-clause key matcher binding)
  key-exists-clause?
  [key key-exists-clause-key
   matcher key-exists-clause-matcher
   binding key-exists-clause-binding])

(defn key-exists-clause
  "Returns a clause that asserts the existence of a non-nil value at `key`.
  Binds the value associated with `key` to `(key->sym key)`."
  [key]
  (make-key-exists-clause key the-existence-matcher (key->sym key)))

;; 4.
(define-record-type
  ^{:doc "A clause that asserts the existence of a non-nil value in a map at the `path`. When evaluated, binds it's result to `binding`."}
  PathExistsClause
  (make-path-exists-clause path matcher binding)
  path-exists-clause?
  [path path-exists-clause-path
   matcher path-exists-clause-matcher
   binding path-exists-clause-binding])

(defn path-exists-clause
  "Returns a clause that asserts the existence of a non-nil value at `key`.
  Binds the value associated with `path` to `(key->sym (last path))`."
  [path]
  (make-path-exists-clause path the-existence-matcher ((comp key->sym last) path)))

;; 5.
(define-record-type
  ^{:doc "Represents an optional clause. Contains the original clause."}
  OptionalClause
  (make-optional-clause clause)
  optional-clause?
  [clause optional-clause-clause])

(defn optional
  "Takes a `clause` and returns it as an optional clause. If it already was an
  optional clause, this is the identity function."
  [clause]
  (if (optional-clause? clause)
    clause
    (make-optional-clause clause)))

(def clause? (some-fn key-matches-clause? path-matches-clause?
                      key-exists-clause?  path-exists-clause?
                      optional-clause?))

;; helpers
(defn clause-lens
  [key-matches-lens path-matches-lens key-exists-lens path-exists-lens]
  (fn [clause]
    (cond
      (key-matches-clause? clause)  key-matches-lens
      (path-matches-clause? clause) path-matches-lens
      (key-exists-clause? clause)   key-exists-lens
      (path-exists-clause? clause)  path-exists-lens
      :else
      (c/assertion-violation `clause-lens "not a valid clause" clause))))

(def binding-lens
  "Returns a function that when applied to a clause, returns a lens focusing on
  the binding of the clause."
  (clause-lens key-matches-clause-binding
               path-matches-clause-binding
               key-exists-clause-binding
               path-exists-clause-binding))

(def matcher-lens
  "Returns a function that when applied to a clause, returns a lens focusing on
  the matcher of the clause."
  (clause-lens key-matches-clause-matcher
               path-matches-clause-matcher
               key-exists-clause-matcher
               path-exists-clause-matcher))

(def path-lens
  "Returns a function that when applied to a clause, returns a lens focusing on
  the matcher of the clause."
  (clause-lens key-matches-clause-key
               path-matches-clause-path
               key-exists-clause-key
               path-exists-clause-path))

(defn bind-match
  "Takes a clause and replaces it's binding with `binding`."
  [clause binding]
  {:pre [(and (clause? clause) (symbol? binding))]}
  (lens/shove clause (binding-lens clause) binding))

;;;; Parse
;; Translate pattern expressions for `active.clojure.match` to clauses

(s/def ::key (s/or :keyword keyword? :symbol symbol? :string string?))
(s/def ::path (s/coll-of ::key :kind vector?))

(defn regex?
  "Is a `thing` a regex."
  [thing]
  #?(:clj  (instance? java.util.regex.Pattern thing)
     :cljs (instance? js/RegExp thing)))

(defn any-but
  [& exclusions]
  (complement (apply some-fn exclusions)))

(s/def ::match-value (s/or :regex regex?
                           :any (any-but regex?)))

(s/def ::binding-key #{:as})
(s/def ::binding symbol?)

(s/def ::key-exists ::key)
(s/def ::key-exists-with-binding
  (s/cat :key ::key :binding-key ::binding-key :binding ::binding))

(s/def ::path-exists ::path)
(s/def ::path-exists-with-binding
  (s/cat :path ::path :binding-key ::binding-key :binding ::binding))

(s/def ::key-matches (s/cat :key ::key :match-value ::match-value))
(s/def ::key-matches-with-binding
  (s/cat :key ::key :match-value ::match-value :binding-key ::binding-key :binding ::binding))

(s/def ::path-matches (s/cat :path ::path :match-value ::match-value))
(s/def ::path-matches-with-binding
  (s/cat :path ::path :match-value ::match-value :binding-key ::binding-key :binding ::binding))

(s/def ::clause
  (s/or :key-exists ::key-exists
        :key-exists-with-binding ::key-exists-with-binding
        :path-exists ::path-exists
        :path-exists-with-binding ::path-exists-with-binding
        :key-matches ::key-matches
        :key-matches-with-binding ::key-matches-with-binding
        :path-matches ::path-matches
        :path-matches-with-binding ::path-matches-with-binding))

(defn match-value->matcher
  [[kind match-value]]
  (if (= :regex kind)
    (match-regex match-value)
    (match-const match-value)))

(defn parse-clause
  [p]
  (let [parse (s/conform ::clause p)]
    (if (s/invalid? parse)
      (c/assertion-violation `match-pattern->clause "not a valid pattern" p (s/explain-str ::clause p))
      (case (first parse)
        :key-exists
        (let [[_ [_ k]] parse]
          (key-exists-clause k))

        :key-exists-with-binding
        (let [[_ {:keys [key binding]}] parse
              [_ k]                     key]
          (bind-match (key-exists-clause k) binding))

        :path-exists
        (let [[_ path] parse
              components (mapv second path)]
          (path-exists-clause components))

        :path-exists-with-binding
        (let [[_ {:keys [path binding]}] parse
              components (mapv second path)]
          (bind-match (path-exists-clause components) binding))

        :key-matches
        (let [[_ {:keys [key match-value]}] parse
              [_ k] key]
          (key-matches-clause k (match-value->matcher match-value)))

        :key-matches-with-binding
        (let [[_ {:keys [key match-value binding]}] parse
              [_ k] key]
          (bind-match (key-matches-clause k (match-value->matcher match-value)) binding))

        :path-matches
        (let [[_ {:keys [path match-value]}] parse
              components (mapv second path)]
          (path-matches-clause components (match-value->matcher  match-value)))

        :path-matches-with-binding
        (let [[_ {:keys [path match-value binding]}] parse
              components (mapv second path)]
          (bind-match (path-matches-clause components (match-value->matcher match-value)) binding))))))

(defn parse-pattern
  "Parse the argument to `defpattern` as a [[Pattern]]"
  [name p]
  (make-pattern name (mapv parse-clause p)))

;; Match

(defn convert-path-element
  [path-element]
  (if (symbol? path-element) (str path-element) path-element))

(defn key-exists-clause->match
  [clause]
  (let [key     (key-exists-clause-key clause)
        binding (key-exists-clause-binding clause)]
    ;; ignore the binding if it is the same as the key
    {(convert-path-element key) binding}))

(defn path-exists-clause->match
  [clause]
  (let [path    (path-exists-clause-path clause)
        binding (path-exists-clause-binding clause)]
    (assoc-in {} (map convert-path-element path) binding)))

(defn key-matches-clause->lhs-match
  [clause]
  (let [key         (key-matches-clause-key clause)
        match-value (matcher->value (key-matches-clause-matcher clause))]
    {key match-value}))

(defn key-matches-clause->rhs-match
  [message clause rhs]
  (let [key         (key-matches-clause-key clause)
        match-value (matcher->value (key-matches-clause-matcher clause))
        binding     (key-matches-clause-binding clause)]
    `[~binding (get-in ~message [~(convert-path-element key)] ~match-value)]))

(defn path-matches-clause->lhs-match
  [clause]
  (let [path        (path-matches-clause-path clause)
        match-value (matcher->value (path-matches-clause-matcher clause))]
    (assoc-in {} (map convert-path-element path) match-value)))

(defn path-matches-clause->rhs-match
  [message clause rhs]
  (let [path         (path-matches-clause-path clause)
        match-value (matcher->value (path-matches-clause-matcher clause))
        binding     (path-matches-clause-binding clause)]
    `[~binding (get-in ~message ~(map convert-path-element path) ~match-value)]))

(defn deep-merge-with
  "Like merge-with, but merges maps recursively, applying the given fn
  only when there's a non-map at a particular level.
  (deep-merge-with + {:a {:b {:c 1 :d {:x 1 :y 2}} :e 3} :f 4}
                     {:a {:b {:c 2 :d {:z 9} :z 3} :e 100}})
  -> {:a {:b {:z 3, :c 3, :d {:z 9, :x 1, :y 2}}, :e 103}, :f 4}"
  [f & maps]
  (apply
   (fn m [& maps]
     (if (every? map? maps)
       (apply merge-with m maps)
       (apply f maps)))
   maps))

(defn pattern->lhs
  [pattern]
  (let [clauses (pattern-clauses pattern)]
    (apply deep-merge-with merge
           (mapv (fn [clause]
                   (cond
                     (key-exists-clause? clause)   (key-exists-clause->match clause)
                     (path-exists-clause? clause)  (path-exists-clause->match clause)
                     (key-matches-clause? clause)  (key-matches-clause->lhs-match clause)
                     (path-matches-clause? clause) (path-matches-clause->lhs-match clause)))
                 clauses))))

(defn pattern->rhs
  [message pattern rhs]
  (let [clauses (pattern-clauses pattern)
        bindings
        (reduce (fn [bindings clause]
                  (cond
                    (or (key-exists-clause? clause)
                        (path-exists-clause? clause))
                    bindings

                    (key-matches-clause? clause)
                    (conj bindings (key-matches-clause->rhs-match message clause rhs))

                    (path-matches-clause? clause)
                    (conj bindings (path-matches-clause->rhs-match message clause rhs))))
                []
                clauses)]
    `(let ~(into [] (apply concat bindings))
       ~rhs)))

#?(:clj
   (defmacro map-matcher
     [& args]
     (let [message              `message#
           patterns+consequents (reduce
                                 (fn [code [lhs* rhs]]
                                   (let [lhs (if (symbol? lhs*) (eval lhs*) lhs*)]
                                     (let [pattern (parse-pattern "pattern" lhs)]
                                       (concat code
                                               [(pattern->lhs pattern)
                                                (pattern->rhs message pattern rhs)]))))
                                 nil
                                 (partition 2 args))]

       `(fn [~message]
          (match/match ~message ~@patterns+consequents)))))
