(ns active.clojure.match
  "Syntactic sugar for map matching around `core.match`."
  (:require [active.clojure.condition :as c]
            [active.clojure.functions :as f]
            [active.clojure.lens :as lens]
            [active.clojure.record :refer [define-record-type]]

            [clojure.spec.alpha :as s]
            [clojure.core.match :as match]
            [clojure.core.match.regex]))

(defmethod match/to-source ::match/regex
  [pat ocr]
  `(and (not= ~ocr ::match/not-found) (re-matches ~(:regex pat) ~ocr)))

(define-record-type Pattern
  (make-pattern name clauses) pattern?
  [name pattern-name
   clauses pattern-clauses])

(defn pattern
  "Takes a name and some `clauses` and returns a [[Pattern]]."
  [& clauses]
  (make-pattern (str (gensym "pattern-")) clauses))

(defn named-pattern
  [pattern the-name]
  (lens/shove pattern pattern-name the-name))

;;    We differentiate between three kinds of matchers:
;; 1. Constant: Match on exactly one value. The value must be equatable.
;; 2. Regex: Match on a regex. The value must be a String.
;; 3. Existence: Match on the existence. That is, not nil.
;; 4. Options: Match on any of a selection of values.
;; 5. Predicate: A function from a to bool.

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

(defn match-options
  [options]
  (make-options-matcher options))

(define-record-type
  ^{:doc "A matcher that matches by applying a function (a -> Bool) to a value."}
  PredicateMatcher
  (make-predicate-matcher pred)
  predicate-matcher?
  [pred predicate-matcher-predicate])

(defn match-predicate
  [p]
  (make-predicate-matcher p))

(def matcher? (some-fn constant-matcher? regex-matcher? existence-matcher? options-matcher? predicate-matcher?))

(defn matcher-default-value
  "Returns the default value of a matcher, if any."
  [matcher]
  (cond
    (constant-matcher? matcher)
    (constant-matcher-value matcher)

    (regex-matcher? matcher)
    (regex-matcher-regex matcher)

    (or (options-matcher? matcher) (existence-matcher? matcher) (predicate-matcher? matcher))
    nil))

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
  [key matcher bind]
  {:pre [(matcher? matcher)]}
  (make-key-matches-clause key matcher bind))

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
  [path matcher bind]
  {:pre [(and (path? path) (matcher? matcher))]}
  (make-path-matches-clause path matcher bind))

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
  [key bind]
  (make-key-exists-clause key the-existence-matcher bind))

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
  [path bind]
  (make-path-exists-clause path the-existence-matcher bind))

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
      (optional-clause? clause)     (lens/>> optional-clause-clause ((clause-lens key-matches-lens path-matches-lens key-exists-lens path-exists-lens)
                                                                     (optional-clause-clause clause)))
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
  the path of the clause."
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
  (instance? java.util.regex.Pattern thing))

(defn any-but
  [& exclusions]
  (complement (apply some-fn exclusions)))


(s/def ::regex regex?)
(s/def ::compare-fn-token #{:compare-fn})
(s/def ::fn? any?)
(s/def ::compare-fn (s/cat :compare-fn ::compare-fn-token :fn ::fn?))

(s/def ::or-token #{:or})

(s/def ::options (s/cat :or ::or-token :options (s/* any?)))

(s/def ::match-value (s/or :regex regex?
                           :options ::options
                           :compare-fn ::compare-fn
                           :any (any-but regex?)))

(s/def ::binding-key #{:as})
(s/def ::binding symbol?)

(s/def ::qmark #{'?})

(s/def ::key-exists
  (s/or :required (s/or :flat ::key
                        :list (s/cat :key ::key))
        :optional (s/cat :qmark ::qmark :key ::key)))

(s/def ::key-exists-with-binding
  (s/or :required (s/cat :key ::key :binding-key ::binding-key :binding ::binding)
        :optional (s/cat :qmark ::qmark :key ::key :binding-key ::binding-key :binding ::binding)))

(s/def ::path-exists
  (s/or :required (s/or :flat ::path
                        :list (s/cat :path ::path))
        :optional (s/cat :qmark ::qmark :path ::path)))

(s/def ::path-exists-with-binding
  (s/or :required (s/cat :path ::path :binding-key ::binding-key :binding ::binding)
        :optional (s/cat :qmark ::qmark :path ::path :binding-key ::binding-key :binding ::binding)))

(s/def ::key-matches
  (s/or :required (s/cat :key ::key :match-value ::match-value)
        :optional (s/cat :qmark ::qmark :key ::key :match-value ::match-value)))

(s/def ::key-matches-with-binding
  (s/or :required (s/cat :key ::key :match-value ::match-value :binding-key ::binding-key :binding ::binding)
        :optional (s/cat :qmark ::qmark :key ::key :match-value ::match-value :binding-key ::binding-key :binding ::binding)))

(s/def ::path-matches
  (s/or :required (s/cat :path ::path :match-value ::match-value)
        :optional (s/cat :qmark ::qmark :path ::path :match-value ::match-value)))

(s/def ::path-matches-with-binding
  (s/or :required (s/cat :path ::path :match-value ::match-value :binding-key ::binding-key :binding ::binding)
        :optional (s/cat :qmark ::qmark :path ::path :match-value ::match-value :binding-key ::binding-key :binding ::binding)))

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
  (cond
    (= :regex kind)      (match-regex match-value)
    (= :options kind)    (match-options (:options match-value))
    (= :compare-fn kind) (match-predicate (:fn match-value))
    :else                (match-const match-value)))

(defn make-key
  "Returns k as a string if k is a symbol, otherwise returns k."
  [k #_[kind k]]
  (let [[kind k] (if (map? k)
                   (:key k)
                   k)]
    (cond
      (= :symbol kind) (str k)
      (= :keyword kind) k
      :else k)))

(defn make-binding
  "Returns k as a string if k is a symbol, otherwise returns k."
  [k]
  (if-not (vector? k)
    (str k)
    (let [[kind k] k]
      (cond
        (= :symbol kind)  (str k)
        (= :keyword kind) (str (name k))
        :else             k))))

(def flat? (partial = :flat))
(def optional? (partial = :optional))

(defmacro parse-clause
  [p]
  (let [parse (s/conform ::clause p)]
    (if (s/invalid? parse)
      (c/assertion-violation `match-pattern->clause "not a valid pattern" p (s/explain-str ::clause p))

      (let [[match parse] parse
            [mode body]   parse]
        (case match
          :key-exists
          (if (optional? mode)
            (let [k (make-key (:key body))
                  b (make-binding (:key body))]
              `(make-optional-clause (key-exists-clause ~k ~b)))
            (let [[mode body] body
                  mflat?      (flat? mode)
                  k           (make-key (if mflat? body (:key body)))
                  b           (make-binding (if mflat? body (:key body)))]
              (if (flat? mode)
                `(key-exists-clause ~k ~b)
                `(key-exists-clause ~k ~b))))

          :key-exists-with-binding
          (let [k (make-key (:key body))
                b (make-binding (:binding body))]
            (if (optional? mode)
              `(make-optional-clause (key-exists-clause ~k ~b))
              `(key-exists-clause ~k ~b)))

          :path-exists
          (if (optional? mode)
            (let [path (mapv make-key (:path body))
                  b    (make-binding (last (:path body)))]
              `(make-optional-clause (path-exists-clause ~path ~b)))
            (let [[mode body] body
                  mflat?      (flat? mode)
                  path        (mapv make-key (if mflat? body (:path body)))
                  b           (make-binding (if mflat? (last body) (last (:path body))))]
              (if (optional? mode)
                `(make-optional-clause (path-exists-clause ~path ~b))
                `(path-exists-clause ~path ~b))))

          :path-exists-with-binding
          (let [path (mapv make-key (:path body))
                b    (make-binding (:binding body))]
            (if (optional? mode)
              `(make-optional-clause (path-exists-clause ~path ~b))
              `(path-exists-clause ~path ~b)))

          :key-matches
          (let [k           (make-key (:key body))
                b           (make-binding (:key body))
                match-value (match-value->matcher (:match-value body))]
            (if (optional? mode)
              `(make-optional-clause (key-matches-clause ~k (match-value->matcher ~(:match-value body)) ~b))
              `(key-matches-clause ~k (match-value->matcher ~(:match-value body)) ~b)))

          :key-matches-with-binding
          (let [k           (make-key (:key body))
                b           (make-binding (:binding body))
                match-value (match-value->matcher (:match-value body))]
            (if (optional? mode)
              `(make-optional-clause (key-matches-clause ~k (match-value->matcher ~(:match-value body)) ~b))
              `(key-matches-clause ~k (match-value->matcher ~(:match-value body)) ~b)))

          :path-matches
          (let [path        (mapv make-key (:path body))
                b           (make-binding (last (:path body)))
                match-value (match-value->matcher (:match-value body))]
            (if (optional? mode)
              `(make-optional-clause (path-matches-clause ~path (match-value->matcher ~(:match-value body)) ~b))
              `(path-matches-clause ~path (match-value->matcher ~(:match-value body)) ~b)))

          :path-matches-with-binding
          (let [path        (mapv make-key (:path body))
                b           (make-binding (:binding body))
                match-value (match-value->matcher (:match-value body))]
            (if (optional? mode)
              `(make-optional-clause (path-matches-clause ~path (match-value->matcher ~(:match-value body)) ~b))
              `(path-matches-clause ~path (match-value->matcher ~(:match-value body)) ~b))))))))

(defmacro parse-clauses
  [cs]
  (when-not (empty? cs)
    `(let [clause# (parse-clause ~(first cs))]
       (cons clause# (parse-clauses ~(rest cs))))))

(defn convert-path-element
  [path-element]
  (if (symbol? path-element) (str path-element) path-element))

(defn fold-path
  [path match]
  (let [path* (->> path
                   (mapv convert-path-element)
                   reverse)]
    (reduce (fn [m path-element]
              {path-element m})
            {(first path*) match}
            (rest path*))))

;; syntax emitter
(defn parse-emit-syntax
  [message p rhs]
  (let [parse (s/conform ::clause p)]
    (if (s/invalid? parse)
      (c/assertion-violation `match-pattern->clause "not a valid pattern" p (s/explain-str ::clause p))

      (let [[match parse] parse
            [mode body]   parse]
        (case match
          :key-exists
          (if (optional? mode)
            (let [k (make-key (:key body))
                  b (make-binding (:key body))]
              [{}
               `[~(symbol b) (get-in ~message [~k])]])
            (let [[mode body] body
                  mflat?      (flat? mode)
                  k           (make-key (if mflat? body (:key body)))
                  b           (make-binding (if mflat? body (:key body)))]
              [`{~k ~(symbol b)}
               `[~(symbol b) (get-in ~message [~k])]]))

          :key-exists-with-binding
          (if (optional? mode)
            (let [k (make-key (:key body))
                  b (make-binding (:binding body))]
              [{}
               `[~(symbol b) (get-in ~message [~k])]])
            (let [k (make-key (:key body))
                  b (make-binding (:binding body))]
              [`{~k ~(symbol b)}
               `[~(symbol b) (get-in ~message [~k])]]))

          :path-exists
          (if (optional? mode)
            (let [path (mapv make-key (:path body))
                  b    (make-binding (last (:path body)))]
              [{}
               `[~(symbol b) (get-in ~message ~path)]])
            (let [[mode body] body
                  mflat?      (flat? mode)
                  path        (mapv make-key (if mflat? body (:path body)))
                  b           (make-binding (if mflat? (last body) (last (:path body))))
                  path-map    (assoc-in {} path (symbol b))]
              [`~path-map
               `[~(symbol b) (get-in ~message ~path)]]))

          :path-exists-with-binding
          (let [path     (mapv make-key (:path body))
                b        (make-binding (:binding body))
                path-map (assoc-in {} path (symbol b))]
            [`~(if (optional? mode)
                 {} path-map)
             `[~(symbol b) (get-in ~message ~path)]])

          :key-matches
          (let [k           (make-key (:key body))
                b           (make-binding (:key body))
                match       (:match-value body)
                predicate?  (= :compare-fn (first match))
                match-value (second match)]
            (cond
              (optional? mode)
              [{}
               `[~(symbol b) (get-in ~message [~k] ~match-value)]]
              predicate?
              [`({~k ~'_} :guard [(constantly (~(:fn match-value) (get-in ~message [~k])))])
               `[~(symbol b) (get-in ~message [~k])]]
              :else
              [`{~k ~match-value}
               `[~(symbol b) (get-in ~message [~k])]]))

          :key-matches-with-binding
          (let [k           (make-key (:key body))
                b           (make-binding (:binding body))
                match       (:match-value body)
                predicate?  (= :compare-fn (first match))
                match-value (second match)]
            (cond
              (optional? mode)
              [{}
               `[~(symbol b) (get-in ~message [~k] ~match-value)]]
              predicate?
              [`({~k ~'_} :guard [(constantly (~(:fn match-value) (get-in ~message [~k])))])
               `[~(symbol b) (get-in ~message [~k])]]
              :else
              [`{~k ~match-value}
               `[~(symbol b) (get-in ~message [~k])]]))

          :path-matches
          (let [path        (mapv make-key (:path body))
                b           (make-binding (last (:path body)))
                match       (:match-value body)
                predicate?  (= :compare-fn (first match))
                match-value (second match)
                path-map    (assoc-in {} path match-value)]
            (cond
              (optional? mode)
              [{}
               `[~(symbol b) (get-in ~message ~path ~match-value)]]
              predicate?
              [`(~(fold-path path '_) :guard [(constantly (~(:fn match-value) (get-in ~message ~path)))])
               `[~(symbol b) (get-in ~message ~path)]]
              :else
              [`~path-map
               `[~(symbol b) (get-in ~message ~path)]]))

          :path-matches-with-binding
          (let [path        (mapv make-key (:path body))
                b           (make-binding (:binding body))
                match       (:match-value body)
                predicate?  (= :compare-fn (first match))
                match-value (second match)
                path-map    (assoc-in {} path match-value)]
            (cond
              (optional? mode)
              [{}
               `[~(symbol b) (get-in ~message ~path ~match-value)]]
              predicate?
              [`(~(fold-path '_) :guard [(constantly (~(:fn match-value) (get-in ~message ~path)))])
               `[~(symbol b) (get-in ~message ~path)]]
              :else
              [`~path-map
               `[~(symbol b) (get-in ~message ~path)]])))))))

(defn deep-merge [v & vs]
  (letfn [(rec-merge [v1 v2]
            (if (and (map? v1) (map? v2))
              (merge-with deep-merge v1 v2)
              v2))]
    (if (some identity vs)
      (reduce #(rec-merge %1 %2) v vs)
      v)))

(defn parse-emit-match-syntax
  [message [pattern rhs]]
  (let [[lhss rhss] (reduce (fn [[clauses bindings] c]
                           (let [[clause binding] (parse-emit-syntax message c rhs)]
                             [(conj clauses clause)
                              (conj bindings binding)]))
                         [[] []]
                         pattern)]
    [(apply deep-merge lhss)
     `(let ~(into [] (apply concat rhss))
        ~rhs)]))

(defmacro parse-pattern
  "Parse the argument to `defpattern` as a [[Pattern]].
  Optionally accepts a `name` (String) that names the pattern. If none is
  provided, automatically assigns a name."
  [pattern]
  (if (vector? pattern)
    `(make-pattern ~(str (gensym "pattern-")) (parse-clauses ~pattern))
    pattern))

;; Match

(defn key-exists-clause->rhs-match
  [message clause]
  (let [key     (key-exists-clause-key clause)
        binding (key-exists-clause-binding clause)]
    `[~(symbol binding) (get-in ~message [~(convert-path-element key)])]))

(defn path-exists-clause->rhs-match
  [message clause]
  (let [key         (path-exists-clause-path clause)
        binding     (path-exists-clause-binding clause)]
    `[~(symbol binding) (get-in ~message ~(mapv convert-path-element key))]))

(defn key-matches-clause->rhs-match
  [message clause]
  (let [key         (key-matches-clause-key clause)
        match-value (matcher-default-value (key-matches-clause-matcher clause))
        binding     (key-matches-clause-binding clause)]
    `[~(symbol binding) (get-in ~message [~(convert-path-element key)] ~match-value)]))

(defn path-matches-clause->rhs-match
  [message clause]
  (let [path        (path-matches-clause-path clause)
        match-value (matcher-default-value (path-matches-clause-matcher clause))
        binding     (path-matches-clause-binding clause)]
    `[~(symbol binding) (get-in ~message ~(mapv convert-path-element path) ~match-value)]))

(defn matcher->value
  "Takes a `matcher` and returns the value/s it matches on.
  `::not-nil` symbolizes the existence matcher."
  [matcher]
  (fn [message path]
    (cond
      (constant-matcher? matcher)
      (constant-matcher-value matcher)

      (regex-matcher? matcher)
      (regex-matcher-regex matcher)

      (options-matcher? matcher)
      (cons :or (options-matcher-options matcher))

      (existence-matcher? matcher)  ; matches on everything.
      ::not-nil

      (predicate-matcher? matcher)
      [`(constantly (~(predicate-matcher-predicate matcher)
                     (get-in ~message ~path)))]

      :else
      (c/assertion-violation `matcher->value "not a matcher" matcher))))

(defn clause->lhs
  [message clause]
  (cond
    (key-exists-clause? clause)
    (let [key     (key-exists-clause-key clause)
          binding (key-exists-clause-binding clause)]
      ;; ignore the binding if it is the same as the key
      {(convert-path-element key) (symbol binding)})

    (path-exists-clause? clause)
    (let [path    (path-exists-clause-path clause)
          binding (path-exists-clause-binding clause)]
      (assoc-in {} (map convert-path-element path) (symbol binding)))

    (key-matches-clause? clause)
    (let [key         (key-matches-clause-key clause)
          matcher     (key-matches-clause-matcher clause)
          match-value (matcher->value matcher)]
      (if (predicate-matcher? matcher)
        `({~key ~'_} :guard ~(match-value message [key]))
        `{~key ~(match-value message [key])}))

    (path-matches-clause? clause)
    (let [path        (path-matches-clause-path clause)
          matcher     (path-matches-clause-matcher clause)
          match-value (matcher->value matcher)]
      (if (predicate-matcher? matcher)
        `(~(fold-path path '_) :guard ~(match-value message path))
        (fold-path path (match-value message path))))

    (optional-clause? clause)
    {}))


(defn reduce-lhs
  [lhss]
  (reduce (fn [acc lhs]
            (cond
              (and (map? acc) (map? lhs))
              (deep-merge acc lhs)

              (and (sequential? acc) (map? lhs))
              (let [[acc-map & tail] acc]
                (cons (deep-merge acc-map lhs) tail))

              (and (map? acc) (sequential? lhs))
              (let [[lhs-map & tail] lhs]
                (cons (deep-merge acc lhs-map) tail))

              (and (sequential? acc) (sequential? lhs))
              (let [[acc-map guard-key acc-tail] acc
                    [lhs-map guard-key lhs-tail] lhs]
                (list (deep-merge acc-map lhs-map) :guard (concat acc-tail lhs-tail)))

              :else
              (c/assertion-violation `reduce-lhs "not a valid lhs pattern:" lhs)))
          {}
          lhss))


(defn pattern->lhs
  [message pattern]
  (->> pattern
       pattern-clauses
       (mapv (partial clause->lhs message))
       reduce-lhs))

(defn clause->rhs
  [message bindings clause]
  (cond
    (key-exists-clause? clause)
    (conj bindings (key-exists-clause->rhs-match message clause))

    (path-exists-clause? clause)
    (conj bindings (path-exists-clause->rhs-match message clause))

    (key-matches-clause? clause)
    (conj bindings (key-matches-clause->rhs-match message clause))

    (path-matches-clause? clause)
    (conj bindings (path-matches-clause->rhs-match message clause))

    (optional-clause? clause)
    (clause->rhs message bindings (optional-clause-clause clause))))

(defn pattern->rhs
  [message pattern rhs]
  (let [clauses (pattern-clauses pattern)
        bindings
        (reduce (partial clause->rhs message)
                []
                clauses)]
    `(let ~(into [] (apply concat bindings))
       ~rhs)))

(defmacro map-matcher
  "Construct a map matcher. Syntactic sugar for `core.match`.

  `map-matcher´ accepts two kinds of inputs:

  1. A sequence of alternating patterns and consequents (see below).
  2. A sequence of alternating Pattern objects and consequents.

  The syntax is `(map-matcher <pattern> <consequent> ... :else <alternative>)` where
  `<pattern>` is a vector of clauses `[<clause>+]` where `clause` is one of the following:

  - `(<key> <value> :as <name>)` which requires the key `<key>` to be
    mapped to `<value>` in the map and binds `<name>` to `<value>`.

  - `(<key-and-name> <value>)` which requires the key `<key-and-name>`
    to be mapped to `<value>` in the map and binds `<key-and-name>` to
    `<value>`.

  - `(<key> :as <name>)` which requires `<key>` to be present in the map
    and binds `<name>` to its value.

  - `<key-and-name>` which requires `<key-and-name>` to be present in
    the map and binds `<key-and-name>` to its value

  The map matcher also supports optional keys:

  - `(? <key> <default> :as <name>)` binds `<name>` to to the value of
    `<key>` in the map or to `<default>` if `<key>` is not in the map.

  - `(? <key-and-name> <default>)` binds `<key-and-name>` to the value of
    `<key-and-name>` in the map or to `<default>` if `<key-and-name>` is not
    in the map.

  - `(? <key> :as <name>)` binds `<name>` to to the value of `<key>`
    in the map or to `nil` if `<key>` is not in the map.

  - `(? <key-and-name>)` binds `<key-and-name>` to the value of
    `<key-and-name>` in the map or to `nil` if `<key-and-name>` is not
    in the map.

  Access to nested values is also possible.  Use `[<key>+]` to access
  a nested value, where `[<key>+]` is a sequence of keys.  When no
  `:as <name>` clause is given, the last `<key>` of the sequence of
  keys is used as a name to bind the value.

  `<key>` and `<key-and-name>` can be either a symbol or a keyword.
  If `<key-and-name>` is a symbol, it is converted to a string when
  used as a key (and used as symbol for binding the value).  If
  `<key-and-name>` is a keyword, it is converted to a name for binding
  the value (and usesd as keyword when used as a key).

  `<value>` can be:

  - any value, regular expressions are also possible (only in Clojure, though,
    `core.match` does not support regex matching in ClojureScript).

  - a list of alternative values in the form of: `(:or <value> <value>*)`.

  - a custom compare function in the form of:
    `(:compare-fn <compare-fn>)` where `<compare-fn>` accepts the value that
    is mapped to `<key>` or `<key-and-name>`.

  `map-matcher` returns a function that accepts a map and evaluates
  `<consequent>` with all the `<name>`s bound when the message matches
  the given `<clause>`s, otherwise it evaluates `<alternative>`. or
  throws `IllegalArgumentException` if `<clause>` matches and no
  `<alternative>` is given.

  Example:

        (def example-map-matcher
          (map-matcher
            [(:x \"x\" :as x)
             (:y \"y\")
             (:z :as z)
             :w]
            (println x y z w)
            [(:a \"a\" :as a)
             (:b \"b\")
             (:c :as c)
             ([:d Z] 42 :as Z)
             ([:d Y] :as Y)
             ([:d X] 65)
             [:d W foo]]
            (println a b c Z Y X foo)
            :else false))

        (example-map-matcher {:a \"a\" :b \"b\" :c \"c\"
                              :d {\"Z\" 42 \"Y\" 23 \"X\" 65
                                  \"W\" {\"foo\" \"bar\"}}})

    prints

     \"a b c d 42 23 65 bar\""
  [& args]
  (when-not (even? (count args))
    (throw (IllegalArgumentException. (str "expecting an even number of arguments " *ns* " " (meta &form)))))
  (let [message              `message#
        patterns+consequents (mapcat
                              (fn [[lhs* rhs]]
                                (let [lhs (if (symbol? lhs*) (eval lhs*) lhs*)]
                                  (cond
                                    (= lhs :else)
                                    [lhs rhs]

                                    (pattern? lhs)
                                    [(pattern->lhs message lhs)
                                     (pattern->rhs message lhs rhs)]

                                    :else
                                    (parse-emit-match-syntax message [lhs rhs]))))
                              (partition 2 args))]
    `(fn [~message]
       (match/match ~message ~@patterns+consequents))))

(defmacro defpattern
  [binding pattern]
  `(def ~binding (parse-pattern ~pattern)))

(defmacro matcher
  [& args]
  (let [event `event#]
    `(fn [~event]
       ((map-matcher ~@args) ~event))))

(defmacro match
  [event & args]
  `((matcher ~@args) ~event))

(define-record-type Dependency
  (make-dependency path matcher for-pattern) dependency?
  [^{:doc "The path that this [[Dependency]] has a restriction on."}
   path dependency-path
   ^{:doc "The matcher that must be successful for the `path`."}
   matcher dependency-matcher
   ^{:doc "The [[Pattern]] that depends on this."}
   for-pattern dependency-for-pattern])

(defn clause->dependency
  [pattern clause]
  (make-dependency
   (lens/yank clause (path-lens clause))
   (lens/yank clause (matcher-lens clause))
   pattern))

(defn pattern->dependencies
  [pattern]
  (mapv (partial clause->dependency pattern) (pattern-clauses pattern)))

(defn dependencies->graph
  [dependencies]
  (reduce (fn [acc dependency]
            (update acc
                    [(dependency-path dependency)
                         (dependency-matcher dependency)]
                    conj
                    (dependency-for-pattern dependency)))

          {}
          dependencies))

;;;; Compose patterns. A composition of patterns is one of the following
;; - A pattern
;; - A conjunction of two compositions
;; - A disjunction of two compositions

(define-record-type Conjunction
  (make-conjunction comp-1 comp-2) conjunction?
  [comp-1 conjunction-comp-1
   comp-2 conjunction-comp-2])

(defn conjunction
  [composition & compositions]
  (reduce make-conjunction composition compositions))

(define-record-type Disjunction
  (make-disjunction comp-1 comp-2) disjunction?
  [comp-1 disjunction-comp-1
   comp-2 disjunction-comp-2])

(defn disjuction
  [composition & compositions]
  (reduce make-disjunction composition compositions))

(defn composition->dependencies
  [composition]
  (cond
    (pattern? composition)
    (pattern->dependencies composition)

    (conjunction? composition)
    (concat (composition->dependencies (conjunction-comp-1 composition))
            (composition->dependencies (conjunction-comp-2 composition)))

    (conjunction? composition)
    (concat (composition->dependencies (disjunction-comp-1 composition))
            (composition->dependencies (disjunction-comp-2 composition)))

    :else (c/assertion-violation `composition->dependencies "not a composition" composition)))
