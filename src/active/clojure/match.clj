(ns active.clojure.match
  "Syntactic sugar for map matching around `core.match`."
  (:require [active.clojure.condition :as c]
            [active.clojure.record :refer [define-record-type]]

            [clojure.spec.alpha :as s]
            [clojure.core.match :as match]
            [clojure.core.match.regex]))

(defmethod match/to-source ::match/regex
  [pat ocr]
  `(and (not= ~ocr ::match/not-found) (re-matches ~(:regex pat) ~ocr)))

(define-record-type Pattern
  (make-pattern clauses) pattern?
  [clauses pattern-clauses])

(defn pattern
  "Takes some `clauses` and returns a [[Pattern]]."
  [& clauses]
  (make-pattern clauses))

;; We differentiate between different kinds of matchers:
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

(define-record-type
  ^{:doc "A matcher that matches on one of several options."}
  OptionsMatcher
  (make-options-matcher options)
  options-matcher?
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

(def path? "Is something a valid path?" (some-fn list? vector?))

;; There are different kinds of clauses
;; 1. Assert the existence of a value in a map for a key.
;;    - without binding - with binding
;;    - optional: with binding
;; 2. Assert the existence of a value in a map for a path.
;;    - without binding - with binding
;;    - optional: with binding
;; 3. Match a key in a map to some specific value.
;;    - without binding - with binding
;;    - optional: with binding
;; 4. Match the value at a path in a map to a specific value.
;;    - without binding - with binding
;;    - optional: with binding

;; 1.
(define-record-type KeyExistsWithoutBindingClause
  ^{:doc "A clause that asserts the existence of a non-nil value in a map at the `key`."}
  (make-key-exists-without-binding-clause key)
  key-exists-without-binding-clause?
  [key key-exists-without-binding-clause-key])

(defn key-exists-without-binding-clause
  "Returns a clause that asserts the existence of a non-nil value at `key`."
  [key]
  (make-key-exists-without-binding-clause key))

(define-record-type KeyExistsWithBindingClause
  ^{:doc "A clause that asserts the existence of a non-nil value in a map at the `key`. When evaluated, binds it's result to `binding`."}
  (make-key-exists-with-binding-clause key binding)
  key-exists-with-binding-clause?
  [key key-exists-with-binding-clause-key
   binding key-exists-with-binding-clause-binding])

(defn key-exists-with-binding-clause
  "Returns a clause that asserts the existence of a non-nil value at `key`. When evaluated, binds it's result to `binding`."
  [key binding]
  (make-key-exists-with-binding-clause key binding))

(define-record-type OptionalKeyExistsWithBindingClause
  ^{:doc "An optional clause that asserts the existence of a non-nil value in a map at the `key`. When evaluated, binds it's result to `binding`."}
  (make-optional-key-exists-with-binding-clause key binding)
  optional-key-exists-with-binding-clause?
  [key optional-key-exists-with-binding-clause-key
   binding optional-key-exists-with-binding-clause-binding])

(defn optional-key-exists-with-binding-clause
  "Returns an optional clause that asserts the existence of a non-nil value at `key`. When evaluated, binds it's result to `binding`."
  [key bind]
  (make-optional-key-exists-with-binding-clause key bind))

;; 2.
(define-record-type PathExistsWithoutBindingClause
  ^{:doc "A clause that asserts the existence of a non-nil value in a map at the `path`."}
  (make-path-exists-without-binding-clause path)
  path-exists-without-binding-clause?
  [path path-exists-without-binding-clause-path])

(defn path-exists-without-binding-clause
  "Returns a clause that asserts the existence of a non-nil value at `path`."
  [path]
  (make-path-exists-without-binding-clause path))

(define-record-type PathExistsWithBindingClause
  ^{:doc "A clause that asserts the existence of a non-nil value in a map at the `path`. When evaluated, binds it's result to `binding`."}
  (make-path-exists-with-binding-clause path binding)
  path-exists-with-binding-clause?
  [path path-exists-with-binding-clause-path
   binding path-exists-with-binding-clause-binding])

(defn path-exists-with-binding-clause
  "Returns a clause that asserts the existence of a non-nil value at `path`. When evaluated, binds it's result to `binding`."
  [path bind]
  (make-path-exists-with-binding-clause path bind))

(define-record-type OptionalPathExistsWithBindingClause
  ^{:doc "An optional clause that asserts the existence of a non-nil value in a map at the `path`. When evaluated, binds it's result to `binding`."}
  (make-optional-path-exists-with-binding-clause path binding)
  optional-path-exists-with-binding-clause?
  [path optional-path-exists-with-binding-clause-path
   binding optional-path-exists-with-binding-clause-binding])

(defn optional-path-exists-with-binding-clause
  "Returns an optional clause that asserts the existence of a non-nil value at `path`. When evaluated, binds it's result to `binding`."
  [path bind]
  (make-optional-path-exists-with-binding-clause path bind))

;; 3.
(define-record-type KeyMatchesWithoutBindingClause
  ^{:doc "A clause that matches the value of `key` of a map using `matcher`."}
  (make-key-matches-without-binding-clause key matcher)
  key-matches-without-binding-clause?
  [key key-matches-without-binding-clause-key
   matcher key-matches-without-binding-clause-matcher])

(defn key-matches-without-binding-clause
  "Returns a clause that matches a `key` with a certain `matcher`."
  [key matcher]
  {:pre [(matcher? matcher)]}
  (make-key-matches-without-binding-clause key matcher))

(define-record-type KeyMatchesWithBindingClause
  ^{:doc "A clause that matches the value of `key` of a map using `matcher`. When evaluated, binds it's result to `binding`."}
  (make-key-matches-with-binding-clause key matcher binding)
  key-matches-with-binding-clause?
  [key key-matches-with-binding-clause-key
   matcher key-matches-with-binding-clause-matcher
   binding key-matches-with-binding-clause-binding])

(defn key-matches-with-binding-clause
  "Returns a clause that matches a `key` with a certain `matcher`, binding the
  match to a symbol based on `key`."
  [key matcher bind]
  {:pre [(matcher? matcher)]}
  (make-key-matches-with-binding-clause key matcher bind))

(define-record-type OptionalKeyWithDefaultBindingClause
  ^{:doc "An optional clause with a default value for `key`. When evaluated, binds it's result to `binding`. If `key` does not exist, binds to `default-value`."}
  (make-optional-key-with-default-binding-clause key default-value binding)
  optional-key-with-default-binding-clause?
  [key optional-key-with-default-binding-clause-key
   default-value optional-key-with-default-binding-clause-default-value
   binding optional-key-with-default-binding-clause])

(defn optional-key-with-default-binding-clause
  "Returns an optional clause that matches a `key`, binding the
  match to a symbol based on `key` or the `default-value`."
  [key default-value bind]
  (make-optional-key-with-default-binding-clause key default-value bind))

;; 4.
(define-record-type PathMatchesWithoutBindingClause
  ^{:doc "A clause that matches the value of a map at the `path` using a `matcher`."}
  (make-path-matches-without-binding-clause path matcher)
  path-matches-without-binding-clause?
  [path path-matches-without-binding-clause-path
   matcher path-matches-without-binding-clause-matcher])

(defn path-matches-without-binding-clause
  "Returns a clause that matches a `path` with a certain `matcher`."
  [path matcher]
  {:pre [(and (path? path) (matcher? matcher))]}
  (make-path-matches-without-binding-clause path matcher))

(define-record-type PathMatchesWithBindingClause
  ^{:doc "A clause that matches the value of a map at the `path` using a `matcher`. When evaluated, binds it's result to `binding`."}
  (make-path-matches-with-binding-clause path matcher binding)
  path-matches-with-binding-clause?
  [path path-matches-with-binding-clause-path
   matcher path-matches-with-binding-clause-matcher
   binding path-matches-with-binding-clause-binding])

(defn path-matches-with-binding-clause
  "Returns a clause that matches a `path` with a certain `matcher`, binding the
  match to a symbol based on `path`."
  [path matcher bind]
  {:pre [(and (path? path) (matcher? matcher))]}
  (make-path-matches-with-binding-clause path matcher bind))

(define-record-type OptionalPathWithDefaultBindingClause
  ^{:doc "An optional clause with a default value at the `path`. When evaluated, binds it's result to `binding`. If `path` does not exist, binds to `default-value`."}
  (make-optional-path-with-default-binding-clause path default-value binding)
  optional-path-with-default-binding-clause?
  [path optional-path-with-default-binding-clause-path
   default-value optional-path-with-default-binding-clause-default-value
   binding optional-path-with-default-binding-clause-binding])

(defn optional-path-with-default-binding-clause
  "Returns an optional clause that matches a `path`, binding the
  match to a symbol based on `path` or the `default-value`."
  [path default-value bind]
  {:pre [(path? path)]}
  (make-optional-path-with-default-binding-clause path default-value bind))

;; Predicates for clauses
(def mandatory-clause?
  (some-fn key-exists-without-binding-clause?
           key-exists-with-binding-clause?
           path-exists-without-binding-clause?
           path-exists-with-binding-clause?
           key-matches-without-binding-clause?
           key-matches-with-binding-clause?
           path-matches-without-binding-clause?
           path-matches-with-binding-clause?))

(def optional-clause?
  (some-fn optional-key-exists-with-binding-clause?
           optional-path-exists-with-binding-clause?
           optional-key-with-default-binding-clause?
           optional-path-with-default-binding-clause?))

(def clause? (some-fn mandatory-clause? optional-clause?))

;;;; Parse
;; Translate pattern expressions for `active.clojure.match` to clauses

(def ^:private reserved-symbols
  ;; Some symbols have a special meaning and must not be matched as
  ;; plain symbols.
  #{'?})

(defn valid-symbol?
  "Is a thing a valid symbol?"
  [s]
  (and (symbol? s) (not (reserved-symbols s))))

(s/def ::qmark #{'?})
(s/def ::key (s/or :keyword keyword?
                   :symbol valid-symbol?
                   :string string?))
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

(s/def ::default-value any?)

(s/def ::binding-key #{:as})
(s/def ::binding symbol?)

;; FIXME: represent optionals as full-fledged terminal symbols in this grammar
(s/def ::key-exists-without-binding
  (s/or :required (s/or :flat ::key
                        :list (s/coll-of ::key :count 1 :kind list?))))

(s/def ::key-exists-with-binding
  (s/or :required (s/cat :key ::key :binding-key ::binding-key :binding ::binding)))

(s/def ::optional-key-exists-with-binding
  (s/or :required (s/cat :qmark ::qmark :key ::key :binding-key ::binding-key :binding ::binding)))

(s/def ::path-exists-without-binding
  (s/or :required (s/or :flat ::path
                        :list (s/cat :path ::path))))

(s/def ::path-exists-with-binding
  (s/or :required (s/cat :path ::path :binding-key ::binding-key :binding ::binding)))

(s/def ::optional-path-exists-with-binding
  (s/or :optional (s/cat :qmark ::qmark :path ::path :binding-key ::binding-key :binding ::binding)))

(s/def ::key-matches-without-binding
  (s/or :required (s/cat :key ::key :match-value ::match-value)))

(s/def ::key-matches-with-binding
  (s/or :required (s/cat :key ::key :match-value ::match-value :binding-key ::binding-key :binding ::binding)
        :optional (s/cat :qmark ::qmark :key ::key :default-value ::default-value :binding-key ::binding-key :binding ::binding)))

(s/def ::path-matches-without-binding
  (s/or :required (s/cat :path ::path :match-value ::match-value)))

(s/def ::path-matches-with-binding
  (s/or :required (s/cat :path ::path :match-value ::match-value :binding-key ::binding-key :binding ::binding)
        :optional (s/cat :qmark ::qmark :path ::path :default-value ::default-value :binding-key ::binding-key :binding ::binding)))

(s/def ::clause
  (s/or :key-exists-without-binding ::key-exists-without-binding
        :key-exists-with-binding ::key-exists-with-binding
        :path-exists-without-binding ::path-exists-without-binding
        :path-exists-with-binding ::path-exists-with-binding
        :key-matches-without-binding ::key-matches-without-binding
        :key-matches-with-binding ::key-matches-with-binding
        :path-matches-without-binding ::path-matches-without-binding
        :path-matches-with-binding ::path-matches-with-binding
        :optional-key-exists-with-binding ::optional-key-exists-with-binding
        :optional-path-exists-with-binding ::optional-path-exists-with-binding))

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
          :key-exists-without-binding
          (let [[mode body] body
                k           (make-key (if (flat? mode) body (first body)))]
            `(key-exists-without-binding-clause ~k))

          :key-exists-with-binding
          (let [k (make-key (:key body))
                b (make-binding (:binding body))]
            `(key-exists-with-binding-clause ~k ~b))

          :optional-key-exists-with-binding
          (let [k (make-key (:key body))
                b (make-binding (:binding body))]
            `(optional-key-exists-with-binding-clause ~k ~b))

          :path-exists-without-binding
          (let [[mode body] body
                path        (mapv make-key (if (flat? mode) body (:path body)))]
            `(path-exists-without-binding-clause ~path))

          :path-exists-with-binding
          (let [path (mapv make-key (:path body))
                b    (make-binding (:binding body))]
            `(path-exists-with-binding-clause ~path ~b))

          :optional-path-exists-with-binding
          (let [path (mapv make-key (:path body))
                b    (make-binding (:binding body))]
            `(optional-path-exists-with-binding-clause ~path ~b))

          :key-matches-without-binding
          (let [k (make-key (:key body))]
            `(key-matches-without-binding-clause ~k (match-value->matcher ~(:match-value body))))

          :key-matches-with-binding
          (let [k (make-key (:key body))
                b (make-binding (:binding body))]
            (if (optional? mode)
              `(optional-key-with-default-binding-clause ~k ~(:default-value body) ~b)
              `(key-matches-with-binding-clause ~k (match-value->matcher ~(:match-value body)) ~b)))

          :path-matches-without-binding
          (let [path (mapv make-key (:path body))]
            `(path-matches-without-binding-clause ~path (match-value->matcher ~(:match-value body))))

          :path-matches-with-binding
          (let [path (mapv make-key (:path body))
                b    (make-binding (:binding body))]
            (if (optional? mode)
              `(optional-path-with-default-binding-clause ~path ~(:default-value body) ~b)
              `(path-matches-with-binding-clause ~path (match-value->matcher ~(:match-value body)) ~b))))))))

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
          :key-exists-without-binding
          (let [[mode body] body
                k           (make-key (if (flat? mode) body (first body)))]
            [`{~k ~'_} `[]])

          :key-exists-with-binding
          (let [k (make-key (:key body))
                b (make-binding (:binding body))]
            [`{~k ~(symbol b)} `[~(symbol b) (get-in ~message [~k])]])

          :optional-key-exists-with-binding
          (let [k (make-key (:key body))
                b (make-binding (:binding body))]
            [{} `[~(symbol b) (get-in ~message [~k])]])

          :path-exists-without-binding
          (let [[mode body] body
                path        (mapv make-key (if (flat? mode) body (:path body)))]
            [`{~path ~'_} `[]])

          :path-exists-with-binding
          (let [path     (mapv make-key (:path body))
                b        (make-binding (:binding body))
                path-map (assoc-in {} path (symbol b))]
            [path-map `[~(symbol b) (get-in ~message ~path)]])

          :optional-path-exists-with-binding
          (let [path     (mapv make-key (:path body))
                b        (make-binding (:binding body))]
            [{} `[~(symbol b) (get-in ~message ~path)]])

          :key-matches-without-binding
          (let [k           (make-key (:key body))
                match       (:match-value body)
                predicate?  (= :compare-fn (first match))
                match-value (second match)]
            (cond
              predicate?
              [`({~k ~'_} :guard [(constantly (~(:fn match-value) (get-in ~message [~k])))])
               `[]]
              :else
              [`{~k ~match-value}
               `[]]))

          :key-matches-with-binding
          (let [k           (make-key (:key body))
                b           (make-binding (:binding body))
                match       (:match-value body)
                predicate?  (= :compare-fn (first match))
                match-value (second match)]
            (cond
              (optional? mode)
              [{}
               `[~(symbol b) (get-in ~message [~k] ~(:default-value body))]]
              predicate?
              [`({~k ~'_} :guard [(constantly (~(:fn match-value) (get-in ~message [~k])))])
               `[~(symbol b) (get-in ~message [~k])]]
              :else
              [`{~k ~match-value}
               `[~(symbol b) (get-in ~message [~k])]]))

          :path-matches-without-binding
          (let [path        (mapv make-key (:path body))
                match       (:match-value body)
                predicate?  (= :compare-fn (first match))
                match-value (second match)
                path-map    (assoc-in {} path match-value)]
            (cond
              predicate?
              [`(~(fold-path path '_) :guard [(constantly (~(:fn match-value) (get-in ~message ~path)))])
               `[]]
              :else
              [`~path-map
               `[]]))

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
               `[~(symbol b) (get-in ~message ~path ~(:default-value body))]]
              predicate?
              [`(~(fold-path path '_) :guard [(constantly (~(:fn match-value) (get-in ~message ~path)))])
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
  "Parse the argument to `defpattern` as a [[Pattern]]."
  [pattern]
  (if (vector? pattern)
    `(make-pattern (parse-clauses ~pattern))
    pattern))

;; Match

(defn key-exists-with-binding-clause->rhs-match
  [message clause]
  (let [key     (key-exists-with-binding-clause-key clause)
        binding (key-exists-with-binding-clause-binding clause)]
    `[~(symbol binding) (get-in ~message [~(convert-path-element key)])]))

(defn optional-key-exists-with-binding-clause->rhs-match
  [message clause]
  (let [key     (optional-key-exists-with-binding-clause-key clause)
        binding (optional-key-exists-with-binding-clause-binding clause)]
    `[~(symbol binding) (get-in ~message [~(convert-path-element key)])]))

(defn path-exists-with-binding-clause->rhs-match
  [message clause]
  (let [key         (path-exists-with-binding-clause-path clause)
        binding     (path-exists-with-binding-clause-binding clause)]
    `[~(symbol binding) (get-in ~message ~(mapv convert-path-element key))]))

(defn optional-path-exists-with-binding-clause->rhs-match
  [message clause]
  (let [key         (optional-path-exists-with-binding-clause-path clause)
        binding     (optional-path-exists-with-binding-clause-binding clause)]
    `[~(symbol binding) (get-in ~message ~(mapv convert-path-element key))]))

(defn key-matches-with-binding-clause->rhs-match
  [message clause]
  (let [key         (key-matches-with-binding-clause-key clause)
        match-value (matcher-default-value (key-matches-with-binding-clause-matcher clause))
        binding     (key-matches-with-binding-clause-binding clause)]
    `[~(symbol binding) (get-in ~message [~(convert-path-element key)] ~match-value)]))

(defn optional-key-with-default-binding-clause->rhs-match
  [message clause]
  (let [key           (optional-key-with-default-binding-clause-key clause)
        default-value (optional-key-with-default-binding-clause-default-value clause)
        binding       (optional-key-with-default-binding-clause clause)]
    `[~(symbol binding) (get-in ~message [~(convert-path-element key)] ~default-value)]))

(defn path-matches-with-binding-clause->rhs-match
  [message clause]
  (let [path        (path-matches-with-binding-clause-path clause)
        match-value (matcher-default-value (path-matches-with-binding-clause-matcher clause))
        binding     (path-matches-with-binding-clause-binding clause)]
    `[~(symbol binding) (get-in ~message ~(mapv convert-path-element path) ~match-value)]))

(defn optional-path-with-default-binding-clause->rhs-match
  [message clause]
  (let [path          (optional-path-with-default-binding-clause-path clause)
        default-value (optional-path-with-default-binding-clause-default-value clause)
        binding       (optional-path-with-default-binding-clause-binding clause)]
    `[~(symbol binding) (get-in ~message ~(mapv convert-path-element path) ~default-value)]))

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
    (key-exists-without-binding-clause? clause)
    (let [key     (key-exists-without-binding-clause-key clause)]
      `{~key ~'_})

    (key-exists-with-binding-clause? clause)
    (let [key     (key-exists-with-binding-clause-key clause)
          binding (key-exists-with-binding-clause-binding clause)]
      {(convert-path-element key) (symbol binding)})

    (optional-key-exists-with-binding-clause? clause)
    {}

    (path-exists-without-binding-clause? clause)
    (let [path    (path-exists-without-binding-clause-path clause)]
      (assoc-in {} (map convert-path-element path) '_))

    (path-exists-with-binding-clause? clause)
    (let [path    (path-exists-with-binding-clause-path clause)
          binding (path-exists-with-binding-clause-binding clause)]
      (assoc-in {} (map convert-path-element path) (symbol binding)))

    (optional-path-exists-with-binding-clause? clause)
    {}

    (key-matches-without-binding-clause? clause)
    (let [key         (key-matches-without-binding-clause-key clause)
          matcher     (key-matches-without-binding-clause-matcher clause)
          match-value (matcher->value matcher)]
      (if (predicate-matcher? matcher)
        `({~key ~'_} :guard ~(match-value message [key]))
        `{~key ~(match-value message [key])}))

    (key-matches-with-binding-clause? clause)
    (let [key         (key-matches-with-binding-clause-key clause)
          matcher     (key-matches-with-binding-clause-matcher clause)
          match-value (matcher->value matcher)]
      (if (predicate-matcher? matcher)
        `({~key ~'_} :guard ~(match-value message [key]))
        `{~key ~(match-value message [key])}))

    (optional-key-with-default-binding-clause? clause)
    {}

    (path-matches-without-binding-clause? clause)
    (let [path        (path-matches-without-binding-clause-path clause)
          matcher     (path-matches-without-binding-clause-matcher clause)
          match-value (matcher->value matcher)]
      (if (predicate-matcher? matcher)
        `(~(fold-path path '_) :guard ~(match-value message path))
        (fold-path path (match-value message path))))

    (path-matches-with-binding-clause? clause)
    (let [path        (path-matches-with-binding-clause-path clause)
          matcher     (path-matches-with-binding-clause-matcher clause)
          match-value (matcher->value matcher)]
      (if (predicate-matcher? matcher)
        `(~(fold-path path '_) :guard ~(match-value message path))
        (fold-path path (match-value message path))))

    (optional-path-with-default-binding-clause? clause)
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
              (let [[acc-map _guard-key acc-tail] acc
                    [lhs-map _guard-key lhs-tail] lhs]
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
    (key-exists-without-binding-clause? clause)
    (conj bindings `[])

    (key-exists-with-binding-clause? clause)
    (conj bindings (key-exists-with-binding-clause->rhs-match message clause))

    (optional-key-exists-with-binding-clause? clause)
    (conj bindings (optional-key-exists-with-binding-clause->rhs-match message clause))

    (path-exists-without-binding-clause? clause)
    (conj bindings `[])

    (path-exists-with-binding-clause? clause)
    (conj bindings (path-exists-with-binding-clause->rhs-match message clause))

    (optional-path-exists-with-binding-clause? clause)
    (conj bindings (optional-path-exists-with-binding-clause->rhs-match message clause))

    (key-matches-without-binding-clause? clause)
    (conj bindings `[])

    (key-matches-with-binding-clause? clause)
    (conj bindings (key-matches-with-binding-clause->rhs-match message clause))

    (optional-key-with-default-binding-clause? clause)
    (conj bindings (optional-key-with-default-binding-clause->rhs-match message clause))

    (path-matches-without-binding-clause? clause)
    (conj bindings `[])

    (path-matches-with-binding-clause? clause)
    (conj bindings (path-matches-with-binding-clause->rhs-match message clause))

    (optional-path-with-default-binding-clause? clause)
    (conj bindings (optional-path-with-default-binding-clause->rhs-match message clause))))

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

  - `(<key> <value>)` which requires the key `<key>` to be mapped to `<value>`
    in the map.

  - `(<key> :as <name>)` which requires `<key>` to be present in the map
    and binds `<name>` to its value.

  - `<key>` which requires `<key>` to be present in the map.

  The map matcher also supports optional keys:

  - `(? <key> <default> :as <name>)` binds `<name>` to to the value of
    `<key>` in the map or to `<default>` if `<key>` is not in the map.

  - `(? <key> :as <name>)` binds `<name>` to to the value of `<key>`
    in the map or to `nil` if `<key>` is not in the map.

  Access to nested values is also possible. Use `[<key>+]` to access
  a nested value, where `[<key>+]` is a sequence of keys. When no
  `:as <name>` clause is given, the last `<key>` of the sequence of
  keys is used as a name to bind the value.

  `<key>` can be either a symbol or a keyword.
  If `<key>` is a symbol, it is converted to a string when
  used as a key.

  `<value>` can be:

  - any value, regular expressions are also possible (only in Clojure, though,
    `core.match` does not support regex matching in ClojureScript).

  - a list of alternative values in the form of: `(:or <value> <value>*)`.

  - a custom compare function in the form of:
    `(:compare-fn <compare-fn>)` where `<compare-fn>` accepts the value that
    is mapped to `<key>`.

  `map-matcher` returns a function that accepts a map and evaluates
  `<consequent>` with all the `<name>`s bound when the message matches
  the given `<clause>`s, otherwise it evaluates `<alternative>`. or
  throws `IllegalArgumentException` if no `<clause>` matches and no
  `<alternative>` is given.

  Example:

        (def example-map-matcher
          (map-matcher
            [(:x \"x\" :as x)
             (:y \"y\")
             (:z :as z)
             :w]
            (println x z)
            [(:a \"a\" :as a)
             (:b \"b\")
             (:c :as c)
             ([:d] :as d)
             ([:d Z] 42 :as Z)
             ([:d Y] :as Y)
             ([:d X] 65)]
            (println a c d Z Y)
            :else false))

        (example-map-matcher {:a \"a\" :b \"b\" :c \"c\"
                              :d {\"Z\" 42 \"Y\" 23 \"X\" 65
                                  \"W\" {\"foo\" \"bar\"}}})

    prints
     \"a c {Z 42, Y 23, X 65, W {foo bar}} 42 23\""
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
