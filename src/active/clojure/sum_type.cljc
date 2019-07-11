(ns active.clojure.sum-type
  (:require
   #?(:clj  [active.clojure.record :as record]
      :cljs [active.clojure.cljs.record :as record :include-macros true])
   [active.clojure.record-helper :as record-helper]
   #?(:clj  [active.clojure.lens :as lens]
      :cljs [active.clojure.lens :as lens :include-macros true])))


(def sum-type-identifier ::sum-type)

(defn debug-info-str [debug-info]
  (str "in " (:ns debug-info) ", line: " (:line debug-info) ", column: " (:column debug-info)))

#?(:clj
   (defn throw-illegal-argument-exception [^java.lang.String msg]
     (throw (new java.lang.IllegalArgumentException msg))))

#?(:clj
   (defn- metadata
     "Returns metadata depending on the environment, clj or cljs.
     If cljs, env is defined, else we assume clj."
     [t env]
     (if (:ns env)
       ;; we resolve cljs.analyzer.api here to make utilizing the
       ;; same source file possible for both, cljs & clj
       (:meta ((resolve 'cljs.analyzer.api/resolve) env t))
       (meta (resolve t)))))


#?(:clj
   (defn- resolve-qualified-str
     "Returns a string representing the namespace-qualified symbol
     depending on the environment, clj or cljs.
     If cljs, env is defined, else we assume clj."
     [t env]
     (if (:ns env)
       ;; we resolve cljs.analyzer.api here to make utilizing the
       ;; same source file possible for both, cljs & clj
       (str (:name ((resolve 'cljs.analyzer.api/resolve) env t)))
       (str (:ns (meta (resolve t))) "/" (:name (meta (resolve t)))))))



;; a clause is one of the following:
;; - ClauseWithPredicate, describing a matching clause based on a prediate
;; - DefaultClause, describing a matching clause based on the special form :default
;; - ClauseWithExtraction, describing a matching clause based on a constructor

(record/define-record-type ClauseWithPredicate
  (make-clause-with-predicate predicate body) clause-with-predicate?
  [predicate clause-with-predicate-predicate
   body clause-with-predicate-body])

(record/define-record-type DefaultClause
  (make-default-clause body) default-clause?
  [body default-clause-body])

(record/define-record-type ClauseWithExtraction
  (make-clause-with-extraction constructor-symbol named-params body) clause-with-extraction?
  [constructor-symbol clause-with-extraction-constructor-symbol
   named-params clause-with-extraction-named-params
   body clause-with-extraction-body])





(defn- order-accessors-1 [args field-tuples]
  (let [accessor-map (into {} field-tuples)]
    (mapv #(vector % (get accessor-map %)) args)))

(defn- order-accessors
  "Orders the accessors `:field-tuples` according to the args in `:args`.
  Does nothing if sum-type meta instead of record meta passed.
  Returns a meta."
  [meta]
  (if (= sum-type-identifier (:t meta))
    meta
    (let [args (:args meta)
          field-tuples (:field-tuples meta)]
      (assoc meta :field-tuples
        (order-accessors-1 args field-tuples)))))


#?(:clj
   (defn- get-predicate [s env]
     (:predicate (metadata s env))))


(defn- sum-type-meta? [meta]
  (= sum-type-identifier (:t meta)))

(defn- record-type-meta? [meta]
  (= record-helper/record-identifier (:t meta)))

(defn- record-or-sum-type-meta? [meta]
  (or
    (record-type-meta? meta)
    (sum-type-meta? meta)))



#?(:clj
   (defn- find-illegal-types [type-symbols env]
     (filter
       (fn [t] (not (record-or-sum-type-meta? (metadata t env))))
       type-symbols)))


#?(:clj
   (defn- throw-illegal-types! [t]
     (throw-illegal-argument-exception
       (apply str "rtd-record or sum-type required, found: " (clojure.string/join ", " t)))))

#?(:clj
   (defn- throw-when-illegal-types! [type-symbols env]
     (let [illegal-types (find-illegal-types type-symbols env)]
       (when-not (empty? illegal-types)
         (throw-illegal-types! illegal-types)))))


#?(:clj
   (defn- add-meta
     [sym meta-info]
     (vary-meta sym (fn [m] (merge meta-info m)))))


#?(:clj
   (defmacro define-sum-type [type-name predicate type-symbols]


     (let [sym-fn                (fn [a] (str *ns* "/" a))
           resolved-type-symbols (mapv #(resolve-qualified-str % &env) type-symbols)
           sum-type-meta         {:predicate (sym-fn predicate)
                                  :t         sum-type-identifier

                                  :sub-types
                                  (mapv #(-> (metadata % &env)
                                           (order-accessors)
                                           (dissoc :file)
                                           (dissoc :meta)
                                           (dissoc :end-line)
                                           (dissoc :end-column)
                                           (dissoc :name) ; this leads to a crash in clj
                                           (dissoc :column)) type-symbols)}]

       (throw-when-illegal-types! type-symbols &env)


       `(do

          (let [rss# ~resolved-type-symbols] ;; we only do resolution once

            (defn ~predicate [arg#]
              (boolean (some true?
                         (map (fn [pred#] (pred# arg#))
                           ~(mapv #(symbol (get-predicate (symbol %) &env)) resolved-type-symbols)))))

            (def ~(add-meta type-name sum-type-meta) ~sum-type-meta))))))




#?(:clj
   (defn- parse-clauses
     "Translates clauses into an internal representation"
     [paired-clauses]
     (mapv
       (fn [[condition body]]
         (cond
           (list? condition)
           (make-clause-with-extraction (first condition) (vec (rest condition)) body)

           (= :default condition)
           (make-default-clause body)

           :else
           (make-clause-with-predicate condition body)))

       paired-clauses)))



#?(:clj
   (defn- has-default? [parsed-clauses debug]
     ;; checks if clauses contains default clause and if it is the last clause
     ;; throws if position of default (last) is violated
     ;; returns true if default is found iff in last position, false else
     (if (some default-clause? parsed-clauses)
       (do
         (if-not (default-clause? (last parsed-clauses))
           (throw (IllegalArgumentException. (str "Default clause only allowed as last clause " (debug-info-str debug))))
           true))
       false)))


(defn- collect-constr-symbols [parsed-clauses]
  (->> parsed-clauses
    (filter clause-with-extraction?)
    (map clause-with-extraction-constructor-symbol)))


(defn- collect-pred-symbols [parsed-clauses]
  (->> parsed-clauses
    (filter clause-with-predicate?)
    (map clause-with-predicate-predicate)))


(defn- expand-default-clause-cljs [clause]
  (let [body (default-clause-body clause)]
    [:default body]))


(defn- expand-clause-with-predicate-cljs [arg clause]
  (let [pred-sym (clause-with-predicate-predicate clause)
        body     (clause-with-predicate-body clause)]
    [(list pred-sym arg) body]))


(defn- expand-clause-with-extraction-cljs [constr-lookup arg clause]
  (let [constr-sym  (clause-with-extraction-constructor-symbol clause)
        body        (clause-with-extraction-body clause)
        constr-args (clause-with-extraction-named-params clause)
        pred        (:predicate (get constr-lookup constr-sym))
        accessors   (mapv second (:field-tuples (get constr-lookup constr-sym)))]

    [(list (symbol pred) arg)
     (list 'let
       (vec
         (mapcat identity
           (map-indexed (fn [idx constr-arg] [constr-arg (list 'active.clojure.lens/yank  arg
                                                           (symbol (accessors idx)))])
             constr-args)))
       body)]))


(defn- expand-clause-cljs [constr-lookup arg clause]
  (cond
    (default-clause? clause)
    (expand-default-clause-cljs clause)

    (clause-with-predicate? clause)
    (expand-clause-with-predicate-cljs arg clause)

    (clause-with-extraction? clause)
    (expand-clause-with-extraction-cljs constr-lookup arg clause)))


#?(:clj
   (def runtime-error throw-illegal-argument-exception)

   :cljs
   (defn runtime-error [msg]
     (throw (js/Error. msg))))


(defn- expand-clauses-cljs [constr-lookup arg parsed-clauses st]
  (let [arg-symbol        (gensym)
        constr-lookup-sym (gensym)]
    (list 'let [arg-symbol arg]
      (list 'if (list (symbol (:predicate st)) arg-symbol)
        (apply list 'cond
          (mapcat #(expand-clause-cljs constr-lookup arg-symbol %) parsed-clauses))
        (list 'active.clojure.sum-type/runtime-error (str "Argument not of type " (:predicate st)))))))


#?(:clj
   (defn- resolved-symbol-lookup
     "Creates a map from symbol to namespace qualified symbol strings"
     [symbols env]
     (into {} (mapv (fn [s] [s (resolve-qualified-str s env)]) symbols))))


(defn- filter-tree
  "Finds nodes matching predicate in the type-tree"
  [predicate tree]
  (concat
    (if (predicate tree) [tree] [])
    (mapcat #(filter-tree predicate %) (:sub-types tree))))


(defn- filter-predicate
  "Finds a node containing the given predicate symbol string in the type-tree"
  [pred-symbol tree]
  (first (filter-tree #(= pred-symbol (:predicate %)) tree)))

(defn- filter-constructor
  "Finds a node containing the given constructor symbol string in the type-tree"
  [constr-symbol tree]
  (first (filter-tree #(= constr-symbol (:constructor %)) tree)))


#?(:clj
   (defn throw-non-type-functions! [t st debug]
     (throw-illegal-argument-exception
       (apply str "The following functions don't belong to records or sum-types of type `"
         st "`: " (clojure.string/join ", " t)
         " " (debug-info-str debug)))))


(defn find-non-type-functions [tree symbols]
  (filter #(not (or (filter-constructor % tree) (filter-predicate % tree))) symbols))


#?(:clj
   (defn throw-when-non-type-functions!
     "Throws if fun-symbols contains functions that are neither
      a predicate nor a constructor in the type-tree"
     [tree fun-symbols t debug]
     (let [non-type-functions (find-non-type-functions tree fun-symbols)]
       (when-not (empty? non-type-functions)
         (throw-non-type-functions! non-type-functions t debug)))))


(defn constr-or-pred?-fn [sym]
  (fn [tree]
    (or
      (= sym (:constructor tree))
      (= sym (:predicate tree)))))


(defn colorize
  "Colorizes a node if pred matches (that is, setting colored? to `true`"
  [pred tree]
  (let [t        (if (pred tree)
                   (assoc tree :colored? true)
                   tree)
        children (:sub-types t)]
    (if children
      (assoc t :sub-types (map #(colorize pred %) children))
      t)))


(defn find-non-colored-leafs
  "Finds non-colored leafs by recursion. Stops descending if colored intermediate node occurs"
  [tree]
  (cond
    (:colored? tree)  []
    (:sub-types tree) (mapcat find-non-colored-leafs (:sub-types tree))
    :default          [tree]))


(defn find-not-covered
  "Finds all predicates in the type-tree that are not covered by symbols"
  [tree symbols]
  (->> symbols
    (reduce (fn [tree sym] (colorize (constr-or-pred?-fn sym) tree)) tree)
    (find-non-colored-leafs)
    (map :predicate)))

#?(:clj
   (defn- throw-not-exhaustive! [t st debug]
     (throw-illegal-argument-exception
       (apply str "Arguments of the following types will fail matching of type `" st
         "`: " (clojure.string/join ", " t) " " (debug-info-str debug)))))


#?(:clj
   (defn- throw-when-not-exhaustive!
     "Throws IllegalArgumentException if matching is not exhaustive"
     [tree symbols t debug]
     (let [not-covered (find-not-covered tree symbols)]
       (when-not (empty? not-covered)
         (throw-not-exhaustive! not-covered t debug)))))


#?(:clj
   (defn- throw-when-not-sum-type-meta
     "Throws IllegalArgumentException if first param is no sum-type"
     [m sym debug]
     (when-not (sum-type-meta? m)
       (throw-illegal-argument-exception (str "First param `" sym "` is no sum-type " (debug-info-str debug))))))

#?(:clj
   (defn- throw-when-not-even
     "Throws IllegalArgumentException if clauses has no even length"
     [clauses debug]
     (when-not (zero? (mod (count clauses) 2))
       (throw-illegal-argument-exception (str "`match` takes an even number of clauses " (debug-info-str debug))))))

#?(:clj
   (defn- throw-when-not-a-symbol
     "Throws IllegalArgumentException if ?sym is not a symbol"
     [?sym debug]
     (when-not (symbol? ?sym)
       (throw-illegal-argument-exception (str ?sym " must be a symbol " (debug-info-str debug))))))

#?(:clj
   (defn debug-info [form ns]
     (assoc (meta form) :ns (str ns))))


#?(:clj
   (defmacro match
     "Takes a sum-type, a argument and a list of clauses, and expands it to a cond form.
     `sum-type` is a type identifier, as defined by `define-sum-type`.
     `arg` is the argument to be matched upon.
     `clauses` are pairs of conditions and bodies, e.g.:

     `(match rgb-color a
         red? \"red\"
         (make-green a) (str \"Green with \" a)
         blue? \"blue\")
     `

     There is also a default clause, denoted by the keyword `:default` as the condition.

     This macro throws at compile time if (ordered):
     - `sum-type` is no symbol
     - `sum-type` doesn't resolve to a sum-type
     - an uneven number of clauses is passed
     - conditions contain a non-related function, that is, not a predicate or constructor of
       the passed sum-type in `sum-type`.
     - The matching is not exhaustive, i.e. a particular predicate/constrcutor is missing.

     The resulting form throws at runtime if the passed argument is not of type `sum-type`"

     [sum-type arg & clauses]

     (let [debug                         (debug-info &form *ns*)
           _                             (throw-when-not-a-symbol sum-type debug)
           sum-type-meta                 (metadata sum-type &env)
           _                             (throw-when-not-sum-type-meta sum-type-meta sum-type debug)
           _                             (throw-when-not-even clauses debug)
           paired-clauses                (partition 2 clauses)
           parsed-clauses                (parse-clauses paired-clauses)
           pred-symbols                  (collect-pred-symbols parsed-clauses)
           constr-symbols                (collect-constr-symbols parsed-clauses)
           resolved-pred-symbols         (map #(resolve-qualified-str % &env) pred-symbols)
           resolved-constr-symbol-lookup (resolved-symbol-lookup constr-symbols &env)
           constr->predicate-lookup      (->> resolved-constr-symbol-lookup
                                           (mapv (fn [[k v]]
                                                   [k (filter-constructor v sum-type-meta)]))
                                           (into {}))

           resolved-function-symbols (concat resolved-pred-symbols
                                       (map second resolved-constr-symbol-lookup))]


       (throw-when-non-type-functions! sum-type-meta resolved-function-symbols sum-type debug)

       (when (not (has-default? parsed-clauses debug))
         (throw-when-not-exhaustive! sum-type-meta resolved-function-symbols sum-type debug))

       (expand-clauses-cljs constr->predicate-lookup arg parsed-clauses sum-type-meta))))
