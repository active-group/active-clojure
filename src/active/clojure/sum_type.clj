(ns active.clojure.sum-type
  (:require [active.clojure.record :as record]))

(record/define-record-type SumTypeMeta
  ;; A SumTypeMeta represents a node of a tree structure.
  ;; Types represent children of the node, again being a SumTypeMeta or a record/RecordMeta.
  (make-sum-type-meta predicate types) sum-type-meta?
  [predicate sum-type-meta-predicate
   types sum-type-meta-types])

(record/define-record-type RecordMeta
  ;; Record meta record-type. Since in record definition no record-types
  ;; are available we translate it.
  (make-record-meta predicate constructor ordered-accessors) record-meta?
  [predicate record-meta-predicate
   constructor record-meta-constructor
   ordered-accessors record-meta-ordered-accessors])


(defn predicate->sum-type-meta [predicate] (:meta (meta predicate)))
(defn sum-type-predicate? [type-or-predicate]
  (sum-type-meta? (predicate->sum-type-meta type-or-predicate)))

;; a clause is one of the following:
;; - ClauseWithPredicate, describing a matching clause based on a prediate
;; - DefaultClause, describing a matching clause based on the special form :default
;; - ClauseWithExtraction, describing a matching clause based on a constructor

(record/define-record-type CaluseWithPredicate
  (make-clause-with-predicate predicate clause) clause-with-predicate?
  [predicate clause-with-predicate-predicate
   clause clause-with-predicate-clause])

(record/define-record-type DefaultClause
  (make-default-clause clause) default-clause?
  [clause default-clause-clause])

(record/define-record-type ClauseWithExtraction
  (make-clause-with-extraction
   predicate constructor-call clause ordered-accessors) clause-with-extraction?
  [predicate clause-with-extraction-predicate
   constructor-call clause-with-extraction-constructor-call
   clause clause-with-extraction-clause
   ordered-accessors clause-with-extraction-ordered-accessors])



(defn- has-predicate? [predicate type-tree]
  (cond
    (sum-type-meta? type-tree)
    (or
     (= predicate (sum-type-meta-predicate type-tree))
     (some identity (map #(has-predicate? predicate %) (sum-type-meta-types type-tree))))

    (record-meta? type-tree)
    (= (record-meta-predicate type-tree) predicate)))

(defn collect-leafs [type-tree]
  ;; collects all leafs from a type-tree, that is, all record-types
  (cond
    (record-meta? type-tree) [type-tree]
    (sum-type-meta? type-tree) (mapcat collect-leafs (sum-type-meta-types type-tree))))


(defn remove-predicate [type-tree predicate]
  ;; removes all types having the given predicate as their predicate from the type-tree
  (cond
    (record-meta? type-tree)
    (when-not (= predicate (record-meta-predicate type-tree))
      type-tree)

    (sum-type-meta? type-tree)
    (when-not (= predicate (sum-type-meta-predicate type-tree))
      (make-sum-type-meta
       (sum-type-meta-predicate type-tree)
       (remove nil? (map #(remove-predicate % predicate)
                         (sum-type-meta-types type-tree)))))))


(defn check-predicates! [type-tree predicates has-default]

  ;; checks if all predicates fullfil the following requirements:
  ;;
  ;; 1) A predicate is corresponds to one of the types in the tree. The only exception is :default
  ;; 2) A predicate is reachable. A predicate is defined unreachable if another
  ;;    predicate matches its value before it is executed.
  ;; 3) A matching is exhaustive. That is, that every input-value has at least one
  ;;    predicate in the type tree that evaluates to true. If default is given this check is
  ;;    skipped.
  ;;
  ;; To fullfil the last two requirements types are removed iteratively by their predicate, in
  ;; `predicates` given order. They idea is that if a sum-type node is removed, all its children
  ;; are removed from the tree, too.
  ;;
  ;; Before each deletion it is checked if the predicate is present in the tree (requirement 2)
  ;; After that, the corresponding type is removed. If leafs exist after all predicates are handled
  ;; the matching is not exhaustive.
  ;;
  ;; Throws an IllegalArgumentException if one the requirements fails.

  (let [missing-predicates
        (-> (reduce (fn [working-type-tree predicate]

                      ;; requirement 1
                      (when-not (or (= :default predicate)(has-predicate? predicate type-tree))
                        (throw (IllegalArgumentException. (str "The following predicate is not of any type in "
                                                               (sum-type-meta-predicate type-tree)
                                                               ": " predicate))))

                      ;; requirement 2
                      ;; TODO: Make this a warning? How does a warning look like?
                      (when-not (or (= :default predicate)(has-predicate? predicate working-type-tree))
                        (throw (IllegalArgumentException. (str "The following predicate will never be reached: "
                                                               predicate))))

                      (remove-predicate working-type-tree predicate))
                    type-tree predicates)
            (collect-leafs))]

    ;; requirement 3
    (when-not (or has-default (empty? missing-predicates))
      (throw (Exception. (str "Non exhaustive match would fail on the following type(s): "
                              (mapv record-meta-predicate missing-predicates)))))))


(defn- ->record-meta [untyped]
  (make-record-meta
   (:predicate untyped)
   (:constructor untyped)
   (:ordered-accessors untyped)))


(defmacro define-sum-type [type-name predicate predicates]
  ;; TODO doc
  (let [qualified-predicates (doall (map #(ns-resolve *ns* %) predicates))
        _ (intern *ns* predicate)]
    (when-not (every? identity
                      (map #(or (record/record-type-predicate? %) (sum-type-predicate? %))
                           qualified-predicates))
      (throw (IllegalArgumentException. (str "Predicates of active.clojure.record or active.clojure.sum-type "
                                             "required, found: " (pr-str (map record/predicate->record-meta qualified-predicates))))))

    `(do
       (defn ~(vary-meta predicate
                        assoc :meta
                        (make-sum-type-meta
                         (resolve predicate)
                         (map #(cond
                                 (record/record-type-predicate? %)
                                 (->record-meta (record/predicate->record-meta %))

                                 (sum-type-predicate? %)
                                 (predicate->sum-type-meta %))
                              qualified-predicates)))
         [arg#]
         (boolean (some identity (map (fn [pred#] (pred# arg#)) ~predicates))))
       )))


(defn- find-meta-by-constructor [constructor type-tree]
  ;; searches for meta in type-tree by constructor
  ;; returns nil if not present, else a RecordMeta
  (cond
    (sum-type-meta? type-tree)
    (some identity (map #(find-meta-by-constructor constructor %)
                        (sum-type-meta-types type-tree)))

    (record-meta? type-tree)
    (when (= (record-meta-constructor type-tree) constructor)
      type-tree)))

(defn parse-clause-with-extraction [condition clause sum-type-meta]
  ;; makes a clause-with-extraction for constructor based matching
  ;; throws if the number of matching arguments differs from constructor definition
  ;; throws if not every constructor argument is a symbol (`_` are ignored).
  ;; throws if constructor is not found in sum-type
  (if-let [meta (find-meta-by-constructor (first condition) sum-type-meta)]
    (let [predicate (record-meta-predicate meta)
          ordered-accessors (record-meta-ordered-accessors meta)]

      ;; check correct number of arguments in constructor
      (when-not (= (count ordered-accessors) (count (rest condition)))
        (throw (IllegalArgumentException.
                (str (first condition) " requires " (count ordered-accessors) " arguments: " condition))))

      ;; check if every constructor argument is a symbol
      (when-not (every? symbol? (rest condition))
        (throw (IllegalArgumentException.
                (str "Every argument in constructor matching must be a symbol: " condition))))

      (make-clause-with-extraction predicate condition clause ordered-accessors))

    ;; if constructor not found in meta
    (throw (IllegalArgumentException. (str "Constructor not found in sum-type types: " (first condition))))))


(defn- parse-clause-with-predicate [predicate clause sum-type-meta]
  ;; makes a clause-with-predicate for predicate based matching
  ;; throws if predicate is not present in sum-type
  (cond
    (= :default predicate)
    (make-default-clause clause)

    (has-predicate? predicate sum-type-meta)
    (make-clause-with-predicate predicate clause)

    :else
    (throw (IllegalArgumentException.
            (str "Predicate " predicate " not found in sum-type type: " (:predicate sum-type-meta))))))

(defn parse-clauses [paired-clauses sum-type-meta]
  ;; parses all clauses-forms to one of the following:
  ;; clause-with-extraction for constructor based matching
  ;; clause-with-predicate for predicate based matching
  ;; default for :default special form
  (mapv
   (fn [[condition clause]]
     (if (list? condition)
       ;; (list (ns-resolve *ns* (first condition)) (rest condition))
       (parse-clause-with-extraction (cons (ns-resolve *ns* (first condition)) (rest condition))
                               clause sum-type-meta)
       (parse-clause-with-predicate (if (= :default condition)
                                :default
                                (ns-resolve *ns* condition))
                              clause sum-type-meta)))

   paired-clauses))


(defn clause->predicate [clause]
  ;; extracts the predicate for a given clause
  (cond
    (clause-with-extraction? clause)
    (clause-with-extraction-predicate clause)

    (clause-with-predicate? clause)
    (clause-with-predicate-predicate clause)

    (default-clause? clause)
    :default))



(defn- expand-clause-with-extraction-forms [clause arg]
  ;; creates forms for constructor based cond case
  (let [predicate (clause-with-extraction-predicate clause)
        constructor-args (rest (clause-with-extraction-constructor-call clause))
        implied-clause (clause-with-extraction-clause clause)
        ordered-accessors (clause-with-extraction-ordered-accessors clause)
        args-to-accessors (->> (map vector constructor-args ordered-accessors)
                               ;; filter _
                               (filter (fn [[ binding _]] (not= '_ binding)))
                               (mapcat (fn [[binding accessor]]
                                         [binding (list accessor arg)])))]
    [(list predicate arg) (list 'let (vec args-to-accessors) implied-clause)]))

(defn- expand-clause-with-predicate-forms [clause arg]
  ;; creates forms for predicate cond case
  (list
   (list (clause-with-predicate-predicate clause) arg)
   (clause-with-predicate-clause clause)))

(defn- expand-default-clause [clause]
  ;; Creates forms for default cond case
  (list :default (default-clause-clause clause)))

(defn expand-clause-forms [clause arg]
  ;; creates form for clause depending on type
  (cond
   (clause-with-extraction? clause)
   (expand-clause-with-extraction-forms clause arg)

   (clause-with-predicate? clause)
   (expand-clause-with-predicate-forms clause arg)

   (default-clause? clause)
   (expand-default-clause clause)))


(defn has-default? [parsed-clauses]
  ;; checks if clauses contains default clause and if it is the last clause
  ;; throws if position of default (last) is violated
  ;; returns true if default is found iff in last position, false else
  (if (some default-clause? parsed-clauses)
    (do
      (if-not (default-clause? (last parsed-clauses))
        (throw (IllegalArgumentException. "Default clause only allowed as last clause"))
        true))
    false))

(defmacro match [sum-type-predicate arg & clauses]
  (let [sum-type-meta (predicate->sum-type-meta (ns-resolve *ns* sum-type-predicate))
        paired-clauses (partition 2 clauses)
        parsed-clauses (parse-clauses paired-clauses sum-type-meta)
        default (has-default? parsed-clauses)
        predicates (mapv clause->predicate parsed-clauses)
        expanded-clauses-forms (mapcat #(expand-clause-forms % arg) parsed-clauses)]

    (do
      (check-predicates! sum-type-meta predicates default)
      `(do
         (when-not (~sum-type-predicate ~arg)
           (throw (IllegalArgumentException. (str "Matching argument not of type " ~sum-type-predicate ": " ~arg))))
         ~(cons 'cond expanded-clauses-forms)))))
