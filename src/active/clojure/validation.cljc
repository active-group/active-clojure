(ns active.clojure.validation
  "This namespace provides the utilities for applicative data validation.
  Chiefly, it provides the [[validation]] function that defines a data
  validation.  It also provides predefined data validators for common
  use-cases (ints, strings, booleans, ...).

  Example:
  ```
  (defn example-validation
    [id name]
    (validation (fn [id name] {:id id :name name})
                (validate-pos-int id :id)
                (validate-string name)))

  (example-validation [42 \"name\"])
  ;; => active.clojure.validation/ValidationSuccess{:candidate {:id 42, :name \"name\"}}
  (example-validation [42 23])
  ;; => active.clojure.validation/ValidationFailure{:errors
  ;;      [active.clojure.validation/ValidationError{:candidate 23,
  ;;                                                 :message :active.clojure.validation/string,
  ;;                                                 :label nil}]}
  (example-validation [\"name\" 42])
  ;; => phoenix.common.validation/ValidationFailure{:errors
  ;;      (active.clojure.validation/ValidationError{:candidate \"name\",
  ;;                                                 :message :active.clojure.validation/pos-int,
  ;;                                                 :label :id}
  ;;       active.clojure.validation/ValidationError{:candidate 42,
  ;;                                                 :message :active.clojure.validation/string,
  ;;                                                 :label nil})}
  ```
  "
  (:require #?(:clj [active.clojure.record :refer [define-record-type]]
               :cljs [active.clojure.cljs.record :refer-macros [define-record-type]])
            [active.clojure.condition :as condition]
            [active.clojure.lens :as lens]))

;;; Either
(define-record-type ^{:doc "Signifies a failured validation.  Holds
  the [[ValidationError]]."}
  ValidationFailure
  make-validation-failure validation-failure?
  [^{:doc "A sequence of [[ValidationError]] that lead to the failed
validation."}
   errors validation-failure-errors])

(define-record-type ^{:doc "Signifies a successful validation.  Holds
  the candidate value that was being validated."}
  ValidationSuccess
  make-validation-success validation-success?
  [^{:doc "The candidate value that was beign validated."}
   candidate validation-success-candidate])

(defn validation-result?
  "Checks if `thing` is a validation result."
  [thing]
  (or (validation-failure? thing)
      (validation-success? thing)))

(defn with-validation-result
  "Takes a validation `result` and applies `f-success` to the whole
  result if it is a [[ValidationSuccess]], otherwise applies
  `f-failure` to the whole [[ValidationFailure]]."
  [f-success f-failure result]
  (cond
    (validation-failure? result) (f-failure result)
    (validation-success? result) (f-success result)))

;; Functor
(defn fmap-success
  "fmap for validation success: Apply `f` to `e`'s candidate iff e is
  a [[ValidationSuccess]]."
  [f e]
  (cond
    (validation-failure? e)  e
    (validation-success? e)
    (make-validation-success (f (validation-success-candidate e)))))

(defn fmap-failure
  "fmap for validation errors: Apply `f` to each of `e`'s errors iff e is a [[ValidationFailure]]"
  [f e]
  (cond
    (validation-failure? e) (make-validation-failure (map f (validation-failure-errors e)))
    (validation-success? e) e))

(defn fmap-result
  "fmap over the result of a validation.  Applies `(fmap-success
  f-success result`) if `result` is a [[ValidationSuccess]]
  and `(fmap-failure f-failure result)` if `result` is
  a [[ValidationFailure]]"
  [f-success f-failure result]
  (cond
    (validation-failure? result) (fmap-failure f-success result)
    (validation-success? result) (fmap-success f-failure result)))

;; Validation specifics
(define-record-type ^{:doc "Signifies a the error of a failed
  validation.  Holds the candidate value that was being validated, the
  corresponding error message and an arbitrary label."}
  ValidationError
  make-validation-error validation-error?
  [^{:doc "The candidate value that was being validated."}
   candidate validation-error-candidate
   ^{:doc "A message signifying was kind of error occured.  It should
possible for the user to interpret the message as they please, so
usually a namespaced keyword representing the error works well."}
   message validation-error-message
   ^{:doc "Arbitrary data that can be added to an error."}
   label validation-error-label])

(defn override-error-labels
  "override validation error labels with `new-label` in each of `e`'s
  error iff `e` is a [[ValidationFailure]]"
  [e new-label]
  (fmap-failure #(validation-error-label % new-label) e))

(defn override-error-messages
  "override validation error messages with `new-message` in each of
  `e`'s error iff `e` is a [[ValidationFailure]]"
  [e new-message]
  (fmap-failure #(validation-error-message % new-message) e))

(defn mappend-validation-failure
  "mappend the [[validation-failure-errors]] of
  two [[ValidationFailure]]s."
  [vf1 vf2]
  (make-validation-failure (concat (validation-failure-errors vf1)
                                   (validation-failure-errors vf2))))

;;; Applicative
(def pure-validation
  "Lift a value into the validation applicative."
  make-validation-success)

(defn seq-validation
  "Apply two validations sequentially, from left to right.  Analogous
  to `Either` where `ValidationFailure` ts `Left` and
  `ValidationSuccess` is `Right`."
  [v-1 v-2]
  (cond
    (and (validation-failure? v-1)
         (validation-failure? v-2))
    (mappend-validation-failure v-1 v-2)

    (validation-failure? v-1)
    v-1

    (validation-success? v-1)
    (fmap-success (validation-success-candidate v-1) v-2)))

(defn and-then
  "Apply validations in sequence.  Takes a validation `e` and a function
  `f` and applies `e`'s candidate iff `e` is a [[ValidationSuccess]]."
  [e f]
  (cond
    (validation-failure? e) e
    (validation-success? e) (f (validation-success-candidate e))

    :else
    (condition/assertion-violation `and-then "not a validation-result" e)))

(defn curry-n
  "Curry a function `f` of arity `n`."
  [f n]
  (if (zero? n)
    f
    (fn [x]
      (if (= n 1)
        (f x)
        (curry-n (partial f x) (dec n))))))

(defn validation
  "Takes a result construtor function and a sequence of validations.
  If all validations success, constructs a result from the validated
  values via `make-result`, wrapped in a `ValidationSuccess`.  The
  arguments will be supplied to `make-result` in the order in which
  they were validated.

  If any one validation fails, returns a `ValidationFailure`,
  containing _all_ failures."
  [make-result & validations]
  (reduce (fn [res v] (seq-validation res v))
          (pure-validation (curry-n make-result (count validations)))
          validations))

(defn- augment-validation-error-message
  [validation-error msg]
  (lens/overhaul validation-error validation-error-message (fn [m] [msg m])))

(defn- augment-validation-error-label
  [validation-error label]
  (lens/overhaul validation-error validation-error-label (fn [l] (if l
                                                                   [label l]
                                                                   label))))

(defn- augment-validation-failure-errors
  [validation-failure f]
  (make-validation-failure (mapv f (validation-failure-errors validation-failure))))

(defn- augment-validation-failure-error-messages
  [validation-failure message]
  (augment-validation-failure-errors validation-failure
                                     #(augment-validation-error-message % message)))

(defn sequence
  "Takes a vector of validation results ([[ValidationSuccess]]
  or [[ValidationFailure]]) and returns a [[ValidationSuccess]] of all
  candidates as a vector iff all are [[ValidationSuccess]].
  Else it returns a [[ValidationFailure]] with all errors accumulated. "
  [validation-results]
  (if (empty? validation-results) 
    (make-validation-success validation-results)
    (apply validation (fn [& args] (vec args)) validation-results)))

;; Combinators

(defn sequence-of
  ;; TODO chose a better name that doesnt remind johannes of monads.

  "Takes a validation function and a sequence of candidates and
  validates each vandidate and returns the combined result.

  If any one validation validation fails, reutrns a
  `ValidationFailure`, containing _all_ failures.

  All failures' [[validation-error-label]]s are prepended with a tuple
  of `label` if present (otherwise, defaults to `::seq`) and the index
  of the value that could not be validated."
  [validation candidates & [label]]
  (if (empty? candidates)
    (make-validation-success candidates)
    (->> candidates
         (map-indexed
          (fn [idx candidate]
            (let [res (validation candidate)]
              (cond
                (validation-success? res)
                res

                (validation-failure? res)
                (lens/overhaul
                 res
                 validation-failure-errors
                 (partial mapv #(augment-validation-error-label % [(or label ::seq) idx])))))))
         (reduce seq-validation (pure-validation (curry-n vector (count candidates)))))))

(defn validate-choice
  "Takes a sequence of `validation` functions and a `candidate`
  and applies each validation function to the `candidate`.

  If exactly one validation succeeds, returns a [[ValidationSuccess]].
  Otherwise, returns a [[ValidationFailure]] with all failed
  validations.

  All failures' [[validation-error-label]]s are prepended with a tuple
  of `label` if present (otherwise, defaults to `::seq`) and the index
  of the value that could not be validated."
  [validators candidate & [label]]
  ;; Choice is interesting because we need to have exactly one
  ;; validator validate the candidate.
  (let [validation-results   (mapv (fn [validate] (validate candidate label)) validators)
        groups               (group-by validation-success? validation-results)
        successes            (get groups true)
        [failure & failures] (get groups false)]
    (if (= 1 (count successes))
      (first successes)
      (reduce mappend-validation-failure failure failures))))

(defn validate-all
  "Takes a sequence of `validations` and a `candidate` and applies all
  `validations` to `candidate` sequentially.  Collects either
  all [[ValidationFailure]]s or returns a [[ValidationSuccess]] for
  the candidate."
  [validations candidate & [label]]
  (reduce (fn [acc v]
            (let [res (v candidate label)]
              (cond
                (and (validation-success? acc)
                     (validation-success? res))
                acc
                
                (and (validation-failure? acc)
                     (validation-failure? res))
                (make-validation-failure (concat (validation-failure-errors acc)
                                                 (validation-failure-errors res)))
                (validation-failure? acc) acc

                (validation-failure? res) res)))
          (pure-validation candidate)
          validations))

(defn optional
  "Takes a validation function `validate` and returns a validation
  function that accepts what `validate` accepts plus `nil`."
  [validate]
  (fn [candidate & [label]]
    (if (nil? candidate)
      (make-validation-success candidate)
      (let [v (validate candidate label)]
        (cond
          (validation-success? v) v
          (validation-failure? v)
          (augment-validation-failure-error-messages v ::optional))))))

;; Some frequently used validators.

(defn make-validator
  "Takes a `candidate` value and a `predicate` the candidate will be
  applied to.  If the `(predicate candidate)` returns false, returns
  a [[ValidationFailure]] with `error-message` as
  the [[ValidationError]], using `label` as the label if provided."
  [candidate predicate error-message & [label]]
  (if (predicate candidate)
    (make-validation-success candidate)
    (make-validation-failure [(make-validation-error candidate error-message label)])))

(defn succeed
  "Validator that always succeeds."
  [candidate & [label]]
  (make-validator candidate (constantly true) ::any label))

(defn validate-string
  "Validates that a candidate is a String."
  [s & [label]]
  (make-validator s string? ::string label))

(defn validate-non-empty-string
  "Validates that a candidate is a non-empty String."
  [s & [label]]
  (make-validator s #(and (string? %) (seq %)) ::non-empty-string label))

(defn validate-int
  "Validates that a candidate is an integer."
  [i & [label]]
  (make-validator i int? ::int label))

(defn validate-pos-int
  "Validates that a candidate is a positive integer."
  [i & [label]]
  (make-validator i pos-int? ::pos-int label))

(defn validate-boolean
  "Validates that a candidate is a boolean"
  [b & [label]]
  (make-validator b (some-fn true? false?) ::boolean label))

(defn validate-keyword
  "Validates that a candidate is a boolean"
  [k & [label]]
  (make-validator k keyword? ::keyword label))

(defn validate-one-of
  "Validates that a candidate is exatly one of `elems`."
  [elems k & [label]]
  (let [s (into #{} elems)]
    (make-validator k #(contains? s %) [::one-of s] label)))

(defn validate-none-of
  "Validates that a candidate is anything except one of `elems`."
  [elems k & [label]]
  (let [s (into #{} elems)]
    (make-validator k #(not (contains? s %)) [::none-of s] label)))

(defn validate-list
  "Validates that a candidate is a list."
  [xs & [label]]
  (make-validator xs list? ::list label))

(defn validate-vector
  "Validates that a candidate is a vector."
  [xs & [label]]
  (make-validator xs vector? ::vector label))

(defn validate-map
  "Validates that a candidate is a map."
  [m & [label]]
  (make-validator m map? ::map label))

(defn validate-set
  "Validates that a candidate is a set."
  [s & [label]]
  (make-validator s set? ::set label))

(defn validate-sequential
  "Validates that a candidate is sequential."
  [s & [label]]
  (make-validator s sequential? ::sequential label))
