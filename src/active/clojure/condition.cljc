(ns active.clojure.condition
  "Conditions, protocol for communicating the causes of exceptions.

  This provides infrastructure for building *condition* objects.
  A condition object provides information about the cause of an exception.
  Conditions thus form a protocol.

  Clojure `ex-info` objects do not, by themselves, enough to form such
  a protocol, as they do not allow classifying an exception easily.

  This condition system builds on the design of
  [R6RS Scheme](http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-8.html#node_sec_7.2).

  Condition objects are represented as specially marked `ex-info` objects.

  One notable difference to the R6RS design is that there is no user-facing type for
  'simple conditions', nor are they regular records."
  (:refer-clojure :exclude (assert))
  #?(:clj (:require [clojure.core :as core] ; get assert back
                    [clojure.stacktrace :as stack]
                    [clojure.main :as main]))
  #?(:clj (:require [io.aviso.exception :as aviso-exception]
                    [io.aviso.columns :as aviso-columns]
                    [io.aviso.ansi :as aviso-ansi]
                    [clojure.string :as string]
                    [active.clojure.macro :refer (if-cljs)]))
  #?(:cljs (:require-macros [active.clojure.condition 
                             :refer (define-condition-type assert condition raise guard throw-condition)]
                            [active.clojure.macro :refer (if-cljs)]
                            [cljs.core :as core]))
  #?(:clj (:import clojure.lang.ExceptionInfo)))

(defn condition?
  [x]
  (and #?(:clj (instance? ExceptionInfo x))
       (contains? (ex-data x) ::condition)))

;; Printing conditions in ClojureScript:
;;
;; TODO `print-method` does not exist in ClojureScript and there is no
;; other way to define how an object should print itself.  For now,
;; you have to call `print-condition` yourself.
;;
;; TODO `*ns*` does not exist in ClojureSript.  Therefore, it would be
;; nice to have at least file name and line number information stored
;; in `ex-info-msg`, but this does not work:
;;
;; (let [e (new js/Error "message" "file" 23)]
;;   (println (.-message e))
;;   (println (.-fileName e))
;;   (println (.-lineNumber e)))
;;
;; as it prints
;;
;; message
;; nil
;; nil
;;
;; TODO Also, there is no run-time-independent way to get a stack
;; trace in ClojureScript.

#?(:clj (declare print-condition))
#?(:clj
(defmethod print-method ExceptionInfo [^ExceptionInfo exc ^java.io.Writer w]
  (if (condition? exc)
    (print-condition exc w)
    (.write w (str "clojure.lang.ExceptionInfo: " (.getMessage exc) " " (str (ex-data exc)))))))

(defn ^:private ex-info-msg 
  [namespace]
  (str "This is a " namespace " active.clojure.condition."))

(defrecord ConditionType
    [name supertype field-names total-field-count])

#?(:clj
(defmethod print-method ConditionType [^ConditionType ct ^java.io.Writer w]
  (.write w (name (:name ct)))))

(def ^{:doc "Root of the condition-type hierarchy."}
  &condition
  (ConditionType. '&condition nil [] 0))

(defrecord ConditionComponent
    [type arguments])

#?(:clj
(defmethod print-method ConditionComponent [^ConditionComponent cc ^java.io.Writer w]
  (print-method (:type cc) w)
  (.write w ": ")
  (print-method (:arguments cc) w)))

(defn- condition-component?
  "Is object a condition component?"
  [x]
  (instance? ConditionComponent x))

(defn- condition-subtype?
  "Is `ty1` subtype of `ty2`?"
  [ty1 ty2]
  (or (= ty1 ty2)
      (if-let [st (:supertype ty1)]
        (condition-subtype? st ty2)
        false)))

(defn- condition-component-of-condition-type?
  "Does condition component `comp` have condition type `type`?"
  [type comp]
  (condition-subtype? (:type comp) type))

(defn- condition-components
  "Extract components from condition."
  [cond]
  {:pre [(condition? cond)]}
  (::components (ex-data cond)))

(declare &message)

(defn make-condition
  "Make a condition from components.

  For internal use only."
  [condition-components]
  (ex-info (or (some (fn [cmp]
                       (and (condition-component-of-condition-type? &message cmp)
                            (first (:arguments cmp))))
                     condition-components)
               #?(:clj (ex-info-msg *ns*))
               #?(:cljs (ex-info-msg "<cljs>")))
           {::condition true
            ::components condition-components}))

#?(:clj
(defn stack-trace-who
  "Get a suitable argument for [[&who]] from an exception object."
  [st]
  ;; the top is the let, above that it's stack-trace-who itself
  (if-let [^StackTraceElement e (nth st 2)]
    (let [class (.getClassName e)
          method (.getMethodName e)]
      ;; adapted from clojure.stacktrace
      (let [match (re-matches #"^([A-Za-z0-9_.-]+)\$(\w+)$" (str class))]
        (if (and match (= "invoke" method))
          (apply format "%s/%s" (rest match))
          (format "%s.%s" class method)))))))

(declare make-throwable make-error make-assertion-violation make-message-condition make-who-condition)

#?(:clj
(defn ->condition
  "Coerce something into a condition object.

  This yields `thing` if it's already a condition,
  and an [[&exception]] condition if it's not."
  [thing]
  (cond
   (condition? thing) thing

   (instance? Throwable thing)
   (let [^Throwable throwable thing]
     (make-condition (mapcat condition-components
                             (filter identity
                                     [(make-throwable throwable)
                                      (make-message-condition (.getMessage throwable))
                                      (cond
                                       (instance? Exception throwable) (make-error)
                                       (instance? Error throwable) (make-assertion-violation)
                                       :else nil)
                                      (make-who-condition (stack-trace-who (.getStackTrace throwable)))]))))

   :else (clojure.core/assert (instance? Throwable thing) "not a throwable"))))

#?(:cljs
(defn ->condition
  "Coerce something into a condition object.

  This yields `thing` if it's already a condition,
  and an [[&exception]] condition if it's not."
  [thing]
  (if (condition? thing)
    thing
    (make-throwable thing))))

(defn combine-conditions
  "Make a compound condition from constituents.

  `nil` and `false` arguments are ignored."
  [& component-conditions]
  (make-condition (mapcat (comp condition-components ->condition) (filter identity component-conditions))))

(defn condition-of-type?
  "Does condition `cond` have type `type`?"
  [type cond]
  (some (partial condition-component-of-condition-type? type) (::components (ex-data cond))))

(defn condition-predicate
  "Make predicate from condition type."
  [type]
  (fn [x]
    (and (condition? x)
         (condition-of-type? type x))))

#?(:cljs
(defn index-of [coll v]
  (let [i (count (take-while #(not= v %) coll))]
    (when (or (< i (count coll))
              (= v (last coll)))
      i))))

(defn- field-index
  "Compute the field index of field named `field-name` for type `type`."
  [type field-name]
  (let [#?(:clj ^java.util.List fn)
        #?(:cljs fn) (:field-names type)
        local-index (#?(:clj .indexOf) #?(:cljs index-of) fn field-name)]
    (core/assert (not= -1 local-index))
    (if-let [supertype (:supertype type)]
      (+ (:total-field-count supertype)
         local-index)
      local-index)))

#?(:cljs
(def Error js/Error))

(defn condition-accessor
  "Create an an accessor for `field-name` for conditions of type `type`."
  [type field-name]
  (let [i (field-index type field-name)]
    (fn [cond]
      {:pre [(condition? cond)
             (condition-of-type? type cond)]}
      (if-let [comp
               (some (fn [comp]
                       (and (condition-component-of-condition-type? type comp)
                            comp))
                     (::components (ex-data cond)))]
        (nth (:arguments comp) i)
        (throw (new Error (str cond " is not a condition of type " type)))))))

#?(:clj
(defmacro define-condition-type
  "    (define-condition-type <condition-type>
        <supertype>
        <constructor> <predicate>
        [<field> <accessor> ...)

  `<Condition-type>`, `<supertype>`, `<constructor>`, and
  `<predicate>` must all be identifiers. Each `<field>` and `<accessor>`
  must be identifiers.

  The `define-condition-type` form defines a condition type named
  `<condition-type>`.  It will have <supertype> has its parent type. The
  remaining identifiers will be bound as follows:

  `<Constructor>` is bound to a constructor for the type: It accepts
  one argument for each of the condition type's complete set of fields
  (including parent types, with the fields of the parent coming before
  those of the extension in the arguments) and returns a condition
  object initialized to those arguments.

  `<Predicate>` is bound to a predicate that identifies conditions of
  type `<condition-type>` or any of its subtypes.

  Each `<accessor>` is bound to a function that extracts the
  corresponding field from a condition of type `<condition-type>`.

  `:doc` properties attached to the metadata of `<condition-type>` and
  `the <field>s will yield proper docstrings for the names defined by this form."
  [?condition-type ?supertype ?constructor ?predicate & ?field-specs]
  (let [?field-pairs (if (and (= 1 (count ?field-specs))
                              (vector? (first ?field-specs)))
                       (partition 2 (first ?field-specs))
                       ;; legacy syntax
                       ?field-specs)
        document (fn [n doc]
                   (vary-meta n
                              (fn [m]
                                (if (contains? m :doc)
                                  m
                                  (assoc m :doc doc)))))
        document-with-arglist (fn [n arglist doc]
                                (vary-meta n
                                           (fn [m]
                                             (let [m (if (contains? m :doc)
                                                       m
                                                       (assoc m :doc doc))]
                                               (if (contains? m :arglists)
                                                 m
                                                 (assoc m :arglists `'(~arglist)))))))
        name-doc (fn [field]
                    (if-let [doc (:doc (meta field))]
                      (str " (" doc ")")
                      ""))
        reference (fn [name]
                    (str "[[" (ns-name *ns*) "/" name "]]"))
        ?docref (str "See " (reference ?condition-type) ".")]
    `(do
       (let [total-field-count# (+ ~(count ?field-pairs) (:total-field-count ~?supertype))]
         (def ~(vary-meta ?condition-type
                          (fn [m]
                            (assoc m :doc
                                   (str (or (get m :doc)
                                            "Condition type.")
                                        "\n"
                                        (apply str
                                               (map (fn [[?field ?accessor]]
                                                      (str "\n`" ?field "`" (name-doc ?field) ": access via " (reference ?accessor)))
                                                    ?field-pairs))))))
           (->ConditionType '~(symbol (str *ns*) (name ?condition-type)) ~?supertype 
                            '[~@(map first ?field-pairs)] total-field-count#))
         (def ~(document ?constructor 
                         (str "Construct a " (reference ?condition-type) " condition."))
           (fn [& args#]
             {:pre [(= (count args#) total-field-count#)]}
             (make-condition [(->ConditionComponent ~?condition-type (vec args#))])))
         (def ~(document-with-arglist ?predicate '[thing]
                                      (str "Is object a " (reference ?condition-type) " condition?"))
           (condition-predicate ~?condition-type))
         ~@(map (fn [[?field-name ?accessor]]
                  `(def ~(document-with-arglist ?accessor
                                                (vector ?condition-type)
                                                (str "Access the `" ?field-name "`" (name-doc ?field-name)
                                                     " field from a " (reference ?condition-type) " condition."))
                     (condition-accessor ~?condition-type '~?field-name)))
                ?field-pairs))))))

; These standard condition types correspond directly to R6RS Scheme

(define-condition-type ^{:doc "Human-reaable message."} &message &condition
  make-message-condition message-condition?
  [^{:doc "message text"} message condition-message])

(define-condition-type ^{:doc "Non-fatal warning."} &warning &condition
  make-warning warning?
  [])

(define-condition-type ^{:doc "Serious condition that should not be ignored."} &serious &condition
  make-serious-condition serious-condition?
  [])

(define-condition-type ^{:doc "Error from environment, not preventable by the program."} &error &serious
  make-error error?
  [])

(define-condition-type ^{:doc "Bug in the program."} &violation &serious
  make-violation violation?
  [])

(define-condition-type ^{:doc "Violation of a specific assertion."}  &assertion &violation
  make-assertion-violation assertion-violation?
  [])

(define-condition-type ^{:doc "Condition with objects providing more information about an exceptional situation."} &irritants &condition
  make-irritants-condition irritants-condition?
  [^{:doc "objects providing more information"} irritants condition-irritants])

(define-condition-type ^{:doc "Information about in what entity an exceptional situation occurred."} &who &condition
  make-who-condition who-condition?
  [^{:doc "name of the entity"} who condition-who])

(define-condition-type ^{:doc "Location information about an exceptional situation"} &location &condition
  make-location-condition location-condition?
  ;; any of these can be nil
  [^{:doc "namespace of the location"} namespace location-condition-namespace
   ^{:doc "file name of the location"} file location-condition-file
   ^{:doc "line number of the loocation"} line location-condition-line])

(define-condition-type
  ^{:doc "Throwable value that's not a condition."} &throwable &serious
  make-throwable throwable?
  [^{:doc "Throwable object"} value throwable-value])

#?(:clj
(defmacro throw-condition
  "Throw a condition.

  For internal use."
  [?base ?who ?message ?irritants]
  `(let [g# (group-by (fn [thing#] (or (condition? thing#) (instance? Throwable thing#))) ~?irritants)
         irritants# (get g# false)
         conditions# (get g# true)
         who# ~?who]
     (throw (apply combine-conditions
                   ~?base
                   (and who# (make-who-condition who#))
                   (make-message-condition ~?message)
                   (and (not-empty irritants#) (make-irritants-condition irritants#))
                   conditions#)))))

(defn error 
  "Throw an exception that signals that an error has occurred.

  This function should be called when an error has occurred,
  typically caused by something that has gone wrong in the interaction
  of the program with the external world or the user.

  The `irritants` arguments can be conditions, in which case they're included
  in the resulting condition, or non-conditions, which are included in an
  [[&irritants]] condition."
  [who message & irritants]
  (throw-condition (make-error) who message irritants))

(defn assertion-violation
  "Throw an exception that signals that an assertion was violated.

  This should be called when an invalid call to a procedure was made,
  either passing an invalid number of arguments, or passing an argument
  that it is not specified to handle.

  The `irritants` arguments can be conditions, in which case they're included
  in the resulting condition, or non-conditions, which are included in an
  [[&irritants]] condition.

  On Java, `irritants` can also include ``Throwable`` objects."
  [who message & irritants]
  (throw-condition (make-assertion-violation) who message irritants))

#?(:clj
(defmacro assert
  "Evaluates expr and throws an exception if it does not evaluate to
  logical true."
  ([x]
     (when *assert*
       (let [?ns (str *ns*)
             ?file  (let [f *file*] (when (not= f "NO_SOURCE_PATH") f))
             ;; TODO Waiting on http://dev.clojure.org/jira/browse/CLJ-865:
             ?line  (:line (meta &form))]
         `(when-not ~x
            (assertion-violation (if-cljs
                                   nil
                                   (stack-trace-who (.getStackTrace (Thread/currentThread))))
                                 (str "Assertion failed")
                                 (make-location-condition '~?ns ~?file ~?line)
                                 '~x)))))
  ([x message]
     (when *assert*
       (let [?ns (str *ns*)
             ?file  (let [f *file*] (when (not= f "NO_SOURCE_PATH") f))
             ;; TODO Waiting on http://dev.clojure.org/jira/browse/CLJ-865:
             ?line  (:line (meta &form))]
         `(when-not ~x
            (assertion-violation (if-cljs
                                  nil
                                  (stack-trace-who (.getStackTrace (Thread/currentThread))))
                                 (str "Assert failed: " ~message)
                                 (make-location-condition '~?ns ~?file ~?line)
                                 '~x)))))))


#?(:clj
(defmacro condition
  [?base ?message & ?irritants]
  `(combine-conditions ~?base
                       (if-cljs
                        nil
                        (make-who-condition (stack-trace-who (.getStackTrace (Thread/currentThread)))))
                       (make-message-condition ~?message)
                       (make-irritants-condition [~@?irritants]))))

#?(:clj
(defmacro raise
  [?base ?message & ?irritants]
  `(throw (condition ~?base ~?message ~@?irritants))))

#?(:cljs
(def Throwable js/Object))

#?(:clj
(defmacro guard
  [?handling & ?body]
  (when-not (vector? ?handling)
    (throw (IllegalArgumentException. (str "guard requires vector in " *ns* " " (meta &form)))))
  (when-not (odd? (count ?handling))
    (throw (IllegalArgumentException. (str "guard requires vector in " *ns* " " (meta &form)))))
  (let [?id (first ?handling)]
    `(try
       ~@?body
       ; If Throwable is not a symbol it means java.lang.Throwable which does not work in ClojureScript.
       (catch ~'Throwable ~?id
         (cond
          ~@(rest ?handling)
          ~@(if (= :else (last (butlast ?handling)))
              `()
              `(:else (throw ~?id)))))))))

(defn delete-first
  [pred? l]
  (letfn [(recurse [l]
            (cond
             (empty? l) l
             (pred? (first l)) (rest l)
             :else
             (cons (first l) (recurse (rest l)))))]
    (recurse l)))

;; Turning this into a constant def screws things up somehow.
(defn covered-condition-types
  []
  #{&error &assertion &serious &violation})

(defn decode-condition
  "Return a keyword describing the type,
  a symbol or string describing the source of the problem, an error
  message or nil, and a sequence of other objects describing the
  problem.  

  Valid type symbols include: `:error`, `:assertion-violation`,
  `:violation`, `:serious`."
  [con]
  ;; adapted from Scheme 48
  (let [type (cond
              (error? con) :error
              (assertion-violation? con) :assertion-violation
              (violation? con) :violation
              (serious-condition? con) :serious
              :else 'unknown)

	who (and (who-condition? con)
                 (condition-who con))

	message (and (message-condition? con)
                     (condition-message con))

	stuff (if (irritants-condition? con)
                (condition-irritants con)
                [])

        more-stuff (delete-first
                    (fn [comp] ; make sure interesting subtypes still get printed
                      (contains? (covered-condition-types)
                                 (:type comp)))
                    ;; we don't expect interesting subtypes here
                    (delete-first
                     (partial condition-component-of-condition-type? &message)
                     (delete-first
                     (partial condition-component-of-condition-type? &who)
                      (delete-first
                       (partial condition-component-of-condition-type? &irritants)
                       (condition-components con)))))]

    [type who message (concat stuff more-stuff)]))

#?(:clj
(defn- preformat-stack-frame
  [frame]
  (cond
    (:omitted frame)
    (assoc frame :formatted-name "..."
                 :file ""
                 :line nil)

    ;; When :names is empty, it's a Java (not Clojure) frame
    (-> frame :names empty?)
    (let [formatted-name      (str (:class frame) "." (:method frame))]
      (assoc frame
        :formatted-name formatted-name))

    :else
    (let [names          (:names frame)
          formatted-name (str
                           (->> names drop-last (string/join "/"))
                           "/"
                           (last names))]
      (assoc frame :formatted-name formatted-name)))))

#?(:clj
(defn print-stack-trace-of
  [writer exception]
  (let [elements (map preformat-stack-frame (aviso-exception/expand-stack-trace exception))]
    (aviso-columns/write-rows writer [:formatted-name
                                      "  "
                                      :file
                                      [#(if (:line %) ": ") :left 2]
                                      #(-> % :line str)]
                              elements))))

#?(:clj
(defn print-condition
  [c ^java.io.Writer w]
  (binding [*out* w]
    (let [[type who message stuff] (decode-condition c)]
      (doto w
        (.write (name type))
        (.write ": "))
      (cond
       (nil? message) (print "<no message>")
       (string? message) (let [^String s message]
                           (.write w s))
       :else (print message))
      (let [^String spaces
            (apply str (repeat (+ (count (name type)) 2)
                               \space))]
        (when who
          (.write w " [")
          (print who)
          (.write w "]"))
        (binding [*print-length* 12]
          (doseq [irritant stuff]
            (doto w
              (.write "\n")
              (.write spaces))
            (pr irritant))
          (.write w "\n"))))
    (print-stack-trace-of w c))))

#?(:cljs
(defn print-condition
  [c]
  (let [[type who message stuff] (decode-condition c)]
    (print (name type))
    (print ": ")
      (cond
       (nil? message) (print "<no message>")
       (string? message) (print message)
       :else (print message))
    (let [spaces
          (apply str (repeat (+ (count (name type)) 2)
                             \space))]
      (when who
        (print " [")
        (print who)
        (print "]"))
      (doseq [irritant stuff]
        (print "\n")
        (print spaces)
        (pr irritant))
      (print "\n")))))

