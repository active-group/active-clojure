(ns active.clojure.pretty-print
  (:require #?(:clj [active.clojure.record :as r]
               :cljs [active.clojure.cljs.record :as r :include-macros true])
            [active.clojure.sum-type :as st]
            [clojure.string :as string]
            [clojure.core :exclude [flatten empty]]))

(defmacro defrec
  "Convenience macro that allows fast definition of rtd-records.

  Usage:

    (defrec car [ps color])

  evaluates to

    (define-record-type car
      make-car
      car?
      [ps car-ps
       color car-color])"
  [name vec-of-fields]
  `(r/define-record-type ~name
     {:rtd-record? true
      :java-class? false}
     ~(symbol (str "make-" name))
     ~(symbol (str name "?"))
     ~(vec (mapcat (fn [field-name]
                     [field-name (symbol (str name "-" field-name))])
                   vec-of-fields))))

(defrec NIL [])
(defrec CONCAT [doc1 doc2])
(defrec NEST [i doc])
(defrec TEXT [s])
(defrec LINE [])
(defrec UNION [doc1 doc2])
(st/define-sum-type DOC DOC? [NIL CONCAT NEST TEXT LINE UNION])


(defn <> [x y] (make-CONCAT x y))
(defn nest [i x] (make-NEST i x))
(defn text [s] (make-TEXT s))
(defn line [] (make-LINE))

(defn empty [] (make-NIL))

(defrec Nil [])
(defrec Text [s doc])
(defrec Line [i doc])
(st/define-sum-type Doc Doc? [Nil Text Line])


(defn flatten
  "Flattens a document to a document of one line."
  [d]
  (st/match DOC d
            NIL?              (make-NIL)
            (make-CONCAT x y) (make-CONCAT (flatten x) (flatten y))
            (make-NEST i x)   (make-NEST i (flatten x))
            (make-TEXT x)     (make-TEXT x)
            LINE?             (make-TEXT " ")
            (make-UNION x y)  (flatten x)))

(defn group
  "Creates a union of the given document and its flatten form.

  Useful when the document can have but doesnt have to have a line break.
  Ex.: (group (line)) inserts a line when necessary."
  [x]
  (make-UNION (flatten x) x))

(defn fits? [w d]
  (let [d (if (delay? d) (force d) d)]
    (if (< w 0)
      false
      (st/match
       Doc d
       Nil?            true
       (make-Text s x) (fits? (- w (count s)) x)
       Line?           true))))


;; x and y are delayed for efficiency reasons
(defn better [w k x y]
  (if (fits? (- w k) (force x))
    (force x)
    (force y)))




(defn be [w k list-of-pairs]
  (if (empty? list-of-pairs)
    (make-Nil)

    (let [[[i d] & z] list-of-pairs]
      (st/match
       DOC d
       NIL?              (be w k z)
       (make-CONCAT x y) (be w k (apply list [i x] [i y] z))
       (make-NEST j x)   (be w k (cons [(+ i j) x] z))
       (make-TEXT s)     (make-Text s (delay (be w (+ k (count s)) z)))
       LINE?             (make-Line i (be w i z))
       (make-UNION x y)  (better w k
                                 (delay (be w k (cons [i x] z)))
                                 (delay (be w k (cons [i y] z))))))))

(defn best
  "Returns the best fitting document for a given doc.
`w` is the maximum width and `k` is the count of characters already placed on
the current line."
  [w k x]
  (be w k [[0 x]]))

(defn pretty
  "Returns the best fitting document for a given doc.
  `w` is the maximum width."
  [w x]
  (best w 0 x))

(defn layout
  "Converts a document to a string."
  [d]
  (let [d (if (delay? d) (force d) d)]
    (st/match
     Doc d
     Nil? ""
     (make-Text s doc) (str s (layout doc))
     (make-Line i doc) (str (apply str "\n"(repeat i " "))
                            (layout doc)))))


;;; Convenience functions

(defn <+>
  "Concats two documents with a space inbetween."
  [x y]
  (<> x
      (<> (text " ") y)))


;; In the paper, this combinator is called `</>`,
;; but this is not a possible variable name in Clojure
(defn <->
  "Concats two documents with a linebreak inbetween."
  [x y]
  (<> x
      (<> (line) y)))

(defn folddoc
  "Folds a list of documents with given function f."
  [f docs]
  (if-let [[x & xs] (not-empty docs)]
    (if (empty? xs)
      x
      (f x (folddoc f xs)))
    (empty)))

(defn spread
  "Concats all docs of `ds` with space inbetween"
  [ds]
  (folddoc <+> ds))

(defn stack
  "Concats all docs of `ds` with linbreak inbetween"
  [ds]
  (folddoc <-> ds))

(defn bracket
  "Puts brackets `l` and `r` around document `x`.
  Indents the document `x` by 2 characters."
  [l x r]
  (<> (group (<> (text l)
                 (nest 2 (<> (line)
                             x))))
      (<> (group (line))
          (text r))))

(defn <+->
  "Concats two documents with linbreak or space."
  [x y]
  (<> x
      (<> (group (line))
          y)))

(def fillwords
  "Takes a string of words and concats them (via `<+->`) as `TEXT`s."
  (comp (partial folddoc <+->)
        (partial map text)
        #(string/split % #" ")))

(defn fill
  "Concats a list of documents with linbreaks or spaces inbetween"
  [docs]
  (if-let [[x & ds] (not-empty docs)]
    (if-let [[y & zs] (not-empty ds)]
      (make-UNION (<+> (flatten x)
                       (fill (cons (flatten y) zs)))
                  (<-> x
                       (fill ds)))
      x)
    (empty)))
