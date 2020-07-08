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


(r/define-record-type NIL
  {:rtd-record? true
   :java-class? false}
  make-NIL NIL? [])
(r/define-record-type CONCAT
  {:rtd-record? true
   :java-class? false}
  make-CONCAT CONCAT?
  [DOC1 CONCAT-DOC1
   DOC2 CONCAT-DOC2])
(r/define-record-type NEST
  {:rtd-record? true
   :java-class? false}
  make-NEST NEST?
  [indent NEST-indent
   DOC NEST-DOC])
(r/define-record-type TEXT
  {:rtd-record? true
   :java-class? false}
  make-TEXT TEXT?
  [string TEXT-string])
(r/define-record-type LINE
  {:rtd-record? true
   :java-class? false}
  make-LINE LINE? [])
(r/define-record-type UNION
  {:rtd-record? true
   :java-class? false}
  make-UNION UNION?
  [DOC1 UNION-DOC1
   DOC2 UNION-DOC2])
(st/define-sum-type DOC DOC? [NIL CONCAT NEST TEXT LINE UNION])


(defn <> [DOC1 DOC2] (make-CONCAT DOC1 DOC2))
(defn nest [indent DOC] (make-NEST indent DOC))
(defn text [string] (make-TEXT string))
(defn line [] (make-LINE))

(defn empty [] (make-NIL))

(r/define-record-type Nil
  {:rtd-record? true
   :java-class? false}
  make-Nil Nil? [])
(r/define-record-type Text
  {:rtd-record? true
   :java-class? false}
  make-Text Text?
  [string Text-string
   Doc Text-Doc])
(r/define-record-type Line
  {:rtd-record? true
   :java-class? false}
  make-Line Line?
  [indent Line-indent
   Doc Line-Doc])
(st/define-sum-type Doc Doc? [Nil Text Line])


(defn flatten
  "Flattens a document to a document of one line."
  [doc]
  (st/match DOC doc
    NIL?                    (make-NIL)
    (make-CONCAT doc1 doc2) (make-CONCAT (flatten doc1) (flatten doc2))
    (make-NEST indent doc)  (make-NEST indent (flatten doc))
    (make-TEXT string)      (make-TEXT string)
    LINE?                   (make-TEXT " ")
    (make-UNION doc1 doc2)  (flatten doc1)))

(defn group
  "Creates a union of the given document and its flatten form.

  Useful when the document can have but doesnt have to have a line break.
  Ex.: (group (line)) inserts a line when necessary."
  [doc]
  (make-UNION (flatten doc) doc))

(defn fits? [width delayed-doc]
  (let [doc (force delayed-doc)]
    (if (< width 0)
      false
      (st/match Doc doc
        Nil?                       true
        (make-Text string delayed) (fits? (- width (count string)) delayed)
        Line?                      true))))


;; The two docs are delayed for efficiency reasons
(defn better [width chars-on-line delayed-doc1 delayed-doc2]
  (if (fits? (- width chars-on-line) delayed-doc1)
    (force delayed-doc1)
    (force delayed-doc2)))

(def zähler (atom 0))




(defn be [width chars-on-line list-of-pairs]
  (swap! zähler inc)
  (if (empty? list-of-pairs)
    (make-Nil)

    (let [[[indent doc] & rest] list-of-pairs]
      (st/match DOC doc
        NIL?                     (be width chars-on-line rest)
        (make-CONCAT doc1 doc2)  (be width chars-on-line (apply list [indent doc1] [indent doc2] rest))
        (make-NEST indent-2 doc) (be width chars-on-line (cons [(+ indent indent-2) doc] rest))
        (make-TEXT string)       (make-Text string (delay (be width (+ chars-on-line (count string)) rest)))
        LINE?                    (make-Line indent (be width indent rest))
        (make-UNION doc1 doc2)   (better width chars-on-line
                                         (delay (be width chars-on-line (cons [indent (force doc1)] rest)))
                                         (delay (be width chars-on-line (cons [indent (force doc2)] rest))))))))

(defn best
  "Returns the best fitting document for a given doc.
  `width` is the maximum width and `chars-on-line` is the count of characters already placed on
  the current line."
  [width chars-on-line doc]
  (be width chars-on-line [[0 doc]]))

(defn pretty
  "Returns the best fitting document for a given doc.
  `width` is the maximum width."
  [width doc]
  (best width 0 doc))

(defn layout
  "Converts a document to a string."
  [doc]
  (st/match Doc doc
    Nil?  ""
    (make-Text string delayed-doc) (str string (layout (force delayed-doc)))
    (make-Line indent doc) (str (apply str "\n"(repeat indent " "))
                                (layout doc))))


;;; Convenience functions

(defn <+>
  "Concats two documents with a space inbetween."
  [doc1 doc2]
  (<> doc1
      (<> (text " ") doc2)))


;; In the paper, this combinator is called `</>`,
;; but this is not a possible variable name in Clojure
(defn <->
  "Concats two documents with a linebreak inbetween."
  [doc1 doc2]
  (<> doc1
      (<> (line) doc2)))

(defn folddoc
  "Folds a list of documents with given function f."
  [f docs]
  (if-let [[doc & rest] (not-empty docs)]
    (if (empty? rest)
      doc
      (f doc (folddoc f rest)))
    (empty)))

(defn spread
  "Concats all docs of the `docs` list with space inbetween to one doc."
  [docs]
  (folddoc <+> docs))

(defn stack
  "Concats all docs of the `docs` list with linbreak inbetween to one doc."
  [docs]
  (folddoc <-> docs))

(defn bracket
  "Puts brackets `left` and `rght` around document `doc`.
  Indents the document `doc` by 2 characters."
  [left doc right]
  (group (<> (<> (text left)
                 (nest 2 (<> (line)
                             doc)))
             (<> (line)
                 (text right)))))

(defn <+->
  "Concats two documents with linbreak or space."
  [doc1 doc2]
  (<> doc1
      (<> (group (line))
          doc2)))

(def fillwords
  "Takes a string of words and concats them (via `<+->`) as `TEXT`s."
  (comp (partial folddoc <+->)
        (partial map text)
        #(string/split % #" ")))

(defn fill
  "Concats a list of documents with linbreaks or spaces inbetween"
  [docs]
  (if-let [[doc1 & rest] (not-empty docs)]
    (if-let [[doc2 & rest-2] (not-empty rest)]
      (make-UNION (delay (<+> (flatten doc1)
                              (fill (cons (flatten doc2) rest-2))))
                  (delay (<-> doc1
                              (fill rest))))
      doc1)
    (empty)))
