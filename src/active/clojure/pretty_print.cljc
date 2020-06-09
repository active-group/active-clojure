(ns active.clojure.pretty-print
  (:require #?(:clj [active.clojure.record :as r]
               :cljs [active.clojure.cljs.record :as r :include-macros true])
            [active.clojure.sum-type :as st]
            [clojure.string :as string]
            [clojure.core :exclude [flatten empty]]))

(defmacro defrec
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


(defn flatten [d]
  (st/match DOC d
            NIL?              (make-NIL)
            (make-CONCAT x y) (make-CONCAT (flatten x) (flatten y))
            (make-NEST i x)   (make-NEST i (flatten x))
            (make-TEXT x)     (make-TEXT x)
            LINE?             (make-TEXT " ")
            (make-UNION x y)  (flatten x)))

(defn group [x] (make-UNION (flatten x) x))


(defn fits? [w d]
  (if (< w 0)
    false
    (st/match
     Doc d
     Nil?            true
     (make-Text s x) (fits? (- w (count s)) x)
     Line?           true)))


;; better has to be a macro for efficiency
;; the second doc does not have to be evaluated, if the first is better
(defmacro better [w k x y]
  `(let [x# ~x]
     (if (fits? (- ~w ~k) x#)
       x#
       ~y)))

(defn be [w k list-of-pairs]
  (if (empty? list-of-pairs)
    (make-Nil)

    (let [[[i d] & z] list-of-pairs]
      (st/match
       DOC d
       NIL?              (be w k z)
       (make-CONCAT x y) (be w k (apply list [i x] [i y] z))
       (make-NEST j x)   (be w k (cons [(+ i j) x] z))
       (make-TEXT s)     (make-Text s (be w (+ k (count s)) z))
       LINE?             (make-Line i (be w i z))
       (make-UNION x y)  (better w k
                                 (be w k (cons [i x] z))
                                 (be w k (cons [i y] z)))))))

(defn best [w k x]
  (be w k [[0 x]]))

(defn pretty [w x]
  (best w 0 x))

(defn layout [d]
  (st/match
   Doc d
   Nil? ""
   (make-Text s doc) (str s (layout doc))
   (make-Line i doc) (str (apply str "\n"(repeat i " "))
                          (layout doc))))


;;; Convenience functions

(defn <+> [x y]
  (<> x
      (<> (text " ") y)))


;; In the paper, this combinator is called `</>`,
;; but this is not a possible variable name in Clojure
(defn <-> [x y]
  (<> x
      (<> (line) y)))

(defn folddoc [f docs]
  (if-let [[x & xs] docs]
    (if (empty? xs)
      x
      (f x (folddoc f xs)))
    (empty)))

(defn spread [ds] (folddoc <+> ds))

(defn stack [ds] (folddoc <-> ds))

(defn bracket [l x r] (<> (group (<> (text l)
                                     (nest 2 (<> (line)
                                                 x))))
                          (<> (line)
                              (text r))))

(defn <+-> [x y]
  (<> x
      (<> (make-UNION (text " ")
                      (line))
          y)))

(def fillwords
  (comp (partial folddoc <+->)
        (partial map text)
        #(string/split % #" ")))

(defn fill [docs]
  (if-let [[x & ds] docs]
    (if-let [[y & zs] ds]
      (make-UNION (<+> (flatten x)
                       (fill (cons (flatten y) zs)))
                  (<-> x
                       (fill ds)))
      x)
    (empty)))
