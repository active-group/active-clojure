(ns active.clojure.pretty-print-test
  (:require [active.clojure.pretty-print :as pp :refer [<> nest text group line layout pretty empty]]
            #?(:clj [active.clojure.record :as r]
               :cljs [active.clojure.cljs.record :as r :include-macros true])
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])))

;;; Example for maps
(declare show-map)

(defn show-tuples
  [ks vs]
  (if (empty? ks)
    (empty)
    (<> (text (str (first ks)))
        (<> (text ":")
            (<> (if (map? (first vs))
                  (nest (+ 1 (count (str (first ks)))) (show-map (first vs)))
                  (text (str (first vs))))
                (<> (group (line))
                    (show-tuples (rest ks) (rest vs))))))))

(defn show-map [m]
  (<> (text "{" )
      (<> (nest 1 (show-tuples (keys m) (vals m)))
          (text "}"))))



(def m {"aaa" {"bb" 1 "cccc" {"aa" 12}} "ddd" 24 "eeeee" 122 "ff" 12})

(def big-map (zipmap
              [:a :b :c :d :e]
              (repeat
               (zipmap [:a :b :c :d :e]
                       (take 5 (range))))))

(def l (show-map m))

(println (layout (pretty 20 l)))

#_(clojure.pprint/pprint big-map)


;;; Example for RTD-Records

(r/define-record-type DerGuteRecord
  {:rtd-record? true
   :java-class? false}
  make-der-gute-record
  der-gute-record?
  [eins der-gute-record-eins
   zwei der-gute-record-zwei
   drei der-gute-record-drei])


(def ex1 (make-der-gute-record "Hallo" "Na" "Was geht!?"))

(def ex2 (make-der-gute-record ex1 ex1 ex1))

(defn show-fields
  [r field-tuples]
  (if-let [[tuple & tuples] field-tuples]
    (<> (text (str (first tuple)))
        (<> (text " ")
            (<>
             (text (pr-str ((second tuple) r)))
             (if tuples
               (<> (text ", ")
                   (<> (line)
                       (show-fields r tuples)))
               (empty)))))
    (empty)))

(defn show-rtd-record
  [r]
  (<> (text (str (r/get-type-from-record r)))
      (<> (text "{")
          (<> (show-fields r (r/get-field-tuples-from-record r))
              (text "}")))))


(show-rtd-record ex1)

(println (layout (pretty 10 (show-rtd-record ex1))))


