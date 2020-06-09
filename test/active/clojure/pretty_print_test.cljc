(ns active.clojure.pretty-print-test
  (:require [active.clojure.pretty-print :as pp :refer [<> nest text group line layout pretty empty
                                                        bracket <+> <-> spread stack <+-> fillwords fill]]
            #?(:clj [active.clojure.record :as r]
               :cljs [active.clojure.cljs.record :as r :include-macros true])
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])))

;; <+> example
(println (layout (pretty 20 (<+> (text "Hallo")
                                 (text "Du!")))))

;; <-> example
(println (layout (pretty 20 (<-> (text "Hallo")
                                 (text "Du!")))))

;; spread example
(println (layout (pretty 20 (spread [(text "Hallo") (text "Du,") (text "wie") (text "gehts?")]))))

;; stack example
(println (layout (pretty 20 (stack [(text "Hallo") (text "Du,") (text "wie") (text "gehts?")]))))

;; bracket example
(println (layout (pretty 5 (bracket "[" (<> (text "Hallo")
                                            (<> (line)
                                                (text  "Hallo"))) "]"))))

;; <+-> examples
(println (layout (pretty 20 (<+-> (text "Hallo")
                                  (text "Du!")))))
(println (layout (pretty 5 (<+-> (text "Hallo")
                                 (text "Du!")))))

;; fillwords example
(println (layout (pretty 12 (fillwords "Hallo Du wie gehts"))))

;; fill example
(println (layout (pretty 15 (fill [(fillwords "Hallo Du wie gehts")
                                   (fillwords "Gut!")
                                   (fillwords "Mir auch")]))))





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

(def my-big-map
  {:a {:a 0 :b 1 :c 2 :d 3}
   :b {:a 0 :b 1 :c 2 :d 3}
   :c {:a 0 :b 1 :c 2 :d 3}
   :d {:f 3}})

(def l (show-map my-big-map))

l

(println (layout (pretty 10 (show-map my-big-map))))
#_(println (layout (pretty 10 (show-map big-map))))


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

(declare show-rtd-record)

(defn show-fields
  [r field-tuples]
  (if-let [[tuple & tuples] field-tuples]
    (<> (text (str (first tuple)))
        (<> (text " ")
            (<>
             (let [value ((second tuple) r)]
               (if (r/rtd-record? value)
                 (show-rtd-record value)
                 (nest (+ 2 (count (str (first tuple))))
                       (fillwords (pr-str ((second tuple) r))))))
             (if tuples
               (<> (text ",")
                   (<> (group (line))
                       (show-fields r tuples)))
               (empty)))))
    (empty)))


(defn show-rtd-record
  [r]
  (nest 2(<> (text (str (r/get-type-from-record r)))
             (<> (text "{")
                 (<> (group (line))
                     (<> (show-fields r (r/get-field-tuples-from-record r))
                         (text "}")))))))

(r/define-record-type Auto
  {:java-class? false
   :rtd-record? true}
  make-auto
  auto?
  [farbe auto-farbe
   ps auto-ps
   flup auto-flup])


(def a1 (make-auto "blau" {:die :ps :sind :ja :krass :viele} (make-auto "yellow" 100 :xxyz)))

(println (layout (pretty 30 (show-rtd-record a1))))


