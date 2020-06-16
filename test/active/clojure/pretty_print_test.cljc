(ns active.clojure.pretty-print-test
  (:require [active.clojure.pretty-print :as pp :refer [<> nest text group line layout pretty empty
                                                        bracket <+> <-> spread stack <+-> fillwords fill]]
            #?(:clj [active.clojure.record :as r]
               :cljs [active.clojure.cljs.record :as r :include-macros true])
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])
            [active.clojure.sum-type :as st]))

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
                  (<> (line)
                      (nest (+ 1 (count (str (first ks)))) (show-map (first vs))))
                  (text (str (first vs))))
                (<> (group (line))
                    (show-tuples (rest ks) (rest vs))))))))

(defn show-map [m]
  (<> (text "{" )
      (<> (nest 1
                (<> (group (line))
                    (show-tuples (keys m) (vals m))))
          (text "}"))))


(def big-map (let [l [:a :b :c :d :e :f :g :h :i :j :k :l :m :n :o :p :q :r :s :t]
                   size (count l)]
               (zipmap
                l
                (repeat
                 (zipmap l
                         (take size (range)))))))

(def my-big-map
  {:a {:a {:a 0 :b 1 :c 2 :d 3}
       :b {:a 0 :b 1 :c 2 :d 3}
       :c {:a 0 :b 1 :c 2 :d 3}}
   :b {:a 0 :b 1 :c 2 :d 3}
   :c {:a 0 :b 1 :c 2 :d {:a {:a 0 :b 1 :c 2 :d 3}
                          :b {:a 0 :b 1 :c 2 :d 3}
                          :c {:a 0 :b 1 :c 2 :d 3}}}})

;; Teste laziness
(do (reset! pp/zähler 0)
    (println (layout (pretty 50 (show-map my-big-map))))
    (println "Anzahl Durchgänge " @pp/zähler))


;; Vergleiche mit eingebautem pretty-print
(do
  (time (clojure.pprint/pprint big-map))
  (time (println (layout (pretty 50 (show-map big-map))))))

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
  (if-let [[tuple & tuples] (not-empty field-tuples)]
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

(pp/defrec auto [farbe ps flup])

(def a1 (make-auto "blau" {:die :ps :sind :ja :krass :viele}
                   (make-auto "yellow" 100 :xxyz)))

(println (layout (pretty 80 (show-rtd-record a1))))

;;; XML Example

(pp/defrec Elt [s atts xmls])
(pp/defrec Txt [s])
(pp/defrec Att [k v])
(st/define-sum-type XML XML? [Elt Txt])

(defn quoted
  [v]
  (<> (text "\"")
      (<> (text v)
          (text "\""))))

(defn show-att
  [att]
  [(<>
    (text (Att-k att))
    (<> (text "=")
        (quoted (Att-v att))))])

(defn show-fill
  [f ds]
  (if (empty? ds)
      (empty)
      (bracket "" (fill (apply concat (map f ds))) "")))

(defn show-tag
  [s atts]
  (<> (text s)
      (show-fill show-att atts)))

(defn show-XMLs
  [x]
  (st/match XML x
            (make-Elt s atts xmls) (if (empty? xmls)
                                     [(<+-> (<> (text "<")
                                                (show-tag s atts))
                                            (text "/>"))]
                                     [(<> (text "<")
                                          (<> (show-tag s atts)
                                              (<> (text ">")
                                                  (<> (show-fill show-XMLs xmls)
                                                      (<> (text "</")
                                                          (<> (text s)
                                                              (text ">")))))))])
            (make-Txt s) (map text (clojure.string/split s #" "))))

(defn show-XML
  [x]
  (pp/folddoc pp/<> (show-XMLs x)))


(def the-xml
  (make-Elt "p"
            [(make-Att "color" "red")
             (make-Att "font" "Times")
             (make-Att "size" "10")]
            [(make-Txt "Here is some")
             (make-Elt "em" [] [(make-Txt "emphasized")])
             (make-Txt "text. Here is a")
             (make-Elt "a"
                       [(make-Att "href" "http://ww.eg.com")]
                       [(make-Txt "link")])
             (make-Txt "elsewhere")]) )

(println (layout (pretty 60 (show-XML the-xml))))
