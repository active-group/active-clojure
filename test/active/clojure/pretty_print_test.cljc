(ns active.clojure.pretty-print-test
  (:require [active.clojure.pretty-print :as pp :refer [<> nest text group line layout pretty empty
                                                        bracket <+> <-> spread stack <+-> fillwords fill]]
            #?(:clj [active.clojure.record :as r]
               :cljs [active.clojure.cljs.record :as r :include-macros true])
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])
            [active.clojure.sum-type :as st]))

;; <+> example
#_(println (layout (pretty 20 (<+> (text "Hallo")
                                 (text "Du!")))))

;; <-> example
#_(println (layout (pretty 20 (<-> (text "Hallo")
                                 (text "Du!")))))

;; spread example
#_(println (layout (pretty 20 (spread [(text "Hallo") (text "Du,") (text "wie") (text "gehts?")]))))

;; stack example
#_(println (layout (pretty 20 (stack [(text "Hallo") (text "Du,") (text "wie") (text "gehts?")]))))

;; bracket example
#_(println (layout (pretty 5 (bracket "[" (<> (text "Hallo")
                                            (<> (line)
                                                (text  "Hallo"))) "]"))))

;; <+-> examples
#_(println (layout (pretty 20 (<+-> (text "Hallo")
                                  (text "Du!")))))
#_(println (layout (pretty 5 (<+-> (text "Hallo")
                                 (text "Du!")))))

;; fillwords example
#_(println (layout (pretty 12 (fillwords "Hallo Du wie gehts"))))

;; fill example
#_(println (layout (pretty 15 (fill [(fillwords "Hallo Du wie gehts")
                                   (fillwords "Gut!")
                                   (fillwords "Mir auch")]))))



(declare show-object)

;;; Example for maps
(declare show-map)

(defn show-tuples
  [ks vs]
  (if (empty? ks)
    (empty)
    (<> (show-object (first ks))
          (<> (<> (group (line))
                  (<> (show-object (first vs))
                      (if (not-empty (rest ks))
                        (text ",")
                        (empty))))
              (<> (group (line))
                  (show-tuples (rest ks) (rest vs)))))))

(defn show-map [m]
  (<> (text "{" )
      (<> (nest 2
                (<> (group (line))
                    (show-tuples (keys m) (vals m))))
          (text "}"))))

;;; Example for RTD-Records
(declare show-rtd-record)

(defn show-fields
  [r field-tuples]
  (if-let [[tuple & tuples] (not-empty field-tuples)]
    (let [field-name (str (first tuple))
          value      ((second tuple) r)]
      (<+> (text field-name)
           (<> (nest (+ 1 (count field-name))
                     (show-object value))
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
                     (<+-> (show-fields r (r/get-field-tuples-from-record r))
                         (text "}")))))))

;;; show-string
(defn show-string
  [s]
  (<> (text "\"")
      (<> (fillwords s)
          (text "\""))))


;;; show list

(defn show-list
  [l]
  (fill (concat [(text "(")]
                (map show-object l)
                [(text ")")])))

;;; show vector
(defn show-vector
  [v]
  (fill (concat [(text "[")]
                (map show-object v)
                [(text "]")])))

;;; show any Clojure object

(defn show-object
  [obj]
  (cond
    (r/rtd-record? obj) (show-rtd-record obj)

    (number? obj) (text (str obj))

    (string? obj) (show-string obj)

    (keyword? obj) (text (str obj))

    (map? obj) (show-map obj)

    (list? obj) (show-list obj)

    (vector? obj) (show-vector obj)

    (nil? obj) (text "nil")

    (boolean? obj) (text (str obj))

    ;; :else (text (str obj))

    :else (do (println "This object has no own show-obj implementation.\n"
                       "Object: " obj " type: " (type obj))
              (text (str obj)))
    ))

;;; Tests

;; Maps
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
#_(do (reset! pp/zähler 0)
    (println (layout (pretty 50 (show-map my-big-map))))
    (println "Anzahl Durchgänge " @pp/zähler))

;; Vergleiche mit eingebautem pretty-print
#_(do
  (time (clojure.pprint/pprint big-map))
  (time (println (layout (pretty 50 (show-object big-map))))))

;; RTD Record
(pp/defrec auto [farbe ps flup])

(def a1 (make-auto "blau" {:die :ps :sind :ja :krass :viele}
                   (make-auto "yellow" 100 :xxyz)))

#_(println (layout (pretty 12 (show-object a1))))


(def ob {:flup [1 2 3 4 5 2 3 4 3 3 3 3 3 3]
         :diedup "hello how you doin? this is gonna be split up"
         :rtd (make-auto "lol" {:this "is" :a "Map"} (list "asdfane" 2 3 {:bdam 3} "einhundert"))
         :listy (list "asdfane" 2 3 {:bdam 3} "einhundert")})

#_(println (layout (pretty 30 (show-object ob))))


#_(do
  (println "\nPPRINT:")
  (time (clojure.pprint/pprint ob))
  (println "\n Prettier Printer")
  (reset! pp/zähler 0)
  (time (println (layout (pretty 30 (show-object ob)))))
  (println "Anzahl Durchgänge " @pp/zähler))



;;; ------------

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

#_(println (layout (pretty 60 (show-XML the-xml))))
