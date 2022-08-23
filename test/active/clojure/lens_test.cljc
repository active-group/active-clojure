(ns active.clojure.lens-test
  (:require [active.clojure.lens :as lens]
            #?(:clj [clojure.test :refer :all])
            #?(:cljs [cljs.test :as t]))
  #?(:cljs
  (:require-macros [cljs.test
                    :refer (is deftest run-tests testing)])))

#?(:cljs
(enable-console-print!))

(defn law-1-holds [l data v]
  ;; you get back what you put in
  (is (= v
         (lens/yank (lens/shove data l v) l))
      "Lens law 1 violated"))

(defn law-2-holds [l data]
  ;; putting back what you got doesn't change anything
  (is (= data
         (lens/shove data l (lens/yank data l)))
      "Lens law 2 violated"))

(defn law-3-holds [l data v1 v2]
  ;; second set wins, or setting once is the same as setting twice
  (is (= (lens/shove data l v1)
         (lens/shove (lens/shove data l v2) l v1))
      "Lens law 3 violated"))

(defn lens-laws-hold [l data v1 v2]
  (and (law-1-holds l data v1)
       (law-2-holds l data)
       (law-3-holds l data v1 v2)))

(deftest t-overhaul
  (is (= {:foo 24}
         (lens/overhaul {:foo 23} :foo inc)))
  (is (= {:foo 42}
         (lens/overhaul {:foo 23} :foo + 19)))
  (is (= {:foo 122}
         (lens/overhaul {:foo 123} (lens/member :foo) dec)))
  (is (= {:foo 120}
         (lens/overhaul {:foo 123} (lens/member :foo 1) - 3)))
  (is (= {:foo 123 :bar -2}
         (lens/overhaul {:foo 123} (lens/member :bar 1) - 3)))
  (is (= [1 4 3]
         (lens/overhaul [1 2 3] (lens/pos 1) + 2))))

(deftest void
  (lens-laws-hold lens/void {} nil nil)
  (is (= nil
         (lens/yank {} lens/void)))
  (is (= {}
         (lens/shove {} lens/void 42))))

(deftest as-map
  (lens-laws-hold lens/as-map [[1 2]] {:a 42} {:b 12})
  (is (= {:a 42}
         (lens/yank [[:a 42]] lens/as-map)))
  (is (= [[3 15]]
         (lens/shove [] lens/as-map {3 15}))))

(deftest as-set
  (lens-laws-hold lens/as-set '[1 2 3] #{:a :b} #{13 12})
  (is (= #{13 42}
         (lens/yank [13 42] lens/as-set)))
  (is (= [15]
         (lens/shove [7] lens/as-set #{15}))))

(deftest head
  (lens-laws-hold lens/head [1 2 3] 7 42)
  (lens-laws-hold lens/head [] nil 42)
  (is (= 13
         (lens/yank [13 42] lens/head)))
  (is (= nil
         (lens/yank [] lens/head)))
  (is (= [15]
         (lens/shove [7] lens/head 15)))
  (is (= [42]
         (lens/shove [] lens/head 42)))
  (is (= []
         (lens/shove [] lens/head nil))))

(deftest nel-head
  (lens-laws-hold lens/nel-head [1 2 3] 7 42)
  (is (= 13
         (lens/yank [13 42] lens/nel-head)))
  (is (= [15]
         (lens/shove [7] lens/nel-head 15)))
  (is (= [42]
         (lens/shove [] lens/nel-head 42)))
  (is (= [nil]
         (lens/shove [] lens/nel-head nil))))

(deftest tail
  (lens-laws-hold lens/tail [1 2 3] [7] [45])
  (lens-laws-hold lens/tail [] [] [42])
  (is (= [42]
         (lens/yank [13 42] lens/tail)))
  (is (= []
         (lens/yank [] lens/tail)))
  (is (= [7 15]
         (lens/shove [7] lens/tail [15])))
  (is (= [nil 15]
         (lens/shove [] lens/tail [15])))
  (is (= [15]
         (lens/shove [15] lens/tail nil)))
  (is (= [42]
         (lens/shove [42] lens/tail []))))

(deftest nel-tail
  (lens-laws-hold lens/nel-tail [1 2 3] [7] [45])
  (is (= [42]
         (lens/yank [13 42] lens/nel-tail)))
  (is (= [7 15]
         (lens/shove [7] lens/nel-tail [15])))
  (is (= [7 nil]
         (lens/shove [7 0] lens/nel-tail [nil])))
  (is (= [42]
         (lens/shove [42] lens/nel-tail []))))

(deftest member
  (let [l (lens/member 42)]
    (lens-laws-hold l {12 "a" 42 "b"} "c" "d")
    (is (= "b"
           (lens/yank {12 "a" 42 "b"} l)))
    (is (= nil
           (lens/yank {} l)))
    (is (= 0
           (lens/yank {} (lens/member 42 0))))
    (is (= {12 "a" 42 "b"}
           (lens/shove {12 "a"} l "b")))
    (is (= {}
           (lens/shove {42 "b"} l nil)))))

(deftest contains
  (let [l (lens/contains 42)]
    (lens-laws-hold l #{12 42} true false)
    (is (lens/yank #{12 42} l))
    (is (not (lens/yank #{} l)))
    (is (= #{13 42}
           (lens/shove #{13} l true)))
    (is (= #{}
           (lens/shove #{42} l false)))))

(deftest pos
  (let [l (lens/pos 1)]
    (lens-laws-hold l [12 42] 7 65)
    (lens-laws-hold l '(12 42) 7 65)
    
    (is (= 42
           (lens/yank [12 42] l)))
    (is (= 42
           (lens/yank '(12 42) l)))
    (is (= nil
           (lens/yank [] l)))
    (is (= nil
           (lens/yank nil l)))
    (is (= nil
           (lens/yank [10] l)))
    
    (is (= [[nil 3]]
           (lens/shove nil (lens/>> (lens/pos 0) (lens/pos 1)) 3)))
    (is (= [nil [nil 3]]
           (lens/shove nil (lens/>> (lens/pos 1) (lens/pos 1)) 3)))
    (is (= [13 42]
           (lens/shove [13 0] l 42)))
    (is (= 42
           (lens/yank [13 42] l)))
    
    (is (= (list 13 42)
           (lens/shove (list 13 0) l 42)))
    (is (= [13 42]
           (lens/shove [13] l 42)))
    (is (= [nil 42]
           (lens/shove [] l 42)))
    (is (= [nil 42] ;; ??
           (lens/shove nil l 42)))))

(deftest at-index-test
  (lens-laws-hold (lens/at-index 0) [12 42] 7 65)
  (lens-laws-hold (lens/at-index 1) '(12 42) 7 65)

  ;; laws must hold for map-entries
  (let [entry (first {3 4})]
    (lens-laws-hold (lens/at-index 0) entry 7 65))

  (is (= [13 42]
         (lens/shove [13 0] (lens/at-index 1) 42)))
  (is (= '(13 42)
         (lens/shove '(13 0) (lens/at-index 1) 42)))
  
  (is (vector? (lens/shove [13 0] (lens/at-index 1) 42)))
  (is (list? (lens/shove '(13 0) (lens/at-index 0) 42))))

(deftest default
  (let [l (lens/default 42)]
    (lens-laws-hold l nil 3 7)

    ;; default is not a "very well behaved" lens [1]
    #_(lens-laws-hold l 42 3 7)
    ;; but it's probably "well behaved" [1]
    (law-1-holds l nil 3)
    (law-2-holds l nil)

    (is (= 42
           (lens/yank nil l)))
    (is (= 13
           (lens/yank 13 l)))
    (is (= nil
           (lens/shove 13 l 42)))
    (is (= 13
           (lens/shove 42 l 13)))))

(deftest xmap
  (let [l (lens/xmap str #?(:clj read-string) #?(:cljs js/parseInt))]
    (lens-laws-hold l 42 "13" "1")
    (is (= "42"
           (lens/yank 42 l)))
    (is (= 13
           (lens/shove 42 l "13")))))

(deftest is-t
  (let [l (lens/is 42)]
    (lens-laws-hold l 13 true false)
    (is (lens/yank 42 l))
    (is (not (lens/yank 13 l)))
    (is (= 42
           (lens/shove 13 l true)))
    (is (= 13
           (lens/shove 13 l false)))
    (is (= nil
           (lens/shove 42 l false)))))

(deftest ++
  (let [l (lens/++ :a :b)]
    (lens-laws-hold l {:a 1 :b 2 :c 3} [6 7] [9 10])
    (is (= [1 2]
           (lens/yank {:a 1 :b 2 :c 3} l)))
    (is (= [nil 42]
           (lens/yank {:b 42} l)))
    (is (= {:a 42 :b 21 :c 3}
           (lens/shove {:a 13 :c 3} l [42 21]))))
  (let [l (lens/++ (lens/pos 1) (lens/pos 3))
        data [-4 2 0 17 -1 1]]
    (lens-laws-hold l data [6 7] [8 9])
    (is (= [2 17]
           (lens/yank data l)))
    (is (= [-4 11 0 12 -1 1]
           (lens/shove data l [11 12]))))
  (is (= [1 nil]
         (lens/yank {:a 1 :b 2 :c 3} (lens/++ :a lens/void)))))

(deftest >>
  (let [l (lens/>> :a :b)]
    (lens-laws-hold l {:a {:b 42} :c 4} 27 5)
    (is (= 42
           (lens/yank {:a {:b 42} :c 4} l)))
    (is (= nil
           (lens/yank {} l)))
    (is (= {:a {:b 8} :c 3}
           (lens/shove {:a {:b 42} :c 3} l 8))))
  (let [l (lens/>> (lens/pos 1) :a (lens/member "shmup"))
        data [{:a 1 :b 2} {:c [1 2 3] :a {"shmup" "shmip" "florb" "flubber"}}]]
    (lens-laws-hold l data "b" "c")
    (is (= "shmip"
           (lens/yank data l)))
    (is (= [{:a 1 :b 2} {:c [1 2 3] :a {"florb" "flubber"}}]
           (lens/shove data l nil))))
  (testing "Empty composition of lenses is `lens/id`"
    (is (= lens/id
           (lens/>>))))
  (testing "Composition of one lens is the lens itself"
    (is (= (lens/member :foo)
           (lens/>> (lens/member :foo)))))
  (testing "`lens/id` is the neutral element w.r.t. lens composition"
    (is (= lens/nel-head
           (lens/>> lens/nel-head
                    lens/id)))
    (is (= lens/nel-head
           (lens/>> lens/id
                    lens/nel-head)))))

(deftest **
  (let [l (lens/** :a lens/id)]
    (lens-laws-hold l [{:a 5} 27] [1 2] [3 4])
    (is (= [5 27]
           (lens/yank [{:a 5} 27] l)))
    (is (= [{:a 42} 13]
           (lens/shove [{:a 5} 27] l [42 13]))))
  (let [l (lens/** (lens/pos 2) (lens/member 42) (lens/default 'dflt))]
    (lens-laws-hold l [[4 3 2 1 0] {1 "a" 42 "b"} nil] [1 2 3] [4 5 'dflt])
    (is (= [2 "b" 'dflt]
           (lens/yank [[4 3 2 1 0] {1 "a" 42 "b"} nil] l)))
    (is (= [[4 3 nil 1 0] {1 "a" 42 42} nil]
           (lens/shove [[4 3 2 1 0] {1 "a" 42 "b"} 'dflt] l [nil 42 'dflt])))))

(deftest explicit
  (let [car (lens/lens first (fn [l v] (cons v (rest l))))]
    (is (= 'foo
           (lens/yank '(foo bar baz) car)))
    (is (= '(bla bar baz)
           (lens/shove '(foo bar baz) car 'bla)))))

(deftest kw
  (testing "Lens laws hold for keywords on maps that contain the respective key"
    (lens-laws-hold :shmup {:flubber "a" :shmup 3} 4 5))
  (is (= 'foo
         (lens/yank {:foo 'foo :bar 'bar} :foo)))
  (is (= {:foo 'baz :bar 'bar}
         (lens/shove {:foo 'foo :bar 'bar} :foo 'baz))))

(deftest at-index
  (lens-laws-hold (lens/at-index 2) [3 17 42] -1 -2)
  (let [l (lens/at-index 2)]
    (is (= 'baz
           (lens/yank '[foo bar baz bla] l)))
    (is (= '[foo bar bam bla]
           (lens/shove '[foo bar baz bla] l 'bam)))))

(deftest id
  (lens-laws-hold lens/id {"a" 3 "b" 4} 5 6)
  (is (= 'baz
         (lens/yank 'baz lens/id))
      (= 'bar
         (lens/shove 'baz lens/id 'bar))))

(deftest explicit-lens-invocation
  (testing "can use explicit lens like a function"
    (testing "for yanking"
      (testing "'argless' lenses"
        (is (= 1
               (lens/nel-head [1 2 3 4])))
        (is (= 4
               ((lens/pos 3) [1 2 3 4]))))
      (testing "lenses with args"
        (is (= :shmup
               ((lens/member :foo) {:bar 123 :foo :shmup}))))
      (testing "composed lenses"
        (is (= true
               ((lens/>> (lens/at-index 1) (lens/is :foo)) [:bar :foo]))))
      (testing "lens sums"
        (is (= [false {"shmup" -1 "bar" 3} -1]
               ((lens/++ (lens/is 42) lens/id (lens/member "shmup")) {"shmup" -1 "bar" 3}))))
      (testing "lens products"
        (is (= [:d nil :b]
               ((lens/** (lens/at-index 1) lens/void :a) [[:c :d] [4 5] {:a :b}])))))
    (testing "for shoving"
      (testing "'argless' lenses"
        (is (= [5 2 3 4]
               (lens/nel-head [1 2 3 4] 5)))
        (is (= [1 2 3 5]
               ((lens/pos 3) [1 2 3 4] 5))))
      (testing "lenses with args"
        (is (= {:a [1 2] :c :d}
               ((lens/member :a) {:a :b :c :d} [1 2]))))
      (testing "composed lenses"
        (is (= [:fst {:b {:foo :shmup}}]
               ((lens/>> (lens/at-index 1) :b (lens/member :foo)) [:fst {}] :shmup))))
      (testing "lens sums"
        (is (= {"shmup" :b :b 65}
               ((lens/++ (lens/member "shmup") :b) {"shmup" -1 :b 3} [:b 65]))))
      (testing "lens products"
        (is (= [[:foo nil :baz] 2 {:a nil :b 4}]
               ((lens/** (lens/at-index 1) lens/void :a) [[:foo :bar :baz] 2 {:a 3 :b 4}] [nil :foo nil])))))))

(deftest explicit-lens-apply
  (testing "can apply explicit lenses (like functions)"
    (testing "'argless lenses"
      (is (= [5 2 3 4]
             (apply lens/nel-head [1 2 3 4] [5])))
      (is (= 3
             (apply lens/void [3 5])))
      (is (= :bar
             (apply lens/id [:foo :bar]))))
    (testing "lenses with args"
      (is (= {:a :b :c 42}
             (apply (lens/member :c) [{:a :b} 42])))))
  #?(:clj (testing "throws when called with the wrong number of arguments"
            (is (thrown-with-msg? java.lang.IllegalArgumentException
                                  #"arguments \(3\)"
                                  (apply (lens/member :a) [{} 2 :too-many-args])))
            (is (thrown-with-msg? java.lang.IllegalArgumentException
                                  #"arguments \(0\)"
                                  (apply (lens/member :a) []))))
     :cljs (testing "throws when called with the wrong number of arguments"
             (is (thrown-with-msg? js/Error
                                   #"arity"
                                   (apply (lens/member :a) [{} 2 :too-many-args])))
             (is (thrown-with-msg? js/Error
                                   #"arity"
                                   (apply (lens/member :a) []))))))

(defrecord Comparer [shmup])

(deftest lens-comparison
  (let [items [:foo
               identity
               #(identity %)
               #(identity %) ;; this is not equal to the line above
               #{1 2 3}
               {:a 1 :b 2}
               nil
               5
               "peter"
               (->Comparer :shmup)]
        combinators [lens/>> lens/++ lens/**]]
    (doall
     (for [item-1 items
           item-2 items
           combinator combinators]
       (testing "Equality of lenses without args behaves as expected:"
         (let [gen-lens-1 #(lens/lens item-1 item-1)
               gen-lens-2 #(lens/lens item-2 item-2)]
           (if (= item-1 item-2)
             (do
               (testing "Lenses constructed from equal values are equal"
                 (is (= (gen-lens-1)
                        (gen-lens-2)))
                 (testing "Compositions of equal lenses are equal"
                   ;; Combined lenses are internally represented as lenses with args
                   (is (= (combinator lens/nel-head (gen-lens-2))
                          (combinator lens/nel-head (gen-lens-1)))))))
             (do
               (testing "Lenses constructed from unequal values are unequal"
                 (is (not= (gen-lens-1)
                           (gen-lens-2)))
                 (testing "Compositions of unequal lenses cannot be equal"
                   ;; = lenses with args where only the args differ
                   (is (not= (combinator lens/nel-tail (gen-lens-2))
                             (combinator lens/nel-tail (gen-lens-1))))))))))))))

(defrecord MergeRec1 [a])
(defrecord MergeRec2 [b])

(deftest merge-lens-test
  (testing "full data"
    (lens-laws-hold lens/merge [{:a 42} {:b 10}] {:a 21 :b 5} {:a 1 :b 2}))

  (testing "preserves records"
    (is (= [(MergeRec1. 10) (MergeRec2. 5)]
           (lens/shove [(MergeRec1. 42) (MergeRec2. 10)]
                       lens/merge
                       {:a 10 :b 5}))))

  (testing "missing data is removed once where it should have been"
    (is (= [{:a 42} {}]
           (lens/shove [{:a 21} {:b 10}]
                       lens/merge
                       {:a 42})))
    (is (= [{} {:b 10}]
           (lens/shove [{:a 42} {:b 5}]
                       lens/merge
                       {:b 10})))
    (is (= [{:a 21} {}]
           (lens/shove [{:a 21} {:a 10}]
                       lens/merge
                       {})))
    
    (lens-laws-hold lens/merge [{:a 42} {:b 10}] {:a 21} {:a 2})
    (lens-laws-hold lens/merge [{:a 42} {:b 10}] {:b 21} {:b 2}))
  
  (testing "new data is added on left-most"
    (is (= [{:a 42 :c 17} {:b 10}]
           (lens/shove [{:a 42} {:b 10}]
                       lens/merge
                       {:a 42 :b 10 :c 17})))
    
    (lens-laws-hold lens/merge [{:a 42} {:b 10}] {:a 42 :b 10 :c 17} {:a 42 :b 10 :c 42}))

  (testing "duplicate keys are taken from right and shadow those on the left"
    (is (= {:a 10}
           (lens/yank [{:a 42} {:a 10}]
                      lens/merge)))
    (is (= [{:a 42} {:a 21}]
           (lens/shove [{:a 42} {:a 10}]
                       lens/merge
                       {:a 21})))
    
    (lens-laws-hold lens/merge [{:a 42} {:a 10}] {:a 21} {:a 2})))

(deftest pattern-lens-test
  (testing "as a map"
    (is (= {:a 42 :b 10}
           (lens/yank {:foo 42 :bar 10}
                      (lens/pattern {:a :foo :b :bar}))))

    (is (= {:foo 21 :bar 5}
           (lens/shove {:foo 42 :bar 10}
                       (lens/pattern {:a :foo :b :bar})
                       {:a 21 :b 5})))
    
    (lens-laws-hold (lens/pattern {:a :foo :b :bar})
                    {:foo 42 :bar 10} {:a 21 :b 5} {:a 11 :b 3}))

  (testing "as a vector"
    (is (= [42 10]
           (lens/yank {:foo 42 :bar 10}
                      (lens/pattern [:foo :bar]))))

    (is (= {:foo 21 :bar 5}
           (lens/shove {:foo 42 :bar 10}
                       (lens/pattern [:foo :bar])
                       [21 5])))
    
    (lens-laws-hold (lens/pattern [:foo :bar])
                    {:foo 42 :bar 10} [21 5] [11 3])))

(defrecord ProjectionSource [foo bar])
(defrecord ProjectionTarget [shmup flup])

(deftest projection-lens-test
  (lens-laws-hold (lens/projection {} {:c :d :e :f})
                  {:d 3 :f "some f"}
                  {:c -1 :e nil}
                  {:c 17 :e "hi"})
  (testing "can convert between maps"
    (let [proj (lens/projection {} {:a :b})]
      (is (= {:a "foobar"}
             (lens/yank {:b "foobar"} proj)))
      (is (= {:b "blub"}
             (lens/shove {:b "foobar"}
                         proj
                         {:a "blub"})))))
  (testing "can convert between records"
    (let [proj (lens/projection (->ProjectionTarget nil nil)
                                {:flup :foo
                                 :shmup :bar})]
      (is (= (->ProjectionTarget 3 "hi")
             (lens/yank (->ProjectionSource "hi" 3) proj)))
      (is (= (->ProjectionSource 0 "empty")
             (lens/shove (->ProjectionSource -2 "abc")
                         proj
                         (->ProjectionTarget "empty" 0))))))
  (testing "combinations with other lenses"
    (let [proj (lens/projection {}
                                {(lens/>> :flup :shmup) (lens/>> :a :b)
                                 (lens/>> :flup :blub) (lens/>> :list
                                                                (lens/at-index 1))})]
      (is (= {:flup {:shmup 17 :blub 2}}
             (lens/yank {:a {:b 17}
                         :list [1 2 3]}
                        proj)))
      (is (= {:a {:b -1}
              :list [5 "abc"]}
             (lens/shove {:a {:b 17}
                          :list [5]}
                         proj
                         {:flup {:shmup -1 :blub "abc"}}))))))

;; [1] J. Nathan Foster, Michael B. Greenwald, Jonathan T. Moore,
;; Benjamin C. Pierce, and Alan Schmitt. “Combinators for
;; Bidirectional Tree Transformations: A Linguistic Approach to the
;; View Update Problem”. In: Principles of Programming Languages. ACM
;; Press, 2005, pages 233-246. doi: 10.1145/1040305.1040325.
