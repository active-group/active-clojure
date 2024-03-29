(ns ^:config active.clojure.config-test
  (:require [active.clojure.config :as c]
            #?(:cljs [active.clojure.cljs.record :refer [define-record-type]])
            #?(:clj [active.clojure.record :refer :all])
            #?(:cljs [cljs.test :as t])
            #?(:clj [clojure.test :refer :all])
            [active.clojure.lens :as lens])
  #?(:cljs (:require-macros [cljs.test :refer (is deftest run-tests testing)])))


(def initialize?
  "Setting for whether a task should initialize its RTDB tags."
  (c/setting :initialize?
             "Should a task initialize its RTDB tags?"
             (c/boolean-range false)
             :inherit? true))

(def mode
  "Setting for a mode: `:development` or `:production`."
  (c/setting :mode
             "The mode FactoryLink runs in."
             (c/one-of-range #{:development :production} :production)))

(def programmable-counters
  (c/section :programmable-counters
             (c/schema "Programmable Counters" initialize?)))

(def schema1
  (c/schema "Test Schema #1"
            mode
            initialize?
            programmable-counters))

(deftest normalize&check1
  (is (= {:mode :development
          :initialize? false
          :programmable-counters {:initialize? true}}
         (c/normalize&check-config-object
          schema1
          []
          {:mode :development
           :programmable-counters {:initialize? true}})))
  (is (c/range-error?
       (c/normalize&check-config-object
        schema1
        []
        {:mode :development
         :programmable-counters {:initialize? 'foo}})))
  (is (c/range-error?
       (c/normalize&check-config-object
        schema1
        []
        {:mode :developpment
         :programmable-counters {:initialize? true}}))))

(deftest defaults
  (is (= {:mode :production
          :initialize? false
          :programmable-counters {:initialize? false}}
         (c/normalize&check-config-object
          schema1
          []
          {}))))

(deftest inheritance
  (is (= {:mode :production 
          :initialize? true
          :programmable-counters {:initialize? true}}
         (c/normalize&check-config-object
          schema1
          []
          {:initialize? true
           :programmable-counters {}})))
  (is (= {:mode :production 
          :initialize? true
          :programmable-counters {:initialize? true}}
         (c/normalize&check-config-object
          schema1
          []
          {:initialize? true}))))

(deftest profiles
  (testing "whether the basic profile mechanism works"
    (is (= {:mode :production 
            :initialize? false
            :programmable-counters {:initialize? true}}
           (c/normalize&check-config-object
            schema1
            [:dev]
            {:programmable-counters {:initialize? true}
             :profiles
             {:dev
              {:mode :production}}}))))
  (testing "whether profiles work for sections"
    (is (= {:mode :production 
          :initialize? false
            :programmable-counters {:initialize? true}}
           (c/normalize&check-config-object
            schema1
            [:dev]
            {:profiles
             {:dev
              {:mode :production
               :programmable-counters {:initialize? true}}}}))))
  (testing "whether profiles override"
    (is (= {:mode :production 
            :initialize? false
            :programmable-counters {:initialize? true}}
           (c/normalize&check-config-object
            schema1
            [:dev]
            {:programmable-counters {:initialize? false}
             :profiles
             {:dev
              {:mode :production
               :programmable-counters {:initialize? true}}}}))))
  (testing "whether profiles don't get mixed in inadvertently"
    (is (= {:mode :production
            :initialize? false
            :programmable-counters {:initialize? true}}
           (c/normalize&check-config-object
            schema1
            []
            {:programmable-counters {:initialize? true}
             :profiles
             {:dev
              {:mode :production}}})))))

(def section2
  (c/section :section2
             (c/schema "nested section" initialize? programmable-counters)))

(def schema2
  (c/schema "Test Schema #1"
          mode
          initialize?
          section2))

(deftest profiles-nested
  (testing "whether nested settings work"
    (is (= {:mode :production 
            :initialize? false
            :section2
            {:initialize? false
             :programmable-counters
             {:initialize? true}}}
           (c/normalize&check-config-object
            schema2
            []
            {:section2
             {:programmable-counters
              {:initialize? true}}}))))
  (testing "whether nested settings work with profiles"
    (is (= {:mode :production 
            :initialize? false
            :section2
            {:initialize? false
             :programmable-counters
             {:initialize? false}}}
           (c/normalize&check-config-object
            schema2
            [:dev]
            {:section2
             {:programmable-counters
              {:initialize? true}}
             :profiles {:dev
                        {:section2
                         {:programmable-counters
                          {:initialize? false}}}}})))))

(def foo-setting
  (c/setting :foo
             "foo setting"
             (c/integer-between-range -10 10 0)))

(deftest access-test
  (let [c1
        (c/make-configuration schema1 []
                              {:programmable-counters
                               {:initialize? true}})]
    (is (= true
           (c/access c1
                     initialize? programmable-counters)))
    (is (thrown? #?(:clj Exception) #?(:cljs js/Error)
                 (c/access c1 foo-setting))))
  (let [c2
        (c/make-configuration schema1 []
                              {:initialize? true
                               :programmable-counters {}})]
    (is (= true
           (c/access c2
                     initialize? programmable-counters)))
    (is (= {:initialize? true}
           (c/access c2
                     programmable-counters)))))

(def section3
  (c/section :inherits
             (c/schema "Section 3" foo-setting)
             :inherit? true))

(def nested3
  (c/section :outer
             (c/schema "nested section" section3)))

(def schema3
  (c/schema "Test Schema #3" section3 nested3))

(deftest section-inheritance
  (is (= {:inherits {:foo 5}
          :outer {:inherits {:foo 5}}}
         (c/normalize&check-config-object
          schema3
          []
          {:inherits {:foo 5}})))
  (is (= {:inherits {:foo 0}
          :outer {:inherits {:foo 0}}}
         (c/normalize&check-config-object
          schema3
          []
          {}))))


(deftest diff
  (is (empty?
       (c/diff-configurations
        schema3
        (c/make-configuration
         schema3
         []
         {:inherits {:foo 5}})
        (c/make-configuration
         schema3
         []
         {:inherits {:foo 5}}))))
  (is (= '([[:inherits :foo] 5 0] [[:outer :inherits :foo] 5 0])
         (c/diff-configurations
          schema3
          (c/make-configuration
           schema3
           []
           {:inherits {:foo 5}})
          (c/make-configuration
           schema3
           []
           {}))))
  (is (= '([[:outer :inherits :foo] 5 0])
         (c/diff-configurations
          schema3
          (c/make-configuration
           schema3
           []
           {:outer {:inherits {:foo 5}}})
          (c/make-configuration
           schema3
           []
           {})))))

(deftest subconfig-test
  (let [config
        (c/make-configuration
         schema3
         []
         {})
        subconfig (c/section-subconfig config nested3)]
    (is (= 0
           (c/access subconfig foo-setting section3)))))

(def string-setting
  (c/setting :string
             "string setting"
             (c/default-string-range "foo")))

(def strings-section
  (c/section :strings
             (c/sequence-schema
              "Sequence of string sections"
              (c/schema
               "String section"
               string-setting))))

(def strings-schema
  (c/schema "Mapping access"
            strings-section))

;; FIXME: also need to test merging, diff

(deftest sequence-schemas-test
  (let [config
        (c/make-configuration
         strings-schema
         []
         {:strings
          [{:string "foo"}
           {:string "bar"}
           {:string "baz"}]})]
    (is (= {:strings
            []}
           (c/normalize&check-config-object
            strings-schema
            []
            {})))
    (is (= {:strings
            [{:string "foo"}
             {:string "foo"}
             {:string "foo"}]}
           (c/normalize&check-config-object
            strings-schema
            []
            {:strings [{} {} {}]})))
    (is (= ["foo" "bar" "baz"]
           (c/access config string-setting strings-section)))
    (is (= "bar"
           (c/access config string-setting strings-section 1)))))

(def nonempty-strings-section
  (c/section :strings
             (c/nonempty-sequence-schema
              "Sequence of string sections that must not be empty"
              (c/schema
               "String section"
               string-setting))))

(def nonempty-strings-schema
  (c/schema "Mapping access"
            nonempty-strings-section))

(deftest nonempty-sequence-schemas-test
  (let [config
        (c/make-configuration
         nonempty-strings-schema
         []
         {:strings
          [{:string "foo"}
           {:string "bar"}
           {:string "baz"}]})]
    (is (thrown? #?(:clj Exception) #?(:cljs js/Error)
                 (c/normalize&check-config-object
                  nonempty-strings-schema
                  []
                  {})))
    (is (= {:strings
            [{:string "foo"}
             {:string "foo"}
             {:string "foo"}]}
           (c/normalize&check-config-object
            strings-schema
            []
            {:strings [{} {} {}]})))
    (is (= ["foo" "bar" "baz"]
           (c/access config string-setting strings-section)))
    (is (= "bar"
           (c/access config string-setting strings-section 1)))))

(def nested-strings-section
  (c/section :nested-strings
             (c/sequence-schema
              "Sequence of nested strings sections"
              strings-schema)))

(def nested-strings-schema
  (c/schema "Nested mapping access"
            nested-strings-section))

(deftest nested-strings-schemas-test
  (let [config (c/make-configuration
                nested-strings-schema
                []
                {:nested-strings
                 [{:strings
                   [{:string "foo"}
                    {:string "bar"}
                    {:string "baz"}]}
                  {:strings
                   [{:string "foo"}
                    {:string "bar"}
                    {:string "baz"}]}]})]
    (is (= [["foo" "bar" "baz"] ["foo" "bar" "baz"]]
           (c/access config string-setting nested-strings-section strings-section)))))

(def nested-sequences-section
  (c/section :nested-sequences
             (c/sequence-schema
              "Sequence of nested sequences"
              (c/sequence-schema "inner strings" (c/schema
                                                   "String section"
                                                   string-setting)))))

(def nested-sequences-schema
  (c/schema "Nested sequences access"
            nested-sequences-section))

(deftest nested-sequence-schemas-test
  (let [config (c/make-configuration
                nested-sequences-schema
                []
                {:nested-sequences
                 [[{:string "foo"}
                   {:string "bar"}
                   {:string "baz"}]
                  [{:string "foo"}
                   {:string "bar"}
                   {:string "baz"}]]})]
    (is (= [["foo" "bar" "baz"] ["foo" "bar" "baz"]]
           (c/access config string-setting nested-sequences-section))))
  (let [config (c/make-configuration
                nested-sequences-schema
                []
                {:nested-sequences
                 [[{:string "foo"}
                   {:string "bar"}
                   {:string "baz"}]
                  [{:string "foo"}
                   {:string "bar"}
                   {:string "baz"}]]})]
    (is (= "bar"
           (c/access config string-setting nested-sequences-section 0 1)))))

(deftest sequence-diff
  (is (= '([[:strings 2] nil {:string "baz"}]
           [[:strings 3] nil {:string "baf"}])
         (c/diff-configurations
          strings-schema
          (c/make-configuration
           strings-schema
           []
           {:strings
            [{:string "foo"}
             {:string "bar"}]})
          (c/make-configuration
           strings-schema
           []
           {:strings
            [{:string "foo"}
             {:string "bar"}
             {:string "baz"}
             {:string "baf"}]}))))
  (is (= '([[:strings 2] {:string "baz"} nil]
           [[:strings 3] {:string "baf"} nil])
         (c/diff-configurations
          strings-schema
          (c/make-configuration
           strings-schema
           []
           {:strings
            [{:string "foo"}
             {:string "bar"}
             {:string "baz"}
             {:string "baf"}]})
          (c/make-configuration
           strings-schema
           []
           {:strings
            [{:string "foo"}
             {:string "bar"}]}))))
  (is (= '([[:strings 1 :string] "bar" "fix"])
         (c/diff-configurations
          strings-schema
          (c/make-configuration
           strings-schema
           []
           {:strings
            [{:string "foo"}
             {:string "bar"}
             {:string "baz"}]})
          (c/make-configuration
           strings-schema
           []
           {:strings
            [{:string "foo"}
             {:string "fix"}
             {:string "baz"}]})))))

(def map-of-range-schema
  (c/schema "map-of-range-diff-example"
            (c/setting :mp
                       "map example"
                       (c/map-of-range c/string-range c/string-range))))

(def optional-map-of-range-schema
  (c/schema "map-of-range-diff-example"
            (c/setting :mp
                       "map example"
                       (c/optional-range (c/map-of-range c/string-range c/string-range)))))

(deftest map-of-range-diff
  (is (= (set
          '([[:mp "1"] "one" nil]
            [[:mp "2"] "two" "fortytwo"]
            [[:mp "23"] nil "twentythree"]))
         (set
          (c/diff-configurations
           map-of-range-schema
           (c/make-configuration
            map-of-range-schema
            []
            {:mp
             {"1" "one"
              "2" "two"
              "3" "three"}})
           (c/make-configuration
            map-of-range-schema
            []
            {:mp
             {"23" "twentythree"
              "2" "fortytwo"
              "3" "three"}})))))
  (is (= (set
          '([[:mp "1"] "one" nil]
            [[:mp "2"] "two" "fortytwo"]
            [[:mp "23"] nil "twentythree"]))
         (set
          (c/diff-configurations
           map-of-range-schema
           (c/make-configuration
            optional-map-of-range-schema
            []
            {:mp
             {"1" "one"
              "2" "two"
              "3" "three"}})
           (c/make-configuration
            optional-map-of-range-schema
            []
            {:mp
             {"23" "twentythree"
              "2" "fortytwo"
              "3" "three"}}))))))

(def a-setting
  (c/setting
    :a
    "a example setting"
    c/string-range))

(def b-setting
  (c/setting
    :b
    "b example setting"
    (c/boolean-range true)))

(def pare-schema
  (c/schema
    "map-schema that is the basis for the record"
    a-setting
    b-setting))

(def pare-section
  (c/section
    :pare
    pare-schema))

(def map-schema
    (c/schema
    "record example schema"
    pare-section))

(def pare-example-map-schema-config-map
  {:pare {:a "Yeah!" :b true}})

(def pare-sequence-schema
  (c/sequence-schema
    "sequence-schema that is the basis for the record"
    pare-schema))

(deftest lens-pare-map-test
  (is (= pare-example-map-schema-config-map
         (c/normalize&check-config-object map-schema [] pare-example-map-schema-config-map)))
  (let [conf (c/make-configuration map-schema [] pare-example-map-schema-config-map)
        l (c/access-lens pare-section)]
    (is (= {:a "Yeah!" :b true} (lens/yank conf l)))
    (is (= {:a "Changed!" :b true}
           (lens/yank (lens/shove conf l {:a "Changed!"}) l))))
  (let [conf (c/make-configuration map-schema [] pare-example-map-schema-config-map)
        l (c/access-lens a-setting pare-section)]
    (is (= "Yeah!" (lens/yank conf l)))
    (is (= "Changed!" (lens/yank (lens/shove conf l "Changed!") l)))))

(def pares-section
  (c/section
      :pares
      pare-sequence-schema))

(def sequence-schema
  (c/schema
    "record example schema"
    pares-section))

(def pare-example-sequence-schema-config-map
  {:pares [{:a "Oh!" :b false} {:a "Yeah!" :b true}]})

(deftest lens-pare-sequence-test
  (is (= pare-example-sequence-schema-config-map
         (c/normalize&check-config-object sequence-schema [] pare-example-sequence-schema-config-map)))
  (let [conf (c/make-configuration sequence-schema [] pare-example-sequence-schema-config-map)
        l (c/access-lens pares-section)]
    (is (= [{:a "Oh!" :b false} {:a "Yeah!" :b true}] (lens/yank conf l)))
    (is (= [{:a "Oh!" :b true} {:a "No!" :b false}]
           (lens/yank (lens/shove conf l [{:a "Oh!"}{:a "No!" :b false}]) l))))
  (let [conf (c/make-configuration sequence-schema [] pare-example-sequence-schema-config-map)
        l (c/access-lens 1 pares-section)]
    (is (= {:a "Yeah!" :b true} (lens/yank conf l)))
    (is (= {:a "Changed!" :b true} (lens/yank (lens/shove conf l {:a "Changed!"}) l))))
  (let [conf (c/make-configuration sequence-schema [] pare-example-sequence-schema-config-map)
        l (c/access-lens a-setting pares-section)]
    (is (= ["Oh!" "Yeah!"] (lens/yank conf l)))
    (is (= ["Changed!" "Cool!"] (lens/yank (lens/shove conf l ["Changed!" "Cool!"]) l))))
  (let [conf (c/make-configuration sequence-schema [] pare-example-sequence-schema-config-map)
        l (c/access-lens a-setting pares-section 1)]
    (is (= "Yeah!" (lens/yank conf l)))
    (is (= "Changed!" (lens/yank (lens/shove conf l "Changed!") l)))))

(deftest lens-nested-strings-schemas-test
  (let [config (c/make-configuration
                nested-strings-schema
                []
                {:nested-strings
                 [{:strings
                   [{:string "foo"}
                    {:string "bar"}
                    {:string "baz"}]}
                  {:strings
                   [{:string "foo"}
                    {:string "bar"}
                    {:string "baz"}]}]})
        l (c/access-lens string-setting nested-strings-section strings-section)]
    (is (= [["foo" "bar" "baz"] ["foo" "bar" "baz"]]
           (l config)))
    (is (= [["1" "2" "3"] ["4" "5" "6"]]
           (l (l config [["1" "2" "3"] ["4" "5" "6"]])))))
  (let [config (c/make-configuration
                nested-strings-schema
                []
                {:nested-strings
                 [{:strings
                   [{:string "foo"}
                    {:string "bar"}
                    {:string "baz"}]}
                  {:strings
                   [{:string "foo"}
                    {:string "bar"}
                    {:string "baz"}]}]})
        l (c/access-lens string-setting nested-strings-section strings-section 1)]
    (is (= ["bar" "bar"]
           (l config)))
    (is (= ["2" "5"]
           (l (l config ["2" "5"]))))))

(deftest lens-nested-sequence-schemas-test
  (let [config (c/make-configuration
                nested-sequences-schema
                []
                {:nested-sequences
                 [[{:string "foo"}
                   {:string "bar"}
                   {:string "baz"}]
                  [{:string "foo"}
                   {:string "bar"}
                   {:string "baz"}]]})
        l (c/access-lens string-setting nested-sequences-section)]
    (is (= [["foo" "bar" "baz"] ["foo" "bar" "baz"]]
           (l config)))
    (is (= [["1" "2" "3"] ["4" "5" "6"]]
           (l (l config [["1" "2" "3"] ["4" "5" "6"]])))))
  (let [config (c/make-configuration
                nested-sequences-schema
                []
                {:nested-sequences
                 [[{:string "foo"}
                   {:string "bar"}
                   {:string "baz"}]
                  [{:string "foo"}
                   {:string "bar"}
                   {:string "baz"}]]})
        l (c/access-lens string-setting nested-sequences-section 0 1)]
    (is (= "bar"
           (l config)))
    (is (= "2"
           (l (l config "2"))))))

(define-record-type Pare
  {:projection-lens pare-projection-lens}
  make-pare
  pare?
  [a pare-a
   b pare-b])

(deftest lens-record-projection-test
  (let [conf (c/make-configuration map-schema [] pare-example-map-schema-config-map)
        l (lens/projection (make-pare nil nil) [[pare-a (c/access-lens a-setting pare-section)]
                                                [pare-b (c/access-lens b-setting pare-section)]])]
    (is (= (make-pare "Yeah!" true) (lens/yank conf l))))

  (let [conf (c/make-configuration map-schema [] pare-example-map-schema-config-map)
        l (lens/projection (make-pare nil nil) [[pare-a (c/access-lens a-setting pare-section)]
                                                [pare-b (c/access-lens b-setting pare-section)]])]
    (is (= (make-pare "Oh!" false) (lens/yank (lens/shove conf l (make-pare "Oh!" false)) l))))

  (let [conf (c/make-configuration sequence-schema [] pare-example-sequence-schema-config-map)
        l (lens/>> (c/access-lens pares-section)
                   (lens/mapl (lens/projection (make-pare nil nil) [[pare-a (c/setting-key a-setting)]
                                                                    [pare-b (c/setting-key b-setting)]])))]
    (is (= [(make-pare "Oh!" false) (make-pare "Yeah!" true)] (lens/yank conf l)))
    (is (= [(make-pare "Oh!" true) (make-pare "No!" false)]
           (lens/yank (lens/shove conf l [(make-pare "Oh!" true) (make-pare "No!" false)]) l))))

  (let [conf (c/make-configuration sequence-schema [] pare-example-sequence-schema-config-map)
        l (lens/>> (c/sequence-schema-subconfig-lens pares-section)
                   (lens/mapl (lens/projection (make-pare nil nil) [[pare-a (c/access-lens a-setting)]
                                                                    [pare-b (c/access-lens b-setting)]])))]
    (is (= [(make-pare "Oh!" false) (make-pare "Yeah!" true)] (lens/yank conf l)))
    (is (= [(make-pare "Oh!" true) (make-pare "No!" false)]
           (lens/yank (lens/shove conf l [(make-pare "Oh!" true) (make-pare "No!" false)]) l))))

  (let [conf (c/make-configuration map-schema [] pare-example-map-schema-config-map)
        l (pare-projection-lens (c/access-lens a-setting pare-section) (c/access-lens b-setting pare-section))]
    (is (= (make-pare "Yeah!" true) (lens/yank conf l)))))
