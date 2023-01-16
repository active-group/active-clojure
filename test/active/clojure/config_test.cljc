(ns ^:config active.clojure.config-test
  (:require [active.clojure.config :as c]
            #?(:cljs [cljs.test :as t])
            #?(:clj [clojure.test :refer :all]))
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
           (c/access config string-setting strings-section)))))

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
           (c/access config string-setting strings-section)))))

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

