(ns active.clojure.freer-monad-test
  #?(:cljs (:require-macros [active.clojure.freer-monad :refer (monadic)]
                            [cljs.test :refer (is deftest run-tests testing)]))
  (:require #?(:clj [active.clojure.freer-monad :refer :all])
            #?(:cljs [active.clojure.freer-monad :refer (return return? return-result
                                                         bind bind? bind-first-action bind-continuations apply-continuations
                                                         call-cc call-cc?
                                                         with-handler signal
                                                         get-state put-state!
                                                         get-env get-env-component with-env-component with-env
                                                         and-finally bind-except sequ sequ_
                                                         make-exception-value exception-value?
                                                         unknown-command unknown-command?
                                                         make-monad-command-config
                                                         combine-monad-command-configs
                                                         null-monad-command-config
                                                         run-freer-reader-state-exception execute-freer-reader-state-exception
                                                         run-monadic execute-monadic
                                                         monad-command-config-run-command
                                                         reify-command reify-as
                                                         put-state-component!)])
            [active.clojure.condition :as c]
            #?(:clj [clojure.test :refer :all])
            #?(:cljs [cljs.test])))

(defrecord Ask [prompt])
(defn ask [prompt] (Ask. prompt))
(defn ask? [x] (instance? Ask x))

(defrecord Tell [msg])
(defn tell [msg] (Tell. msg))
(defn tell? [x] (instance? Tell x))

(defn run
  [m mp]
  (loop [m m
         msgs []]
    (cond
     (return? m) [(return-result m) msgs]

     (bind? m)
     (let [m1 (bind-first-action m)
           conts (bind-continuations m)]
       (cond
        (ask? m1)
        (if-let [ans (get mp (:prompt m1))]
          (recur (apply-continuations conts ans) msgs)
          (c/assertion-violation `run "unknown question" (:prompt m1) m1))

        (tell? m1)
        (recur (apply-continuations conts nil) (conj msgs (:msg m1)))))

     (ask? m)
     (if-let [ans (get mp (:prompt m))]
       [ans msgs]
       (c/assertion-violation `run "unknown question" (:prompt m)))

     (tell? m)
     [nil (conj msgs (:msg m))])))

(defn ex1
  []
  (monadic [first (ask "what's your first name?")
            last (ask "what's your last name?")]
           (let [s (str "Hello, " first " " last)])
           (tell s)))

(deftest test-run
  (is (= [nil ["Hello, Mike Sperber"]]
         (run (ex1)
           {"what's your first name?" "Mike"
            "what's your last name?" "Sperber"}))))

(defn tester-ask
  [prompt]
  (fn [mp]
    (if-let [ans (get mp prompt)]
      [[] ans]
      (c/assertion-violation `tester-ask "unknown question" prompt mp))))

(defn tester-tell
  [msg]
  (fn [mp]
    [[msg] nil]))

(defn tester-throw
  [ex]
  (fn [mp]
    [[] (make-exception-value ex)]))

(defn tester-with-handler
  [handler body]
  (fn [mp]
    (let [[o1 a :as res] (body mp)]
      (if (exception-value? a)
        ((handler (:exception a)) mp)
        res))))

(defn run-tester
  [m mp]
  (m mp))

(defn interact->tester
  [m]
  (cond
   (ask? m) (tester-ask (:prompt m))
   (tell? m) (tester-tell (:msg m))))

(defn run-ask-tell
  [run-any env state comp]
  (cond
   (ask? comp)
   [(get (::answers env) (:prompt comp)) state]

   (tell? comp)
   [nil (update state ::output #(conj % (:msg comp)))]

   :else unknown-command))


(defn run-ask-tell-config
  [qas]
  (make-monad-command-config run-ask-tell {::answers qas} {::output []}))


(defn ex2
  []
  (monadic [first (ask "what's your first name?")]
           (if (= first "Mike")
             (signal "It's Mike")
             (monadic
              [last (ask "what's your last name?")]
              (tell (str "Hello, " first " " last))))))

;; FIXME DELETEME
#_(deftest test2-freer->m-exception
  (is (= [[] (make-exception-value "It's Mike")]
         (run-tester (free->tester-m (ex2))
                     {"what's your first name?" "Mike"
                      "what's your last name?" "Sperber"}))))

(defn ex3
  []
  (let [first-name
        (monadic [first (ask "what's your first name?")]
                 (if (= first "Mike")
                   (signal "It's Mike")
                   (return first)))]
    (monadic [first (with-handler (fn [ex]
                                    (if (= "It's Mike" ex)
                                      (return "Michael")
                                      (return "Unknown")))
                      first-name)
              last (ask "what's your last name?")]
             (tell (str "Hello, " first " " last)))))

(deftest frse-ex3
  (is (= [nil {::output ["Hello, David Frese"]}]
         (run-freer-reader-state-exception (run-ask-tell-config
                                            {"what's your first name?" "David"
                                             "what's your last name?" "Frese"})
                                           (ex3)))))
(deftest frse-ex3-with-handler
  (is (= [nil {::output ["Hello, Michael Sperber"]}]
         (run-freer-reader-state-exception (run-ask-tell-config
                                            {"what's your first name?" "Mike"
                                             "what's your last name?" "Sperber"})
                                           (ex3)))))

(deftest with-handler-state
  (is (= [nil {::with-handler-state true}]
         (run-freer-reader-state-exception (null-monad-command-config nil nil)
                                           (monadic
                                            (with-handler
                                              (fn [exn]
                                                (put-state-component! ::with-handler-state true))
                                              (monadic
                                               (signal 'something))))))))

(deftest test-and-finally
  (is (= [(make-exception-value "It's Mike") {::output ["Hello"]}]
         (run-freer-reader-state-exception (run-ask-tell-config
                                            {"what's your first name?" "Mike"
                                             "what's your last name?" "Doe"})
                                           (and-finally (ex2)
                                                        (tell "Hello"))))))

(deftest test-bind-except
  (is (= [nil {::output ["Hello"]}]
         ;; ex2 causes exception, so only handler is called
         (run-freer-reader-state-exception (run-ask-tell-config
                                            {"what's your first name?" "Mike"
                                             "what's your last name?" "Doe"})
                                           (bind-except (ex2)
                                                        (fn [e] (tell "Hello"))
                                                        (fn [v] (tell "Hola"))))))

  (is (= [nil {::output ["Hola"]}]
         ;; return does not cause exception, so body is called with result
         (run-freer-reader-state-exception (run-ask-tell-config {})
                                           (bind-except (return "Hola")
                                                        (fn [e] (tell "Hello"))
                                                        (fn [v] (tell v)))))))
(defn ex4
  []
  (sequ [(monadic (tell "hello")
                  (return 1))
         (monadic (tell "world")
                  (return 2))]))

(deftest tsequ
  (is (= [[1 2] {::output ["hello" "world"]}]
         (run-freer-reader-state-exception (run-ask-tell-config {})
                                          (ex4)))))


(defn ex5
  []
  (sequ_ [(monadic (tell "hello")
                   (return 1))
          (monadic (tell "world")
                   (return 2))]))


(deftest tsequ_
  (is (= [nil {::output ["hello" "world"]}]
         (run-freer-reader-state-exception (run-ask-tell-config {})
                                           (ex5))))
  (testing "sequ_ does not consume stack"
    (is (= [nil nil]
           (run-freer-reader-state-exception (null-monad-command-config nil nil)
                                             (sequ_ (repeat 20000 (return "hello"))))))))


(deftest frse-trivial
  (is (= ["Hola" nil]
         (run-freer-reader-state-exception (null-monad-command-config nil nil) (return "Hola"))))
  (is (= ["Hola" nil]
         (run-freer-reader-state-exception (null-monad-command-config nil nil)
                                           (bind (return "Hola")
                                                 (fn [x]
                                                   (return x)))))))

(deftest frse-ask-tell
  (is (= [nil {::output ["Hello, Mike Sperber"]}]
         (run-freer-reader-state-exception (run-ask-tell-config 
                                            {"what's your first name?" "Mike"
                                             "what's your last name?" "Sperber"})
                                           (ex1)))))


(deftest frse-with-handler
  (is (= [nil {::output ["Hello, David Frese"]}]
         (run-freer-reader-state-exception (run-ask-tell-config
                                            {"what's your first name?" "David"
                                             "what's your last name?" "Frese"})
                                           (ex2)))))

(deftest frse-exception
  (is (= [(make-exception-value "It's Mike") {::output []}]
         (run-freer-reader-state-exception (run-ask-tell-config
                                            {"what's your first name?" "Mike"
                                             "what's your last name?" "Sperber"})
                                           (ex2)))))

(deftest frse-env
  (is (= [['foo 'bar] nil]
         (run-freer-reader-state-exception (null-monad-command-config 'foo nil)
                                           (monadic [x (get-env)
                                                     y (with-env (constantly 'bar)
                                                         (get-env))]
                                                    (return [x y]))))))

(deftest frse-env-component
  (is (= [['foo 'bar] nil]
         (run-freer-reader-state-exception (null-monad-command-config {::stuff 'foo} nil)
                                           (monadic [x (get-env-component ::stuff)
                                                     y (with-env-component ::stuff (constantly 'bar)
                                                         (get-env-component ::stuff))]
                                                    (return [x y]))))))

(deftest frse-state
  (is (= [{:x 'foo} {:x 'bar}]
         (run-freer-reader-state-exception (null-monad-command-config 'nil {:x 'foo})
                                           (monadic [x (get-state)]
                                                    (put-state! {:x 'bar})
                                                    (return x))))))

(deftest null-config
  (let [c (null-monad-command-config nil nil)]
    (is (unknown-command? ((monad-command-config-run-command c)
                           (constantly nil) nil nil
                           'foo)))))


(defrecord Incr [])
(defn incr [] (Incr.))
(defn incr? [x] (instance? Incr x))

(defn run-incr
  [run-any env state comp]
  (cond
    (incr? comp)
    [(::count state) (update state ::count inc)]

    :else unknown-command))

(defn run-incr-config
  [initial]
  (make-monad-command-config run-incr {} {::count initial}))

(deftest combined
  (let [c (combine-monad-command-configs
           (run-ask-tell-config {"what's your first name?" "Mike"
                                 "what's your last name?" "Sperber"})
           (run-incr-config 15))]
    (is (= [15 {::output [] ::count 16}]
           (run-freer-reader-state-exception c (incr))))
    (is (= [15 {::output ["Hello, Mike Sperber"] ::count 16}]
           (run-freer-reader-state-exception c
                                             (monadic
                                              (ex1)
                                              (incr)))))))

#?(:clj
(deftest execute
  (is
   (thrown? Exception
            (execute-freer-reader-state-exception (combine-monad-command-configs)
                                                 (signal (Exception. "foo")))))))

(deftest reify-command-test
  (let [m (return 42)]
    (is (= :blub
           (reify-command (reify-as m :blub))))
    (is (= m
           (reify-command m)))))

; inlining this results in no metadata; go figure
(defn fake-return 
  [result]
  `(return ~result))

(deftest metadata-test
  (let [stmt (monadic
              [a (fake-return 42)] ; plain return will collapse everything to a single return
              [b (fake-return 21)]
              (return 10))]
    (let [base (meta stmt)]
      (is (= #{:line :column :statement}
             (set (keys (select-keys base #{:line :column :statement})))))
      
      (is (= '[a (fake-return 42)]
             (:statement base)))
      
      (is (= {:statement '[b (fake-return 21)]
              :column (:column base)
              :line (inc (or (:line base) -1))}
             (select-keys (meta (apply-continuations (bind-continuations stmt) nil)) #{:line :column :statement})))))
  
  ;; and we don't need/want metadata on this:
  (is (= nil
         (meta (monadic (return 42)))))
  (is (= (return 42)
         (monadic (return 42)))))

(deftest call-cc-test
  (testing "non-tail escape call"
    (let [f (fn [cont] (monadic
                        (cont 42)
                        (return 23)))]
      (is (= [23 nil]
             (execute-monadic
              (null-monad-command-config nil nil)
              (f return))))
      (is (= [42 nil]
             (execute-monadic
              (null-monad-command-config nil nil)
              (monadic
               [r (call-cc f)]
               (return r)))))
      (is (= [42 nil]
             (execute-monadic
              (null-monad-command-config nil nil)
              (call-cc f))))))
  (testing "tail escape call"
    (let [f (fn [cont] (cont 42))]
      (is (= [42 nil]
             (execute-monadic
              (null-monad-command-config nil nil)
              (f return))))
      (is (= [42 nil]
             (execute-monadic
              (null-monad-command-config nil nil)
              (monadic
               [r (call-cc f)]
               (return r)))))
      (is (= [42 nil]
             (execute-monadic
              (null-monad-command-config nil nil)
              (call-cc f)))))))
