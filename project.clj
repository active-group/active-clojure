(defproject de.active-group/active-clojure "0.44.1"
  :description "Active Clojure: Various Clojure utilities in use at Active Group"
  :url "http://github.com/active-group/active-clojure"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.0" :scope "provided"]
                 [org.clojure/clojurescript "1.10.238" :scope "provided"]
                 [io.aviso/pretty "1.1.1"]
                 [org.clojure/core.match "1.0.0"]
                 [org.clojure/test.check "0.10.0-alpha4"]]

  :generated-paths ["target"]

  :profiles {;; to use figwheel-main
             ;; run `lein fig` and then open browser at
             ;; http://localhost:9500/figwheel-extra-main/auto-testing
             :dev {:dependencies   [[com.bhauman/figwheel-main "0.2.0"]
                                    [com.bhauman/rebel-readline-cljs "0.1.4"]
                                    [compojure "1.6.1"]]
                   :source-paths ["src" "dev"]
                   :clean-targets ^{:protect false} ["resources/public/js/compiled"
                                                     "resources/public/cljs-out"
                                                     :target-path]}

             :codox {:dependencies [[codox-theme-rdash "0.1.2"]]}

             :cljs {:dependencies [[org.clojure/clojurescript "1.10.238"]
                                   [cider/piggieback "0.4.0"]
                                   [nrepl/nrepl "0.6.0"]]
                    ;; run CLJS repl with
                    ;; lein with-profile cljs repl
                    ;; (cemerick.piggieback/cljs-repl (cljs.repl.rhino/repl-env))
                    :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}}}

  :aliases {"fig" ["trampoline" "with-profile" "+dev,+test" "run" "-m" "figwheel.main" "-b" "dev" "-r"]
            "figtest" ["run" "-m" "figwheel.main" "-co" "test.cljs.edn" "-m" active.clojure.figwheel-test-runner]
            "figtest-headless" ["run" "-m" "figwheel.main" "-fwo" "{:launch-js [\"run-chrome.sh\" :open-url]}" "-co" "test.cljs.edn" "-m" active.clojure.figwheel-test-runner]}

  :plugins [[lein-codox "0.10.8"]]

  :codox {:language :clojure
          :metadata {:doc/format :markdown}
          :themes [:rdash]
          :src-uri "http://github.com/active-group/active-clojure/blob/main/"
          :src-linenum-anchor-prefix "L"}

  :global-vars {*warn-on-reflection* true})
