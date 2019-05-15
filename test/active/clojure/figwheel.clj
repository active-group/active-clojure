(ns active.clojure.figwheel
  (:require  [compojure.core :as compojure]))

(def figwheel-test-handler
  (do
    (compojure/routes
     (compojure/GET "/test"
                    []
                    "<!DOCTYPE html><html><body><h1>Test host page</h1><script src=\"cljs-out/test-main.js\" type=\"text/javascript\"></script></body></html>"))))
