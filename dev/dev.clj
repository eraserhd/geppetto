(ns dev
  (:require
   [clojure.repl :refer [apropos doc dir source find-doc]]
   [clojure.tools.namespace.repl :refer [refresh]]
   [eftest.runner :as eftest]))

(defn run-tests
  ([]          (run-tests "src" "test"))
  ([& symbols] (eftest/run-tests (eftest/find-tests symbols) {:capture-output? false})))
