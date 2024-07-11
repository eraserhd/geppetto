(ns net.eraserhead.geppetto-test
  (:require
   [clojure.test :refer [deftest is]]))

(deftest t-foo
  (is (= 4 (+ 2 2))))
