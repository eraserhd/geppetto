(ns net.eraserhead.geppetto.gcode-test
  (:require
   [clojure.test :refer [deftest are]]))

(deftest t-parse-line
  (are (= result (parse-line line)) [line result]
    "" nil))
