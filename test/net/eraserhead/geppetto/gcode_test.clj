(ns net.eraserhead.geppetto.gcode-test
  (:require
   [clojure.test :refer [deftest are]]
   [net.eraserhead.geppetto.gcode :as gcode]))

(deftest t-parse-line
  (are [line result] (= result (gcode/parse-line line))
    "" 42))
