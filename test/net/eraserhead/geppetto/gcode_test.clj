(ns net.eraserhead.geppetto.gcode-test
  (:require
   [clojure.test :refer [deftest are]]
   [net.eraserhead.geppetto.gcode :as gcode]))

(deftest t-parse-line
  (are [line k result] (= result (k (gcode/parse-line line)))
    ""        ::gcode/source ""
    "; hello" ::gcode/source "; hello"
    "N100"    ::gcode/source "N100"))
