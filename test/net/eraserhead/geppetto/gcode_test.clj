(ns net.eraserhead.geppetto.gcode-test
  (:require
   [clojure.test :refer [deftest are]]
   [net.eraserhead.geppetto.gcode :as gcode]))

(deftest t-parse-line
  (are [line tree] (= tree (gcode/parse-line line))
    "N105"  [[::gcode/line-number 105]]
    "n105"  [[::gcode/line-number 105]]
    "G-1.4" [[::gcode/G -1.4]]
    "g-1.4" [[::gcode/G -1.4]]
    "F2400" [[::gcode/F 2400]]
    "f2400" [[::gcode/F 2400]]))
