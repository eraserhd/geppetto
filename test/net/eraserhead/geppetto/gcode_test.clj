(ns net.eraserhead.geppetto.gcode-test
  (:require
   [clojure.test :refer [deftest are]]
   [net.eraserhead.geppetto.gcode :as gcode]))

(deftest t-parse-line
  (are [line tree] (= tree (gcode/parse-line line))
    "N105"         [[::gcode/line-number 105]]
    "n105"         [[::gcode/line-number 105]]
    " n105"        [[::gcode/line-number 105]]
    "\t n105"      [[::gcode/line-number 105]]
    "n105 \t\t"    [[::gcode/line-number 105]]
    "n 105"        [[::gcode/line-number 105]]
    "n1 2\t3  4 5" [[::gcode/line-number 12345]]
    "G-1.4"        [[::gcode/G -1.4]]
    "g-1.4"        [[::gcode/G -1.4]]
    "F2400"        [[::gcode/F 2400.0]]
    "f2400"        [[::gcode/F 2400.0]]
    "f-4"          [[::gcode/F -4.0]]
    "f+4"          [[::gcode/F +4.0]]
    "f.4"          [[::gcode/F 0.4]]
    "f-.4"         [[::gcode/F -0.4]]
    "f+.4"         [[::gcode/F +0.4]]
    "g1x4.2f99z6"  [[::gcode/G 1.0] [::gcode/X 4.2] [::gcode/F 99.0] [::gcode/Z 6.0]]))
