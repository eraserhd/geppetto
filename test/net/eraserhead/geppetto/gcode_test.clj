(ns net.eraserhead.geppetto.gcode-test
  (:require
   [clojure.test :refer [deftest are]]
   [net.eraserhead.geppetto.gcode :as gcode]))

(deftest t-word-keyword
  (are [letter kw] (= kw (gcode/word-keyword letter))
    "G" ::gcode/G
    "X" ::gcode/X
    "(" ::gcode/comment
    ";" ::gcode/comment))

(deftest t-parse-words
  (are [line words] (= words (gcode/parse-words line))
    "N100"  [[::gcode/N 100]]))

;(deftest t-parse-line
;  (are [line k result] (= result (k (gcode/parse-line line)))
;    ""        ::gcode/source ""
;    "; hello" ::gcode/source "; hello"
;    "N100"    ::gcode/source "N100"
;
;    "N100"    ::gcode/N      100))
