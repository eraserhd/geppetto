(ns net.eraserhead.geppetto.gcode
  "Implement of g-code parsing per RS274/NGC Interpreter."
  (:require
   [clojure.string :as str]))

(defn word-keyword [letter]
  (case letter
   (";" "(") ::comment
   (keyword "net.eraserhead.geppetto.gcode" letter)))

(defn parse-words [line]
  (str/split line #"(?=[ABCDFGHIJKLMNPQRSTXYZ])"))

;(defn parse-line [line]
;  "Parse a single line of g-code, presented as a string."
;  (let [words (str/split line #"(?=[ABCDFGHIJKLMNPQRSTXYZ])")]
;    (transduce (filter (complement empty?))
;               (fn [block word]
;                 (let [letter (subs word 0 1)
;                       value  (read-string (subs word 1))]
;                   (assoc block (word-keyword letter) value)))
;               {::source line}
;               words)))
