(ns net.eraserhead.geppetto.gcode
  "Implement of g-code parsing per RS274/NGC Interpreter.")

(defn parse-line [line]
  "Parse a single line of g-code, presented as a string."
  {::source line})
