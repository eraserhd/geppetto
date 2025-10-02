(ns net.eraserhead.geppetto.gcode
  "Implement of g-code parsing per RS274/NGC Interpreter."
  (:require
   [blancas.kern.core :as k]
   [net.eraserhead.geppetto.gcode.parse :as p]))

(def order-of-execution
  ;; Where does E fit for 3d printers?
  [::O
   ::comment
   ::feed-rate-mode ; Set feed rate mode  (G93, G94).
   ::F
   ::S
   ::T
   ::HAL ; HAL pin I/O (M62-M68).
   ; Change tool (M6) and Set Tool Number (M61).
   ; Spindle on or off (M3, M4, M5).
   ; Save State (M70, M73), Restore State (M72), Invalidate State (M71).
   ; Coolant on or off (M7, M8, M9).
   ; Enable or disable overrides (M48, M49,M50,M51,M52,M53).
   ; User-defined Commands (M100-M199).
   ; Dwell (G4).
   ; Set active plane (G17, G18, G19).
   ; Set length units (G20, G21).
   ; Cutter radius compensation on or off (G40, G41, G42)
   ; Cutter length compensation on or off (G43, G49)
   ; Coordinate system selection (G54, G55, G56, G57, G58, G59, G59.1, G59.2, G59.3).
   ; Set path control mode (G61, G61.1, G64)
   ; Set distance mode (G90, G91).
   ; Set retract mode (G98, G99).
   ; Go to reference location (G28, G30) or change coordinate system data (G10) or set axis offsets (G52, G92, G92.1, G92.2, G94).
   ; Perform motion (G0 to G3, G33, G38.n, G73, G76, G80 to G89), as modified (possibly) by G53.
   ::stop ; Stop (M0, M1, M2, M30, M60).
   ,])

(defn parse-line
  "Parse a line of gcode.

  The result is a vector of block components.  Every block component
  except ::block-delete is inside a vector.  See net.eraserhead.geppetto.gcode-test
  for a full list of items."
  [line]
  (-> (k/parse p/line line)
      :value
      (with-meta {:source-text line})))
