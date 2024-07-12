(ns net.eraserhead.geppetto.gcode
  "Implement of g-code parsing per RS274/NGC Interpreter."
  (:require
   [instaparse.core :as insta]))

(def parser
  (insta/parser
    "
line                     = ['/'] + [line_number] + {segment} .

arc_tangent_combo        = 'ATAN' + expression + '/' + expression .
binary_operation         = binary_operation1 | binary_operation2 | binary_operation3 .
binary_operation1        = '**' .
binary_operation2        = '/' | 'MOD' | '*' .
binary_operation3        = 'AND' | 'XOR' | '-' | 'OR' | '+' .
comment                  = message | ordinary_comment .
comment_character        = #'[^()]' .
digit                    = '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' .
expression               = '[' + real_value + { binary_operation + real_value } + ']' .
letter_a                 = 'A' | 'a' .
letter_b                 = 'B' | 'b' .
letter_c                 = 'C' | 'c' .
letter_d                 = 'D' | 'd' .
letter_f                 = 'F' | 'f' .
letter_g                 = 'G' | 'g' .
letter_h                 = 'H' | 'h' .
letter_i                 = 'I' | 'i' .
letter_j                 = 'J' | 'j' .
letter_k                 = 'K' | 'k' .
letter_l                 = 'L' | 'l' .
letter_m                 = 'M' | 'm' .
letter_n                 = 'N' | 'n' .
letter_p                 = 'P' | 'p' .
letter_q                 = 'Q' | 'q' .
letter_r                 = 'R' | 'r' .
letter_s                 = 'S' | 's' .
letter_t                 = 'T' | 't' .
letter_x                 = 'X' | 'x' .
letter_y                 = 'Y' | 'y' .
letter_z                 = 'Z' | 'z' .
line_number              = <letter_n> + digit + [digit] + [digit] + [digit] + [digit] .
message                  = '(' + {white_space} + letter_m + {white_space} + letter_s +
                           {white_space} + letter_g + {white_space} + ',' +
                           {comment_character} + ')' .
<mid_line_letter>        = letter_a | letter_b | letter_c | letter_d | letter_f |
                           letter_g | letter_h | letter_i | letter_j | letter_k |
                           letter_l | letter_m | letter_p | letter_q | letter_r |
                           letter_s | letter_t | letter_x | letter_y | letter_z .
<mid_line_word>          = mid_line_letter + real_value .
ordinary_comment         = '(' + {comment_character} + ')' .
ordinary_unary_combo     = ordinary_unary_operation + expression .
ordinary_unary_operation = 'ABS' | 'ACOS' | 'ASIN' | 'COS' | 'EXP' |
                           'FIX' | 'FUP' | 'LN' | 'ROUND' | 'SIN' |
                           'SQRT' | 'TAN' .
parameter_index          = real_value .
parameter_setting        = '#' + parameter_index + '=' + real_value .
parameter_value          = '#' + parameter_index .
real_number              = [ '+' | '-' ] +
                           (( digit + { digit } + ['.'] + {digit}) |
                            ( '.' + digit + {digit})) .
<real_value>             = real_number | expression | parameter_value | unary_combo .
segment                  = mid_line_word | comment | parameter_setting .
unary_combo              = ordinary_unary_combo | arc_tangent_combo .
white_space              = ' ' | '\t' .
   "))

(defn parse-line [line]
  (->> (parser line)
       (insta/transform
        {:letter_g (constantly ::G)

         :line
         vector

         :line_number
         (fn line-number* [& digits]
           [::line-number (->> digits
                               (map second)
                               (apply str)
                               read-string)])

         :real_number
         (fn real-number* [& parts]
           (->> (vec parts)
                (tree-seq vector? seq)
                (filter string?)
                (apply str)
                read-string))

         :segment
         vector})))

(parse-line "N004G1X200")
