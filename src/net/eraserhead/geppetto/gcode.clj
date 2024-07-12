(ns net.eraserhead.geppetto.gcode
  "Implement of g-code parsing per RS274/NGC Interpreter."
  (:require
   [clojure.string :as str]
   [instaparse.core :as insta]))

(def parser
  (insta/parser
    "
<line>                   = <{white_space}> + ['/'] + [line_number] + {segment} .

arc_tangent_combo        = 'ATAN' + expression + '/' + expression .
binary_operation         = binary_operation1 | binary_operation2 | binary_operation3 .
binary_operation1        = '**' .
binary_operation2        = '/' | 'MOD' | '*' .
binary_operation3        = 'AND' | 'XOR' | '-' | 'OR' | '+' .
comment                  = message | ordinary_comment .
comment_character        = #'[^()]' .
digit                    = '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' .
expression               = '[' + real_value + { binary_operation + real_value } + ']' .
letter_g                 = 'G' | 'g' .
letter_m                 = 'M' | 'm' .
letter_s                 = 'S' | 's' .
line_number              = <'N' | 'n'> + digit + [digit] + [digit] + [digit] + [digit] .
message                  = '(' + {white_space} + letter_m + {white_space} + letter_s +
                           {white_space} + letter_g + {white_space} + ',' +
                           {comment_character} + ')' .
mid_line_letter          = #'[AaBbCcDdFfGgHhIiJjKkLlMmPpQqRrSsTtXxYyZz]'
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
        {:line_number
         (fn line_number* [& digits]
           [::line-number (->> digits
                               (map second)
                               (apply str)
                               read-string)])
 
         :mid_line_letter
         (fn mid_line_letter* [letter]
           (keyword "net.eraserhead.geppetto.gcode" (str/upper-case letter)))

         :real_number
         (fn real_number* [& parts]
           (->> (vec parts)
                (tree-seq vector? seq)
                (filter string?)
                (apply str)
                Double/parseDouble))

         :segment
         vector})))

(parse-line "N004G1X200")
