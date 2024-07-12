(ns net.eraserhead.geppetto.gcode
  "Implement of g-code parsing per RS274/NGC Interpreter."
  (:require
   [clojure.string :as str]
   [instaparse.core :as insta]))

(def parser
  (insta/parser
    "
<line>                   = <{white_space}> + ['/'] + [line_number] + {segment} + <{white_space}>.

arc_tangent_combo        = 'ATAN' + expression + '/' + expression .
binary_operation         = binary_operation1 | binary_operation2 | binary_operation3 .
binary_operation1        = '**' .
binary_operation2        = '/' | 'MOD' | '*' .
binary_operation3        = 'AND' | 'XOR' | '-' | 'OR' | '+' .
comment                  = message | ordinary_comment .
comment_character        = #'[^()]' .
digit                    = <{white_space}> + ('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9') .
expression               = '[' + real_value + { binary_operation + real_value } + ']' .
line_number              = <'N'> + digit + [digit] + [digit] + [digit] + [digit] .
message                  = '(' + {white_space} + 'M' + {white_space} + 'S' +
                           {white_space} + 'G' + {white_space} + ',' +
                           {comment_character} + ')' .
mid_line_letter          = <{white_space}> #'[AaBbCcDdFfGgHhIiJjKkLlMmPpQqRrSsTtXxYyZz]'
<mid_line_word>          = mid_line_letter + real_value .
ordinary_comment         = '(' + {comment_character} + ')' .
ordinary_unary_combo     = ordinary_unary_operation + expression .
ordinary_unary_operation = 'ABS' | 'ACOS' | 'ASIN' | 'COS' | 'EXP' |
                           'FIX' | 'FUP' | 'LN' | 'ROUND' | 'SIN' |
                           'SQRT' | 'TAN' .
parameter_index          = real_value .
parameter_setting        = '#' + parameter_index + '=' + real_value .
parameter_value          = '#' + parameter_index .
real_number              = <{white_space}> + [ '+' | '-' ] +
                           (( digit + { digit } + [<{white_space}> '.'] + {digit}) |
                            (<{white_space}> + '.' + digit + {digit})) .
<real_value>             = real_number | expression | parameter_value | unary_combo .
segment                  = mid_line_word | comment | parameter_setting .
unary_combo              = ordinary_unary_combo | arc_tangent_combo .
white_space              = ' ' | '\t' .
   "
   :string-ci true))

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
