(ns net.eraserhead.geppetto.gcode
  "Implement of g-code parsing per RS274/NGC Interpreter."
  (:require
   [clojure.string :as str]
   [instaparse.core :as insta]))


(defn normalize-line
  "Removes any spaces, and lowercase letters, not in comments per RS274/NGC D.3.1."
  [s]
  (->> s
    (reduce (fn [[state out] ch]
              (case [state ch]
                ([:start \space] [:start \tab]) [:start out]
                ([:start \(])                   [\( (conj out ch)]
                ([:start \;])                   [\; (conj out ch)]
                ([\( \)])                       [:start (conj out ch)]
                   
                (case state
                  (\( \;) [state (conj out ch)]
                  [state (conj out (Character/toLowerCase ch))])))
            [:start []])
    second
    (apply str)))



(def parser
  (insta/parser
    "
<line>                   = ['/'] [line_number] {segment} .

arc_tangent_combo        = <'atan'> expression <'/'> expression .
binary_operation1        = real_value ['**' binary_operation1] .
binary_operation2        = [binary_operation2 ('/' | 'mod' | '*')] binary_operation1 .
binary_operation3        = [binary_operation3 ('and' | 'xor' | '-' | 'or' | '+')] binary_operation2 .
<comment>                = message / ordinary_comment .
<comment_character>      = #'[^()]' .
<digit>                  = '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' .
<expression>             = <'['> binary_operation3 <']'> .
line_number              = <'N'> digit [digit] [digit] [digit] [digit] .
message                  = <'(' {white_space} 'm' {white_space} 's'
                           {white_space} 'g' {white_space} ','>
                           {comment_character} <')'> .
mid_line_letter          = #'(?i)[ABCDFGHIJKLMPQRSTXYZ]'
mid_line_word            = mid_line_letter + real_value .
ordinary_comment         = <'('> {comment_character} <')'> .
ordinary_unary_combo     = ordinary_unary_operation expression .
ordinary_unary_operation = 'abs' | 'acos' | 'asin' | 'cos' | 'exp' |
                           'fix' | 'fup' | 'ln' | 'round' | 'sin' |
                           'sqrt' | 'tan' .
<parameter_index>        = real_value .
parameter_setting        = <'#'> parameter_index <'='> real_value .
parameter_value          = <'#'> parameter_index .
real_number              = [ '+' | '-' ]
                           (( digit {digit} ['.'] {digit}) |
                            ('.' digit {digit})) .
<real_value>             = real_number | expression | parameter_value | unary_combo .
<segment>                = mid_line_word | comment | parameter_setting .
<unary_combo>            = ordinary_unary_combo | arc_tangent_combo .
white_space              = ' ' | '\t' .
   "
   :string-ci true))

(defn- binary-operation
  ([a]      a)
  ([a op b] (list (symbol op) a b)))

(defn parse-line [line]
  (->> line
       normalize-line
       parser
       (insta/transform
        {:arc_tangent_combo        #(apply list 'atan %&)
         :binary_operation1        binary-operation
         :binary_operation2        binary-operation
         :binary_operation3        binary-operation
         :line_number              #(vector ::line-number (Long/parseLong (apply str %&)))
         :message                  #(vector ::message (apply str %&))
         :mid_line_letter          #(keyword "net.eraserhead.geppetto.gcode" (str/upper-case %1))
         :mid_line_word            vector
         :ordinary_comment         #(vector ::comment (apply str %&))
         :ordinary_unary_operation symbol
         :ordinary_unary_combo     list
         :parameter_setting        #(list ::parameter= %1 %2)
         :parameter_value          #(list 'parameter %1)
         :real_number              #(Double/parseDouble (apply str %&))})))
