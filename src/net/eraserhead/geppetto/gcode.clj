(ns net.eraserhead.geppetto.gcode
  "Implement of g-code parsing per RS274/NGC Interpreter."
  (:require
   [clojure.string :as str]
   [instaparse.core :as insta]
   [meander.epsilon :as m]))

(defn normalize-line
  "Removes any spaces, and lowercase letters, not in comments per RS274/NGC D.3.1."
  [s]
  (->> s
    (reduce (fn [[state out] ch]
              (m/match [state ch]
                [:start (m/or \space \tab)] [:start out]
                [:start \(]                 [\( (conj out \()]
                [:start \;]                 [\; (conj out \;)]
                [\(     \)]                 [:start (conj out \))]
                [\(     ?ch]                [\( (conj out ?ch)]
                [\;     ?ch]                [\; (conj out ?ch)]
                [?state ?ch]                [?state (conj out (Character/toLowerCase ?ch))]))
            [:start []])
    second
    (apply str)))

(def parser
  (insta/parser
    "
<line>                   = ['/'] [line_number] {segment} .

binary_operation1        = real_value ['**' binary_operation1] .
binary_operation2        = [binary_operation2 ('/' | 'mod' | '*')] binary_operation1 .
binary_operation3        = [binary_operation3 ('+' | '-')] binary_operation2 .
binary_operation4        = [binary_operation4 ('eq' | 'ne' | 'gt' | 'ge' | 'lt' | 'le')] binary_operation3 .
binary_operation5        = [binary_operation5 ('and' | 'xor' | 'or' )] binary_operation4 .
<expression>             = <'['> binary_operation5 <']'> .

<unary_combo>            = ordinary_unary_combo | arc_tangent_combo | exists_combo .
arc_tangent_combo        = <'atan'> expression <'/'> expression .
ordinary_unary_combo     = ordinary_unary_operation expression .
ordinary_unary_operation = 'abs' | 'acos' | 'asin' | 'cos' | 'exp' |
                           'fix' | 'fup' | 'ln' | 'round' | 'sin' |
                           'sqrt' | 'tan' .
exists_combo             = <'exists[#'> parameter_name <']'>

comment                  = <'('> {comment_character} <')'>.
<comment_character>      = #'[^()]' .

<digit>                  = '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' .
line_number              = <'N'> integer [<'.'> integer] .
mid_line_letter          = #'(?i)[ABCDFGHIJKLMPQRSTUVWXYZ]'
mid_line_word            = mid_line_letter + real_value .
<parameter_index>        = real_value | parameter_name .
parameter_name           = <'<'> {#'[^>]'} <'>'>
parameter_setting        = <'#'> parameter_index <'='> real_value .
parameter_value          = <'#'> parameter_index .
integer                  = [ '+' | '-' ] digit {digit} .
decimal                  = [ '+' | '-' ] (( digit {digit} '.' {digit}) | ('.' digit {digit})) .
<real_number>            = integer | decimal .
<real_value>             = real_number | expression | parameter_value | unary_combo .
<segment>                = mid_line_word | comment | parameter_setting .

<white_space>            = ' ' | '\t' .
   "
   :string-ci true))

(defn- binary-operation
  ([a]      a)
  ([a op b] (list (symbol op) a b)))
            
(defn- gcode-comment [& args]
  (let [full-text  (apply str args)
        [tag text] (str/split full-text #"," 2)
        tag        (str/lower-case (str/replace tag #"\s+" ""))]
    (m/match [tag text]
      ["debug" ?text]     [::debug ?text]
      ["log" ?text]       [::log ?text]
      ["logappend" ?text] [::logappend ?text]
      ["logopen" ?text]   [::logopen ?text]
      ["logclose" nil]    [::logclose]
      ["msg" ?text]       [::message ?text]
      ["print" ?text]     [::print ?text]
      ["probeclose" nil]  [::probeclose]
      _                   [::comment full-text])))

(defn parse-line [line]
  (->> line
       normalize-line
       parser
       (insta/transform
        {:arc_tangent_combo        #(list 'atan %1 %2)
         :binary_operation1        binary-operation
         :binary_operation2        binary-operation
         :binary_operation3        binary-operation
         :binary_operation4        binary-operation
         :binary_operation5        binary-operation
         :comment                  gcode-comment
         :debug                    #(vector ::debug (apply str %&))
         :exists_combo             #(list 'exists %1)
         :integer                  #(Long/parseLong (apply str %&))
         :line_number              #(vector ::line-number (vec %&))
         :message                  #(vector ::message (apply str %&))
         :mid_line_letter          #(keyword "net.eraserhead.geppetto.gcode" (str/upper-case %1))
         :mid_line_word            vector
         :ordinary_comment         #(vector ::comment (apply str %&))
         :ordinary_unary_operation symbol
         :ordinary_unary_combo     list
         :parameter_name           #(apply str %&)
         :parameter_setting        #(list ::parameter= %1 %2)
         :parameter_value          #(list 'parameter %1)
         :decimal                  #(Double/parseDouble (apply str %&))})))
