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
line                     = ['/'] [line_number] {segment} .

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

(defn- spacey-incasey-prefix-matcher
  "Is prefix a prefix of full-text, ignoring case and any whitespace in full-text?

  Returns false or the remainder of the text for further processing."
  [prefix]
  (fn [full-text]
    (loop [i 0
           j 0]
      (cond
       (= (count prefix) j)
       (subs full-text i)

       (= (count full-text) i)
       nil

       (= (Character/toLowerCase (nth full-text i))
          (Character/toLowerCase (nth prefix j)))
       (recur (inc i) (inc j))

       (Character/isWhitespace (nth full-text i))
       (recur (inc i) j)

       :else
       nil))))

(defn- find-comment-parameters [text]
  (->> (re-seq #"(?:([^#%]+)|#\s*(\d+)|#\s*<([^>]*)>|%(d|f|lf)|%\.(\d)f)" text)
       (map (fn [matches]
              (m/match matches
               [_ (m/some ?text) _ _ _ _]         [::text ?text]
               [_ _ (m/some ?param-number) _ _ _] [::parameter (Long/parseLong ?param-number)]
               [_ _ _ (m/some ?param-name) _ _]   [::parameter (normalize-line ?param-name)]
               [_ _ _ _ "d" _]                    [::format-decimals 0]
               [_ _ _ _ "f" _]                    [::format-decimals 4]
               [_ _ _ _ "lf" _]                   [::format-decimals 6]
               [_ _ _ _ _ (m/some ?digit)]        [::format-decimals (Long/parseLong ?digit)])))))

(m/defsyntax special-comment [prefix pat]
  `(m/app (spacey-incasey-prefix-matcher ~prefix) (m/some ~pat)))
(m/defsyntax text [pat]
  `(m/app (partial apply str) ~pat))

(defn- line->map [& words]
  (reduce (fn [line word]
            (m/match word
              [:line_number & ?line_number]
              (assoc line ::line-number ?line_number)

              [::F ?F-value]
              (assoc line ::F ?F-value)

              [:parameter_setting ?p ?value]
              (update line ::parameter= #(conj (or % []) [?p ?value]))

              ?word
              (update line ::words #(conj (or % []) ?word))))
          {}
          words))

(defn- binary-operation? [kw]
  (and (keyword? kw)
       (str/starts-with? (name kw) "binary_operation")))

(defn- fixup [tree]
  (m/match tree
    [:arc_tangent_combo (m/cata ?a) (m/cata ?b)]             (m/subst (atan ?a ?b))
    [(m/pred binary-operation?) (m/cata ?a)]                 (m/subst ?a)
    [(m/pred binary-operation?) (m/cata ?a) ?op (m/cata ?b)] (m/subst ((m/app symbol ?op) ?a ?b))
    [:comment & (text (special-comment "debug," ?text))]     (into [::debug] (find-comment-parameters ?text))
    [:comment & (text (special-comment "log," ?text))]       (into [::log] (find-comment-parameters ?text))
    [:comment & (text (special-comment "logappend," ?text))] [::logappend ?text]
    [:comment & (text (special-comment "logclose" (m/pred str/blank?)))]   [::logclose]
    [:comment & (text (special-comment "logopen," ?text))]   [::logopen ?text]
    [:comment & (text (special-comment "msg," ?text))]       [::message ?text]
    [:comment & (text (special-comment "print," ?text))]     (into [::print] (find-comment-parameters ?text))
    [:comment & (text (special-comment "probeclose" (m/pred str/blank?)))] [::probeclose]
    [:comment & (text (special-comment "probeopen " ?text))] [::probeopen ?text]
    [:comment & (text ?text)]                                [::comment ?text]
    [:exists_combo (m/cata ?varname)]                        (m/subst (exists ?varname))
    [:integer & (text ?digits)]                              (Long/parseLong ?digits)
    [:line . (m/cata !words) ...]                            (apply line->map !words)
    [:mid_line_letter ?letter]                               (keyword "net.eraserhead.geppetto.gcode" (str/upper-case ?letter))
    [:mid_line_word . (m/cata !args) ...]                    (vec !args)
    [:ordinary_unary_operation ?op]                          (m/subst (m/app symbol ?op))
    [:ordinary_unary_combo (m/cata ?op) (m/cata ?arg)]       (m/subst (?op ?arg))
    [:parameter_name & (text ?parts)]                        ?parts
    [:parameter_value (m/cata ?name)]                        (m/subst (parameter ?name))
    [:decimal & (text ?parts)]                               (Double/parseDouble ?parts)
    [(m/pred keyword? ?kw) . (m/cata !args) ...]             (m/subst [?kw . !args ...])
    ?other                                                   (m/subst ?other)))

(def parse-line (comp fixup parser normalize-line))
