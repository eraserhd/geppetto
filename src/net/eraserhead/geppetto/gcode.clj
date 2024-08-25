(ns net.eraserhead.geppetto.gcode
  "Implement of g-code parsing per RS274/NGC Interpreter."
  (:require
   [clojure.string :as str]
   [instaparse.core :as insta]
   [meander.epsilon :as m]
   [meander.strategy.epsilon :as r]))

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

(m/defsyntax text [pat]
  `(m/app (partial apply str) ~pat))
(m/defsyntax comment-text
  ([prefix]
   `(comment-text ~prefix (m/pred str/blank?)))
  ([prefix pat]
   `[:comment & (text (m/app (spacey-incasey-prefix-matcher ~prefix) (m/some ~pat)))]))

(def ^:private fix-map
  (r/repeat
   (r/rewrite
    {::words [],
     & ?rest}
    ?rest

    {::words [[:line_number & ?line-number] & ?words],
     & ?rest}
    {::line-number ?line-number,
     ::words ?words,
     & ?rest}

    {::words [!xs ... [::comment & ?comment] . !ys ...],
     & ?rest}
    {::words [!xs ... . !ys ...],
     ::comment ?comment,
     & ?rest}

    {::words [!xs ... (m/and ?mode (m/or ::G93 ::G94)) . !ys ...]
     & ?rest}
    {::feed-rate-mode ?mode,
     ::words [!xs ... . !ys ...]
     & ?rest}

    {::words [!xs ... [(m/and ?type (m/or ::F ::S ::T)) ?value] . !ys ...],
     & ?rest}
    {?type ?value
     ::words [!xs ... . !ys ...]
     & ?rest}
    
    {::words [!xs ... (m/and ?mode (m/or ::M0 ::M1 ::M2 ::M30 ::M60)) . !ys ...]
     & ?rest}
    {::stop ?mode,
     ::words [!xs ... . !ys ...],
     & ?rest}
    
    {::parameter= (m/or [!parameter=s ...] nil)
     ::words [!xs ... [:parameter_setting ?param ?value] . !ys ...],
     & ?rest}
    {::parameter= [!parameter=s ... . [?param ?value]]
     ::words [!xs ... . !ys ...]
     & ?rest})))

(defn- binary-operation? [kw]
  (and (keyword? kw)
       (str/starts-with? (name kw) "binary_operation")))

(defn- keywordize-g-or-m-code [letter number]
  (let [rounded     (Math/round (double number))
        diff        (Math/abs (- number rounded))
        number-str  (if (< diff 0.05)
                      (str (long rounded))
                      (format "%.1f" number))]
    (keyword
      "net.eraserhead.geppetto.gcode"
      (str letter number-str))))

(defn- fixup [tree]
  (m/rewrite tree
    [:arc_tangent_combo (m/cata ?a) (m/cata ?b)]             (atan ?a ?b)
    [(m/pred binary-operation?) (m/cata ?a)]                 ?a
    [(m/pred binary-operation?) (m/cata ?a) ?op (m/cata ?b)] ((m/symbol ?op) ?a ?b)
    (comment-text "debug," ?text)                            [::comment ::debug & (m/app find-comment-parameters ?text)]
    (comment-text "log," ?text)                              [::comment ::log & (m/app find-comment-parameters ?text)]
    (comment-text "logappend," ?text)                        [::comment ::logappend ?text]
    (comment-text "logclose")                                [::comment ::logclose]
    (comment-text "logopen," ?text)                          [::comment ::logopen ?text]
    (comment-text "msg," ?text)                              [::comment ::message ?text]
    (comment-text "print," ?text)                            [::comment ::print & (m/app find-comment-parameters ?text)]
    (comment-text "probeclose")                              [::comment ::probeclose]
    (comment-text "probeopen " ?text)                        [::comment ::probeopen ?text]
    [:comment & (text ?text)]                                [::comment ::comment ?text]
    [:exists_combo (m/cata ?varname)]                        (exists ?varname)
    [:integer & (text ?digits)]                              (m/app Long/parseLong ?digits)
    [:line . (m/cata !words) ...]                            (m/app fix-map {::words [!words ...]})
    [:mid_line_letter ?letter]                               (m/keyword "net.eraserhead.geppetto.gcode" (m/app str/upper-case ?letter))
    [:mid_line_word [:mid_line_letter (m/or "g" "G")] (m/cata ?n)]    (m/app keywordize-g-or-m-code "G" ?n)
    [:mid_line_word [:mid_line_letter (m/or "m" "M")] (m/cata ?n)]    (m/app keywordize-g-or-m-code "M" ?n)
    [:mid_line_word . (m/cata !args) ...]                    [!args ...]
    [:ordinary_unary_operation ?op]                          (m/symbol ?op)
    [:ordinary_unary_combo (m/cata ?op) (m/cata ?arg)]       (?op ?arg)
    [:parameter_name & (text ?parts)]                        ?parts
    [:parameter_value (m/cata ?name)]                        (parameter ?name)
    [:decimal & (text ?parts)]                               (m/app Double/parseDouble ?parts)
    [(m/pred keyword? ?kw) . (m/cata !args) ...]             [?kw . !args ...]
    ?other                                                   ?other))

(def parse-line (comp fixup parser normalize-line))
