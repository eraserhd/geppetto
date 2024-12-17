(ns net.eraserhead.geppetto.gcode.parse
  (:require
   [blancas.kern.core :as k :refer [<:> <|> <*> <$> << >> >>= bind return many many1 optional skip fwd]]
   [clojure.string :as str]
   [meander.epsilon :as m]))

(def ws (<|> k/space k/tab))

(defn skip-ws [p]
  (<:> (>> (many ws) p)))

(defn sym [c]
  (skip-ws (k/sym- c)))

(defn token
  ([s]
   (<:> (>>= (apply <*> (map sym s))
             #(return (symbol (apply str %))))))
  ([s & more]
   (apply <|> (map token (cons s more)))))

(def digit (skip-ws k/digit))

(def decimal
  (<:> (bind [sign   (<|> (sym \+) (sym \-) (return \+))
              body   (<|> (<*> (many1 digit)
                               (sym \.)
                               (many digit))
                          (<*> (sym \.)
                               (many1 digit)))]
            (return (Double/parseDouble (apply str sign (flatten body)))))))

(def integer
  (<:> (bind [sign   (<|> (sym \+) (sym \-) (return \+))
              digits (many1 digit)]
        (return (Long/parseLong (apply str sign digits))))))

(def line-number
  (bind [_ (sym \N)
         a integer
         b (optional (>> (sym \.) integer))]
    (return [:net.eraserhead.geppetto.gcode/line-number (if (some? b)
                                                          [a b]
                                                          [a])])))

(declare real-value)

(defn left-associative-binary-operation [term op]
  (bind [lhs term
         rhses (many (<*> op term))]
    (return (reduce (fn [lhs [op rhs]]
                      (list op lhs rhs))
                    lhs
                    rhses))))

(def binary-operation1
  (bind [lhs   (fwd real-value)
         rhses (many (>> (token "**") (fwd real-value)))]
    (return (reduce #(list '** %2 %1)
                    (reverse (cons lhs rhses))))))

(def binary-operation2
  (left-associative-binary-operation
    binary-operation1
    (token "/" "mod" "*")))

(def binary-operation3
  (left-associative-binary-operation
    binary-operation2
    (token "+" "-")))

(def binary-operation4
  (left-associative-binary-operation
    binary-operation3
    (token "eq" "ne" "gt" "ge" "lt" "le")))

(def binary-operation5
  (left-associative-binary-operation
    binary-operation4
    (token "and" "xor" "or")))

(def expression
  (k/between (sym \[) (sym \]) binary-operation5))

(def parameter-name-char
  (skip-ws (<$> #(Character/toLowerCase %) (k/satisfy (complement #{\( \) \>})))))
(def parameter-name
  (bind [_  (sym \<)
         cs (many parameter-name-char)
         _  (sym \>)]
    (return (apply str cs))))
(def parameter-index
  (<|> (fwd real-value) parameter-name))
(def parameter-value
  (>>= (>> (sym \#) parameter-index)
       #(return (list 'parameter %))))

(def ordinary-unary-combo
  (bind [operator (token "abs" "acos" "asin" "cos" "exp" "fix" "fup" "ln" "round"
                         "sin" "sqrt" "tan")
         operand expression]
    (return (list operator operand))))

(def exists-combo
  (bind [_ (token "exists[#")
         i parameter-index
         _ (sym \])]
    (return (list 'exists i))))

(def arc-tangent-combo
  (bind [_ (token "atan")
         a expression
         _ (sym \/)
         b expression]
    (return (list 'atan a b))))

(def unary-combo
  (<|> ordinary-unary-combo arc-tangent-combo exists-combo))

(def real-value
  (<|> decimal integer expression parameter-value unary-combo))

(def normal-letters "ABCDFGHIJKLMPQRSTUVWXYZ")
(defn normal-letter [c]
  (>> (sym c) (return (keyword "net.eraserhead.geppetto.gcode" (str c)))))
(def mid-line-letter
  (apply <|> (map normal-letter normal-letters)))
(def mid-line-word
  (<*> mid-line-letter real-value))

(def format-specifier
  (>>= (>> (k/sym- \%)
           (<|> (>> (sym \l) (sym \f) (return 6))
                (>> (sym \d) (return 0))
                (>> (sym \f) (return 4))
                (bind [_ (sym \.)
                       d digit
                       _ (sym \f)]
                  (return (Character/digit d 10)))))
       #(return [:net.eraserhead.geppetto.gcode/format-decimals %])))

(def inline-comment
  (let [text             (>>= (many (k/satisfy (complement #{\( \)})))
                              #(return (apply str %)))
        special          (fn [prefix p]
                           (let [kw-name (if (str/ends-with? prefix ",")
                                           (subs prefix 0 (dec (count prefix)))
                                           prefix)
                                 kw      (keyword "net.eraserhead.geppetto.gcode" kw-name)]
                             (skip-ws (bind [_    (apply skip (map sym prefix))
                                             body p
                                             _    (sym \))]
                                        (return (into [kw] body))))))

        parameter-value  (>>= (>> (k/sym- \#) (<|> real-value parameter-name))
                              #(return [:net.eraserhead.geppetto.gcode/parameter %]))

        non-var-text    (>>= (many1 (k/satisfy (complement #{\# \% \( \)})))
                             #(return [:net.eraserhead.geppetto.gcode/text (apply str %)]))
        text-and-vars   (many (<|> parameter-value
                                   format-specifier
                                   non-var-text))]

    (>> (k/sym- \()
        (<|> (special "debug," text-and-vars)
             (special "log," text-and-vars)
             (special "logappend," (<$> vector text))
             (special "logopen," (<$> vector text))
             (special "logclose" (return nil))
             (special "msg," (<$> vector text))
             (special "print," text-and-vars)
             (special "probeopen" (>> (k/skip-many1 ws) (<$> vector text)))
             (special "probeclose" (return nil))
             (>>= (<< text (sym \)))
                  #(return [:net.eraserhead.geppetto.gcode/comment %]))))))

(def parameter-setting
  (bind [_ (sym \#)
         i parameter-index
         _ (sym \=)
         v real-value]
    (return [:net.eraserhead.geppetto.gcode/parameter= i v])))

(def segment
  (<|> mid-line-word inline-comment parameter-setting))

(def o-code
  (bind [_ (sym \o)
         o real-value
         _ (many ws)
         t (<|> (token "sub")
                (token "endsub")
                (token "return"))
         _ (many ws)]
    (return [(keyword "net.eraserhead.geppetto.gcode" (name t)) o])))

(def line
  (bind [block-delete (optional (sym \/))
         line-number  (optional line-number)
         main-part    (<|> o-code (many segment))
         _            (many ws)]
    (return (vec (concat
                  (if block-delete [:net.eraserhead.geppetto.gcode/block-delete])
                  (if line-number  [line-number])
                  main-part)))))
