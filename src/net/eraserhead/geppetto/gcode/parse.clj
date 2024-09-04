(ns net.eraserhead.geppetto.gcode.parse
  (:require
   [blancas.kern.core :as k :refer [<:> <|> <*> <$> << >> >>= bind return many many1 optional skip fwd]]
   [clojure.string :as str]
   [meander.epsilon :as m]))

;; Each parser skips trailing whitespace and NOT leading whitespace, so that we
;; do not consume anything on failure.

(def ws (<|> k/space k/tab))

(defn skip-ws [p]
  (<:> (>> (many ws) p)))

(defn sym [c]
  (skip-ws (k/sym- c)))

(defn token [s]
  (<:> (>>= (apply <*> (map sym s))
            #(return (symbol (apply str %))))))

(def digit
  (skip-ws k/digit))

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
    (return (loop [lhs   lhs
                   rhses rhses]
              (if-some [[[op rhs] & rhses'] (seq rhses)]
                (recur (list op lhs rhs) rhses')
                lhs)))))

(def binary-operation1
  (bind [lhs   (fwd real-value)
         rhses (many (>> (token "**") (fwd real-value)))]
    (return (apply (fn rewrite*
                     ([a] a)
                     ([a & more] (list '** a (apply rewrite* more))))
                   (cons lhs rhses)))))

(def binary-operation2
  (left-associative-binary-operation
    binary-operation1
    (<|> (token "/")
         (token "mod")
         (token "*"))))

(def binary-operation3
  (left-associative-binary-operation
    binary-operation2
    (<|> (token "+")
         (token "-"))))

(def binary-operation4
  (left-associative-binary-operation
    binary-operation3
    (<|> (token "eq")
         (token "ne")
         (token "gt")
         (token "ge")
         (token "lt")
         (token "le"))))

(def binary-operation5
  (left-associative-binary-operation
    binary-operation4
    (<|> (token "and")
         (token "xor")
         (token "or"))))

(def expression
  (k/between (sym \[) (sym \]) binary-operation5))

(def real-value
  (<|> decimal integer expression))

(def normal-letters "ABCDFGHIJKLMPQRSTUVWXYZ")
(defn normal-letter [c]
  (>> (sym c) (return (keyword "net.eraserhead.geppetto.gcode" (str c)))))
(def mid-line-letter
  (apply <|> (map normal-letter normal-letters)))
(def mid-line-word
  (<*> mid-line-letter real-value))

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

        name-char        (skip-ws (k/satisfy (complement #{\> \( \)})))
        name             (<$> (partial apply str) (many name-char))
        parameter-name   (k/between (sym \<) (sym \>) name)
        parameter-value  (>>= (>> (k/sym- \#) (<|> real-value parameter-name))
                              #(return [:net.eraserhead.geppetto.gcode/parameter %]))

        format-specifier (>>= (>> (sym \%)
                                  (<|> (>> (sym \l) (k/sym- \f) (return 6))
                                       (>> (k/sym- \d) (return 0))
                                       (>> (k/sym- \f) (return 4))
                                       (bind [_ (sym \.)
                                              d digit
                                              _ (k/sym- \f)]
                                         (return (Character/digit d 10)))))
                              #(return [:net.eraserhead.geppetto.gcode/format-decimals %]))

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

(def segment
  (<|> mid-line-word
       inline-comment)) ; | comment | ...

(def line
  (bind [block-delete (optional (sym \/))
         line-number  (optional line-number)
         segments     (many segment)
         _            (many ws)]
    (return (vec (concat
                  (if block-delete [:net.eraserhead.geppetto.gcode/block-delete])
                  (if line-number  [line-number])
                  segments)))))
