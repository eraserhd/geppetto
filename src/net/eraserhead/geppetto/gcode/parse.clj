(ns net.eraserhead.geppetto.gcode.parse
 (:require
  [blancas.kern.core :as k :refer [<:> <|> << >> >>= bind return many many1 optional]]
  [clojure.string :as str]))

;; Each parser skips trailing whitespace and NOT leading whitespace, so that we
;; do not consume anything on failure.

(def ws (<|> k/space k/tab))

(defn skip-trailing-ws [p]
  (<< p (many ws)))

(defn sym [c]
  (skip-trailing-ws (k/sym- c)))

(def digit
  (skip-trailing-ws k/digit))

(def decimal
  (bind [sign   (<|> (sym \+) (sym \-) (return \+))
         body   (<|> (k/<*> (many1 digit)
                            (sym \.)
                            (many digit)
                          (k/<*> (sym \.)
                                 (many1 digit))))]
    (return (Double/parseDouble (apply str sign (flatten body))))))

(def integer
  (bind [sign   (<|> (sym \+) (sym \-) (return \+))
         digits (many1 digit)]
    (return (Long/parseLong (apply str sign digits)))))

(def line-number
  (bind [_ (sym \N)
         a integer
         b (optional (>> (sym \.) integer))]
    (return [:net.eraserhead.geppetto.gcode/line-number (if (some? b)
                                                          [a b]
                                                          [a])])))

(def real-value
  (<|> (<:> decimal) integer))

(def normal-letters "ABCDFGHIJKLMPQRSTUVWXYZ")
(defn normal-letter [c]
  (>> (sym c) (return (keyword "net.eraserhead.geppetto.gcode" (str c)))))
(def mid-line-letter
  (apply <|> (map normal-letter normal-letters)))
(def mid-line-word
  (k/<*> mid-line-letter real-value))

(def inline-comment
  (let [text    (>>= (many (k/satisfy (complement #{\( \)})))
                     #(return (apply str %)))
        special (fn [prefix p]
                  (let [kw-name (if (str/ends-with? prefix ",")
                                  (subs prefix 0 (dec (count prefix)))
                                  prefix)
                        kw      (keyword "net.eraserhead.geppetto.gcode" kw-name)]
                    (<:> (bind [_    (apply >> (many ws) (map sym prefix))
                                body p
                                _    (sym \))]
                           (return (vec (concat
                                         [kw]
                                         (when body [body]))))))))]
    (>> (k/sym- \()
        (<|> (special "debug," text)
             (special "log," text)
             (special "logappend," text)
             (special "logopen," text)
             (special "logclose" (return nil))
             (special "print" text)
             (special "probeopen" text)
             (special "probeclose" (return nil))
             (>>= (<< text (sym \)))
                  #(return [:net.eraserhead.geppetto.gcode/comment %]))))))

(def segment
  (<|> mid-line-word
       inline-comment)) ; | comment | ...

(def line
  (bind [_            (many ws)
         block-delete (optional (sym \/))
         line-number  (optional line-number)
         segments     (many segment)]
    (return (vec (concat
                  (if block-delete [:net.eraserhead.geppetto.gcode/block-delete])
                  (if line-number  [line-number])
                  segments)))))
