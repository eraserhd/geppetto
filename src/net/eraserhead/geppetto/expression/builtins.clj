(ns net.eraserhead.geppetto.expression.builtins)

(defn **
  "Raise a to power b."
  [a b]
  (Math/pow a b))
 
;; * MOD

;; + -

(defn eq
  "Determine if two numbers are equal.

  Per section 7 of the LinuxCNC G-code Overview, values are considered equal if their
  absolute difference is less than 1e-6."
  {:test #(do
            (assert (eq 0 0))
            (assert (eq 0 1e-7))
            (assert (eq 0 9e-7))
            (assert (not (eq -1e-6 1e-6)))
            (assert (not (eq 1 2)))
            (assert (not (eq 0 1e-6))))}
  [a b]
  (< (abs (- a b)) 1e-6))

(defn ne [a b]
  (not (eq [a b])))

(defn gt
  {:test #(do
            (assert (gt 2 1))
            (assert (not (gt 0 1e-6))))}
  [a b]
  (and (not (eq a b))
       (> a b)))

(defn ge
  {:test #(do
            (assert (ge 2 1)))}
  [a b]
  (or (eq a b) (> a b)))

;; GT LT LE

;; AND OR XOR
