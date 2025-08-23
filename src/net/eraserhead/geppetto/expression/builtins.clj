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

(defn ne
  "Not equal."
  [a b]
  (not (eq [a b])))

(defn gt
  "Greater than, but not equal to (see eq)."
  {:test #(do
            (assert (gt 2 1))
            (assert (not (gt 0 1e-6))))}
  [a b]
  (and (not (eq a b))
       (> a b)))

(defn ge
  "Greater than or equal to (see eq)."
  {:test #(do
            (assert (ge 2 1)))}
  [a b]
  (or (eq a b) (> a b)))

(defn lt
  "Less than, but not equal to (see eq)."
  {:test #(do
            (assert (lt 1 2))
            (assert (not (lt 1 1)))
            (assert (not (lt 0 1e-7))))}
  [a b]
  (and (not (eq a b))
       (< a b)))

(defn le
  "Less than or equal to (see eq)."
  {:test #(do
            (assert (le 1 2))
            (assert (le 0 1e-7))
            (assert (le 1e-7 0)))}
  [a b]
  (or (eq a b) (< a b)))

;; AND OR XOR
