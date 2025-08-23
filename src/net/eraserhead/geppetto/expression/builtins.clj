(ns net.eraserhead.geppetto.expression.builtins
  (:refer-clojure :exclude [and or]))

;; 6. Binary Operators

(defn **
  "Raise a to power b."
  [a b]
  (Math/pow a b))

;; * MOD + -  -- just imported: same as in Clojure

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
  (clojure.core/and
    (not (eq a b))
    (> a b)))

(defn ge
  "Greater than or equal to (see eq)."
  {:test #(do
            (assert (ge 2 1)))}
  [a b]
  (clojure.core/or
    (eq a b)
    (> a b)))

(defn lt
  "Less than, but not equal to (see eq)."
  {:test #(do
            (assert (lt 1 2))
            (assert (not (lt 1 1)))
            (assert (not (lt 0 1e-7))))}
  [a b]
  (clojure.core/and
    (not (eq a b))
    (< a b)))

(defn le
  "Less than or equal to (see eq)."
  {:test #(do
            (assert (le 1 2))
            (assert (le 0 1e-7))
            (assert (le 1e-7 0)))}
  [a b]
  (clojure.core/or
    (eq a b)
    (< a b)))

(defn- falsey? [x] (eq 0 x))
(defn- truthy? [x] (not (falsey? x)))

(defn and
  "Logical AND.

  Returns 1 if both arguments are non-zero, 0 otherwise."
  {:test #(do
            (assert (zero? (and 0 0)))
            (assert (zero? (and 0 1)))
            (assert (zero? (and 1 0)))
            (assert (zero? (and 9e-7 9e-7)))
            (assert (not (zero? (and 1 1)))))}
  [a b]
  (if (clojure.core/and (truthy? a) (truthy? b))
    1
    0))

(defn or
  "Logical OR.

  Returns 1 if either argument is non-zero, 0 otherwise."
  {:test #(do
            (assert (zero? (or 0 0)))
            (assert (not (zero? (or 0 1))))
            (assert (not (zero? (or 1 0))))
            (assert (not (zero? (or 1 1))))
            (assert (zero? (or 9e-7 9e-7))))}
  [a b]
  (if (clojure.core/or (truthy? a) (truthy? b))
    1
    0))

(defn xor
  "Logical XOR.

  Returns 1 if either--bot not both--arguments are non-zero, 0 otherwise."
  {:test #(do
            (assert (= 0 (xor 0 0)))
            (assert (= 1 (xor 1 0)))
            (assert (= 1 (xor 0 1)))
            (assert (= 0 (xor 1 1))))}
  [a b]
  (case [(truthy? a) (truthy? b)]
    [false false] 0
    [true  false] 1
    [false true ] 1
    [true  true ] 0))

;; 8. Functions

(defn atan [a b] (Math/atan2 a b))
; abs - same as in Clojure
(defn acos  [x]   (Math/acos x))
(defn asin  [x]   (Math/asin x))
(defn cos   [x]   (Math/cos x))
(defn exp   [x]   (Math/exp x))
(defn fix   [x]   (assert false))
(defn fup   [x]   (assert false))
(defn round [x]   (assert false))
(defn ln    [x]   (assert false))
(defn sin   [x]   (Math/sin x))
(defn sqrt  [x]   (Math/sqrt x))
(defn tan   [x]   (Math/tan x))


(def ^:dynamic *parameters* {})

(defn parameter
  "Returns value of parameter"
  {:test #(binding [*parameters* {"foo" 42
                                  97    66
                                  "_g0" 78}]
            (assert (= 66 (parameter 97)))
            (assert (= 0  (parameter 86)))
            (assert (= 42 (parameter "foo")))
            (assert (= 78 (parameter "_g0")))
            (assert (= 0  (parameter "_g1")))
            (assert (try
                      (parameter "missing_local")
                      false
                      (catch Exception e
                        true))))}
  [p]
  (let [value (get *parameters* p ::missing)]
    (cond
     (clojure.core/and (= ::missing value) (string? p) (= \_ (get p 0)))
     0

     (clojure.core/and (= ::missing value) (number? p))
     0

     (= ::missing value)
     (throw (ex-info "Attempt to access unset local parameter." {:parameter p}))

     :else
     value)))

(defn exists
  "Returns 1 if variables exists, 0 otherwise."
  {:test #(binding [*parameters* {"foo" 0}]
            (assert (= 1 (exists "foo")))
            (assert (= 0 (exists "bar"))))}
  [p]
  (if (contains? *parameters* p)
    1
    0))
