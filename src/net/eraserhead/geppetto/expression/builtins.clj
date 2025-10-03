(ns net.eraserhead.geppetto.expression.builtins
  (:refer-clojure :exclude [and or]))

(defn- falsey? [x] (< (abs x) 1e-6))
(defn- truthy? [x] (not (falsey? x)))

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
            (assert (truthy? (eq 0 0)))
            (assert (truthy? (eq 0 1e-7)))
            (assert (truthy? (eq 0 9e-7)))
            (assert (falsey? (eq -1e-6 1e-6)))
            (assert (falsey? (eq 1 2)))
            (assert (falsey? (eq 0 1e-6))))}
  [a b]
  (if (< (abs (- a b)) 1e-6)
    1
    0))

(defn ne
  "Not equal."
  {:test #(do
           (assert (falsey? (ne 7 7)))
           (assert (truthy? (ne 6 7))))}
  [a b]
  (if (zero? (eq a b))
    1
    0))

(defn gt
  "Greater than, but not equal to (see eq)."
  {:test #(do
            (assert (truthy? (gt 2 1)))
            (assert (falsey? (gt 1 2)))
            (assert (falsey? (gt 0 1e-6))))}
  [a b]
  (cond
   (truthy? (eq a b)) 0
   (> a b)            1
   :else              0))

(defn ge
  "Greater than or equal to (see eq)."
  {:test #(do
            (assert (truthy? (ge 1 1)))
            (assert (truthy? (ge 2 1)))
            (assert (falsey? (ge 1 2))))}
  [a b]
  (cond
   (truthy? (eq a b)) 1
   (> a b)            1
   :else              0))

(defn lt
  "Less than, but not equal to (see eq)."
  {:test #(do
            (assert (truthy? (lt 1 2)))
            (assert (falsey? (lt 1 1)))
            (assert (falsey? (lt 0 1e-7))))}
  [a b]
  (cond
   (truthy? (eq a b)) 0
   (< a b)            1
   :else              0))

(defn le
  "Less than or equal to (see eq)."
  {:test #(do
            (assert (truthy? (le 1 2)))
            (assert (truthy? (le 0 1e-7)))
            (assert (truthy? (le 1e-7 0))))}
  [a b]
  (cond
   (truthy? (eq a b)) 1
   (< a b)            1
   :else              0))

(defn and
  "Logical AND.

  Returns 1 if both arguments are non-zero, 0 otherwise."
  {:test #(do
            (assert (falsey? (and 0 0)))
            (assert (falsey? (and 0 1)))
            (assert (falsey? (and 1 0)))
            (assert (falsey? (and 9e-7 9e-7)))
            (assert (truthy? (and 1 1))))}
  [a b]
  (if (clojure.core/and (truthy? a) (truthy? b))
    1
    0))

(defn or
  "Logical OR.

  Returns 1 if either argument is non-zero, 0 otherwise."
  {:test #(do
            (assert (falsey? (or 0 0)))
            (assert (truthy? (or 0 1)))
            (assert (truthy? (or 1 0)))
            (assert (truthy? (or 1 1)))
            (assert (falsey? (or 9e-7 9e-7))))}
  [a b]
  (if (clojure.core/or (truthy? a) (truthy? b))
    1
    0))

(defn xor
  "Logical XOR.

  Returns 1 if either--bot not both--arguments are non-zero, 0 otherwise."
  {:test #(do
            (assert (falsey? (xor 0 0)))
            (assert (truthy? (xor 1 0)))
            (assert (truthy? (xor 0 1)))
            (assert (falsey? (xor 1 1))))}
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
(defn fix   [x]   (Math/floor x))
(defn fup   [x]   (Math/ceil x))
(defn round [x]   (Math/round x))
(defn ln    [x]   (Math/log x))
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
            (assert (truthy? (exists "foo")))
            (assert (falsey? (exists "bar"))))}
  [p]
  (if (contains? *parameters* p)
    1
    0))
