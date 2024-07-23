(ns net.eraserhead.geppetto.gcode-test
  (:require
   [clojure.test :refer [deftest are]]
   [net.eraserhead.geppetto.gcode :as gcode]))

(deftest t-parse-line
  (are [line tree] (= tree (gcode/parse-line line))
    "N105"             [[::gcode/line-number [105]]]
    "n105"             [[::gcode/line-number [105]]]
    "n010"             [[::gcode/line-number [10]]]
    " n105"            [[::gcode/line-number [105]]]
    "\t n105"          [[::gcode/line-number [105]]]
    "n105 \t\t"        [[::gcode/line-number [105]]]
    "n 105"            [[::gcode/line-number [105]]]
    "n1 2\t3  4 5"     [[::gcode/line-number [12345]]]
    "G-1.4"            [[::gcode/G -1.4]]
    "g-1.4"            [[::gcode/G -1.4]]
    "F2400"            [[::gcode/F 2400]]
    "f2400"            [[::gcode/F 2400]]
    "f-4"              [[::gcode/F -4]]
    "f+4"              [[::gcode/F +4]]
    "f.4"              [[::gcode/F 0.4]]
    "f-.4"             [[::gcode/F -0.4]]
    "f+.4"             [[::gcode/F +0.4]]
    "f -4"             [[::gcode/F -4]]
    "f+ .4"            [[::gcode/F 0.4]]
    "f+0 .4"           [[::gcode/F 0.4]]
    "g1x4.2f99z6"      [[::gcode/G 1] [::gcode/X 4.2] [::gcode/F 99] [::gcode/Z 6]]
    "g1 x4.2 f99 z6"   [[::gcode/G 1] [::gcode/X 4.2] [::gcode/F 99] [::gcode/Z 6]]
    "f[4]"             '[[::gcode/F 4]]
    "f[4 + 7]"         '[[::gcode/F (+ 4 7)]]
    "f[4 + 7 + 9]"     '[[::gcode/F (+ (+ 4 7) 9)]]
    "f[4-7+8-6]"       '[[::gcode/F (- (+ (- 4 7) 8) 6)]]
    "f[4*9]"           '[[::gcode/F (* 4 9)]]
    "f[4-6*9]"         '[[::gcode/F (- 4 (* 6 9))]]
    "f[4*6/9]"         '[[::gcode/F (/ (* 4 6) 9)]]
    "f[1**2**3]"       '[[::gcode/F (** 1 (** 2 3))]]
    "f[[3+6]*4]"       '[[::gcode/F (* (+ 3 6) 4)]]
    "f[abs[-6]]"       '[[::gcode/F (abs -6)]]
    "f[atan[6]/[4]]"   '[[::gcode/F (atan 6 4)]]
    "f#1"              '[[::gcode/F (parameter 1)]]
    "f##1"             '[[::gcode/F (parameter (parameter 1))]]
    "#22=32.4"         '[[::gcode/parameter= 22 32.4]]
    "#[1+2]=[3+4]"     '[[::gcode/parameter= (+ 1 2) (+ 3 4)]]
    "##2=3"            '[[::gcode/parameter= (parameter 2) 3]]
    "(Test comment)"   '[[::gcode/comment "Test comment"]]
    "(MSG,Hello TH)"   '[[::gcode/message "Hello TH"]]
    "(m S\tg ,Hel TH)" '[[::gcode/message "Hel TH"]]

    ;; linuxcnc extensions
    "n100.74"          '[[::gcode/line-number [100 74]]]
    "u62.4 v1.6 w11.4" '[[::gcode/U 62.4] [::gcode/V 1.6] [::gcode/W 11.4]]
    "F#<localvalue>"   '[[::gcode/F (parameter "localvalue")]]
    "F#<LoC aL vAl Ue>"'[[::gcode/F (parameter "localvalue")]]
    "#<foo>=42.0"      '[[::gcode/parameter= "foo" 42.0]]
    "##<foo>=#<bar>"   '[[::gcode/parameter= (parameter "foo") (parameter "bar")]]

    ;; linuxcnc operators
    "f[1 eq 2]"        '[[::gcode/F (eq 1 2)]]
    "f[1ne 2]"         '[[::gcode/F (ne 1 2)]]
    "f[1 gt2]"         '[[::gcode/F (gt 1 2)]]
    "f[1ge2]"          '[[::gcode/F (ge 1 2)]]
    "f[1.0lt 2.4]"     '[[::gcode/F (lt 1.0 2.4)]]
    "f[1 le 2]"        '[[::gcode/F (le 1 2)]]

    ;; linuxcnc precedence, from table 3
    "f[1 and 1 * 2 ** 3 eq 2 - 1 and 6 mod 2]"
    '[[::gcode/F (and (and 1
                           (eq (* 1 (** 2 3))
                               (- 2 1)))
                      (mod 6 2))]]))
                                                
