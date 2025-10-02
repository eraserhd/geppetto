(ns net.eraserhead.geppetto.gcode-test
  (:require
   [clojure.test :refer [deftest are]]
   [net.eraserhead.geppetto.gcode :as gcode]))

;; G-code Overview: https://linuxcnc.org/docs/html/gcode/overview.html
;; O Codes: https://linuxcnc.org/docs/html/gcode/o-code.html

(deftest t-parse-line
 (are [line tree] (= tree (gcode/parse-line line))
   ;; 2.1. Block Delete
   "/(Test)"          [::gcode/block-delete [::gcode/comment "Test"]]
   "/N105"            [::gcode/block-delete [::gcode/line-number [105]]]

   ;; 2.2. Line Number (and white space)
   "N105"             [[::gcode/line-number [105]]]
   "n105"             [[::gcode/line-number [105]]]
   "n010"             [[::gcode/line-number [10]]]
   " n105"            [[::gcode/line-number [105]]]
   "\t n105"          [[::gcode/line-number [105]]]
   "n105 \t\t"        [[::gcode/line-number [105]]]
   "n 105"            [[::gcode/line-number [105]]]
   "n1 2\t3  4 5"     [[::gcode/line-number [12345]]]
   "n100.74"          [[::gcode/line-number [100 74]]]

   ;; 2.3. Words
   "G-1.4"            [[::gcode/G -1.4]]
   "g-1.4"            [[::gcode/G -1.4]]
   "F2400"            [[::gcode/F 2400]]
   "f2400"            [[::gcode/F 2400]]
   "/f2400"           [::gcode/block-delete [::gcode/F 2400]]
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
   "f2s4t9"           [[::gcode/F 2] [::gcode/S 4] [::gcode/T 9]]
   "u62.4 v1.6 w11.4" [[::gcode/U 62.4] [::gcode/V 1.6] [::gcode/W 11.4]] ; linuxcnc

   ;; 3.1. Numbered Parameters
   "f#1"              '[[::gcode/F (parameter 1)]]
   "f##1"             '[[::gcode/F (parameter (parameter 1))]]
   "#22=32.4"         '[[::gcode/parameter= 22 32.4]]
   "##2=3"            '[[::gcode/parameter= (parameter 2) 3]]
   "#[1+2]=[3+4]"     '[[::gcode/parameter= (+ 1 2) (+ 3 4)]]

   ;; 3.3. Named Parameters
   "F#<localvalue>"   '[[::gcode/F (parameter "localvalue")]]
   "F#<LoC aL vAl Ue>"'[[::gcode/F (parameter "localvalue")]]
   "#<foo>=42.0"      '[[::gcode/parameter= "foo" 42.0]]
   "##<foo>=#<bar>"   '[[::gcode/parameter= (parameter "foo") (parameter "bar")]]

   ;; 4. HAL pins and INI values
   "Y#<_ini[setup]ypos>"              '[[::gcode/Y (parameter "_ini[setup]ypos")]]
   "y#<_hal[motion-controller.time]>" '[[::gcode/Y (parameter "_hal[motion-controller.time]")]]

   ;; 5. Expressions
   "f[4]"             '[[::gcode/F 4]]

   ;; 6. Binary Operators
   "f[4 + 7]"         '[[::gcode/F (+ 4 7)]]
   "f[4 + 7 + 9]"     '[[::gcode/F (+ (+ 4 7) 9)]]
   "f[4-7+8-6]"       '[[::gcode/F (- (+ (- 4 7) 8) 6)]]
   "f[4*9]"           '[[::gcode/F (* 4 9)]]
   "f[4-6*9]"         '[[::gcode/F (- 4 (* 6 9))]]
   "f[4*6/9]"         '[[::gcode/F (/ (* 4 6) 9)]]
   "f[1**2**3]"       '[[::gcode/F (** 1 (** 2 3))]]
   "f[[3+6]*4]"       '[[::gcode/F (* (+ 3 6) 4)]]
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
                     (mod 6 2))]]

   ;; 8. Functions
   "f[atan[6]/[4]]"    '[[::gcode/F (atan 6 4)]]
   "f[abs[-6]]"        '[[::gcode/F (abs -6)]]
   "x[acos[0.7]]"      '[[::gcode/X (acos 0.7)]]
   "x[cos[3]]"         '[[::gcode/X (cos 3)]]
   "x[exp[1]]"         '[[::gcode/X (exp 1)]]
   "x[fix[0.6]]"       '[[::gcode/X (fix 0.6)]]
   "x[fup[0.6]]"       '[[::gcode/X (fup 0.6)]]
   "x[round[0.4]]"     '[[::gcode/X (round 0.4)]]
   "x[ln[10]]"         '[[::gcode/X (ln 10)]]
   "x[sin[1/5]]"       '[[::gcode/X (sin (/ 1 5))]]
   "x[sqrt[4]]"        '[[::gcode/X (sqrt 4)]]
   "x[taN[7]]"         '[[::gcode/X (tan 7)]]
   "f[ExiSTS[#<foo>]]" '[[::gcode/F (exists "foo")]]

   ;; 12. Polar Coordinates
   "G91 @.5 ^90"       '[[::gcode/G 91] [::gcode/polar-distance 0.5] [::gcode/polar-angle 90]]
   "@.5 ^90"           '[[::gcode/polar-distance 0.5] [::gcode/polar-angle 90]]
   "^90"               '[[::gcode/polar-angle 90]]

   ;; 14. Comments
   "x1 (Test comment) y2" '[[::gcode/X 1] [::gcode/comment "Test comment"] [::gcode/Y 2]]
   "x1y2 (Test comment)"  '[[::gcode/X 1] [::gcode/Y 2] [::gcode/comment "Test comment"]]
   "x1 (in; comment)"     '[[::gcode/X 1] [::gcode/comment "in; comment"]]
   "x1 ;Test comment"     '[[::gcode/X 1] [::gcode/comment "Test comment"]]
   "x1;Test comment"      '[[::gcode/X 1] [::gcode/comment "Test comment"]]
   ";Test comment"        '[[::gcode/comment "Test comment"]]

   "(MSG,Hello TH)"   '[[::gcode/msg "Hello TH"]]
   "(m S\tg ,Hel TH)" '[[::gcode/msg "Hel TH"]]
   "(msg,h)(text)"    '[[::gcode/msg "h"] [::gcode/comment "text"]]


   "(DEBUG,h ello)"    '[[::gcode/debug [::gcode/text "h ello"]]]
   "(print,hi mom)"    '[[::gcode/print [::gcode/text "hi mom"]]]
   "(DEBUG,#23)"       '[[::gcode/debug [::gcode/parameter 23]]]
   "(DEBUG,->#23<-)"   '[[::gcode/debug
                          [::gcode/text "->"]
                          [::gcode/parameter 23]
                          [::gcode/text "<-"]]]
   "(DEBUG,->#<foo><-)"'[[::gcode/debug
                          [::gcode/text "->"]
                          [::gcode/parameter "foo"]
                          [::gcode/text "<-"]]]
   "(DEBUG,#1#<f>#3)"  '[[::gcode/debug
                          [::gcode/parameter 1]
                          [::gcode/parameter "f"]
                          [::gcode/parameter 3]]]
   "(DEBUG,#< f o o >)"'[[::gcode/debug [::gcode/parameter "foo"]]]
   "(DEBUG,%d)"        '[[::gcode/debug [::gcode/format-decimals 0]]]
   "(DEBUG,%f)"        '[[::gcode/debug [::gcode/format-decimals 4]]]
   "(DEBUG,%lf)"       '[[::gcode/debug [::gcode/format-decimals 6]]]
   "(DEBUG,%.7f)"      '[[::gcode/debug [::gcode/format-decimals 7]]]
   "(print,%d#6 hi)"   '[[::gcode/print
                          [::gcode/format-decimals 0]
                          [::gcode/parameter 6]
                          [::gcode/text " hi"]]]
   "(LoG,%d#6 hi)"     '[[::gcode/log
                          [::gcode/format-decimals 0]
                          [::gcode/parameter 6]
                          [::gcode/text " hi"]]]

   "(LO GOpEN,fo.txt)" '[[::gcode/logopen "fo.txt"]]
   "(LOGAPPEND,x.log)" '[[::gcode/logappend "x.log"]]
   "(logClose)"        '[[::gcode/logclose]]
   "(logClose )"       '[[::gcode/logclose]]
   "(logCloseX)"       '[[::gcode/comment "logCloseX"]]
   "(log,birch)"       '[[::gcode/log [::gcode/text "birch"]]]

   "(PROBEOPEN fn.p)"  '[[::gcode/probeopen "fn.p"]]
   "(PROBECLOSE)"      '[[::gcode/probeclose]]
   "(probEC LOSE)"     '[[::gcode/probeclose]]
   "(probEC LOSEX)"    '[[::gcode/comment "probEC LOSEX"]]

   ;; O Codes, 4. Subroutines
   "o100 sub"          '[[::gcode/sub 100]]
   "o101 endsub"       '[[::gcode/endsub 101 nil]]
   "o102 call [1] [3]" '[[::gcode/call 102 [1 3]]]
   "o102 call #2 [2+4]"'[[::gcode/call 102 [(parameter 2) (+ 2 4)]]]
   "o103 return"       '[[::gcode/return 103 nil]]

   ;; O Codes, 5. Looping
   "o104 while [#1 LT 10]" '[[::gcode/while 104 (lt (parameter 1) 10)]]
   "o105 endwhile"     '[[::gcode/endwhile 105]]
   "o106 do"           '[[::gcode/do 106]]
   "o107 break"        '[[::gcode/break 107]]
   "o108 continue"     '[[::gcode/continue 108]]

   ;; O Codes, 6. Conditional
   "o109 if [#31 EQ 3]"'[[::gcode/if 109 (eq (parameter 31) 3)]]
   "o110 elseif [#2 LT 2]"'[[::gcode/elseif 110 (lt (parameter 2) 2)]]
   "o111 else"         '[[::gcode/else 111]]
   "o112 endif"        '[[::gcode/endif 112]]

   ;; O Codes, 7. Repeat
   "o113 repeat [5]"   '[[::gcode/repeat 113 5]]
   "o114 endrepeat"    '[[::gcode/endrepeat 114]]

   ;; O Codes, 8. Indirection
   "o[#101+2] call"    '[[::gcode/call (+ (parameter 101) 2) []]]

   ;; O Codes, 9. Calling Files
   "o<myfile> call"    '[[::gcode/call "myfile" []]]
   "o<MyFile> call"    '[[::gcode/call "myfile" []]]
   "o<myfile> sub"     '[[::gcode/sub "myfile"]]
   "o<myfile> endsub"  '[[::gcode/endsub "myfile" nil]]
   "o<myfile> return"  '[[::gcode/return "myfile" nil]]

   ;; O Codes, 10. Subroutine return values
   "o123 return [#2 * 5]" '[[::gcode/return 123 (* (parameter 2) 5)]]
   "o123 endsub [3 * 4]"  '[[::gcode/endsub 123 (* 3 4)]]))
