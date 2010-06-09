;;; EuLisp system 'youtoo'
;;;   Interface file for module ex-expr

(definterface ex-expr
  (import (i-all p-env sx-node sx-obj ex-import ex-syntax ex-direct cg-dld)
   syntax (_macros _i-aux0 _ex-aux0)
   full-import (i-error i-notify i-param i-level1 boot1 boot symbol stream3 random handler read table table1 vector convert1 format list socket stream2 lock stream1 stream float character compare collect fpi number integer copy convert string callback let-cc dynamic thread event condition bit mop-alloc mop-access mop-prim mop-key mop-class mop-init mop-inspect mop-gf mop-meth mop-defcl telos level1 aux-table i-all sx-obj sx-obj1 sx-obj2 p-env i-ffi sx-node cg-interf i-modify ex-import ex-syntax ex-expose ex-direct cg-dld)
   export (
    ((name . *nil*) (pos . 2) (origin ex-expr . *nil*) (class . constant))
    ((name . expand-exprs) (pos . 14) (origin ex-expr . expand-exprs))
    ((name . filter-vars) (pos . 6) (origin ex-expr . filter-vars))
    ((name . expand-expr) (pos . 27) (origin ex-expr . expand-expr))
    ((name . protect-newline) (pos . 33) (origin ex-expr . protect-newline))
    ((name . protect-backslash) (pos . 3) (origin ex-expr . protect-backslash))
    ((name . complete-lambda-node) (pos . 26) (origin ex-expr . complete-lambda-node))
    ((name . filter-init-forms) (pos . 30) (origin ex-expr . filter-init-forms))
    ((name . protect-doublequote) (pos . 25) (origin ex-expr . protect-doublequote))
    ((name . get-macro-expander) (pos . 29) (origin ex-expr . get-macro-expander))
    ((name . protect-tilde) (pos . 15) (origin ex-expr . protect-tilde))
   )
   local-literals (
    (top-level . 321)
    (protect-backslash . 320)
    (check-appl-arity . 319)
    (box-binding . 318)
    (filter-vars . 317)
    (labelssetq . 316)
    (get-appl-expander . 315)
    (expand-local-static-vars . 314)
    (labelsvar . 313)
    (expand-local-static-vars* . 312)
    (get-id-expander . 311)
    (default-appl-expander . 310)
    (expand-exprs . 309)
    (protect-tilde . 308)
    (check-id-binding . 307)
    (expand-fun-form . 306)
    (get-keyword-node . 305)
    (unfold-rest-arg-appl . 304)
    (lift-appl . 303)
    (lambda-rest-args? . 302)
    (lift-let*-vars . 301)
    (expr-expander . 300)
    (protect-doublequote . 299)
    (complete-lambda-node . 298)
    (expand-expr . 297)
    (compute-range-and-domain . 296)
    (get-macro-expander . 295)
    (filter-init-forms . 294)
    (dummy-args . 293)
    (rest-args? . 292)
    (protect-newline . 291)
    (get-t-node . 290)
    (install-expr-expander . 289)
    ("redefinition of expander ~a" . 287)
    ("\n" . 284)
    ("\n" . 283)
    ("\n" . 282)
    ("\n" . 281)
    ("RESULT: ~a" . 273)
    (execute . 272)
    ("APPLY MACRO: ~a" . 271)
    (|(method G005812)| . 270)
    ("bad macro expansion of ~a" . 268)
    ("compile time error condition: " . 267)
    ("\"" . 259)
    ("\"" . 258)
    ("\"" . 257)
    ("\"" . 256)
    ("    Expanding ~a" . 254)
    (int-binary . 246)
    (setter . 242)
    ("no lexical binding ~a available" . 241)
    (ff . 239)
    (opencoding . 238)
    ("~~" . 236)
    ("~~" . 235)
    ("~~" . 234)
    ("~~" . 233)
    (args: . 229)
    (fun: . 228)
    ("no lexical binding ~a available" . 224)
    (int-zerop . 216)
    (binary= . 215)
    (binary- . 214)
    (int-binary- . 213)
    (dec . 212)
    (inc . 211)
    (binary+ . 210)
    (boot1 . 209)
    (int-binary+ . 208)
    (<= . 207)
    (int-binary= . 206)
    (>= . 205)
    (< . 204)
    (= . 203)
    (int-binary< . 202)
    (compare . 201)
    (> . 200)
    (number . 199)
    ((+ - * / %) . 198)
    (% . 197)
    (/ . 196)
    (* . 195)
    (- . 194)
    (+ . 193)
    ("  wrapping lambda in operator position: ~s" . 192)
    (arity . 187)
    ("box binding ~a" . 186)
    ("too few arguments calling ~a" . 184)
    ("too many arguments calling ~a" . 183)
    ("too few arguments calling ~a" . 182)
    ("\\" . 180)
    ("\\" . 179)
    ("\\" . 178)
    ("\\" . 177)
    (opencoded-lambda . 175)
    (named-lambda . 174)
    (lambda . 173)
    (|(method lift-setq)| . 172)
    (setq . 171)
    (|(method lift-if)| . 170)
    (call-next-method . 169)
    (|(method check-appl)| . 168)
    (lift-setq . 167)
    (lift-if . 166)
    (check-appl . 165)
    ("no applicable object ~a" . 163)
    ("macro binding ~a should be in syntax import" . 162)
    (*encl-lambda* . 155)
    ("missing else branch in (if ~a ...)" . 152)
    ("bad if syntax (if ~a ...)" . 151)
    (|(method G006256)| . 150)
    ("bad if syntax ~a" . 148)
    ("compile time error condition: " . 147)
    (else: . 144)
    (then: . 143)
    (pred: . 142)
    (if . 137)
    (value: . 132)
    (|(method G006347)| . 131)
    ("bad quote syntax" . 129)
    ("compile time error condition: " . 128)
    (quote . 125)
    (|(method G006369)| . 124)
    ("bad quasiquote syntax" . 122)
    ("compile time error condition: " . 121)
    (append . 118)
    (unquote-splicing . 117)
    (cons . 116)
    (unquote . 115)
    (quasiquote . 114)
    ("immutable binding ~a cannot be modified" . 112)
    ("no binding ~a available" . 111)
    (*clean-ups* . 110)
    (tail-pos? . 109)
    (|(method G006417)| . 108)
    ("bad setq syntax" . 106)
    ("compile time error condition: " . 105)
    ("body ~a not a list" . 94)
    (|(method G006503)| . 93)
    ("bad lambda syntax" . 91)
    ("compile time error condition: " . 90)
    ("body ~a not a list" . 87)
    ("~a" . 86)
    (|(method G006529)| . 85)
    ("bad named lambda syntax" . 83)
    ("compile time error condition: " . 82)
    ("body ~a not a list" . 79)
    (|(method G006560)| . 78)
    ("bad lambda syntax" . 76)
    ("compile time error condition: " . 75)
    ("body ~a not a list" . 72)
    (|(method G006586)| . 71)
    ("bad opencoded-lambda syntax" . 69)
    ("compile time error condition: " . 68)
    (labels . 65)
    (inlined-lambda . 64)
    (let* . 63)
    (|(method G006614)| . 62)
    ("bad let syntax" . 60)
    ("compile time error condition: " . 59)
    ("" . 55)
    (progn . 54)
    (|(method G006662)| . 53)
    ("bad let* syntax" . 51)
    ("compile time error condition: " . 50)
    (let . 47)
    (|(method G006699)| . 46)
    (anonymous . 45)
    (ct-error-value: . 43)
    ("bad labels syntax" . 42)
    ("compile time error condition: " . 41)
   )
   literals (
   )
))
