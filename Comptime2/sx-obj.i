;;; EuLisp system 'youtoo'
;;;   Interface file for module sx-obj

(definterface sx-obj
  (import (i-level1 sx-obj1 sx-obj2)
   syntax (_macros _sx-obj0)
   full-import (aux-table level1 telos mop-defcl mop-meth mop-gf mop-inspect mop-init mop-class mop-key mop-prim mop-access mop-alloc bit condition event thread dynamic let-cc callback string convert copy integer number fpi collect compare character float stream3 vector stream stream1 lock stream2 socket list format convert1 table1 table handler random symbol read boot boot1 i-level1 sx-obj1 sx-obj2)
   export (
    ((name . local-static-var-captured!) (pos . 35) (origin sx-obj2 . local-static-var-captured!) (inline (G001467 (stack-ref 1) (static-ref0) (stack-ref 2) (binding-ref ? <local-static-var>) (set-primitive-relative-ref) (nobble 2))))
    ((name . binding-local-name!) (pos . 19) (origin sx-obj1 . binding-local-name!) (inline (G00598 (stack-ref 1) (static-fpi-ref 6) (stack-ref 2) (binding-ref ? <binding>) (set-primitive-relative-ref) (nobble 2))))
    ((name . local-static-var-captured?) (pos . 22) (origin sx-obj2 . local-static-var-captured?) (inline (G001465 (static-ref0) (binding-ref ? <local-static-var>) (primitive-relative-ref))))
    ((name . binding-obj!) (pos . 55) (origin sx-obj1 . binding-obj!) (inline (G00582 (stack-ref 1) (static-ref2) (stack-ref 2) (binding-ref ? <binding>) (set-primitive-relative-ref) (nobble 2))))
    ((name . module-inlined-lambdas!) (pos . 54) (origin sx-obj1 . module-inlined-lambdas!) (inline (G00646 (stack-ref 1) (static-fpi-ref 11) (stack-ref 2) (binding-ref ? <module>) (set-primitive-relative-ref) (nobble 2))))
    ((name . appl-args?) (pos . 53) (origin sx-obj . appl-args?) (inline (G001997 (static-ref0) (binding-ref ? <appl>) (primitive-relative-ref))))
    ((name . module-inlined-setters!) (pos . 73) (origin sx-obj1 . module-inlined-setters!) (inline (G00642 (stack-ref 1) (static-fpi-ref 10) (stack-ref 2) (binding-ref ? <module>) (set-primitive-relative-ref) (nobble 2))))
    ((name . module-used-syntax-modules?) (pos . 71) (origin sx-obj1 . module-used-syntax-modules?) (inline (G00608 (static-ref2) (binding-ref ? <module>) (primitive-relative-ref))))
    ((name . <interface-binding>) (pos . 72) (origin sx-obj1 . <interface-binding>) (class . constant))
    ((name . <named-const>) (pos . 21) (origin sx-obj2 . <named-const>) (class . constant))
    ((name . setq-binding?) (pos . 34) (origin sx-obj2 . setq-binding?) (inline (G001477 (static-ref1) (binding-ref ? <setq>) (primitive-relative-ref))))
    ((name . named-const-binding!) (pos . 41) (origin sx-obj2 . named-const-binding!) (inline (G001487 (stack-ref 1) (static-ref0) (stack-ref 2) (binding-ref ? <named-const>) (set-primitive-relative-ref) (nobble 2))))
    ((name . <var>) (pos . 11) (origin sx-obj2 . <var>) (class . constant))
    ((name . fun-has-unknown-appls!) (pos . 52) (origin sx-obj . fun-has-unknown-appls!) (inline (G001955 (stack-ref 1) (static-ref0) (stack-ref 2) (binding-ref ? <fun>) (set-primitive-relative-ref) (nobble 2))))
    ((name . appl-fun?) (pos . 19) (origin sx-obj . appl-fun?) (inline (G002001 (static-ref1) (binding-ref ? <appl>) (primitive-relative-ref))))
    ((name . binding-obj?) (pos . 18) (origin sx-obj1 . binding-obj?) (inline (G00580 (static-ref2) (binding-ref ? <binding>) (primitive-relative-ref))))
    ((name . <call-next-method>) (pos . 51) (origin sx-obj . <call-next-method>) (class . constant))
    ((name . module-binding-vector-size?) (pos . 17) (origin sx-obj1 . module-binding-vector-size?) (inline (G00672 (static-fpi-ref 18) (binding-ref ? <module>) (primitive-relative-ref))))
    ((name . fun-has-unknown-appls?) (pos . 44) (origin sx-obj . fun-has-unknown-appls?) (inline (G001953 (static-ref0) (binding-ref ? <fun>) (primitive-relative-ref))))
    ((name . fun-appls?) (pos . 43) (origin sx-obj . fun-appls?) (inline (G001957 (static-ref1) (binding-ref ? <fun>) (primitive-relative-ref))))
    ((name . save-binding-local-name?) (pos . 27) (origin sx-obj . save-binding-local-name?))
    ((name . binding-info?) (pos . 35) (origin sx-obj1 . binding-info?) (inline (G00572 (static-ref0) (binding-ref ? <binding>) (primitive-relative-ref))))
    ((name . varp) (pos . 10) (origin sx-obj2 . varp))
    ((name . module-top-level-forms?) (pos . 53) (origin sx-obj1 . module-top-level-forms?) (inline (G00624 (static-fpi-ref 6) (binding-ref ? <module>) (primitive-relative-ref))))
    ((name . <appl>) (pos . 42) (origin sx-obj . <appl>) (class . constant))
    ((name . binding-immutable?) (pos . 52) (origin sx-obj1 . binding-immutable?) (inline (G00588 (static-fpi-ref 4) (binding-ref ? <binding>) (primitive-relative-ref))))
    ((name . let*p) (pos . 41) (origin sx-obj . let*p))
    ((name . module-c-module-name!) (pos . 15) (origin sx-obj1 . module-c-module-name!) (inline (G00678 (stack-ref 1) (static-fpi-ref 19) (stack-ref 2) (binding-ref ? <module>) (set-primitive-relative-ref) (nobble 2))))
    ((name . var-value?) (pos . 33) (origin sx-obj2 . var-value?) (inline (G001453 (static-ref1) (binding-ref ? <var>) (primitive-relative-ref))))
    ((name . module-lexical-env?) (pos . 16) (origin sx-obj1 . module-lexical-env?) (inline (G00664 (static-fpi-ref 16) (binding-ref ? <module>) (primitive-relative-ref))))
    ((name . call-next-method-p) (pos . 50) (origin sx-obj . call-next-method-p))
    ((name . lambda-delegated-vars!) (pos . 26) (origin sx-obj . lambda-delegated-vars!) (inline (G001991 (stack-ref 1) (static-ref1) (stack-ref 2) (binding-ref ? <lambda>) (set-primitive-relative-ref) (nobble 2))))
    ((name . module-used-module-names?) (pos . 51) (origin sx-obj1 . module-used-module-names?) (inline (G00612 (static-fpi-ref 3) (binding-ref ? <module>) (primitive-relative-ref))))
    ((name . <literal-const>) (pos . 32) (origin sx-obj2 . <literal-const>) (class . constant))
    ((name . var-binding!) (pos . 20) (origin sx-obj2 . var-binding!) (inline (G001459 (stack-ref 1) (static-ref2) (stack-ref 2) (binding-ref ? <var>) (set-primitive-relative-ref) (nobble 2))))
    ((name . binding-module?) (pos . 50) (origin sx-obj1 . binding-module?) (inline (G00592 (static-fpi-ref 5) (binding-ref ? <binding>) (primitive-relative-ref))))
    ((name . binding-local-index!) (pos . 70) (origin sx-obj1 . binding-local-index!) (inline (G00578 (stack-ref 1) (static-ref1) (stack-ref 2) (binding-ref ? <binding>) (set-primitive-relative-ref) (nobble 2))))
    ((name . global-var-p) (pos . 31) (origin sx-obj2 . global-var-p))
    ((name . fun-args!) (pos . 49) (origin sx-obj . fun-args!) (inline (G001975 (stack-ref 1) (static-fpi-ref 5) (stack-ref 2) (binding-ref ? <fun>) (set-primitive-relative-ref) (nobble 2))))
    ((name . <fun>) (pos . 18) (origin sx-obj . <fun>) (class . constant))
    ((name . module-external-env!) (pos . 34) (origin sx-obj1 . module-external-env!) (inline (G00662 (stack-ref 1) (static-fpi-ref 15) (stack-ref 2) (binding-ref ? <module>) (set-primitive-relative-ref) (nobble 2))))
    ((name . module-name!) (pos . 14) (origin sx-obj1 . module-name!) (inline (G00686 (stack-ref 1) (static-fpi-ref 21) (stack-ref 2) (binding-ref ? <module>) (set-primitive-relative-ref) (nobble 2))))
    ((name . binding?) (pos . 17) (origin sx-obj . binding?))
    ((name . module-binding-vector-size!) (pos . 49) (origin sx-obj1 . module-binding-vector-size!) (inline (G00674 (stack-ref 1) (static-fpi-ref 18) (stack-ref 2) (binding-ref ? <module>) (set-primitive-relative-ref) (nobble 2))))
    ((name . if-else?) (pos . 25) (origin sx-obj . if-else?) (inline (G002005 (static-ref0) (binding-ref ? <if>) (primitive-relative-ref))))
    ((name . syntax-expr-p) (pos . 69) (origin sx-obj1 . syntax-expr-p))
    ((name . module-load-dir?) (pos . 13) (origin sx-obj1 . module-load-dir?) (inline (G00680 (static-fpi-ref 20) (binding-ref ? <module>) (primitive-relative-ref))))
    ((name . setq-obj!) (pos . 19) (origin sx-obj2 . setq-obj!) (inline (G001475 (stack-ref 1) (static-ref0) (stack-ref 2) (binding-ref ? <setq>) (set-primitive-relative-ref) (nobble 2))))
    ((name . var-name?) (pos . 9) (origin sx-obj2 . var-name?) (inline (G001461 (static-fpi-ref 3) (binding-ref ? <var>) (primitive-relative-ref))))
    ((name . lambda-binding-refs!) (pos . 40) (origin sx-obj . lambda-binding-refs!) (inline (G001987 (stack-ref 1) (static-ref0) (stack-ref 2) (binding-ref ? <lambda>) (set-primitive-relative-ref) (nobble 2))))
    ((name . save-binding-module-name?) (pos . 16) (origin sx-obj . save-binding-module-name?))
    ((name . binding-immutable!) (pos . 68) (origin sx-obj1 . binding-immutable!) (inline (G00590 (stack-ref 1) (static-fpi-ref 4) (stack-ref 2) (binding-ref ? <binding>) (set-primitive-relative-ref) (nobble 2))))
    ((name . <const>) (pos . 29) (origin sx-obj2 . <const>) (class . constant))
    ((name . fun-arity?) (pos . 15) (origin sx-obj . fun-arity?) (inline (G001969 (static-fpi-ref 4) (binding-ref ? <fun>) (primitive-relative-ref))))
    ((name . module-syntax-env!) (pos . 48) (origin sx-obj1 . module-syntax-env!) (inline (G00658 (stack-ref 1) (static-fpi-ref 14) (stack-ref 2) (binding-ref ? <module>) (set-primitive-relative-ref) (nobble 2))))
    ((name . const-value?) (pos . 18) (origin sx-obj2 . const-value?) (inline (G001481 (static-ref0) (binding-ref ? <const>) (primitive-relative-ref))))
    ((name . global-static-var-p) (pos . 8) (origin sx-obj2 . global-static-var-p))
    ((name . syntax-def-p) (pos . 33) (origin sx-obj1 . syntax-def-p))
    ((name . var-name!) (pos . 30) (origin sx-obj2 . var-name!) (inline (G001463 (stack-ref 1) (static-fpi-ref 3) (stack-ref 2) (binding-ref ? <var>) (set-primitive-relative-ref) (nobble 2))))
    ((name . module-all-used-module-names!) (pos . 32) (origin sx-obj1 . module-all-used-module-names!) (inline (G00606 (stack-ref 1) (static-ref1) (stack-ref 2) (binding-ref ? <module>) (set-primitive-relative-ref) (nobble 2))))
    ((name . <global-static-var>) (pos . 28) (origin sx-obj2 . <global-static-var>) (class . constant))
    ((name . if-then?) (pos . 14) (origin sx-obj . if-then?) (inline (G002009 (static-ref1) (binding-ref ? <if>) (primitive-relative-ref))))
    ((name . <syntax-def>) (pos . 31) (origin sx-obj1 . <syntax-def>) (class . constant))
    ((name . <local-static-var>) (pos . 7) (origin sx-obj2 . <local-static-var>) (class . constant))
    ((name . setqp) (pos . 17) (origin sx-obj2 . setqp))
    ((name . module-used-module-names!) (pos . 12) (origin sx-obj1 . module-used-module-names!) (inline (G00614 (stack-ref 1) (static-fpi-ref 3) (stack-ref 2) (binding-ref ? <module>) (set-primitive-relative-ref) (nobble 2))))
    ((name . local-static-var-lambda!) (pos . 6) (origin sx-obj2 . local-static-var-lambda!) (inline (G001471 (stack-ref 1) (static-ref1) (stack-ref 2) (binding-ref ? <local-static-var>) (set-primitive-relative-ref) (nobble 2))))
    ((name . module-all-used-module-names?) (pos . 47) (origin sx-obj1 . module-all-used-module-names?) (inline (G00604 (static-ref1) (binding-ref ? <module>) (primitive-relative-ref))))
    ((name . <lambda>) (pos . 13) (origin sx-obj . <lambda>) (class . constant))
    ((name . if-else!) (pos . 24) (origin sx-obj . if-else!) (inline (G002007 (stack-ref 1) (static-ref0) (stack-ref 2) (binding-ref ? <if>) (set-primitive-relative-ref) (nobble 2))))
    ((name . const-value!) (pos . 27) (origin sx-obj2 . const-value!) (inline (G001483 (stack-ref 1) (static-ref0) (stack-ref 2) (binding-ref ? <const>) (set-primitive-relative-ref) (nobble 2))))
    ((name . lambda-binding-refs?) (pos . 48) (origin sx-obj . lambda-binding-refs?) (inline (G001985 (static-ref0) (binding-ref ? <lambda>) (primitive-relative-ref))))
    ((name . module-inlined-lambdas?) (pos . 67) (origin sx-obj1 . module-inlined-lambdas?) (inline (G00644 (static-fpi-ref 11) (binding-ref ? <module>) (primitive-relative-ref))))
    ((name . appl-args!) (pos . 47) (origin sx-obj . appl-args!) (inline (G001999 (stack-ref 1) (static-ref0) (stack-ref 2) (binding-ref ? <appl>) (set-primitive-relative-ref) (nobble 2))))
    ((name . named-const-p) (pos . 16) (origin sx-obj2 . named-const-p))
    ((name . var-used!) (pos . 26) (origin sx-obj2 . var-used!) (inline (G001451 (stack-ref 1) (static-ref0) (stack-ref 2) (binding-ref ? <var>) (set-primitive-relative-ref) (nobble 2))))
    ((name . <if>) (pos . 9) (origin sx-obj . <if>) (class . constant))
    ((name . module-named-constants!) (pos . 66) (origin sx-obj1 . module-named-constants!) (inline (G00654 (stack-ref 1) (static-fpi-ref 13) (stack-ref 2) (binding-ref ? <module>) (set-primitive-relative-ref) (nobble 2))))
    ((name . module-lexical-env!) (pos . 10) (origin sx-obj1 . module-lexical-env!) (inline (G00666 (stack-ref 1) (static-fpi-ref 16) (stack-ref 2) (binding-ref ? <module>) (set-primitive-relative-ref) (nobble 2))))
    ((name . fun-arity!) (pos . 39) (origin sx-obj . fun-arity!) (inline (G001971 (stack-ref 1) (static-fpi-ref 4) (stack-ref 2) (binding-ref ? <fun>) (set-primitive-relative-ref) (nobble 2))))
    ((name . module-used-syntax-modules!) (pos . 11) (origin sx-obj1 . module-used-syntax-modules!) (inline (G00610 (stack-ref 1) (static-ref2) (stack-ref 2) (binding-ref ? <module>) (set-primitive-relative-ref) (nobble 2))))
    ((name . <global-var>) (pos . 4) (origin sx-obj2 . <global-var>) (class . constant))
    ((name . local-name?) (pos . 8) (origin sx-obj . local-name?))
    ((name . module-interactive-lexical-env!) (pos . 46) (origin sx-obj1 . module-interactive-lexical-env!) (inline (G00602 (stack-ref 1) (static-ref0) (stack-ref 2) (binding-ref ? <module>) (set-primitive-relative-ref) (nobble 2))))
    ((name . module-load-dir!) (pos . 45) (origin sx-obj1 . module-load-dir!) (inline (G00682 (stack-ref 1) (static-fpi-ref 20) (stack-ref 2) (binding-ref ? <module>) (set-primitive-relative-ref) (nobble 2))))
    ((name . setq-obj?) (pos . 3) (origin sx-obj2 . setq-obj?) (inline (G001473 (static-ref0) (binding-ref ? <setq>) (primitive-relative-ref))))
    ((name . syntax-expr-encl-lambda?) (pos . 65) (origin sx-obj1 . syntax-expr-encl-lambda?) (inline (G00568 (static-ref0) (binding-ref ? <syntax-expr>) (primitive-relative-ref))))
    ((name . module-max-binding-vector-size?) (pos . 9) (origin sx-obj1 . module-max-binding-vector-size?) (inline (G00668 (static-fpi-ref 17) (binding-ref ? <module>) (primitive-relative-ref))))
    ((name . named-const-name!) (pos . 15) (origin sx-obj2 . named-const-name!) (inline (G001491 (stack-ref 1) (static-ref1) (stack-ref 2) (binding-ref ? <named-const>) (set-primitive-relative-ref) (nobble 2))))
    ((name . <binding>) (pos . 44) (origin sx-obj1 . <binding>) (class . constant))
    ((name . binding-info!) (pos . 43) (origin sx-obj1 . binding-info!) (inline (G00574 (stack-ref 1) (static-ref0) (stack-ref 2) (binding-ref ? <binding>) (set-primitive-relative-ref) (nobble 2))))
    ((name . module-c-module-name?) (pos . 8) (origin sx-obj1 . module-c-module-name?) (inline (G00676 (static-fpi-ref 19) (binding-ref ? <module>) (primitive-relative-ref))))
    ((name . module-top-level-forms!) (pos . 63) (origin sx-obj1 . module-top-level-forms!) (inline (G00626 (stack-ref 1) (static-fpi-ref 6) (stack-ref 2) (binding-ref ? <module>) (set-primitive-relative-ref) (nobble 2))))
    ((name . module-local-literals?) (pos . 64) (origin sx-obj1 . module-local-literals?) (inline (G00616 (static-fpi-ref 4) (binding-ref ? <module>) (primitive-relative-ref))))
    ((name . var-value!) (pos . 5) (origin sx-obj2 . var-value!) (inline (G001455 (stack-ref 1) (static-ref1) (stack-ref 2) (binding-ref ? <var>) (set-primitive-relative-ref) (nobble 2))))
    ((name . constp) (pos . 14) (origin sx-obj2 . constp))
    ((name . origin-name?) (pos . 38) (origin sx-obj . origin-name?))
    ((name . bindingp) (pos . 30) (origin sx-obj1 . bindingp))
    ((name . fun-range-and-domain?) (pos . 11) (origin sx-obj . fun-range-and-domain?) (inline (G001965 (static-fpi-ref 3) (binding-ref ? <fun>) (primitive-relative-ref))))
    ((name . lambdap) (pos . 12) (origin sx-obj . lambdap))
    ((name . modulep) (pos . 7) (origin sx-obj1 . modulep))
    ((name . module-name?) (pos . 29) (origin sx-obj1 . module-name?) (inline (G00684 (static-fpi-ref 21) (binding-ref ? <module>) (primitive-relative-ref))))
    ((name . var-used?) (pos . 13) (origin sx-obj2 . var-used?) (inline (G001449 (static-ref0) (binding-ref ? <var>) (primitive-relative-ref))))
    ((name . binding!) (pos . 35) (origin sx-obj . binding!))
    ((name . fun-args?) (pos . 32) (origin sx-obj . fun-args?) (inline (G001973 (static-fpi-ref 5) (binding-ref ? <fun>) (primitive-relative-ref))))
    ((name . fun-appls!) (pos . 34) (origin sx-obj . fun-appls!) (inline (G001959 (stack-ref 1) (static-ref1) (stack-ref 2) (binding-ref ? <fun>) (set-primitive-relative-ref) (nobble 2))))
    ((name . binding-imported!) (pos . 62) (origin sx-obj1 . binding-imported!) (inline (G00586 (stack-ref 1) (static-fpi-ref 3) (stack-ref 2) (binding-ref ? <binding>) (set-primitive-relative-ref) (nobble 2))))
    ((name . fun-body!) (pos . 33) (origin sx-obj . fun-body!) (inline (G001963 (stack-ref 1) (static-ref2) (stack-ref 2) (binding-ref ? <fun>) (set-primitive-relative-ref) (nobble 2))))
    ((name . lambda-inlined!) (pos . 37) (origin sx-obj . lambda-inlined!) (inline (G001995 (stack-ref 1) (static-ref2) (stack-ref 2) (binding-ref ? <lambda>) (set-primitive-relative-ref) (nobble 2))))
    ((name . <opencoding>) (pos . 10) (origin sx-obj . <opencoding>) (class . constant))
    ((name . if-pred!) (pos . 36) (origin sx-obj . if-pred!) (inline (G002015 (stack-ref 1) (static-ref2) (stack-ref 2) (binding-ref ? <if>) (set-primitive-relative-ref) (nobble 2))))
    ((name . module-local-literals!) (pos . 61) (origin sx-obj1 . module-local-literals!) (inline (G00618 (stack-ref 1) (static-fpi-ref 4) (stack-ref 2) (binding-ref ? <module>) (set-primitive-relative-ref) (nobble 2))))
    ((name . <setq>) (pos . 25) (origin sx-obj2 . <setq>) (class . constant))
    ((name . if-then!) (pos . 31) (origin sx-obj . if-then!) (inline (G002011 (stack-ref 1) (static-ref1) (stack-ref 2) (binding-ref ? <if>) (set-primitive-relative-ref) (nobble 2))))
    ((name . module-external-env?) (pos . 42) (origin sx-obj1 . module-external-env?) (inline (G00660 (static-fpi-ref 15) (binding-ref ? <module>) (primitive-relative-ref))))
    ((name . <module>) (pos . 60) (origin sx-obj1 . <module>) (class . constant))
    ((name . <local-var>) (pos . 24) (origin sx-obj2 . <local-var>) (class . constant))
    ((name . module-lexical-binding-refs!) (pos . 41) (origin sx-obj1 . module-lexical-binding-refs!) (inline (G00622 (stack-ref 1) (static-fpi-ref 5) (stack-ref 2) (binding-ref ? <module>) (set-primitive-relative-ref) (nobble 2))))
    ((name . module-named-lambdas?) (pos . 6) (origin sx-obj1 . module-named-lambdas?) (inline (G00636 (static-fpi-ref 9) (binding-ref ? <module>) (primitive-relative-ref))))
    ((name . funp) (pos . 30) (origin sx-obj . funp))
    ((name . module-static-variables?) (pos . 28) (origin sx-obj1 . module-static-variables?) (inline (G00648 (static-fpi-ref 12) (binding-ref ? <module>) (primitive-relative-ref))))
    ((name . applp) (pos . 23) (origin sx-obj . applp))
    ((name . ifp) (pos . 7) (origin sx-obj . ifp))
    ((name . module-interactive-lexical-env?) (pos . 40) (origin sx-obj1 . module-interactive-lexical-env?) (inline (G00600 (static-ref0) (binding-ref ? <module>) (primitive-relative-ref))))
    ((name . module-foreign-functions!) (pos . 5) (origin sx-obj1 . module-foreign-functions!) (inline (G00630 (stack-ref 1) (static-fpi-ref 7) (stack-ref 2) (binding-ref ? <module>) (set-primitive-relative-ref) (nobble 2))))
    ((name . <syntax-obj>) (pos . 27) (origin sx-obj1 . <syntax-obj>) (class . constant))
    ((name . module-inlined-setters?) (pos . 39) (origin sx-obj1 . module-inlined-setters?) (inline (G00640 (static-fpi-ref 10) (binding-ref ? <module>) (primitive-relative-ref))))
    ((name . appl-fun!) (pos . 29) (origin sx-obj . appl-fun!) (inline (G002003 (stack-ref 1) (static-ref1) (stack-ref 2) (binding-ref ? <appl>) (set-primitive-relative-ref) (nobble 2))))
    ((name . lambda-inlined?) (pos . 22) (origin sx-obj . lambda-inlined?) (inline (G001993 (static-ref2) (binding-ref ? <lambda>) (primitive-relative-ref))))
    ((name . if-pred?) (pos . 21) (origin sx-obj . if-pred?) (inline (G002013 (static-ref2) (binding-ref ? <if>) (primitive-relative-ref))))
    ((name . fun-range-and-domain!) (pos . 6) (origin sx-obj . fun-range-and-domain!) (inline (G001967 (stack-ref 1) (static-fpi-ref 3) (stack-ref 2) (binding-ref ? <fun>) (set-primitive-relative-ref) (nobble 2))))
    ((name . interface-binding-p) (pos . 4) (origin sx-obj1 . interface-binding-p))
    ((name . lambda-delegated-vars?) (pos . 5) (origin sx-obj . lambda-delegated-vars?) (inline (G001989 (static-ref1) (binding-ref ? <lambda>) (primitive-relative-ref))))
    ((name . local-static-var-lambda?) (pos . 40) (origin sx-obj2 . local-static-var-lambda?) (inline (G001469 (static-ref1) (binding-ref ? <local-static-var>) (primitive-relative-ref))))
    ((name . binding-imported?) (pos . 59) (origin sx-obj1 . binding-imported?) (inline (G00584 (static-fpi-ref 3) (binding-ref ? <binding>) (primitive-relative-ref))))
    ((name . fun-body?) (pos . 4) (origin sx-obj . fun-body?) (inline (G001961 (static-ref2) (binding-ref ? <fun>) (primitive-relative-ref))))
    ((name . opencodingp) (pos . 20) (origin sx-obj . opencodingp))
    ((name . module-syntax-env?) (pos . 22) (origin sx-obj1 . module-syntax-env?) (inline (G00656 (static-fpi-ref 14) (binding-ref ? <module>) (primitive-relative-ref))))
    ((name . module-anonymous-lambdas?) (pos . 25) (origin sx-obj1 . module-anonymous-lambdas?) (inline (G00632 (static-fpi-ref 8) (binding-ref ? <module>) (primitive-relative-ref))))
    ((name . module-named-constants?) (pos . 38) (origin sx-obj1 . module-named-constants?) (inline (G00652 (static-fpi-ref 13) (binding-ref ? <module>) (primitive-relative-ref))))
    ((name . syntax-expr-encl-lambda!) (pos . 26) (origin sx-obj1 . syntax-expr-encl-lambda!) (inline (G00570 (stack-ref 1) (static-ref0) (stack-ref 2) (binding-ref ? <syntax-expr>) (set-primitive-relative-ref) (nobble 2))))
    ((name . binding-module!) (pos . 57) (origin sx-obj1 . binding-module!) (inline (G00594 (stack-ref 1) (static-fpi-ref 5) (stack-ref 2) (binding-ref ? <binding>) (set-primitive-relative-ref) (nobble 2))))
    ((name . syntax-obj-p) (pos . 24) (origin sx-obj1 . syntax-obj-p))
    ((name . module-max-binding-vector-size!) (pos . 58) (origin sx-obj1 . module-max-binding-vector-size!) (inline (G00670 (stack-ref 1) (static-fpi-ref 17) (stack-ref 2) (binding-ref ? <module>) (set-primitive-relative-ref) (nobble 2))))
    ((name . named-const-name?) (pos . 12) (origin sx-obj2 . named-const-name?) (inline (G001489 (static-ref1) (binding-ref ? <named-const>) (primitive-relative-ref))))
    ((name . named-const-binding?) (pos . 39) (origin sx-obj2 . named-const-binding?) (inline (G001485 (static-ref0) (binding-ref ? <named-const>) (primitive-relative-ref))))
    ((name . setq-binding!) (pos . 2) (origin sx-obj2 . setq-binding!) (inline (G001479 (stack-ref 1) (static-ref1) (stack-ref 2) (binding-ref ? <setq>) (set-primitive-relative-ref) (nobble 2))))
    ((name . fun-binding?) (pos . 3) (origin sx-obj . fun-binding?) (inline (G001977 (static-fpi-ref 6) (binding-ref ? <fun>) (primitive-relative-ref))))
    ((name . fun-name!) (pos . 46) (origin sx-obj . fun-name!) (inline (G001983 (stack-ref 1) (static-fpi-ref 7) (stack-ref 2) (binding-ref ? <fun>) (set-primitive-relative-ref) (nobble 2))))
    ((name . module-anonymous-lambdas!) (pos . 37) (origin sx-obj1 . module-anonymous-lambdas!) (inline (G00634 (stack-ref 1) (static-fpi-ref 8) (stack-ref 2) (binding-ref ? <module>) (set-primitive-relative-ref) (nobble 2))))
    ((name . binding-local-index?) (pos . 23) (origin sx-obj1 . binding-local-index?) (inline (G00576 (static-ref1) (binding-ref ? <binding>) (primitive-relative-ref))))
    ((name . local-var-p) (pos . 38) (origin sx-obj2 . local-var-p))
    ((name . local-static-var-p) (pos . 37) (origin sx-obj2 . local-static-var-p))
    ((name . module-foreign-functions?) (pos . 36) (origin sx-obj1 . module-foreign-functions?) (inline (G00628 (static-fpi-ref 7) (binding-ref ? <module>) (primitive-relative-ref))))
    ((name . module-static-variables!) (pos . 21) (origin sx-obj1 . module-static-variables!) (inline (G00650 (stack-ref 1) (static-fpi-ref 12) (stack-ref 2) (binding-ref ? <module>) (set-primitive-relative-ref) (nobble 2))))
    ((name . literal-const-p) (pos . 23) (origin sx-obj2 . literal-const-p))
    ((name . fun-binding!) (pos . 45) (origin sx-obj . fun-binding!) (inline (G001979 (stack-ref 1) (static-fpi-ref 6) (stack-ref 2) (binding-ref ? <fun>) (set-primitive-relative-ref) (nobble 2))))
    ((name . <syntax-expr>) (pos . 3) (origin sx-obj1 . <syntax-expr>) (class . constant))
    ((name . fun-name?) (pos . 28) (origin sx-obj . fun-name?) (inline (G001981 (static-fpi-ref 7) (binding-ref ? <fun>) (primitive-relative-ref))))
    ((name . <let*>) (pos . 2) (origin sx-obj . <let*>) (class . constant))
    ((name . module-lexical-binding-refs?) (pos . 56) (origin sx-obj1 . module-lexical-binding-refs?) (inline (G00620 (static-fpi-ref 5) (binding-ref ? <module>) (primitive-relative-ref))))
    ((name . module-named-lambdas!) (pos . 2) (origin sx-obj1 . module-named-lambdas!) (inline (G00638 (stack-ref 1) (static-fpi-ref 9) (stack-ref 2) (binding-ref ? <module>) (set-primitive-relative-ref) (nobble 2))))
    ((name . binding-local-name?) (pos . 20) (origin sx-obj1 . binding-local-name?) (inline (G00596 (static-fpi-ref 6) (binding-ref ? <binding>) (primitive-relative-ref))))
    ((name . var-binding?) (pos . 36) (origin sx-obj2 . var-binding?) (inline (G001457 (static-ref2) (binding-ref ? <var>) (primitive-relative-ref))))
   )
   local-literals (
    (top-level . 223)
    (fun-binding? . 222)
    (fun-body? . 221)
    (lambda-delegated-vars? . 220)
    (fun-range-and-domain! . 219)
    (local-name? . 218)
    (fun-range-and-domain? . 217)
    (if-then? . 216)
    (fun-arity? . 215)
    (save-binding-module-name? . 214)
    (binding? . 213)
    (appl-fun? . 212)
    (if-pred? . 211)
    (lambda-inlined? . 210)
    (if-else! . 209)
    (if-else? . 208)
    (lambda-delegated-vars! . 207)
    (save-binding-local-name? . 206)
    (fun-name? . 205)
    (appl-fun! . 204)
    (if-then! . 203)
    (fun-args? . 202)
    (fun-body! . 201)
    (fun-appls! . 200)
    (binding! . 199)
    (if-pred! . 198)
    (lambda-inlined! . 197)
    (origin-name? . 196)
    (fun-arity! . 195)
    (lambda-binding-refs! . 194)
    (fun-appls? . 193)
    (fun-has-unknown-appls? . 192)
    (fun-binding! . 191)
    (fun-name! . 190)
    (appl-args! . 189)
    (lambda-binding-refs? . 188)
    (fun-args! . 187)
    (fun-has-unknown-appls! . 186)
    (appl-args? . 185)
    (*actual-module* . 153)
    (|(method ifp)| . 143)
    (|(method call-next-method-p)| . 142)
    (|(method applp)| . 141)
    (|(method let*p)| . 140)
    (|(method opencodingp)| . 139)
    (|(method lambdap)| . 138)
    (|(method funp)| . 137)
    (ifp . 136)
    (call-next-method-p . 135)
    (applp . 134)
    (let*p . 133)
    (opencodingp . 132)
    (lambdap . 131)
    (funp . 130)
    ((else: then: pred:) . 129)
    (if . 128)
    (else: . 127)
    (else . 126)
    (then: . 125)
    (then . 124)
    (pred: . 123)
    (pred . 122)
    (call-next-method . 121)
    ((args: fun:) . 120)
    (appl . 119)
    (fun: . 118)
    (let* . 117)
    (opencoding . 116)
    ((binding-refs: delegated-vars: inlined:) . 115)
    (lambda . 114)
    (binding-refs: . 113)
    (binding-refs . 112)
    (delegated-vars: . 111)
    (delegated-vars . 110)
    (inlined: . 109)
    (inlined . 108)
    ((has-unknown-appls: appls: body: range-and-domain: arity: args: binding: name:) . 107)
    (direct-keywords: . 106)
    (direct-slots: . 105)
    (direct-superclasses: . 104)
    (fun . 103)
    (has-unknown-appls: . 102)
    (has-unknown-appls . 101)
    (appls: . 100)
    (appls . 99)
    (body: . 98)
    (body . 97)
    (range-and-domain: . 96)
    (range-and-domain . 95)
    (arity: . 94)
    (arity . 93)
    (args: . 92)
    (args . 91)
    (binding: . 90)
    (binding . 89)
    (keyword: . 88)
    (anonymous . 87)
    (default: . 86)
    (name . 85)
    (name: . 84)
   )
   literals (
   )
))