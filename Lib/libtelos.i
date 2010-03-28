;;; EuLisp system 'youtoo'
;;;   Library interface file for module telos

(definterface telos
  (import ()
   syntax ()
   full-import (telos boot1 boot mop-defcl mop-meth mop-gf mop-inspect mop-init mop-class mop-key mop-prim mop-access mop-alloc)
   export (
    ((name . <keyword>) (pos . 81) (origin mop-class . <keyword>) (class . constant))
    ((name . slot-name) (pos . 80) (origin mop-class . slot-name) (inline (G00727 (static-ref2) (primitive-ref))) (setter (G00787 (stack-ref 1) (static-ref2) (stack-ref 2) (set-primitive-ref) (nobble 2))))
    ((name . member1-list) (pos . 28) (origin boot . member1-list) (inline (G00134 (static-ref-nil) (memq))))
    ((name . compute-primitive-writer-using-class) (pos . 12) (origin mop-access . compute-primitive-writer-using-class))
    ((name . name-slot-defaults) (pos . 78) (origin mop-class . name-slot-defaults) (class . constant))
    ((name . compute-specialized-slot) (pos . 15) (origin mop-alloc . compute-specialized-slot))
    ((name . init-list-ref) (pos . 27) (origin boot . init-list-ref))
    ((name . method-function) (pos . 76) (origin mop-class . method-function) (inline (G00721 (static-ref2) (primitive-ref))) (setter (G00781 (stack-ref 1) (static-ref2) (stack-ref 2) (set-primitive-ref) (nobble 2))))
    ((name . listify-env-string) (pos . 24) (origin boot . listify-env-string))
    ((name . primitive-metaclass-p) (pos . 14) (origin mop-inspect . primitive-metaclass-p))
    ((name . setter) (pos . 41) (origin boot1 . setter))
    ((name . compute-slot-reader) (pos . 11) (origin mop-access . compute-slot-reader))
    ((name . reverse-list) (pos . 23) (origin boot . reverse-list))
    ((name . class-instance-length) (pos . 74) (origin mop-class . class-instance-length) (inline (G00681 (static-ref1) (primitive-ref))) (setter (G00741 (stack-ref 1) (static-ref1) (stack-ref 2) (set-primitive-ref) (nobble 2))))
    ((name . function-name) (pos . 73) (origin mop-class . function-name) (inline (G00697 (static-ref0) (primitive-ref))) (setter (G00757 (stack-ref 1) (static-ref0) (stack-ref 2) (set-primitive-ref) (nobble 2))))
    ((name . <simple-class>) (pos . 71) (origin mop-class . <simple-class>) (class . constant))
    ((name . class-keywords) (pos . 70) (origin mop-class . class-keywords) (inline (G00689 (static-fpi-ref 5) (primitive-ref))) (setter (G00749 (stack-ref 1) (static-fpi-ref 5) (stack-ref 2) (set-primitive-ref) (nobble 2))))
    ((name . make-vector1) (pos . 54) (origin boot1 . make-vector1) (class . ff) (arity . 2) (ff (0 8) 6 (eul_make_vector . ff_stub_eul_make_vector251)))
    ((name . list-ref) (pos . 20) (origin boot . list-ref))
    ((name . class-direct-superclasses) (pos . 69) (origin mop-class . class-direct-superclasses) (inline (G00683 (static-ref2) (primitive-ref))) (setter (G00743 (stack-ref 1) (static-ref2) (stack-ref 2) (set-primitive-ref) (nobble 2))))
    ((name . warning) (pos . 18) (origin boot . warning))
    ((name . compute-discriminating-function) (pos . 12) (origin mop-meth . compute-discriminating-function))
    ((name . simple-function-code) (pos . 66) (origin mop-class . simple-function-code))
    ((name . slotp) (pos . 12) (origin mop-inspect . slotp))
    ((name . int-zerop) (pos . 33) (origin boot1 . int-zerop) (inline (G0080 (fpi-zerop))))
    ((name . class-slotz) (pos . 65) (origin mop-class . class-slotz) (class . constant))
    ((name . <list>) (pos . 64) (origin mop-class . <list>) (class . constant))
    ((name . method-size) (pos) (origin mop-class . method-size) (class . constant) (value 3))
    ((name . class-direct-subclasses) (pos . 63) (origin mop-class . class-direct-subclasses) (inline (G00685 (static-fpi-ref 3) (primitive-ref))) (setter (G00745 (stack-ref 1) (static-fpi-ref 3) (stack-ref 2) (set-primitive-ref) (nobble 2))))
    ((name . member1-string) (pos . 51) (origin boot1 . member1-string) (class . ff) (arity . 2) (ff (1 3) 6 (eul_str_member1 . ff_stub_eul_str_member1248)))
    ((name . methodp) (pos . 11) (origin mop-inspect . methodp))
    ((name . symbol-name) (pos . 62) (origin mop-class . symbol-name) (inline (G00735 (static-ref0) (primitive-ref))))
    ((name . intp) (pos . 32) (origin boot1 . intp) (inline (G0049 (fpip))))
    ((name . int-binary+) (pos . 17) (origin boot1 . +) (inline (G0060 (fpi-sum))))
    ((name . slot-keyword) (pos . 60) (origin mop-class . slot-keyword) (inline (G00729 (static-fpi-ref 3) (primitive-ref))) (setter (G00789 (stack-ref 1) (static-fpi-ref 3) (stack-ref 2) (set-primitive-ref) (nobble 2))))
    ((name . generic-function-domain) (pos . 61) (origin mop-class . generic-function-domain) (inline (G00703 (static-ref1) (primitive-ref))) (setter (G00763 (stack-ref 1) (static-ref1) (stack-ref 2) (set-primitive-ref) (nobble 2))))
    ((name . sf-size) (pos) (origin mop-class . sf-size) (class . constant) (value 5))
    ((name . time-start) (pos . 29) (origin boot1 . time-start))
    ((name . characterp) (pos . 27) (origin boot1 . characterp) (inline (G0041 (characterp))))
    ((name . make-generic-function) (pos . 16) (origin mop-gf . make-generic-function))
    ((name . int-binary-) (pos . 18) (origin boot1 . -) (inline (G0062 (fpi-difference))))
    ((name . <local-slot>) (pos . 57) (origin mop-class . <local-slot>) (class . constant))
    ((name . inc) (pos . 23) (origin boot1 . inc) (inline (G0076 (fpi-inc))))
    ((name . subclassp) (pos . 9) (origin mop-inspect . subclassp))
    ((name . generic-function-p) (pos . 8) (origin mop-inspect . generic-function-p))
    ((name . <simple-method>) (pos . 55) (origin mop-class . <simple-method>) (class . constant))
    ((name . list-equal) (pos . 47) (origin boot1 . equal))
    ((name . find-key) (pos . 2) (origin mop-key . find-key))
    ((name . <slot>) (pos . 50) (origin mop-class . <slot>) (class . constant))
    ((name . lsd-size) (pos) (origin mop-class . lsd-size) (class . constant) (value 6))
    ((name . list-remove) (pos . 5) (origin boot . list-remove))
    ((name . simple-function-environment) (pos . 48) (origin mop-class . simple-function-environment) (inline (G00701 (static-fpi-ref 3) (primitive-ref))) (setter (G00761 (stack-ref 1) (static-fpi-ref 3) (stack-ref 2) (set-primitive-ref) (nobble 2))))
    ((name . compute-slot-writer) (pos . 10) (origin mop-access . compute-slot-writer))
    ((name . cons-slots) (pos . 47) (origin mop-class . cons-slots) (class . constant))
    ((name . error-no-applicable-methods) (pos . 15) (origin mop-gf . error-no-applicable-methods))
    ((name . int-binary-mod) (pos . 40) (origin boot1 . mod) (inline (G0070 (fpi-remainder))))
    ((name . method-keywords) (pos . 46) (origin mop-class . method-keywords) (class . constant))
    ((name . simple-function-p) (pos . 6) (origin boot1 . simple-function-p) (inline (G0051 (lambdap))))
    ((name . sort-list) (pos . 3) (origin boot . sort-list))
    ((name . finalize) (pos . 14) (origin mop-gf . finalize))
    ((name . name-size) (pos) (origin mop-class . name-size) (class . constant) (value 1))
    ((name . compute-and-ensure-slot-accessors) (pos . 9) (origin mop-access . compute-and-ensure-slot-accessors))
    ((name . write-object) (pos) (origin boot1 . write-object) (class . opencoding) (arity . 2) (opencoding (write-object)))
    ((name . compute-primitive-reader-using-class) (pos . 8) (origin mop-access . compute-primitive-reader-using-class))
    ((name . gf-slot-defaults) (pos . 38) (origin mop-class . gf-slot-defaults) (class . constant))
    ((name . format) (pos . 17) (origin mop-gf . generic-format))
    ((name . generic-function-discriminating-function) (pos . 36) (origin mop-class . generic-function-discriminating-function) (inline (G00713 (static-fpi-ref 7) (primitive-ref))) (setter (G00773 (stack-ref 1) (static-fpi-ref 7) (stack-ref 2) (set-primitive-ref) (nobble 2))))
    ((name . method-slot-defaults) (pos . 35) (origin mop-class . method-slot-defaults) (class . constant))
    ((name . symbolp) (pos . 43) (origin boot1 . symbolp) (inline (G0045 (symbolp))))
    ((name . slot-reader) (pos . 33) (origin mop-class . slot-reader) (inline (G00723 (static-ref0) (primitive-ref))) (setter (G00783 (stack-ref 1) (static-ref0) (stack-ref 2) (set-primitive-ref) (nobble 2))))
    ((name . filter-keywords) (pos . 3) (origin mop-key . filter-keywords))
    ((name . make-vector) (pos . 39) (origin boot1 . make-vector))
    ((name . primitive-prin) (pos . 38) (origin boot1 . prin))
    ((name . cdr) (pos . 37) (origin boot1 . cdr) (inline (G0084 (cdr))) (setter (G0098 (set-cdr))))
    ((name . compatible-superclass-p) (pos . 14) (origin mop-alloc . compatible-superclass-p))
    ((name . sf-slot-defaults) (pos . 30) (origin mop-class . sf-slot-defaults) (class . constant))
    ((name . primitive-allocate) (pos . 3) (origin mop-prim . primitive-allocate) (inline (G00528 (primitive-allocate))))
    ((name . stringp) (pos . 36) (origin boot1 . stringp) (inline (G0043 (stringp))))
    ((name . gf-size) (pos) (origin mop-class . gf-size) (class . constant) (value 9))
    ((name . lsd-slots) (pos . 29) (origin mop-class . lsd-slots) (class . constant))
    ((name . method-slots) (pos . 27) (origin mop-class . method-slots) (class . constant))
    ((name . <null>) (pos . 26) (origin mop-class . <null>) (class . constant))
    ((name . <function-class>) (pos . 25) (origin mop-class . <function-class>) (class . constant))
    ((name . compute-class-precedence-list) (pos . 13) (origin mop-alloc . compute-class-precedence-list))
    ((name . map1-list) (pos . 15) (origin boot . map1-list))
    ((name . slot-value-using-slot) (pos . 10) (origin mop-defcl . slot-value-using-slot))
    ((name . <method>) (pos . 23) (origin mop-class . <method>) (class . constant))
    ((name . lsd-keywords) (pos . 22) (origin mop-class . lsd-keywords) (class . constant))
    ((name . tailstring) (pos . 50) (origin boot1 . tailstring) (class . ff) (arity . 2) (ff (3 0) 3 (eul_tailstr . ff_stub_eul_tailstr247)))
    ((name . primitive-print) (pos . 3) (origin boot1 . print))
    ((name . class-code) (pos . 20) (origin mop-class . class-code) (inline (G00695 (static-fpi-ref 9) (primitive-ref))) (setter (G00755 (stack-ref 1) (static-fpi-ref 9) (stack-ref 2) (set-primitive-ref) (nobble 2))))
    ((name . compute-primitive-writer-using-slot) (pos . 7) (origin mop-access . compute-primitive-writer-using-slot))
    ((name . primitive-slot-value) (pos . 3) (origin mop-inspect . primitive-slot-value))
    ((name . *argv*) (pos . 28) (origin boot1 . *argv*) (class . constant))
    ((name . <simple-generic-function>) (pos . 16) (origin mop-class . <simple-generic-function>) (class . constant))
    ((name . compute-defined-slot-class) (pos . 12) (origin mop-alloc . compute-defined-slot-class))
    ((name . name-keywords) (pos . 9) (origin mop-class . name-keywords) (class . constant))
    ((name . append) (pos . 11) (origin boot . append))
    ((name . function-domain) (pos . 17) (origin mop-class . function-domain) (inline (G00699 (static-ref1) (primitive-ref))) (setter (G00759 (stack-ref 1) (static-ref1) (stack-ref 2) (set-primitive-ref) (nobble 2))))
    ((name . exit) (pos . 22) (origin boot1 . exit))
    ((name . add-method) (pos . 8) (origin mop-meth . add-method))
    ((name . apply) (pos . 9) (origin boot . apply))
    ((name . object-slots) (pos . 12) (origin mop-class . object-slots) (class . constant))
    ((name . object-size) (pos) (origin mop-class . object-size) (class . constant) (value 0))
    ((name . class-abstract-p) (pos . 14) (origin mop-class . class-abstract-p) (inline (G00693 (static-fpi-ref 7) (primitive-ref))) (setter (G00753 (stack-ref 1) (static-fpi-ref 7) (stack-ref 2) (set-primitive-ref) (nobble 2))))
    ((name . converter) (pos . 13) (origin mop-class . converter))
    ((name . generic-function-method-keywords) (pos . 11) (origin mop-class . generic-function-method-keywords) (inline (G00707 (static-fpi-ref 4) (primitive-ref))) (setter (G00767 (stack-ref 1) (static-fpi-ref 4) (stack-ref 2) (set-primitive-ref) (nobble 2))))
    ((name . slot-default) (pos . 10) (origin mop-class . slot-default) (inline (G00731 (static-fpi-ref 4) (primitive-ref))) (setter (G00791 (stack-ref 1) (static-fpi-ref 4) (stack-ref 2) (set-primitive-ref) (nobble 2))))
    ((name . string-size) (pos . 19) (origin boot1 . string-size) (inline (G0088 (primitive-size))))
    ((name . substring1) (pos . 49) (origin boot1 . substring) (class . ff) (arity . 3) (ff (3 0 0) 3 (eul_substr . ff_stub_eul_substr246)))
    ((name . *warning*) (pos . 6) (origin boot . *warning*))
    ((name . consp) (pos . 15) (origin boot1 . consp) (inline (G0047 (consp))))
    ((name . compute-method-lookup-function) (pos . 7) (origin mop-meth . compute-method-lookup-function))
    ((name . string-ref) (pos . 10) (origin boot1 . string-ref) (inline (G0090 (string-ref))) (setter (G00100 (set-string-ref))))
    ((name . initialize) (pos . 12) (origin mop-gf . initialize))
    ((name . ensure-slot-writer) (pos . 6) (origin mop-access . ensure-slot-writer))
    ((name . getchar) (pos . 31) (origin boot . getchar) (class . ff) (arity . 0) (ff () 1 (getchar . ff_stub_getchar396)))
    ((name . system) (pos . 4) (origin boot1 . system))
    ((name . stable-add-method) (pos . 5) (origin mop-meth . stable-add-method))
    ((name . objectp) (pos . 48) (origin boot1 . objectp))
    ((name . *backtrace-nframes*) (pos . 30) (origin boot . *backtrace-nframes*))
    ((name . gf-slots) (pos . 79) (origin mop-class . gf-slots) (class . constant))
    ((name . cons-keywords) (pos . 77) (origin mop-class . cons-keywords) (class . constant))
    ((name . *absent*) (pos . 46) (origin boot1 . *absent*) (class . constant))
    ((name . anyp1-list) (pos . 26) (origin boot . anyp1-list))
    ((name . primitive-ref) (pos . 4) (origin mop-prim . primitive-ref) (inline (G00532 (primitive-ref))) (setter (G00536 (set-primitive-ref))))
    ((name . <class>) (pos . 75) (origin mop-class . <class>) (class . constant))
    ((name . add-subclass) (pos . 9) (origin mop-defcl . add-subclass))
    ((name . int-binary%) (pos . 34) (origin boot1 . %) (inline (G0068 (fpi-remainder))))
    ((name . <cons>) (pos . 72) (origin mop-class . <cons>) (class . constant))
    ((name . member-list) (pos . 19) (origin boot . member-list))
    ((name . sig=) (pos . 11) (origin mop-gf . sig=))
    ((name . primitive-class-of) (pos . 2) (origin mop-prim . primitive-class-of) (inline (G00530 (primitive-class-of))) (setter (G00534 (set-primitive-class-of))))
    ((name . class-slot-defaults) (pos . 68) (origin mop-class . class-slot-defaults) (class . constant))
    ((name . function-slot-defaults) (pos . 67) (origin mop-class . function-slot-defaults) (class . constant))
    ((name . compatible-superclasses-p) (pos . 10) (origin mop-alloc . compatible-superclasses-p))
    ((name . init-class) (pos . 3) (origin mop-init . init-class) (inline (G001330 (stack-ref 7) (stack-ref 7) (stack-ref 1) (static-ref0) (stack-ref 2) (set-primitive-ref) (nobble 2) (pop1) (stack-ref 7) (stack-ref 6) (stack-ref 1) (static-ref1) (stack-ref 2) (set-primitive-ref) (nobble 2) (pop1) (stack-ref 7) (stack-ref 5) (stack-ref 1) (static-ref2) (stack-ref 2) (set-primitive-ref) (nobble 2) (pop1) (stack-ref 7) (stack-ref 4) (stack-ref 1) (static-fpi-ref 3) (stack-ref 2) (set-primitive-ref) (nobble 2) (pop1) (stack-ref 7) (static-ref-nil) (stack-ref 1) (static-fpi-ref 4) (stack-ref 2) (set-primitive-ref) (nobble 2) (pop1) (stack-ref 7) (stack-ref 3) (stack-ref 1) (static-fpi-ref 5) (stack-ref 2) (set-primitive-ref) (nobble 2) (pop1) (stack-ref 7) (stack-ref 2) (cons) (stack-ref 8) (stack-ref 1) (stack-ref 1) (static-fpi-ref 6) (stack-ref 2) (set-primitive-ref) (nobble 2) (pop1) (stack-ref 8) (stack-ref 2) (stack-ref 1) (static-fpi-ref 7) (stack-ref 2) (set-primitive-ref) (nobble 2) (nobble 1) (nobble 8))))
    ((name . assoc-list-ref) (pos . 16) (origin boot . assoc-list-ref))
    ((name . compute-inherited-keywords) (pos . 9) (origin mop-alloc . compute-inherited-keywords))
    ((name . make-symbol) (pos . 52) (origin boot1 . make-symbol) (class . ff) (arity . 1) (ff (3) 6 (eul_make_symbol . ff_stub_eul_make_symbol249)))
    ((name . int-as-character) (pos) (origin boot1 . int-as-character) (class . opencoding) (arity . 1) (opencoding (fpi-as-character)))
    ((name . find-slot-names) (pos . 5) (origin mop-access . find-slot-names))
    ((name . cpl-subclass-p) (pos . 10) (origin mop-inspect . cpl-subclass-p))
    ((name . *argc*) (pos . 30) (origin boot1 . *argc*) (class . constant))
    ((name . method-domain) (pos . 59) (origin mop-class . method-domain) (inline (G00719 (static-ref1) (primitive-ref))) (setter (G00779 (stack-ref 1) (static-ref1) (stack-ref 2) (set-primitive-ref) (nobble 2))))
    ((name . int-binary=) (pos . 12) (origin boot1 . =) (inline (G0074 (fpi-equal))))
    ((name . time-stop) (pos . 25) (origin boot1 . time-stop))
    ((name . generic-function-method-cache) (pos . 58) (origin mop-class . generic-function-method-cache) (inline (G00715 (static-fpi-ref 8) (primitive-ref))) (setter (G00775 (stack-ref 1) (static-fpi-ref 8) (stack-ref 2) (set-primitive-ref) (nobble 2))))
    ((name . eq) (pos . 24) (origin boot1 . eq) (inline (G0035 (eq))))
    ((name . method-generic-function) (pos . 54) (origin mop-class . method-generic-function) (inline (G00717 (static-ref0) (primitive-ref))) (setter (G00777 (stack-ref 1) (static-ref0) (stack-ref 2) (set-primitive-ref) (nobble 2))))
    ((name . gf-keywords) (pos . 56) (origin mop-class . gf-keywords) (class . constant))
    ((name . slot-writer) (pos . 53) (origin mop-class . slot-writer) (inline (G00725 (static-ref1) (primitive-ref))) (setter (G00785 (stack-ref 1) (static-ref1) (stack-ref 2) (set-primitive-ref) (nobble 2))))
    ((name . list-size) (pos . 8) (origin boot . list-size))
    ((name . primitive-find-slot-position) (pos . 7) (origin mop-inspect . primitive-find-slot-position))
    ((name . make-keyword) (pos . 53) (origin boot1 . make-keyword) (class . ff) (arity . 1) (ff (3) 6 (eul_make_keyword . ff_stub_eul_make_keyword250)))
    ((name . class-slots) (pos . 51) (origin mop-class . class-slots) (inline (G00687 (static-fpi-ref 4) (primitive-ref))) (setter (G00747 (stack-ref 1) (static-fpi-ref 4) (stack-ref 2) (set-primitive-ref) (nobble 2))))
    ((name . generic-function-method-class) (pos . 49) (origin mop-class . generic-function-method-class) (inline (G00705 (static-fpi-ref 3) (primitive-ref))) (setter (G00765 (stack-ref 1) (static-fpi-ref 3) (stack-ref 2) (set-primitive-ref) (nobble 2))))
    ((name . allocate) (pos . 9) (origin mop-gf . allocate))
    ((name . make-method) (pos . 3) (origin mop-meth . make-method))
    ((name . find-slot) (pos . 8) (origin mop-defcl . find-slot))
    ((name . the-method-lookup-function) (pos . 7) (origin mop-gf . the-method-lookup-function))
    ((name . compute-slots) (pos . 8) (origin mop-alloc . compute-slots))
    ((name . compute-primitive-reader-using-slot) (pos . 4) (origin mop-access . compute-primitive-reader-using-slot))
    ((name . vector-ref) (pos . 8) (origin boot1 . vector-ref) (inline (G0094 (primitive-ref))) (setter (G00102 (set-primitive-ref))))
    ((name . discriminating-domain) (pos . 6) (origin mop-gf . discriminating-domain))
    ((name . null) (pos . 7) (origin boot1 . null) (inline (G0039 (null))))
    ((name . do1-list) (pos . 4) (origin boot . do1-list))
    ((name . int-binary*) (pos . 35) (origin boot1 . *) (inline (G0064 (fpi-product))))
    ((name . compute-defined-slot) (pos . 7) (origin mop-alloc . compute-defined-slot))
    ((name . <generic-function>) (pos . 45) (origin mop-class . <generic-function>) (class . constant))
    ((name . sf-direct-slot-defaults) (pos . 44) (origin mop-class . sf-direct-slot-defaults) (class . constant))
    ((name . generic-function-methods) (pos . 43) (origin mop-class . generic-function-methods) (inline (G00709 (static-fpi-ref 5) (primitive-ref))) (setter (G00769 (stack-ref 1) (static-fpi-ref 5) (stack-ref 2) (set-primitive-ref) (nobble 2))))
    ((name . cons) (pos . 2) (origin boot1 . cons) (inline (G0086 (cons))))
    ((name . sf-slots) (pos . 42) (origin mop-class . sf-slots) (class . constant))
    ((name . cons-slot-defaults) (pos . 41) (origin mop-class . cons-slot-defaults) (class . constant))
    ((name . slot-value) (pos . 7) (origin mop-defcl . slot-value))
    ((name . pprint) (pos . 3) (origin mop-access . pprint))
    ((name . list-remove-duplicates) (pos . 29) (origin boot . list-remove-duplicates))
    ((name . ensure-slot-reader) (pos . 2) (origin mop-access . ensure-slot-reader))
    ((name . compute-keywords) (pos . 6) (origin mop-alloc . compute-keywords))
    ((name . functionp) (pos . 6) (origin mop-inspect . functionp))
    ((name . sf-direct-slots) (pos . 39) (origin mop-class . sf-direct-slots) (class . constant))
    ((name . dec) (pos . 45) (origin boot1 . dec) (inline (G0078 (fpi-dec))))
    ((name . int-binary<) (pos . 42) (origin boot1 . <) (inline (G0072 (fpi-lt))))
    ((name . eql) (pos . 44) (origin boot1 . eql) (inline (G0037 (eq))))
    ((name . <name>) (pos . 34) (origin mop-class . <name>) (class . constant))
    ((name . primitive-stdout) (pos) (origin boot1 . stdout) (class . constant) (value 1))
    ((name . primitive-stderr) (pos) (origin boot1 . stderr) (class . constant) (value 2))
    ((name . *error*) (pos . 22) (origin boot . *error*))
    ((name . sd-slots) (pos . 31) (origin mop-class . sd-slots) (class . constant))
    ((name . error) (pos . 21) (origin boot . error))
    ((name . generic-function-method-lookup-function) (pos . 32) (origin mop-class . generic-function-method-lookup-function) (inline (G00711 (static-fpi-ref 6) (primitive-ref))) (setter (G00771 (stack-ref 1) (static-fpi-ref 6) (stack-ref 2) (set-primitive-ref) (nobble 2))))
    ((name . anyp2-list) (pos . 17) (origin boot . anyp2-list))
    ((name . predefined-reader) (pos . 6) (origin mop-defcl . predefined-reader))
    ((name . <function>) (pos . 28) (origin mop-class . <function>) (class . constant))
    ((name . sd-size) (pos) (origin mop-class . sd-size) (class . constant) (value 2))
    ((name . class-of) (pos . 5) (origin mop-inspect . class-of) (inline (G001459 (primitive-class-of))))
    ((name . class-size) (pos) (origin mop-class . class-size) (class . constant) (value 10))
    ((name . cons-size) (pos) (origin mop-class . cons-size) (class . constant) (value 2))
    ((name . class-keywordz) (pos . 24) (origin mop-class . class-keywordz) (class . constant))
    ((name . *stack-nvalues*) (pos . 14) (origin boot . *stack-nvalues*))
    ((name . <object>) (pos . 21) (origin mop-class . <object>) (class . constant))
    ((name . backtrace) (pos . 13) (origin boot . backtrace))
    ((name . car) (pos . 31) (origin boot1 . car) (inline (G0082 (car))) (setter (G0096 (set-car))))
    ((name . function-size) (pos) (origin mop-class . function-size) (class . constant) (value 3))
    ((name . sd-keywords) (pos . 19) (origin mop-class . sd-keywords) (class . constant))
    ((name . object-keywords) (pos . 18) (origin mop-class . object-keywords) (class . constant))
    ((name . list) (pos . 26) (origin boot1 . list))
    ((name . character-as-int) (pos) (origin boot1 . character-as-int) (class . opencoding) (arity . 1) (opencoding (character-as-fpi)))
    ((name . keyword-name) (pos . 7) (origin mop-class . keyword-name) (inline (G00737 (static-ref0) (primitive-ref))))
    ((name . mapcan) (pos . 10) (origin boot . mapcan))
    ((name . compute-inherited-slots) (pos . 3) (origin mop-alloc . compute-inherited-slots))
    ((name . gf-reset-cache) (pos . 3) (origin mop-gf . gf-reset-cache))
    ((name . getenv) (pos . 21) (origin boot1 . getenv))
    ((name . class-name) (pos . 15) (origin mop-class . class-name) (inline (G00679 (static-ref0) (primitive-ref))) (setter (G00739 (stack-ref 1) (static-ref0) (stack-ref 2) (set-primitive-ref) (nobble 2))))
    ((name . compute-specialized-slot-class) (pos . 2) (origin mop-alloc . compute-specialized-slot-class))
    ((name . format1) (pos . 20) (origin boot1 . format1))
    ((name . compute-class-codes) (pos . 2) (origin mop-init . compute-class-codes))
    ((name . remove-class) (pos . 4) (origin mop-defcl . remove-class))
    ((name . stack-values) (pos . 7) (origin boot . stack-values))
    ((name . class-precedence-list) (pos . 8) (origin mop-class . class-precedence-list) (inline (G00691 (static-fpi-ref 6) (primitive-ref))) (setter (G00751 (stack-ref 1) (static-fpi-ref 6) (stack-ref 2) (set-primitive-ref) (nobble 2))))
    ((name . predefined-writer) (pos . 3) (origin mop-defcl . predefined-writer))
    ((name . listp) (pos . 16) (origin boot1 . listp) (inline (G0055 (listp))))
    ((name . classp) (pos . 2) (origin mop-inspect . classp))
    ((name . atom) (pos . 13) (origin boot1 . atom) (inline (G0057 (consp) (null))))
    ((name . <symbol>) (pos . 6) (origin mop-class . <symbol>) (class . constant))
    ((name . simple-generic-function-p) (pos . 11) (origin boot1 . simple-generic-function-p) (inline (G0053 (gfp))))
    ((name . make) (pos . 2) (origin mop-gf . make))
    ((name . vector-size) (pos . 9) (origin boot1 . vector-size) (inline (G0092 (primitive-size))))
    ((name . lsd-slot-defaults) (pos . 5) (origin mop-class . lsd-slot-defaults) (class . constant))
    ((name . int-binary/) (pos . 14) (origin boot1 . /) (inline (G0066 (fpi-quotient))))
    ((name . name-slots) (pos . 4) (origin mop-class . name-slots) (class . constant))
    ((name . slot-required-p) (pos . 2) (origin mop-class . slot-required-p) (inline (G00733 (static-fpi-ref 5) (primitive-ref))) (setter (G00793 (stack-ref 1) (static-fpi-ref 5) (stack-ref 2) (set-primitive-ref) (nobble 2))))
    ((name . <simple-function>) (pos . 3) (origin mop-class . <simple-function>) (class . constant))
   )
   literals (
   )
  )
)  ; end of interface