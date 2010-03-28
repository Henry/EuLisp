;;; EuLisp system 'youtoo'
;;;   Interface file for module boot1

(definterface boot1
  (import ()
   syntax (_boot0)
   full-import ()
   export (
    ((name . stdout) (pos) (origin boot1 . stdout) (class . constant) (value 1))
    ((name . objectp) (pos . 48) (origin boot1 . objectp))
    ((name . equal) (pos . 47) (origin boot1 . equal))
    ((name . write-object) (pos) (origin boot1 . write-object) (class . opencoding) (arity . 2) (opencoding (write-object)))
    ((name . *absent*) (pos . 46) (origin boot1 . *absent*) (class . constant))
    ((name . dec) (pos . 45) (origin boot1 . dec) (inline (G0078 (fpi-dec))))
    ((name . <) (pos . 42) (origin boot1 . <) (inline (G0072 (fpi-lt))))
    ((name . symbolp) (pos . 43) (origin boot1 . symbolp) (inline (G0045 (symbolp))))
    ((name . eql) (pos . 44) (origin boot1 . eql) (inline (G0037 (eq))))
    ((name . setter) (pos . 41) (origin boot1 . setter))
    ((name . mod) (pos . 40) (origin boot1 . mod) (inline (G0070 (fpi-remainder))))
    ((name . make-vector) (pos . 39) (origin boot1 . make-vector))
    ((name . prin) (pos . 38) (origin boot1 . prin))
    ((name . cdr) (pos . 37) (origin boot1 . cdr) (inline (G0084 (cdr))) (setter (G0098 (set-cdr))))
    ((name . make-vector1) (pos . 54) (origin boot1 . make-vector1) (class . ff) (arity . 2) (ff (0 8) 6 (eul_make_vector . ff_stub_eul_make_vector251)))
    ((name . stringp) (pos . 36) (origin boot1 . stringp) (inline (G0043 (stringp))))
    ((name . *) (pos . 35) (origin boot1 . *) (inline (G0064 (fpi-product))))
    ((name . %) (pos . 34) (origin boot1 . %) (inline (G0068 (fpi-remainder))))
    ((name . int-zerop) (pos . 33) (origin boot1 . int-zerop) (inline (G0080 (fpi-zerop))))
    ((name . stderr) (pos) (origin boot1 . stderr) (class . constant) (value 2))
    ((name . member1-string) (pos . 51) (origin boot1 . member1-string) (class . ff) (arity . 2) (ff (1 3) 6 (eul_str_member1 . ff_stub_eul_str_member1248)))
    ((name . make-symbol) (pos . 52) (origin boot1 . make-symbol) (class . ff) (arity . 1) (ff (3) 6 (eul_make_symbol . ff_stub_eul_make_symbol249)))
    ((name . int-as-character) (pos) (origin boot1 . int-as-character) (class . opencoding) (arity . 1) (opencoding (fpi-as-character)))
    ((name . tailstring) (pos . 50) (origin boot1 . tailstring) (class . ff) (arity . 2) (ff (3 0) 3 (eul_tailstr . ff_stub_eul_tailstr247)))
    ((name . intp) (pos . 32) (origin boot1 . intp) (inline (G0049 (fpip))))
    ((name . car) (pos . 31) (origin boot1 . car) (inline (G0082 (car))) (setter (G0096 (set-car))))
    ((name . *argc*) (pos . 30) (origin boot1 . *argc*) (class . constant))
    ((name . time-start) (pos . 29) (origin boot1 . time-start))
    ((name . *argv*) (pos . 28) (origin boot1 . *argv*) (class . constant))
    ((name . list) (pos . 26) (origin boot1 . list))
    ((name . characterp) (pos . 27) (origin boot1 . characterp) (inline (G0041 (characterp))))
    ((name . character-as-int) (pos) (origin boot1 . character-as-int) (class . opencoding) (arity . 1) (opencoding (character-as-fpi)))
    ((name . time-stop) (pos . 25) (origin boot1 . time-stop))
    ((name . inc) (pos . 23) (origin boot1 . inc) (inline (G0076 (fpi-inc))))
    ((name . eq) (pos . 24) (origin boot1 . eq) (inline (G0035 (eq))))
    ((name . exit) (pos . 22) (origin boot1 . exit))
    ((name . getenv) (pos . 21) (origin boot1 . getenv))
    ((name . format1) (pos . 20) (origin boot1 . format1))
    ((name . string-size) (pos . 19) (origin boot1 . string-size) (inline (G0088 (primitive-size))))
    ((name . substring) (pos . 49) (origin boot1 . substring) (class . ff) (arity . 3) (ff (3 0 0) 3 (eul_substr . ff_stub_eul_substr246)))
    ((name . -) (pos . 18) (origin boot1 . -) (inline (G0062 (fpi-difference))))
    ((name . +) (pos . 17) (origin boot1 . +) (inline (G0060 (fpi-sum))))
    ((name . make-keyword) (pos . 53) (origin boot1 . make-keyword) (class . ff) (arity . 1) (ff (3) 6 (eul_make_keyword . ff_stub_eul_make_keyword250)))
    ((name . listp) (pos . 16) (origin boot1 . listp) (inline (G0055 (listp))))
    ((name . consp) (pos . 15) (origin boot1 . consp) (inline (G0047 (consp))))
    ((name . /) (pos . 14) (origin boot1 . /) (inline (G0066 (fpi-quotient))))
    ((name . atom) (pos . 13) (origin boot1 . atom) (inline (G0057 (consp) (null))))
    ((name . =) (pos . 12) (origin boot1 . =) (inline (G0074 (fpi-equal))))
    ((name . simple-generic-function-p) (pos . 11) (origin boot1 . simple-generic-function-p) (inline (G0053 (gfp))))
    ((name . string-ref) (pos . 10) (origin boot1 . string-ref) (inline (G0090 (string-ref))) (setter (G00100 (set-string-ref))))
    ((name . vector-size) (pos . 9) (origin boot1 . vector-size) (inline (G0092 (primitive-size))))
    ((name . vector-ref) (pos . 8) (origin boot1 . vector-ref) (inline (G0094 (primitive-ref))) (setter (G00102 (set-primitive-ref))))
    ((name . null) (pos . 7) (origin boot1 . null) (inline (G0039 (null))))
    ((name . simple-function-p) (pos . 6) (origin boot1 . simple-function-p) (inline (G0051 (lambdap))))
    ((name . system) (pos . 4) (origin boot1 . system))
    ((name . print) (pos . 3) (origin boot1 . print))
    ((name . cons) (pos . 2) (origin boot1 . cons) (inline (G0086 (cons))))
   )
   local-literals (
    (top-level . 163)
    (cons . 162)
    (print . 161)
    (system . 160)
    (set-setter . 159)
    (simple-function-p . 158)
    (null . 157)
    (vector-ref . 156)
    (vector-size . 155)
    (string-ref . 154)
    (simple-generic-function-p . 153)
    (= . 152)
    (atom . 151)
    (/ . 150)
    (consp . 149)
    (listp . 148)
    (+ . 147)
    (- . 146)
    (string-size . 145)
    (format1 . 144)
    (getenv . 143)
    (exit . 142)
    (inc . 141)
    (eq . 140)
    (time-stop . 139)
    (list . 138)
    (characterp . 137)
    (time-start . 136)
    (car . 135)
    (intp . 134)
    (int-zerop . 133)
    (% . 132)
    (* . 131)
    (stringp . 130)
    (cdr . 129)
    (prin . 128)
    (make-vector . 127)
    (mod . 126)
    (setter . 125)
    (< . 124)
    (symbolp . 123)
    (eql . 122)
    (dec . 121)
    (equal . 120)
    (objectp . 119)
    (anonymous . 91)
    ("
" . 72)
    (|(setter vector-ref)| . 69)
    (|(setter string-ref)| . 68)
    (|(setter cdr)| . 67)
    (|(setter car)| . 66)
    ((*absent*) . 65)
    (*absent* . 64)
   )
   literals (
   )
))
