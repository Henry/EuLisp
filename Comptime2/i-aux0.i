;;; EuLisp system 'youtoo'
;;;   Interface file for module i-aux0

(definterface i-aux0
  (import (level1)
   syntax (macros)
   full-import (read symbol random handler table table1 convert1 format list socket stream2 lock stream1 stream vector stream3 float character compare collect fpi number integer copy convert string callback let-cc dynamic thread event condition bit mop-alloc mop-access mop-prim mop-key mop-class mop-init mop-inspect mop-gf mop-meth mop-defcl boot boot1 telos level1)
   export (
    ((name . as-foreign-function-stub-name) (pos . 13) (origin i-aux0 . as-foreign-function-stub-name))
    ((name . as-C-library-link-string) (pos . 18) (origin i-aux0 . as-C-library-link-string))
    ((name . main-link-string) (pos . 12) (origin i-aux0 . main-link-string))
    ((name . destination-link-string) (pos . 27) (origin i-aux0 . destination-link-string))
    ((name . as-C-file-name) (pos . 19) (origin i-aux0 . as-C-file-name))
    ((name . as-C-library-dir-link-string) (pos . 28) (origin i-aux0 . as-C-library-dir-link-string))
    ((name . youtoo) (pos . 21) (origin i-aux0 . youtoo))
    ((name . vm-link-string) (pos . 17) (origin i-aux0 . vm-link-string))
    ((name . as-included-C-file-name) (pos . 11) (origin i-aux0 . as-included-C-file-name))
    ((name . full-C-library-dir-link-string) (pos . 24) (origin i-aux0 . full-C-library-dir-link-string))
    ((name . untrace) (pos . 10) (origin i-aux0 . untrace))
    ((name . as-interface-file-name) (pos . 6) (origin i-aux0 . as-interface-file-name))
    ((name . as-source-file-name) (pos . 20) (origin i-aux0 . as-source-file-name))
    ((name . as-C-library-file-name) (pos . 26) (origin i-aux0 . as-C-library-file-name))
    ((name . as-module-init-flag-name) (pos . 25) (origin i-aux0 . as-module-init-flag-name))
    ((name . destination-library-link-string) (pos . 4) (origin i-aux0 . destination-library-link-string))
    ((name . as-module-init-function-name) (pos . 14) (origin i-aux0 . as-module-init-function-name))
    ((name . as-compiled-C-file-name) (pos . 5) (origin i-aux0 . as-compiled-C-file-name))
    ((name . full-C-library-link-string) (pos . 23) (origin i-aux0 . full-C-library-link-string))
    ((name . as-C-library-interface-file-name) (pos . 22) (origin i-aux0 . as-C-library-interface-file-name))
    ((name . with-ct-handler) (pos . 9) (origin i-aux0 . with-ct-handler))
    ((name . destination-object-string) (pos . 2) (origin i-aux0 . destination-object-string))
    ((name . as-C-hook-object-file-name) (pos . 16) (origin i-aux0 . as-C-hook-object-file-name))
    ((name . as-C-hook-source-file-name) (pos . 15) (origin i-aux0 . as-C-hook-source-file-name))
    ((name . gc-link-string) (pos . 7) (origin i-aux0 . gc-link-string))
    ((name . as-C-hook-name) (pos . 8) (origin i-aux0 . as-C-hook-name))
    ((name . trace) (pos . 3) (origin i-aux0 . trace))
   )
   local-literals (
    (destination-object-string . 163)
    (trace . 162)
    (destination-library-link-string . 161)
    (as-interface-file-name . 160)
    (gc-link-string . 159)
    (as-C-hook-name . 158)
    (with-ct-handler . 157)
    (untrace . 156)
    (as-included-C-file-name . 155)
    (main-link-string . 154)
    (as-foreign-function-stub-name . 153)
    (as-module-init-function-name . 152)
    (as-C-hook-source-file-name . 151)
    (as-C-hook-object-file-name . 150)
    (vm-link-string . 149)
    (as-C-file-name . 148)
    (as-source-file-name . 147)
    (as-C-library-interface-file-name . 146)
    (full-C-library-link-string . 145)
    (full-C-library-dir-link-string . 144)
    (as-module-init-flag-name . 143)
    (destination-link-string . 142)
    (" -L" . 140)
    ("~a~a~a" . 138)
    ("lib~a.a" . 136)
    ("~a~alib~a.a" . 135)
    ("-init-flag" . 133)
    ((let ((str-list (map1-list (lambda (name) (as-C-library-dir-link-string name)) *C-library-load-path*))) (if (null str-list) "" (apply concatenate str-list))) . 131)
    (*C-library-load-path* . 130)
    (as-C-library-dir-link-string . 129)
    ((let ((str-list (map1-list (lambda (name) (as-C-library-link-string name)) *linked-C-libraries*))) (if (null str-list) "" (apply concatenate str-list))) . 127)
    (null . 126)
    (*linked-C-libraries* . 125)
    (as-C-library-link-string . 124)
    (lambda . 123)
    (map1-list . 122)
    (str-list . 121)
    ("lib~a.i" . 119)
    (main . 117)
    (cons . 116)
    (youtoo . 115)
    (".em" . 113)
    (".c" . 111)
    (" -l" . 109)
    (" -leulvm" . 107)
    ("_.o" . 105)
    ("_.o" . 104)
    ("_.c" . 102)
    (*debug* . 100)
    (make-symbol . 99)
    ("-init-fun" . 98)
    ("ff_stub_" . 96)
    (gensym . 95)
    ((let ((name (format () "Lib.~a/eul-appl.o" (get-config-info (quote ARCH))))) (format () "~a~a~a" *eulysses-dir* *delimiter* name)) . 93)
    (*eulysses-dir* . 92)
    (ARCH . 91)
    (quote . 90)
    (get-config-info . 89)
    (name . 88)
    (".h" . 86)
    (with-handler . 83)
    (generic-lambda . 82)
    (method: . 81)
    (*no-ct-handlers* . 80)
    (error . 79)
    (<ct-error> . 78)
    (ct-error-value: . 77)
    (c . 76)
    (f . 75)
    ("_" . 73)
    ((if *no-gc* "" "-lgc") . 71)
    (*no-gc* . 70)
    (".i" . 68)
    (if . 66)
    (".o" . 65)
    (*object-dir* . 64)
    (string-append . 63)
    (".o" . 62)
    (symbol-name . 61)
    (stringp . 60)
    ("~a~a~a" . 58)
    (as-C-library-file-name . 57)
    (progn . 55)
    (car . 54)
    (named-lambda . 53)
    (let . 52)
    ("<<< ~~aTRACE [~a]: ~~a => ~~a
" . 51)
    (res . 50)
    (dynamic-let . 49)
    (concatenate . 48)
    (" " . 47)
    (stderr . 46)
    (dynamic . 45)
    (*trace-indent* . 44)
    (">>> ~~aTRACE [~a]: ~~a
" . 43)
    (apply . 42)
    (args . 41)
    (setq . 40)
    (list . 39)
    (*redefine-imported-bindings* . 38)
    (deflocal . 37)
    (| | . 36)
    (format . 34)
    ("~a~a~a" . 33)
    (*delimiter* . 32)
    (or . 31)
    (*dest-file-name* . 30)
    (as-compiled-C-file-name . 29)
   )
   literals (
   )
))
