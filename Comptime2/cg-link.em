;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: comp (EuLisp to Bytecode Compiler -- EuLysses)
;;;  Authors: Andreas Kind, Keith Playford
;;; Description: linker which decodes assembled code into a C code again
;;;-----------------------------------------------------------------------------
(defmodule cg-link
  (syntax (_macros _i-aux0)
   import (i-all i-modify p-env sx-obj sx-node cg-state cg-asm cg-interf
           i-ffi ex-expr cg-dld)
   export (decode write-C-module-file))

;;;-----------------------------------------------------------------------------
;;; Reset decoder
;;;-----------------------------------------------------------------------------
  (defun reset-decoder (module)
    (let ((module-name (module-name? module)))
      (notify0 "  Reset decoder ...")
      (setq *open-bv* ())
      ;; Reuse only local symbols/keys
      (setq *get-literal* (make-access-table comparator: equal))
      (clear-table *local-bytevectors*)))

;;;-----------------------------------------------------------------------------
;;; Decode into C state; bytevectors like (label size byte ... byte).
;;;-----------------------------------------------------------------------------
  (defun decode (module state)
    (with-ct-handler "decoder error" module
      (setq *pass* 'decode)
      (reset-decoder module)
      (let* ((bv-states (asm-state-bytevectors? state))
             (init-bv-state (asm-state-init-bytevector? state))
             (C-state (make-C-state)))
        (dynamic-let ((*C-state* C-state))
          (set-up-bindings module)
          (set-up-foreign-functions module)
          (set-up-bytevectors bv-states)
          (set-up-init-bytevector init-bv-state module)
          C-state))))

;;;-----------------------------------------------------------------------------
;;; Write C state
;;;-----------------------------------------------------------------------------
  (defun write-C-module-file (module state)
    (and (or *create-C-module* *create-C-library* *stand-alone*)
         (let ((module-name (module-name? module)))
           (notify "  Creating ~a.c ..." module-name)
           (write-C-state module state)
           (write-C-include-file module)
           (and (eq module-name *tmp-start-source-file-name*)
                (or *create-C-library* *stand-alone*)
                (write-C-hook-file module)))))

;;;-----------------------------------------------------------------------------
;;; Globals
;;;-----------------------------------------------------------------------------
  (defun register-new-global (obj . default-index)
    (let* ((module-name (module-name? (dynamic *actual-module*)))
           (index (or (and default-index (car default-index))
                     (global-index)))
           (str (as-C-module-name module-name)))
      (add-global "  ~a_bindings[ ~a] = ~a\;\n" str index obj)
      (and (null default-index)
           (set-global-index (+ index 1)))
      index))

  (defun set-up-bindings (module)
    (let* ((C-module-name (as-C-module-name (module-name? module)))
           (env (module-lexical-env? module))
           (bindings (select-list true-local-binding-p
                                  (access-table-values env)))
           (n (list-size bindings)))
      (and (< 0 n)
           (let ((start-index (global-index)))
             (do1-list (lambda (binding)
                   (let ((index (global-index)))
                     (binding-local-index! binding index)
                     (set-global-index (+ index 1))))
                 bindings)
             (add-global "  {\n    int i\;\n")
             (add-global "    for (i = ~a\; i < ~a\; i++)\n"
                         start-index (global-index))
             (add-global "      ~a_bindings[i] = eul_nil\;\n" C-module-name)
             (add-global "  }\n\n")))))

  (defun set-up-foreign-functions (module)
    (let ((bindings (module-foreign-functions? module)))
      (do1-list (lambda (binding)
            (let* ((ff-spec (get-binding-info binding 'ff))
                   (name (caddr ff-spec))
                   (stub-name (as-foreign-function-stub-name name)))
              ;; For later reuse ...
              ((setter car) (cddr ff-spec) (cons name stub-name))
              (binding-local-index! binding (global-index))
              (notify0 "ff binding ~a: index ~a" name (global-index))
              (register-new-global
               (string-append
                "(LispRef) (LispRef (*) (Stack *, LispRef *, LispRef *)) "
                (as-C-string stub-name)))))
          bindings)))

  (defun set-up-init-bytevector (function-state module)
    (set-up-init-flag)
    (let* ((module-name (module-name? module))
           ;;(init-fun-name (as-module-init-function-name module-name))
           ;;(sym-loc (get-constant-loc init-fun-name))
           (init-code (asm-function-state-code? function-state))
           (new-init-code (wrap-init-code init-code module))
           (bv-loc (make-symbol
                    (format () "~a_bindings[0]" (as-C-module-name module-name))))
           (lambda-name (make-symbol (string-append
                                      "initialize-"
                                      (symbol-name module-name)))))
      (compute-bytevector new-init-code lambda-name bv-loc)))

  (defun set-up-init-flag ()
    (register-new-global "eul_nil" 1))

  (defun wrap-init-code (code module)
    (notify0 "  Wrap bv: ~a" code)
    (let* ((imports (module-used-module-names? module))
           (C-module-name (as-C-module-name (module-name? module)))
           (import-code (wrap-init-code-aux () imports))
           (flag-name (make-symbol (format () "B(~a ,1)" C-module-name))))
      `(135 37 0 0   ; (static-ref-t) (set-binding-ref
        ,flag-name   ;  <initialized-flag>)
        ,import-code ; <import-wrap>
        ,@code       ; <init-code>
        172)        ; (return)
      ))

  (defun wrap-init-code-aux (code imports)
    (if (null imports)
        code
      (let* ((C-module-name (as-C-module-name (car imports)))
             (lambda-name (make-symbol (format () "B(~a ,0)" C-module-name)))
             (flag-name (make-symbol (format () "B(~a ,1)" C-module-name))))
        (add-used-module-name C-module-name)
        (wrap-init-code-aux `(36  0  0 0    ; (binding-ref
                              ,flag-name    ;  <initialized-flag>
                              62 11 36 0    ; (branch-true-pos <offset>)
                                            ; (binding-ref
                              ,lambda-name  ;  <initialization-lambda>)
                              60  0 33 1    ; (call-operator) (pop 1)
                              ,@code)       ; <rest-imports>
                            (cdr imports)))))

  (defun set-up-bytevectors (function-states)
    (do1-list
     (lambda (state)
       (let ((code (asm-function-state-code? state))
             (handle (asm-function-state-handle? state))
             (name (asm-function-state-binding-name? state)))
         (notify0 "  Processing bv: ~s binding-name: ~a code: ~a"
                  handle name code)
         (set-bytevector-pos
          handle (register-new-global (compute-bytevector code name)))))
     function-states))

;;;-----------------------------------------------------------------------------
;;; Local bytevector positions
;;;-----------------------------------------------------------------------------
  (deflocal *local-bytevectors* (make <table>))

  (defun get-bytevector-pos (name)
    (table-ref *local-bytevectors* name))

  (defun set-bytevector-pos (name pos)
    (notify0 "  Set up bytevector: ~a" name)
    ((setter table-ref) *local-bytevectors* name pos))

;;;-----------------------------------------------------------------------------
;;; Literals
;;;-----------------------------------------------------------------------------
  (defgeneric get-constant-loc (obj))

  (defmethod get-constant-loc (obj)
    ;; This might be for strings ...
    (convert-constant obj))

  (defmethod get-constant-loc ((obj <name>))
    ;; There should only be local symbol/key references to reduce
    ;; inter-module dependencies
    ;; Use the C variable to refer symbol/key
    (caddr (get-literal-entry obj)))

  (defgeneric get-literal-entry (obj))

  (defmethod get-literal-entry (obj)
    (let* ((loc (convert-constant obj))
           (index (register-new-global loc))
           (module (dynamic *actual-module*))
           (module-name (module-name? module)))
      (module-local-literals! module
                              (cons (cons obj index)
                                    (module-local-literals? module)))
      (list module-name index loc)))

  (defmethod get-literal-entry ((obj <name>))
    (let* ((entry (*get-literal* obj)))
      (or entry
          (let ((new-entry (call-next-method)))
            ((setter *get-literal*) obj new-entry)
            new-entry))))

;;;-----------------------------------------------------------------------------
;;; Write bytevector
;;;-----------------------------------------------------------------------------
  (deflocal *open-bv* ())
  (deflocal *bytevector-cache* (make <vector> size: 3))
  (deflocal *bytevector-cache-index* 0)
  (deflocal *bytevector-size* 0)
  (defextern as-hex-aux (<int>) ptr "eul_int_as_hex_str")

  (defun as-hex (x)
    (or (as-hex-aux x)
        (ct-error "00" "too many top-level forms after macro expansion")))

  (defun open-bytevector ()
    (reset-code-vector-str)
    (setq *bytevector-size* 0))

  (defun write-next-bv-byte (x)
    (if (numberp x)
        (if (= *bytevector-cache-index* 3)
            (progn
              (check-bv-delimiter)
              (write-to-bv-str "I(~a,~a,~a,~a)"
                               (vector-ref *bytevector-cache* 0)
                               (vector-ref *bytevector-cache* 1)
                               (vector-ref *bytevector-cache* 2)
                               (as-hex x))
              (setq *bytevector-cache-index* 0)
              (setq *bytevector-size* (+ *bytevector-size* 1)))
          (progn
            ((setter vector-ref) *bytevector-cache* *bytevector-cache-index*
             (as-hex x))
            (setq *bytevector-cache-index* (+ *bytevector-cache-index* 1))))
      (progn
        (flush-bytevector-cache)
        (check-bv-delimiter)
        (write-to-bv-str "~a" x))))

  (defun flush-bytevector-cache ()
    (if (= *bytevector-cache-index* 0) ()
      (progn
        (check-bv-delimiter)
        (write-to-bv-str "I(~a,~a,~a,~a)"
                         (vector-ref *bytevector-cache* 0)
                         (if (< 1 *bytevector-cache-index*)
                             (vector-ref *bytevector-cache* 1) "00")
                         (if (< 2 *bytevector-cache-index*)
                             (vector-ref *bytevector-cache* 2) "00")
                         "00")
        (setq *bytevector-cache-index* 0)
        (setq *bytevector-size* (+ *bytevector-size* 1)))))

  (defun close-bytevector (cv-name bv-name binding-name is-init-bv)
    (flush-bytevector-cache)
    (let ((bv-str-list (C-state-code-vector-str? (dynamic *C-state*))))
      (add-code-vector-def
       "  /* Byte-vector with size: ~a is_init: ~a index: ~a binding: ~a */\n"
        *bytevector-size* (if is-init-bv 1 0)
        (if is-init-bv 0 (global-index)) (check-method-name binding-name))
      (add-code-vector-def "  static const void *~a[] = {" cv-name)
      (do1-list add-code-vector-def (reverse bv-str-list))
      (add-code-vector-def "}\;\n\n"))
    (add-initialization "  eul_allocate_bytevector( ~a,~a)\;\n"
                        bv-name cv-name)
    (setq *open-bv* ()))

  (defun check-method-name (x)
    ;; Make sure binding name can be read as one string with C'c scanf
    ;; as binding name can be (method foo) or (setter foo)
    (let ((str (symbol-name x)))
      (if (eq (string-ref str 0) #\()
          (let ((new-str (shallow-copy str)))
            ((setter string-ref) new-str 7 #\-)
            new-str)
        str)))

  (defun check-bv-delimiter ()
    (if *open-bv*
        (write-to-bv-str ",")
      (setq *open-bv* t)))

  (defun write-to-bv-str (str . args)
    (let* ((state (dynamic *C-state*))
           (bv-str-list (C-state-code-vector-str? state))
           (bv-str (car bv-str-list))
           (size (string-size bv-str)))
      (if (< 100 size)  ; avoid to have large strings
          (C-state-code-vector-str!
           state
           (cons (apply format () str args) bv-str-list))
        (C-state-code-vector-str!
           state
           (cons (string-append bv-str (apply format () str args))
                 (cdr bv-str-list))))))

;;;-----------------------------------------------------------------------------
;;; Convert compile-time constants to run-time constants
;;;-----------------------------------------------------------------------------
  (defgeneric convert-constant (value))

  (defmethod convert-constant ((value <int>))
    (make-symbol (format () "c_int_as_eul_int(~a)" value)))

  (defmethod convert-constant ((value <double>))
    (let ((loc (gensym "dbl_")))
      (add-decl loc)
      (add-initialization "  eul_allocate_double(~a,~a)\;\n" loc value)
      loc))

  (defmethod convert-constant ((value <character>))
    (make-symbol (format () "c_char_as_eul_char('~a')" value)))

  (defmethod convert-constant ((value <vector>))
    (let ((loc (gensym "vec_"))
          (init-loc (convert-constant (convert value <list>)))
          (n (vector-size value)))
      (add-decl loc)
      (add-initialization "  eul_allocate_vector(~a,~a,~a)\;\n"
                          loc n init-loc)
      loc))

  (defmethod convert-constant ((value <null>))
    'eul_nil)

  (defun static-allocatable-p (x)
    (null (or (symbolp x) (keywordp x) (null x) (vectorp x) (floatp x))))

  (defmethod convert-constant ((value <cons>))
    (let* ((loc (gensym "cons_"))
           (the-car (car value))
           (the-cdr (cdr value))
           (car-loc (get-constant-loc the-car))
           (cdr-loc (get-constant-loc the-cdr)))
      (add-code-vector-def "  eul_allocate_static_cons(~a, " loc)
      (add-initialization "  object_class(~a) = eul_static_cons_class\;~%"
                          loc)
      (if (static-allocatable-p the-car)
          (if (or (integerp the-car) (characterp the-car))
              (add-code-vector-def "~a, " car-loc)
            (add-code-vector-def "eul_as_static(~a), " car-loc))
        (progn
          (add-code-vector-def "NULL, ")
          (add-initialization "  eul_car(~a) = ~a\;\n" loc car-loc)))
      (if (static-allocatable-p the-cdr)
          (if (or (integerp the-cdr) (characterp the-cdr))
              (add-code-vector-def "~a)\;\n" cdr-loc)
            (add-code-vector-def "eul_as_static(~a))\;\n" cdr-loc))
        (progn
          (add-code-vector-def "NULL)\;\n")
          (add-initialization "  eul_cdr(~a) = ~a\;\n" loc cdr-loc)))
      loc))

  (defmethod convert-constant ((value <string>))
    (let ((loc (gensym "str_"))
          (str (protect-newline
                (protect-doublequote
                 (protect-backslash
                  (protect-tilde value))))))
      (add-code-vector-def "  eul_allocate_static_string(~a, ~s, ~a)\;\n"
                           loc str (string-size value))
      (add-initialization "  object_class(~a) = eul_static_string_class\;~%"
                          loc)
      loc))

  (defmethod convert-constant ((value <symbol>))
    (if (eq value t)
        'eul_true
      (let ((loc (gensym "sym_")))
        (add-decl loc)
        (add-initialization "  eul_intern_symbol(~a,~s)\;\n"
                            loc (symbol-name value))
        loc)))

  (defmethod convert-constant ((value <keyword>))
    (let ((loc (gensym "key_")))
      (add-decl loc)
      (add-initialization "  eul_intern_keyword(~a,~s)\;\n"
                          loc (keyword-name value))
      loc))

;;;-----------------------------------------------------------------------------
;;; Resolving
;;;-----------------------------------------------------------------------------
  (defun compute-bytevector (code binding-name . lambda-locs)
    (let ((cv-name (gensym))  ; constant (void *) vector
          (bv-name (gensym))) ; bytevector object
      (add-decl bv-name)
      (open-bytevector)
      (compute-bytevector-aux code)
      (close-bytevector cv-name bv-name binding-name lambda-locs)
      (if lambda-locs
          ;; watch out for youtoo/feel difference
          ;(add-statement "  eul_allocate_lambda( ~a, \\\"~a\\\", 0, ~a)\;\n"
          (add-statement "  eul_allocate_lambda( ~a, \"~a\", 0, ~a)\;\n"
                         (car lambda-locs) binding-name bv-name)
        ())
      bv-name))

  (defun compute-bytevector-aux (code)
    (if (null code) ()
      (let ((x (car code)))
        (cond
         ((numberp x)
          (write-next-bv-byte x))
         ((symbolp x)
          (check-bv-delimiter)
          (write-to-bv-str "~a" x)
          (setq *bytevector-size* (+ *bytevector-size* 1)))
         ((consp x)
          (let ((key (car x))
                (args (cdr x))
                (arg1 (car (cdr x))))
            (cond
             ((eq key 'CODE-VECTOR)
              (compute-code-vector arg1))
             ((eq key 'STATIC)
              (compute-static arg1))
             ((eq key 'BINDING)
              (compute-binding arg1 (car (cdr args))))
             ((integerp key)
              (compute-bytevector-aux x))
             ((eq key 'FF)
              (compute-foreign-function-binding arg1))
             ;; must be a branch with key=branch-code and arg1=offset
             (t
              (write-next-bv-byte key)
              (write-next-bv-byte arg1))))))
        (compute-bytevector-aux (cdr code)))))

  (defun compute-code-vector (binding-name)
    (let* ((module (dynamic *actual-module*))
           (module-name (module-name? module))
           (local-index (get-bytevector-pos binding-name)))
      (set-fixed-bytes module-name local-index)))

  (defun compute-binding (module-name binding-name)
    (with-ct-handler (format () "can't compute binding ~a of module ~a"
                             binding-name module-name) binding-name
      (notify0 "compute-binding ~a ~a" module-name binding-name)
      (let* ((binding (get-lexical-binding binding-name))
             (origin-module-name
              (if (eq module-name '?)
                  (save-binding-module-name? binding)
                module-name))
             (local-index (binding-local-index? binding)))
        (notify0 "set-fixed-byes ~a ~a ~a"
                 binding-name origin-module-name local-index)
        (set-fixed-bytes origin-module-name local-index))))

  (defun compute-static (obj)
    ;; Static symbols
    (let* ((literal-entry (get-literal-entry obj))
           (module-name (car literal-entry))
           (module (get-module module-name))
           (local-index (cadr literal-entry)))
      (set-fixed-bytes module-name local-index)))

  (defun get-imported-module-or-library (module-name)
    (or (get-module module-name)
        (labels
         ((loop (ll)
                (if (null ll) ()
                  (let ((lib (get-module (car ll))))
                    (if (member1-list module-name
                                (module-all-used-module-names? lib))
                        lib
                      (loop (cdr ll)))))))
         (loop *linked-C-libraries*))))

  (defun set-fixed-bytes (module-name local-index)
    (let ((C-module-name (as-C-module-name module-name)))
      (check-bv-delimiter)
      (write-to-bv-str "B(~a ,~a)" C-module-name local-index)
      (add-used-module-name C-module-name)
      (setq *bytevector-size* (+ *bytevector-size* 1))))

;;;-----------------------------------------------------------------------------
;;; Foreign function call
;;;-----------------------------------------------------------------------------
  (defun compute-foreign-function-binding (binding-name)
    (let* ((binding
            (or (get-lexical-binding binding-name)
                (ct-error
                 -1 "body of inlined function contains non exported binding ~a"
                 binding-name)))
           (module-name (binding-origin-module-name binding))
           (local-index (binding-local-index? binding)))
      (set-fixed-bytes module-name local-index)))

;;;-----------------------------------------------------------------------------
;;; Execute init bytevector
;;;-----------------------------------------------------------------------------
  (defun initialize-imported-modules ()
    (let ((imports (module-used-module-names? (dynamic *actual-module*))))
      (do1-list (lambda (name)
            (write-to-C-file "  initialize_module_~a()\;\n"
                             (as-C-module-name name)))
          imports)))

;;;-----------------------------------------------------------------------------
;;; Link state access
;;;-----------------------------------------------------------------------------
  (defun add-decl (x . args)
    (let ((state (dynamic *C-state*))
          (entry (if (symbolp x) x
                   (apply format () x args))))
      (C-state-decls! state (cons entry (C-state-decls? state)))))

  (defun add-initialization (str . args)
    (let ((state (dynamic *C-state*)))
      (C-state-initializations!
       state
       (cons (apply format () str args) (C-state-initializations? state)))))

  (defun add-code-vector-def (str . args)
    (let ((state (dynamic *C-state*)))
      (C-state-code-vector-defs!
       state
       (cons (apply format () str args) (C-state-code-vector-defs? state)))))

  (defun add-global (str . args)
    (let ((state (dynamic *C-state*)))
      (C-state-globals! state(cons (apply format () str args)
                                   (C-state-globals? state)))))

  (defun add-statement (str . args)
    (let ((state (dynamic *C-state*)))
      (C-state-statements! state (cons (apply format () str args)
                                       (C-state-statements? state)))))

  (defun add-used-module-name (x)
    (let* ((module-name (module-name? (dynamic *actual-module*)))
           (state (dynamic *C-state*))
           (new-name (make-symbol (format () "~a" x)))
           (names (C-state-used-module-names? state)))
      (if (or (eq (make-symbol (as-C-module-name module-name)) new-name)
              (member1-list new-name names)) ()
        (C-state-used-module-names! state (cons new-name names)))))

  (defun global-index ()
    (C-state-global-index? (dynamic *C-state*)))

  (defun set-global-index (i)
    (C-state-global-index! (dynamic *C-state*) i))

  (defun reset-code-vector-str ()
    (C-state-code-vector-str! (dynamic *C-state*) '("")))

;;;-----------------------------------------------------------------------------
;;; Output to C file
;;;-----------------------------------------------------------------------------
  (defun write-C-state (module state)
    (let* ((module-name (module-name? module))
           (file-name (as-C-file-name module-name))
           (absolute-file-name (format () "~a~a~a"
                                       (module-load-dir? module)
                                       *delimiter*
                                       file-name)))
      (with-output-file (stream absolute-file-name)
        (dynamic-let  ((*c-stream* stream))
          (notify0 "Writing link state of module ~a" module-name)
          (write-C-file-header module state)
          (write-to-C-file "  /* Declarations */\n")
          (write-decls-to-C-file state)
          (write-to-C-file "\n  /* Code vector and literal definitions */\n")
          (do1-list write-to-C-file
                    (reverse (C-state-code-vector-defs? state)))
          (write-to-C-file "\n  /* Initializations */\n")
          (do1-list write-to-C-file
                    (reverse (C-state-initializations? state)))
          (write-to-C-file "\n  /* Set local bindings */\n")
          (do1-list write-to-C-file (reverse (C-state-globals? state)))
          (do1-list write-to-C-file (reverse (C-state-statements? state)))
          (write-C-file-end module state)))))

  (defun write-decls-to-C-file (state)
    (let ((decls (C-state-decls? state)))
      (and decls
           (progn
             (write-to-C-file "  LispRef ~a" (car decls))
             (do1-list (lambda (decl)
                   (write-to-C-file ", ~a" decl))
                 (cdr decls))
             (write-to-C-file "\;\n")))))

;;;-----------------------------------------------------------------------------
;;; Write C include file for foreign function in-calls
;;;-----------------------------------------------------------------------------
  (defun write-C-include-file (module)
    (let* ((module-name (module-name? module))
           (C-module-name (as-C-module-name module-name))
           (file-name (as-included-C-file-name module-name))
           (absolute-file-name (format () "~a~a~a"
                                       (module-load-dir? module)
                                       *delimiter*
                                       file-name)))
      (with-output-file (stream absolute-file-name)
        (dynamic-let  ((*c-stream* stream))
          (notify "  Creating ~a ..." file-name)
          (write-copyright-to-C-file module-name "C include file")
          (write-to-C-file "extern LispRef ~a_bindings[]\;\n\n" C-module-name)
          (write-to-C-file "/* Module binding indices */\n")
          (access-table-do
           (lambda (name binding)
             ;; Attention -- name is ptr to C string!
;            (write-to-C-file "#define eul_~a_~a_fn_index ~a \n"
;                             C-module-name
             (write-to-C-file "#define ~a_fn_index ~a \n"
                              (as-C-string (binding-local-name? binding))
                              (binding-local-index? binding)))
                           (module-external-env? module))
          (write-to-C-file "\n\n\n/* eof */\n")))))

;;;-----------------------------------------------------------------------------
;;; Compute C signature string
;;;-----------------------------------------------------------------------------
;;  (defun compute-C-signature-string (binding)
;;    (let* ((ff-spec (get-binding-info binding 'ff))
;;         (arg-convs (car ff-spec))
;;         (res-conv (cadr ff-spec))
;;         (ext-name (caddr ff-spec))
;;         (str (format () "~a (~a) ("
;;                      (res-converter-as-C-type res-conv)
;;                      ext-name)))
;;      (labels ((loop (l)
;;                   (if (null l) (string-append str ")")
;;                     (let ((first (car l))
;;                           (rest (cdr l)))
;;                       (setq str
;;                             (concatenate str
;;                                          (arg-converter-as-C-type first)))
;;                       (and rest
;;                            (setq str (string-append str ", ")))
;;                       (loop rest)))))
;;            (loop arg-convs))))

;;;-----------------------------------------------------------------------------
;;; Write C hook file for root module of stand-alone application
;;;-----------------------------------------------------------------------------
  (defun write-C-hook-file (module)
    (let* ((module-name (module-name? module))
           (C-module-name (as-C-module-name module-name))
           (file-name (as-C-hook-source-file-name module-name))
           (absolute-file-name (format () "~a~a~a"
                                       (module-load-dir? module)
                                       *delimiter*
                                       file-name))
           (n (+ (list-size (module-all-used-module-names? module)) 1)))
      (with-output-file (stream absolute-file-name)
        (dynamic-let ((*c-stream* stream))
          (notify "  Creating hook ~a ..." file-name)
          (write-copyright-to-C-file module-name "C hook file")
          (write-to-C-file "\#include \"eulisp.h\"\n\n")
          (write-to-C-file "\n/* Initialize module ~a ... */\n" module-name)
          (write-to-C-file "extern void initialize_module_~a()\;\n" C-module-name)
          (write-to-C-file "extern LispRef ~a_bindings[]\;\n" C-module-name)
          (write-to-C-file "\n/* Run application ~a ... */\n" module-name)
          (write-to-C-file "void run_application()\n{\n")
          (write-to-C-file "  initialize_module_~a()\;\n" C-module-name)
          (write-to-C-file "  execute_lambda(~a_bindings[0])\;\n}"
                           C-module-name)
          (write-to-C-file "\n\n\n/* eof */\n")))))

;;;-----------------------------------------------------------------------------
;;; Formatted write to C file
;;;-----------------------------------------------------------------------------
  (defun write-to-C-file (str . args)
    (apply format (dynamic *c-stream*) str args))

;;;-----------------------------------------------------------------------------
;;; Write C file header
;;;-----------------------------------------------------------------------------
  (defun write-C-file-header (module state)
    (let* ((module-name (module-name? module))
           (C-module-name (as-C-module-name module-name))
           (direct-names (module-used-module-names? module))
           (n (C-state-global-index? state)))
      (write-copyright-to-C-file module-name "C source file")
      (write-to-C-file "\#include \"eulisp.h\"\n\n")
      (write-to-C-file "\n/* Imported modules */\n")
      (do1-list (lambda (name)
            (write-to-C-file "extern void initialize_module_~a()\;\n"
                             (as-C-module-name name)))
          direct-names)
      (do1-list (lambda (module-name)
            (write-to-C-file "extern LispRef ~a_bindings[]\;\n" module-name))
          (C-state-used-module-names? state))
      (write-to-C-file "\n/* Module bindings with size ~a */\n" n)
      (write-to-C-file "LispRef ~a_bindings[~a]\;\n" C-module-name n)
      (write-to-C-file "\n/* Foreign functions */\n")
      (do1-list (lambda (binding)
                  ;;          (write-to-C-file "extern ~a\;\n\n"
                  ;;                           (compute-C-signature-string binding))
            (write-ff-stub-to-C-file binding))
          (module-foreign-functions? module))
      (write-to-C-file "\n/* Initialize module only once */\n")
      (write-to-C-file "static int is_initialized = 0\;\n")
      (write-to-C-file "\n/* Initialize module ~a */\n" module-name)
      (write-to-C-file "void initialize_module_~a()\n{\n" C-module-name)
      (write-to-C-file "  if (is_initialized) return\;\n")
      (initialize-imported-modules)
      ;(write-to-C-file "  eul_fast_table_set(eul_modules,\\\"~a\\\",(LispRef) ~a_bindings)\;\n" C-module-name C-module-name)  ; youtoo
      (write-to-C-file "  eul_fast_table_set(eul_modules,\"~a\",(LispRef) ~a_bindings)\;\n" C-module-name C-module-name)  ; feel
      (write-to-C-file "  is_initialized = 1\;\n  {\n")))

  (defun write-C-file-end (module state)
    (let ((module-name (module-name? module)))
      (write-to-C-file "\n  }\n}")
      (write-to-C-file "\n\n\n/* eof */\n")))

;;;-----------------------------------------------------------------------------
;;; Foreign-function subs
;;;-----------------------------------------------------------------------------
  (defun write-ff-stub-to-C-file (binding)
    (let* ((ff-spec (get-binding-info binding 'ff))
           (arg-convs (car ff-spec))
           (res-conv (cadr ff-spec))
           (name-pair (caddr ff-spec))
           (ext-name (car name-pair))
           (stub-name (cdr name-pair))
           (arg-names (map1-list (lambda (x) (gensym)) arg-convs))
           (arg-names-str-aux (map1-list (lambda (x)
                                           (format () "~a, " x))
                                         arg-names))
           (arg-names-str (if (null arg-names-str-aux) ""
                            (apply concatenate arg-names-str-aux))))
      (write-to-C-file "static LispRef ~a (" stub-name)
      (write-to-C-file "Stack *reg_value_stack, ")
      (write-to-C-file "LispRef *sreg_value_sp, ")
      (write-to-C-file "LispRef *sreg_value_sb)\n{\n")
      (write-to-C-file "  LispRef ~ares\;\n\n" arg-names-str)
      (do1-list (lambda (arg) (write-to-C-file "  POPVAL1(~a)\;\n" arg))
          (reverse arg-names))
      (write-to-C-file "  FF_RES_CONVERT~a(res,~a(~a" res-conv ext-name
                       (if (null arg-names) ""
                           (format () "FF_ARG_CONVERT~a(~a)"
                                   (car arg-convs)
                                   (car arg-names))))
      ;      (if (null arg-names) ()
      ;       (do2-list (lambda (arg conv)
      ;             (write-to-C-file ", FF_ARG_CONVERT~a(~a)" conv arg))
      ;           (cdr arg-names)
      ;           (cdr arg-convs)))
      (labels
       ((loop (names convs)
              (if (null names) ()
                (let ((arg (car names))
                      (conv (car convs)))
                  (write-to-C-file ", FF_ARG_CONVERT~a(~a)" conv arg)
                  (loop (cdr names) (cdr convs))))))
       (if (null arg-names) ""
         (loop (cdr arg-names) (cdr arg-convs))))
      (write-to-C-file "))\;\n")
      (write-to-C-file "  return res;\n")
      (write-to-C-file "}\n\n")))

;;;-----------------------------------------------------------------------------
;;; Write banner and description
;;;-----------------------------------------------------------------------------
  (defun write-copyright-to-C-file (module-name description)
    (write-to-C-file
"/** ----------------------------------------------------------------------- **
")
    (write-to-C-file
" **                 Generated by EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
")
    (write-to-C-file
" **  Description: ~a of EuLisp module ~a
 **  Copyright: See file ~a.em
 ** ----------------------------------------------------------------------- **/
\n" description module-name module-name))

;;;-----------------------------------------------------------------------------
;;; Convert Lisp identifiers to C identifiers (not very safe!)
;;;-----------------------------------------------------------------------------
  (defun as-C-string (value)
    (let* ((str (format () "~a" value))
           (n (string-size str))
           (new-str (make <string> size: n)))
      (labels
       ((loop (i post-str)
              (if (= i n)
                  post-str
                (let ((c (string-ref str i)))
                  (cond ((eq c #\-)
                         ((setter string-ref) new-str i #\_)
                         (loop (+ i 1) post-str))
                        ((eq c #\_)
                         ((setter string-ref) new-str i #\_)
                         (loop (+ i 1) post-str))
                        ((null (alnump c))
                         ((setter string-ref) new-str i #\_)
                         (loop (+ i 1)
                               (format () "~a_X~a" post-str
                                       (character-as-int c))))
                         (t
                          ((setter string-ref) new-str i c)
                          (loop (+ i 1) post-str)))))))
      (string-append new-str (loop 0 "")))))

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
