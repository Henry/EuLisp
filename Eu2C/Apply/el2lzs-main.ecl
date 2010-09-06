;;; Copyright 1994-2010 Fraunhofer ISST
;;; Copyright 2010 Henry G. Weller
;;;-----------------------------------------------------------------------------
;;  This file is part of
;;; ---                           EuLisp System 'Eu2C'
;;;-----------------------------------------------------------------------------
;;
;;  Eu2C is free software: you can redistribute it and/or modify it under the
;;  terms of the GNU General Public License version 2 as published by the Free
;;  Software Foundation.
;;
;;  Eu2C is distributed in the hope that it will be useful, but WITHOUT ANY
;;  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;  details.
;;
;;  You should have received a copy of the GNU General Public License along with
;;  this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;-----------------------------------------------------------------------------
;;;  Title: EuLisp-to-LZS-Transformer: the common parts
;;;  Description:
;;    This module contains all things which are needed for the implementation of
;;    transformation rules (implemented in EL2LZS-RULES) and for the work with
;;    LZS-modules. It provides also the function TRANS-MODULE which transforms an
;;    EuLisp-Module in list-representation into an LZS-module.
;;;  Documentation:
;;;  Notes:
;;    Up to now no error-checking or error-handling takes places. This means, that
;;    the incoming EuLisp-module must be in correct syntax.
;;;  Requires:
;;;  Problems:
;;    To avoid module dependency cycles type-inference::set-signature-from-classes was
;;    used with explicit package qualifier. In the future el2lzs-main should be
;;    divided into 2 modules.
;;;  Authors: Ingo Mohr
;;;-----------------------------------------------------------------------------

#module el2lzs-main
(import ((except (member
                  concatenate)
                 level-0)
         dynamic
         class-ext
         (only (first
                second
                third
                fourth
                nthcdr
                caar
                concatenate
                intern
                push
                pushnew
                string
                make-instance
                delete-if
                delete-if-not
                class-of
                find-if
                delete
                delete-duplicates
                remove-if-not
                cddr
                nsubst
                append
                mapcar
                assoc
                mapc
                mapcan
                member
                reverse
                subclass?
                nconc
                get
                list?
                vector)
               common-lisp)
         lzs accessors
         binding
         el2lzs-load
         el2lzs-error)
 syntax (level-0
         dynamic
         el2lzs-basic
         class-ext
         (only (case
                declare
                ignore)
               common-lisp))
 expose (lzs
         accessors
         el2lzs-basic)
 expose ((only (first
                rest
                cadar
                caar
                make-instance
                ignore declare
                case)
               common-lisp)))

;;;-----------------------------------------------------------------------------
;;; For debugging and error messages
;;;-----------------------------------------------------------------------------
(export-syntax with-defining-form)

(defmacro with-defining-form body
  `(dynamic-let ((current-defining-form
                  (or (dynamic current-defining-form) ;to shadow mapping of def-forms
                      (whole-form))))
                ,@body))

;;;-----------------------------------------------------------------------------
;;; initialization of transformations
;;;-----------------------------------------------------------------------------

(define-transformation trans)

(define-transformation transmod)

(define-transformation transdef)

(define-transformation transsyn)

;;;-----------------------------------------------------------------------------
;;;  some environments for transformations
;;;-----------------------------------------------------------------------------

(export find-in-env
 find-in-lex-env find-in-symbol-env find-in-dynamic-env find-in-mac-env
 find-lexical-binding
 find-syntax-binding
 make-undefined-function
 add-to-lex-env add-to-lex-env* in-lex-env
 find-module
 module-env
 env-plus
 reset-environments
 )

(export-syntax lex-env ; only an identifier, nesseciary when using in-lex-env
               )

;;ATTENTION: the environments are not handled right, the lex must be
;;module-specific and the dynamic and the symbol must be global!!! ?????

(defun find-in-env (env id)
  (get-lzs-object
   (find-binding env id)))

(defun find-binding (env id)
  (find-if (lambda (el)
             (eq id (?identifier el)))
           env))

;; adds one new element to an environment if it is not NIL
;; in the cases when this element cannot be NIL cons is used for efficiency
(defun env-plus (new env)
  (if (null? new)
      env
    (cons new env)))

;;;-----------------------------------------------------------------------------
(defglobal lex-env ())      ;; the lexical environment which can grow and shrink

(defun find-in-lex-env (id)
  (or (find-in-env (dynamic lex-env) id)
      (find-in-symbol-env id) ;;***HGW added to handle keywords as symbols
      (error-no-lexical-binding id)))

(defun find-lexical-binding (id)
  (or (find-binding (dynamic lex-env) id)
      (error-no-lexical-binding id)))

(defun add-top-lexical (obj)
  (cond ((null? (member (?identifier obj) (dynamic lex-env)
                       :key #'?identifier))
         (push obj (dynamic lex-env))
         t)
        ((member (?identifier obj)
                 (?lex-env (dynamic *current-module*)) ;here are only the
                 ;;imported bindings
                 :key #'?identifier)
         (error-redefinition-of-imported-lexical obj)
         ())
        (t (error-redefinition-of-top-lexical obj)
           ())))

(defun add-to-lex-env (obj)
  (push obj (dynamic lex-env)))

(defun add-to-lex-env* (objects)
  (dynamic-setq lex-env
                (append objects (dynamic lex-env))))

(defmacro in-lex-env (new-env . forms)
  `(let ((lex-env (dynamic lex-env)))
     (dynamic-let ((lex-env ,new-env))
                  ,@forms)))

;;;-----------------------------------------------------------------------------
(defglobal mac-env ())      ;; the lexical environment for syntax expansion

(defun find-in-mac-env (id)
  (find-in-env (dynamic mac-env) id))

(defun add-to-mac-env (obj)
  (push obj (dynamic mac-env)))

;;;-----------------------------------------------------------------------------
(defglobal symbol-env ())   ;; the set of used symbols

(defun find-in-symbol-env (id)
  (find-in-env (dynamic symbol-env) id))

(defmacro add-to-symbol-env (obj)
  `(push ,obj (dynamic symbol-env)))

;;;-----------------------------------------------------------------------------
(defglobal dynamic-env ())  ;; the set of dynamic variables
;; (all are viewed as global ones)
(defun find-in-dynamic-env (id)
  (find-in-env (dynamic dynamic-env) id))

(defmacro add-to-dynamic-env (obj)
  `(push ,obj (dynamic dynamic-env)))

(defglobal undefined-functions () )

(defun make-undefined-function (fun)
  (let ((undefined-fun (find-in-env (dynamic undefined-functions) fun)))
    (unless undefined-fun
            (setq undefined-fun
                  (make-instance <global-fun>
                                 :identifier fun
                                 :params ()
                                 :body 'undefined))
            (dynamic-setq undefined-functions
                          (nconc (dynamic undefined-functions) (list undefined-fun))))
    undefined-fun))

;;;-----------------------------------------------------------------------------
(deflocal module-env ())   ;; the set of modules

(defun find-module (id)
  (find-in-env module-env id))

(defmacro add-to-module-env (mod)
  `(let ((mod ,mod))
     (push mod module-env)
     mod))

;;;-----------------------------------------------------------------------------

(defun reset-environments ()
  (setq module-env (list (find-module ^%tail)))
  (dynamic-setq dynamic-env ())
  (dynamic-setq symbol-env ()))

;;;-----------------------------------------------------------------------------
;;; %TAIL: the basic module
;;;-----------------------------------------------------------------------------
(defconstant $tail-module (make-instance <module> :identifier ^%tail))
(add-to-module-env $tail-module)
(export $tail-module)

;;;-----------------------------------------------------------------------------
;;;  auxilliary functions
;;;-----------------------------------------------------------------------------
(defun instance-of? (instance class)
  (subclass? (class-of instance) class))

(defun find-instances-of (zws-class environment)
  (remove-if-not (lambda (obj)
                   (instance-of? obj zws-class))
                 environment))

(defun make-defined-sym (id)
  ;; creates a <defined symbol> in package "eulisp" and adds it to the global symbol
  ;; environment
  (let ((sym (find-in-symbol-env id)))
    (unless sym
            (setq sym (make-instance <defined-sym>
                                     :name (string id)
                                     :package "eulisp"
                                     :identifier id))
            (add-to-symbol-env sym))
    sym))

(export instance-of? find-instances-of make-defined-sym)

;;;-----------------------------------------------------------------------------
;;; transformation of a module definition: the main function
;;;-----------------------------------------------------------------------------

(defglobal waiting-export-directives ())

(defun trans-module (module-def)
  (let* ((name (second module-def))
         body
         (%unsigned-word-integer           ; to avoid recursive modules
          (find-in-env (?lex-env (find-module 'es::%tail))
                       'es::%unsigned-word-integer))
         (%eq              ;; to avoid recursive modules
          (find-in-env (?lex-env (find-module 'es::%tail))
                       'es::%eq))
         ($unsigned-0 (make-instance <literal-instance>
                                     :class %unsigned-word-integer
                                     :value-list '(0)))
         ($unsigned-1 (make-instance <literal-instance>
                                     :class %unsigned-word-integer
                                     :value-list '(1)))
         (initflag (make-instance <global-static>
                                  :identifier (list ^init name ^done)
                                  :module ()
                                  :type %unsigned-word-integer
                                  :initial-value $unsigned-0)))
    (dynamic-let ((*current-module* (make-instance <module> :identifier name))
                  (waiting-export-directives ()))
                 (setq body (trans-module-header module-def))
                 (setf (?module initflag) (dynamic *current-module*))

                 (dynamic-let ((mac-env (?syntax-env (dynamic *current-module*))))
                              (setq body
                                    (transsyn (cons 'es::progn body)))); syntaxexpands the forms and
                 ;; simplifies syntax as embedded progn's
                 ;; and implicit progn's

                 (setq body             ;; this is neccesary because of the
                       ;; progn-simplification
                       (cond ((null? body) ())         ; no expressions?
                             ((and (cons? body)        ; more than 1 expression?
                                   (eq (first body) 'es::progn))
                              (cdr body))
                             (t (list body))))         ; exactly 1 expression

                 (dynamic-let ((lex-env (?lex-env (dynamic *current-module*))) ;imported bindings
                               (dynamic-env (remove-if-not #'dynamic?
                                                           (?var-list (dynamic *current-module*))))
                               ;;(symbol-env (?sym-list (dynamic *current-module*)))
                               (undefined-functions ()))

                              (setq body (collect-top-lexicals body))

                              ;; now the module holds its top-level lexical environment and the object-lists
                              ;; are filled with the global defined objects with empty structure part

                              ;;now handle the collected export-directives from the module header
                              (trans-export-directives (dynamic waiting-export-directives))

                              (setq body (mapcan #'transdef body)); transforms bodies of defs, handles
                              ;; exports and collects top-level forms
                              ;;  (make-interface-file (dynamic lex-env)
                              ;;                       (dynamic symbol-env))

                              (push initflag (?var-list (dynamic *current-module*)))
                              (setq body
                                    (make-instance <if-form>
                                                   :pred
                                                   (make-instance <app>
                                                                  :function %eq
                                                                  :arg-list (list (make-instance <var-ref> :var initflag)
                                                                                  $unsigned-0))
                                                   :then
                                                   (make-instance <progn-form>
                                                                  :form-list (append (used-modules-initialization
                                                                                      (?used-runtime-modules (dynamic *current-module*)))
                                                                                     body
                                                                                     (list (make-instance <setq-form>
                                                                                                          :location (make-instance <var-ref>
                                                                                                                                   :var initflag)
                                                                                                          :form $unsigned-1))))
                                                   :else
                                                   $unsigned-1))

                              (setf (?toplevel-forms (dynamic *current-module*))
                                    (make-instance <module-init-fun>
                                                   :range-and-domain (vector %unsigned-word-integer)
                                                   :params (make-instance <params>)
                                                   :body body
                                                   :exported ()
                                                   :identifier (module-init-function-name name)
                                                   :module (dynamic *current-module*)))
                              (cl:funcall (cl:symbol-function
                                           (cl:find-symbol (string 'set-signature-from-classes)
                                                           "TI-SIGNATURE"))
                                          (?toplevel-forms (dynamic *current-module*))
                                          (list %unsigned-word-integer))

                              ;; the extension of the module list was placed here to guarantee that all
                              ;; modules which are needed by import or by expose are already in the module
                              ;; environment
                              (add-to-module-env (dynamic *current-module*))

                              ;; reverse the collected objects in the module such that the order of
                              ;; definitions in the generated code is the same as in the source
                              (setf (?fun-list (dynamic *current-module*))
                                    (reverse (?fun-list (dynamic *current-module*))))
                              (setf (?class-def-list (dynamic *current-module*))
                                    (reverse (?class-def-list (dynamic *current-module*))))
                              (setf (?named-const-list (dynamic *current-module*))
                                    (reverse (?named-const-list (dynamic *current-module*))))
                              (setf (?var-list (dynamic *current-module*))
                                    (reverse (?var-list (dynamic *current-module*))))
                              (setf (?sym-list (dynamic *current-module*))
                                    (reverse (?sym-list (dynamic *current-module*))))

                              (dynamic *current-module*)
                              ))))

(export trans-module)

(defun collect-top-lexicals (body)
  ;; collects top-level lexical bindings and removes forms which redefines
  ;; previously defined top lexicals
  (setq body
        (mapcar (lambda (form)
                  (dynamic-let ((current-defining-form form))
                               ;;to provide the current form for error message output
                               (if (add-top-lexical-bindings (transmod form))
                                   form ())))
                body))
  (setf (?lex-env (dynamic *current-module*)) (dynamic lex-env))
  body)

(defun add-top-lexical-bindings (binding-s)
  (cond ((null? binding-s) t)
        ((null? (cons? binding-s))
         (add-top-lexical binding-s))
        ((add-top-lexical-bindings (car binding-s))
         (add-top-lexical-bindings (cdr binding-s)))
        (t ;if the car leads to an error then the rest of the lexical bindings
         ;;should also be added, at least to get possible error messages,
         ;;but the overall result is FALSE
         (add-top-lexical-bindings (cdr binding-s))
         ())))

(defun module-init-function-name (module-name)
  (list ^init module-name))

(defun used-modules-initialization (modules)
  (if (null? modules) ()
    (let ((initfun (?toplevel-forms (car modules))))
      (if (null? initfun)
          (used-modules-initialization (cdr modules))
        (cons (make-instance <app> :function initfun :arg-list () )
              (used-modules-initialization (cdr modules)))))))

(defun splice-lists (l)
  (cond ((null? l) ())
        ((list? (car l))
         (nconc (car l) (splice-lists (cdr l))))
        ((null? (cdr l)) l)
        (t (splice-lists-1 l))))

(defun splice-lists-1 (l)
  ;;l has at least 2 elements and«its first one isn't a list
  (cond ((null? (cdr l))
         (if (list? (car l)) (car l) l))
        ((list? (second l))
         (setf (cdr l)
               (nconc (second l) (splice-lists (cddr l))))
         l)
        ((null? (cddr l)) l)
        (t (splice-lists (cdr l))
           l)))

;;;-----------------------------------------------------------------------------
;;; transformation of interface modules
;;;-----------------------------------------------------------------------------

(export set-module-init-function)

(defun trans-if-module (module-def)
  (let* ((name (second module-def))
         body)
    (dynamic-let ((*current-module* (make-instance <module> :identifier name))
                  (waiting-export-directives ()))
                 (setq body (trans-module-header module-def))

                 (dynamic-let ((lex-env (?lex-env (dynamic *current-module*))) ;imported bindings
                               (dynamic-env (remove-if-not #'dynamic?
                                                           (?var-list (dynamic *current-module*))))
                               ;;(symbol-env (?sym-list (dynamic *current-module*)))
                               (undefined-functions ()))

                              (setq body (collect-top-lexicals body))

                              ;; now the module holds its top-level lexical environment and the object-lists
                              ;; are filled with the global defined objects with empty structure part

                              ;;now handle the collected export-directives from the module header
                              (trans-export-directives (dynamic waiting-export-directives))

                              (setq body (mapcan #'transdef body)); transforms bodies of defs, handles
                              ;; exports and collects top-level forms
                              ;;  (make-interface-file (dynamic lex-env)
                              ;;                       (dynamic symbol-env))

                              ;; the default-function in ?toplevel-forms is set by %annotate-function with key
                              ;; init-function

                              ;; the extension of the module list was placed here to guarantee that all
                              ;; modules which are needed by import or by expose are already in the module
                              ;; environment
                              (add-to-module-env (dynamic *current-module*))

                              ;; reverse the collected objects in the module such that the order of
                              ;; definitions in the generated code is the same as in the source
                              (setf (?fun-list (dynamic *current-module*))
                                    (reverse (?fun-list (dynamic *current-module*))))
                              (setf (?class-def-list (dynamic *current-module*))
                                    (reverse (?class-def-list (dynamic *current-module*))))
                              (setf (?named-const-list (dynamic *current-module*))
                                    (reverse (?named-const-list (dynamic *current-module*))))
                              (setf (?var-list (dynamic *current-module*))
                                    (reverse (?var-list (dynamic *current-module*))))
                              (setf (?sym-list (dynamic *current-module*))
                                    (reverse (?sym-list (dynamic *current-module*))))

                              (dynamic *current-module*)
                              ))))

(defun set-module-init-function (fun key value)
  (setf (?toplevel-forms (dynamic *current-module*))
        fun))

;;;-----------------------------------------------------------------------------
;;; transformation of module header
;;;-----------------------------------------------------------------------------

(defun trans-module-header (module-def)
  (if (has-old-style-module-header module-def)
      (trans-old-style-module-header module-def)
    (progn (trans-directives (third module-def))
           (cdr (cddr module-def)))))

(defun has-old-style-module-header (module-def)
  (and (>= (length module-def) 4)
       (cons? (fourth module-def))
       (eq ^syntax (car (fourth module-def)))))

(defun trans-old-style-module-header (module-def)
  (info-old-style-module-header module-def)
  (trans-imports (third module-def))    ; creates the imported lexical
  ;; environment
  (trans-old-style-syntax-imports (fourth module-def))
  (trans-old-style-local-syntax-definitions (cddr (fourth module-def)))
  (cddr (cddr module-def)))

(defun trans-directives (module-directives)
  (cond ((null? module-directives) ())
        ((null? (cons? module-directives))
         (error-bad-module-directive module-directives ()))
        ((null? (cdr module-directives))
         (error-bad-module-directive (car module-directives) ()))
        ((null? (member (first module-directives)
                       ^(import syntax expose export c-import)))
         (error-bad-module-directive (first module-directives)
                                     (second module-directives))
         (trans-directives (cddr module-directives)))
        ((null? (list? (second module-directives)))
         (error-bad-module-directive (first module-directives)
                                     (second module-directives))
         (trans-directives (cddr module-directives)))
        (t
         (trans-directive (first module-directives)
                          (second module-directives))
         (trans-directives (cddr module-directives)))))

(defun trans-directive (key value)
  (cond ((eq key ^export)
         ;; this must wait until all imports and all global
         ;; lexicals are collected
         (dynamic-setq waiting-export-directives
                       (cons value (dynamic waiting-export-directives))))
        ((eq key ^expose)
         (trans-expose value))
        ((eq key ^import)
         (trans-imports value))
        ((eq key ^syntax)
         (trans-syntax-imports value))
        ((eq key ^c-import)
         (trans-c-imports value))
        ))

(defun trans-imports (import-specs)
  (setf (?lex-env (dynamic *current-module*))
        (append
         (compute-runtime-interface import-specs)
         (?lex-env (dynamic *current-module*)))))

(defun trans-syntax-imports (import-specs)
  (setf (?syntax-env (dynamic *current-module*))
        (append
         (compute-syntax-interface import-specs)
         (?syntax-env (dynamic *current-module*)))))

(defun trans-expose (import-specs)
  (let ((iface (compute-interface import-specs t t)))
    (setf (?exports (dynamic *current-module*))
          (append (if-import iface)
                  (?exports (dynamic *current-module*))))
    (setf (?syntax-exports (dynamic *current-module*))
          (append (if-syntax iface)
                  (?syntax-exports (dynamic *current-module*))))
    ;;    (format t "~%exposed by ~A: ~%import=~A~%syntax=~A"
    ;;            (?identifier (dynamic *current-module*))
    ;;            (mapcar #'?identifier (if-import iface))
    ;;            (mapcar #'?identifier (if-syntax iface)))
    ))

(defun trans-c-imports (file-spec-list)
  (mapc (lambda (file)
          (if (or (symbol? file)
                  (string? file))
              (setf (?c-imports (dynamic *current-module*))
                    (cons file (?c-imports (dynamic *current-module*))))
            (error-bad-c-import-spec file)))
        file-spec-list))

(defun trans-export-directives (export-directives)
  (mapc (lambda (export-spec)
          (transdef (cons ^export export-spec)))
        export-directives))

;;;-----------------------------------------------------------------------------
;;; analyzing import specification
;;;-----------------------------------------------------------------------------

(defun compute-runtime-interface (import-specs)
  (if-import (compute-interface import-specs t ())))

(defun compute-syntax-interface (import-specs)
  (if-syntax (compute-interface import-specs () t)))

(defgeneric trans-old-style-syntax-imports (import-specs))

(defmethod trans-old-style-syntax-imports ((import-specs <null>))
  ())

(defmethod trans-old-style-syntax-imports ((import-specs <pair>))
  (if (and (eq (first import-specs) ^syntax)
           (cdr import-specs)
           (list? (second import-specs)))
      (setf (?syntax-env (dynamic *current-module*))
            (append
             (compute-syntax-interface (second import-specs))
             (?syntax-env (dynamic *current-module*))))
    (error-bad-old-style-syntax-import import-specs)
    ))

(defmethod trans-old-style-syntax-imports (import-specs)
  (error-bad-old-style-syntax-import import-specs))




(defun compute-interface (import-specs import? syntax?)
  (let ((iface (compute-interface-without-check import-specs import? syntax?)))
    (mk-if
     (delete-duplicates (if-import iface)
                        :test #'equal-binding?)
     (delete-duplicates (if-syntax iface)
                        :test #'equal-binding?))))

(defun equal-binding? (binding1 binding2)
  (cond ((eq binding1 binding2) ; a shortcut for the following test
         t)
        ((eq (?identifier binding1)
             (?identifier binding2))
         (unless (eq (get-lzs-object binding1)
                     (get-lzs-object binding2))
                 (error-name-clash binding1
                                   binding2))
         t)
        (t ())))

(defun compute-interface-without-check (import-specs import? syntax?)
  (append-if-s
   (mapcar (lambda (import-spec)
             (compute-interface-1 import-spec import? syntax?))
           import-specs)))

(defun compute-interface-1 (import-spec import? syntax?)
  (if (symbol? import-spec)            ; a module name
      (mk-if (and import? (module-runtime-interface import-spec))
             (and syntax? (module-syntax-interface import-spec)))
    (let ((interface (compute-interface-without-check (cddr import-spec)
                                                      import? syntax?)))
      (case (car import-spec)
            (ES::except (except-interface (second import-spec) interface import-spec))
            (ES::only   (only-interface   (second import-spec) interface import-spec))
            (ES::rename (rename-interface (second import-spec) interface import-spec))))))

;; an interface in this context is a pair of import bindins and syntax bindings
(defun if-import (interface)
  (car interface))

(defun if-syntax (interface)
  (cdr interface))

(defun add-if (import syntax interface)
  (mk-if (if import (cons import (if-import interface)) (if-import interface))
         (if syntax (cons syntax (if-syntax interface)) (if-syntax interface))))

(defun mk-if (import syntax)
  (cons import syntax))

(defun if-diff (if1 if2)
  (mk-if (if-diff-1 (if-import if1)
                    (if-import if2))
         (if-diff-1 (if-syntax if1)
                    (if-syntax if2))))

(defun if-diff-1 (if1 if2)
  (cond ((null? if1) ())
        ((member (car if1) if2)
         (if-diff-1 (cdr if1) if2))
        (t (cons (car if1)
                 (if-diff-1 (cdr if1) if2)))))

(defun append-if-s (if-list)
  (mk-if (mapcan #'if-import if-list)
         (mapcan #'if-syntax if-list)))

(defun except-interface (identifiers interface if-spec)
  (if-diff interface
           (only-interface identifiers interface if-spec)))

(defun only-interface (identifiers interface if-spec)
  (if (null? identifiers) ()
    (let ((object-in-import
           (member (car identifiers) (if-import interface)
                   :key #'?identifier))
          (object-in-syntax
           (member (car identifiers) (if-syntax interface)
                   :key #'?identifier)))
      (cond ((and object-in-import object-in-syntax)
             (warning-binding-in-import-and-syntax (car identifiers) if-spec)
             (add-if (and object-in-import (car object-in-import))
                     ()
                     (only-interface (cdr identifiers) interface if-spec)))
            ((or object-in-import object-in-syntax)
             (add-if (and object-in-import (car object-in-import))
                     (and object-in-syntax (car object-in-syntax))
                     (only-interface (cdr identifiers) interface if-spec)))
            (t
             (warning-non-existent-binding-in-interface (car identifiers) if-spec)
             (only-interface (cdr identifiers) interface if-spec))))))

(defun rename-interface (rename-spec interface if-spec)
  (let ((bindings-to-be-renamed
         (only-interface (mapcar #'car rename-spec)
                         interface
                         if-spec)))
    (mk-if (mapcar (lambda (binding)
                     (rename-binding binding (if-import bindings-to-be-renamed) rename-spec))
                   (if-import interface))
           (mapcar (lambda (binding)
                     (rename-binding binding (if-syntax bindings-to-be-renamed) rename-spec))
                   (if-syntax interface)))))

(defun rename-binding (binding bindings-to-be-renamed rename-spec)
  (cond ((null? bindings-to-be-renamed) binding)
        ((eq binding (car bindings-to-be-renamed))
         (make-binding :identifier (second (car rename-spec))
                       :refers-to binding))
        (t (rename-binding binding (cdr bindings-to-be-renamed) (cdr rename-spec)))))

(defun module-runtime-interface (module-id)
  (let ((module (find-or-load-module module-id)))
    (if module
        (progn
          (pushnew module (?used-runtime-modules (dynamic *current-module*))
                   :key #'?identifier)
          (copy-list (?exports module)))
      ())))

(defun module-syntax-interface (module-id)
  (let ((module (find-or-load-module module-id)))
    (if module
        (progn
          (pushnew module (?used-syntax-modules (dynamic *current-module*))
                   :key #'?identifier)
          (copy-list (?syntax-exports module)))
      ())))

;;;-----------------------------------------------------------------------------
;;; loading module files
;;;-----------------------------------------------------------------------------

(export load-module load-if-module)

(defun find-or-load-module (module-id)
  (or (find-module module-id) (load-module module-id)))

(defun load-module (module-id)
  (load-apply-module module-id #'trans-module))

(defun load-if-module (module-id)
  (load-def-file module-id #'trans-if-module))

;;;-----------------------------------------------------------------------------
;;; installing local syntax definitions
;;;-----------------------------------------------------------------------------

(defun trans-old-style-local-syntax-definitions (defmacro-forms)
  (dynamic-let ((mac-env (?syntax-env (dynamic *current-module*)))
                (lex-env (?lex-env (dynamic *current-module*))))
               (setf (?syntax-env (dynamic *current-module*))
                     (nconc (mapcar #'transmod defmacro-forms)
                            (?syntax-env (dynamic *current-module*))))))

#module-end
