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
;;;  Title: generation of interface files (only for Basic System Compilation)
;;;  Description:
;;    The generation of .def-files does only work for basic System
;;    Compilation. It doesn't work now for Module Compilation!
;;
;;    When compiling a Basic System a .def-file is generated containing exactly
;;    one module definition. This module contains interface descriptions of
;;    exported objects, macro definitions and additional compiler
;;    informations. The naming of explicitely and invisible exported objects is
;;    done previously during export marking (directly after loading the module
;;    files).
;;;  Notes:
;;    There are some hacks in the file which should be replaced as soon as
;;    possible.  They are marked with *hack*.
;;;  Problems:
;;    Name conflicts (i.e. different objects may have theoretical the same
;;                         Lisp-identifiers) may occur in the following cases:
;;
;;    * Invisible exported objects have no explicit export name. Therefore the
;;    definition identifier is taken and this may result in a name conflict.
;;
;;    * Local variables in macros and interpreted functions may shadow global
;;    bindings The reason for this is that the explicitely and invisible
;;    exported objects of the member modules of the basic system are put
;;    together in only one module.  Because the explicitely exported objects are
;;    renamed according to the given export interface (this was checked already
;;    for name conflicts!) they are not the problem. A problem are the invisible
;;    exported objects and local variables in interpreted forms because they are
;;    teared out of the original binding context.  They should be renamed to
;;    unique identifiers.
;;;  Authors: Ingo Mohr
;;;-----------------------------------------------------------------------------

#module generate-def-file
(import (eulisp0
         el2lzs
         (only ($tail-module)
               el2lzs-main)
         (only (*literal-expanders*
                ?literal-class
                ?expander)
               el2lzs-basic)
         annotate
         accessors
         lzs
         lzs-mop
         tail-module
         ti-write
         predicates
         export
         code-identifier
         representation
         (only (result-type
                function-signature)
               c-typing) ; the use of these functions guarantees equivalence in
         ;; types in .c- and .def-files
         (only (max-used-type-descriptor
                max-used-card-descriptor)
               apply-funs)
         (only (format
                dolist
                list*
                svref
                mapcar
                mapc
                second
                *package*
                symbol-package
                append
                assoc
                minusp
                butlast
                last)
               common-lisp))
 syntax (eulisp0
         dynamic
         (only (with-open-file
                push)
               common-lisp))
 export (generate-module-def))

;;;-----------------------------------------------------------------------------
;;; main functions
;;;-----------------------------------------------------------------------------
(defun generate-module-def (main-module modules)
  (let ((id (?identifier main-module))
        (*package* (symbol-package ^t)))
    ;; a *hack* is all stuff for <object> and <class>
    (write-def "~%(defmodule ~a~
                    ~% (import (%tail~
                    ~%          ti-sys-signatures~
                    ~%          (rename ((%class <class>)) (only (%class) %tail))
                    ~%          ~A)~
                    ~%  syntax (%tail)~
                    ~%  export (<object> <class>)~
                    ~%  expose ~1@*(~A))"
               id
               (generate-%tail-exports))
    (generate-compiler-info-declarations)
    (generate-interface modules)
    (generate-initfun-def main-module)
    (gen-literal-expansion-definitions)
    (generate-*hack*)
    (write-def "~%); end of interface definition for ~a"
               id)
    ))



(defun write-def (format . args)
  (apply #'format (dynamic code-output) format args))

(defun lattice-type-hack ()
  ;;*hack*
  ;;the following definitions are written directly into the .def-file because it
  ;;is forgotten by the appropriate interface functions of the type inference
  ;;system
  (write-def
   "
(%define-lattice-type mono-list (<cons>) (bottom) t)
(%define-lattice-type singleton (%object) (bottom))
(%define-lattice-type fpi-one
  (<int> singleton)
  (bottom) () 1)

(%define-lattice-type fpi-zero
  (<int> singleton)
  (bottom) () 0)
"))

;;;-----------------------------------------------------------------------------
;;; interface identifiers
;;;-----------------------------------------------------------------------------
(defun if-identifier (object)
  ;; returns the interface identifier of an object: the identifier in
  ;; the module which is the compilation unit
  (export-identifier object)
  )

;;;-----------------------------------------------------------------------------
;;; retrieving defined types of program objects
;;;-----------------------------------------------------------------------------
(defun defined-type (typed-object)
  (or (?type typed-object) %object))

(defun arg-n-type (range-and-domain n)
  (svref range-and-domain n))

;;;-----------------------------------------------------------------------------
;;; the module init function
;;;-----------------------------------------------------------------------------
(defun generate-initfun-def (module)
  (gen-interface (?toplevel-forms module))
  (write-def "~2%(%annotate-function ~S init-function t)"
             (if-identifier (?toplevel-forms module))))

;;;-----------------------------------------------------------------------------
;;; main working functions
;;;-----------------------------------------------------------------------------
(defun generate-interface (modules)
  (mapc (lambda (module)
          (mapc #'gen-interface-if-exported (?class-def-list module)))
        modules)
  (lattice-type-hack) ;*hack*
  (def-write-remaining-strategic-lattice-types (dynamic code-output))
  (mapc (lambda (module)
          (mapc #'gen-interface-if-exported (?fun-list module))
          (mapc #'gen-interface-if-exported (?named-const-list module))
          (mapc #'gen-interface-if-exported (?var-list module))
          (mapc #'gen-interface-if-exported (?sym-list module))
          )
        modules))

(defun gen-interface-if-exported (object)
  (cond ((hard-wired-p object)
         ;; the exports are done in the interface section
         ;; the type schemes of hard-wired special-sys-funs are loaded
         ;; explicitely by importing ti-sys-signatures.def
         )
        ((exported-for-lisp-p object)
         (write-def "~2%~:[(export ~S~:*)~%~;~]"
                    (invisible-exported-p object)
                    (if-identifier object))
         (gen-interface object))
        ))

(defgeneric gen-interface (object))

(defmethod gen-interface (object)
  ;; generate only interfaces for specific objects
  ())

;;;-----------------------------------------------------------------------------
;;; providing special compiler infos for following compilations
;;;-----------------------------------------------------------------------------
(defun generate-compiler-info-declarations ()
  (write-def "~2%(%provide-compiler-info~
              ~%   max-used-type-descriptor ~A~
              ~%   max-used-card-descriptor ~A)"
             max-used-type-descriptor
             max-used-card-descriptor))

;;;-----------------------------------------------------------------------------
;;; exporting objects of %tail
;;;-----------------------------------------------------------------------------
(defun generate-%tail-exports ()
  (gen-%tail-exports (append (?fun-list $tail-module)
                             (?class-def-list $tail-module)
                             (?var-list $tail-module)
                             (?named-const-list $tail-module))
                     () ()))

(defun gen-%tail-exports (objects rename-part only-part)
  (cond ((null? objects)
         `(rename ,rename-part (only ,only-part %tail)))
        ((and (hard-wired-p (car objects))
              (exported-for-lisp-p (car objects))
              (null? (invisible-exported-p (car objects))))
         (gen-%tail-exports (cdr objects)
                            (if (eq (?identifier (car objects))
                                    (if-identifier (car objects)))
                                rename-part
                              (cons (list (?identifier (car objects))
                                          (if-identifier (car objects)))
                                    rename-part))
                            (cons (?identifier (car objects))
                                  only-part)))
        (t (gen-%tail-exports (cdr objects) rename-part only-part))))

(defgeneric hard-wired-p (object))
(defmethod hard-wired-p (object) ())
(defmethod hard-wired-p ((object <basic-class-def>)) t)
(defmethod hard-wired-p ((object <special-sys-fun>)) t)
(defmethod hard-wired-p ((object <class-def>))
  (or (eq object %object)
      (eq object %class)
      (eq object %abstract-class)
      (eq object %tail-class)))

;;;-----------------------------------------------------------------------------
;;; annotations
;;;-----------------------------------------------------------------------------
(defun gen-annotate-function (fun)
  (let ((annotations (get-function-annotations fun)))
    (when annotations
          (write-def "~2%(%annotate-function ~S ~:{~%   ~A ~:S~})"
                     (if-identifier fun)
                     annotations))))

(defun get-function-annotations (fun)
  (let ((annotations (get-saved-annotations fun)))
    (when (?reduce fun)
          (push (get-reduce-annotation (?reduce fun))
                annotations))
    (when (?fread-gloc fun)
          (push (list ^read-location ^t)
                annotations))
    (when (?fwrite-gloc fun)
          (push (list ^write-location ^t)
                annotations))
    (when (?setter fun)
          (push (list ^setter (if-identifier (?setter fun)))
                annotations))
    annotations))

(defun get-reduce-annotation (reduce-value)
  (setf (car reduce-value)
        (if-identifier (car reduce-value)))
  (when (cons? (second reduce-value))
        (setf (car (second reduce-value))
              (if-identifier (car (second reduce-value)))))
  (list ^reduce reduce-value))

(defun gen-annotate-class (class)
  (let ((annotations (get-saved-annotations class)))
    (when annotations
          (write-def "~2%(%annotate-class ~S ~:{~%   ~A ~:S~})"
                     (if-identifier class)
                     annotations))))

(defun gen-annotate-binding (binding)
  ;; binding is a global-static or a named-const
  (let ((annotations (get-saved-annotations binding)))
    (when annotations
          (write-def "~2%(%annotate-binding ~S ~:{~%   ~A ~:S~})"
                     (if-identifier binding)
                     annotations))))

;;;-----------------------------------------------------------------------------
;;; constants and variables
;;;-----------------------------------------------------------------------------
(defmethod gen-interface ((var <global-static>))
  (write-def "(%declare-external-variable ~S ~S~
              ~% external-name |~A|)"
             (if-identifier var)
             (if-identifier (defined-type var))
             (?code-identifier var)
             )
  (gen-annotate-binding var))

(defun gen-const-value (value)
  (let ((lit (gen-literal value)))
    (if (or (sym-p value)
            (and (structured-literal-p value)
                 (cons? (?value value))))
        (list ^quote lit)
      lit)))

(defmethod gen-interface ((const <named-const>))
  (unless (fun-p (?value const))
          (write-def "(%declare-external-constant ~S ~S~
                 ~% external-name |~A|~
                 ~@[~% value ~A~])"
                     (if-identifier const)
                     (if-identifier (defined-type const))
                     (?code-identifier const)
                     (gen-const-value (?value const)))
          (gen-annotate-binding const)))

(defgeneric gen-literal (value))
(defmethod gen-literal (value) ())
(defmethod gen-literal ((value <null>))
  "()")
(defmethod gen-literal ((value <number>))
  value)
(defmethod gen-literal ((value <character>))
  (format () "#\\~A" value))
(defmethod gen-literal ((value <sym>))
  (?identifier value))
(defmethod gen-literal ((value <structured-literal>))
  (gen-structured-literal (?value value)))
(defmethod gen-literal ((value <class-def>))
  (?identifier value))
(defmethod gen-literal ((value <fun>))
  (?identifier value))

(defgeneric gen-structured-literal (obj))

(defmethod gen-structured-literal ((obj <string>))
  (format () "~S" obj))

(defmethod gen-structured-literal ((obj <vector>))
  (map #'gen-literal obj))

(defmethod gen-structured-literal ((obj <pair>))
  (labels ((gen-list (l)
                     (cond ((null? l) ())
                           ((atom? l) (gen-literal l))
                           (t (cons (gen-list (car l))
                                    (gen-list (cdr l)))))))
          (gen-list obj)))

;;;-----------------------------------------------------------------------------
;;; symbols
;;;-----------------------------------------------------------------------------
(defmethod gen-interface ((sym <sym>))
  (write-def "(%declare-external-symbol ~S |~A|)"
             (if-identifier sym)
             (?code-identifier sym)))

;;;-----------------------------------------------------------------------------
;;; simple functions
;;;-----------------------------------------------------------------------------
(defun specialized-parameters (fun)
  ;; it must be checked if the original parameter list contained a rest
  ;; parameter, because this is replaced in preceding compilation steps by a
  ;; required parameter
  (cond ((?rest (?params fun))
         (spec-params (?var-list (?params fun))
                      (?rest (?params fun))
                      (function-signature fun)
                      1))
        ((and (?arg-num fun) (minusp (?arg-num fun))) ; this function originally had a rest parameter?
         (spec-params (butlast (?var-list (?params fun)))
                      (car (last (?var-list (?params fun)))) ; the parameter
                      ;; which was
                      ;; originally the
                      ;; rest parameter
                      (function-signature fun)
                      1))
        (t ; without rest parameter
         (spec-params (?var-list (?params fun))
                      ()
                      (function-signature fun)
                      1))))

(defun spec-params (vars rest range-and-domain type-idx)
  (cond (vars
         (cons (list (?identifier (car vars))
                     (if-identifier (arg-n-type range-and-domain type-idx)))
               (spec-params (cdr vars) rest range-and-domain (+ type-idx 1))))
        (rest (?identifier rest))
        (t ())))

(defmethod gen-interface ((fun <simple-fun>))
  (write-def "(%declare-external-function (~S ~S)~
              ~% ~:S~
              ~% external-name |~A|)"
             (if-identifier fun)
             (if-identifier (result-type fun))
             (specialized-parameters fun)
             (?code-identifier fun))
  (ti-def-write (dynamic code-output) fun)
  (gen-annotate-function fun)
  )

(defmethod gen-interface ((fun <special-sys-fun>))
  ;; they are hard-wired into the compiler
  ())

(defmethod gen-interface ((fun <discriminating-fun>))
  ;; Don't generate an interface for discriminating funs because they are only
  ;; partially defined in a basic system and may be extended by new methods in
  ;; modules using the basic system. The interface is generated for the generic
  ;; function only.
  ())

;;;-----------------------------------------------------------------------------
;;; generic functions
;;;-----------------------------------------------------------------------------
(defmethod gen-interface ((gf <generic-fun>))
  (write-def "(%declare-external-generic (~S ~S)~
              ~% ~:S~
              ~% external-name |~A|~
              ~% methods (~{~S~
              ~^~%          ~}))"
             (if-identifier gf)
             (if-identifier (result-type gf))
             (specialized-parameters gf)
             (?code-identifier gf)
             (mapcar (lambda (method)
                       (list* (if-identifier (~method-function method))
                              (if-identifier %object) ; result class
                              (mapcar #'if-identifier
                                      (?domain method))))
                     (~generic-function-methods gf))
             )
  (gen-annotate-function gf)
  )

;;;-----------------------------------------------------------------------------
;;; classes
;;;-----------------------------------------------------------------------------
(defun make-slot-specification (slot-desc)
  (list (?identifier slot-desc)
        ^type (if-identifier (?type slot-desc))
        ^c-identifier (?code-identifier slot-desc)))

(defmethod gen-interface ((class <class-def>))
  (let ((super-strategic-lattice-types
         (def-write-super-strategic-lattice-types
           (dynamic code-output)
           class)))
    (write-def "(%declare-external-class (~S ~S) ~:S~
                ~% (~{~:S~^~%  ~})~
                ~% representation ~A~
                ~% object-identifier |~A|~
                ~% type-identifier |~A|~
                ~@[~% direct-super-lattice-types ~S~]~
                ~@[~% converter ~S~])"
               (if-identifier class)
               (if-identifier (?class class))
               (mapcar #'if-identifier (?supers class))
               (mapcar #'make-slot-specification (?effective-slots class))
               (get-representation-id (?representation class))
               (?code-identifier class)
               (?type-identifier class)
               super-strategic-lattice-types
               (and (?converter class) (if-identifier (?converter class))))
    (gen-annotate-class class)))

(defgeneric get-representation-id (representation))
(defmethod get-representation-id ((r <%pointer-to-struct>)) ^pointer-to-struct)
(defmethod get-representation-id ((r <%pointer-to-vector>)) ^pointer-to-vector)
(defmethod get-representation-id ((r <%pointer-to-void>)) ^pointer-to-void)
(defmethod get-representation-id ((r <%direct>)) ^direct)

;;;-----------------------------------------------------------------------------
;;; %define-literal-expansion
;;;-----------------------------------------------------------------------------
(defun gen-literal-expansion-definitions ()
  ;; it is required, that no strings appear in the expansion forms, also no
  ;; symbols with escaped characters should appear there
  (mapcar (lambda (lexp)
            (write-def "~2%(%define-literal-expansion ~A~
                        ~%   ~A)"
                       (?literal-class lexp)
                       (make-expansion-form (?expander lexp))))
          *literal-expanders*))

;; To avoid: Warning: Undefined variable ES::*LOCAL-VAR-NAMES*
#+ :cmu (defvar *local-var-names* ())

(defun make-expansion-form (expander-fun)
  (dynamic-let ((*local-var-names*
                 (get-local-var-name-table (?var-list (?params expander-fun)))))
               (lzs2list (?body expander-fun))))

;;;-----------------------------------------------------------------------------
;;; converting LZS-representation of functions/forms into list-representation
;;;-----------------------------------------------------------------------------
(defgeneric lzs2list (form))

(defmethod lzs2list ((obj <object>))
  ;; take simple lzs-literals as they are
  obj)

(defmethod lzs2list ((obj <null>))
  ;; usage of a string is possible, because the expression is written out by ~A
  ;; this avoids package markers in the generated sources
  "()")

(defmethod lzs2list ((obj <lzs-object>))
  (if (global-p obj)
      (if-identifier obj)
    ()))

(defmethod lzs2list ((obj <sym>))
  (list ^quote (if-identifier obj)))

(defmethod lzs2list ((obj <app>))
  (cons (if-identifier (?function obj))
        (mapcar #'lzs2list (?arg-list obj))))

(defmethod lzs2list ((obj <named-const>))
  (if-identifier obj))

(defmethod lzs2list ((obj <var-ref>))
  (get-var-name (?var obj)))

(defmethod lzs2list ((obj <setq-form>))
  (list ^setq
        (lzs2list (?location obj))
        (lzs2list (?form obj))))

(defmethod lzs2list ((obj <progn-form>))
  (cons ^progn
        (mapcar #'lzs2list (?form-list obj))))

(defmethod lzs2list ((obj <if-form>))
  (list ^if
        (lzs2list (?pred obj))
        (lzs2list (?then obj))
        (lzs2list (?else obj))))

(defmethod lzs2list ((obj <switch-form>))
  )

(defmethod lzs2list ((obj <labeled-form>))
  )

(defun make-unique-var-names (let*-vars)
  ;; it makes ONLY sure that no improper shadowing occures during later
  ;; evaluation of init-forms
  ;; this function doesn't check for name conflicts with global bindings!
  (dynamic-let ((*unique-index* (car (dynamic *local-var-names*))))
               (let ((name-alist
                      (mapcar
                       (lambda (local-static)
                         (dynamic-setq *unique-index*
                                       (+ 1 (dynamic *unique-index*)))
                         (cons local-static
                               (make-eulisp-symbol
                                (format () "~S/~D"
                                        (?identifier local-static)
                                        (dynamic *unique-index*)))))
                       let*-vars)))
                 (cons (dynamic *unique-index*)
                       (append name-alist
                               (cdr (dynamic *local-var-names*)))))))

(defmethod lzs2list ((obj <let*-form>))
  ;; NOTE: it works only for local-static's
  ;; an extension for dynamic bindings should be made in the future
  (dynamic-let ((*local-var-names* (make-unique-var-names (?var-list obj))))
               (list ^let*
                     (mapcar (lambda (var init)
                               (list (get-var-name var)
                                     (lzs2list init)))
                             (?var-list obj)
                             (?init-list obj))
                     (lzs2list (?body obj)))))

(defmethod lzs2list ((obj <labels-form>))
  )

(defmethod lzs2list ((obj <let/cc-form>))
  (list ^let/cc
        (lzs2list (?cont obj))
        (lzs2list (?body obj))))

(defmethod lzs2list ((obj <tagbody-form>))
  )

(defmethod lzs2list ((obj <tagged-form>))
  )

(defmethod lzs2list ((obj <mv-lambda>))
  )

(defmethod lzs2list ((obj <get-slot-value>))
  )

(defmethod lzs2list ((obj <set-slot-value>))
  )

;;;-----------------------------------------------------------------------------
;;; Handling local variable names
;;;-----------------------------------------------------------------------------
(defvar *local-var-names* (list 0)) ; a list (unique-index (var1 . name1)...)

(defgeneric get-var-name (var))

(defmethod get-var-name ((var <local-static>))
  (cdr (assoc var (cdr (dynamic *local-var-names*)))))

(defmethod get-var-name ((var <global-static>))
  (if-identifier var))

(defmethod get-var-name ((var <dynamic>))
  (list ^dynamic (if-identifier var)))

(defun get-local-var-name-table (vars)
  (cons 0 (mapcar (lambda (var)
                    (cons var (?identifier var)))
                  vars)))

;;;-----------------------------------------------------------------------------
;;; *hack*
;;;-----------------------------------------------------------------------------
;;; The following hack is necessary because
;;; 1. the macro definitions are not yet moved to the def-file
;;; 2. variables imported from c are not moved to the def-file because
;;; include-statements for the c-preprocessor can#t be generated, due to the
;;; explicit declaration of c-import in the module interface without connection
;;; to the variable declarations

(defun generate-*hack* ()
  (write-def
   "
;;; -------------------
;;; from module syntax-0
;;; -------------------

;; (defmacro cond clauses
;;   (if (null? clauses) ()
;;     (if (null? (cdr (car clauses))) `(or ,(car (car clauses))
;;                                         (cond ,@(cdr clauses)))
;;       (if (eq (car (car clauses)) 't) `(progn ,@(cdr (car clauses)))
;;         `(if ,(car (car clauses))
;;              (progn ,@(cdr (car clauses)))
;;            (cond ,@(cdr clauses)))
;;         ))))

(defun mk-progn (forms)
  (if (null? forms) ()
    (if (null? (cdr forms)) (car forms)
      (cons 'progn forms))))

(defun cond1 (clauses)
  (if (null? clauses) ()
    (if (null? (cdr (car clauses)))
        (list 'or (car (car clauses))
              (cond1 (cdr clauses)))
      (if (eq (car (car clauses)) 't)
          (mk-progn (cdr (car clauses)))
        (list 'if (car (car clauses))
              (mk-progn (cdr (car clauses)))
              (cond1 (cdr clauses)))
        ))))

(defun cond clauses
  (cond1 clauses))
(export-syntax cond)

;; (defmacro and forms
;;   (if (null? forms) 't
;;     (if (null? (cdr forms)) (car forms)
;;       `(if ,(car forms)
;;            (and ,@(cdr forms))
;;          ())
;;       )))

(defun and1 (forms)
  (if (null? forms) 't
    (if (null? (cdr forms)) (car forms)
      (list 'if (car forms)
            (and1 (cdr forms))
            ())
      )))

(defun and clauses
  (and1 clauses))
(export-syntax and)

;; (defmacro when (cond . forms)
;;   (if (null? forms) ()
;;     `(if ,cond (progn ,@forms) () )))

(defun when (cond . forms)
  (if (null? forms) ()
    (list 'if cond (mk-progn forms) () )))
(export-syntax when)

;; (defmacro unless (cond . forms)
;;   (if (null? forms) ()
;;     `(if ,cond () (progn ,@forms) )))

(defun unless (cond . forms)
  (if (null? forms) ()
    (list 'if cond () (mk-progn forms) )))
(export-syntax unless)

;; (defmacro block (identifier . forms)
;;   (if (null? forms) ()
;;     `(let/cc ,identifier ,@forms)))

(defun block (identifier . forms)
  (if (null? forms) ()
    (cons 'let/cc
          (cons identifier forms))))
(export-syntax block)

;; (defmacro return-from (identifier . form)
;;   `(,identifier ,(if (null? form) () (car form))))

(defun return-from (identifier . form)
  (list identifier (if (null? form) () (car form))))
(export-syntax return-from)

;;; ----------------------
;;; from module condition
;;; ----------------------

;; (defmacro defcondition (condition-class-name super-class-name
;;                                              slot-descriptions . init-options)
;;   `(progn
;;      (if (%subclassp (if ,super-class-name ,super-class-name <condition>) <condition>)
;;          ()
;;        (error defcondition-error-string <condition>))
;;      (%define-standard-class (,condition-class-name <class>)
;;        ,(if super-class-name super-class-name <condition>)
;;        ,slot-descriptions
;;        representation pointer-to-struct
;;        allocation multiple-type-card
;;        ,@init-options)))

(defun defcondition (condition-class-name super-class-name
                                          slot-descriptions . init-options)
  (list 'progn
        (list 'if
              (list %subclassp
                    (list 'if super-class-name super-class-name <condition>)
                    <condition>)
              ()
              (list 'error
                    \"Superclass in defcondition is not a subclass of <condition>\"
                    <condition>))
        (cons 'defclass (cons condition-class-name
                               (cons (if super-class-name super-class-name <condition>)
                                     (cons slot-descriptions
                                           init-options))))))
(export-syntax defcondition)

;; (defmacro with-handler (handler-fun . protected-forms)
;;   `(dynamic-let ((shadowed-default-handler (dynamic dynamic-default-signal-handler))
;;                  (dynamic-default-signal-handler
;;                   (lambda(condition continuation)
;;                     (,handler-fun condition continuation)
;;                     (let ((active-handler (dynamic shadowed-default-handler )))
;;                       (active-handler
;;                        condition continuation )))))
;;                 ,@protected-forms))

(defun with-handler (handler-fun . protected-forms)
  (cons 'dynamic-let
        (cons (list '(shadowed-default-handler (dynamic dynamic-default-signal-handler))
                    (list 'dynamic-default-signal-handler
                          (list 'lambda '(condition continuation)
                                (list handler-fun 'condition 'continuation)
                                '(let ((active-handler (dynamic shadowed-default-handler )))
                                   (active-handler
                                    condition continuation )))))
              protected-forms)))
(export-syntax with-handler)

;;; ----------------------
;;; from module mm-interface
;;; ----------------------

(%declare-external-variable mtss %signed-byte-integer
  language c
  external-name |MTSS|)

(%declare-external-variable stms %signed-byte-integer
  language c
  external-name |STMS|)
(%declare-external-variable stss %signed-byte-integer
  language c
  external-name |STSS|)

(%annotate-binding mtss is-special-binding mtss)
(%annotate-binding stms is-special-binding stms)
(%annotate-binding stss is-special-binding stss)

;;; --------------------
;;; from module object-0-i
;;; --------------------

;; (defmacro defclass (class-name superclass
;;                                 slot-descriptions . class-options)
;;   `(%define-standard-class
;;      (,class-name <structure-class>)
;;      ,superclass
;;      ,slot-descriptions
;;      representation pointer-to-struct
;;      allocation multiple-type-card
;;      ,@class-options))

(defun defclass (class-name superclass
                             slot-descriptions . class-options)
  (cons '%define-standard-class
        (cons (list class-name '<structure-class>)
              (cons superclass
                    (cons slot-descriptions
                          (cons 'representation (cons 'pointer-to-struct
                                                      (cons 'allocation (cons 'multiple-type-card
                                                                              class-options)))))))))
(export-syntax defclass)

"

))

#module-end
