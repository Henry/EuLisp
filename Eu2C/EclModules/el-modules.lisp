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

;;;-----------------------------------------------------------------------------
;;; setting some special features
;;;-----------------------------------------------------------------------------

#+:cmu
(eval-when (compile eval load)
  (setq *features*
        (union '(:eulisp)
               *features*)))

#-:eulisp ;the features are not yet set for a special lisp system, use the default
(eval-when (:compile-toplevel :execute :load-toplevel)
  (setq *features*
        (union '(:eulisp :eval-when-cltl2)
               *features*)))

;;;-----------------------------------------------------------------------------
;;; defining the package
;;;-----------------------------------------------------------------------------

(defpackage "EL-MODULES"
  (:shadow "EXPORT" "MAKE-SYMBOL" #-:eval-when-cltl2 "EVAL-WHEN")
  (:import-from common-lisp-user cl-user::*ecl-module-search-path*
                #+:eval-when-cltl2 eval-when )
  (:use #:cl) ;;***HGW
  (:export
   eval-when
   $eulisp-symbol-package make-eulisp-symbol make-symbol make-identifier
   EXPORT EXPORT-SYNTAX EXPOSE
   make-eulisp-class
   read-eulisp
   *ecl-module-search-path* $eulisp-readtable))

;; additionally defines cl-user::require-eulisp-module to be used in load-Files

(in-package "EL-MODULES")

#-:eval-when-cltl2
(defmacro eval-when (situations . forms)
  `(progn
     (defvar eval-when-state nil)

     (cl:eval-when (compile load eval) (setq eval-when-state nil))

     (cl:eval-when (eval)
                   (setq eval-when-state :execute))

     (cl:eval-when (load)
                   (unless (eq eval-when-state :execute)
                     (setq eval-when-state :load-toplevel)))

     (cl:eval-when (compile)
                   (unless (eq eval-when-state :execute)
                     (setq eval-when-state :compile-toplevel)))

     (cl:eval-when (compile load eval)
                   (when (member eval-when-state ',situations)
                     ,@forms) )))



;;;-----------------------------------------------------------------------------
;;; the package for symbols and identifiers of dynamic variables
;;;-----------------------------------------------------------------------------

(defpackage "ES"
  (:nicknames "EULISP-SYMBOL")
  (:use)
  (:import-from "COMMON-LISP" "QUOTE") ; to make sure that the right QUOTE is
  ;; used when reading 'x
  )

(defconstant $eulisp-symbol-package (find-package "EULISP-SYMBOL"))

;;;-----------------------------------------------------------------------------
;;; Module Definition (by IM)
;;;-----------------------------------------------------------------------------
;;;
;;;An EuLisp-module is implemented as a CommonLisp-package. So it is possible to
;;;provide the encapsulation of lexical bindings using the package encapsulation
;;;mechanism for symbols. However, because symbols and dynamic bindings are
;;;located in EL in one global namespace they must be handled in a special way.
;;;Therefore all identifiers of dynamic variables and all symbol constants are
;;;placed in a special package (bound to the constant $EULISP-SYMBOL-PACKAGE).
;;;
;;;Because identifiers of dynamic variables appear only in special contexts like
;;;(dynamic ...) their handling is easy. It is only necessary to define the
;;;appropriate special forms as macros, which place the identifiers in the symbol
;;;package.
;;;
;;;The handling of symbols is not so easy. One solution would be the redefinition
;;;of all functions working with symbols, especially eq, eql, equal, get. But in
;;;this set of functions are also substitute, member e.t.c. And if one uses a
;;;CL-function he must take into account if symbols play a role or not.
;;;Altogether, this solution would be a bad one. Another possibility is to mark
;;;every symbol constant in a special way. Using read-macros it is possible to
;;;put every symbol into the symbol package. I have choosed the sign ^, which
;;;replaces in some sense the single quote '. The ^ works like ' with the only
;;;difference, that the following expression is read in using the EuLisp symbol
;;;package.
;;;
;;;Another problem is the syntactic form of DEFMODULE. It cannot be defined as a
;;;macro with arguments as specified in the EuLisp-definition. This is because
;;;the package must be set using IN-PACKAGE before the defining forms are read
;;;in. But the DEFMODULE form were be read in the whole including the
;;;definitions. So we must split the module definition into different parts. I
;;;have used read macros to read the different parts of the module header. So a
;;;module must be written as follows:
;;;
;;;#module-name module-identifier
;;;#module-import
;;; ( import-spec... )
;;;#module-syntax-import
;;; ( import-spec... ) ; syntax import
;;;#module-syntax-definitions
;;; macro-definition...
;;;#module-header-end
;;; top-level-form...

(defclass eulisp-module ()
  ((package :reader module-package
            :initarg :package)
   (path    :accessor module-path
            :initform nil)
   (dependent-modules :accessor dependent-modules
                      :initform ())
   (used-modules :accessor used-modules
                 :initform ())
   (invalidated? :accessor module-invalidated?
                 :initform ())
   (exports :accessor module-exports
            :initarg :exports
            :initform ())
   (syntax-exports :accessor module-syntax-exports
                   :initform ())
   (imports :accessor module-imports
            :initform ())
   (syntax-imports :accessor module-syntax-imports
                   :initform ())))

(defmethod initialize :after ((m eulisp-module) &rest options)
  (declare (ignore options))
  (setf (module-exports m) (remove-duplicates (module-exports m))))

(defvar *eulisp-modules* ())

(defmethod find-eulisp-module ((package package))
  (find package *eulisp-modules* :key #'module-package))

(defmethod find-eulisp-module ((name symbol))
  (find (find-package name) *eulisp-modules* :key #'module-package))

(defmethod find-eulisp-module ((name string))
  (find (find-package name) *eulisp-modules* :key #'module-package))

(defmethod dependent-modules ((name symbol))
  (let ((m (find-eulisp-module name)))
    (if m
        (dependent-modules m)
      nil)))

(defun current-module ()
  (find-eulisp-module *package*))

(defun add-module (name state)
  ;; If a module with package NAME doesn't exists then create a new module
  ;; object and a package with the same name If a module already exists then a
  ;; reloading is in action. In this case all symbols of the module-package must
  ;; be uninterned and the loader of the module must recreate all dependent
  ;; modules
  (let ((module (find-eulisp-module name)))
    (if (null module)
        (if (and(eq state :compile)*compile-file-truename*)
            (progn
              (load *compile-file-truename*)
              (setq module (find-eulisp-module name)))
          (progn (setq module (make-instance
                               'eulisp-module
                               :package (make-package (string name) :use nil)))
                 (push module *eulisp-modules*)))
      (unless (eq state :compile)
        (invalidate-module module)))
    (setf (module-path module) *load-pathname*)
    module))

(defun invalidate-module (module)
  (unless (module-invalidated? module)
    (setf (module-invalidated? module) t)
    (setf (module-exports module) nil)
    (setf (module-syntax-exports module) nil)
    (setf (module-imports module) nil)
    (setf (module-syntax-imports module) nil)
    (mapc #'invalidate-module
          (dependent-modules module))
    (setf (used-modules module) nil)
    (setf (dependent-modules module) nil)
    )
  (let ((package (module-package module)))
    (do-symbols (sym package nil)
      (unintern sym package))))

(defun add-package-as-module (package)
  (let ((module (make-instance 'eulisp-module
                               :package package
                               :exports (find-all-external-symbols package))))
    (setf (module-syntax-exports module) (module-exports module))
    (push module *eulisp-modules*)
    module))

(defun recreate-module (module)
  (when (module-invalidated? module)
    (format t "~%;recreating module ~A" (module-name module))
    (or (load (module-path module) :if-does-not-exist nil :verbose t)
        (load-eulisp-module (module-name module))))
  module)

(defun find-all-external-symbols (package)
  (let ((syms ()))
    (do-external-symbols (sym package syms)
                         (push sym syms))))

(defun get-eulisp-module (name)
  (let ((module (find-eulisp-module name))
        (package (find-package name)))
    (cond ((null module)      ; unknown module?
           (if package        ; does a package exists with name NAME?
               (add-package-as-module package) ; yes: create module for cl-package
             (load-eulisp-module name)))     ; no:  load el-module
          ((module-invalidated? module)
           (recreate-module module))
          (t module))))

(defun get-module-exports (module-name)
  (let ((module (get-eulisp-module module-name)))
    (when module
      (pushnew (current-module) (dependent-modules module))
      (pushnew module (used-modules (current-module)))
      (module-exports module))))

(defun get-module-syntax-exports (module-name)
  (let ((module (get-eulisp-module module-name)))
    (when module
      (pushnew (current-module) (dependent-modules module))
      (pushnew module (used-modules (current-module)))
      (module-syntax-exports module))))

(defun add-exports (symbols)
  (let ((m (current-module)))
    (setf (module-exports m)
          (union symbols (module-exports m)))))

(defun add-syntax-exports (symbols)
  (let ((m (current-module)))
    (setf (module-syntax-exports m)
          (union symbols (module-syntax-exports m)))))

(defmacro in-eulisp-symbol-package (&rest forms)
  `(let ((*package* $eulisp-symbol-package))
     ,@forms))

(defparameter module-header-end-marker (gensym)) ;;***HGW

(defun read-module-definition-1 (stream subchar arg) ; this extra function
  (read-module-definition stream subchar arg))        ; allows tracing of
;; read-module-definition
(defun read-module-definition (stream subchar arg)
  (declare (ignore arg))
  (unread-char subchar stream)            ; put #\m back into the stream
  (case (in-eulisp-symbol-package *package* (read stream))

    ;;OLD STYLE HEADER: module-syntax-definitions
    (ES::module-name
     `(progn
        ,(read-module-name-old stream)
        ,(read-module-import stream)
        ,(read-module-syntax-import stream)))
    ;;OLD STYLE HEADER: module-syntax-definitions
    (ES::module-syntax-definitions (read-module-syntax-definitions stream))
    ;;OLD STYLE HEADER: module-header-end
    (ES::module-header-end module-header-end-marker)

    (ES::module
     `(progn
        ,(read-module-name stream)
        ,@(read-module-interface stream)))
    (ES::module-end
     `(eval-when (:load-toplevel :execute)
        (format t "~%;module ~A successfully loaded"
                (module-name (current-module)))
        (mapc #'recreate-module
              (reverse *eulisp-modules*))
        nil))
    (t (read stream))))

(set-dispatch-macro-character #\# #\m #'read-module-definition-1)

;;OLD STYLE HEADER
(defun read-module-name-old (stream)
  (let ((module-name (in-eulisp-symbol-package (read stream))))
    `(progn
       (warn "module ~A has old style module header" ',module-name)
       (eval-when(:compile-toplevel)
         (add-module ',module-name :compile))
       (eval-when (:load-toplevel :execute)
         (add-module ',module-name :load))
       (setf (module-invalidated? (find-eulisp-module ',module-name)) nil)
       (in-package ,(string module-name)))))

(defun read-module-name (stream)
  (let ((module-name (in-eulisp-symbol-package (read stream))))
    `(progn
       (eval-when(:compile-toplevel)
         (add-module ',module-name :compile))
       (eval-when (:load-toplevel :execute)
         (add-module ',module-name :load))
       (setf (module-invalidated? (find-eulisp-module ',module-name)) nil)
       (in-package ,(string module-name)))))

(defun read-module-interface (stream)
  (create-module-interface-forms
   (in-eulisp-symbol-package (read stream))))

(defun create-module-interface-forms (option-list)
  (cond ((null option-list) nil)
        ((or (null (consp option-list))
             (null (symbolp (first option-list)))
             (null (listp (second option-list))))
         (error "invalid module interface specification: ~A" option-list))
        (t (cons
            (create-module-interface-form (first option-list)
                                          (second option-list))
            (create-module-interface-forms (cddr option-list))))))

(defun make-local (symbols)
  (mapcar #'(lambda (sym)
              (intern (symbol-name sym) *package*))
          symbols))

(defun create-module-interface-form (directive value)
  (case directive
    (ES::import
     `(eval-when(:compile-toplevel :load-toplevel :execute)
        (import (imported-symbols ',value))))
    (ES::syntax
     `(eval-when(:compile-toplevel :load-toplevel :execute)
        (import (imported-symbols ',value
                                  t))))
    (ES::export
     `(eval-when(:compile-toplevel :load-toplevel :execute)
        (add-exports (make-local ',value))))
    (ES::expose
     `(eval-when(:compile-toplevel :load-toplevel :execute)
        (add-exports (imported-symbols ',value))
        (add-syntax-exports (imported-symbols ',value t))))
    (t
     (error "invalid directive in module interface specification: ~A"
            directive))))

;;OLD STYLE HEADER
(defun read-module-import (stream)
  `(eval-when(:compile-toplevel :load-toplevel :execute)
     (import (imported-symbols ',(in-eulisp-symbol-package (read stream))))))

;;OLD STYLE HEADER
(defun read-module-syntax-import (stream)
  `(eval-when(:compile-toplevel :load-toplevel :execute)
     (import (imported-symbols ',(in-eulisp-symbol-package (read stream))
                               t))))

;;OLD STYLE HEADER
(defun read-module-syntax-definitions (stream)
  (do ((form (read stream) (read stream))
       (forms nil (cons form forms)))
      ((eq form module-header-end-marker)
       `(progn ,@(reverse forms)))
    ))

(defun set-intersection (l1 l2 &key (test #'eq) (key #'identity))
  ;; intersection can't be used because only the elements of the first list
  ;; must returned
  ;; this hint and the definition come from Keith Playford
  (if (or (null l1) (null l2)) '()
    (let ((keep '()))
      (dolist (e l1)
        (when (member e l2 :test test :key key)
          (push e keep)))
      keep)))

(defun imported-symbols (import-specs &optional syntax?)
  (let ((*loading-used-eulisp-module* t))
    (declare (special *loading-used-eulisp-module*))
    (remove-duplicates (imported-symbols-1 import-specs syntax?))))

(defun imported-symbols-1 (import-specs syntax?)
  ;; this function is to avoid multiple calls of remove-duplicates
  (apply #'append
         (mapcar
          #'(lambda (import-spec)
              (if (symbolp import-spec)
                  (if syntax?
                      (get-module-syntax-exports import-spec)
                    (get-module-exports import-spec))
                (case (find-symbol (symbol-name (first import-spec))
                                   $eulisp-symbol-package) ; these symbols are
                  ;; read in
                  ;; the local package !!
                  (ES::except (set-difference ;don't change the argument order
                               ;;because the result must contain
                               ;;the symbols of the first argument
                               (imported-symbols-1 (cddr import-spec)
                                                   syntax?)
                               (second import-spec)
                               :test #'string= :key #'symbol-name))
                  (ES::only (set-intersection
                             (imported-symbols-1 (cddr import-spec)
                                                 syntax?)
                             (second import-spec)
                             :test #'string= :key #'symbol-name))
                  (ES::rename nil)       ; there is nothing to do because it
                  ;; describes only that some external
                  ;; symbols of another package are used
                  ;; in single-colon-syntax
                  )))
          import-specs)))

;;;-----------------------------------------------------------------------------
;;; exports
;;;-----------------------------------------------------------------------------

(defmacro export (&rest symbols)
  `(eval-when(:compile-toplevel :load-toplevel :execute)
     (add-exports ',symbols)))

(defmacro export-syntax (&rest symbols)
  `(eval-when(:compile-toplevel :load-toplevel :execute)
     (add-syntax-exports ',symbols)))

;;OLD STYLE HEADER
(defmacro expose (&rest directives)
  `(eval-when(:compile-toplevel :load-toplevel :execute)
     (add-exports (imported-symbols ',directives))
     (add-syntax-exports (imported-symbols ',directives t))))

;;;-----------------------------------------------------------------------------
;;; loading EuLisp modules
;;;-----------------------------------------------------------------------------

(defvar *loading-used-eulisp-module* nil)
(defvar *eulisp-directory* nil)

(defun load-eulisp-module (name &optional path)
  (let ((file (find-file-to-be-loaded name)))
    (if (and file (load file :if-does-not-exist nil :verbose t))
        (find-eulisp-module name)
      (progn (warn "can't find EuLisp module ~A" name) nil))))

(defun cl-user::require-eulisp-module (name &optional path)
  (unless (find-eulisp-module name)
    (load-eulisp-module name path))
  name)

(unless (boundp 'cl-user::*eulisp-module-file-extension*)
  (defvar cl-user::*eulisp-module-file-extension*
    (make-pathname :type "ecl")))

(unless (boundp 'cl-user::*compiled-eulisp-module-file-extension*)
  (defvar cl-user::*compiled-eulisp-module-file-extension*
    (make-pathname :type "fasl")))

(defun find-file-to-be-loaded (name)
  ;; the default load mechanism with path searching and loading of compiled
  ;; files if they are newer that the source files cannot be used because this
  ;; is realized in very different ways in CL implementations
  (some #'(lambda (path)
            (let* ((name (string-downcase (string name)))
                   (file (probe-file
                          (merge-pathnames
                           (merge-pathnames
                            name
                            cl-user::*eulisp-module-file-extension*)
                           path)))
                   (compiled-file
                    (and file
                         (probe-file
                          (merge-pathnames
                           cl-user::*compiled-eulisp-module-file-extension*
                           (truename file))))))
              (cond ((null file) compiled-file)
                    ((null compiled-file) file)
                    ((<= (file-write-date file)
                         (file-write-date compiled-file))
                     compiled-file)
                    (t file))))
        *ecl-module-search-path*))

;;;-----------------------------------------------------------------------------
;;;  class names
;;;-----------------------------------------------------------------------------

;; make-apply-class makes it possible to access a CL-class under a new name in
;; the same way as EL-classes. This includes both evaluation places and non
;; evaluation positions (like in defclass or defmethod). The APPLY-name is _new_
;; sourrounded by < and >. If the CL-name is not the same as the name used for
;; APPLY, this must be given as _old_. The new name is additionally exported
;; from module/package EULISP.

(defmacro make-eulisp-class (new &optional (old new))
  (setq new (intern (concatenate 'string "<" (symbol-name new) ">")))
  (setq old (find-symbol (symbol-name old) 'common-lisp))
  `(eval-when(;;:compile-toplevel
              :load-toplevel :execute)
     (add-exports '(,new))    ;; is EuLisp-export!!
     (defconstant ,new (find-class ',old))
     #+ :cmu (deftype ,new () ',old)
     (setf (find-class ',new) (find-class ',old))
     ;;(setf (class-name ,new) ',new) ; PP don't work with changed class-names
     ))

;;;-----------------------------------------------------------------------------
;;; Symbols
;;;-----------------------------------------------------------------------------

(defun make-eulisp-symbol (sym)
  (cond ((keywordp sym) sym)
        ((symbolp sym)
         (intern (symbol-name sym) $eulisp-symbol-package))
        ((stringp sym)
         (intern sym $eulisp-symbol-package))))

(defun read-eulisp-symbol-quote (stream char)
  (declare (ignore char))
  (let ((*package* $eulisp-symbol-package))
    `(quote ,(read stream))))

(set-macro-character #\^ #'read-eulisp-symbol-quote)

(defun make-symbol (string)
  (if (eq (elt string 0) #\:)
      (intern (cl:subseq string 1) (find-package 'cl:keyword))
    (intern string $eulisp-symbol-package)))

(defun make-identifier (string)
  (intern string))



;;;-----------------------------------------------------------------------------
;;; Boolean Values
;;;-----------------------------------------------------------------------------

(defun read-boolean (stream subchar arg)
  (declare (ignore stream arg))
  (case subchar
    ((#\f #\F) nil)
    ((#\t #\T) t)))

(set-dispatch-macro-character #\# #\f #'read-boolean)
(set-dispatch-macro-character #\# #\F #'read-boolean)
(set-dispatch-macro-character #\# #\t #'read-boolean)
(set-dispatch-macro-character #\# #\T #'read-boolean)

;;;-----------------------------------------------------------------------------
;;;  reading EuLisp programs
;;;-----------------------------------------------------------------------------
;;
;; (read-eulisp [stream]) reads an eulisp-expression
;;
;; all symbols (including NIL) are in the package ES ($eulisp-symbol-package)
;; '()' is returned as the empty-list of CL, which is the symbol CL:NIL
;; : can't be redefined so it is still the package marker and can't be used in
;; application programs
;;
;; read-eulisp should be used for programs analyzing source-code

;; --- eulisp specific character definitions ---

(defparameter $eulisp-readtable nil)
(defparameter $el-cl-special-char-mapping nil)

(eval-when (:load-toplevel :execute)

  (defun read-eulisp-quasiquote (stream char)
    (declare (ignore char))
    (list 'es::quasiquote (read stream t nil t)))

  (defun read-eulisp-unquote (stream char)
    (declare (ignore char))
    (list (if (eq #\@ (peek-char nil stream t nil t))
              (progn (read-char stream t nil t)
                     'es::unquote-splicing)
            'es::unquote)
          (read stream t nil t)))

  (defun read-tail-literal (stream subchar arg)
    (declare (ignore subchar arg))
    (let ((char (read-char stream t nil t)))
      (if (eq char #\()
          (progn
            (unread-char char stream)
            (cons 'ES::%literal (read stream)))
        (list 'ES::%literal
              (case char
                (#\i 'ES::%signed-word-integer)
                (#\I 'ES::%unsigned-word-integer)
                (#\h 'ES::%signed-half-integer)
                (#\H 'ES::%unsigned-half-integer)
                (#\b 'ES::%signed-byte-integer)
                (#\B 'ES::%unsigned-byte-integer)
                (#\f 'ES::%single-float)
                (#\d 'ES::%double-float)
                (#\e 'ES::%extended-float)
                (#\t 'ES::%tetra-float)
                (t nil))
              (read stream t nil t)))))

  (setq $el-cl-special-char-mapping
        '(
          (#\a . #\0)
          (#\b . #\backspace)
          (#\d . #\rubout)
          (#\f . #\page)
          (#\l . #\linefeed)
          (#\n . #\newline)
          (#\r . #\return)
          (#\t . #\tab)
          (#\v)
          (#\" . #\")
          (#\\ . #\\)
          (#\x)
          ))

  (defun read-eulisp-string (stream char)
    (declare (ignore char))
    (do ((char (read-char stream t nil t)
               (read-char stream t nil t))
         (char-list nil))
        ((eq char #\") (coerce (nreverse char-list) 'string))
      (push char char-list)
      (when (eq char #\\)
        (setf (car char-list)
              (or (cdr (assoc (char-downcase (read-char stream t nil t))
                              $el-cl-special-char-mapping))
                  #\space)))))

  (setq $eulisp-readtable (copy-readtable))

  (set-macro-character #\` #'read-eulisp-quasiquote nil $eulisp-readtable)
  (set-macro-character #\, #'read-eulisp-unquote nil $eulisp-readtable)
  (set-macro-character #\" #'read-eulisp-string nil $eulisp-readtable)
  (set-dispatch-macro-character #\# #\% #'read-tail-literal $eulisp-readtable)
  )

(defun read-eulisp (&optional (stream *standard-input*))
  #+sbcl (declare (sb-ext:disable-package-locks *readtable*)) ;;***HGW
  (let ((*readtable* $eulisp-readtable))
    (declare (special *readtable*))
    (let ((*package* $eulisp-symbol-package))
      (read stream))))

;;;-----------------------------------------------------------------------------
;;; Debugging and Introspection
;;;-----------------------------------------------------------------------------

(defun module-name (m)
  (package-name (or (and m (module-package m))
                    (return-from module-name nil))))

(defun describe-modules (&optional long?)
  (mapc #'(lambda (module)
            (describe-module-main module long?))
        (reverse *eulisp-modules*))
  nil)

(defun describe-module (name &optional long?)
  (let ((module (find-eulisp-module name)))
    (if module
        (describe-module-main module long?)
      (format t "~%A module with name ~A doesn't exists~%" name))))

(defun describe-module-short (module)
  (format t "~2%module ~A~:[~; (invalidated)~]~
                       ~2%~:[no dependent modules~;dependent modules: ~:*~{~(~A~) ~}~]"
          (module-name module)
          (module-invalidated? module)
          (mapcar #'module-name (dependent-modules module))))

(defconstant number-of-printed-symbols 50)

(defun print-symbols (symbols)
  (format t "~V{~A ~}~@[ ...~]"
          number-of-printed-symbols
          symbols
          (> (length symbols) number-of-printed-symbols)))

(defun describe-module-main (module long?)
  (describe-module-short module)
  (when long?
    (format t "~2%~:[module created from a package~;loaded from: ~:*~A~]"
            (module-path module))
    (format t "~2%exported bindings:~%")
    (print-symbols
     (sort (union (mapcar #'symbol-name (module-exports module))
                  (mapcar #'symbol-name (module-syntax-exports module))
                  :test #'string=)
           #'string-lessp))
    ;;    (format t "~2%available bindings:~%")
    ;;    (print-symbols (sort (module-identifiers module)
    ;;                         #'string-lessp))
    ))

(defun module-identifiers (module)
  (let ((ids nil))
    (do-symbols (s (module-package module) ids)
      (pushnew (symbol-name s) ids :test #'string-equal))))

(defvar *found-modules* nil)
(defvar *indent* 0)

(defun module-hierarchy (module-name &optional relation)
  (let ((*found-modules* nil)
        (*indent* 0))
    (print-module-hierarchy
     (compute-module-hierarchy module-name
                               (case relation
                                 (:used #'used-modules)
                                 (:dependent #'dependent-modules)
                                 (t #'used-modules))))))

(defun compute-module-hierarchy (module relation)
  (cond ((null (find-eulisp-module module)) nil)
        ((member module *found-modules*)
         (cons module (if (funcall relation (find-eulisp-module module))
                          '* nil)))
        (t (push module *found-modules*)
           (cons module
                 (mapcar
                  #'(lambda (m)
                      (compute-module-hierarchy (module-name m) relation))
                  (funcall relation (find-eulisp-module module)))))))

(defun print-module-hierarchy (hierarchy)
  (format t "~%~VT~A" *indent* (car hierarchy))
  (if (eq (cdr hierarchy) '*)
      (format t " *")
    (let ((*indent* (+ *indent* 4)))
      (mapc #'print-module-hierarchy (cdr hierarchy))))
  nil)
