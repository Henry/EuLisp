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
;;; Title: Generation of header files
;;;  Notes:
;;    The first version of this module was copied in its main parts from module
;;    'c-code'. Future implementations should integrate both implementation of
;;    declaration generation. The main difference is that
;;    1. not explicitly exported classes may be represented in the header file
;;       as void* and that
;;    2. declarations are generated only for exported objects
;;;  Requires:
;;    (dynamic code-output) must be bound to a stream to which the C-code should
;;    be written
;;;  Authors: Ingo Mohr
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

#module generate-header-file
(import (level-0
         accessors
         lzs
         representation
         binding
         code-identifier
         c-typing
         predicates
         lzs-mop
         (only (expand-literal)
               expand-literal)
         (only (mapc
                mapcar
                list*
                svref)
               common-lisp)
         (only (generate-default-function-prototype)
               c-code))
 syntax (level-0
         c-code-syntax
         code-identifier)
 export (generate-header-file))

(defun generate-header-file (main-module modules)
  (let ((id (?identifier main-module)))
    (write-code "/*header file for EuLisp module '~(~A~)' */" id)
    (generate-eu2c-includes)
    (mapc (lambda (module)
            (mapc #'generate-type-declaration (?class-def-list module)))
          modules)
    (mapc (lambda (module)
            (mapc #'generate-struct-declaration (?class-def-list module)))
          modules)
    (mapc (lambda (module)
            (mapc #'generate-class-object-declaration (?class-def-list module)))
          modules)
    (mapc (lambda (module)
            (mapc #'generate-function-declaration (?fun-list module))
            (mapc #'generate-const-declaration (?named-const-list module))
            (mapc #'generate-var-declaration (?var-list module))
            (mapc #'generate-sym-declaration (?sym-list module))
            )
          modules)
    (mapc #'generate-default-function-prototype modules)
    (write-code "~2%/***    module initialization function    ***/")
    (write-code  "~%/*** must be called one and only one time ***/")
    (write-code "~%extern void ~A();" (main-function-id main-module))
    ;;(write-code "~2%/*** renamed exports ***/")
    ;;(mapc #'generate-rename-macro *rename-exports*)
    (write-code
     "~2%/*end of header file for EuLisp module '~(~A~)'  */~2%" id)))

(defun generate-eu2c-includes ()
  (when (eq *compilation-type* :basic-system)
        (write-code "~%#include \"eu2c-total.h\"~
                 ~%#include <stdio.h>")))

;;;-----------------------------------------------------------------------------
;;; Type, Structure and Object Declarations for Classes
;;;-----------------------------------------------------------------------------
(defun generate-type-declaration (class-def)
  (when (exported? class-def)
        (type-declaration class-def (?representation class-def))))

(defun object-declaration (obj)
  ;; generates declarations for function and class objects and also for symbols;
  ;; because of this restricted set of exported literals only structure classes
  ;; have to be considered
  (let ((literal-instance (expand-literal obj)))
    (write-code "~%extern S_LITERAL(~A, ~A);"
                (type-identifier (?class literal-instance))
                (c-identifier literal-instance))))

(defgeneric type-declaration (class-def representation))

(defmethod type-declaration ((class-def <imported-class>) representation)
  ;; declarations for imported classes should come with included header files
  ())

(defmethod type-declaration (class-def (representation <%pointer-to-struct>))
  (write-code "~%~@<typedef ~;~Istruct ~A ~:_*~:*~A~;;~:>"
              (type-identifier class-def)))

(defmethod type-declaration (class-def (representation <%pointer-to-vector>))
  (write-code "~%~@<typedef ~;~I~A ~:_*~A~;;~:>"
              (type-identifier (~vector-class-element-type class-def))
              (type-identifier class-def)))

(defmethod type-declaration (class-def (representation <%pointer-to-void>))
  (write-code "~%typedef void* ~A;" (type-identifier class-def)))

(defmethod type-declaration (class-def (representation <%direct>))
  (write-code "~%~@<typedef ~;~I~A ~:_~A~;;~:>"
              (type-identifier (get-referred-class representation class-def))
              (type-identifier class-def)))

(defmethod type-declaration ((class-def <basic-class-def>) representation)
  ;; basic types are mapped directly to their C counterparts
  ())

(defgeneric get-referred-class (representation class))

(defmethod get-referred-class (representation class) class)

(defmethod get-referred-class ((representation <%direct>) class)
  (let ((ref-class (~slot-type
                    (car (~class-slots class)))))
    (get-referred-class (?representation ref-class)
                        ref-class)))

(defun generate-struct-declaration (class-def)
  (when (exported? class-def)
        (struct-declaration class-def (?representation class-def))))

(defgeneric struct-declaration (class-def representation))

(defmethod struct-declaration ((class-def <imported-class>) representation)
  ;; declarations for imported classes should come with included header files
  ())

(defmethod struct-declaration (class-def representation)
  ())

(defmethod struct-declaration ((class-def <basic-class-def>) representation)
  ())

(defmethod struct-declaration (class-def (rep <%pointer-to-struct>))
  (with-local-identifiers
   (write-code "~2%~@<struct ~;~:I~A ~:_{~:I~:{~A ~A;~:^ ~_~}~;};~:>"
               (type-identifier class-def)
               (mapcar (lambda (slot)
                         (list (type-identifier (~slot-type slot))
                               (local-c-identifier slot)))
                       (~class-slots class-def)))))

(defgeneric generate-class-object-declaration (class))
(defmethod generate-class-object-declaration (class)
  (when (exported? class)
        (object-declaration class)))
(defmethod generate-class-object-declaration ((class <basic-class-def>))
  ())

;;;-----------------------------------------------------------------------------
;;; Function Declarations
;;;-----------------------------------------------------------------------------

(defun generate-function-declaration (fun)
  (when (exported? fun)
        (write-code "~%")
        (function-declaration fun)))

(defgeneric function-declaration (fun))

(defmethod function-declaration ((fun <generic-fun>))
  (object-declaration fun))

(defmethod function-declaration ((fun <discriminating-fun>))
  ;; discriminating functions of exported generic functions have to be defined
  ;; in the application, therefore no prototype is needed this scheme doesn't
  ;; work for module compilation because in a single module a discriminating
  ;; function may be defined or not the function object is not needed in any
  ;; case
  ())

(defmethod function-declaration ((fun <special-sys-fun>))
  ;; do nothing
  ())

(defun types-and-parameters-1 (required function-signature i)
  (if (null? required)
      ()
    (progn
      (setf (?type (car required))
            (svref function-signature i))
      (list* (type-identifier (svref function-signature i))
             (local-c-identifier (car required))
             (types-and-parameters-1 (cdr required)
                                     function-signature
                                     (+ i 1))))))

(defun types-and-parameters (fun)
  ;; only required parameters must be considered, because functions with other
  ;; parameters are mapped to functions which have required parameters only
  (types-and-parameters-1 (?var-list (?params fun))
                          (function-signature fun)
                          1))

(defun generate-function-header (fun)
  (write-code "~%~@<extern ~A ~:I~A~:_(~:I~{~A ~A~^, ~_~})~:>"
              (type-identifier (result-type fun))
              (c-identifier fun)
              (types-and-parameters fun)))

(defun function-prototype (fun)
  (with-local-identifiers
   (generate-function-header fun)
   (write-code ";")
   ;; reset generated code-identifiers of parameters
   ;; they are recreated when generating function definitions
   ;; this is because function header and body must be handled in the same
   ;; 'with-local-identifier'-context
   (mapc (lambda (var)
           (setf (?code-identifier var) ()))
         (?var-list (?params fun)))))

(defmethod function-declaration ((fun <defined-fun>))
  (function-prototype fun)
  (object-declaration fun))

(defmethod function-declaration ((fun <imported-fun>))
  ;; the prototype of imported functions is got by including header-files into
  ;; the C source
  ())

;;;-----------------------------------------------------------------------------
;;; constants, variables and symbols
;;;-----------------------------------------------------------------------------

(defun generate-var-declaration (var)
  (when (exported? var)
        (write-code "~%extern ~A ~A;"
                    (type-identifier (global-var-type var))
                    (c-identifier var))))

(defun generate-const-declaration (const)
  (when (exported? const)
        (write-code "~%extern ~A ~A;"
                    (type-identifier (global-var-type const))
                    (c-identifier const))))

(defun generate-sym-declaration (sym)
  (when (exported? sym)
        (object-declaration sym)))

;;;-----------------------------------------------------------------------------
;;; Rename Exports
;;;-----------------------------------------------------------------------------

(defun generate-rename-macro (binding)
  (write-code "~%#define ~A ~A"
              (c-identifier binding)
              (c-identifier (finally-refered-object binding))))

;;;-----------------------------------------------------------------------------
;;; general predicates
;;;-----------------------------------------------------------------------------
(defun subclass? (class superclass)
  (member superclass (~class-precedence-list class)))

;;;-----------------------------------------------------------------------------
#module-end
;;;-----------------------------------------------------------------------------
