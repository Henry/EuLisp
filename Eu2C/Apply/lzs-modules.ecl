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

#module lzs-modules
(import ((except (concatenate)
                 level-0)
         dynamic
         lzs
         accessors
         el2lzs-main
         (only (*PRINT-PRETTY*
                *PRINT-MISER-WIDTH*
                concatenate
                cerror
                intern
                make-instance
                mapcar)
               common-lisp)
         (only (make-eulisp-class-id)
               apply-standard)
         debugging)
 syntax (level-0
         dynamic
         (only (generic-flet push) common-lisp)
         el2lzs-main
         class-ext
         debugging)
 export ($tail-module
         pp-module
         find-lexical add-lexical
         export-lexical export-syntax-lexical
         tail-environment))

;;;-----------------------------------------------------------------------------
;;; add-defined-object (obj module)
;;;-----------------------------------------------------------------------------
;;; add-defined-object adds obj to one of the modules object-lists fun-list,
;;; class-def-list, named-const-list, var-list and sym-list. Which list must
;;; be modified is decided by the class of the object obj: <fun>, <class-def>,
;;; <named-const>, <var>, <sym> (in the order of object-lists above)
;;; This generic function is needed for get-lexical

(defgeneric add-defined-object (obj module))

(defmethod add-defined-object (obj (module <symbol>))
  (add-defined-object obj (find-module module)))

(defmethod add-defined-object ((obj <fun>) (module <module>))
  (push obj (?fun-list module))
  (push obj (?lex-env module))
  obj)

(defmethod add-defined-object ((obj <class-def>) (module <module>))
  (push obj (?class-def-list module))
  (push obj (?lex-env module))
  obj)

(defmethod add-defined-object ((obj <named-const>) (module <module>))
  (push obj (?named-const-list module))
  (push obj (?lex-env module))
  obj)

(defmethod add-defined-object ((obj <static>) (module <module>))
  (push obj (?named-const-list module))
  (push obj (?lex-env module))
  obj)

(defmethod add-defined-object ((obj <dynamic>) (module <module>))
  (push obj (?var-list module))
  obj)

(defmethod add-defined-object ((obj <sym>) (module <module>))
  (push obj (?sym-list module))
  obj)

;;;-----------------------------------------------------------------------------
;;; add-lexical (obj module export)
;;;-----------------------------------------------------------------------------
;;; add-lexical puts the given object in a module such that this object is
;;; acessible under its identifier in this module and appears in one of the
;;; lists of defined objects in the module. An error is signalled if a lexical
;;; binding already exists with the same name as in the identifier-slot of obj.
;;; The result of add-lexical is obj.

(defun add-lexical (obj module export)
  (when (and (development-mode) (find-lexical (?identifier obj) module))
        (cerror "bind ~A with ~A which then shadows the old binding ~A"
                "a lexical binding for ~A already exists"
                (?identifier obj) obj (find-lexical (?identifier obj) module)))
  (add-defined-object obj module)
  (cond ((eq export ^export) (export-lexical obj module))
        ((eq export ^syntax) (export-syntax-lexical obj module)))
  obj)

;;;-----------------------------------------------------------------------------
;;; find-lexical (identifier module)
;;;-----------------------------------------------------------------------------
;;; It returns the object which is bound lexically to identifier at module
;;; top-level. As the argument module a symbol (the name of the module) or the
;;; module object itself may be given.

(defgeneric find-lexical (identifier module))

(defmethod find-lexical (identifier (module <symbol>))
  (let ((module (find-module module)))
    (if module
        (find-lexical identifier module)
      ())))

(defmethod find-lexical (identifier (module <module>))
  (find-in-env (?lex-env module) identifier))

;;;-----------------------------------------------------------------------------
;;; export-lexical lzs-object
;;; export-syntax-lexical lzs-object
;;;-----------------------------------------------------------------------------

(defun export-lexical (obj module-object)
  (setf (?exports module-object) (cons obj (?exports module-object))))

(defun export-syntax-lexical (obj module-object)
  (setf (?syntax-exports module-object) (cons obj (?syntax-exports module-object))))

;;;-----------------------------------------------------------------------------
;;; define-tail
;;;-----------------------------------------------------------------------------
;;;(define-tail identifier export supers . initial-values) installs a basic tail
;;;object and initializes it with the given initial-values. The following things
;;;are done:
;;;* Definition of a class <identifier> as a direct subclass of classes in supers
;;;* Definition of a constant named identifier holding the only instance of
;;;<identifier>, which is initialized according to initial-values
;;;* If export is not NIL the instance is installed in the basic module TAIL, such
;;;that it is lexically available under the name identifier, and it is installed
;;;in the appropriate list of defined objects
;;;* export=EXPORT - the instance is exported from module TAIL
;;;        =SYNTAX - the instance is syntax-exported from module TAIL
;;;        = NIL   - the instance isn't exported and is not included in the
;;;                  module TAIL
;;;* <identifier> and identifier are exported from the current module.

(defmacro define-tail (identifier export supers . initial-values)
  (let ((class-id (make-eulisp-class-id identifier)))
    `(progn
       (defstandardclass ,class-id ,(mapcar #'make-eulisp-class-id supers))
       (defconstant ,identifier (make-instance ,class-id
                                               :identifier ',(make-eulisp-symbol identifier)
                                               :module  $tail-module
                                               ,@initial-values))
       (add-lexical ,identifier $tail-module ',(make-eulisp-symbol export))
       (export ,identifier ,class-id))))

;;;-----------------------------------------------------------------------------
;;; pretty-printing modules
;;;-----------------------------------------------------------------------------

(defun pp-module (mod)
  (let ((*print-pretty* t)
        (*print-miser-width* ()))
    (print (if (symbol? mod) (find-module mod) mod))))

;;;-----------------------------------------------------------------------------
;;; retrieving the environment of the module TAIL
;;;-----------------------------------------------------------------------------
(defun tail-environment ()
  (?exports $tail-module))

;;;-----------------------------------------------------------------------------
#module-end
;;;-----------------------------------------------------------------------------
