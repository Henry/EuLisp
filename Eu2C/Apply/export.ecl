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
;;; Title: Handling of explicit and invisible exports.
;;;  Description:
;;;  Documentation:
;;    All explicitly exported objects are marked as exported by storing a pair
;;    (interface-identifier . interface-module) in the annotation exported. The
;;    interface-identifier is taken from the export-environment of the module which
;;    is the current compilation unit.
;;    Some objects must be exported invisible because they are not part of the
;;    interface but are needed for subsequently compilation steps basing on the
;;    current compilation unit. Invisible exported objects are the following objects
;;    if they are not explicitely exported:
;;    (1) global objects refered by a function to be inlined
;;    (2) global objects refered directly and indirectly by exported macros
;;    (3) a class which appears in a function signature or in another place as a type
;;    (4) the method functions of an explicitely exported generic function
;;    (5) converter (and also allocator ??) of an explicitely exported class
;;    (6) setter of an explicitely exported function
;;    (7) a function which appears in the reduce annotation of an explicitely exported
;;    function
;;    (8) all default discriminator functions for generic dispatch
;;    (9) all objects explicitely annotated by is-special-function/class/binding
;;    (10) all symbols to make them global available
;;    (11) the discriminating functions of all imported generic functions because they
;;    are not defined in the basic system to allow better optimizations in an
;;    application where the discriminating functions of all imported generic functions
;;    have to be defined
;;    Some of the invisible exports (1 and 3 - if a class appears in a signature
;;                                     computed by the TI) can't be marked before transforming the modules into the
;;    MZS-representation because therefore informations are needed which are
;;    computed by the type-inference or the inlining step. The name of an invisible
;;    exported object is computed from the lisp-identifier and the identifier of the
;;    defining module. To distinct invisible exported objects from explicitely
;;    exported ones their interface-identifier is wrapped by a list.
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Ingo Mohr
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

#module export
(import (level-0
         binding
         apply-funs
         lzs accessors
         lzs-mop
         messages
         predicates
         (only (*literal-expanders*
                ?expander)
               el2lzs-basic)
         (only (map-to-std-discr-funs)
               generic-dispatch)
         (only (mapc
                mapcar
                append
                svref)
               common-lisp))
 syntax (level-0
         (only (dotimes)
               common-lisp))
 export (export-objects
         export-identifier
         exporting-module
         invisible-exported?))


;;;-----------------------------------------------------------------------------
;;; access to special infos of exported objects
;;;-----------------------------------------------------------------------------

(defun export-identifier (object)
  (or (and (global? object)
           (?exported object)
           (if (cons? (car (?exported object)))
               (car (car (?exported object)))
             (car (?exported object))))
      (?identifier object)))

(defun exporting-module (object)
  (and (global? object)
       (?exported object)
       (cdr (?exported object))))

(defun invisible-exported? (object)
  (and (global? object)
       (?exported object)
       (cons? (car (?exported object)))))

(defun explicitely-exported? (object)
  (and (global? object)
       (?exported object)
       (null? (cons? (car (?exported object))))))

;;;-----------------------------------------------------------------------------
;;; creating identifiers for export
;;;-----------------------------------------------------------------------------

(defgeneric identifier-for-export (id))

(defmethod identifier-for-export ((id <cons>))
  (make-symbol (format () "~{~A~^-~}" id)))

(defmethod identifier-for-export ((id <string>))
  (make-symbol id))

(defmethod identifier-for-export ((id <symbol>))
  id)

;;;-----------------------------------------------------------------------------
;;; main functions for export
;;;-----------------------------------------------------------------------------

(defun export-objects (module module-env)
  (mapc (lambda (object)
          (export-obj object module ()))
        (?exports module))
  (export-invisible* (?sym-list module) module)
  (when (eq *compilation-type* :application)
        (export-invisible (?symtab-initfun module) module)
        (when *basic-system*
              (mapc (lambda (fun)
                      (export-discriminating-functions-invisible fun module))
                    (?fun-list *basic-system*))))
  (when (eq *compilation-type* :basic-system)
        (search-and-mark-invisible-exports module-env module)
        (map-to-std-discr-funs (lambda (fun)
                                 (export-invisible fun module)))
        (export-invisible* *special-functions* module)
        (export-invisible* *special-classes* module)
        (export-invisible* *special-bindings* module)
        (export-objects-used-in-literal-expanders module)
        ))

(defun mark-object-as-exported (object module export-identifier)
  (setf (?exported object)
        (cons (identifier-for-export export-identifier)
              module)))

(defgeneric export-obj (object module export-identifier))

(defmethod export-obj (object module export-identifier)
  (mark-object-as-exported object module
                           (or export-identifier (?identifier object))))

(defmethod export-obj ((object <binding>) module export-identifier)
  (export-obj (finally-refered-object object)
              module
              (or export-identifier (?identifier object))))

(defmethod export-obj ((object <named-const>) module export-identifier)
  ;;Notes:
  ;; 1. the case of generated functions bound to constants is handled separately
  ;;    then only the function is exported
  ;; 2. no invisible exports are possible for constants
  (if (fun? (?value object))
      (export-obj (?value object) module (or export-identifier (?identifier object)))
    (mark-object-as-exported object module (or export-identifier (?identifier object)))))

;;;-----------------------------------------------------------------------------
;;; invisible export (general)
;;;-----------------------------------------------------------------------------

(defun search-and-mark-invisible-exports (module-env if-module)
  (labels ((mark-invisible-exports-for-object-list (objects)
                                                   (mapc (lambda (obj)
                                                           (when (explicitely-exported? obj)
                                                                 (mark-invisible-exports obj if-module)))
                                                         objects)))
          (mapc (lambda (module)
                  (mark-invisible-exports-for-object-list (?fun-list module))
                  (mark-invisible-exports-for-object-list (?class-def-list module))
                  )
                module-env)))

(defun export-invisible* (objects module)
  (mapc (lambda (obj)
          (export-invisible obj module))
        objects))

(defun export-invisible (object module)
  (unless (?exported object)
          (setf (?exported object)
                (cons (list (identifier-for-export (?identifier object)))
                      module))
          (mark-invisible-exports object module)))

(defun mark-invisible-exported-classes-in-signature (sig-vector module)
  (dotimes (i (length sig-vector))
           (export-invisible (svref sig-vector i) module)))

(defgeneric mark-invisible-exports (object module)
  ;; assumes that this function is called only for exported objects (including
  ;; also invisible exported objects)
  ;; NOTE: the invisible export is not recursive, but I'm not sure whether it is
  ;; necessary to do it recursively or not
  )

(defmethod mark-invisible-exports (object module)
  ;; default: nothing must be exported invisible
  ())

(defmethod mark-invisible-exports ((fun <fun>) module)
  (mark-invisible-exported-classes-in-signature (?range-and-domain fun) module)
  (when (?setter fun) (export-invisible (?setter fun) module)))

(defmethod mark-invisible-exports ((gf <generic-fun>) module)
  (mark-invisible-exported-classes-in-signature (?range-and-domain gf) module)
  (export-invisible (?discriminating-fun gf) module)
  (mapcar (lambda (method) (export-invisible (?fun method) module))
          (?method-list gf))
  (when (?setter gf) (export-invisible (?setter gf) module)))

(defmethod mark-invisible-exports ((class <class-def>) module)
  (when (?converter class) (export-invisible (?converter class) module))
  (export-invisible (?class class) module)
  (mapc (lambda (slot)
          (when (?type slot)
                (export-invisible (?type slot) module)))
        (~class-slots class)))

;;;-----------------------------------------------------------------------------
;;; invisible export of discriminating functions of imported generic functions
;;;-----------------------------------------------------------------------------
;;; The discriminating functions which are generated in the application for
;;; imported generic functions have to be exported to guarantee their analysis
;;; by the compiler. The discriminating function of a generic function is not
;;; defined in the basic system. They have to be defined in any case in the
;;; application because they may be called in the basic system too and so they
;;; are needed also if they are not needed directly by the application module(s).

(defgeneric export-discriminating-functions-invisible (fun module))

(defmethod export-discriminating-functions-invisible (fun module)
  ())

(defmethod export-discriminating-functions-invisible ((fun <imported-generic-fun>) module)
  (export-invisible (?discriminating-fun fun) module))

;;;-----------------------------------------------------------------------------
;;; invisible export of objects used in macros and literal expanders
;;;-----------------------------------------------------------------------------

(defun export-objects-used-in-literal-expanders (module)
  (mapc (lambda (lexp)
          (when (?expander lexp)
                (export-objects-used-in-macro-function (?expander lexp) module)))
        *literal-expanders*))

;;;-----------------------------------------------------------------------------
;;; general traversing of lzs-forms
;;;-----------------------------------------------------------------------------

(defun warning-traverse-method-not-yet-implemented (special-form-name)
  (write-message ^internal-compiler-problem
                 "traversing of LZS-form ~A is not yet implemented~
                  ~%(see definition of 'traverse-lzs-form'"
                 special-form-name))

(defgeneric traverse-lzs-form (form function)
  ;; the form form is traversed in the following way:
  ;; 1. if form is a special (or compound) form all forms contained in this are
  ;; traversed recursively (such forms are progn-forms, init-forms ...)
  ;; 2. if the form is a function, variable reference, class, constant or a
  ;; literal then the function 'function' is applied to it
  )

(defun export-objects-used-in-macro-function (fun module)
  (traverse-lzs-form (?body fun)
                     (lambda (form)
                       (when (global? form)
                             (export-invisible form module)))))

(defmethod traverse-lzs-form (form function)
  (funcall function form))

(defmethod traverse-lzs-form ((form <app>) function)
  (traverse-lzs-form (?function form) function)
  (mapc (lambda (form)
          (traverse-lzs-form form function))
        (?arg-list form)))

(defmethod traverse-lzs-form ((form <setq-form>) function)
  (traverse-lzs-form (?location form) function)
  (traverse-lzs-form (?form form) function))

(defmethod traverse-lzs-form ((form <progn-form>) function)
  (mapc (lambda (form)
          (traverse-lzs-form form function))
        (?form-list form)))

(defmethod traverse-lzs-form ((form <if-form>) function)
  (traverse-lzs-form (?pred form) function)
  (traverse-lzs-form (?then form) function)
  (traverse-lzs-form (?else form) function))

(defmethod traverse-lzs-form ((form <switch-form>) function)
  (warning-traverse-method-not-yet-implemented ^switch-form))

(defmethod traverse-lzs-form ((form <labeled-form>) function)
  (warning-traverse-method-not-yet-implemented ^labeled-form))

(defmethod traverse-lzs-form ((form <let*-form>) function)
  (mapc (lambda (init)
          (traverse-lzs-form init function))
        (?init-list form))
  (traverse-lzs-form (?body form) function))

(defmethod traverse-lzs-form ((form <labels-form>) function)
  (warning-traverse-method-not-yet-implemented ^labels-form))

(defmethod traverse-lzs-form ((form <let/cc-form>) function)
  (traverse-lzs-form (?cont form) function)
  (traverse-lzs-form (?body form) function))

(defmethod traverse-lzs-form ((form <tagbody-form>) function)
  (mapc (lambda (form)
          (traverse-lzs-form form function))
        (?form-list form)))

(defmethod traverse-lzs-form ((form <tagged-form>) function)
  (warning-traverse-method-not-yet-implemented ^tagged-form))

(defmethod traverse-lzs-form ((form <mv-lambda>) function)
  (warning-traverse-method-not-yet-implemented ^mv-lambda))

(defmethod traverse-lzs-form ((form <get-slot-value>) function)
  (warning-traverse-method-not-yet-implemented ^get-slot-value))

(defmethod traverse-lzs-form ((form <set-slot-value>) function)
  (warning-traverse-method-not-yet-implemented ^set-slot-value))

#module-end
