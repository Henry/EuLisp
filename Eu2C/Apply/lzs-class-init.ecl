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
;;;  Title: basic initialisation of the object system
;;;  Description:
;;;  Documentation:
;;;  Notes:
;;    The initialization of predefined classes is divided into two steps to break the
;;    circle
;;    initialization of <class> -> slot-accessors are needed to generate runtime
;;    initialization -> <class> has to be initialized .
;;;  Authors: Winfried Heicking
;;;-----------------------------------------------------------------------------
;;;begin module lzs-class-init

#module lzs-class-init

(import
 (accessors
  eulisp1
  lzs
  lzs-mop
  lzs-modules
  standard-init
  el2lzs
  expand-literal
  tail-module
  apply-funs
  (only (make-defined-sym) el2lzs-main)
  (only (*basic-system* *compilation-type*) predicates)
  (only (make-instance append mapc apply mapcar vector remove-if)
        common-lisp)
  machine-description)

 syntax
 (eulisp1
  apply-standard
  debugging
  (only (instance-of-p) el2lzs-main)
  (only (setf push)
        common-lisp)
  )

 export
 (initialize-predefined-standard-classes
  initialize-predefined-standard-classes-part-2
  handle-symbols
  )
 )

(defconstant $class-type-descriptor 4)
(defconstant $abstract-class-type-descriptor 5)
(defconstant $tail-class-type-descriptor 6)
(defconstant $%string-type-descriptor 7)

(defgeneric generated-function-p (fun))
(defmethod generated-function-p (fun) nil)
(defmethod generated-function-p ((fun <slot-accessor-fun>)) t)
(defmethod generated-function-p ((fun <slot-init-fun>)) t)
(defmethod generated-function-p ((fun <constructor-fun>)) t)

(defun slot-description-option ()
  (if *basic-system* ^effective-slot-descriptions ^direct-slot-descriptions))

(defun accessor-bindings-needed-p ()
  (null? *basic-system*))

(defun class-allocation (alloc-if-defined)
  (if *basic-system* nil alloc-if-defined))

(defun slot-specs-for-class ()
  (list (list ^name ^class-precedence-list
              ^type %object)
        (list ^name ^slot-descriptions
              ^type %object)
        (list ^name ^mm-type
              ^type %signed-word-integer)
        (list ^name ^mm-card
              ^type %signed-word-integer)
        (list ^name ^gc-tracer
              ^type %function)
        (list ^name ^converter
              ^type %object)
        (list ^name ^allocator
              ^type %function)
        ))

(defun initialize-predefined-standard-classes ()
  (dynamic-let ((*current-module* $tail-module))

;;; remove all generated accessor-functions... created in a previous run for
;;; %object... in module %tail
               (setf (?fun-list $tail-module)
                     (remove-if #'generated-function-p (?fun-list $tail-module)))

               (~initialize %object
                            (list ^name ^%object
                                  ^direct-superclasses ()
                                  (slot-description-option) ()
                                  ^direct-keywords ()
                                  ^representation ^pointer-to-void
                                  ))
               (~initialize %class
                            (list ^name ^%class
                                  ^direct-superclasses (list %object)
                                  (slot-description-option)
                                  (slot-specs-for-class)
                                  ^direct-keywords (let ((keywords ^(name
                                                                     direct-superclasses
                                                                     direct-slot-descriptions
                                                                     direct-keywords)))
                                                     (mapc #'make-defined-sym keywords)
                                                     keywords)
                                  ^representation ^pointer-to-struct
                                  ^allocation (class-allocation ^multiple-type-card)
                                  ^mm-type $class-type-descriptor
                                  ))

               (when (accessor-bindings-needed-p)
                     (name-and-export-reader %class ^class-precedence-list
                                             ^%class-precedence-list)
                     (name-and-export-reader %class ^slot-descriptions
                                             ^%class-slot-descriptions)
                     (name-and-export-accessor %class ^mm-type
                                               ^%class-mm-type)
                     (name-and-export-accessor %class ^mm-card
                                               ^%class-mm-card)
                     (name-and-export-accessor %class ^gc-tracer
                                               ^%class-gc-tracer)
                     (name-and-export-reader %class ^converter
                                             ^%class-converter)
                     (name-and-export-reader %class ^allocator
                                             ^%class-allocator)
                     )

               (~initialize %abstract-class
                            (list ^name ^%abstract-class
                                  ^direct-superclasses (list %class)
                                  (slot-description-option)
                                  (if (null? *basic-system*)
                                      ()
                                    (slot-specs-for-class))
                                  ^direct-keywords ()
                                  ^representation ^pointer-to-struct
                                  ^mm-type $abstract-class-type-descriptor
                                  ^allocation (class-allocation ^multiple-type-card)
                                  ))
               (~initialize %tail-class
                            (list ^name ^%tail-class
                                  ^direct-superclasses (list %class)
                                  (slot-description-option)
                                  (if (null? *basic-system*)
                                      ()
                                    (slot-specs-for-class))
                                  ^direct-keywords ()
                                  ^representation ^pointer-to-struct
                                  ^mm-type $tail-class-type-descriptor
                                  ^allocation (class-allocation ^multiple-type-card)
                                  ))
               (~initialize %string
                            ;; ATTENTION: %string is an imported class in every case
                            (list ^name ^%string
                                  ^direct-superclasses ()
                                  ^effective-slot-descriptions
                                  (list (list ^name ^length)
                                        (list ^name ^element
                                              ^type %unsigned-byte-integer
                                              ^reader (accessor-bindings-needed-p)
                                              ^writer (accessor-bindings-needed-p))
                                        )
                                  ^direct-keywords (let ((keywords ^(length element)))
                                                     (mapc #'make-defined-sym keywords)
                                                     keywords)
                                  ^representation ^pointer-to-vector
                                  ^mm-type $%string-type-descriptor
                                  ^allocation ^multiple-size-card
                                  ))

               ()

;;; the accessor %string-ref is not yet needed in the implementation of eulisp0
;;; to provide this accessor a different mechanism is necessary because %string
;;; is an imported class
;;;(name-and-export-accessor %string ^element
;;;                            ^%string-ref)

               (mapc #'initialize-basic-class (?class-def-list $tail-module))

               ))

(defun initialize-predefined-standard-classes-part-2 ()
  (unless *basic-system*
          (dynamic-let ((*current-module* $tail-module))
                       (add-toplevel-forms-for-tail-module
                        (~compute-runtime-initialization %object))
                       (add-toplevel-forms-for-tail-module
                        (~compute-runtime-initialization %class))
                       (add-toplevel-forms-for-tail-module
                        (~compute-runtime-initialization %abstract-class))
                       (add-toplevel-forms-for-tail-module
                        (~compute-runtime-initialization %tail-class))
                       (add-toplevel-forms-for-tail-module
                        (~compute-runtime-initialization %string))

                       )))

(defgeneric initialize-basic-class (class))
(defmethod initialize-basic-class (class) nil)
(defmethod initialize-basic-class ((class <%string>))
  (setf (?class class) %tail-class)
  (setf (?expanded-literal class) nil)
  (setf (?class-precedence-list class) nil))
(defmethod initialize-basic-class ((class <basic-class-def>))
  (setf (?place class) nil)
  (setf (?expanded-literal class) nil)
  (setf (?class class) %tail-class)
  (setf (?class-precedence-list class) nil)
  ;;lattice-type is set by the initialization of the type inference
  (~compute-runtime-initialization class) ;only for side effect
  )

(defun handle-symbols (main-module modules)
  (setf (?sym-list main-module) (dynamic symbol-env))
  (when (eq *compilation-type* :application)
        (let ((symtab-initfun
               (make-instance <global-fun>
                              :identifier (list ^symbol-table-initializator)
                              :params (make-instance <params>
                                                     :var-list nil
                                                     :rest nil)
                              :body (make-instance <progn-form>
                                                   :form-list
                                                   (mapcar (lambda (sym)
                                                             (make-instance <app>
                                                                            :function %add-symbol
                                                                            :arg-list (list sym)))
                                                           (dynamic symbol-env))))))
          (setf (?symtab-initfun main-module) symtab-initfun)
          (add-lexical symtab-initfun main-module nil))))

(defun name-and-export-reader (class slot-name reader-name)
  (let ((reader (~slot-description-slot-reader
                 (~find-slot-description class slot-name))))
    (setf (?identifier reader) reader-name)
    (push reader (?lex-env $tail-module))
    (push reader (?exports $tail-module))))

(defun name-and-export-accessor (class slot-name accessor-name)
  (let* ((slot (~find-slot-description class slot-name))
         (accessor (~slot-description-slot-reader slot))
         (writer (~slot-description-slot-writer slot)))
    (setf (?identifier accessor) accessor-name)
    (setf (?identifier writer) (list ^setter accessor-name))
    (setf (?setter accessor) writer)
    (push accessor (?lex-env $tail-module))
    (push accessor (?exports $tail-module))))

#module-end
;;;eof lzs-class-init
