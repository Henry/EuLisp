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
;;; Title: Transformation of Literals into LZS
;;;  Description:
;;;  Notes:
;;    expand-literal for <standard-class-def> should use the slot descriptions
;;    of the class and not explicitely an explicit slot order and explicit slot
;;    types.  The whole technique to transform objects into literal instances
;;    has to be reworked, at least for imported objects. The stuff for imported
;;    objects is more a hack than a pretty implementation. Search for "imported"
;;    to get these places.
;;;  Authors: Ingo Mohr
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

#module el2lzs-literals
(import (level-0
         apply-standard
         option-lists
         el2lzs-rules
         expand-literal
         lzs
         accessors
         lzs-mop
         lzs-eval
         list-ext
         tail-module
         representation
         (only (mapcar
                mapc
                make-instance
                find
                identity
                warn
                format
                string)
               common-lisp))
 syntax (level-1
         el2lzs-rules
         (only (push)
               common-lisp))
 export (expand-literal
         literal-type
         reset-literal-expanders))

;;;-----------------------------------------------------------------------------
;;; %literal
;;;-----------------------------------------------------------------------------
(deftranssyn (%literal class . values)
  (whole-form))

(deftrans (%literal class . values)
  (let ((class-def (trans class)))
    (make-literal-instance
     class-def
     (deftrans-literal-values class-def
       (~class-representation class-def)
       values))))

(defgeneric deftrans-literal-values (class representation values))

(defmethod deftrans-literal-values (class-def representation values)
  ;;the default case: return the value list as it is
  ;; this is used for the following representations:
  ;; direct, vector, pointer-to-vector
  values)

(defmethod deftrans-literal-values
  ((class-def <class-def>)
   (representation <%pointer-to-struct>)
   values)
  (mapcar (lambda (slot)
            (let ((entry (find-option (~slot-name slot) values ())))
              (if entry (lzslit (car entry)) ;formerly: (trans (car entry))
                ^unknown)))
          (~class-slots class-def)))

;;(defmethod deftrans-literal-values
;;           ((class-def <class-def>)
;;            (representation <%struct>)
;;            values)
;;  (mapcar (lambda (slot)
;;            (let ((entry (find-option (~slot-name slot) values ())))
;;              (if entry (lzslit (car entry)) ;formerly: (trans (car entry))
;;                  ^unknown)))
;;          (~class-slots class-def)))

(defmethod deftrans-literal-values
  ((class-def <class-def>)
   (representation <%pointer-to-vector>)
   values)
  (list (car values) ; the length specification
        (cond ((cons? (car (cdr values)))
               (mapcar #'lzslit (car (cdr values))))
              (t (car (cdr values))))))

;;;-----------------------------------------------------------------------------
;;; %function
;;;-----------------------------------------------------------------------------
;; (%function fun)
;; usable in the same places as quote
;; fun must be a constant binding created by defun, defgeneric, %define-function
;; or %declare-external-function

(deftranssyn (%function function)
  (whole-form))

(deftrans (%function function)
  (%function-literal (trans function)))

;;;-----------------------------------------------------------------------------
;;; %define-literal-expansion
;;;-----------------------------------------------------------------------------
(deftranssyn (%define-literal-expansion literal-class expander)
  (with-defining-form
   (setf (third (whole-form))
         (transsyn (third (whole-form))))
   (whole-form)))

;;TM (transmod): not necessary, because no global lexical binding is defined

(deftransdef (%define-literal-expansion literal-class expander)
  (with-defining-form
   (add-literal-expander
    literal-class
    (complete-function (make-instance <global-fun>)
                       (get-literal-expander-arguments literal-class)
                       expander
                       (dynamic lex-env)))
   ()))

;;;-----------------------------------------------------------------------------
;;; Definitions of Literal Classes
;;;-----------------------------------------------------------------------------
(defun list-of-vector-elements (vector)
  (cl:map 'cl:list #'identity vector))

(def-literal-class null <null>)

(def-literal-class integer <integer>
  (value . #'identity))

(def-literal-class character <character>
  (value . #'identity))

(def-literal-class float <float>
  (value . #'identity))

;; (def-literal-class ratio <ratio>
;;                    (nominator . #'nominator)
;;                    (denominator . #'denominator))

;; (def-literal-class complex <complex>
;;                    (realpart . #'realpart)
;;                    (imagpart . #'imagpart))

(def-literal-class symbol <defined-sym>
  (name . #'?name))

(def-literal-class symbol <imported-sym>
  (name . (lambda (ignored) "")))

(def-literal-class pair <pair>
  (car . #'car)
  (cdr . #'cdr))

(def-literal-class string <string>
  (elements . #'identity))

(def-literal-class vector <vector>
  (elements . #'list-of-vector-elements))

;; (def-literal-class array <array>
;;                    (elements . ???))

(def-literal-class function <defined-fun>
  (argument-descriptor . #'?arg-num)
  (function-pointer . #'%function-literal)
  (setter . #'?setter)
  (name . (lambda (fun) (format () "~A" (?identifier fun)))))

(def-literal-class function <imported-fun>
  (argument-descriptor . (lambda (ignored) ()))
  (function-pointer . (lambda (ignored) ()))
  (setter . (lambda (ignored) ()))
  (name . (lambda (ignored) ())))

(def-literal-class generic-function <defined-generic-fun>
  (argument-descriptor . (lambda (gf)
                           (?arg-num
                            (~generic-function-discriminating-function gf))))
  (function-pointer . #'%function-literal)
  (setter . #'?setter)
  (methods . #'~generic-function-methods)
  (name . (lambda (fun) (format () "~A" (?identifier fun))))
  (discrimination-depth . #'~generic-function-discrimination-depth))

(def-literal-class generic-function <imported-generic-fun>
  (argument-descriptor . (lambda (ignored) ()))
  (function-pointer . (lambda (ignored) ()))
  (setter . (lambda (ignored) ()))
  (methods . (lambda (ignored) ()))
  (name . (lambda (ignored) ()))
  (discrimination-depth . (lambda (ignored) ())))

(def-literal-class method <method-def>
  (domain . #'~method-domain)
  (function . #'~method-function)
  (function-pointer . (lambda (m)
                        (%function-literal (~method-function m))))
  (generic-function . #'~method-generic-function))

;; classes are handled directly, because <class> & co. are defined as part of
;; the compiler

(def-literal-class slot <slot-desc>
  (name . (lambda (sd) (format () "~A" (?identifier sd))))
  ;; (default-function . (lambda (sd) (if (eq (?initvalue sd) ^unknown)
  ;;                                  (~slot-default-function sd)
  ;;                                ^t)))
  (default-function . #'~slot-default-function)
  (initvalue . #'?initvalue)
  (keyword . #'~slot-keyword)
  (reader . #'~slot-slot-reader)
  (writer . #'~slot-slot-writer)
  )

;;;-----------------------------------------------------------------------------
;;; expand-literal
;;;-----------------------------------------------------------------------------
;; expand-literal is defined in expand-literal.em to avoid cyclic dependencies
;; in the module-dependency-graph
;; (defgeneric expand-literal (literal))

(defmethod expand-literal ((literal <literal-instance>))
  (unless (?expanded literal)
          (setf (?expanded literal) t) ; to avoid infinite expansion loops this
                                       ; flag must be set before expanding the
                                       ; slots
          (setf (?value-list literal)
                (expand-slot-values
                 literal
                 (?class literal)
                 (?representation (?class literal)) (?value-list literal)))
          (setf (?expanded literal) t))
  literal)

(defmethod expand-literal ((literal <structured-literal>))
  (or (?expanded-literal literal)
      (progn (setf (?expanded-literal literal)
                   (expand-literal (?value literal)))
             (setf (?unexpanded (?expanded-literal literal))
                   literal)
             (?expanded-literal literal))))

(defmethod expand-literal ((literal <symbol>))
  ;; if a special marker like UNKNOWN was found return this
  literal)

;; NOTE: More methods for expand-literal are defined by def-literal-class. These
;;       methods are calling expand-literal-using-desc.

(defmethod expand-literal (literal)
  ;; The default case: no special expander defined
  (error-invalid-literal literal))

(defun error-invalid-literal (literal)
  (format t "~%!!! Invalid literal ~A (a literal class doesn't exist for this)~%"
          literal))

(defun error-literal-expander-not-defined (literal exp-desc)
  (format t "~%!!! No literal expander defined for literal class ~A during
expansion of ~A~%"
          (?literal-class exp-desc) literal))

(defun check-exp-desc (literal exp-desc)
  (cond ((null? exp-desc)
         (error-invalid-literal literal)
         ())
        ((null? (?expander exp-desc))
         (error-literal-expander-not-defined literal exp-desc)
         ())
        (t t)))

(defmethod expand-literal-using-desc (literal exp-desc)
  ;; the standard mechanism for expanding literals
  ;; a cache is used which guarantees that eql-equal objects have exactly one
  ;; literal instance object
  (when (check-exp-desc literal exp-desc)
        (let ((entry (find literal (?expansions exp-desc) :key #'?unexpanded)))
          (or entry
              (let ((exp (expand-literal-first-time literal exp-desc)))
                (push exp (?expansions exp-desc))
                exp)))))

(defun expand-literal-first-time (literal exp-desc)
  (let ((exp (expand-literal-first-time-incomplete literal exp-desc)))
    (expand-literal exp)))

(defun expand-literal-first-time-incomplete (literal exp-desc)
  (dynamic-let ((lex-env (?lex-env (?module exp-desc))))
               (let ((exp (trans
                           (call (?expander exp-desc)
                                 (mapcar (lambda (slot)
                                           (funcall (cdr slot) literal))
                                         (?slots exp-desc))))))
                 (setf (?unexpanded exp) literal)
                 exp)))

(defmethod expand-literal-using-desc ((literal <null>) exp-desc)
  ;;this is an optimization for the empty list which uses a private cache - the
  ;;variable expanded-empty-list - to avoid the search in the standard cache
  (unless expanded-empty-list
          (when (check-exp-desc literal exp-desc)
                (setq expanded-empty-list
                      (dynamic-let ((lex-env (?lex-env (?module exp-desc))))
                                   (trans
                                    (call (?expander exp-desc) ()))))
                (setf (?unexpanded expanded-empty-list) () )
                (expand-literal expanded-empty-list)))
  expanded-empty-list)

(defmethod expand-literal-using-desc ((literal <fun>) exp-desc)
  ;;this is an optimization for functions. The use of the standard cache can be
  ;;avoided because the annotation expanded-literal can be used to store the
  ;;expanded literal
  (or (?expanded-literal literal)
      (when (check-exp-desc literal exp-desc)
            (let ((exp (expand-literal-first-time-incomplete literal exp-desc)))
              (setf (?expanded-literal literal) exp)
              (expand-literal exp)))))

(defmethod expand-literal-using-desc ((literal <method-def>) exp-desc)
  (or (?expanded-literal literal)
      (when (check-exp-desc literal exp-desc)
            (let ((exp (expand-literal-first-time-incomplete literal exp-desc)))
              (setf (?expanded-literal literal) exp)
              (expand-literal exp)))))

(defgeneric expand-slot-values (literal class representation values))

(defmethod expand-slot-values (literal (class <basic-class-def>)
                                       representation values)
  values)

(defmethod expand-slot-values (literal class representation values)
  (mapcar (lambda (literal)
            (if (and (cons? literal)
                     (eq (car literal) ^%literal))
                (expand-literal (trans literal))
              (expand-literal literal)))
          values))

(defmethod expand-slot-values (literal class
                                       (representation <%direct>) values)
  (setq class (~slot-type (car (~class-slots class))))
  (expand-slot-values literal
                      class
                      (~class-representation class)
                      values))

(defmethod expand-slot-values
  (literal class (representation <%pointer-to-vector>) values)
  (list (car values)
        (expand-vector-elements (car (cdr values)))))

(defgeneric expand-vector-elements (elements))
(defmethod expand-vector-elements ((elements <string>))
  elements)
(defmethod expand-vector-elements ((elements <null>))
  elements)
(defmethod expand-vector-elements ((elements <pair>))
  (mapcar #'expand-literal
          elements))

(defun literal-type (literal)
  (?class (expand-literal literal)))

;;;-----------------------------------------------------------------------------
;;; Expansion for Standard Classes
;;;-----------------------------------------------------------------------------
(defmethod expand-literal ((class <defined-class>))
  (expand-class class (~class-representation class)))

(defmethod expand-literal ((class <imported-class>))
  class)

(defgeneric expand-class (class representation))

;; slots of classes are (in this order):
;; class-name
;; class-precedence-list
;; slots
;; mm-type
;; mm-card
;; gc-tracer
;; converter
;; allocator

(defmethod expand-class ((class <standard-class-def>) representation)
  (or (?expanded-literal class)
      (progn
        (setf (?expanded-literal class)
              (make-literal-instance
               (~class-of class)
               (list (string (~class-name class))
                     (~class-precedence-list class)
                     (~class-slots class)
                     (?mm-type representation)
                     (?mm-card representation)
                     ^unknown ; gc-tracer not needed, slot should be removed
                     (~converter class)
                     (if (?allocator class)
                         (%function-literal (?allocator class))
                       ^unknown)
                     )))
        (setf (?unexpanded (?expanded-literal class)) class)
        (expand-literal (?expanded-literal class))
        (?expanded-literal class))))

(defmethod expand-class ((class <standard-class-def>)
                         (representation <%pointer-to-vector>))
  ;;slots = () --- they are not needed for vector classes
  (or (?expanded-literal class)
      (progn
        (setf (?expanded-literal class)
              (make-literal-instance
               (~class-of class)
               (list (string (~class-name class))
                     (~class-precedence-list class)
                     ()
                     (?mm-type representation)
                     (?mm-card representation)
                     ^unknown ; gc-tracer not needed, slot should be removed
                     (~converter class)
                     (if (?allocator class)
                         (%function-literal (?allocator class))
                       ^unknown)
                     )))
        (setf (?unexpanded (?expanded-literal class)) class)
        (expand-literal (?expanded-literal class))
        (?expanded-literal class))))

(defmethod expand-class ((class <tail-class-def>) representation)
  ;;don't write out slots
  (or (?expanded-literal class)
      (progn
        (setf (?expanded-literal class)
              (make-literal-instance
               (~class-of class)
               (list (string (~class-name class))
                     (~class-precedence-list class)
                     ()
                     (?mm-type representation)
                     (?mm-card representation)
                     ^unknown ; gc-tracer not needed, slot should be removed
                     (~converter class)
                     (if (?allocator class)
                         (%function-literal (?allocator class))
                       ^unknown)
                     )))
        (setf (?unexpanded (?expanded-literal class)) class)
        (expand-literal (?expanded-literal class))
        (?expanded-literal class))))

(defmethod expand-class ((class <abstract-class-def>) representation)
  ;;don't write out slots
  (or (?expanded-literal class)
      (progn
        (setf (?expanded-literal class)
              (make-literal-instance
               (~class-of class)
               (list (string (~class-name class))
                     (~class-precedence-list class)
                     () ; slot-descs are needed only for make, and this is not
                     ;; allowed for abstract classes
                     (?mm-type representation)
                     (?mm-card representation)
                     ^unknown ; gc-tracer not needed, slot should be removed
                     (~converter class)
                     ^unknown ; instance creation is not allowed for abstract
                              ; classes
                     )))
        (setf (?unexpanded (?expanded-literal class)) class)
        (expand-literal (?expanded-literal class))
        (?expanded-literal class))))

;;;-----------------------------------------------------------------------------
#module-end
;;;-----------------------------------------------------------------------------
