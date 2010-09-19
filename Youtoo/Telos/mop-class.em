;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: telos (The EuLisp Object System -- TELOS)
;;;  Authors: Russel Bradford, Andreas Kind
;;; Description: initial class hierarchy
;;
;;    Some classes are predefined when the Vm is initialized:
;;    <keyword>, <simple-generic-funtion>, <simple-function>,
;;    <simple-class>, <character>, <cons>, <string>, <null>, <int>, <double>,
;;    <vector>, <hash-table>, <simple-method>, <symbol>
;;;-----------------------------------------------------------------------------
(defmodule mop-class
  (syntax (_boot0)
   import (boot mop-prim)
   export (<object>
           <metaclass>
           <simple-class> <class> <function-class>
           <function> <simple-function>
           <generic-function> <simple-generic-function>
           <method> <simple-method>
           <slot> <local-slot>
           <list> <cons> <null>
           <name> <symbol> <keyword>
           object-size class-size lsd-size gf-size method-size
           sd-size name-size function-size sf-size cons-size
           class-slotz sf-slots gf-slots method-slots lsd-slots
           sd-slots object-slots cons-slots sf-direct-slots name-slots
           class-slot-defaults lsd-slot-defaults function-slot-defaults
           sf-slot-defaults gf-slot-defaults cons-slot-defaults
           method-slot-defaults name-slot-defaults function-slot-defaults
           sf-direct-slot-defaults
           class-keywordz lsd-keywords gf-keywords method-keywords sd-keywords
           object-keywords name-keywords cons-keywords
           class-name class-instance-length class-direct-superclasses
           class-direct-subclasses class-slots class-keywords
           class-precedence-list class-abstract? converter class-code
           function-name function-domain generic-function-method-class
           simple-function-environment simple-function-code
           generic-function-domain
           generic-function-method-keywords generic-function-methods
           generic-function-method-lookup-function
           generic-function-discriminating-function
           generic-function-method-cache
           method-generic-function method-domain
           method-function slot-reader slot-writer slot-keyword slot-name
           slot-default slot-required? symbol-name keyword-name
           ))

;;;-----------------------------------------------------------------------------
;;; Shape of <class>
;;;-----------------------------------------------------------------------------
(defconstant class-size 10)

(defconstant <simple-class>
  (primitive-allocate () class-size))

(defconstant <metaclass>
  (primitive-allocate () class-size))

((setter primitive-class-of) <metaclass> <simple-class>)
((setter primitive-class-of) <simple-class> <metaclass>)

(defconstant <class>
  (primitive-allocate <metaclass> class-size))

(defconstant <function-class>
  (primitive-allocate <metaclass> class-size))

(defconstant class-slotz
  '(name instance-length direct-superclasses direct-subclasses slots
         keywords class-precedence-list abstract? converter code))

(defconstant class-keywordz
  '(name: direct-superclasses: direct-slots: direct-keywords: abstract?:))

(defconstant class-slot-defaults
  (list (lambda () 'anonymous)
        *default-slot-value*
        (lambda () (list <object>))
        *default-slot-value*
        *default-slot-value*
        *default-slot-value*
        *default-slot-value*
        *default-slot-value*
        *default-slot-value*
        *default-slot-value*))

(defun class-name (cl) (primitive-ref cl 0))
(declare-inline class-name)

(defun (setter class-name) (cl val)
  ((setter primitive-ref) cl 0 val))

(defun class-instance-length (cl) (primitive-ref cl 1))
(declare-inline class-instance-length)

(defun (setter class-instance-length) (cl val)
  ((setter primitive-ref) cl 1 val))

(defun class-direct-superclasses (cl) (primitive-ref cl 2))
(declare-inline class-direct-superclasses)

(defun (setter class-direct-superclasses) (cl val)
  ((setter primitive-ref) cl 2 val))

(defun class-direct-subclasses (cl) (primitive-ref cl 3))
(declare-inline class-direct-subclasses)

(defun (setter class-direct-subclasses) (cl val)
  ((setter primitive-ref) cl 3 val))

(defun class-slots (cl) (primitive-ref cl 4))
(declare-inline class-slots)

(defun (setter class-slots) (cl val)
  ((setter primitive-ref) cl 4 val))

(defun class-keywords (cl) (primitive-ref cl 5))
(declare-inline class-keywords)

(defun (setter class-keywords) (cl val)
  ((setter primitive-ref) cl 5 val))

(defun class-precedence-list (cl) (primitive-ref cl 6))
(declare-inline class-precedence-list)

(defun (setter class-precedence-list) (cl val)
  ((setter primitive-ref) cl 6 val))

(defun class-abstract? (cl) (primitive-ref cl 7))
(declare-inline class-abstract?)

(defun (setter class-abstract?) (cl val)
  ((setter primitive-ref) cl 7 val))

(defun converter (cl)
  (let ((fun (primitive-ref cl 8)))
    (or (simple-function? fun)
        (simple-generic-function? fun)
        (error () "non-functional converter ~a at class <~a>"
               fun (class-name cl)))))

(defun (setter converter) (cl val)
  ((setter primitive-ref) cl 8 val))

(defun class-code (cl) (primitive-ref cl 9))
(declare-inline class-code)

(defun (setter class-code) (cl val)
  ((setter primitive-ref) cl 9 val))

;;;-----------------------------------------------------------------------------
;;; Shape of <object>
;;;-----------------------------------------------------------------------------
(defconstant *default-slot-value* (lambda () ()))
(defconstant object-size 0)
(defconstant <object>
  (primitive-allocate <simple-class> class-size))
(defconstant object-slots '())   ; to be changed in ()
(defconstant object-keywords '()); to be changed in ()

;;;-----------------------------------------------------------------------------
;;; Shape of <function>
;;;-----------------------------------------------------------------------------
(defconstant function-size 3)
(defconstant <function>
  (primitive-allocate <function-class> class-size))
(defconstant function-slots '(name domain setter))
(defconstant function-slot-defaults
  (list (lambda () 'anonymous)
        *default-slot-value*
        *default-slot-value*))

(defun function-name (gf) (primitive-ref gf 0))
(declare-inline function-name)

(defun (setter function-name) (gf val)
  ((setter primitive-ref) gf 0 val))

(defun function-domain (gf) (primitive-ref gf 1))
(declare-inline function-domain)

(defun (setter function-domain) (gf val)
  ((setter primitive-ref) gf 1 val))

;;;-----------------------------------------------------------------------------
;;; Shape of <simple-function>
;;;-----------------------------------------------------------------------------
(defconstant sf-size 5)
(defconstant <simple-function> (get-global-register lambda-class))
((setter primitive-class-of) <simple-function> <function-class>)
(defconstant sf-direct-slots '(environment code))
(defconstant sf-slots (append function-slots sf-direct-slots))

(defconstant sf-direct-slot-defaults
  (list *default-slot-value* required:))

(defconstant sf-slot-defaults
  (append function-slot-defaults sf-direct-slot-defaults))

(defun simple-function-environment (sf) (primitive-ref sf 3))
(declare-inline simple-function-environment)
(defun (setter simple-function-environment) (sf val)
  ((setter primitive-ref) sf 3 val))

;  (defun simple-function-code (sf) (primitive-ref sf 4))
;  (declare-inline simple-function-code)

;  (defun (setter simple-function-code) (sf val)
;    ((setter primitive-ref) sf 4 val))

(defextern eul_get_lambda_code (ptr) ptr)

(defun simple-function-code (sf) (eul_get_lambda_code sf))
(defextern eul_set_lambda_code (ptr ptr) ptr)

(defun (setter simple-function-code) (sf val) (eul_set_lambda_code sf val))

;;;-----------------------------------------------------------------------------
;;; Shape of <generic-function>
;;;-----------------------------------------------------------------------------
(defconstant gf-size 9)

(defconstant <generic-function>
  (primitive-allocate <function-class> class-size))

(defconstant <simple-generic-function> (get-global-register generic-class))

((setter primitive-class-of) <simple-generic-function> <function-class>)

(defconstant gf-slots
  (append function-slots
          '(method-class method-keywords methods
                         method-lookup-function discriminating-function
                         method-cache)))

(defconstant gf-keywords
  '(name: domain: class: method-class: method-keywords: methods:
          method-lookup-function: discriminating-function:))

(defconstant gf-slot-defaults
  (list (lambda () 'anonymous)
        required:
        *default-slot-value*
        (lambda () (list <simple-method>))
        *default-slot-value*
        *default-slot-value*
        *default-slot-value*
        *default-slot-value*
        *default-slot-value*))

(defun generic-function-domain (gf) (primitive-ref gf 1))
(declare-inline generic-function-domain)

(defun (setter generic-function-domain) (gf val)
  ((setter primitive-ref) gf 1 val))

(defun generic-function-method-class (gf) (primitive-ref gf 3))
(declare-inline generic-function-method-class)

(defun (setter generic-function-method-class) (gf val)
  ((setter primitive-ref) gf 3 val))

(defun generic-function-method-keywords (gf) (primitive-ref gf 4))
(declare-inline generic-function-method-keywords)

(defun (setter generic-function-method-keywords) (gf val)
  ((setter primitive-ref) gf 4 val))

(defun generic-function-methods (gf) (primitive-ref gf 5))
(declare-inline generic-function-methods)

(defun (setter generic-function-methods) (gf val)
  ((setter primitive-ref) gf 5 val))

(defun generic-function-method-lookup-function (gf) (primitive-ref gf 6))
(declare-inline generic-function-method-lookup-function)

(defun (setter generic-function-method-lookup-function) (gf val)
  ((setter primitive-ref) gf 6 val))

(defun generic-function-discriminating-function (gf) (primitive-ref gf 7))
(declare-inline generic-function-discriminating-function)

(defun (setter generic-function-discriminating-function) (gf val)
  ((setter primitive-ref) gf 7 val))

(defun generic-function-method-cache (gf) (primitive-ref gf 8))
(declare-inline generic-function-method-cache)

(defun (setter generic-function-method-cache) (gf val)
  ((setter primitive-ref) gf 8 val))

;;;-----------------------------------------------------------------------------
;;; Shape of <method>
;;;-----------------------------------------------------------------------------
(defconstant method-size 3)

(defconstant <method> (primitive-allocate <simple-class> class-size))

(defconstant <simple-method> (get-global-register method-class))

((setter primitive-class-of) <simple-method> <simple-class>)

(defconstant method-slots '(generic-function domain function))

(defconstant method-keywords '(domain: function: generic-function:))

(defconstant method-slot-defaults
  (list *default-slot-value* required: required:))

(defun method-generic-function (md) (primitive-ref md 0))
(declare-inline method-generic-function)

(defun (setter method-generic-function) (md val)
  ((setter primitive-ref) md 0 val))

(defun method-domain (md) (primitive-ref md 1))
(declare-inline method-domain)

(defun (setter method-domain) (md val)
  ((setter primitive-ref) md 1 val))

(defun method-function (md) (primitive-ref md 2))
(declare-inline method-function)

(defun (setter method-function) (md val)
  ((setter primitive-ref) md 2 val))

;;;-----------------------------------------------------------------------------
;;; Shape of <slot> (also called slot description)

;;;-----------------------------------------------------------------------------
(defconstant sd-size 2)
(defconstant <slot> (primitive-allocate <simple-class> class-size))
(defconstant sd-slots '(reader writer))
(defconstant sd-keywords '(reader: writer:))

(defconstant sd-slot-defaults
  (list *default-slot-value* *default-slot-value*))

(defun slot-reader (sd) (primitive-ref sd 0))
(declare-inline slot-reader)

(defun (setter slot-reader) (sd val)
  ((setter primitive-ref) sd 0 val))

(defun slot-writer (sd) (primitive-ref sd 1))
(declare-inline slot-writer)

(defun (setter slot-writer) (sd val)
  ((setter primitive-ref) sd 1 val))

;;;-----------------------------------------------------------------------------
;;; Shape of <local-slot> (also called local slot description)

;;;-----------------------------------------------------------------------------
(defconstant lsd-size 6)
(defconstant <local-slot> (primitive-allocate <simple-class> class-size))
(defconstant lsd-slots (append sd-slots '(name keyword default required?)))

(defconstant lsd-keywords
  (append sd-keywords '(name: keyword: default: required?:)))

(defconstant lsd-slot-defaults
  (append sd-slot-defaults
          (list required:
                *default-slot-value*
                *default-slot-value*
                *default-slot-value*)))

(defun slot-name (sd) (primitive-ref sd 2))
(declare-inline slot-name)

(defun (setter slot-name) (sd val)
  ((setter primitive-ref) sd 2 val))

(defun slot-keyword (sd) (primitive-ref sd 3))
(declare-inline slot-keyword)

(defun (setter slot-keyword) (sd val)
  ((setter primitive-ref) sd 3 val))

(defun slot-default (sd) (primitive-ref sd 4))
(declare-inline slot-default)

(defun (setter slot-default) (sd val)
  ((setter primitive-ref) sd 4 val))

(defun slot-required? (sd) (primitive-ref sd 5))
(declare-inline slot-required?)

(defun (setter slot-required?) (sd val)
  ((setter primitive-ref) sd 5 val))

;;;-----------------------------------------------------------------------------
;;; Shape of <name>
;;;-----------------------------------------------------------------------------
(defconstant name-size 1)
(defconstant <name> (primitive-allocate <simple-class> class-size))
(defconstant <symbol> (get-global-register symbol-class))

((setter primitive-class-of) <symbol> <simple-class>)

(defconstant <keyword> (get-global-register keyword-class))

((setter primitive-class-of) <keyword> <simple-class>)

(defconstant name-slots '(name))
(defconstant name-slot-defaults '(required:))
(defconstant name-keywords '(name:))

(defun symbol-name (sym) (primitive-ref sym 0))
(declare-inline symbol-name)

(defun keyword-name (key) (primitive-ref key 0))
(declare-inline keyword-name)

;;  Now defined as generic function
;;  (defun name (key) (primitive-ref key 0))
;;  (declare-inline keyword-name)

;;;-----------------------------------------------------------------------------
;;; Shape of <list>, <cons> and <null>
;;;-----------------------------------------------------------------------------
(defconstant cons-size 2)
(defconstant cons-slots '(car cdr))

(defconstant cons-slot-defaults
  (list *default-slot-value* *default-slot-value*))

(defconstant cons-keywords '(car: cdr:))
(defconstant <list> (primitive-allocate <simple-class> class-size))
(defconstant <cons> (get-global-register cons-class))

((setter primitive-class-of) <cons> <simple-class>)

(defconstant <null> (get-global-register null-class))

((setter primitive-class-of) <null> <simple-class>)

;;;-----------------------------------------------------------------------------
)  ;; End of module
;;;-----------------------------------------------------------------------------
