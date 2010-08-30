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
;;;  Title:
;;;  Description:
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Horst Friedrich
;;;-----------------------------------------------------------------------------

(defmodule standard-generic-function


  (import ( tail
            function ; <function>
            basic-list
;;; printf-1 ; for debug
            basic-compare ; eq
            tail-introspection ; %class-of, %member
;;;string ; for error messages
;;; symbol
;;; vector
            (rename ((no-applicable-method-error no-applicable-method)) basic-condition)
            )

   ;; syntax
   syntax (apply-level-1 basic-syntax))

;;;-----------------------------------------------------------------------------
;;;  T E S T
;;;-----------------------------------------------------------------------------
;; (%std-discrfun-1 () ())
;; (%std-discrfun-2 () () ())
;; (%std-discrfun-n () () () ())
;;;-----------------------------------------------------------------------------
;;already defined in basic-condition

;;(defun no-applicable-method (gf class-list . args)
;;  "no applicable-method for gf class-list"
;;  (error "no applicable method" <no-applicable-method> gf class-list args)
;;)

(%define-function (class-precedence-list <cons>)
  ((cl <class>))
  (%select cl <class> class-precedence-list))

(%define-function (%assq <object>)
  ((ele <object>)
   (li <list>))
  (if li
      (if (%eq (car (car li)) ele) (car li)
        (%assq ele (cdr li)))
    ()))

(%define-function (%equal-list <object>)
  ((l1 <list>)
   (l2 <list>))
  (if l1
      (if (%eq (car l1) (car l2))
          (%equal-list (cdr l1) (cdr l2))
        ())
    t))


;;;-----------------------------------------------------------------------------
(%define-standard-class (<generic-function> <class>) ; !!! <function-class>)
  <function>
  ;; inherited from <function> are:
  ;;                        ((closure-vars type <list>
  ;;                                       default ()
  ;;                                       accessor function-closure-vars)
  ;;                         (address type %function
  ;;                                  accessor function-address)
  ;;                         (arg-descr type %signed-word-integer
  ;;                                    accessor function-arg-descr))
  ;; (defun foo () ..) --> arg-descr = 0
  ;; (defun foo (x) ..) --> arg-descr = 1
  ;; (defun foo x ..) --> arg-descr = -1
  ;; (defun foo (x y) ..) --> arg-descr = 2
  ;; (defun foo (x . y) ..) --> arg-descr = -2 ...
  ;;                        keywords (closure-vars arg-descr address)
  ;;                        constructor (make-function arg-descr closure-vars address)

  (
   ;;                         (domain type <list>
   ;;                                 accessor generic-function-domain)
   ;;                         (method-class type <class>
   ;;                                       accessor generic-function-method-class)
   (methods type <list>
            writer set-generic-function-methods
            accessor generic-function-methods)
   ;;                         (method-lookup-function
   ;;                          type <function>
   ;;                          accessor generic-function-lookup-function)
   ;;                         (discriminating-function
   ;;                          type <function>
   ;;                          accessor generic-function-discriminating-function)
   ;; optimization for method-lookup
   (discrimination-depth type %signed-word-integer
                         default #%i2
                         accessor generic-discrimination-depth)
   (fast-class-cache
    default '(()) ; for (car fast-class-cache)
    ;; and (cdr fast-class-cache)
    accessor generic-fast-class-cache
    writer set-generic-fast-class-cache)
   (fast-method-cache
    accessor generic-fast-method-cache
    writer set-generic-fast-method-cache)
   (slow-cache type <list>
               default (list ())
               accessor generic-slow-cache
               writer set-generic-slow-cache)
   )
  representation pointer-to-struct
  allocation single-card
  )

(%define-standard-class (<standard-generic-function> <class>)
  <generic-function>
  ()
  )

(%define-standard-class (<method> <class>)
  <object>
  ((domain type <list>
           accessor method-domain)
   (function-address type %function
                     accessor method-function-address)
   (function type <function>
             accessor method-function)
   (generic-function type <generic-function>
                     accessor method-generic-function))
  representation pointer-to-struct
  allocation single-card
  )


(%define-literal-expansion generic-function
  ;; argument-descriptor (integer)
  ;; function-pointer (%function)
  ;; setter (function)
  ;; name (string)
  ;; methods (list)
  ;; discrimination-depth (integer)
  `(%literal ,<standard-generic-function>
             name (%literal ,%string () ,name)
             closure-vars ()
             address ,function-pointer
             arg-descr (%literal ,%signed-word-integer ,argument-descriptor)
             methods ,methods
             discrimination-depth (%literal ,%signed-word-integer ,discrimination-depth)
             fast-class-cache (())
             slow-cache ,(cons () ())
             setter ,(if setter setter error-no-setter-defined)
             ))

(%define-literal-expansion method
  `(%literal ,<method>
             domain ,domain
             function ,function
             function-address ,function-pointer
             generic-function ,generic-function))

;;;-----------------------------------------------------------------------------
;;; E R R O R S
;;;-----------------------------------------------------------------------------

;;(setq *no-applicable-method-error* (lambda (gf .args)
;;                                (%error (function-name
;;                                        (%cast <standard-generic-function> gf))
;;                                        (%literal %string ()
;;                                                  "<no-applicalble-method>"))
;;                    ;;make type-inference happy
;;                                        gf))

;; standard-discriminating functions for
;; many methods without closure-vars

(%define-function (%std-discrfun-1 %function)
  ((gf <standard-generic-function>) (arg <object>))
  (%let ((class-of-arg <class> (%class-of arg)))
        (if (%eq class-of-arg (generic-fast-class-cache gf))
            (%cast %function (generic-fast-method-cache gf))
          (special-lookup-fn-1 gf class-of-arg arg))
        ))

(%annotate-function %std-discrfun-1 is-standard-discriminator () )

(%define-function (special-lookup-fn-1 %function)
  ((gf <standard-generic-function>)
   (class <class>)
   (arg <object>))
  (let ((entry (%assq class (generic-slow-cache gf))))
    (if entry
        (let ((method (cdr entry)))
          (set-generic-fast-class-cache gf class)
          (set-generic-fast-method-cache gf method)
          (%cast %function method))
      (let ((method (special-lookup-fn-1-aux
                     (generic-function-methods gf)
                     (class-precedence-list class)
                     <object> ())))
        (if method
            ;; applicable method found
            (%let ((fun %function
                        (function-address
                         (method-function (%cast <method> method)))))
                  (set-generic-fast-class-cache gf class)
                  (set-generic-fast-method-cache gf
                                                 (%cast <object> fun)) ; *TI*
                  (set-generic-slow-cache gf
                                          (cons (cons class
                                                      (%cast <object> fun)) (generic-slow-cache gf)))
                  fun)
          (progn
            (no-applicable-method gf (list class) arg)
            (function-address
             (lambda (a) ())) ; default-function, returned ()
            ))))))

(%define-function (special-lookup-fn-1-aux <object>)
  ((methods <list>)
   (cpl <list>)
   (class <class>)
   (method <object>))
  (if (null? methods) method
    (%let* ((dclass <class>
                    (car (method-domain (%cast <method> (car methods))))))
           (if (%member dclass cpl) ; method is applicable
               (if (%member class (class-precedence-list dclass))
                   ;; new method is more specific
                   (special-lookup-fn-1-aux (cdr methods) cpl dclass
                                            (car methods))
                 (special-lookup-fn-1-aux (cdr methods) cpl class
                                          method))
             (special-lookup-fn-1-aux (cdr methods) cpl class
                                      method)))))

(%define-function (%std-discrfun-2 %function)
  ((gf <standard-generic-function>)
   (arg1 <object>)
   (arg2 <object>))
  (%let ((class-of-arg1 <class> (%class-of arg1))
         (class-of-arg2 <class> (%class-of arg2))
         (class-cache <cons> (generic-fast-class-cache gf)))
        (if (%eq class-of-arg1 (car class-cache))
            (if (%eq class-of-arg2 (cdr class-cache))
                (%cast %function (generic-fast-method-cache gf))
              (special-lookup-fn-2 gf class-of-arg1 class-of-arg2
                                   arg1 arg2))
          (special-lookup-fn-2 gf class-of-arg1 class-of-arg2
                               arg1 arg2))
        ))

(%annotate-function %std-discrfun-2 is-standard-discriminator () )

(%define-function (special-lookup-fn-2 %function)
  ((gf <standard-generic-function>)
   (class1 <class>)
   (class2 <class>)
   (arg1 <object>)
   (arg2 <object>))
  (let ((entry (%assq class1 (generic-slow-cache gf))))
    (if entry
        (let ((entry2 (%assq class2 (cdr entry))))
          (if entry2
              (let* ((cl-method (cdr entry2))
                     (method (cdr cl-method)))
                (set-generic-fast-class-cache gf (car cl-method))
                (set-generic-fast-method-cache gf method)
                (%cast %function method))
            (let ((method (special-lookup-fn-2-aux
                           (generic-function-methods gf)
                           (class-precedence-list class1)
                           (class-precedence-list class2)
                           <object> <object> ())))
              (if method
                  ;; applicable method found
                  (%let ((cl-lst <cons> (cons class1 class2))
                         (fun %function
                              (function-address
                               (method-function (%cast <method> method)))))
                        (set-generic-fast-class-cache gf cl-lst)
                        (set-generic-fast-method-cache gf
                                                       (%cast <object> fun)) ; *ti*
                        ((setter cdr) entry
                         (cons (cons class2 (cons cl-lst
                                                  (%cast <object> fun) ; *ti*
                                                  )) (cdr entry)))
                        fun)
                (progn
                  (no-applicable-method gf (list class1 class2) arg1 arg2)
                  (function-address
                   (lambda (a b) a)) ; default-function, returned a
                  )))))
      (let ((method (special-lookup-fn-2-aux
                     (generic-function-methods gf)
                     (class-precedence-list class1)
                     (class-precedence-list class2)
                     <object> <object> ())))
        (if method
            ;; applicable method found
            (%let ((cl-lst <cons> (cons class1 class2))
                   (fun %function
                        (function-address
                         (method-function (%cast <method> method)))))
                  (set-generic-fast-class-cache gf cl-lst)
                  (set-generic-fast-method-cache gf (%cast <object> fun)) ; *ti*
                  (set-generic-slow-cache gf
                                          (cons (list class1 (cons class2 (cons cl-lst
                                                                                (%cast <object> fun)))) ; *ti*
                                                (generic-slow-cache gf)))
                  fun)
          (progn
            (no-applicable-method gf (list class1 class2) arg1 arg2)
            (function-address
             (lambda (a b) a)) ; default-function, returned a
            ))))))

(%define-function (special-lookup-fn-2-aux <object>)
  ((methods <list>)
   (cpl1 <list>)
   (cpl2 <list>)
   (class1 <class>)
   (class2 <class>)
   (method <object>))
  (if (null? methods) method
    (%let* ((mdomain <cons> (method-domain (%cast <method> (car methods))))
            (dclass1 <class> (car mdomain))
            (dclass2 <class> (car (cdr mdomain))))
           (if (%member dclass1 cpl1)
               (if (%member dclass2 cpl2)
                   ;; method is applicable
                   (let ((more-specific
                          (if (%eq dclass1 class1)
                              (if (%member class2 (class-precedence-list dclass2))
                                  t ())
                            (if (%member class1 (class-precedence-list dclass1))
                                t ()))))
                     (if more-specific
                         (special-lookup-fn-2-aux (cdr methods)
                                                  cpl1 cpl2
                                                  dclass1 dclass2
                                                  (car methods))
                       (special-lookup-fn-2-aux (cdr methods)
                                                cpl1 cpl2
                                                class1 class2
                                                method)))
                 (special-lookup-fn-2-aux (cdr methods)
                                          cpl1 cpl2
                                          class1 class2
                                          method))
             (special-lookup-fn-2-aux (cdr methods)
                                      cpl1 cpl2
                                      class1 class2
                                      method)))))

(%define-function (%std-discrfun-n %function)
  ((gf <standard-generic-function>)
   (arg1 <object>)
   (arg2 <object>)
   . args)
  ;;                   (args <list>)) ; to test
  ;; result is a %function - object, that must be called with a %funcall
  ;; the standard-discriminating-function for a generic function with more than
  ;; two arguments must be generated as following example:
  ;; (defgeneric gf (x y z) ...)
  ;; => (defun gf (x y z)
  ;;      (%funcall (%std-discrfun-n gf x y z) x y z))
  ;; or
  ;; (defgeneric gf (x y . z) ... )
  ;; => (defun gf (x y . z)
  ;;      (%funcall (%std-discrfun-n gf x y z) x y z))
  ;; discrimination-depth (dt) = number (%signed-word-integer)
  ;; fast-class-cache = (/class-1/ /class-2/ ... /class-dt/)
  ;; fast-method-cache = normal function (<function>)
  ;; slow-cache = (for depth = 3)
  ;;     ((/class-arg1-1/ . ((cl-2-1 . ((cl-3-1 class-list1 . function1)
  ;;                                    (cl-3-2 class-list2 . function2)))
  ;;                         (cl-2-2 . ...)))
  (%let* ((class-of-arg1 <class> (%class-of arg1))
          (class-of-arg2 <class> (%class-of arg2))
          (discrimination-depth
           %signed-word-integer (generic-discrimination-depth gf))
          (classes-of-args <list>
                           (if (%gt discrimination-depth #%i2)
                               (compute-class-list (%minus discrimination-depth #%i2) args)
                             ()))
          (class-cache <cons> (generic-fast-class-cache gf)))
         ;; result is a <function>
         (if (%eq class-of-arg1 (car class-cache))
             (if (%eq class-of-arg2 (car (cdr class-cache)))
                 (if (%equal-list (cdr (cdr class-cache)) classes-of-args)
                     (function-address
                      (%cast <function> (generic-fast-method-cache gf)))
                   (special-lookup-fn-n gf class-of-arg1 class-of-arg2
                                        classes-of-args
                                        arg1 arg2 args))
               (special-lookup-fn-n gf class-of-arg1 class-of-arg2
                                    classes-of-args
                                    arg1 arg2 args))
           (special-lookup-fn-n gf class-of-arg1 class-of-arg2
                                classes-of-args
                                arg1 arg2 args)))
  )


(%annotate-function %std-discrfun-n is-standard-discriminator () )

(%define-function (compute-class-list <list>)
  ((depth %signed-word-integer)
   (args <list>))
  ;; (printf-1 (%literal %string () "\n depth %d \n") depth)
  (if (%eq depth #%i0) ()
    (cons (%class-of (car args))
          (compute-class-list (%minus depth #%i1)
                              (cdr args)))))

(%define-function (special-lookup-fn-n %function)
  ((gf <standard-generic-function>)
   (class1 <class>)
   (class2 <class>)
   (cl-lst <list>)
   (arg1 <object>)
   (arg2 <object>)
   (args <list>))
  (let ((entry (%assq class1 (cdr (generic-slow-cache gf)))))
    (if entry
        (let ((entry2 (%assq class2 (cdr entry))))
          (if entry2
              (if cl-lst
                  (let ((cl-method (walk-cache gf
                                               (cons class1
                                                     (cons class2 cl-lst))
                                               cl-lst entry2)))
                    (if cl-method
                        (let ((method (cdr cl-method)))
                          (set-generic-fast-class-cache gf (car cl-method))
                          (set-generic-fast-method-cache gf method)
                          (function-address
                           (%cast <function> method)))
                      (progn
                        (no-applicable-method gf (cons class1
                                                       (cons class2
                                                             cl-lst))
                                              arg1 arg2 args)
                        (function-address (lambda (a b . c) ())))))
                ;; cl-lst = () - result in entry2
                (let* ((cl-method (cdr entry2))
                       (method (cdr cl-method)))
                  (set-generic-fast-class-cache gf (car cl-method))
                  (set-generic-fast-method-cache gf method)
                  (function-address
                   (%cast <function> method))))
            ;; slow-cache failed after entry
            (let* ((cl-lst1 (cons class2 cl-lst))
                   (cl-method
                    (walk-cache gf (cons class1 cl-lst1) cl-lst1 entry)))
              (if cl-method
                  (let ((method (cdr cl-method)))
                    (set-generic-fast-class-cache gf (car cl-method))
                    (set-generic-fast-method-cache gf method)
                    (function-address
                     (%cast <function> method)))
                (progn
                  (no-applicable-method gf (cons class1
                                                 (cons class2
                                                       cl-lst))
                                        arg1 arg2 args)
                  (function-address
                   (lambda (a b . c) ())))))))
      ;; slow-cache faild
      (let* ((cl-lst2 (cons class1 (cons class2 cl-lst)))
             (cl-method
              (walk-cache gf cl-lst2
                          cl-lst2 (generic-slow-cache gf))))
        (if cl-method
            (let ((method (cdr cl-method)))
              (set-generic-fast-class-cache gf (car cl-method))
              (set-generic-fast-method-cache gf method)
              (function-address
               (%cast <function> method)))
          (progn
            (no-applicable-method gf cl-lst2
                                  arg1 arg2 args)
            (function-address
             (lambda (a b . c) ())))))))
  )

(%define-function (walk-cache <list>)
  ((gf <generic-function>)
   (cl-lst <list>)
   (walk-lst <list>)
   (cache <list>))
  ;; result (class-list . <function>-object) or ()
  (let ((entry (%assq (car walk-lst) (cdr cache))))
    (if entry
        (if (cdr walk-lst)
            (walk-cache gf cl-lst (cdr walk-lst) entry)
          ;; method found in slow-cache
          (cdr entry)) ; (class-list . <function>-object)
      ;; missing entry in slow-cache
      (let* ((appl-method (special-lookup-fn-n-aux
                           (generic-function-methods gf)
                           cl-lst)))
        (if appl-method
            (let ((cl-method-fun (cons cl-lst
                                       (method-function
                                        (%cast <method> appl-method)))))
              ;; fill the slow-cache
              ((setter cdr) cache
               (cons (compute-subcache walk-lst cl-method-fun)
                     (cdr cache)))
              ;; return (class-list . <function>-object)
              cl-method-fun)
          ())) ; no applicable method
      )))

(defun compute-subcache (class-lst item)
  (if (cdr class-lst)
      (list (car class-lst)
            (compute-subcache (cdr class-lst) item))
    (cons (car class-lst) item)))

(defun special-lookup-fn-n-aux
  (walk-methods arg-classes)
  ;; search first applicable method
  (if walk-methods
      (%let* ((cur-meth <method> (car walk-methods))
              (cur-domain <cons> (method-domain cur-meth)))
             (if (domain-of-applicable-method-p cur-domain
                                                arg-classes)
                 (special-lookup-fn-n-aux1
                  (cdr walk-methods) arg-classes cur-domain
                  cur-meth)
               (special-lookup-fn-n-aux (cdr walk-methods) arg-classes)))
    ()))

(defun special-lookup-fn-n-aux1
  (walk-methods arg-classes domain method)
  (if walk-methods
      (%let* ((cur-meth <method> (car walk-methods))
              (cur-domain <cons> (method-domain cur-meth)))
             (if (domain-of-applicable-method-p cur-domain
                                                arg-classes)
                 (if (more-specific-domain cur-domain domain)
                     (special-lookup-fn-n-aux1 (cdr walk-methods)
                                               arg-classes
                                               cur-domain
                                               cur-meth)
                   (special-lookup-fn-n-aux1 (cdr walk-methods)
                                             arg-classes
                                             domain
                                             method))
               (special-lookup-fn-n-aux1 (cdr walk-methods)
                                         arg-classes
                                         domain
                                         method)))
    method))

(defun domain-of-applicable-method-p (m-dom arg-dom)
  (if (null? arg-dom) t
    (if (%member (car m-dom)
                 (class-precedence-list (%cast <class> (car arg-dom))))
        (domain-of-applicable-method-p (cdr m-dom) (cdr arg-dom))
      ())))

(defun more-specific-domain (m-dom1 m-dom2)
  (%let ((cl1 <class> (car m-dom1))
         (cl2 <class> (car m-dom2)))
        (if (%eq cl1 cl2)
            (more-specific-domain (cdr m-dom1) (cdr m-dom2))
          (if (%member cl2
                       (class-precedence-list cl1))
              t ()))))


(defun add-method (gf method)
  ;; first the caches have to be cleared
  (set-generic-fast-class-cache gf (list ()))
  (set-generic-fast-method-cache gf ())
  (set-generic-slow-cache gf (list ()))
  ;; then the method may be added
  (set-generic-function-methods
   gf
   (cons method (generic-function-methods gf)))
  )

(%annotate-function add-method is-special-function add-method)

)
