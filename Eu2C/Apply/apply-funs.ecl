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
;;;  Title: Providing LZS-Objects definied in the Application Module APPLY
;;;  Description:
;;    The function set-apply-funs sets some variables to LZS-objects extracted from
;;    the lexical environment of the module APPLY. The variables are all exported. The
;;    function set-apply-funs can be called after! the module apply was loaded.
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Ingo Mohr
;;;-----------------------------------------------------------------------------

#module apply-funs

(import (eulisp0
         el2lzs
         el2lzs-error
         lzs accessors
         tail-module
         (only (stable-sort find)
               common-lisp))
 syntax (eulisp0
         (only (case push)
               common-lisp))
 export (set-apply-objects
         set-apply-level-1-objects
         set-apply-level-2-objects
         apply-environment
         set-special-function
         set-special-class
         set-special-binding
         *special-functions*
         *special-classes*
         *special-bindings*)

;;; ---------------------------
;;; apply-level-1-objects begin

 export (trace-pair
         trace-pointer
         trace-nothing
         trace-general-object
         make-type-descriptor
         make-card-descriptor
         set-type-descriptor
         set-card-descriptor
         set-class-mm-type
         set-class-mm-card
         class-mm-type
         class-mm-card
         allocate-on-single-card
         allocate-on-multiple-type-card
         allocate-on-multiple-size-card

         $mtss
         $stms
         $stss

         max-used-card-descriptor
         max-used-type-descriptor

         <pointer-to-void>
         )
;;; apply-level-1-objects end
;;; -------------------------
;;; apply-level-2-objects begin

 export (%instance-of-p
         %class-of

         %vector-class-instance-size

         %cons %list <null>-class <function>-class <fpi>-class
         nullfun
         eqfun
         test-functions
         no-applicable-method-error
         %call-next-method
         %next-method-p
         typecheck
         )
;;; apply-level-2-objects end
;;; -------------------------

 export (
         ;; for closures
         %closure-push ; (var closure-or-())
         %closure-value ; (closure pos-fixnum)
         %set-closure-value ; (closure pos-fixnum value)
         %make-function ; (closure function)
         ;; for function-call
         %apply ; (fun . args)
         %apply1 ; (fun args)
         %apply2 ; (fun arg1 args)
         %apply3 ; (fun arg1 arg2 args)
         %apply4 ; (fun arg1 arg2 arg3 args)
         %apply5 ; (fun arg1 arg2 arg3 arg4 args)
         %apply6 ; (fun arg1 arg2 arg3 arg4 arg5 args)
         %apply7 ; (fun arg1 arg2 arg3 arg4 arg5 arg6 args)
         %apply8 ; (fun arg1 arg2 arg3 arg4 arg5 arg6 arg7 args)

         %funcall0 ; (fun)
         %funcall1 ; (fun arg1)
         %funcall2 ; (fun arg1 arg2)
         %funcall3 ; (fun arg1 arg2 arg3)
         %funcall4 ; (fun arg1 arg2 arg3 arg4)
         %funcall5 ; (fun arg1 arg2 arg3 arg4 arg5)
         %funcall6 ; (fun arg1 arg2 arg3 arg4 arg5 arg6)
         %funcall7 ; (fun arg1 arg2 arg3 arg4 arg5 arg6 arg7)
         %funcall8 ; (fun arg1 arg2 arg3 arg4 arg5 arg6 arg7)

         ;;let/cc, dynamic, unwind-protect
         %unwind
         %stop-unwind-before
         %continue-at
         %unwind-continue
         %letcc-result
         %dynamic
         %top-dynamic
         %get-dynamic
         %set-dynamic
         %make-dynamic
         %initialize-global-dynamic

         ;; for symbol table initialization
         %add-symbol
         %symtab-initfun-var

         ;; dynamic method table extension
         %add-method

         ;; subclassp, not yet needed directly by the compiler, it is only used
         ;; in the macro defcondition
         %subclassp
         )

 export (provide-compiler-info)
;;; apply objects end
;;; -----------------
 )

;;;-----------------------------------------------------------------------------
;;; apply-level-1-objects begin
;;;-----------------------------------------------------------------------------

(deflocal trace-pair ())
(deflocal trace-pointer ())
(deflocal trace-nothing ())
(deflocal trace-general-object ())
(deflocal make-type-descriptor ())
(deflocal make-card-descriptor ())
(deflocal set-type-descriptor ())
(deflocal set-card-descriptor ())
(deflocal set-class-mm-type ())
(deflocal set-class-mm-card ())
(deflocal class-mm-type ())
(deflocal class-mm-card ())
(deflocal allocate-on-single-card ())
(deflocal allocate-on-multiple-type-card ())
(deflocal allocate-on-multiple-size-card ())
(deflocal $mtss ())
(deflocal $stms ())
(deflocal $stss ())

(deflocal max-used-card-descriptor ())
(deflocal max-used-type-descriptor ())

(deflocal <pointer-to-void> ())

;;;-----------------------------------------------------------------------------
;;; apply-level-1-objects end
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;; apply-level-2-objects begin
;;;-----------------------------------------------------------------------------

(deflocal %instance-of-p ())
(deflocal %class-of ())
(deflocal %vector-class-instance-size ())
(deflocal %list ())
(deflocal %cons ())
(deflocal <null>-class ())
(deflocal <function>-class ())
(deflocal <fpi>-class ())
(deflocal nullfun ())
(deflocal eqfun ())
(deflocal test-functions ())
(deflocal no-applicable-method-error ())
(deflocal %call-next-method ())
(deflocal %next-method-p ())
(deflocal typecheck ())

;;;-----------------------------------------------------------------------------
;;; apply-level-2-objects end
;;;-----------------------------------------------------------------------------

;; for closures
(deflocal %closure-push ())
(deflocal %closure-value ())
(deflocal %set-closure-value ())
(deflocal %make-function ())

;; for function-call
(deflocal   %apply  ()) ; (fun . args)
(deflocal   %apply1 ()) ; (fun args)
(deflocal   %apply2 ()) ; (fun arg1 args)
(deflocal   %apply3 ()) ; (fun arg1 arg2 args)
(deflocal   %apply4 ()) ; (fun arg1 arg2 arg3 args)
(deflocal   %apply5 ()) ; (fun arg1 arg2 arg3 arg4 args)
(deflocal   %apply6 ()) ; (fun arg1 arg2 arg3 arg4 arg5 args)
(deflocal   %apply7 ()) ; (fun arg1 arg2 arg3 arg4 arg5 arg6 args)
(deflocal   %apply8 ()) ; (fun arg1 arg2 arg3 arg4 arg5 arg6 arg7 args)

(deflocal   %funcall0 ()) ; (fun)
(deflocal   %funcall1 ()) ; (fun arg1)
(deflocal   %funcall2 ()) ; (fun arg1 arg2)
(deflocal   %funcall3 ()) ; (fun arg1 arg2 arg3)
(deflocal   %funcall4 ()) ; (fun arg1 arg2 arg3 arg4)
(deflocal   %funcall5 ()) ; (fun arg1 arg2 arg3 arg4 arg5)
(deflocal   %funcall6 ()) ; (fun arg1 arg2 arg3 arg4 arg5 arg6)
(deflocal   %funcall7 ()) ; (fun arg1 arg2 arg3 arg4 arg5 arg6 arg7)
(deflocal   %funcall8 ()) ; (fun arg1 arg2 arg3 arg4 arg5 arg6 arg7)

;;let/cc, dynamic, unwind-protect
(deflocal %unwind ())
(deflocal %stop-unwind-before ())
(deflocal %continue-at ())
(deflocal %unwind-continue ())
(deflocal %letcc-result ())
(deflocal %dynamic ())
(deflocal %top-dynamic ())
(deflocal %get-dynamic ())
(deflocal %set-dynamic ())
(deflocal %make-dynamic ())
(deflocal %initialize-global-dynamic ())

;; for symbol table initialization
(deflocal %add-symbol ())
(deflocal %symtab-initfun-var ())

;; dynamic method table extension
(deflocal %add-method ())

(deflocal %subclassp ())

;;;-----------------------------------------------------------------------------
;;; apply objects end
;;;-----------------------------------------------------------------------------

(deflocal apply-environment ())

(defun set-apply-objects (apply-module)
  (setq apply-environment (?lex-env apply-module))
  (setq %apply  (get-apply-function 0))
  (setq %apply1 (get-apply-function 1))
  (setq %apply2 (get-apply-function 2))
  (setq %apply3 (get-apply-function 3))
  (setq %apply4 (get-apply-function 4))
  (setq %apply5 (get-apply-function 5))
  (setq %apply6 (get-apply-function 6))
  (setq %apply7 (get-apply-function 7))
  (setq %apply8 (get-apply-function 8))

  (setq %funcall0 (get-funcall-function 0))
  (setq %funcall1 (get-funcall-function 1))
  (setq %funcall2 (get-funcall-function 2))
  (setq %funcall3 (get-funcall-function 3))
  (setq %funcall4 (get-funcall-function 4))
  (setq %funcall5 (get-funcall-function 5))
  (setq %funcall6 (get-funcall-function 6))
  (setq %funcall7 (get-funcall-function 7))
  (setq %funcall8 (get-funcall-function 8))
  )

(defun set-apply-level-1-objects ()
  )

(defun set-apply-level-2-objects ()
  (setq test-functions (list %neq %eq %gt %lt %ge %le))
  )
;;;-----------------------------------------------------------------------------
;;; hadling apply- and funcall-functions
;;;-----------------------------------------------------------------------------
;;; The following looks a bit complicated if the retrieval is done as above.
;;; However, the implementation is prepared for a more flexible use by following
;;; compiler passes.

(deflocal *apply-functions* ())
(deflocal *funcall-functions* ())

(defun add-to-apply/funcall-table (fun table)
  (stable-sort (cons fun table)
               #'more-required-arguments-p))

(defun more-required-arguments-p (fun1 fun2)
  (> (length (?var-list (?params fun1)))
     (length (?var-list (?params fun2)))))

(defun get-from-apply/funcall-table (number-of-args table)
  ;; requires that the items in table are sorted with decreasing number of
  ;; required arguments
  (find number-of-args table
        :test #'>=
        :key #'funcall-argnum))

(defun funcall-argnum (fun)
  (- (length (?var-list (?params fun))) 1))

(defun add-apply-function (fun)
  (setq *apply-functions*
        (add-to-apply/funcall-table fun *apply-functions*)))

(defun add-funcall-function (fun)
  (setq *funcall-functions*
        (add-to-apply/funcall-table fun *funcall-functions*)))

(defun get-apply-function (number-of-args)
  (get-from-apply/funcall-table number-of-args *apply-functions*))

(defun get-funcall-function (number-of-args)
  (get-from-apply/funcall-table number-of-args *funcall-functions*))

;;;-----------------------------------------------------------------------------
;;; set-special-function: handles keyword is-special-function of %annotate-function
;;;-----------------------------------------------------------------------------

(deflocal *special-functions* ())

(defun set-special-function (function keyword type)
  ;; keyword is 'is-special-function'
  (push function *special-functions*)
  (case type
        ;;apply-objects
        ;; for closures
        (es::closure-push (setq %closure-push function))
        (es::closure-value (setq %closure-value function))
        (es::set-closure-value (setq %set-closure-value function))
        (es::make-function (setq %make-function function))
        ;; for function-call
        (es::apply (add-apply-function function))
        (es::funcall (add-funcall-function function))

        (es::unwind-continue (setq %unwind-continue function))
        (es::get-dynamic (setq %get-dynamic function))
        (es::set-dynamic (setq %set-dynamic function))
        (es::make-dynamic (setq %make-dynamic function))
        (es::initialize-global-dynamic (setq %initialize-global-dynamic function))

        ;;apply-level-1-objects
        (es::trace-pair (setq trace-pair function))
        (es::trace-pointer (setq trace-pointer function))
        (es::trace-nothing (setq trace-nothing function))
        (es::trace-general-object (setq trace-general-object function))
        (es::make-type-descriptor (setq make-type-descriptor function))
        (es::make-card-descriptor (setq make-card-descriptor function))
        (es::set-type-descriptor (setq set-type-descriptor function))
        (es::set-card-descriptor (setq set-card-descriptor function))
        (es::set-class-mm-type (setq set-class-mm-type function))
        (es::set-class-mm-card (setq set-class-mm-card function))
        (es::class-mm-type (setq class-mm-type function))
        (es::class-mm-card (setq class-mm-card function))
        (es::allocate-on-single-card (setq allocate-on-single-card function))
        (es::allocate-on-multiple-type-card (setq allocate-on-multiple-type-card function))
        (es::allocate-on-multiple-size-card (setq allocate-on-multiple-size-card function))

        ;;apply-level-2-objects
        (es::instance-of-p (setq %instance-of-p function))
        (es::class-of (setq %class-of function))
        (es::vector-class-instance-size (setq %vector-class-instance-size function))
        (es::cons (setq %cons function))
        (es::null (setq nullfun function))
        (es::eq (setq eqfun function))
        (es::no-applicable-method-error (setq no-applicable-method-error function))
        (es::call-next-method (setq %call-next-method function))
        (es::next-method-p (setq %next-method-p function))
        (es::typecheck (setq typecheck function))

        ;;others
        (es::add-symbol (setq %add-symbol function))
        (es::add-method (setq %add-method function))
        (es::subclassp (setq %subclassp function))
        (t (error-invalid-special-specification keyword type))))

;;;-----------------------------------------------------------------------------
;;; set-special-class: handles keyword is-special-class of %annotate-class
;;;-----------------------------------------------------------------------------

(deflocal *special-classes* ())

(defun set-special-class (class keyword type)
  ;; keyword is 'is-special-class'
  (push class *special-classes*)
  (case type
        (es::<dynamic> (setq %dynamic class))
        (es::<pointer-to-void> (setq <pointer-to-void> class))
        (es::<list> (setq %list class))
        (es::<null> (setq <null>-class class))
        (es::<fpi> (setq <fpi>-class class))
        (es::<function> (setq <function>-class class))
        (t (error-invalid-special-specification keyword type))))


;;;-----------------------------------------------------------------------------
;;; set-special-binding: handles keyword is-special-binding of %annotate-binding
;;;-----------------------------------------------------------------------------

(deflocal *special-bindings* ())

(defun set-special-binding (binding keyword type)
  ;; keyword is 'is-special-binding'
  (push binding *special-bindings*)
  (case type
        ;;for mm-initialize
        (es::mtss (setq $mtss binding))
        (es::stms (setq $stms binding))
        (es::stss (setq $stss binding))
        ;;to implement let/cc,unwind-protect & co.
        (es::unwind (setq %unwind binding))
        (es::stop-unwind-before (setq %stop-unwind-before binding))
        (es::continue-at (setq %continue-at binding))
        (es::letcc-result (setq %letcc-result binding))
        (es::top-dynamic (setq %top-dynamic binding))
        (es::symtab-initfun-var (setq %symtab-initfun-var binding))
        (t (error-invalid-special-specification keyword type))))

;;;-----------------------------------------------------------------------------
;;; providing special infos from the application to the compiler
;;;-----------------------------------------------------------------------------

(defun provide-compiler-info (key value)
  (case key
        (es::max-used-type-descriptor
         (setq max-used-type-descriptor value))
        (es::max-used-card-descriptor
         (setq max-used-card-descriptor value))
        (t (error-invalid-key-for-%provide-compiler-info key))))

#module-end
