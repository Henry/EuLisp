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

#module test-context
(import
 (level-1
  SIMPLE-PROGRAMMING
  LZS
  MZS
  context
  el2lzs
  progn-context
  function-call-context
  if-form
  setq-form
  letstar-form
  tail-module
  inline
  lzs-to-mzs-fun
  slot-value
  analyse-h
  gutter
  apply-funs ; nullfun & eqfun
  (only (string=
         ;;symbol-name ; it is in level-0
         ) common-lisp)
  )
 ;; typeinfernce

 syntax
 (level-1)
 )



(defconstant $false ())

;; -----------------------------

(defmethod finish-a ((con <test>) var-or-constant)
  (finish-test-a con var-or-constant))

(defun finish-test-a (con var-or-constant)
  (cond ((or (var? var-or-constant)
             (tempvar? var-or-constant))
         (test-variable var-or-constant con))
        ((named-const? var-or-constant)
         (if (eq (?value var-or-constant) $false)
             (setf (?then-block con) ())
           (setf (?else-block con) ())))
        ((eq var-or-constant $false)
         ;; !!! constants with value NIL !!!
         (setf (?then-block con) ()))
        (t (setf (?else-block con) ()))))

;;------------------------------
;; constants
;;------------------------------


(defmethod l2m-a ((con <test>) (form <named-const>))
  ;;
  ;; form = <defined-named-constant>, <imported-named-constant>
  ;;
  (if (eq (?value form) $false)
      (setf (?then-block con) ())
    (setf (?else-block con) ()))
  (setf (?function con) ()))

(defmethod l2m-a ((con <test>) (form <sym>))
  ;;
  ;; form = <defined-symbol>, <imported-symbol>
  ;;
  (setf (?else-block con) ())
  (setf (?function con) ()))

;; symbol is a slot-name, and not allowed in normal functions

(defmethod l2m-a ((con <test>) (form <structured-literal>))
  ;;
  ;; value = <vector>, <pair>, <string>, LITERAL-INSTANCE
  ;;
  (setf (?else-block con) ())
  (setf (?function con) ()))

(defmethod l2m-a ((con <test>) (form <fpi>))
  ;;
  ;; <spint> - single precisition integer
  ;;
  (setf (?else-block con) ())
  (setf (?function con) ()))

(defmethod l2m-a ((con <test>) (form <double-float>))
  (setf (?else-block con) ())
  (setf (?function con) ()))

(defmethod l2m-a ((con <test>) (form <character>))
  (setf (?else-block con) ())
  (setf (?function con) ()))

(defmethod l2m-a ((con <test>) (form <class-def>))
  ;;
  ;;  form = <defined-class>, <imported-class>
  ;;
  (setf (?else-block con) ())
  (setf (?function con) ()))

(defmethod l2m-a ((con <test>) (form <literal-instance>))
  (setf (?else-block con) ())
  (setf (?function con) ()))

(defmethod l2m-a ((con <test>) (form <global-fun>))
  (setf (?else-block con) ())
  (setf (?function con) ()))

(defmethod l2m-a ((con <test>) (form <local-fun>))
  ;; a local fun in test-position of a if-form is dull
  (setf (?else-block con) ())
  (setf (?function con) ()))

(defmethod l2m-a ((con <test>) (form <imported-fun>))
  (setf (?else-block con) ())
  (setf (?function con) ()))

(defmethod l2m-a ((con <test>) (form <special-sys-fun>))
  (setf (?else-block con) ())
  (setf (?function con) ()))

(defmethod l2m-a ((con <test>) (form <global-generic-fun>))
  (setf (?else-block con) ())
  (setf (?function con) ()))

(defmethod l2m-a ((con <test>) (form <local-generic-fun>))
  (setf (?else-block con) ())
  (setf (?function con) ()))

(defmethod l2m-a ((con <test>) (form <imported-generic-fun>))
  (setf (?else-block con) ())
  (setf (?function con) ()))

(defmethod l2m-a ((con <test>) (form <cont>))
  (setf (?else-block con) ())
  (setf (?function con) ()))

(defmethod l2m-a ((con <test>) (form <null>))
  (setf (?then-block con) ())
  (setf (?function con) ()))

;;------------------------------
;;   end of constans and variables, begin of special-forms
;;------------------------------

;;(deflocal test-functions ()) ; defined in apply-funs

;;(deflocal nullfun ()) ; defined with eqfun in apply-funs

(defun test-variable (var test)
  ;;  (if (null? test-functions)
  ;;    (setq test-functions
  ;;          (list %neq %eq %gt %lt %ge %le))
  ;;    ())
  (let* ((tempvar (make <tempvar>
                        :tnr (dynamic *counter*)))

         (neq (car test-functions))
         (varvec (make-vector 3))
         (vardescr (make <var-descr> :var-vec varvec
                         :constant-counter 0)))
    (setf (dynamic *counter*) (+ (dynamic *counter*) 1))
    (setf (?function test) neq)
    (setf (vector-ref varvec 0) tempvar)
    (setf (vector-ref varvec 1) var)
    (setf (vector-ref varvec 2) $false)
    (setf (?var-descr test) vardescr)
    (setf (?arg-num test) 2)
    ))

(defmethod l2m-a ((con <test>) (form <var-ref>))
  ;;
  ;; var = <local-static>, <global-static>, <imported-static>, <dynamic>
  ;;
  (let ((var (?var form)))
    (test-variable (if (local-static? var) (rename var) var) con)))


(defmethod l2m-a ((con <test>) (form <app>))
  ;;
  ;; function = <global-fun>, <local-fun>, <imported-fun>, <special-sys-fun>,
  ;; <global-generic-fun>, <local-generic-fun>, imported-generic-fun>, <var-ref>,
  ;; <cont>, <defined-named-constant>, <imported-named-constant>
  ;;
  (let ((fun (?function form)))
    ;;    (if (null? nullfun) (setq nullfun
    ;;                             (find-lexical ^null? ^null?))
    ;;        ())
    ;;    (if (null? test-functions)
    ;;      (setq test-functions
    ;;            (list %neq %eq %gt %lt %ge %le))
    ;;      ())
    (cond ((eq fun nullfun)
           (setq fun (?then-block con))
           (setf (?then-block con) (?else-block con))
           (setf (?else-block con) fun)
           ;;           (common-lisp::format t
           ;;                   "~% l2m-a (after change) then-block ~s" (?then-block con))
           ;;           (common-lisp::format t
           ;;                                "~% else-block ~s " (?else-block con))
           (l2m-a con (car (?arg-list form))))
          ((or (member fun test-functions)
               (eq fun eqfun))
           (let* ((tempvar (make <tempvar>
                                 :tnr (dynamic *counter*)))
                  (varvec (make-vector 3))
                  (vardescr (make <var-descr> :var-vec varvec
                                  :constant-counter 0)))
             (setf (dynamic *counter*) (+ (dynamic *counter*) 1))
             (setf (vector-ref varvec 0) tempvar)
             (setf (?var-descr con) vardescr)
             (setf (?function con)
                   (if (eq fun eqfun) (car (cdr test-functions)) fun))
             (setf (?read-glocs con) (?read-glocs form))
             (l2m-call con (?arg-list form))
             ))
          ;;          ((eq (?inline fun) ^test) ...
          ;;          ((eq (?inline fun) ^value) ...
          ((global-fun? fun)
           ;; analyse the called fun first
           (lzs2mzs-fun fun)
           (if (eq (?inline fun) ^test)
;;;          (if (string= (symbol-name (?identifier fun)) "CONSP" )
;;;            ; inline-hack !!!!!!!!
               (let* ((arglist (?arg-list form))
                      (arg-num (length arglist))
                      (var-vec (make-vector (+ arg-num 1)))
                      (call (make <call>
                                  :function fun
                                  :arg-num arg-num
                                  :var-descr (make <var-descr>
                                                   :var-vec var-vec
                                                   :constant-counter 0))))
                 (l2m-call call arglist)
                 (print "************************ Start inline ****************")
                 (inline-a con fun var-vec ()))
             (test-variable (l2m-a (dynamic *arg-context*) form)
                            con)))
          (t (test-variable (l2m-a (dynamic *arg-context*) form)
                            con)))
    ))

(defmethod l2m-a ((con <test>) (form <set-slot-value>))
  (finish-test-a con (set-slot-value-a con form)))

(defmethod l2m-a ((con <test>) (form <get-slot-value>))
  (finish-test-a con (get-slot-value-a con form)))

(defmethod l2m-a ((con <test>) (form <setq-form>))
  ;;
  ;; location = <local-static>, <global-static>, <imported-static>, <dynamic>,
  ;; <defined-named-const>, <imported-named-const>
  ;;
  (finish-test-a con (setq-form-a con form))
  )

(defmethod l2m-a ((con <test>) (form <progn-form>))
  (l2m-progn con (?form-list form)))

(defmethod l2m-a ((con <test>) (form <if-form>))
  (finish-test-a con (if-form-a con form)))

(defmethod l2m-a ((con <test>) (form <switch-form>))
  (print "<test> <switch-form> not implemented")
  ())

(defmethod l2m-a ((con <test>) (form <let*-form>))
  (letstar-a con form))

(defmethod l2m-a ((con <test>) (form <labels-form>))
  (l2m-a con (?body form))
  )

(defmethod l2m-a ((con <test>) (form <let/cc-form>))
  (print "<test> <let/cc-form> not implemented")
  ())

;;
;;(defmethod l2m-a ((con <test>) (form <labeled-form>))
;;   (print "<test> <labeled-form> not implemented")
;;   ())
;;
;;
;;(defmethod l2m-a ((con <test>) (form <tagbody-form>))
;;   (print "<test> <tagbody-form> not implemented")
;;   ())
;;
;;
;;(defmethod l2m-a ((con <test>) (form <mv-lambda>))
;;   (print "<test> <mv-lambda> not implemented")
;;   ())
;;

#module-end
