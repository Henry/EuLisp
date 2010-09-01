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
;;;  Authors: Horst Friedrich
;;;-----------------------------------------------------------------------------

#module arg-context
(import (eulisp1
         SIMPLE-PROGRAMMING
         LZS
         MZS
         context
         analyse-h
         progn-context
         type-propagation
         (only (error)
               common-lisp)
         type-inference
         lzs-to-mzs-fun
         function-call-context
         vector
         function-call
         if-form
         letstar-form
         setq-form
         slot-value
         gutter)
 syntax (eulisp1))

(defmethod finish-a ((con <arg>) form) form)

;;;-----------------------------------------------------------------------------
;;; Constants
;;;-----------------------------------------------------------------------------
(defmethod l2m-a ((con <arg>) (form <named-const>))
  ;;
  ;; form = <defined-named-constant>, <imported-named-constant>
  ;;
  form)

(defmethod l2m-a ((con <arg>) (form <sym>))
  ;;
  ;; form = <defined-symbol>, <imported-symbol>
  ;;
  form)

(defmethod l2m-a ((con <arg>) (form <symbol>))
  ;;
  ;; form = slot-name
  ;;
  form)

(defmethod l2m-a ((con <arg>) (form <structured-literal>))
  ;;
  ;; value = <vector>, <pair>, <string>, LITERAL-INSTANCE
  ;;
  form)

(defmethod l2m-a ((con <arg>) (form <fpi>))
  ;;
  ;; <fpi> - single precisition integer
  ;;
  form)

(defmethod l2m-a ((con <arg>) (form <double-float>))
  form)

(defmethod l2m-a ((con <arg>) (form <character>))
  form)

(defmethod l2m-a ((con <arg>) (form <class-def>))
  ;;
  ;;  form = <defined-class>, <imported-class>
  ;;
  form)

(defmethod l2m-a ((con <arg>) (form <literal-instance>))
  form)

(defmethod l2m-a ((con <arg>) (form <global-fun>))
  (setf (?function-type form) $data)
  form)

(defmethod l2m-a ((con <arg>) (form <local-fun>))
  (lzs2mzs-fun form)
  (if (eq (?function-type form) $closure)
      (make-a-closure-function form (dynamic block))
    form))

(defmethod l2m-a ((con <arg>) (form <imported-fun>))
  (setf (?function-type form) $data)
  form)

(defmethod l2m-a ((con <arg>) (form <special-sys-fun>))
  (setf (?function-type form) $data)
  form)

(defmethod l2m-a ((con <arg>) (form <global-generic-fun>))
  (setf (?function-type form) $data)
  form)

(defmethod l2m-a ((con <arg>) (form <local-generic-fun>))
  (setf (?function-type form) $data)
  form)

(defmethod l2m-a ((con <arg>) (form <imported-generic-fun>))
  (setf (?function-type form) $data)
  form)

(defmethod l2m-a ((con <arg>) (form <cont>))
  form)

(defmethod l2m-a ((con <arg>) (form <null>))
  form)

;;;-----------------------------------------------------------------------------
;;; Variables
;;;-----------------------------------------------------------------------------
(defmethod l2m-a ((con <arg>) (form <var-ref>))
  ;;
  ;; var = <local-static>, <global-static>, <imported-static>, <dynamic>
  ;;
  (let ((var (?var form)))
    (if (local-static? var) (rename var)
      var))
  )

;;;-----------------------------------------------------------------------------
;;; function call
;;;-----------------------------------------------------------------------------
(defmethod l2m-a ((con <arg>) (form <app>))
  ;;
  (let ((fun (?function form)))
    (if (cont? fun) (error "continuation in argument position")
      ;; (if (eq (common-lisp::class-of fun) lzs-mop::<slot-accessor-fun>)
      ;;   (let* ((arglist (?arg-list form))
      ;;             (arg-num (length arglist))
      ;;             (var-vec (make-vector (+ arg-num 1)))
      ;;             (call (make <call>
      ;;                         :function fun
      ;;                         :arg-num arg-num
      ;;                         :var-descr (make <var-descr>
      ;;                                          :var-vec var-vec
      ;;                                          :constant-counter 0))))
      ;;     (lzs2mzs-fun fun)
      ;;        (l2m-call call arglist)
      ;;        (print "************************ Start inline ****************")
      ;;        (inline::inline-a con fun var-vec ()))
      (call-a-function fun (?arg-list form)
                       () (?read-glocs form)))))
;;)

;;;-----------------------------------------------------------------------------
;;; Other
;;;-----------------------------------------------------------------------------
(defmethod l2m-a ((con <arg>) (form <set-slot-value>))
  (set-slot-value-a con form))

(defmethod l2m-a ((con <arg>) (form <get-slot-value>))
  (get-slot-value-a con form))

(defmethod l2m-a ((con <arg>) (form <setq-form>))
  ;;
  ;; location = <local-static>, <global-static>, <imported-static>, <dynamic>,
  ;; <defined-named-const>, <imported-named-const>
  ;;
  (setq-form-a con form)
  )

(defmethod l2m-a ((con <arg>) (form <progn-form>))
  (l2m-progn con (?form-list form))
  )

(defmethod l2m-a ((con <arg>) (form <if-form>))
  (if-form-a con form)
  )

(defmethod l2m-a ((con <arg>) (form <switch-form>))
  (print "<arg> <switch-form> not implemented")
  ())

(defmethod l2m-a ((con <arg>) (form <let*-form>))
  (letstar-a con form)
  )

(defmethod l2m-a ((con <arg>) (form <labels-form>))
  (l2m-a con (?body form)))

(defmethod l2m-a ((con <arg>) (form <let/cc-form>))
  (print "<arg> <let/cc-form> not implemented")
  ())

;;
;;(defmethod l2m-a ((con <arg>) (form <labeled-form>))
;;   (print "<arg> <labeled-form> not implemented")
;;   ())
;;
;;
;;(defmethod l2m-a ((con <arg>) (form <tagbody-form>))
;;   (print "<arg> <tagbody-form> not implemented")
;;   ())
;;
;;
;;(defmethod l2m-a ((con <arg>) (form <mv-lambda>))
;;   (print "<arg> <mv-lambda> not implemented")
;;   ())
;;

;;;-----------------------------------------------------------------------------
#module-end
;;;-----------------------------------------------------------------------------
