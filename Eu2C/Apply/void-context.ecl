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

#module void-context
(import (level-1
         SIMPLE-PROGRAMMING
         LZS
         MZS
         context
         analyse-h
         progn-context
         type-propagation
         type-inference
         function-call-context
         (only (append
                error)
               common-lisp)
         lzs-to-mzs-fun
         vector
         function-call
         if-form
         setq-form
         letstar-form
         slot-value)
 syntax (level-1))

;;------------------------------
(defmethod finish-a ((con <void>) res) res)

;;------------------------------
;; constants
;;------------------------------
(defmethod l2m-a ((con <void>) (form <named-const>))
  ;;
  ;; form = <defined-named-constant>, <imported-named-constant>
  ;;
  ;; (warning "not used constant ~s " form)
  ()
  )

(defmethod l2m-a ((con <void>) (form <sym>))
  ;;
  ;; form = <defined-symbol>, <imported-symbol>
  ;;
  ;; (warning "not used constant ~s " form)
  () )

;; symbol is a slot-name and in void-context not allowed

(defmethod l2m-a ((con <void>) (form <structured-literal>))
  ;;
  ;; value = <vector>, <pair>, <string>, LITERAL-INSTANCE
  ;;
  ;; (warning "not used constant ~s " form)
  () )

(defmethod l2m-a ((con <void>) (form <fpi>))
  ;;
  ;; <spint> - single precisition integer
  ;;
  ;; (warning "not used constant ~s " form)
  () )

(defmethod l2m-a ((con <void>) (form <double-float>))
  ;; (warning "not used constant ~s " form)
  () )

(defmethod l2m-a ((con <void>) (form <character>))
  ;; (warning "not used constant ~s " form)
  () )

(defmethod l2m-a ((con <void>) (form <class-def>))
  ;;
  ;;  form = <defined-class>, <imported-class>
  ;;
  ;; (warning "not used constant ~s " form)
  () )

(defmethod l2m-a ((con <void>) (form <literal-instance>))
  ;; (warning "not used constant ~s " form)
  () )

(defmethod l2m-a ((con <void>) (form <global-fun>))
  ;; (warning "not used constant ~s " form)
  () )

(defmethod l2m-a ((con <void>) (form <local-fun>))
  ;; (warning "not used constant ~s " form)
  ()
  )

(defmethod l2m-a ((con <void>) (form <imported-fun>))
  ;; (warning "not used constant ~s " form)
  () )

(defmethod l2m-a ((con <void>) (form <special-sys-fun>))
  ;; (warning "not used constant ~s " form)
  () )

(defmethod l2m-a ((con <void>) (form <global-generic-fun>))
  ;; (warning "not used constant ~s " form)
  () )

(defmethod l2m-a ((con <void>) (form <local-generic-fun>))
  ;; (warning "not used constant ~s " form)
  () )

(defmethod l2m-a ((con <void>) (form <imported-generic-fun>))
  ;; (warning "not used constant ~s " form)
  () )

(defmethod l2m-a ((con <void>) (form <cont>))
  ;; (warning "not used constant ~s " form)
  () )

(defmethod l2m-a ((con <void>) (form <null>))
  ;;  ; (warning "not used constant ~s " form)
  () )

;;------------------------------
;;   end of constans
;;------------------------------

;;------------------------------
;;   begin of variables
;;------------------------------

(defmethod l2m-a ((con <void>) (form <var-ref>))
  ;;
  ;; var = <local-static>, <global-static>, <imported-static>, <dynamic>
  ;;
  ;; (warning "not used variable ~s " (?var form))
  () )

;;------------------------------
;;   begin of function-application
;;------------------------------

(defmethod l2m-a ((con <void>) (form <app>))
  ;;
  ;; function = <global-fun>, <local-fun>, <imported-fun>, <special-sys-fun>,
  ;; <global-generic-fun>, <local-generic-fun>, imported-generic-fun>, <var-ref>,
  ;; <cont>, <defined-named-constant>, <imported-named-constant>
  ;;
  ;; like arg-context, but with cont-handling
  ;; (not jet impleemnted)
  ;;
  (let ((fun (?function form)))
    (if (cont? fun) (error "continuation not implemented")
      (call-a-function fun (?arg-list form) () (?read-glocs form)))))

(defmethod l2m-a ((con <void>) (form <set-slot-value>))
  (set-slot-value-a con form))

(defmethod l2m-a ((con <void>) (form <get-slot-value>))
  (get-slot-value-a con form))

(defmethod l2m-a ((con <void>) (form <setq-form>))
  ;;
  ;; location = <local-static>, <global-static>, <imported-static>, <dynamic>,
  ;; <defined-named-const>, <imported-named-const>
  ;;
  (setq-form-a con form))

(defmethod l2m-a ((con <void>) (form <progn-form>))
  (l2m-progn con (?form-list form)))

(defmethod l2m-a ((con <void>) (form <if-form>))
  (if-form-a con form))

(defmethod l2m-a ((con <void>) (form <switch-form>))
  (print "<void> <switch-form> not implemented")
  ())

(defmethod l2m-a ((con <void>) (form <let*-form>))
  (letstar-a con form))

(defmethod l2m-a ((con <void>) (form <labels-form>))
  (l2m-a con (?body form))
  )

(defmethod l2m-a ((con <void>) (form <let/cc-form>))
  (print "<void> <let/cc-form> not implemented")
  ())

;;
;;(defmethod l2m-a ((con <void>) (form <labeled-form>))
;;   (print "<void> <labeled-form> not implemented")
;;   ())
;;
;;
;;(defmethod l2m-a ((con <void>) (form <tagbody-form>))
;;   (print "<void> <tagbody-form> not implemented")
;;   ())
;;
;;
;;(defmethod l2m-a ((con <void>) (form <mv-lambda>))
;;   (print "<void> <mv-lambda> not implemented")
;;   ())
;;

;;;-----------------------------------------------------------------------------
#module-end
;;;-----------------------------------------------------------------------------
