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

#module join-label-context
(import
 (eulisp1
  SIMPLE-PROGRAMMING
  LZS
  MZS
  context
  progn-context
  if-form
  setq-form
  letstar-form
  function-call
  lzs-to-mzs-fun
  slot-value
  gutter
  (only (error) common-lisp)
  analyse-h) ; make-vector and vector-ref
 ;; typeinfernce

 syntax
 (eulisp1)
 )

;;;-----------------------------------------------------------------------------
;;; constants
;;;-----------------------------------------------------------------------------

(defmethod l2m-a ((con <join-label>) (form <named-const>))
  ;;
  ;; form = <defined-named-constant>, <imported-named-constant>
  ;;
  form)

(defmethod l2m-a ((con <join-label>) (form <sym>))
  ;;
  ;; form = <defined-symbol>, <imported-symbol>
  ;;
  form)

;; symbol is a slot name and not allowed in normal functions
;;(defmethod l2m-a ((con <join-label>) (form <symbol>))
;;
;; form = <defined-symbol>, <imported-symbol>
;;
;;  form)

(defmethod l2m-a ((con <join-label>) (form <structured-literal>))
  ;;
  ;; value = <vector>, <pair>, <string>, LITERAL-INSTANCE
  ;;
  form)

(defmethod l2m-a ((con <join-label>) (form <fpi>))
  ;;
  ;; <spint> - single precisition integer
  ;;
  form)

(defmethod l2m-a ((con <join-label>) (form <double-float>))
  form)

(defmethod l2m-a ((con <join-label>) (form <character>))
  form)

(defmethod l2m-a ((con <join-label>) (form <class-def>))
  ;;
  ;;  form = <defined-class>, <imported-class>
  ;;
  form)

(defmethod l2m-a ((con <join-label>) (form <literal-instance>))
  form)

(defmethod l2m-a ((con <join-label>) (form <global-fun>))
  (setf (?function-type form) $data)
  form)

(defmethod l2m-a ((con <join-label>) (form <local-fun>))
  (lzs2mzs-fun form)
  (if (eq (?function-type form) $closure)
      (make-a-closure-function form (dynamic block))
    form)
  )

(defmethod l2m-a ((con <join-label>) (form <imported-fun>))
  (setf (?function-type form) $data)
  form)

(defmethod l2m-a ((con <join-label>) (form <special-sys-fun>))
  (setf (?function-type form) $data)
  form)

(defmethod l2m-a ((con <join-label>) (form <global-generic-fun>))
  (setf (?function-type form) $data)
  form)

(defmethod l2m-a ((con <join-label>) (form <local-generic-fun>))
  (setf (?function-type form) $data)
  form)

(defmethod l2m-a ((con <join-label>) (form <imported-generic-fun>))
  (setf (?function-type form) $data)
  form)

(defmethod l2m-a ((con <join-label>) (form <cont>))
  form)

(defmethod l2m-a ((con <join-label>) (form <null>))
  form)

;;;-----------------------------------------------------------------------------
;;; end of constans and variables, begin of special-forms
;;;-----------------------------------------------------------------------------

(defmethod l2m-a ((con <join-label>) (form <var-ref>))
  ;;
  ;; var = <local-static>, <global-static>, <imported-static>, <dynamic>
  ;;
  (let ((var (?var form)))
    (if (local-static-p var) (rename var)
      var))
  )

(defmethod l2m-a ((con <join-label>) (form <app>))
  ;;
  ;; function = <global-fun>, <local-fun>, <imported-fun>, <special-sys-fun>,
  ;; <global-generic-fun>, <local-generic-fun>, imported-generic-fun>, <var-ref>,
  ;; <cont>, <defined-named-constant>, <imported-named-constant>
  ;;
  (let ((fun (?function form)))
    (if (cont-p fun) (error "continuation in argument position")
      (call-a-function fun (?arg-list form) () (?read-glocs form)))))

(defmethod l2m-a ((con <join-label>) (form <set-slot-value>))
  (set-slot-value-a con form))

(defmethod l2m-a ((con <join-label>) (form <get-slot-value>))
  (get-slot-value-a con form))

(defmethod l2m-a ((con <join-label>) (form <setq-form>))
  ;;
  ;; location = <local-static>, <global-static>, <imported-static>, <dynamic>,
  ;; <defined-named-const>, <imported-named-const>
  ;;
  (setq-form-a con form)
  )

(defmethod l2m-a ((con <join-label>) (form <progn-form>))
  (l2m-progn con (?form-list form))
  )

(defmethod l2m-a ((con <join-label>) (form <if-form>))
  (if-form-a con form))

(defmethod l2m-a ((con <join-label>) (form <switch-form>))
  (print "<join-label> <switch-form> not implemented")
  ())

(defmethod l2m-a ((con <join-label>) (form <let*-form>))
  (letstar-a con form))

(defmethod l2m-a ((con <join-label>) (form <labels-form>))
  (l2m-a con (?body form))
  )

(defmethod l2m-a ((con <join-label>) (form <let/cc-form>))
  (print "<join-label> <let/cc-form> not implemented")
  ())

;;
;;(defmethod l2m-a ((con <join-label>) (form <labeled-form>))
;;   (print "<join-label> <labeled-form> not implemented")
;;   ())
;;
;;
;;(defmethod l2m-a ((con <join-label>) (form <tagbody-form>))
;;   (print "<join-label> <tagbody-form> not implemented")
;;   ())
;;
;;
;;(defmethod l2m-a ((con <join-label>) (form <mv-lambda>))
;;   (print "<join-label> <mv-lambda> not implemented")
;;   ())
;;

#module-end
