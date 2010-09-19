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
;;; Title: 
;;;  Description:
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Horst Friedrich
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

#module switch-context
(import
 (level-1
  SIMPLE-PROGRAMMING
  LZS
  MZS
  context
  progn-context
  slot-value
  analyse-h) ; make-vector and vector-ref
 ;; typeinfernce

 syntax
 (level-1)
 )



;;--------------------------------------
;; constants
;;--------------------------------------

(defmethod l2m-a ((con <switch>) (form <named-const>))
  ;;
  ;; form = <defined-named-constant>, <imported-named-constant>
  ;;
  (print "<switch> <named-const> not implemented")
  ())

(defmethod l2m-a ((con <switch>) (form <sym>))
  ;;
  ;; form = <defined-symbol>, <imported-symbol>
  ;;
  (print "<switch> <sym> not implemented")
  ())

;; symbol is a slot name and not allowed in normal functions

(defmethod l2m-a ((con <switch>) (form <structured-literal>))
  ;;
  ;; value = <vector>, <pair>, <string>, LITERAL-INSTANCE
  ;;
  (print "<switch> <structured-literal> not implemented")
  ())

(defmethod l2m-a ((con <switch>) (form <fpi>))
  ;;
  ;; <spint> - single precisition integer
  ;;
  (print "<switch> <class-def> not implemented")
  ())

(defmethod l2m-a ((con <switch>) (form <double-float>))
  (print "<switch> <double-float> not implemented")
  ())

(defmethod l2m-a ((con <switch>) (form <character>))
  (print "<switch> <character> not implemented")
  ())

(defmethod l2m-a ((con <switch>) (form <class-def>))
  ;;
  ;;  form = <defined-class>, <imported-class>
  ;;
  (print "<switch> <class-def> not implemented")
  ())

(defmethod l2m-a ((con <switch>) (form <literal-instance>))
  (print "<switch> <literal-instance> not implemented")
  ())

(defmethod l2m-a ((con <switch>) (form <global-fun>))
  (print "<switch> <global-fun> not implemented")
  ())

(defmethod l2m-a ((con <switch>) (form <local-fun>))
  (print "<switch> <local-fun> not implemented")
  ())

(defmethod l2m-a ((con <switch>) (form <imported-fun>))
  (print "<switch> <imported-fun> not implemented")
  ())

(defmethod l2m-a ((con <switch>) (form <special-sys-fun>))
  (print "<switch> <special-sys-fun> not implemented")
  ())

(defmethod l2m-a ((con <switch>) (form <global-generic-fun>))
  (print "<switch> <global-generic-fun> not implemented")
  ())

(defmethod l2m-a ((con <switch>) (form <local-generic-fun>))
  (print "<switch> <local-generic-fun> not implemented")
  ())

(defmethod l2m-a ((con <switch>) (form <imported-generic-fun>))
  (print "<switch> <imported-generic-fun> not implemented")
  ())

(defmethod l2m-a ((con <switch>) (form <cont>))
  (print "<switch> <cont> not implemented")
  ())

(defmethod l2m-a ((con <switch>) (form <null>))
  (print "<switch> <null> not implemented")
  ())

;;--------------------------------------
;;   end of constans and variables, begin of special-forms
;;--------------------------------------

(defmethod l2m-a ((con <switch>) (form <var-ref>))
  ;;
  ;; var = <local-static>, <global-static>, <imported-static>, <dynamic>
  ;;
  (print "<switch> <var-ref> not implemented")
  ())

(defmethod l2m-a ((con <switch>) (form <app>))
  ;;
  ;; function = <global-fun>, <local-fun>, <imported-fun>, <special-sys-fun>,
  ;; <global-generic-fun>, <local-generic-fun>, imported-generic-fun>, <var-ref>,
  ;; <cont>, <defined-named-constant>, <imported-named-constant>
  ;;
  (print "<switch> <app> not implemented")
  ())

(defmethod l2m-a ((con <switch>) (form <set-slot-value>))
  (print "<switch> <set-slot-value> not implemented")
  ())

(defmethod l2m-a ((con <switch>) (form <get-slot-value>))
  (print "<switch> <get-slot-value> not implemented")
  ())

(defmethod l2m-a ((con <switch>) (form <setq-form>))
  ;;
  ;; location = <local-static>, <global-static>, <imported-static>, <dynamic>,
  ;; <defined-named-const>, <imported-named-const>
  ;;
  (print "<switch> <setq-form> not implemented")
  ())

(defmethod l2m-a ((con <switch>) (form <progn-form>))
  (l2m-progn con (?form-list form)))

(defmethod l2m-a ((con <switch>) (form <if-form>))
  (print "<switch> <if-form> not implemented")
  ())

(defmethod l2m-a ((con <switch>) (form <switch-form>))
  (print "<switch> <switch-form> not implemented")
  ())

(defmethod l2m-a ((con <switch>) (form <let*-form>))
  (print "<switch> <let*-form> not implemented")
  ())

(defmethod l2m-a ((con <switch>) (form <labels-form>))
  (print "<switch> <labels-form> not implemented")
  ())

(defmethod l2m-a ((con <switch>) (form <let/cc-form>))
  (print "<switch> <let/cc-form> not implemented")
  ())

;;
;;(defmethod l2m-a ((con <switch>) (form <labeled-form>))
;;   (print "<switch> <labeled-form> not implemented")
;;   ())
;;
;;
;;(defmethod l2m-a ((con <switch>) (form <tagbody-form>))
;;   (print "<switch> <tagbody-form> not implemented")
;;   ())
;;
;;
;;(defmethod l2m-a ((con <switch>) (form <mv-lambda>))
;;   (print "<switch> <mv-lambda> not implemented")
;;   ())
;;

#module-end
