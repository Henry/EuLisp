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
;;;  Title: Expand-Literal
;;;  Description:
;;    Provides things to transform literals into instances of <literal-instance>.
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Ingo Mohr
;;;-----------------------------------------------------------------------------

#module expand-literal
(import
 (level-0
  lzs accessors
  ;; tail-module using explicitely machine-description::%function
  (only (remove-if-not make-instance mapcan)
        common-lisp))
 syntax
 (level-1
  (only (push)
        common-lisp))

 export
 (expand-literal
  %function-literal
  make-literal-instance
  reset-literals
  *literals*
  get-functions-used-in-literals)
 )


(defgeneric expand-literal (literal))

(defun %function-literal (function-object)
  (if (symbol? function-object)
      function-object            ; return ^unknown... as it is
    (make-literal-instance
     ;;%function
     (cl:symbol-value (cl:find-symbol "%FUNCTION" "MACHINE-DESCRIPTION"))
     (list (if (generic-fun? function-object)
               (?discriminating-fun function-object)
             function-object)))))

(deflocal *literals* ())

(defun reset-literals ()
  (setq *literals* ()))

(defun make-literal-instance (class value-list)
  (let ((inst (make-instance <literal-instance>
                             :class class
                             :value-list value-list)))
    (push inst *literals*)
    inst))

(defmethod get-functions-used-in-literals ()
  (mapcan (lambda (lit)
            (remove-if-not #'fun? (?value-list lit)))
          *literals*))

(defmethod ?byte-size-as-component ((pobj t))
  (if (expand-literal pobj)
      (if (?class (expand-literal pobj))
          (?byte-size-as-component (?class (expand-literal pobj)))
        4)
    4))

#module-end
