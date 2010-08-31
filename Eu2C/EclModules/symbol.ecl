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
;;;  Title: EL-in-CL: symbol
;;;  Description:
;;;  Authors: Ingo Mohr
;;;-----------------------------------------------------------------------------

#module symbol
(import (eulisp-kernel
         ;;compare
         (only (intern
                find-symbol
                eq)
               common-lisp)
         (rename ((gensym cl:gensym))
                 common-lisp))
 syntax (eulisp-kernel
         (rename ((defun cl:defun)
                  (symbolp cl:symbolp))
                 common-lisp)
         (only (&optional)
               common-lisp))
 expose ((only (symbol-name)
               common-lisp))
 export (gensym
         symbol-exists-p
         symbol?))

(make-eulisp-class symbol)

(defun symbol? (object)
  (if (eq object ())
      ()
    (cl:symbolp object)))

(cl:defun gensym (&optional string)
          (intern (cl:string (if string
                                 (cl:gensym (cl:string string))
                               (cl:gensym)))
                  $eulisp-symbol-package))

(defun symbol-exists-p (name)
  (find-symbol name $eulisp-symbol-package))

;;;-----------------------------------------------------------------------------
#module-end
;;;-----------------------------------------------------------------------------
