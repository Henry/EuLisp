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
;;;-----------------------------------------------------------------------------

(defmodule plist
  (import ( tail basic-symbol
                 level-0)
   syntax (level-0)
   export (get symbol-plist put))


(%define-function (find-entry <object>)
  ((element <symbol>)
   (li <list>))
  (if (%eq (%cast %unsigned-word-integer li)
           (%cast %unsigned-word-integer ()))
      ()
    (if (%eq (%cast %unsigned-word-integer element)
             (%cast %unsigned-word-integer (car li)))
        li
      (find-entry element (cdr li)))))



(defun get(symbol indicator)
  (let ((val (find-entry indicator (symbol-plist symbol))))
    (if val
        (car (cdr val))
      val)))

(defun put
  (symbol indicator new)
  (let* ((sp (symbol-plist symbol))
         (val (find-entry indicator sp)))
    (if val
        ((setter car) (cdr val) new)
      ((setter symbol-plist) symbol (cons indicator (cons new sp)))))
  new
  )

((setter setter) get put)
)
