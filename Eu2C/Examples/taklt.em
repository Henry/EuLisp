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
;;; Title: Benchmark 'takl'
;;;  Description:
;;    This benchmark was taken from R.P.Gabriel: "Performance and Evaluation of
;;    Lisp-Systems". Additionally a version was added where the function 'shorter?' is
;;    realized as a generic function with two methods specializing on the first
;;    argument. The specialization was restricted to the first argument to be
;;    comparable with C++.
;;;  Documentation:
;;;  Notes:
;;    The benchmark is called with 24, 12 and 6 instead of 18, 12 and 6 as in Gabriel
;;    to get times greater than one second on SPARC-machines.
;;    Compile this module with basic system level-0x.
;;;  Requires:
;;;  Problems:
;;;  Authors: Ingo Mohr
;;;-----------------------------------------------------------------------------

(defmodule taklt
  (import (level-0x timing)
   syntax (level-0x timing))

(defun listn (n)
  (if (eql n 0)
      ()
    (cons n (listn (- n 1)))))


(deflocal l24 (listn 24))
(deflocal l18 (listn 18))
(deflocal l12 (listn 12))
(deflocal l6 (listn 6))

(defun takl (x y z)
  (if (null? (shorter? y x))
      z
    (takl (takl (cdr x) y z)
          (takl (cdr y) z x)
          (takl (cdr z) x y))))

(defun shorter? (x y)
  (if (null? y )
      ()
    (if  (null? x) t
      (shorter? (cdr x)
                (cdr y))) ))

(time (takl l24 l12 l6)
      "\n(takl i24 l12 l6) : %.2f sec %.2f sec system %.2f sec sum\n")

;;;-----------------------------------------------------------------------------
;;; takl with generic shorter?
;;;-----------------------------------------------------------------------------

(defun gtakl (x y z)
  (if (null? (gshorter? y x))
      z
    (gtakl (gtakl (cdr x) y z)
           (gtakl (cdr y) z x)
           (gtakl (cdr z) x y))))

(defgeneric gshorter? ((x <list>) y))

(defmethod gshorter? ((x <null>) y)
  y)

(defmethod gshorter? ((x <cons>) y)
  (if (null? y)
      ()
    (gshorter? (cdr x) (cdr y))))

(time (gtakl l24 l12 l6)
      "\n(gtakl 24l 12l 6l): %.2f sec %.2f sec system %.2f sum\n")

)
