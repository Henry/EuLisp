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
;;;  Title: Example 'test-number'
;;;  Description:
;;    Simple mathematical expression interpreter
;;;  Compilation:
;;    Compile this module with basic system level-0.
;;;-----------------------------------------------------------------------------
(defmodule test-number
  (import (level-0)
   syntax (level-0))

(deflocal interpreter-fcn
  (list "binary+" binary+
        "binary-" binary-
        "binary*" binary*
        "binary/" binary/
        "binary%" binary%
        "binary-mod" binary-mod
        "binary-lcm" binary-lcm
        "binary-gcd" binary-gcd
        "binary<" binary<
        "acos" acos
        "asin" asin
        "atan" atan
        "atan2" atan2
        "cos" cos
        "sin" sin
        "tan" tan
        "cosh" cosh
        "sinh" sinh
        "tanh" tanh
        "exp" exp
        "log" log
        "log10" log10
        "pow" pow
        "sqrt" sqrt
        "ceiling" ceiling
        "floor" floor
        "round" round
        "truncate" truncate
        ))

(defun get-fun(item li)
  (if (null? li)
      ()
    (if (equal item (car li))
        (car (cdr li))
      (get-fun item (cdr (cdr li))))))

(defun rep()
  (format t  "~%Don't Panic> ")
  (let ((sexpr (read)))
    (if (cons? sexpr)
        (let ((fcn (get-fun (as-lowercase (symbol-name (car sexpr)))
                            interpreter-fcn) ))
          (if (null? fcn)
              (print sexpr)
            (format t "~%~s -> ~s" sexpr (apply fcn
                                                (cdr sexpr)) ))
          (rep))
      (progn (print sexpr)
             (if (eq 'bye sexpr)
                 ()
               (rep) ))) ))

(format t "~%most-positive-double-float ~s" most-positive-double-float)
(format t "~%least-positive-double-float ~s" least-positive-double-float)
(format t "~%most-negative-double-float ~s" most-negative-double-float)
(format t "~%least-negative-double-float ~s" least-negative-double-float)

(rep)

;;;-----------------------------------------------------------------------------
)  ;; end of module
;;;-----------------------------------------------------------------------------
