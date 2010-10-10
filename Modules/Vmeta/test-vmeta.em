;;; Copyright 2003 T. Kurt Bond
;;; Copyright 2010 Henry G. Weller
;;;-----------------------------------------------------------------------------
;;  This file is part of
;;; ---                         EuLisp System 'Youtoo'
;;;-----------------------------------------------------------------------------
;;
;;  Youtoo is free software: you can redistribute it and/or modify it under the
;;  terms of the GNU General Public License version 2 as published by the Free
;;  Software Foundation.
;;
;;  Youtoo is distributed in the hope that it will be useful, but WITHOUT ANY
;;  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;  details.
;;
;;  You should have received a copy of the GNU General Public License along with
;;  this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;-----------------------------------------------------------------------------
;;; Title: Test verbose META in EuLisp
;;;  Author: T. Kurt Bond
;;;  Maintainer: Henry G. Weller
;;;  Description:
;;    See vmeta.em
;;;-----------------------------------------------------------------------------

(defmodule test-vmeta
  (syntax (syntax-0
           vmeta)
   import (level-0))

(defun digit (c)
  (and (character? c)
       (<= #\0 c #\9)))

(defun whitespace (c)
  (and (character? c)
       (or (eql c #\  )
           (eql c #\ ))))

(defun letter (c)
  (and (character? c)
       (or (<= #\a c #\z)
           (<= #\A c #\Z))))

(defun identifier (c)
  (or (letter c)
      (digit c)
      (eql #\- c)))

(let (name args number)
  (match-expr
   "(def-bytecode write-object (x)   66 (in obj1 obj2) (out obj))"
   (seq
    (star (type whitespace))
    "(def-bytecode"
    (star (type whitespace))
    (name name (seq (type letter)
                    (star (type identifier))))
    (star (type whitespace))
    "("
    (star (seq
           (push args (star (type identifier) 1 ()))
           (star (alt (type whitespace)))))
    ")"
    (star (alt (type whitespace)))
    (name number (seq (star (type digit) 1 ())))))
  (format "~a ~a ~a ~%" name args number))

(defun match-test (sequence)
  (let ((index 0)
        (end (size sequence)))
    (let (name args number)
      (match
       (seq
        (star (type whitespace))
        "(def-bytecode"
        (star (type whitespace))
        (name name (seq (type letter)
                        (star (type identifier))))
        (star (type whitespace))
        "("
        (star (seq
               (push args (star (type identifier) 1 ()))
               (star (alt (type whitespace)))))
        ")"
        (star (alt (type whitespace)))
        (name number (seq (star (type digit) 1 ())))))
      (format "~a ~a ~a ~%" name args number))
    ))

(match-test "(def-bytecode write-object (x)   66 (in obj1 obj2) (out obj))")

;;;-----------------------------------------------------------------------------
)  ;; End of module test-vmeta
;;;-----------------------------------------------------------------------------
