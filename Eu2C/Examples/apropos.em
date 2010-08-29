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
;;;  Title: Example 'apropos'
;;;  Compilation:
;;    Compile this module with basic system eulisp-level-0
;;    eu2c -bs eulisp-level-0 apropos
;;;-----------------------------------------------------------------------------
(defmodule apropos
  (import (eulisp-level-0 cl-ext)
   syntax (eulisp-level-0 cl-ext))

(defun apropos (word table)
  (labels ((get-words-with-char
            (i)
            (remove-if
             (lambda (word-in-table)
               (null (eql (element (as-lowercase word) i)
                          (element (as-lowercase word-in-table) i))))
             table)))
          (dotimes (i (size word))
                   (setq table (get-words-with-char i)))
          table))

(deflocal words
  '("Einstein" "Europa" "Eulisp"
    "Lisp" "Liebe" "Liste"))

(defun run ()
  (format t "~%Please enter the word beginning: ")
  (let ((input (read)))
    (when (symbolp input)
          (print (apropos (symbol-name input) words))
          (run))))

;;;-----------------------------------------------------------------------------
;;; Run the test
;;;-----------------------------------------------------------------------------

(run)

;;;-----------------------------------------------------------------------------
)  ;; end of module
;;;-----------------------------------------------------------------------------
