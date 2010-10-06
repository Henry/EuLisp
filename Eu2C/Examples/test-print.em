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
;;; Title: Example 'test-introspection'
;;;  Description:
;;    Basic tests of the new print functions
;;;  Authors: Henry G. Weller
;;;-----------------------------------------------------------------------------
(defmodule test-print
  (import (level-0)
   syntax (syntax-0))

(sprint stderr "hmm " 1 " end\n")

;; (sprint stdin "hmm " 1 " end")

;; (with-handler
;;  (lambda (condition continuation)
;;    (print "error" nl))
;;    (sprint stdin "hmm " 1 " end"))

(sprint stdout (stream? stdout))
(sprint stdout (file-stream? stdout))

(sformat stdout "hello ~a~%" 1)
(format "hello ~a~%" 1)
(sprint stdout (fmt "hello ~a~%" 1))
(sprint stdout "End\n" nl nl)

(write #\\n)
(swrite stdout #\\n #\\f)

(print nl nl)

;;;-----------------------------------------------------------------------------
)  ;; End of module test-print
;;;-----------------------------------------------------------------------------
