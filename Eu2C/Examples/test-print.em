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
  (import (level-0
           print)
   syntax (level-0))

(sprint stderr "hmm " 1 " end")

(sprint stdin "hmm " 1 " end")

(with-handler
 (lambda (condition continuation)
   (print "error"))
   (sprint stdin "hmm " 1 " end"))

(sprint stdout (stream? stdout))
(sprint stdout (file-stream? stdout))

(sprint stdout "End")

;;;-----------------------------------------------------------------------------
)  ;; end of module
;;;-----------------------------------------------------------------------------
