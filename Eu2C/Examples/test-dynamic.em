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
;;; Title: Example 'test-dynamic'
;;;  Description:
;;    Basic tests of dynamic
;;;  Authors: Henry G. Weller
;;;-----------------------------------------------------------------------------
(defmodule test-dynamic
  (import (level-0)
   syntax (level-0))

(defglobal hmm 1)

(print (dynamic hmm) nl)

;;;-----------------------------------------------------------------------------
)  ;; end of module
;;;-----------------------------------------------------------------------------
