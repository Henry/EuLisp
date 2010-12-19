;;; Copyright 1997 A. Kind & University of Bath
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
;;; Title: accessing a URL
;;;  Authors: Andreas Kind
;;;  Maintainer: Henry G. Weller
;;;  Compilation
;;    youtoo url3 -l level-1
;;;  Run
;;    url3 http://www.cs.bath.ac.uk/~jap/ak1/youtoo/home.html
;;;-----------------------------------------------------------------------------

(defmodule url3
  (syntax (syntax-1)
   import (level-1))

(defun open-url (url)
  (system (fmt "firefox ~a" url)))

(open-url (vector-ref *argv* 1))

;;;-----------------------------------------------------------------------------
)  ;; End of module url3
;;;-----------------------------------------------------------------------------
