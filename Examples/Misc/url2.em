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
;;;  Library: misc
;;;  Authors: Andreas Kind
;;; Description: accessing a URL
;;;  Compilation
;;    youtoo url2 -l level1
;;;  Run
;;    url2
;;;-----------------------------------------------------------------------------
(defmodule url2
  (syntax (macros)
   import (level1))

(let ((c (make <connection> host: "www.cs.bath.ac.uk" port: 80))
      x)
  (sprint c "GET /~jap/ak1/youtoo/home.html" nl)
  (while (setq x (read-line c () ()))
    (print x))
  (disconnect c))

;;;-----------------------------------------------------------------------------
)  ;; End of module
;;;-----------------------------------------------------------------------------
