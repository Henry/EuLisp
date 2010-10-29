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
;;; Title: Accessing a URL
;;;  Authors: Andreas Kind
;;;  Maintainer: Henry G. Weller
;;;  Compilation
;;    youtoo url -l level-1
;;;  Run
;;    url www.cs.bath.ac.uk /~jap/ak1/youtoo/home.html
;;;-----------------------------------------------------------------------------
(defmodule url
  (syntax (macros)
   import (level-1))

(let* ((host (vector-ref *argv* 1))
       (file-name (vector-ref *argv* 2))
       (c (make <connection> host: host port: 80))
       x)
  (sformat c "GET ~a\n" file-name)
  (while (null? (eq (setq x (read-line c () (eos-default-value)))
                    (eos-default-value)))
    (print x))
  (disconnect c))

;;;-----------------------------------------------------------------------------
)  ;; End of module url
;;;-----------------------------------------------------------------------------
