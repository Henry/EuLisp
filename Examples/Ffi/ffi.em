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
;;; Title: Foreign function test
;;;  Authors: Andreas Kind
;;;  Maintainer: Henry G. Weller
;;;  Compilation
;;    youtoo ffi -l level-0
;;;-----------------------------------------------------------------------------

(defmodule ffi
  (syntax (syntax-0)
   import (level-0))

(defextern ext_get_struct () ptr)
(defextern ext_print_struct (ptr) ptr)
(deflocal structs '())

(let loop ((n 0))
     (cond ((< n 50)
            (let ((s (ext_get_struct)))
              (format "struct #~d~%" n)
              (print "  ")
              (ext_print_struct s)
              (print "  ")
              (write s)
              (setq structs (cons s structs))
              (loop (+ n 1))))
           (t
            (print "*** Finished creating" nl))))

(do (lambda (s) (ext_print_struct s)) structs)

;;;-----------------------------------------------------------------------------
)  ;; End of module ffi
;;;-----------------------------------------------------------------------------
