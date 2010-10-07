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
;;;  Author: Andreas Kind
;;; Description: foreign function test
;;;  Compilation
;;    cc $(CFLAGS) -c eul-ffi2.c
;;    youtoo ffi2 -l level-0 -fff eul-ffi2
;;;-----------------------------------------------------------------------------
(defmodule ffi2
  (syntax (syntax-0)
   import (level-0))

;;;-----------------------------------------------------------------------------
;;; External functions using C pointers
;;;-----------------------------------------------------------------------------
(defextern ext_foo_int (<int*>) <int>)
(defextern ext_foo_double (<double*>) <double>)
(defextern ext_foo_string (<string*>) <string>)
(defextern ext_foo_int2 () <int*>)
(defextern ext_foo_double2 () <double*>)
(defextern ext_foo_string2 () <string*>)
(defextern ext_nil () ptr)

;;;-----------------------------------------------------------------------------
;;; Example calls
;;;-----------------------------------------------------------------------------
(let ((x (convert 3 <int*>)))
  (ext_foo_int x)
  (print (convert x <int>) nl))
(let ((x (convert 3.0 <double*>)))
  (ext_foo_double x)
  (print (convert x <double>) nl))
(let ((x (convert "abc" <string*>)))
  (ext_foo_string x)
  (print (convert x <string>) nl))
(let ((x (ext_foo_int2)))
  (print (convert x <int>) nl))
(let ((x (ext_foo_double2)))
  (print (convert x <double>) nl))
(let ((x (ext_foo_string2)))
  (print (convert x <string>) nl))

(print (eq (ext_nil) 'nil) nl)
(print (eq (ext_nil) '()) nl)

;;;-----------------------------------------------------------------------------
)  ;; End of module
;;;-----------------------------------------------------------------------------
