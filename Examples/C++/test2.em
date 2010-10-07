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
;;;  Library: cc
;;;  Authors: Andreas Kind
;;; Description: C++ interoperability
;;;  Compilation
;;   youtoo -c test2 -l level1
;;   gcc -c -I../Gc eul-cc2.c
;;   g++ -c eul-cc3.cc
;;   youtoo test2 -l level1 -fff eul-cc2 -fff eul-cc3 -recompile
;;;-----------------------------------------------------------------------------
(defmodule test2
  (syntax (macros)
   import (level1)
   export (bar))

;;;-----------------------------------------------------------------------------
;;; In-call from C++
;;;-----------------------------------------------------------------------------
(defun bar (str i)
  (string-ref str i))

;;;-----------------------------------------------------------------------------
;;; Out-call to C++
;;;-----------------------------------------------------------------------------
(defextern foo (<string> <int>) <character>)
(print (foo "Hello world!" 4) nl)

;;;-----------------------------------------------------------------------------
)  ;; End of module
;;;-----------------------------------------------------------------------------
