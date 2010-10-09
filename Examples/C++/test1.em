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
;;; Title: C++ interoperability
;;;  Authors: Andreas Kind
;;;  Maintainer: Henry G. Weller
;;;  Compilation
;;   g++ -c eul-cc1.cc
;;   youtoo test1 -l level-0 -fff eul-cc1
;;;-----------------------------------------------------------------------------
(defmodule test1
  (syntax (syntax-0)
   import (level-0))

;;;-----------------------------------------------------------------------------
;;; Out-call to C++
;;;-----------------------------------------------------------------------------
(defextern foo (<string> <int>) <character>)
(print (foo "Hello world!" 4) nl)

;;;-----------------------------------------------------------------------------
)  ;; End of module test1
;;;-----------------------------------------------------------------------------
