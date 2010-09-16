;;; Copyright 1994-2010 Fraunhofer ISST
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
;;; Title:
;;;  Description:
;;    basic-syntax provides all stuff which is needed to write simple macros and
;;    to use quasiquote.
;;;  Documentation:
;;;  Notes:
;;    append and cons? are defined in this module and should be imported from
;;    here.
;;;  Authors: Ingo Mohr
;;;-----------------------------------------------------------------------------

(defmodule basic-syntax
  (import (%tail
           basic-list
           basic-compare)
   syntax (%tail)
   export (<list>
           <cons>
           <null>
           car
           cdr
           cons
           null?
           cons?
           append
           t
           eq))

;;;-----------------------------------------------------------------------------
) ;end of module basic-syntax
;;;-----------------------------------------------------------------------------
