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
;;; Title: Basic syntax
;;;  Description:
;;    Provides all the functionality needed to write simple macros and to use
;;    `quasiquote'.
;;;  Notes:
;;    `append' and `cons?' are exported from this module and should be imported
;;    from here.
;;;  Authors: Ingo Mohr
;;;  Maintainer: Henry G. Weller
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
)  ;; End of module basic-syntax
;;;-----------------------------------------------------------------------------
