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
;;; Title: Level-1 but without the generic arithmetic/reversal
;;;  Library: comp (EuLisp to Bytecode Compiler -- EuLysses)
;;;  Authors: Andreas Kind, Keith Playford
;;;  Maintainer: Henry G. Weller
;;;  Description:
;;    binds together the almost-everywhere-used modules
;;    And we don't need generic arithmetic/reversal.
;;;-----------------------------------------------------------------------------

(defmodule i-level1
  (import ((rename ((reverse-list reverse))
                   boot))
   expose ((except (+
                    -
                    *
                    /
                    %
                    =
                    <
                    reverse)
                   level1)
           aux-table)
   export (+
           -
           *
           /
           %
           =
           <
           reverse))

;;;-----------------------------------------------------------------------------
)  ;; End of module i-level1
;;;-----------------------------------------------------------------------------
