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
;;; Title: events not yet implemented
;;;  Library: level-1
;;;  Authors: Andreas Kind, Julian Padget
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule event
  (syntax (_telos0)
   import (telos)
   export (wait
           ticks-per-second))

;;;-----------------------------------------------------------------------------
;;; Wait (timeout should be <integer>)
;;;-----------------------------------------------------------------------------
(defgeneric wait (x timeout))

(defmethod wait ((x <object>) (timeout <object>))
  (error () "wait not yet implemented"))

(defextern eul_ticks_per_second () <fpi>)
(defconstant ticks-per-second (eul_ticks_per_second))

;;;-----------------------------------------------------------------------------
)  ;; End of module event
;;;-----------------------------------------------------------------------------
