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
;;; Title: EL-in-CL: standard module copy
;;;  Description:
;;;  Authors: Ingo Mohr
;;;-----------------------------------------------------------------------------

#module copy
(import (eulisp-kernel
         (only (deep-copy
                shallow-copy) list)
         (only (deep-copy
                shallow-copy) string)
         (only (deep-copy
                shallow-copy)
               vector))
 syntax (eulisp-kernel)
 export (deep-copy
         shallow-copy))

(defmethod deep-copy ((object <object>))
  object)

(defmethod shallow-copy ((object <object>))
  object)

;;;-----------------------------------------------------------------------------
#module-end
;;;-----------------------------------------------------------------------------
