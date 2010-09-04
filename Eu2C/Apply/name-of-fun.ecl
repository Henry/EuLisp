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
;;;  Title: 
;;;  Description:
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Horst Friedrich
;;;-----------------------------------------------------------------------------

#module name-of-fun

(import (level-1
         accessors
         lzs
         mzs
         )

 syntax (level-1)

 export (name-of funtype-of)

 )



(defgeneric name-of (fun))

(defmethod name-of ((fun <slot-accessor-fun>))
  (?identifier (?slot fun)))

(defmethod name-of ((fun <slot-init-fun>))
  (?identifier (?slot fun)))

(defmethod name-of ((fun <constructor-fun>))
  (?identifier (?constructor-for fun)))

(defmethod name-of (fun)
  (?identifier fun))

(defgeneric funtype-of (fun))

(defmethod funtype-of ((fun <slot-accessor-fun>))
  "slot-accessor")

(defmethod funtype-of ((fun <slot-init-fun>))
  "slot-init")

(defmethod funtype-of ((fun <constructor-fun>))
  "constructor")

(defmethod funtype-of ((fun <global-fun>))
  "global")

(defmethod funtype-of ((fun <global-generic-fun>))
  "global generic")

(defmethod funtype-of ((fun <local-fun>))
  "local")

(defmethod funtype-of ((fun <imported-fun>))
  "extern")


#module-end
