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
;;;  Title: Type Inference of Constants
;;;  Description:
;;    Link between EuLisp constants and type expressions.
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Andreas Kind
;;;-----------------------------------------------------------------------------

#module ti-const
(import (ti lzs ti-lattice ti-exprs ti-signature
            (only (~class-of) lzs-mop)
            (only (expand-literal) expand-literal))
 syntax (ti)

 export (constant-type))

;;;-----------------------------------------------------------------------------
;;; CONSTANT TYPE EXPRESSIONS
;;;-----------------------------------------------------------------------------

(defgeneric constant-type (value))

(defmethod constant-type (value)        ; may be already a <literal-instance>
  (let* ((expanded-literal (expand-literal value))
         (lattice-type (find-lattice-type-for-literal expanded-literal)))
    (if (null? lattice-type)
        (setq lattice-type (?lattice-type (~class-of expanded-literal))))
    (lattice-type-to-atomic-type lattice-type)))

(defmethod constant-type ((value <null>))
  (<null>-type))

;;; Handling slot names.
(defmethod constant-type ((value <symbol>))
  (make <slot-id> :slot-name value))

;;; Handling function objects.
(defmethod constant-type ((value <fun>))
  (<function>-type))

(defmethod constant-type ((value <named-const>))
  (lattice-type-to-atomic-type (?lattice-type (?type value))))

#module-end
