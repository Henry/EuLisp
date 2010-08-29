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
;;;  Title: EL-in-CL: standard module stream
;;;  Description:
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Ingo Mohr
;;;-----------------------------------------------------------------------------

#module el-stream

(import
 ((except (let) eulisp-kernel) ; zur Sicherheit, da noch nicht zwischen export
  ;; und syntax-export unterschieden wird
  (only (*package*) common-lisp)
  (rename ((read cl:read)) common-lisp))

 syntax
 ((except (let) eulisp-kernel)
  (only (let &optional declare special) common-lisp)
  (rename ((defun cl:defun)) common-lisp))

 export
 (read)

 expose
 ((only (streamp close read-char read-byte peek-char write-char write-byte
                 print )
        common-lisp))
 )

(cl:defun read (&optional stream)
          (let ((*package* $eulisp-symbol-package))
            (declare (special *package*))
            (if stream
                (read-eulisp stream)
              (read-eulisp))))

#module-end
