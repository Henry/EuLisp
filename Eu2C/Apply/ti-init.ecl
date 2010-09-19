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
;;; Title: Initialization of the Typ Inference System
;;;  Description:
;;    This module provides functions to reset and initialize the type
;;    inference system
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Andreas Kind
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

#module ti-init
(import (lzs mzs ti ti-codes ti-lattice ti-exprs ti-eqs ti-meet-join
             ti-write ti-signature
             (only (mapc maphash dolist) common-lisp))
 syntax (ti)
 export (ti-initialize update-all-type-expr-codes))

;;;-----------------------------------------------------------------------------
;;; INITIALIZATION
;;;-----------------------------------------------------------------------------

;; Initialize the type inference system.
(defun ti-initialize ()
  (reset-funs-with-defined-signatures)
  (reset-actual-type-var-id)
  (reset-def-strategic-lattice-types)
  (initialize-ti-statistics)
  (initialize-lattice)
  (add-sys-types))

;;;-----------------------------------------------------------------------------
;;; ADDING SYSTEM TYPES
;;;-----------------------------------------------------------------------------

;; Add the predefined system functions (TAIL functions) to the lattice.
(defun add-sys-types ()
  (def-strategic-lattice-type ^(%number (top) (bottom)))
  (setq *%integer*
        (def-strategic-lattice-type ^(%integer (%number) (bottom))))
  (def-strategic-lattice-type ^(%unsigned-integer (%integer) (bottom)))
  (def-strategic-lattice-type ^(%signed-integer (%integer) (bottom)))
  (def-sys-lattice-type ^(%unsigned-byte-integer (%unsigned-integer) (bottom)))
  (def-sys-lattice-type ^(%signed-byte-integer (%signed-integer) (bottom)))
  (def-sys-lattice-type ^(%unsigned-half-integer (%unsigned-integer) (bottom)))
  (def-sys-lattice-type ^(%signed-half-integer (%signed-integer) (bottom)))
  (def-sys-lattice-type ^(%unsigned-word-integer (%unsigned-integer) (bottom)))
  (def-sys-lattice-type ^(%signed-word-integer (%signed-integer) (bottom)))
  (def-strategic-lattice-type ^(%float (%number) (bottom)))
  (def-sys-lattice-type ^(%single-float (%float) (bottom)))
  (def-sys-lattice-type ^(%double-float (%float) (bottom)))
  (def-sys-lattice-type ^(%extended-float (%float) (bottom)))
  ;;----------------------------------------------------------------------------
  (setq *%void*
        (def-sys-lattice-type ^(%void (top) (bottom))))
  (setq *%false*
        (def-strategic-lattice-type ^(%false (top) (bottom))))
  ;;  (def-strategic-lattice-type ^(%character (top) (bottom)))
  (def-sys-lattice-type ^(%string (top) (bottom)))
  ;;----------------------------------------------------------------------------
  (def-strategic-lattice-type ^(%defined-type (top) (bottom)))
  ;;  (def-strategic-lattice-type ^(%pointer (%defined-type) (bottom)))
  ;;  (def-strategic-lattice-type ^(%vector (%defined-type) (bottom)))
  ;; All user defined classes are subclasses of %struct.
  (def-strategic-lattice-type ^(%struct (%defined-type) (bottom)))
  ;;  (def-strategic-lattice-type ^(%prestruct (%defined-type) (bottom)))
  ;;  (def-strategic-lattice-type ^(%union (%defined-type) (bottom)))
  ;;----------------------------------------------------------------------------
  (def-sys-lattice-type ^(%jmpbuf (top) (bottom)))
  (def-sys-lattice-type ^(%pjmpbuf (top) (bottom)))
  ;;----------------------------------------------------------------------------
  (setq *%function*
        (def-sys-lattice-type ^(%function (top) (bottom))))
  ;;----------------------------------------------------------------------------
  (def-sys-lattice-type ^(%sighandler (top) (bottom))))

;;;-----------------------------------------------------------------------------
;;; RECOMPUTE THE CODES OF ALL TYPE EXPRESSIONS
;;;-----------------------------------------------------------------------------

;; Signatures are read in before all lattice types are included into the
;; lattice. Thus before inference starts the type expression codes have to
;; be updated.
(defun update-all-type-expr-codes ()
  (ti-format t "~%Update all type expression codes ...")
  (update-lattice-type-expr-codes *top*)
  (mapc #'update-type-expr-codes (funs-with-defined-signatures))
  (ti-format t " done."))

;; Set all codes of lattice type expressions registered in the *the-lattice*
;; to the code of the corresponding lattice type.
(defun update-lattice-type-expr-codes (lattice-type)
  (let ((expr (?atomic-expr lattice-type)))
    (if expr
        (setf (?code expr) (?code lattice-type))))
  (mapc #'update-lattice-type-expr-codes (?subtypes lattice-type)))

(defun update-atomic-type-expr-codes (atomic-expr)
  (let ((new-code (?code (compute-to-atom (?name atomic-expr))))
        (old-code (?code atomic-expr)))
    (if (null? (eq-code? old-code new-code))
        (progn
          (setf (?code atomic-expr) new-code)
          (ti-format t "~%Notice: code of expr ~A updated"
                     (ti-print-string-no-cr atomic-expr))))))

(defgeneric update-type-expr-codes (obj))

(defmethod update-type-expr-codes ((obj <type-descr>))
  (dolist (equ (?equations (?type-vars obj)))
          (let ((left-expr (?left-expr equ))
                (right-expr (?right-expr equ)))
            (if (atomic-type? left-expr)
                (update-atomic-type-expr-codes left-expr))
            (if (atomic-type? right-expr)
                (update-atomic-type-expr-codes right-expr)))))

(defmethod update-type-expr-codes ((obj <fun>))
  (mapc #'update-type-expr-codes (?signature obj)))

#module-end
