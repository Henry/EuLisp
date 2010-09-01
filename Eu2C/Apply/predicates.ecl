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
;;;  Authors: Ingo Mohr
;;;-----------------------------------------------------------------------------

#module predicates
(import (eulisp0
         lzs
         accessors)
 syntax (eulisp0)
 expose ()
 export (*compilation-type*  ; set by `compile' in module apply-compiler
         *basic-system*      ; set by `load-basic-modules' in module apply-compiler
         signature-needed-for-code-generation-p
         exported-for-lisp-p
         exported-p
         class-sealed?
         generic-function-sealed-p
         unknown-applications-p
         is-lisp))

;;;-----------------------------------------------------------------------------
;;; Global variables
;;;-----------------------------------------------------------------------------
(deflocal *compilation-type* ())
;; The following values are supported: :application :basic-system

(deflocal *basic-system* ())
;; either () if no precompiled basic system is used
;; or the module describing the basic system (got from a .def-file)

;;;-----------------------------------------------------------------------------
;;; Functions
;;;-----------------------------------------------------------------------------
(defun signature-needed-for-code-generation-p (fun)
  ;;answer whether the type scheme can be set to ()
  (or (null? (eq *compilation-type* :application))
      (null? (special-sys-fun-p fun))))

;;;-----------------------------------------------------------------------------
;;; used after mark-as-exported was called in compile[apply-compiler]
;;;-----------------------------------------------------------------------------
(defun exported-for-lisp-p (obj)
  (and (eq *compilation-type* :basic-system)
       (global-p obj)
       (?exported obj)
       (null? (discriminating-fun-p obj))))

(defun exported-p (obj)
  (and (global-p obj)
       (?exported obj)))

(defun class-sealed? (class)
  ;; returns true if it is impossible to create an additional subclass for class
  ;; outside the compilation unit (i.e in using modules or at runtime)
  (or (null? (?exported class))
      (eq *compilation-type* :application)))

(defun generic-function-sealed-p (gf)
  ;; returns true if it is impossible to add additional methods outside the
  ;; compilation unit (i.e in using modules or at runtime)
  (or (null? (?exported gf))
      (eq *compilation-type* :application)))

;;;-----------------------------------------------------------------------------
;;; called after side effect analysis
;;;-----------------------------------------------------------------------------
(defun unknown-applications-p (fun)
  (or (?exported fun)
      (?expanded-literal fun)))

;;;-----------------------------------------------------------------------------
;;; tests for lisp functions
;;;-----------------------------------------------------------------------------
(defun is-lisp (obj)
  (or (null? (imported-p obj))
      (eq (?language obj) ^lisp)))

;;;-----------------------------------------------------------------------------
#module-end
;;;-----------------------------------------------------------------------------
