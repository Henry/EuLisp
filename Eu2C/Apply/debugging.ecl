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
;;; Title: Tools for Compiler Debugging
;;;  Description:
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Ingo Mohr
;;;-----------------------------------------------------------------------------

#module debugging
(import
 (level-0
  (only (format) common-lisp)
  (only (*ti-break*) configuration))

 syntax
 (level-0
  dynamic
  )

 export ; export-syntax
 (development-mode
  ti-verbose
  ti-break)

 export
 (*development-mode*            ; until macro exp. is hygienic in EL
  *ti-verbose*   ;; printing on/off during ti
  toggle-ti-verbose
  *ti-break*     ;; stop/go ahead at type clash
  toggle-ti-break
  toggle-code-debug
  code-debug
  analysed-fun     ;; answer which function is analysed now
  start-analyse-fun
  end-analyse-fun
  info-format)

 expose
 ((only (*global-optimization*) configuration))
 )

;;(expose (only (*global-optimization* *ti-break*) configuration))

(cl:import '(toggle-ti-verbose toggle-ti-break toggle-code-debug analysed-fun)
           (cl:find-package "CL-USER"))

(deflocal *development-mode* t)

(defmacro development-mode ()
  '*development-mode*)

(deflocal *code-debug* ())

(defun toggle-code-debug ()
  (if *code-debug*
      (setq *code-debug* ())
    (setq *code-debug* t)))

(defun code-debug () *code-debug*)

;;; Protocol and break flag for type inference
(deflocal *ti-verbose* ())

(defmacro ti-verbose ()
  '*ti-verbose*)

(defun toggle-ti-verbose ()
  (if *ti-verbose*
      (setq *ti-verbose* ())
    (setq *ti-verbose* t)))

(defmacro ti-break ()
  '*ti-break*)

(defun toggle-ti-break ()
  (if *ti-break*
      (setq *ti-break* ())
    (setq *ti-break* t)))

;;; What function is being analysed? Can be reimplemented with dynamic-let.
(deflocal *analysed-fun* '())

(defun start-analyse-fun (fun)
  (setq *analysed-fun* (cons fun *analysed-fun*)))

(defun end-analyse-fun (fun)
  (setq *analysed-fun* (cdr *analysed-fun*)))

(defun analysed-fun ()
  (if *analysed-fun*
      (car *analysed-fun*)
    ()))

(defun info-format (level string . args)
  (if (>= (dynamic *info-level*) level)
      (apply #'format t string args)))

#module-end
