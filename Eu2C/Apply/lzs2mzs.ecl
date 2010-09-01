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

#module lzs2mzs
(import
 ((except (format) eulisp1)
  simple-programming
  LZS
  MZS
  context
  analyse-h
  vector ; make-vector and vector-ref
  arg-context
  function-call-context
  function-label
  join-label-context
  progn-context
  switch-context
  test-context
  void-context
  type-propagation
  type-inference
  (only (mapc format) common-lisp)
  lzs-to-mzs-fun
  cleartypes
  side-effects
  expand-literal
  apply-funs
  types-hand-on
  debugging
  )

 syntax
 (eulisp1)

 export
 (lzs2mzs)
 )


;;;-----------------------------------------------------------------------------
;;; (lzs2mzs /list-of-<module>-or-<modul>/) ---> /list-of-moduel/
;;;-----------------------------------------------------------------------------

(defun lzs2mzs (modul-or-list)
  (lzs2mzs1 (if (cons? modul-or-list) modul-or-list (list modul-or-list))))

(defun lzs2mzs1 (mlist)
  ;; -------------------------------------------
  (analysis-side-effects mlist)
  ;; (mapc #'lzs2mzs-modul mlist) only the top-level form from first module
  ;; -------------------------------------------
  (lzs2mzs-modul (car mlist))
  (mapc #'lzs2mzs-fun (get-functions-used-in-literals))
  (lzs2mzs-fun typecheck)
  (mapc #'lzs2mzs-exported-funs-in-modul (cdr mlist))
  ;;(lzs2mzs-fun no-applicable-method-error)
  ;; -------------------------------------------
  (info-format 1 "~%Reduce type schemes of statements ... ")
  (mapc #'clear-types-modul1 mlist)
  (info-format 1 "done.")
  ;; --------------------------------------------
  ;; second step: balance-applications
  (cond (*global-optimization*
         (info-format 1 "~%Global inference ... ")
         (mapc #'clear-types-modul2-global-optimization mlist)
         (info-format 1 "done."))
        (t
         (info-format 1 "~%Reduce type schemes of functions ... ")
         (mapc #'clear-types-modul2 mlist)
         (info-format 1 "done.")))
  ;; --------------------------------------------
  ;; third step: propagete new types
  (cond (*global-optimization*
         (info-format 1 "~%Reanalysis of function statements ... ")
         (types-hand-on-modules mlist)
         (info-format 1 "done.")))
  ;; --------------------------------------------
  ;; fourth step: balance types
  (cond (*global-optimization*
         (info-format 1 "~%Reduce type schemes of functions ... ")
         (mapc #'clear-types-modul1 mlist)
         (info-format 1 "done.")))
  ;; --------------------------------------------
  ;; fifth step: convert to sys-types
  ;;  (mapc #'reset-signature (?fun-list $tail-module))
  (info-format 1 "~%Convert type schemes to range and domain vectors ... ")
  (mapc #'clear-types-modul3 mlist)
  (info-format 1 "done.")
  (synthesis-types-in-side-effects mlist)
  mlist)

(defun lzs2mzs-exported-funs-in-modul (mod)
  (mapc #'lzs2mzs-exported-fun (?fun-list mod)))

(defun clear-types-modul1 (modul)
  (let ((funs (?fun-list modul))
        (main-fun (?toplevel-forms modul)))
    (mapc #'clear-types1 funs)
    (if main-fun (clear-types1 main-fun))))

(defun clear-types-modul2 (modul)
  (let ((funs (?fun-list modul))
        (main-fun (?toplevel-forms modul)))
    (mapc #'clear-types2 funs)
    (if main-fun (clear-types2 main-fun))))

(defun clear-types-modul2-global-optimization (modul)
  (let ((funs (?fun-list modul))
        (main-fun (?toplevel-forms modul)))
    (mapc #'clear-types2-global-optimization funs)
    (if main-fun (clear-types2-global-optimization main-fun))))

(defun clear-types-modul3 (modul)
  (let ((funs (?fun-list modul))
        (main-fun (?toplevel-forms modul)))
    (mapc #'clear-types3 funs)
    (if main-fun (clear-types3 main-fun))))

(defun lzs2mzs-modul (modul)
  (setq indent-counter 0)
  ;;  (mapc #'lzs2mzs-fun (?fun-list modul))
  (if (?toplevel-forms modul) (lzs2mzs-fun (?toplevel-forms modul))
    (format t "~%Warning: modul contains no top-level forms !!!"))
  (mapc #'lzs2mzs-exported-fun (?fun-list modul))
  ;;  (mapc #'clear-types (?fun-list modul))
  ;;  (if (?toplevel-forms modul) (clear-types (?toplevel-forms modul)) ())
  )

(defun lzs2mzs-exported-fun (fun)
  (if (eq (?pass fun) 3) ()
    (if (and (global-fun? fun) (?exported fun))
        (lzs2mzs-fun fun) ())))

#module-end
