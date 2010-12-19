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
;;; Title: simulation of runtime value stack and display
;;;  Library: comp (EuLisp to Bytecode Compiler -- EuLysses)
;;;  Authors: Andreas Kind, Keith Playford
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule cg-stack
  (syntax (_syntax-1)
   import (i-all
           sx-obj
           cg-state)
   export (push-display
           pop-display
           display-var-index
           move-stack
           push-stack-var
           stack-var-index))

;;;-----------------------------------------------------------------------------
;;; Simulating display movement for code generation
;;;-----------------------------------------------------------------------------
(defun push-display (vars state)
  (code-state-display! state (cons vars (code-state-display? state))))

(defun pop-display (n state)
  (if (= n 0) state
    (progn
      (code-state-display! state (cdr (code-state-display? state)))
      (pop-display (- n 1) state))))

(defun display-var-index (var state)
  (labels
   ((loop (display depth)
          (if (null? display)
              (ct-serious-warning () "no display-index for ~a"
                                  (var-name? var))
            (let ((pos (find1-list var (car display))))
              (if pos
                  (list depth pos)
                (loop (cdr display) (+ 1 depth)))))))
   (loop (code-state-display? state) 0)))

;;;-----------------------------------------------------------------------------
;;; Simulating stack movement for code generation
;;;-----------------------------------------------------------------------------
(defun move-stack (i state)
  (if (= i 0) ()
    (let* ((n (code-state-stack-size? state))
           (m (+ n i)))
      (if (< m 0)
          (ct-serious-warning () "stack underflow")
        (progn
          (code-state-stack-size! state m)
          (if (< i 0)
              (update-stack-vars m state)
            ()))))))

(defun push-stack-var (var state)
  (let ((n (code-state-stack-size? state))
        (l (code-state-stack-vars? state)))
    (move-stack 1 state)
    (code-state-stack-vars! state (cons n (cons var l)))))

(defun stack-var-index (var state)
  (labels
   ((loop (l)
          (if l
              (if (eq (cadr l) var)
                  (car l)
                (loop (cddr l)))
            (error <condition> "parameter not on stack"))))
   (- (code-state-stack-size? state)
      (+ (loop (code-state-stack-vars? state)) 1))))

(defun update-stack-vars (n state)
  (labels
   ((loop (l)
          (if (or (null? l) (< (car l) n))
              l
            (loop (cddr l)))))
   (code-state-stack-vars! state (loop (code-state-stack-vars? state)))))

;;;-----------------------------------------------------------------------------
)  ;; End of module cg-stack
;;;-----------------------------------------------------------------------------
