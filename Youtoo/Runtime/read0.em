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
;;; Title: match-let syntax operator used in read
;;;  Library: level-1
;;;  Authors: Julian Padget, Andreas Kind
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule read0
  (syntax (boot0)
   import (level-1))

;;;-----------------------------------------------------------------------------
;;; (match-let (expression default-initializers) form*)
;;;-----------------------------------------------------------------------------
;;;  Examples of use
;;
;;;   (match-let foo ((a 1)) bar)
;;    => (let ((a (if (null? foo) 1 (car foo)))) bar)
;;
;;;   (match-let foo ((a 1) (b 2)) bar)
;;    => (let ((a ()) (b ()))
;;        (progn (if (null? foo) (setq a 1)
;;                   (progn (setq a (car foo)) (setq foo (cdr foo))))
;;               (if (null? foo) (setq b 2)
;;                   (progn (setq b (car foo)) (setq foo (cdr foo)))))
;;        bar)
;;
;;;   (match-let '(foo bar) ((a 1) (b 2)) baz)
;;    => (let ((G00055 (foo bar)) (a ()) (b ()))
;;        (progn (if (null? G00055) (setq a 1)
;;                   (progn (setq a (car G00055)) (setq G00055 (cdr G00055))))
;;               (if (null? G00055) (setq b 2)
;;                   (progn (setq b (car G00055)) (setq G00055 (cdr G00055)))))
;;        bar)
;;;-----------------------------------------------------------------------------
(defsyntax match-let (expression default-initializers . body)
  (if (eql 1 (size default-initializers))
      `(let ((,(caar default-initializers)
              (if (null? ,expression)
                  ,(cadr (car default-initializers))
                (car ,expression))))
         ,@body)
    (let* ((var (if (symbol? expression) expression (gensym)))
           (update-vars
            (labels
             ((loop (l)
                    (if (null? l)
                        ()
                      (cons
                       `(if (null? ,var)
                            (setq ,(caar l) ,(cadr (car l)))
                          (progn
                            (setq ,(caar l) (car ,var))
                            (setq ,var (cdr ,var))))
                       (loop (cdr l))))))
             (loop default-initializers))))
      `(let (,@(if (symbol? expression) () `((,var ,expression)))
             ,@(map (lambda (x) `(,(car x) ())) default-initializers))
         (progn ,@update-vars)
         ,@body))))

;;;-----------------------------------------------------------------------------
)  ;; End of module read0
;;;-----------------------------------------------------------------------------
