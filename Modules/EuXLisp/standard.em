;;; Copyright 1994 Russell Bradford
;;; Copyright 2010 Henry G. Weller
;;;-----------------------------------------------------------------------------
;;  This file is part of
;;; ---                           EuLisp System 'EuXLisp'
;;;-----------------------------------------------------------------------------
;;
;;  EuXLisp is free software: you can redistribute it and/or modify it under the
;;  terms of the GNU General Public License version 2 as published by the Free
;;  Software Foundation.
;;
;;  EuXLisp is distributed in the hope that it will be useful, but WITHOUT ANY
;;  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;  details.
;;
;;  You should have received a copy of the GNU General Public License along with
;;  this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;-----------------------------------------------------------------------------
;;; Title: Some non-EuLisp functionality for Feel users
;;;  Authors: Russell Bradford
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule standard
  (import (level-0)
   export (subst
           nconc
           make-symbol
           symbol-name
           while
           for
           ++
           --
           defstruct)
   expose (level-0))

;; subst a for b in c
(defun subst (a b c)
  (if (list? c)
      (map (lambda (e) (if (binary= b e) a e)) c)
    (error <bad-type>
           "trying to to subst into a non-list"
           value: c
           expected-type: <list>)))

(defun nconc (a b)
  (if (null? a)
      b
    (progn ((setter cdr) (last-pair a) b)
           a)))

(defun make-symbol (string)
  (convert string <symbol>))

(defun symbol-name (symbol)
  (convert symbol <string>))

(defmacro for (init condition inc . body)
  `(progn
     ,init
     (while ,condition
       ,@body
       ,inc)))

(defmacro ++ (sym . inc)
  `(setq ,sym (+ ,sym ,(if (null? inc) 1 (car inc)))))

(defmacro -- (sym . inc)
  `(setq ,sym (- ,sym ,(if (null? inc) 1 (car inc)))))


(defmacro defstruct (name superclass slot-descriptions . class-options)
  `(defclass ,name
     (,(if (null? superclass) '<object> superclass))
     ,(map (lambda (slot)
             (if (atom? slot)
                 slot
               (map (lambda (sym)
                      (cond ((eq sym 'accessor) accessor:)
                            ((eq sym 'reader) reader:)
                            ((eq sym 'writer) writer:)
                            ((eq sym 'initform) default:)
                            ((eq sym 'initarg) keyword:)
                            (t sym)))
                    slot)))
           slot-descriptions)
     ,@(map (lambda (sym)
              (cond ((eq sym 'constructor) constructor:)
                    ((eq sym 'predicate) predicate:)
                    (t sym)))
            class-options)))

;;;-----------------------------------------------------------------------------
)  ;; End of module standard
;;;-----------------------------------------------------------------------------
