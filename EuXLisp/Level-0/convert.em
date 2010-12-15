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
;;; Title: EuLisp Level-0 convert module
;;;  Description:
;;    from\to  string  list  vector  table  symbol  char  integer  float  number
;;
;;    string      X     X       X      X      X                             X
;;    list        X     X       X      X
;;    vector      X     X       X      X
;;    table       X     X       X      X
;;    symbol      X                           X
;;    char        X                           X       X       X
;;    integer     X                           X       X       X       X
;;    float       X                           X               X       X
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule convert
  (syntax (syntax)
   import (root
           setter
           condition
           thread)
   export (converter
           convert
           <conversion-condition>
           <no-converter>))

(deflocal converter-table (make-table))

(defun converter (cl)
        (table-ref converter-table cl))

((setter setter) converter
 (lambda (cl fun)
   ((setter table-ref) converter-table cl fun)))

(defcondition <conversion-condition> <condition>
  ()
  abstract?: t)

(defcondition <no-converter> <conversion-condition>
              ((source default: ())
               (class default: ())))

(defun convert (obj cl)
        (if (eq (class-of obj) cl)
            obj
          (let ((conv (converter cl)))
            (if (function? conv)
                (conv obj)
              (no-converter obj cl)))))

(defun no-converter (obj class)
        (error <no-converter>
               "no such converter"
               source: obj
               class: class))

(defgeneric converter->string (obj))
(defmethod converter->string ((obj <symbol>))
               (symbol->string obj))
(defmethod converter->string ((obj <list>))
               (list->string obj))
(defmethod converter->string ((obj <char>))
               (make-string 1 obj))
(defmethod converter->string ((obj <vector>))
               (list->string (vector->list obj)))
(defmethod converter->string ((obj <table>))
               (list->string (table-values obj)))
(defmethod converter->string ((obj <number>))
               (number->string obj))
(defmethod converter->string ((obj <string>))
               obj)

((setter converter) <string> converter->string)

(defgeneric converter->symbol (obj))
(defmethod converter->symbol ((obj <string>))
               (string->symbol obj))
(defmethod converter->symbol ((obj <symbol>))
               obj)
(defmethod converter->symbol ((obj <number>))
               (string->symbol (number->string obj)))
(defmethod converter->symbol ((obj <char>))
               (string->symbol (make-string 1 obj)))

((setter converter) <symbol> converter->symbol)

(defgeneric converter->list (obj))
(defmethod converter->list ((obj <vector>))
               (vector->list obj))
(defmethod converter->list ((obj <string>))
               (string->list obj))
(defmethod converter->list ((obj <table>))
               (table-values obj))
(defmethod converter->list ((obj <list>))
               obj)

((setter converter) <list> converter->list)

(defgeneric converter->cons (obj))
(defmethod converter->cons ((obj <vector>))
               (if (%= (vector-size obj) 0)
                   (no-converter obj <cons>)
                 (vector->list obj)))
(defmethod converter->cons ((obj <string>))
               (if (string-null? obj)
                   (no-converter obj <cons>)
                 (string->list obj)))
(defmethod converter->cons ((obj <table>))
               (let ((vals (table-values obj)))
                 (if (null? vals)
                     (no-converter obj <cons>)
                   vals)))
(defmethod converter->cons ((obj <cons>))
               obj)

((setter converter) <cons> converter->cons)

(defgeneric converter->null (obj))
(defmethod converter->null ((obj <vector>))
               (if (%= (vector-size obj) 0)
                   ()
                 (no-converter obj <null>)))
(defmethod converter->null ((obj <string>))
               (if (string-null? obj)
                   ()
                 (no-converter obj <null>)))
(defmethod converter->null ((obj <table>))
               (let ((vals (table-values obj)))
                 (if (null? vals)
                     ()
                   (no-converter obj <null>))))
(defmethod converter->null ((obj <null>))
               obj)

((setter converter) <null> converter->null)

(defgeneric converter->vector (obj))
(defmethod converter->vector ((obj <list>))
               (list->vector obj))
(defmethod converter->vector ((obj <string>))
               (list->vector (string->list obj)))
(defmethod converter->vector ((obj <table>))
               (list->vector (table-values obj)))
(defmethod converter->vector ((obj <vector>))
               obj)

((setter converter) <vector> converter->vector)

(defgeneric converter->char (obj))
(defmethod converter->char ((obj <integer>))
               (integer->char obj))
(defmethod converter->char ((obj <char>))
               obj)
(defmethod converter->char ((obj <string>))
               (if (%= (string-size obj) 1)
                   (string-ref obj 0)
                 (no-converter obj <string>)))

((setter converter) <char> converter->char)
((setter converter) <simple-char> converter->char)

(defgeneric converter->integer (obj))
(defmethod converter->integer ((obj <char>))
               (char->integer obj))
(defmethod converter->integer ((obj <float>))
               (floor obj))
(defmethod converter->integer ((obj <string>))
               (converter->integer (string->number obj)))
(defmethod converter->integer ((obj <integer>))
               obj)

((setter converter) <integer> converter->integer)
((setter converter) <fpi> converter->integer)

(defgeneric converter->float (obj))
(defmethod converter->float ((obj <integer>))
               (%+ obj 0.0))
(defmethod converter->float ((obj <string>))
               (converter->float (string->number obj)))
(defmethod converter->float ((obj <float>))
               obj)

((setter converter) <float> converter->float)
((setter converter) <double-float> converter->float)

(defgeneric converter->number (obj))
(defmethod converter->number ((obj <string>))
               (string->number obj))

((setter converter) <number> converter->number)

(defgeneric converter->table (obj))
(defmethod converter->table ((obj <list>))
               (make-index-table (make-table eql) obj 0))
(defmethod converter->table ((obj <vector>))
               (make-index-table (make-table eql) (vector->list obj) 0))
(defmethod converter->table ((obj <string>))
               (make-index-table (make-table eql) (string->list obj) 0))
(defmethod converter->table ((obj <table>))
               obj)

(defun make-index-table (table list index)
        (while (cons? list)
          (table-set! table index (car list))
          (setq list (cdr list))
          (setq index (%+ index 1)))
        table)

((setter converter) <table> converter->table)
((setter converter) <hash-table> converter->table)

;;;-----------------------------------------------------------------------------
)  ;; End of module convert
;;;-----------------------------------------------------------------------------
