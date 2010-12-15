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
;;; Title: EuLisp Level-0 copy module
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule copy
  (syntax (syntax)
   import (root)
   export (deep-copy
           shallow-copy))

(defun identity (x) x)

(defgeneric deep-copy (obj))

(defgeneric shallow-copy (obj))

(defmethod deep-copy ((s <object>))
               (let ((cl (class-of s)))
                 (copy-loop
                  (allocate cl ())
                  s
                  1
                  (class-instance-size cl)
                  deep-copy)))

(defmethod shallow-copy ((s <object>))
               (let ((cl (class-of s)))
                 (copy-loop
                  (allocate cl ())
                  s
                  1
                  (class-instance-size cl)
                  identity)))

(defun copy-loop (new old index len copy)
        (if (%<= index len)
            (progn
              (setivar new index (copy (getivar old index)))
              (copy-loop new old (%+ index 1) len copy))
          new))

;; conses
(defmethod deep-copy ((c <cons>))
               (cons-copy-loop c deep-copy))

(defmethod shallow-copy ((c <cons>))
               (cons-copy-loop c identity))

(defun cons-copy-loop (c copy)
        (if (atom? c)
            (copy c)
          (cons (copy (car c))
                (cons-copy-loop (cdr c) copy))))

;; strings
(defmethod deep-copy ((s <string>))
               (shallow-copy s))

(defmethod shallow-copy ((s <string>))
               (list->string (string->list s)))

;; characters
(defmethod deep-copy ((c <simple-char>))
               (shallow-copy c))

(defmethod shallow-copy ((c <simple-char>))
               (integer->char (char->integer c)))

;; vectors
(defmethod deep-copy ((v <vector>))
               (let ((len (vector-size v) ))
                 (vector-copy-loop
                  (make-vector len ())
                  v
                  0
                  len
                  deep-copy)))

(defmethod shallow-copy ((v <vector>))
               (let ((len (vector-size v) ))
                 (vector-copy-loop
                  (make-vector len ())
                  v
                  0
                  len
                  identity)))

(defun vector-copy-loop (new old index len copy)
        (if (%< index len)
            (progn
              (vector-set! new index (copy (vector-ref old index)))
              (vector-copy-loop new old (%+ index 1) len copy))
          new))

;; tables
(defmethod deep-copy ((t <hash-table>))
               (let ((new (make-table (table-comparator t) (deep-copy (table-fill t)))))
                 (map-list
                  (lambda (k v)
                    ((setter table-ref) new k (deep-copy v)))
                  (table-keys t)
                  (table-values t))
                 new))

(defmethod shallow-copy ((t <hash-table>))
               (let ((new (make-table (table-comparator t) (table-fill t))))
                 (map-list
                  (lambda (k v)
                    ((setter table-ref) new k v))
                  (table-keys t)
                  (table-values t))
                 new))

;; floats
(defmethod deep-copy ((f <double-float>))
               (shallow-copy f))

(defmethod shallow-copy ((f <double-float>))
               (%+ f 0.0))

;;;-----------------------------------------------------------------------------
)  ;; End of module copy
;;;-----------------------------------------------------------------------------
