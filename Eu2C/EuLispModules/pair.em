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
;;    The functions length-pair, copy-alist, copy-list and copy-tree are tested in an Common Lisp
;;    environment.
;;;  Requires:
;;;  Problems:
;;;  Authors: Ingo Mohr
;;;-----------------------------------------------------------------------------

(defmodule pair

  (import (tail
           (only (consp) basic-syntax)
           basic-list
           basic-compare
           (only (make-fpint) eulisp-kernel)
           )
   syntax (tail
           (only (cond) syntax-0)
           )
   export (atom
           consp
           copy-alist
           copy-list
           copy-tree
           )
   expose ((only (<cons> cons car cdr list)
                 basic-list))
   )

;;(%define-function (consp <list>)       defined in basic-syntax
;;                  ((object <object>))
;;  (if (%eq (%class-of object) <cons>)
;;    object
;;    () ))

(defun atom (object)    ;consp? z.z. nicht mšglich 1.8.93
  (if (consp object)     ;versuch (?identifier NIL)???in el2lzs
      ()
    't))

;;(defmethod equal ((pair1 <cons>) (pair2 <cons>))
;;  (and (equal (car pair1) (car pair2))
;;       (equal (cdr pair1) (cdr pair2))))

;;(defmethod copy ((pair <cons>))
;;  (cons (car pair) (cdr pair)))

;;(defun list l l) aus basic-list basic-list-0 ;am 1.8.93 nicht moglich

;;(defmethod length ((pair <cons>))
;;  (length-pair pair))

(defun length-pair(pair)
  (make-fpint (length-pair-1)))

(defun length-pair-1 (pair)         ;s.o. consp?
  (if (consp (cdr pair))
      (%plus #%i1 (length-pair-1 (cdr pair)))
    #%i0))

(defun copy-alist (alist)
  (cond ((atom alist) alist)
        ((atom (car alist))
         (cons (car alist)
               (copy-alist (cdr alist))))
        (t (cons (cons (car (car alist))
                       (cdr (car alist)))
                 (copy-alist (cdr alist))))))

(defun copy-list (list)
  (if (atom list) list
    (cons (car list)
          (copy-list (cdr list)))))

(defun copy-tree (tree)
  (if (atom tree) tree
    (cons (copy-tree (car tree))
          (copy-tree (cdr tree)))))

;;generic-prin
generic-write

;;;-----------------------------------------------------------------------------
;;; Type schemes for type inference
;;;-----------------------------------------------------------------------------

(%annotate-function
  atom new-signature
  (((var0 var1)
    ((var var0) (atom (and <object> (not <null>))))
    ((var var1) (atom (and <object> (not <cons>)))))
   ((var0 var1)
    ((var var0) (atom <null>))
    ((var var1) (atom <cons>)))))

(%annotate-function
  copy-alist new-signature
  (((var0 var1)
    ((var var0) (atom <list>))
    ((var var1) (var var0)))))

(%annotate-function
  copy-list new-signature
  (((var0 var1)
    ((var var0) (atom <list>))
    ((var var1) (var var0)))))

(%annotate-function
  copy-tree new-signature
  (((var0 var1)
    ((var var0) (atom <list>))
    ((var var1) (var var0)))))

(%annotate-function
  length-pair-1 new-signature
  (((var0 var1)
    ((var var0) (atom %signed-word-integer))
    ((var var1) (atom <cons>)))))

(%annotate-function
  length-pair new-signature
  (((var0 var1)
    ((var var0) (atom <int>))
    ((var var1) (atom <cons>)))))

;;;-----------------------------------------------------------------------------

) ;end of module pair
