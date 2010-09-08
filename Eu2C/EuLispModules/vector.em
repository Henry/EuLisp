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
;;;  Problems:
;;    commented generic functions
;;    commented literal expander
;;;  Authors: Ingo Mohr
;;;-----------------------------------------------------------------------------

(defmodule vector
  (import (tail
           (only (%pair-size)
                 basic-list)
           (only (make-fpint)
                 int-i)
           (only (deep-copy
                  shallow-copy)
                 copy)
           (only (equal)
                 compare-generic))
   syntax (tail
           syntax-0)
   export (<vector>
           vector?
           vector-size
           vector-ref
           setf-vector-ref
           make-initialized-vector
           make-uninitialized-vector
           make-vector-with-init
           maximum-vector-index
           equal
           vector-equal
           ;;copy
           ;;vector-copy
           deep-copy
           shallow-copy
           initialize-vector-from-list
           primitive-vector-ref
           setf-primitive-vector-ref
           primitive-vector-size
           make-vector))

;;;-----------------------------------------------------------------------------
;;; <vector> and vector?
;;;-----------------------------------------------------------------------------
(%define-standard-class (<vector> <class>)
  <object>
  ((length type %unsigned-word-integer keyword length
           reader primitive-vector-size)
   (element keyword element
            type <object> ;new
            reader primitive-vector-ref
            writer setf-primitive-vector-ref))
  representation pointer-to-vector
  allocation multiple-size-card
  constructor (make-uninitialized-vector
               length)
  constructor (make-vector-with-init length element)
  predicate vector?
  )

(defgeneric (converter <vector>)(object))

(defun make-vector (length . init)
  (make-vector-with-init
   (%cast %unsigned-word-integer (make-swi length))
   (if init
       (car init)
     init)))


(%define-literal-expansion vector
  `(%literal ,<vector>  () ,elements) ; false vector is not expanded
  ;; `(%literal ,<vector>  () ,@(convert-vector-list elements)) ; false vector is not expanded
  )

;;(%annotate-function convert-vector-list interpreter elements)
;;
;;(%define-function (convert-vector-list <list>)
;;                  ((vector <vector>))
;;  (convert-vector-list1 vector #%I0 (primitive-vector-size vector)))
;;
;;(%define-function (convert-vector-list1 <list>)
;;                  ((vector <vector>)
;;                   (i %unsigned-word-integer)
;;                   (l %unsigned-word-integer ))
;;  (if (%eq i l) ()
;;      (cons (primitive-vector-ref vector i)
;;            (convert-vector-list1 vector (%plus i #%I1) l))))

;;(defun length(l)            ;; For what ? rr
;;  (make-fpint (%pair-size l))) ;;(((.. size-1 l)))

;;(defun size-1 (l)        ; better basic-pair ?
;;    (if (null? l)
;;      #%i0
;;      (%plus #%i1 (size-1 (cdr l)))))

(defun vector-ref (vec index)
  (primitive-vector-ref
   vec
   (%cast %unsigned-word-integer (make-swi index))))


(defun setf-vector-ref (vec index val)
  (setf-primitive-vector-ref
   vec
   (%cast %unsigned-word-integer (make-swi index)) val))

;;;-----------------------------------------------------------------------------
;;; maximum-vector-index
;;;-----------------------------------------------------------------------------
(defconstant maximum-vector-index #xffff)

;;;-----------------------------------------------------------------------------
;;; Size
;;;-----------------------------------------------------------------------------
(defun vector-size (object)
  (make-fpint (%cast %signed-word-integer (primitive-vector-size object))))

;;;-----------------------------------------------------------------------------
;;; make-initialized-vector
;;;-----------------------------------------------------------------------------
(defun make-initialized-vector elements
  (initialize-vector-from-list
   (make-uninitialized-vector
    (%cast %unsigned-word-integer (%pair-size elements))) #%I0 elements))

(%define-function (initialize-vector-from-list <vector>)
  ((vector <vector>)
   (i %unsigned-word-integer)
   (elements <list>))
  (if (null? elements) vector
    (progn (setf-primitive-vector-ref vector i (car elements))
           (initialize-vector-from-list
            vector
            (%plus i #%I1) (cdr elements)))))

;;;-----------------------------------------------------------------------------
;;; equal
;;;-----------------------------------------------------------------------------
(defmethod equal ((object1 <vector>) (object2 <vector>))
  (vector-equal object1 object2))

(defun vector-equal (object1 object2)
  (if (%eq (%cast %unsigned-word-integer
                  (primitive-vector-size object1))
           (%cast %unsigned-word-integer
                  (primitive-vector-size object2)))
      (compare-vectors object1 object2 #%I0
                       (%cast %unsigned-word-integer
                              (primitive-vector-size object2)))
    ()))

(%define-function (compare-vectors <object>)
  ((vector1 <vector>)
   (vector2 <vector>)
   (i %unsigned-word-integer)
   (end %unsigned-word-integer))
  (cond ((%ge i end) t)
        ((equal (primitive-vector-ref vector1 i)
                (primitive-vector-ref vector2 i))
         (compare-vectors vector1 vector2 (%plus i #%I1) end))
        (t ())))

;;(defmethod equal ((object1 <vector> (object2 <vector>)))
;;  (if (%eq (vector-size object1)
;;           (vector-size object2))
;;    (compare-vectors object1 object2 #%I0 (vector-size object2))
;;    ()))

;;(defun vector-equal (object1 object2)
;;  (if (%eq (%cast %unsigned-word-integer (vector-size object1))
;;           (%cast %unsigned-word-integer (vector-size object2)))
;;    (compare-vectors object1 object2 #%I0 (%cast %unsigned-word-integer
;;                                                 (vector-size object2)))
;;    ()))
;;
;;(%define-function (compare-vectors <object>)
;;                  ((vector1 <vector>) (vector2 <vector>)
;;                   (i %unsigned-word-integer) (end %unsigned-word-integer))
;;  (cond ((%ge i end) t)
;;        ((equal (primitive-vector-ref vector1 i)
;;                (primitive-vector-ref vector2 i))
;;         (compare-vectors vector1 vector2 (%plus i #%I1) end))
;;        (t ())))

;;;-----------------------------------------------------------------------------
;;; shallow-copy
;;;-----------------------------------------------------------------------------
(defmethod shallow-copy ((object <vector>))
  (initialize-vector-from-vector
   (make-uninitialized-vector (primitive-vector-size object))
   object #%I0
   (primitive-vector-size object)))

(defun vector-copy (object)
  (initialize-vector-from-vector
   (make-uninitialized-vector (primitive-vector-size object))
   object #%I0
   (primitive-vector-size object)))

(%define-function (initialize-vector-from-vector <vector>)
  ((vector <vector>)
   (src-vector <vector>)
   (i %unsigned-word-integer)
   (end %unsigned-word-integer))
  (if (%lt i end)
      (progn (setf-primitive-vector-ref
              vector
              i
              (primitive-vector-ref src-vector i))
             (initialize-vector-from-vector vector src-vector
                                            (%plus i #%I1) end))
    vector))


;;;-----------------------------------------------------------------------------
;;; deep-copy
;;;-----------------------------------------------------------------------------
;;(defmethod deep-copy ((object <vector>))
;;  (initialize-vector-from-vector
;;   (make-uninitialized-vector (primitive-vector-size object))
;;   object #%I0
;;   (primitive-vector-size object)))

(defmethod deep-copy ((object <vector>))
  (%let ((vec-size %unsigned-word-integer
                        (primitive-vector-size object)))
        (deep-copy-vec
         object
         (make-uninitialized-vector vec-size)
         vec-size
         #%I0)))

(%define-function (deep-copy-vec <vector>)
  ((old-vector <vector>)
   (new-vector <vector>)
   (leng %unsigned-word-integer)
   (index %unsigned-word-integer))
  (if (%lt index leng)
      (progn (setf-primitive-vector-ref
              new-vector
              index
              (deep-copy
               (primitive-vector-ref old-vector index)))
             (deep-copy-vec old-vector new-vector leng (%plus index #%I1)))
    new-vector))

;;***HGW converter <pair>
;;generic-prin
;;generic-write

;;;-----------------------------------------------------------------------------
;;; Type schemes for type inference
;;;-----------------------------------------------------------------------------

;; automatically generated

;;(%annotate-function
;; vector? new-signature
;; (((var0 var1)
;;   ((var var0) (atom? (not <null>)))
;;   ((var var1) (atom? <vector>)))
;;  ((var0 var1)
;;   ((var var0) (atom? <null>))
;;   ((var var1) (atom? (not <vector>))))))

(%annotate-function
  make-initialized-vector new-signature
  (((var0 var1)
    ((var var0) (atom? <vector>))
    ((var var1) (atom? <list>)))))

(%annotate-function
  vector-equal new-signature
  (((var0 var1 var2)
    ((var var0) (atom? <object>))
    ((var var1) (atom? <vector>))
    ((var var2) (atom? <vector>)))))

;;;-----------------------------------------------------------------------------
)  ;; end of vector
;;;-----------------------------------------------------------------------------
