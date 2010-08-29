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
;;;  Title: collection for vectors
;;;  Description: collection for vectors gives the functionality described in A.2
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Winfried Heicking
;;;-----------------------------------------------------------------------------


(defmodule collection-vector

  (import
   (tail
    apply
    eulisp-kernel
    ;;collection-aux
    (only (eql) compare)
    basic-list
    vector
    (only (print)
          print)
    (only (<conversion-condition> error)
          condition)

    collection-generic
    collection-aux
    )

   syntax
   (tail
    syntax-0)

   export
   (accumulate
    accumulate1
    anyp
    concatenate
    do
    element
    emptyp
    fill
    map
    member
    reverse
    size
    ;;converter
    ;;    generic-prin
    ;;    generic-write
    )
   )





;;;------------------------------------------------------------
;;; accumulate
;;;------------------------------------------------------------------


;;  (defmethod accumulate ((function <function>)
;;                         (object <object>)
;;                         (vec <vector>))
;;    (accumulate-vector function object vec))


(defmethod accumulate ((function <function>)
                       (object <object>)
                       (vec <vector>))
  (map-accumulate function vec object (primitive-vector-length vec) #%I0))


(%define-function (map-accumulate <object>)
  ((function <function>)
   (vec <vector>)
   (res <object>)
   (max-len %unsigned-word-integer)
   (index %unsigned-word-integer))
  (if (%lt index max-len)
      (map-accumulate function
                      vec
                      (function res (primitive-vector-ref vec index));take this if funcall ok
                      max-len
                      (%plus index #%I1))
    res))



;;;------------------------------------------------------------
;;; accumulate1
;;;------------------------------------------------------------


(defmethod accumulate1 ((function <function>)
                        (vec <vector>))
  (if (%eq (primitive-vector-length vec) #%I0)
      ()
    (map-accumulate function vec
                    (primitive-vector-ref vec #%I0) ;initial element
                    (primitive-vector-length vec)
                    #%I1))) ;start is second element


;;;------------------------------------------------------------
;;; anyp
;;;------------------------------------------------------------




;;  (defmethod anyp ((function <function>)
;;                   (vec <vector>) . more-collections)
;;    (anyp-vector function vec more-collections))


(defmethod anyp ((function <function>)
                 (vec <vector>) . more-collections)
  (%let ((rest-list-length %signed-word-integer
                           (%list-length more-collections)))
        (cond ((%eq rest-list-length #%i0)
               (anyp-with-one-vector function vec #%I0
                                     (primitive-vector-length vec)))
              ((%eq rest-list-length #%i1)
               (anyp-with-two-args function vec (car more-collections)))
              (t (anyp-collection function vec more-collections)))
        ))


(%define-function (anyp-with-one-vector <object>)
  ((function <function>)
   (vec <vector>)
   (index %unsigned-word-integer)
   (len %unsigned-word-integer))
  (if (%lt index len)
      (if (function (primitive-vector-ref vec index))
          t
        (anyp-with-one-vector function vec (%plus index #%I1) len))
    nil))

(defmethod anyp-with-two-args
  ((function <function>)
   (vec1 <vector>)
   (vec2 <vector>))
  (%let*((vec-length1 %unsigned-word-integer
                      (primitive-vector-length vec1))
         (vec-length2 %unsigned-word-integer
                      (primitive-vector-length vec2))
         (min-length %unsigned-word-integer
                     (if (%le vec-length1 vec-length2)
                         vec-length1
                       vec-length2)))
        (anyp-with-two-vectors function
                               vec1
                               vec2
                               min-length
                               #%I0)))

(%define-function (anyp-with-two-vectors <object>)
  ((function <function>)
   (vec1 <vector>)
   (vec2 <vector>)
   (min-length %unsigned-word-integer)
   (index %unsigned-word-integer))
  (if (%lt index min-length)
      (if (function (primitive-vector-ref vec1 index)
                    (primitive-vector-ref vec2 index))
          t
        (anyp-with-two-vectors function vec1 vec2 min-length
                               (%plus index #%I1)))
    nil))


(defmethod anyp-with-two-args
  ((function <function>)
   (vec1 <vector>)
   (collection <object>))
  (anyp-collection function vec1 (cons collection nil)))




;;;------------------------------------------------------------
;;; concatenate
;;;------------------------------------------------------------


(defmethod concatenate ((vec <vector>) . more-collections)
  (%let ((rest-list-length %signed-word-integer
                           (%list-length more-collections)))
        (cond ((%eq rest-list-length #%i0)
               (%let ((vec-length %unsigned-word-integer
                                  (primitive-vector-length vec)))
                     (concat-with-one-vector vec #%I0
                                             vec-length
                                             (make-uninitialized-vector
                                              vec-length))))
              ((%eq rest-list-length #%i1)
               (concat-with-two-args vec (car more-collections)))
              (t (concat-collection vec more-collections)))
        ))


(%define-function (concat-with-one-vector <vector>)
  ((vec <vector>)
   (index %unsigned-word-integer)
   (len %unsigned-word-integer)
   (result <vector>))
  (if (%lt index len)
      (progn (setf-primitive-vector-ref
              result
              index
              (primitive-vector-ref vec index))
             (concat-with-one-vector vec
                                     (%plus index #%I1) len result))
    result))


(%define-function (concat-next-vector <vector>)
  ((src-vec <vector>)
   (src-length %unsigned-word-integer)
   (index %unsigned-word-integer)
   (start %unsigned-word-integer)
   (result <vector>))
  (if (%lt index src-length)
      (progn (setf-primitive-vector-ref
              result
              start
              (primitive-vector-ref src-vec index))
             (concat-next-vector src-vec src-length
                                 (%plus index #%I1)
                                 (%plus start #%I1)
                                 result))
    result))

(defmethod concat-with-two-args ((vec1 <vector>) (vec2 <vector>))
  (%let ((vec-length1 %unsigned-word-integer
                      (primitive-vector-length vec1))
         (vec-length2 %unsigned-word-integer
                      (primitive-vector-length vec2)))
        (concat-with-two-vectors
         vec1
         vec-length1
         vec2
         vec-length2
         #%I0
         (make-uninitialized-vector
          (%plus vec-length1 vec-length2)))))

;;;; result should be a list because of <null> !!!
;;  (defmethod concat-with-two-args ((no-vec <null>) (vec <vector>))
;;    (%let ((vec-length %unsigned-word-integer
;;                                (primitive-vector-length vec)))
;;      (concat-with-one-vector vec #%I0
;;                              vec-length
;;                              (make-uninitialized-vector
;;                               vec-length))))


(defmethod concat-with-two-args ((no-vec <null>) (vec <vector>))
  (vector-to-list vec
                  #%I0
                  (primitive-vector-length vec)))


(%define-function (vector-to-list <list>)
  ((vec <vector>)
   (index %unsigned-word-integer)
   (vec-length %unsigned-word-integer))
  (if (%lt index vec-length)
      (cons (primitive-vector-ref vec index)
            (vector-to-list vec
                            (%plus index #%I1)
                            vec-length))
    ()))


(defmethod concat-with-two-args ((vec <vector>) (no-vec <null>))
  (%let ((vec-length %unsigned-word-integer
                     (primitive-vector-length vec)))
        (concat-with-one-vector vec #%I0
                                vec-length
                                (make-uninitialized-vector
                                 vec-length))))


(%define-function (concat-with-two-vectors <object>)
  ((vec1 <vector>)
   (vec-length1 %unsigned-word-integer)
   (vec2 <vector>)
   (vec-length2 %unsigned-word-integer)
   (index %unsigned-word-integer)
   (result-vector <vector>))
  (concat-with-one-vector vec1 index vec-length1 result-vector)
  (concat-next-vector vec2 vec-length2 #%I0 vec-length1
                      result-vector)
  result-vector)

(defmethod concat-with-two-args
  ((vec <vector>) (collection <object>))
  (concat-collection vec (cons collection nil)))



;;;------------------------------------------------------------
;;; do
;;;------------------------------------------------------------




;;  (defmethod do ((function <function>)
;;                 (vec <vector>) . more-collections)
;;    (do-vector function vec more-collections))


(defmethod do ((function <function>)
               (vec <vector>) . more-collections)
  (%let ((rest-list-length %signed-word-integer
                           (%list-length more-collections)))
        (cond ((%eq rest-list-length #%i0)
               (do-with-one-vector function vec #%I0
                                   (primitive-vector-length vec)))
              ((%eq rest-list-length #%i1)
               (do-with-two-args function vec (car more-collections)))
              (t (do-collection function vec more-collections)))
        ))

(%define-function (do-with-one-vector <object>)
  ((function <function>)
   (vec <vector>)
   (index %unsigned-word-integer)
   (len %unsigned-word-integer))
  (if (%lt index len)
      (progn (function (primitive-vector-ref vec index))
             (do-with-one-vector function vec (%plus index #%I1) len))
    nil))

(defmethod do-with-two-args
  ((function <function>)
   (vec1 <vector>)
   (vec2 <vector>))
  (%let*((vec-length1 %unsigned-word-integer
                      (primitive-vector-length vec1))
         (vec-length2 %unsigned-word-integer
                      (primitive-vector-length vec2))
         (min-length %unsigned-word-integer
                     (if (%le vec-length1 vec-length2)
                         vec-length1
                       vec-length2)))
        (do-with-two-vectors function
                             vec1
                             vec2
                             min-length
                             #%I0)))

(%define-function (do-with-two-vectors <object>)
  ((function <function>)
   (vec1 <vector>)
   (vec2 <vector>)
   (min-length %unsigned-word-integer)
   (index %unsigned-word-integer))
  (if (%lt index min-length)
      (progn (function (primitive-vector-ref vec1 index)
                       (primitive-vector-ref vec2 index))
             (do-with-two-vectors function vec1 vec2 min-length
                                  (%plus index #%I1)))
    nil))

(defmethod do-with-two-args
  ((function <function>)
   (vec1 <vector>)
   (collection <object>))
  (do-collection function vec1 (cons collection nil)))



;;do-vector is equal to do-list ... and can be used for all collections!!!

;;  (defun do-vector (function vec . more-collections)
;;    (map-do-apply
;;     function
;;     (mapc-more-collections
;;      (cons
;;       (construct-collection-info vec)
;;       nil)
;;      more-collections)
;;     ))


;;;------------------------------------------------------------
;;; element and (setter element)
;;;------------------------------------------------------------


(defmethod element ((vec <vector>)
                    (key <fixed-precision-integer>))
  (vector-ref vec key))


(defmethod (setter element) ((vec <vector>)
                             (key <fixed-precision-integer>)
                             (value <object>))
  (setf-vector-ref vec key value))


;;;------------------------------------------------------------
;;; emptyp
;;;------------------------------------------------------------


(defmethod emptyp ((vec <vector>))
  (if (%eq (primitive-vector-length vec) #%I0)
      t
    ()))


;;  (defun emptyp-vector (vec)
;;    (if (%eq (primitive-vector-length vec) #%I0)
;;      t
;;      ()))


;;;------------------------------------------------------------
;;; fill
;;;------------------------------------------------------------


;;  (defmethod fill ((vec <vector>)
;;                   (object <character>) ;object to fill
;;                   (start <fixed-precision-integer>)
;;                   (end <fixed-precision-integer>))
;;    (fill-vector vec object start end))


(defmethod fill ((vec <vector>)
                 (object <object>) . keys)
  (%let ((vec-len %signed-word-integer
                  (%cast %signed-word-integer
                         (primitive-vector-length vec)))
         (rest-list-length %signed-word-integer
                           (%list-length keys)))
        (if (%eq #%i0 rest-list-length)
            (progn
              (fill-vector-aux vec object #%I0
                               (%cast %unsigned-word-integer
                                      (%minus vec-len #%i1)))
              nil)
          (if (%eq #%i1 rest-list-length)
              (error "fill: collection does not have natural order"
                     <conversion-condition>)
            (%let ((start %signed-word-integer
                          (make-swi (car keys)))
                   (end %signed-word-integer
                        (make-swi (car (cdr keys)))))
                  (if (test-range-indizes start end vec-len)
                      (progn
                        (fill-vector-aux
                         vec
                         object
                         (%cast %unsigned-word-integer start)
                         (%cast %unsigned-word-integer end))
                        nil)
                    nil))))))


(%define-function (fill-vector-aux %void)
  ((vec <vector>)
   (object <object>)
   (start %unsigned-word-integer)
   (end %unsigned-word-integer))
  (if (%gt start end)
      nil
    (progn (setf-primitive-vector-ref vec start object)
           (fill-vector-aux vec object (%plus start #%I1) end))))



;;------------------------------------------------------------



;;  (defmethod map ((function <function>)
;;                  (vec <vector>) . more-collections)
;;    (map-vector function vec more-collections))


(defmethod map ((function <function>)
                (vec <vector>) . more-collections)
  (%let ((rest-list-length %signed-word-integer
                           (%list-length more-collections)))
        (cond ((%eq rest-list-length #%i0)
               (%let ((vec-length %unsigned-word-integer
                                  (primitive-vector-length vec)))
                     (map-with-one-vector function vec #%I0
                                          vec-length
                                          (make-uninitialized-vector
                                           vec-length))))
              ((%eq rest-list-length #%i1)
               (map-with-two-args function vec (car more-collections) nil))
              (t (map-collection function vec more-collections)))
        ))

(%define-function (map-with-one-vector <vector>)
  ((function <function>)
   (vec <vector>)
   (index %unsigned-word-integer)
   (len %unsigned-word-integer)
   (result <vector>))
  (if (%lt index len)
      (progn (setf-primitive-vector-ref
              result
              index
              (function (primitive-vector-ref vec index)))
             (map-with-one-vector function vec
                                  (%plus index #%I1) len result))
    result))

(defmethod map-with-two-args
  ((function <function>)
   (vec1 <vector>)
   (vec2 <vector>)
   (not-used <object>))
  (%let* ((vec-length1 %unsigned-word-integer
                       (primitive-vector-length vec1))
          (vec-length2 %unsigned-word-integer
                       (primitive-vector-length vec2))
          (min-length %unsigned-word-integer
                      (if (%le vec-length1 vec-length2)
                          vec-length1
                        vec-length2)))
         (map-with-two-vectors function
                               vec1
                               vec2
                               min-length
                               #%I0
                               (make-uninitialized-vector
                                min-length))))

(%define-function (map-with-two-vectors <object>)
  ((function <function>)
   (vec1 <vector>)
   (vec2 <vector>)
   (min-length %unsigned-word-integer)
   (index %unsigned-word-integer)
   (result-vector <vector>))
  (if (%lt index min-length)
      (progn (setf-primitive-vector-ref
              result-vector
              index
              (function (primitive-vector-ref vec1 index)
                        (primitive-vector-ref vec2 index)))
             (map-with-two-vectors function vec1 vec2 min-length
                                   (%plus index #%I1) result-vector))
    result-vector))

(defmethod map-with-two-args
  ((function <function>)
   (vec1 <vector>)
   (collection <object>)
   (not-used <object>))
  (map-collection function vec1 (cons collection nil)))


;;map-vector is equal to map-list... and can be used for all collections!!!

;;  (defun map-vector (function vec . more-collections)
;;    (map-tab-apply
;;     function
;;     (mapc-more-collections
;;      (cons
;;       (construct-collection-info vec)
;;       nil)
;;      more-collections)
;;     nil
;;     vec))

;;;------------------------------------------------------------
;;; member
;;;------------------------------------------------------------


;;  (defmethod member ((object <object>)
;;                     (vec <vector>)
;;                     (test <function>))
;;    (member-vector object vec test))


(defmethod member ((object <object>)
                   (vec <vector>) . test)
  (let ((test-fct (if test
                      (car test)
                    eql)))
    (if (eq test-fct eq)
        (memq-vector object vec #%I0 (primitive-vector-length vec))
      (member-vector-aux object vec #%I0
                         (primitive-vector-length vec)
                         test-fct))))

(%define-function (member-vector-aux <object>)
  ((object <object>)
   (vec <vector>)
   (index %unsigned-word-integer)
   (len %unsigned-word-integer)
   (test <function>))
  (if (%lt index len)
      (if (test (primitive-vector-ref vec index) object)
          t
        (member-vector-aux object vec (%plus index #%I1) len test))
    nil))


(%define-function (memq-vector <object>)
  ((object <object>)
   (vec <vector>)
   (index %unsigned-word-integer)
   (len %unsigned-word-integer))
  (if (%lt index len)
      (if (eq (primitive-vector-ref vec index) object)
          t
        (memq-vector object vec (%plus index #%I1) len))
    nil))

;;;------------------------------------------------------------
;;; reverse
;;;------------------------------------------------------------


(defmethod reverse ((vec <vector>))
  (%let* ((vec-len %unsigned-word-integer
                   (primitive-vector-length vec))
          (rev-vec <vector>
                   (make-uninitialized-vector vec-len)))
         (if (%eq vec-len #%I0)
             rev-vec
           (reverse-vector-aux vec rev-vec #%I0
                               (%minus vec-len #%I1)))))


;;  (defun reverse-vector (vec)
;;    (%let* ((vec-len %unsigned-word-integer
;;                     (primitive-vector-length vec))
;;            (rev-vec <vector>
;;                     (make-uninitialized-vector vec-len)))
;;           (if (%eq vec-len #%I0)
;;             rev-vec
;;             (reverse-vector-aux vec rev-vec #%I0
;;                                 (%minus vec-len #%I1)))))

(%define-function (reverse-vector-aux <vector>)
  ((vec <vector>)
   (rev-vec <vector>)
   (index %unsigned-word-integer)
   (vec-len %unsigned-word-integer))
  (if (%le index vec-len)
      (progn
        (setf-primitive-vector-ref
         rev-vec
         index
         (primitive-vector-ref vec (%minus vec-len index)))
        (reverse-vector-aux vec rev-vec (%plus index #%I1) vec-len))
    rev-vec)
  )




;;;------------------------------------------------------------
;;; size
;;;------------------------------------------------------------


(defmethod size ((vec <vector>))
  (vector-length vec))


;;  (defun size-vector (vec)
;;    (vector-length vec))

)

;;;eof

