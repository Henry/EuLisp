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
;;;  Title: auxiliary functions for collections
;;;  Description: collection gives the functionality described in A.2
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Winfried Heicking
;;;-----------------------------------------------------------------------------

(defmodule collection-aux
  (import (apply
           tail
           eulisp-kernel
           (only (make-string
                  string-pointer
                  allocate-%string)
                 string-ii)
           collection-convert
           (only (binary+
                  binary<)
                 int
                 ;; number ;take not this: recursive module load
                 )
           basic-list
           (only (list
                  atom?)
                 pair)
           (only (eql)
                 compare)
           (only (print)
                 print)
           (only (<string>
                  string?
                  )
                 string)
           character
           vector
           (only (<table>
                  table?
                  table-ref
                  ?fill-value)
                 table)
           (only ($standard-table-size)
                 table-aux)
           (only (construct-collection-info ; all defgenerics
                  ;;construct-result ; it is a function now
                  take-next-elt)
                 collection-generic)
           collection-i
           (only (strlen) c-string-interface))
   syntax (tail
           apply
           syntax-0
           setf)
   export (;;construct-collection-info
           ;;construct-result
           ;;take-next-elt
           reverse-list
           test-range-indizes
           any?-collection
           concat-collection
           do-collection
           map-collection))

;; with funcall
;; (defun take-next-list-element (lst)
;;   (function (lambda ()
;;               (let (x)
;;                 (if lst
;;                     (progn
;;                       (setq x (car lst))
;;                       (setq lst (cdr lst))
;;                       x)
;;                   ())))))
;;
;; (setq xx (take-next-element '(a b c d)))
;;
;; (funcall xx)

(deflocal $end-string "end of collection")

;;;-----------------------------------------------------------------
;;first the aux functions

(defun or-aux (a b) (if a t (if b t ())))

(defun any?-collection (function lst  more-collections)
  (any?-with-apply
   function
   (mapc-more-collections
    (cons
     lst
     more-collections))
   ))


(defun do-collection (function lst  more-collections)
  (do-with-apply
   function
   (mapc-more-collections
    (cons
     lst
     more-collections))
   ))




;;;      ############## new 6.12.
(defun map-collection (function lst  more-collections)
  (construct-result
   lst
   (map-with-apply
    function
    (mapc-more-collections
     (cons
      lst
      more-collections)))))


(defun any?-with-apply (function collection-list)
  (let ((first-apply-arg
         (take-next-elt (car (car collection-list))
                        collection-list))
        (rest-elts
         (apply-rest-list
          (cdr collection-list) ())))
    (if
        (if (eq first-apply-arg $end-string)
            t
          (if (eq rest-elts $end-string)
              t
            ()))
        ()
      (if (apply function
                 first-apply-arg
                 rest-elts)
          t
        (any?-with-apply function collection-list)))))


(defun do-with-apply (function collection-list)
  (let ((first-apply-arg
         (take-next-elt (car (car collection-list))
                        collection-list))
        (rest-elts
         (apply-rest-list
          (cdr collection-list) ())))
    ;;(print first-apply-arg)
    ;;(print rest-elts)
    (if
        (if (eq first-apply-arg $end-string)
            t
          (if (eq rest-elts $end-string)
              t
            ()))
        ()
      (progn
        (apply function
               first-apply-arg
               rest-elts)
        (do-with-apply function collection-list))
      )))



;;; new 6.12.
(defun map-with-apply (function collection-list)
  (let ((first-apply-arg
         (take-next-elt (car (car collection-list))
                        collection-list))
        (rest-elts
         (apply-rest-list
          (cdr collection-list) ())))
    (if      ;; = (or (eq ...) (eq ...))
        (if (eq first-apply-arg $end-string)
            t
          (if (eq rest-elts $end-string)
              t
            ()))
        ()
      (cons (apply function
                   first-apply-arg
                   rest-elts)
            (map-with-apply function collection-list))
      )))



(defun mapc-more-collections (li)
  (cons (construct-collection-info (car li))
        (mapc-more-collections1 (cdr li) ()))
  )

(defun mapc-more-collections1 (li res)
  (if li
      (mapc-more-collections1
       (cdr li)
       (cons (construct-collection-info (car li)) res))

    res))


;;new 6.12.
(defmethod construct-collection-info  ((collection <table>))
  (list collection 0))

(defmethod construct-collection-info  ((collection <vector>))
  (list collection 0))


(defmethod construct-collection-info  ((collection <string>))
  (list collection 0))


(defmethod construct-collection-info  ((collection <list>))
  (cons collection ()))


(defmethod construct-collection-info (collection)
  (print "unknown type for collection"))



;;;new 6.12.
(defun construct-result (collection result)
  (convert result (%class-of collection)))



(%define-function (initialize-vector-from-list-rev <vector>)
  ((vector <vector>)
   (index %unsigned-word-integer)
   (elements <list>))
  (if (null? elements)
      vector
    (progn (setf-primitive-vector-ref vector index (car elements))
           (initialize-vector-from-list-rev vector
                                            (%minus index #%I1)
                                            (cdr elements)))))


(defmethod take-next-elt ((element <list>) (collection <object>))
  (next-list-elt collection))

(defmethod take-next-elt ((element <vector>) (collection <object>))
  (next-vector-elt collection))

(defmethod take-next-elt ((element <string>) (collection <object>))
  (next-string-elt collection))


;;new 6.12.
(defmethod take-next-elt ((element <table>) (collection <object>))
  (next-table-elt collection))


(defmethod take-next-elt ((element <object>) (collection <object>))
  (print 'unknown-collection))



(defun apply-rest-list (collection-list result-list)
  (if collection-list
      (let ((next-elt
             (take-next-elt (car (car collection-list))
                            collection-list)))
        (if (eq next-elt $end-string)
            $end-string
          (apply-rest-list (cdr collection-list)
                           (cons next-elt result-list))
          ))
    result-list))



;;*collection-infos*: ((list) ...)
(defun next-list-elt (liste)
  (let ((one-arg-list (car liste))
        (lst (car (car liste))))
    (if lst
        (progn (setf (car one-arg-list)   ;set cdr of list
                     (cdr lst))
               (car lst))
      $end-string)))


;;list of table-infos:: ((vector index) ...)
;;for vectors too!

(defun next-vector-elt (collection-info)
  (let ((index (car (cdr (car collection-info))))
        (vector (car (car collection-info)))
        res)
    (if (binary< index (vector-length vector)) ;=lenght of vector
        (progn
          (setq res
                (vector-ref vector index)) ;give element
          (setf (car (cdr (car collection-info)))        ;set index
                (binary+ index 1))
          res)
      $end-string)))

(defun next-string-elt (liste)
  (let ((index (car (cdr (car liste))))
        (str (car (car liste)))
        res)
    (if (binary< index (string-length str)) ;=lenght of string
        (progn
          (setq res
                (string-ref str index)) ;give element
          (setf (car (cdr (car liste)))        ;set index
                (binary+ index 1))
          res)
      $end-string)))



(defun next-table-elt (collection-info)
  (let* ((index (car (cdr (car collection-info))))
         (table (car (car collection-info)))
         (tab-res (table-ref table index)))
    (if (eq tab-res (?fill-value table))
        $end-string
      (progn (setf (car (cdr (car collection-info)))
                   (binary+ index 1))
             tab-res))))


(%define-function (test-range-indizes <object>)
  ((start %signed-word-integer)
   (end %signed-word-integer)
   (max-index %signed-word-integer))
  (cond ((%lt start #%i0)
         (print 'error-start-end-index-of-list-in-fill1)
         ())
        ((%lt end #%i0)
         (print 'error-start-end-index-of-list-in-fill2)
         ())
        ((%ge start max-index)
         (print 'error-start-end-index-of-list-in-fill3)
         ())
        ((%ge end max-index)
         (print 'error-start-end-index-of-list-in-fill4)
         ())
        ((%gt start end)
         (print 'error-start-end-index-of-list-in-fill5)
         ())
        (t t)))

(defun reverse-list (liste)
  (reverse-list-aux liste ()))

(defun reverse-list-aux (liste result)
  (if (cons? liste)
      (reverse-list-aux (cdr liste) (cons (car liste) result))
    result))

;;-------------------------------------

;;; concat for all collections (default)

;;  (defgeneric concat-collection (collection more-collections))
;;
;;  (defgeneric copy-to-list (collection result))
;;
;;  (defgeneric copy-to-vector (collection result start))
;;
;;  (defgeneric copy-to-string (collection result start))



;;  (defmethod concat-collection ((collection <list>)
;;                                (more-collections <list>))
;;    (let ((result (cons 1 ())))
;;      (conc-coll-list more-collections
;;                      (copy-list-to-list collection result))
;;      (cdr result)))



(defun concat-collection (collection more-collections)

  (let ((res (cons 1 ())))
    (conc-coll (cons collection more-collections) res)
    (convert (cdr res)
             (%class-of collection))))


(defun conc-coll (more-collections res)
  (if more-collections
      (conc-coll (cdr more-collections)
                 (nconc-wh res (convert (car more-collections) <list>)))
    res))

(defun nconc-wh (liste element)
  (if (cons? liste)
      (progn
        (nconc2 liste element)
        liste)
    element))

(defun nconc2 (liste element)
  (if (atom? (cdr liste))
      (setf (cdr liste) element)
    (nconc2 (cdr liste) element)))

;;  (defun nconc1 (liste element)
;;    (if (cons? liste)
;;      (if (atom? (cdr liste))
;;        (progn
;;          (setf (cdr liste) element)
;;          liste)
;;        (nconc1 (cdr liste) element))
;;      element))





)

;;eof
