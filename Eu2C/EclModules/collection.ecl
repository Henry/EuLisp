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
;;; Title: EL-in-CL: standard module collection
;;;  Description:
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Ingo Mohr
;;;-----------------------------------------------------------------------------

#module collection
(import (eulisp-kernel
         list
         (only (<vector>)
               vector)
         (only (<string>)
               string)
         (only (<table>)
               table)
         (only (<character>)
               character)
         function
         (only (<cons>
                <null>
                <list>)
               list)
         (only (type-of
                coerce
                get
                ;;funcall
                ;;apply
                car
                cdr
                cadr
                length
                elt + = cons eq list print < string vector error
                ) common-lisp)
         (rename ((map cl:map)
                  (find cl:find)
                  (concatenate cl:concatenate)
                  (fill cl:fill))
                 common-lisp))
 syntax (eulisp-kernel
         (only (defsetf
                 setf
                 case)
               common-lisp))
 export (accumulate
         accumulate1
         any?
         collection?
         concatenate
         do
         element
         setter-element
         empty?
         fill
         map
         member
         sequence?
         size)
 expose ((only (reverse)
               common-lisp)))


(make-eulisp-class collection sequence)

;;(defun member (element collection)
;;(find element collection))

(deflocal $end-string "end of collection")

;;;-----------------------------------------------------------------------------
;;; converter
;;;-----------------------------------------------------------------------------
;; e.g.: (convert '(a b c) <list>)

;;(defmacro convert (collection dest-class)
;;  `(let ((coll ,collection) (class ',dest-class))
;;     (coerce coll
;;          (case class
;;            (<list> 'cl:list)
;;            (<null> 'cl:list)
;;            (<vector> 'cl:vector)
;;            (<string> 'cl:string)
;;            (<character> 'cl:character)
;;            (t ())))))

(defmacro convert (collection dest-class)
  `(let ((coll ,collection) (class ,dest-class))
     (coerce coll
             (cond
               ((eq class <list>) 'cl:list)
               ((eq class <null>) 'cl:list)
               ((eq class <vector>) 'cl:vector)
               ((eq class <string>) 'cl:string)
               ((eq class <character>) 'cl:character)
               (t ())))))

;;;-----------------------------------------------------------------------------
;;; accumulate
;;;-----------------------------------------------------------------------------
(defmethod accumulate ((function <function>)
                       (object <object>)
                       (lst <cons>))
  (map-accumulate-list function lst object))

(defmethod accumulate ((function <function>)
                       (object <object>)
                       (lst <null>))
  object)


(defun map-accumulate-list (function lst res)
  (if (cons? lst)
      (map-accumulate-list function
                           (cdr lst)
                           (funcall function res (car lst))
                           )
    res))
(defmethod accumulate ((function <function>)
                       (object <object>)
                       (vec <vector>))
  (map-accumulate-vector function vec object (length vec) 0))


(defun map-accumulate-vector (function vec res max-len index)
  (if (< index max-len)
      (map-accumulate-vector function
                             vec
                             (funcall function res (elt vec index))
                             max-len
                             (+ index 1))
    res))

(defmethod accumulate ((function <function>)
                       (object <object>)
                       (str <string>))
  (map-accumulate-vector function str object
                         (length str) 0))

;;;-----------------------------------------------------------------------------
;;; accumulate1
;;;-----------------------------------------------------------------------------
(defmethod accumulate1 ((function <function>)
                        (lst <cons>))
  (map-accumulate-list function (cdr lst) (car lst)))

(defmethod accumulate1 ((function <function>)
                        (lst <null>))
  lst)


(defmethod accumulate1 ((function <function>)
                        (vec <vector>))
  (if (= (length vec) 0)
      ()
    (map-accumulate-vector function vec
                           (elt vec 0) ;initial element
                           (length vec)
                           1))) ;start is second element


(defmethod accumulate1 ((function <function>)
                        (str <string>))
  (if (empty?-string str)
      ()
    (map-accumulate-vector function str
                           (elt str 0)
                           (length str)
                           1)))
(defun empty?-string (str)
  (if (= (length str) 0)
      t
    ()))

;;;-----------------------------------------------------------------------------
;;; any?
;;;-----------------------------------------------------------------------------
(defmethod any? ((function <function>)
                 (lst <cons>) . more-collections)
  (any?-collection function lst more-collections))


(defmethod any? ((function <function>)
                 (lst <null>) . more-collections)
  ())


(defmethod any? ((function <function>)
                 (str <vector>) . more-collections)
  (any?-collection function str more-collections))


(defmethod any? ((function <function>)
                 (str <string>) . more-collections)
  (any?-collection function str more-collections))

(defun any?-collection (function lst  more-collections)
  (any?-with-apply
   function
   (mapc-more-collections
    (cons
     lst
     more-collections))
   ))

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

(defun any?-with-apply (function collection-list)
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
      (if (apply function
                 first-apply-arg
                 rest-elts)
          t
        (any?-with-apply function collection-list)))))


(defmethod construct-collection-info  ((collection <list>))
  (cons collection ()))


(defmethod construct-collection-info  ((collection <vector>))
  (list collection 0))

(defmethod construct-collection-info  ((collection <string>))
  (list collection 0))

(defmethod construct-collection-info (collection)
  (print "unknown type for collection"))

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


(defmethod take-next-elt ((element <list>) (collection <object>))
  (next-list-elt collection))

(defmethod take-next-elt ((element <vector>) (collection <object>))
  (next-vector-elt collection))

(defmethod take-next-elt ((element <string>) (collection <object>))
  (next-vector-elt collection))


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
    (if (< index (length vector)) ;=lenght of vector
        (progn
          (setq res
                (elt vector index)) ;give element
          (setf (car (cdr (car collection-info)))        ;set index
                (+ index 1))
          res)
      $end-string)))


(defmethod collection? ((object <list>)) t)

(defmethod collection? ((object <vector>)) t)

(defmethod collection? ((object <string>)) t)

(defmethod collection? ((object <table>)) t)

(defmethod collection? (object) ())


(defun concatenate (collection . more-collections)
  (apply #'cl:concatenate
         (type-of collection)
         ;;'list
         collection more-collections))

;;(defmethod concatenate ((collection <vector>) . more-collections)
;;  (apply #'cl:concatenate 'vector collection more-collections))
;;
;;(defmethod concatenate ((collection <string>) . more-collections)
;;  (apply #'cl:concatenate 'string collection more-collections))


;;(defmethod do ((function <function>)
;;               (collection <null>) . more-collections)
;;  ())

(defun do (function collection . more-collections)
  ;;(collection <cons>) . more-collections)
  (apply #'cl:map
         (type-of collection)
         function collection more-collections)
  ())

;;(defmethod do ((function <function>)
;;               (collection <vector>) . more-collections)
;;  (apply #'cl:map 'vector function collection more-collections)
;;  ())
;;
;;(defmethod do ((function <function>)
;;               (collection <string>) . more-collections)
;;  (apply #'cl:map 'vector function collection more-collections)
;;  ())

(defun element (collection key)
  (elt collection key))

(defsetf element (collection key) (value)
  `(setter-element ,collection ,key ,value))

(defun setter-element (collection key value)
  (setf (elt collection key) value))


(defmethod empty? ((collection <cons>))
  ())

(defmethod empty? ((collection <null>))
  t)

(defmethod empty? ((collection <vector>))
  (if (= (length collection) 0)
      t
    ()))

(defmethod empty? ((collection <string>))
  (if (= (length collection) 0)
      t
    ()))


(defun fill (collection object . keys)
  (let ((rest-list-size (length keys)))
    (if (= rest-list-size 0)
        (cl:fill collection object)
      (if (= rest-list-size 1)
          (error  "collection with list of indizes")
        (let ((start (car keys))
              (end (cadr keys)))
          (cl:fill collection object :start start :end end))))
    )
  ())


;;(defmethod map ((function <function>)
;;               (collection <null>) . more-collections)
;;  ())

(defun map (function collection . more-collections)
  ;; (collection <cons>) . more-collections)
  (apply #'cl:map
         (type-of collection)
         function collection more-collections))


;;(defmethod map ((function <function>)
;;               (collection <vector>) . more-collections)
;;  (apply #'cl:map 'vector function collection more-collections))
;;
;;
;;(defmethod map ((function <function>)
;;               (collection <string>) . more-collections)
;;  (apply #'cl:map 'vector function collection more-collections))


(defmethod member (object (collection <list>) . rest)
  (if rest
      (cl:member object collection :test (car rest))
    (cl:member object collection)))

(defmethod member (object collection . rest)
  (error  "collection not a list"))

;;;-----------------------------------------------------------------------------
;;; sequence?
;;;-----------------------------------------------------------------------------
(defmethod sequence? ((collection <list>))
  t)

(defmethod sequence? ((collection <vector>))
  t)

(defmethod sequence? ((collection <string>))
  t)

(defmethod sequence? (collection)
  ())


(defun size (collection) (length collection))

;;;-----------------------------------------------------------------------------
#module-end
;;;-----------------------------------------------------------------------------
