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
;;; Title: string processing
;;;  Library: level1
;;;  Authors: Andreas Kind, Julian Padget
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule string
  (syntax (_telos0)
   import (telos
           convert
           copy
           collect
           compare
           fpi)
   export (<character-sequence>
           <string>
           string?
           string-data
           string-size
           string-ref
           substring
           tailstring
           string-compare
           string-equal
           string-append
           string-as-fpi
           string-empty?
           member1-string
           do1-string
           map1-string
           listify-string
           eul_list_as_eul_string))

;;;-----------------------------------------------------------------------------
;;; Classes: <string>
;;;-----------------------------------------------------------------------------
(defclass <character-sequence> <sequence> () abstract?: t)

(defprimclass <string> string-class (<character-sequence>)
              ((data accessor: string-data)))

(defmethod initialize ((str <string>) inits)
  (call-next-method)
  (let ((n (init-list-ref inits size: 0))
        (c (init-list-ref inits fill-value: #\ )))
    (eul_init_string str n c)))

(defextern eul_init_string (ptr <fpi> <character>) ptr)

;;;-----------------------------------------------------------------------------
;;; Predicates
;;;-----------------------------------------------------------------------------
(defun string-empty? (str) (fpi-binary= (string-size str) 0))
(declare-inline string-empty?)

(defmethod emptyp ((str <string>)) (string-empty? str))

(defun string-equal (str1 str2)
  (fpi-binary= (string-compare str1 str2) 0))
(declare-inline string-equal)

(defextern string-compare (<string> <string>) <fpi> "strcmp")

(defmethod binary= ((str1 <string>) (str2 <string>))
  (if (string-equal str1 str2) t ()))

(defmethod binary< ((str1 <string>) (str2 <string>))
  (fpi-binary< (string-compare str1 str2) 0))

;;;-----------------------------------------------------------------------------
;;; Iteration
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;;  Accumulate
;;;-----------------------------------------------------------------------------
(defmethod accumulate ((fun <function>) init (str <string>))
  (accumulate-string fun init str))

(defun accumulate-string (fun init str)
  (let ((n (string-size str))
        (i 0))
    (labels
     ((loop ()
            (if (fpi-binary< i n)
                (progn
                  (setq init (fun init (string-ref str i)))
                  (setq i (fpi-binary+ i 1))
                  (loop))
              init)))
     (loop))))

(defmethod accumulate1 ((fun <function>) (str <string>))
  (accumulate1-string fun str))

(defun accumulate1-string (fun str)
  (if (fpi-binary= (string-size str) 0) ()
    (accumulate-string fun (string-ref str 0) (tailstring str 1))))

;;;-----------------------------------------------------------------------------
;;;  Any?
;;;-----------------------------------------------------------------------------
(defmethod any? ((fun <function>) (str <string>) . cs)
  (if (null? cs)
      (anyp1-string fun str)
    (call-next-method)))

(defun anyp1-string (fun str)
  (let ((n (string-size str))
        (i 0))
    (labels
     ((loop ()
            (and (fpi-binary< i n)
                 (or (fun (string-ref str i))
                     (progn
                       (setq i (fpi-binary+ i 1))
                       (loop))))))
     (loop))))

;;;-----------------------------------------------------------------------------
;;;  All?
;;;-----------------------------------------------------------------------------
(defmethod all? ((fun <function>) (str <string>) . cs)
  (if (null? cs)
      (all?1-string fun str)
    (call-next-method)))

(defun all?1-string (fun str)
  (let ((n (string-size str))
        (i 0))
    (labels
     ((loop ()
            (if (fpi-binary< i n)
                (and (fun (string-ref str i))
                     (progn
                       (setq i (fpi-binary+ i 1))
                       (loop)))
              str)))
     (loop))))

;;;-----------------------------------------------------------------------------
;;;  Do
;;;-----------------------------------------------------------------------------
(defmethod do ((fun <function>) (str <string>) . cs)
  (if (null? cs)
      (do1-string fun str)
    (call-next-method)))

(defun do1-string (fun str)
  (let ((n (string-size str))
        (i 0))
    (labels
     ((loop ()
            (if (fpi-binary< i n)
                (progn
                  (fun (string-ref str i))
                  (setq i (fpi-binary+ i 1))
                  (loop))
              ())))
     (loop))))

;;;-----------------------------------------------------------------------------
;;;  Map
;;;-----------------------------------------------------------------------------
(defmethod map ((fun <function>) (str <string>) . cs)
  (if (null? cs)
      (map1-string fun str)
    (call-next-method)))

(defun map1-string (fun str)
  (let* ((n (string-size str))
         (res (make <string> size: n))
         (i 0))
    (labels
     ((loop ()
            (if (fpi-binary< i n)
                (progn
                  ((setter string-ref) res i (fun (string-ref str i)))
                  (setq i (fpi-binary+ i 1))
                  (loop))
              res)))
     (loop))))

;;;-----------------------------------------------------------------------------
;;;  Member
;;;-----------------------------------------------------------------------------
(defmethod member (x (str <string>) . preds)
  (if (null? preds)
      (member1-string x str)
    (let ((pred (car preds))
          (n (string-size str))
          (i 0))
      (labels
       ((loop ()
              (and (fpi-binary< i n)
                   (if (eql x (string-ref str i))
                       i
                     (progn
                       (setq i (fpi-binary+ i 1))
                       (loop))))))
       (loop)))))

;;;-----------------------------------------------------------------------------
;;; Access
;;;-----------------------------------------------------------------------------
(defmethod element ((str <string>) (i <fpi>))
  (string-ref str i))

;;;-----------------------------------------------------------------------------
;;; Size
;;;-----------------------------------------------------------------------------
(defmethod size ((str <string>))
  (string-size str))

;;;-----------------------------------------------------------------------------
;;; Reverse
;;;-----------------------------------------------------------------------------
(defmethod reverse ((str <string>))
  (reverse-string str))
(defextern reverse-string (<string>) <string> "eul_reverse_str")

;;;-----------------------------------------------------------------------------
;;; Concatenation
;;;-----------------------------------------------------------------------------
(defmethod concatenate ((str <string>) . cs)
  (labels
   ((loop (ccs)
          (if (null? ccs) str
            (progn
              (setq str
                    (eul_str_append str (convert (car ccs) <string>)))
              (loop (cdr ccs))))))
   (loop cs)))
(defextern eul_str_append (<string> <string>) <string>)

(defun string-append strs
  (eul_list_as_eul_string strs))
(defextern eul_list_as_eul_string (ptr) ptr)

;;;-----------------------------------------------------------------------------
;;; Slice
;;;-----------------------------------------------------------------------------
(defun substring (str i j)
  (let ((ii (or i 0))
        (jj (or j (string-size str))))
    (substring1 str ii jj)))

(defmethod slice ((str <string>) (s <fpi>) (e <fpi>))
  (substring1 str s e))

;;;-----------------------------------------------------------------------------
;;; Copy
;;;-----------------------------------------------------------------------------
(defmethod shallow-copy ((str <string>))
  (copy-string str))

(defmethod deep-copy ((str <string>))
  (copy-string str))

(defextern copy-string (<string>) <string> "eul_str_copy")

;;;-----------------------------------------------------------------------------
;;; Conversion
;;;-----------------------------------------------------------------------------
(defgeneric (converter <string>) (x))

(defextern string-as-fpi (<string>) <fpi> "atoi")

(defun listify-string (str . separators)
  (let ((c (if separators (car separators) #\ )))
    (labels
     ((loop (s res)
            (let* ((i (member c s))
                   (x (make <symbol> name: (substring s () i))))
              (if (and i (fpi-binary< i (fpi-binary- (string-size s) 1)))
                  (loop (substring s (fpi-binary+ i 1) ()) (cons x res))
                (reverse-list (cons x res))))))
     (if (eql c (string-ref str 0))
         (loop (substring str 1 ()) ())
       (loop str ())))))

;;;-----------------------------------------------------------------------------
)  ;; End of module string
;;;-----------------------------------------------------------------------------
