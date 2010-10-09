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
;;; Title: conversion
;;;  Library: level1
;;;  Authors: Andreas Kind, Julian Padget
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule convert1
  (syntax (_telos0)
   import (telos
           condition
           convert
           collect
           fpi
           character
           list
           string
           vector
           table))

;;;-----------------------------------------------------------------------------
;;; Conversion list, string, vector, table -> list
;;;-----------------------------------------------------------------------------
(defmethod (converter <list>) ((l <list>)) l)

(defmethod (converter <list>) ((str <string>))
  (let ((n (int-binary- (string-size str) 1)))
    (labels
     ((loop (res)
            (if (int-binary< n 0) res
              (let ((char (string-ref str n)))
                (setq n (int-binary- n 1))
                (loop (cons char res))))))
     (loop ()))))

(defmethod (converter <list>) ((vec <vector>))
  (let ((n (int-binary- (vector-size vec) 1)))
    (labels
     ((loop (res)
            (if (int-binary< n 0) res
              (let ((char (vector-ref vec n)))
                (setq n (int-binary- n 1))
                (loop (cons char res))))))
     (loop ()))))

(defmethod (converter <list>) ((tab <table>))
  (table-values tab))

;;;-----------------------------------------------------------------------------
;;; Conversion list, string, vector, table -> string
;;;-----------------------------------------------------------------------------
(defmethod (converter <string>) ((l <list>))
  (eul_list_as_eul_string l))
;;  (defmethod (converter <string>) ((l <list>))
;;    (let* ((n (list-size l))
;;         (res (make <string> size: n))
;;         (i 0))
;;      (labels
;;       ((loop (ll)
;;            (if (int-binary< i n)
;;                (let ((x (car ll)))
;;                  (if (character? x) ()
;;                    (error <condition>
;;                           "conversion to string with non character element"))
;;                  ((setter string-ref) res i x)
;;                  (setq i (int-binary+ i 1))
;;                  (loop (cdr ll)))
;;              res)))
;;       (loop l))))

(defmethod (converter <string>) ((str <string>)) str)

(defmethod (converter <string>) ((vec <vector>))
  (let* ((n (vector-size vec))
         (res (make <string> size: n)))
    (setq n (int-binary- n 1))
    (labels
     ((loop ()
            (if (int-binary< n 0) res
              (let ((x (vector-ref vec n)))
                (if (character? x) ()
                  (error <condition>
                         "conversion to string with non character element "))
                ((setter string-ref) res n x)
                (setq n (int-binary- n 1))
                (loop)))))
     (loop))))

(defmethod (converter <string>) ((tab <table>))
  (convert (table-values tab) <string>))

;;;-----------------------------------------------------------------------------
;;; Conversion list, string, vector, table -> vector
;;;-----------------------------------------------------------------------------
(defmethod (converter <vector>) ((l <list>))
  (make-vector1 (list-size l) l))

(defmethod (converter <vector>) ((str <string>))
  (let* ((n (string-size str))
         (res (make-vector n)))
    (setq n (int-binary- n 1))
    (labels
     ((loop ()
            (if (int-binary< n 0) res
              (let ((x (string-ref str n)))
                ((setter vector-ref) res n x)
                (setq n (int-binary- n 1))
                (loop)))))
     (loop))))

(defmethod (converter <vector>) ((vec <vector>)) vec)

(defmethod (converter <vector>) ((tab <table>))
  (convert (table-values tab) <vector>))

;;;-----------------------------------------------------------------------------
;;; Conversion list, string, vector, table -> table
;;;-----------------------------------------------------------------------------
(defmethod (converter <table>) ((l <list>))
  (let ((res (make <table>))
        (i 0))
    (labels
     ((loop (ll)
            (if (null? ll) res
              (let ((x (car ll)))
                ((setter table-ref) res i x)
                (setq i (int-binary+ i 1))
                (loop (cdr ll))))))
     (loop l))))

;  (defmethod (converter <table>) ((str <string>))
;    ;; doesn't make sense!
;    )

(defmethod (converter <table>) ((vec <vector>))
  (let ((n (vector-size vec))
        (res (make <table>)))
    (setq n (int-binary- n 1))
    (labels
     ((loop ()
            (if (int-binary< n 0) res
              (let ((x (vector-ref vec n)))
                ((setter table-ref) res n x)
                (setq n (int-binary- n 1))
                (loop)))))
     (loop))))

(defmethod (converter <table>) ((tab <table>)) tab)

;;;-----------------------------------------------------------------------------
;;; Conversion int, character -> string
;;;-----------------------------------------------------------------------------
(defmethod (converter <string>) ((x <int>)) (int-as-string x))
(defmethod (converter <string>) ((x <character>)) (character-as-string x))
(defmethod (converter <string>) ((x <name>)) (name x))

;;;-----------------------------------------------------------------------------
;;; Conversion character, string, int -> int
;;;-----------------------------------------------------------------------------
(defmethod (converter <int>) ((str <string>)) (string-as-int str))
(defmethod (converter <int>) ((c <character>)) (character-as-int c))
(defmethod (converter <int>) ((x <int>)) x)

;;;-----------------------------------------------------------------------------
;;; Conversion int -> character
;;;-----------------------------------------------------------------------------
(defmethod (converter <character>) ((x <int>)) (int-as-character x))

;;;-----------------------------------------------------------------------------
)  ;; End of module convert1
;;;-----------------------------------------------------------------------------
