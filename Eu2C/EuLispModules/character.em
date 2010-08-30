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
;;;  Title: character
;;;  Problems:
;;    character box vector cannot be allocated statically til now
;;    literal expander for vectors needed
;;;  Authors: E. Ulrich Kriegel
;;;-----------------------------------------------------------------------------

(defmodule character

  (import
   (tail
    (only (binary< equal) compare-generic)
    (only (generic-prin generic-write) stream-generic)
    (only (upperp upper2lower lowerp lower2upper) char-tables)
    (only (make-swi make-fpint) int-i)
    ;;copy
    basic-compare      ;; instead of compare which should be used if implemented right
    ;;convert
    )

   syntax (tail)

   export
   (<character> convert-int-char convert-char-int char-code
                characterp equal-character
                binary<
                equal
                as-lowercase as-uppercase
                generic-prin generic-write)
   )

;;(expose (only (copy) copy))
;;(expose (only (equal) compare))
;;(expose (only (converter) convert))
;;(expose printer-generic)


(%define-standard-class (<character> <class>)
  <object>
  ((char-code type %signed-word-integer ;rr
              keyword char-code
              accessor char-code))      ;rr
  representation pointer-to-struct  ;direct ;rr
  constructor (make-character char-code)   ;rr
  predicate characterp)


(%define-standard-class (<character-box-vector> <class>)
  <object>
  ((element type <class> default <character>)
   (length type %unsigned-word-integer default #%I257))
  ;; keywords (length element); *UK* 29.06.93
  constructor (make-character-box-vector)
  representation pointer-to-vector
  allocation multiple-type-card)

;;; at time static generation not possible, take dynamic generation
;;(defconstant $character-box-vector (%literal <character-box-vector> 257
;; rr class is not possible hier                         (<character>)))
;; rr mehrfache anlegen des vectors in asm               ()))

;;(defun fill-vector (vector index last-index obj)
;;  (if (%eq (%cast %unsigned-word-integer last-index)
;;           (%cast %unsigned-word-integer index))
;;    ()
;;    (progn (%setf-extract (%cast <character-box-vector> vector)
;;                          (%cast %unsigned-word-integer index)
;;                          obj)
;;           (fill-vector vector
;;                        (%plus
;;                         (%cast %unsigned-word-integer index)
;;                         #%I1) last-index obj))))
;;
;;(fill-vector $character-box-vector #%I0 #%I257 <character>)

;;; rr dynamic generation is not possible, crash with %class-of, vector will be
;;; inside of heap
;;; (%define-variable $character-box-vector <character-box-vector>)
;;;value of %define-variable must be a literal!
;;;so take %setf to overcome this
;;; rr (%setf $character-box-vector
;;;       (make-character-box-vector))

(%define-function (convert-int-char <character>)
  ((int <int>))
  ;;  (%cast <character> (%plus  (%cast %signed-word-integer
  ;;                                    $character-box-vector )
  ;;                             (%ashiftl (%plus (make-swi int) #%i1) #%B2))))
  (make-character       ;(%cast %unsigned-word-integer ;RR
   (make-swi int)))     ;)  ;;RR

(%define-function (convert-char-int <int>)
  ((char <character>))
  (make-fpint
   ;;         (%minus (%ashiftr (%minus (%cast %signed-word-integer char)
   ;;                                   (%cast %signed-word-integer
   ;;                                          $character-box-vector))
   ;;                           #%B2)
   ;;                 #%i1)))
   ;;(%cast %signed-word-integer ;RR
   (char-code char)))    ;;)               ;;RR

(%annotate-function convert-char-int interpreter char-code)

;; rr nicht ok!
;;(%define-literal-expansion character
;;        `(%literal ,<character> ,(convert-char-int value)))
(%define-literal-expansion character
  `(%literal ,<character> char-code
             (%literal ,%signed-word-integer ,(convert-char-int value))));RR
;;`(%literal ,<character> char-code ,(convert-char-int value))) warnung
;;verlangt %unsigned-word-integer gegeben <fixed-precision-int16>

(defun equal-character (c1 c2)
  (if (%eq   ;;(%cast %unsigned-word-integer
       (char-code c1)    ;;)
       ;;(%cast %unsigned-word-integer
       (char-code c2))   ;;)
      c1
    ()))

(defmethod equal ((c1 <character>) (c2 <character>))
  (if (%eq   ;;(%cast %unsigned-word-integer
       (char-code c1)    ;;)
       ;;(%cast %unsigned-word-integer
       (char-code c2))   ;;)
      c1
    ()))

;;    (defmethod equal ((c1 <character>) (c2 <character>))
;;      (if (%eq (%cast %unsigned-word-integer c1)
;;               (%cast %unsigned-word-integer c2))
;;          c1
;;        ()))

;; siehe print.am
;; (defmethod generic-prin
;;   ((c <character>) stream))

;; (defmethod generic-write
;;   ((c <character>) stream))

(defmethod binary< ((c1 <character>)
                    (c2 <character>))
  (if (%lt (char-code c1) (char-code c2))
      c1 ()))

(defgeneric as-lowercase ((object <object>)))
(defgeneric as-uppercase ((object <object>)))

(defmethod as-lowercase ((object <object>))
  (%let ((ch %signed-word-integer (char-code object)))  ;RR
        (if (upperp ch)
            (make-character (upper2lower ch))
          object)))

(defmethod as-uppercase ((object <object>))
  (%let ((ch %signed-word-integer (char-code object)))  ;RR
        (if (lowerp ch)
            (make-character (lower2upper ch))
          object)))

;;;-----------------------------------------------------------------------------
;;; Type schemes for type inference
;;;-----------------------------------------------------------------------------

;; The predicate function is generated automatically.

;;(%annotate-function
;; characterp new-signature
;; (((var0 var1)
;;   ((var var0) (atom (not <null>)))
;;   ((var var1) (atom <character>)))
;;  ((var0 var1)
;;   ((var var0) (atom <null>))
;;   ((var var1) (atom (not <character>))))))

(%annotate-function
  equal-character new-signature
  (((var0 var1 var2)
    ((var var0) (atom <null>))
    ((var var1) (atom <character>))
    ((var var2) (atom <character>)))
   ((var0 var1 var2)
    ((var var0) (atom <character>))
    ((var var1) (var var0))
    ((var var2) (var var0)))))

(%annotate-function
  as-lowercase new-signature
  (((var0 var1)
    ((var var0) (atom <object>))
    ((var var1) (var var0)))))

(%annotate-function
  as-uppercase new-signature
  (((var0 var1)
    ((var var0) (atom <object>))
    ((var var1) (var var0)))))

)
