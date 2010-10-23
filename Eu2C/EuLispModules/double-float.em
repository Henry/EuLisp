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
;;; Title: double float module
;;;  Authors: E. Ulrich Kriegel
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule double-float
  (import (eulisp-kernel
           double-float-i
           c-math
           number-generic
           float-generic
           elementary-functions-generic
           (only (binary=
                  binary<
                  equal)
                 compare-generic)
           integer-32
           convert)
   c-import (<float.h>)
   syntax (eulisp-kernel
           (only (and)
                 syntax-i))
   export (<double-float>
           double-float?
           most-positive-double-float
           least-positive-double-float
           least-negative-double-float
           most-negative-double-float)
   expose (number-generic
           float-generic
           (only (binary=
                  binary<
                  equal)
                 compare)
           elementary-functions-generic))

;;;-----------------------------------------------------------------------------
;;; Constants and externel definitions
;;;-----------------------------------------------------------------------------
(%declare-external-variable mpdf %double-float
  language c
  external-name |DBL_MAX|)

(%declare-external-variable lpdf %double-float
  language c
  external-name |DBL_MIN|)

(defconstant most-positive-double-float (make-dble mpdf))
(defconstant least-positive-double-float (make-dble lpdf))
(defconstant most-negative-double-float (make-dble (%neg mpdf)))
(defconstant least-negative-double-float (make-dble (%neg lpdf)))

(%define-variable double-float-buffer <double-float>)
(setq double-float-buffer (make-dble #%d0.0))

;;;-----------------------------------------------------------------------------
;;; Functions and Methods
;;;-----------------------------------------------------------------------------
(defmethod binary+ ((d1 <double-float>)
                    (d2 <double-float>))
  (make-dble (%plus (dble d1)(dble d2))))

(defmethod binary+ ((f <fpi>)
                    (d <double-float>))
  (make-dble (%plus (%citod(make-swi f))(dble d))))

(defmethod binary+ ((d <double-float>)
                    (f <fpi>))
  (make-dble (%plus (%citod(make-swi f))(dble d))))

(defmethod binary- ((d1 <double-float>)
                    (d2 <double-float>))
  (make-dble (%minus (dble d1)(dble d2))))

(defmethod binary- ((f <fpi>)
                    (d <double-float>))
  (make-dble (%minus (%citod(make-swi f))(dble d))))

(defmethod binary- ((d <double-float>)
                    (f <fpi>))
  (make-dble (%minus (dble d)(%citod(make-swi f)))))

(defmethod binary* ((d1 <double-float>)
                    (d2 <double-float>))
  (make-dble (%mult (dble d1)(dble d2))))

(defmethod binary* ((f <fpi>)
                    (d <double-float>))
  (make-dble (%mult (%citod(make-swi f))(dble d))))

(defmethod binary* ((d <double-float>)
                    (f <fpi>))
  (make-dble (%mult (dble d)(%citod(make-swi f)))))

(defmethod binary/ ((d1 <double-float>)
                    (d2 <double-float>))
  (make-dble (%div (dble d1)(dble d2))))

(defmethod binary/ ((d <double-float>)
                    (f <fpi>))
  (make-dble (%div (dble d)(%citod(make-swi f)))))

(defmethod binary/ ((f <fpi>)
                    (d <double-float>))
  (make-dble (%div (%citod(make-swi f))
                   (dble d))))

(defmethod binary% ((d1 <double-float>)
                    (d2 <double-float>))
  (make-dble (%fmod (dble d1)(dble d2))))

(defmethod binary% ((f <fpi>)
                    (d <double-float>))
  (make-dble (%fmod (%citod(make-swi f))(dble d))))

(defmethod binary% ((d <double-float>)
                    (f <fpi>))
  (make-dble (%fmod (dble d) (%citod(make-swi f)))))

(defmethod binary-mod ((d1 <double-float>)
                       (d2 <double-float>))
  (binary-mod-d (dble d1)
                (dble d2)))

(defmethod binary-mod ((f <fpi>)
                       (d <double-float>))
  (binary-mod-d (%citod(make-swi f))
                (dble d)))

(defmethod binary-mod ((d <double-float>)
                       (f <fpi>))
  (binary-mod-d (dble d)
                (%citod(make-swi f))))

(%define-function (binary-mod-d  <double-float>)
  ((d1 %double-float)
   (d2 %double-float))
  (%let* ((frac %double-float (%fmod d1 d2))
          (int %double-float (%minus d1 (%mult d2 frac))))
         ;; %neq etc can appear only inside test context
         ;; dont use and and or
         (if (%lt frac #%d0.0 )
             (if (%lt #%d0.0 d2)
                 (make-dble(%plus frac d2))
               (make-dble frac))
           (if (%lt d2 #%d0.0 )
               (if (%lt #%d0.0 frac)
                   (make-dble(%plus frac d2))
                 (make-dble frac))
             (make-dble frac)))))

;;;-----------------------------------------------------------------------------
;;; methods for gf in compare
;;;-----------------------------------------------------------------------------
(defmethod binary= ((d1 <double-float>)
                    (d2 <double-float>))
  (if  (%eq (dble d1)(dble d2))
      d1
    ()))

(defmethod binary= ((d <double-float>)
                    (f <fpi>))
  (if  (%eq (dble d)(%citod(make-swi f)))
      d
    ()))
(defmethod binary= ((f <fpi>)
                    (d <double-float>))
  (if  (%eq (dble d)(%citod(make-swi f)))
      d
    ()))

(defmethod binary< ((d1 <double-float>)
                    (d2 <double-float>))
  (if  (%lt (dble d1)(dble d2))
      d1
    ()))

(defmethod binary< ((d <double-float>)
                    (f <fpi>))
  (if  (%lt (dble d)(%citod(make-swi f)))
      d
    ()))


(defmethod binary< ((f <fpi>)
                    (d <double-float>))
  (let ((d2 (%citod(make-swi f))))
    (if  (%lt d2 (dble d) )
        (make-dble d2)
      ())))


(defmethod equal ((d1 <double-float>)
                  (d2 <double-float>))
  (binary= d1 d2))

;;;-----------------------------------------------------------------------------
;;; methods for gf in number
;;;-----------------------------------------------------------------------------
(defmethod negate ((d <double-float>))
  (make-dble(%neg (dble d))))

(defmethod zero? ((d <double-float>))
  (if (%eq (dble d) #%d0.0)
      d
    ()))

;;;-----------------------------------------------------------------------------
;;; methods for elementary-functions
;;;-----------------------------------------------------------------------------
(defmethod acos ((d <double-float>))
  (make-dble (%acos (dble d))))

(defmethod asin ((d <double-float>))
  (make-dble (%asin (dble d))))

(defmethod atan ((d <double-float>))
  (make-dble (%atan (dble d))))

(defmethod atan2 ((d1 <double-float>)
                  (d2 <double-float>))
  (make-dble (%atan2 (dble d1)
                     (dble d2))))

(defmethod cos ((d <double-float>))
  (make-dble (%cos (dble d))))

(defmethod sin ((d <double-float>))
  (make-dble (%sin (dble d))))

(defmethod tan ((d <double-float>))
  (make-dble (%tan (dble d))))

(defmethod cosh ((d <double-float>))
  (make-dble (%cosh (dble d))))

(defmethod sinh ((d <double-float>))
  (make-dble (%sinh (dble d))))

(defmethod tanh ((d <double-float>))
  (make-dble (%tanh (dble d))))

(defmethod exp ((d <double-float>))
  (make-dble (%exp (dble d))))

(defmethod log ((d <double-float>))
  (make-dble (%log (dble d))))

(defmethod log10 ((d <double-float>))
  (make-dble (%log10 (dble d))))

(defmethod pow ((d1 <double-float>)
                (d2 <double-float>))
  (make-dble (%pow (dble d1)
                   (dble d2))))

(defmethod sqrt ((d <double-float>))
  (make-dble (%sqrt (dble d))))

;;;-----------------------------------------------------------------------------
;;; methods for float
;;;-----------------------------------------------------------------------------
(defmethod ceiling ((d <double-float>))
  (make-dble (%ceil (dble d))))

(defmethod floor ((d <double-float>))
  (make-dble (%floor (dble d))))

(defmethod truncate ((d <double-float>))
  (%modf (dble d) (%cast <double*> double-float-buffer))
  (make-fpi (%cdtoi (dble double-float-buffer))))

(defmethod round ((d <double-float>))
  (%let* ((dd %double-float (dble d))
          (frac %double-float (%modf dd (%cast <double*> double-float-buffer)))
          (inew %signed-word-integer (%cdtoi (dble double-float-buffer))))
         ;;%eq can only appear inside test context
         (if (%eq frac #%d0.5 )
             (if (%eq (%and inew #%i1) #%i1)
                 (setq inew (%plus inew #%i1))
               ())
           (if (%gt (%fabs frac) #%d0.5)
               (if (%gt frac #%d0.0)
                   (setq inew (%plus inew #%i1))
                 (setq inew (%minus inew #%i1)))
             ()))
         (make-fpi inew)))

;;;-----------------------------------------------------------------------------
;;; converter
;;;-----------------------------------------------------------------------------
(defmethod (converter <fpi>) ((object <double-float>))
  (make-fpi(%cdtoi (dble object))))

;;;-----------------------------------------------------------------------------
;;; Type schemes for type inference
;;;-----------------------------------------------------------------------------
(%annotate-function
  binary+ renew-signature
  (((var0 var1 var2)
    ((var var0) (atom? <double-float>))
    ((var var1) (atom? (or <fpi> <double-float>)))
    ((var var2) (atom? <double-float>)))
   ((var0 var1 var2)
    ((var var0) (atom? <double-float>))
    ((var var1) (atom? <double-float>))
    ((var var2) (atom? (or <fpi> <double-float>))))
   ((var0 var1 var2)
    ((var var0) (atom? <fpi>))
    ((var var1) (atom? <fpi>))
    ((var var2) (atom? <fpi>)))
   ((var0 var1 var2)
    ((var var0) (atom? (and <number> (and (not <fpi>)
                                         (not <double-float>)))))
    ((var var1) (atom? (and <number> (and (not <fpi>)
                                         (not <double-float>)))))
    ((var var2) (atom? <number>)))
   ((var0 var1 var2)
    ((var var0) (atom? (and <number> (and (not <fpi>)
                                         (not <double-float>)))))
    ((var var1) (atom? <number>))
    ((var var2) (atom? (and <number> (and (not <fpi>)
                                         (not <double-float>))))))))

(%annotate-function
  binary- renew-signature
  (((var0 var1 var2)
    ((var var0) (atom? <double-float>))
    ((var var1) (atom? (or <fpi> <double-float>)))
    ((var var2) (atom? <double-float>)))
   ((var0 var1 var2)
    ((var var0) (atom? <double-float>))
    ((var var1) (atom? <double-float>))
    ((var var2) (atom? (or <fpi> <double-float>))))
   ((var0 var1 var2)
    ((var var0) (atom? <fpi>))
    ((var var1) (atom? <fpi>))
    ((var var2) (atom? <fpi>)))
   ((var0 var1 var2)
    ((var var0) (atom? (and <number> (and (not <fpi>)
                                         (not <double-float>)))))
    ((var var1) (atom? (and <number> (and (not <fpi>)
                                         (not <double-float>)))))
    ((var var2) (atom? <number>)))
   ((var0 var1 var2)
    ((var var0) (atom? (and <number> (and (not <fpi>)
                                         (not <double-float>)))))
    ((var var1) (atom? <number>))
    ((var var2) (atom? (and <number> (and (not <fpi>)
                                         (not <double-float>))))))))

(%annotate-function
  binary/ renew-signature
  (((var0 var1 var2)
    ((var var0) (atom? <double-float>))
    ((var var1) (atom? (or <fpi> <double-float>)))
    ((var var2) (atom? <double-float>)))
   ((var0 var1 var2)
    ((var var0) (atom? <double-float>))
    ((var var1) (atom? <double-float>))
    ((var var2) (atom? (or <fpi> <double-float>))))
   ((var0 var1 var2)
    ((var var0) (atom? <fpi>))
    ((var var1) (atom? <fpi>))
    ((var var2) (atom? <fpi>)))
   ((var0 var1 var2)
    ((var var0) (atom? (and <number> (and (not <fpi>)
                                         (not <double-float>)))))
    ((var var1) (atom? (and <number> (and (not <fpi>)
                                         (not <double-float>)))))
    ((var var2) (atom? <number>)))
   ((var0 var1 var2)
    ((var var0) (atom? (and <number> (and (not <fpi>)
                                         (not <double-float>)))))
    ((var var1) (atom? <number>))
    ((var var2) (atom? (and <number> (and (not <fpi>)
                                         (not <double-float>))))))))

(%annotate-function
  binary* renew-signature
  (((var0 var1 var2)
    ((var var0) (atom? <double-float>))
    ((var var1) (atom? (or <fpi> <double-float>)))
    ((var var2) (atom? <double-float>)))
   ((var0 var1 var2)
    ((var var0) (atom? <double-float>))
    ((var var1) (atom? <double-float>))
    ((var var2) (atom? (or <fpi> <double-float>))))
   ((var0 var1 var2)
    ((var var0) (atom? <fpi>))
    ((var var1) (atom? <fpi>))
    ((var var2) (atom? <fpi>)))
   ((var0 var1 var2)
    ((var var0) (atom? (and <number> (and (not <fpi>)
                                         (not <double-float>)))))
    ((var var1) (atom? (and <number> (and (not <fpi>)
                                         (not <double-float>)))))
    ((var var2) (atom? <number>)))
   ((var0 var1 var2)
    ((var var0) (atom? (and <number> (and (not <fpi>)
                                         (not <double-float>)))))
    ((var var1) (atom? <number>))
    ((var var2) (atom? (and <number> (and (not <fpi>)
                                         (not <double-float>))))))))

(%annotate-function
  binary% renew-signature
  (((var0 var1 var2)
    ((var var0) (atom? <double-float>))
    ((var var1) (atom? (or <fpi> <double-float>)))
    ((var var2) (atom? <double-float>)))
   ((var0 var1 var2)
    ((var var0) (atom? <double-float>))
    ((var var1) (atom? <double-float>))
    ((var var2) (atom? (or <fpi> <double-float>))))
   ((var0 var1 var2)
    ((var var0) (atom? <fpi>))
    ((var var1) (atom? <fpi>))
    ((var var2) (atom? <fpi>)))
   ((var0 var1 var2)
    ((var var0) (atom? (and <number> (and (not <fpi>)
                                         (not <double-float>)))))
    ((var var1) (atom? (and <number> (and (not <fpi>)
                                         (not <double-float>)))))
    ((var var2) (atom? <number>)))
   ((var0 var1 var2)
    ((var var0) (atom? (and <number> (and (not <fpi>)
                                         (not <double-float>)))))
    ((var var1) (atom? <number>))
    ((var var2) (atom? (and <number> (and (not <fpi>)
                                         (not <double-float>))))))))

(%annotate-function
  binary-mod renew-signature
  (((var0 var1 var2)
    ((var var0) (atom? <double-float>))
    ((var var1) (atom? (or <fpi> <double-float>)))
    ((var var2) (atom? <double-float>)))
   ((var0 var1 var2)
    ((var var0) (atom? <double-float>))
    ((var var1) (atom? <double-float>))
    ((var var2) (atom? (or <fpi> <double-float>))))
   ((var0 var1 var2)
    ((var var0) (atom? <fpi>))
    ((var var1) (atom? <fpi>))
    ((var var2) (atom? <fpi>)))
   ((var0 var1 var2)
    ((var var0) (atom? (and <number> (and (not <fpi>)
                                         (not <double-float>)))))
    ((var var1) (atom? (and <number> (and (not <fpi>)
                                         (not <double-float>)))))
    ((var var2) (atom? <number>)))
   ((var0 var1 var2)
    ((var var0) (atom? (and <number> (and (not <fpi>)
                                         (not <double-float>)))))
    ((var var1) (atom? <number>))
    ((var var2) (atom? (and <number> (and (not <fpi>)
                                         (not <double-float>))))))))

;;;-----------------------------------------------------------------------------
)  ;; End of module double-float
;;;-----------------------------------------------------------------------------
