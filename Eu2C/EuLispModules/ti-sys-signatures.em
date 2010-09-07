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
;;;  Title: TAIL Type Schemes
;;;  Description:
;;    This application modules describes type schemes of all predefined TAIL
;;    functions and adds some strategic lattice types to the lattice used
;;    for type inference.
;;;  Authors: Andreas Kind
;;;-----------------------------------------------------------------------------

(defmodule ti-sys-signatures
  (import (%tail)
   syntax (%tail))

;;;-----------------------------------------------------------------------------
;;; Adding singleton type to lattice
;;;-----------------------------------------------------------------------------
(%define-lattice-type singleton (%object) (bottom))

;;;-----------------------------------------------------------------------------
;;; Adding signatures to predefined tail functions
;;;-----------------------------------------------------------------------------
(%annotate-function    ;;supposed to change?
  %abs new-signature
  (((var0 var1) ((var var0) (atom? %number))
    ((var var1) (var var1)))))

(%annotate-function
  %and new-signature
  (((var0 var1 var2) ((var var0) (atom? %integer))
    ((var var1) (var var0))
    ((var var2) (var var0)))))

(%annotate-function
  %ashiftl new-signature
  (((var0 var1 var2) ((var var0) (atom? %integer))
    ((var var1) (var var0))
    ((var var2) (atom? %unsigned-integer)))))

(%annotate-function
  %ashiftr new-signature
  (((var0 var1 var2) ((var var0) (atom? %integer))
    ((var var1) (var var0))
    ((var var2) (atom? %unsigned-integer)))))

(%annotate-function
  %cast new-signature
  (((var0 var1 var2) ((var var0) (atom? top))
    ((var var1) (var var0))
    ((var var2) (atom? top)))))

(%annotate-function
  %citod new-signature
  (((var0 var1) ((var var0) (atom? %double-float))
    ((var var1) (atom? %integer)))))

(%annotate-function
  %citos new-signature
  (((var0 var1) ((var var0) (atom? %single-float))
    ((var var1) (atom? %integer)))))

(%annotate-function
  %citox new-signature
  (((var0 var1) ((var var0) (atom? %extended-float))
    ((var var1) (atom? %integer)))))

(%annotate-function
  %cdtoi new-signature
  (((var0 var1) ((var var0) (atom? %signed-word-integer))
    ((var var1) (atom? %double-float)))))

(%annotate-function
  %cdtos new-signature
  (((var0 var1) ((var var0) (atom? %single-float))
    ((var var1) (atom? %double-float)))))

(%annotate-function
  %cdtox new-signature
  (((var0 var1) ((var var0) (atom? %extended-float))
    ((var var1) (atom? %double-float)))))

(%annotate-function
  %cstod new-signature
  (((var0 var1) ((var var0) (atom? %double-float))
    ((var var1) (atom? %single-float)))))

(%annotate-function
  %cstoi new-signature
  (((var0 var1) ((var var0) (atom? %signed-word-integer))
    ((var var1) (atom? %single-float)))))

(%annotate-function
  %cstox new-signature
  (((var0 var1) ((var var0) (atom? %extended-float))
    ((var var1) (atom? %single-float)))))

(%annotate-function
  %cxtod new-signature
  (((var0 var1) ((var var0) (atom? %double-float))
    ((var var1) (atom? %extended-float)))))

(%annotate-function
  %cxtoi new-signature
  (((var0 var1) ((var var0) (atom? %signed-word-integer))
    ((var var1) (atom? %extended-float)))))

(%annotate-function
  %cxtos new-signature
  (((var0 var1) ((var var0) (atom? %single-float))
    ((var var1) (atom? %extended-float)))))

(%annotate-function
  %div new-signature
  (((var0 var1 var2) ((var var0) (atom? %number))
    ((var var1) (var var0))
    ((var var2) (var var0)))))

;; Has to be extended for all subtypes of singleton!
(%annotate-function
  %eq new-signature
  (((var0 var1 var2) ((var var0) (atom? (not %false)))
    ((var var1) (atom? singleton))
    ((var var2) (var var1)))
   ((var0 var1 var2) ((var var0) (atom? top))
    ((var var1) (atom? (not singleton)))
    ((var var2) (atom? (not singleton))))))

;;  (%annotate-function
;;   %extract new-signature
;;   (((var0 var1 var2) ((var var0) (atom? top))
;;                    ((var var1) (atom? %vector))
;;                    ((var var2) (atom? %integer)))))

(%annotate-function
  %ge new-signature
  (((var0 var1 var2) ((var var0) (atom? top))
    ((var var1) (atom? %number))
    ((var var2) (var var1)))))

(%annotate-function
  %gt new-signature
  (((var0 var1 var2) ((var var0) (atom? top))
    ((var var1) (atom? %number))
    ((var var2) (var var1)))))

(%annotate-function
  %le new-signature
  (((var0 var1 var2) ((var var0) (atom? top))
    ((var var1) (atom? %number))
    ((var var2) (var var1)))))

(%annotate-function
  %lt new-signature
  (((var0 var1 var2) ((var var0) (atom? top))
    ((var var1) (atom? %number))
    ((var var2) (var var1)))))

(%annotate-function
  %lshiftl new-signature
  (((var0 var1 var2) ((var var0) (atom? %integer))
    ((var var1) (var var0))
    ((var var2) (atom? %unsigned-integer)))))

(%annotate-function
  %lshiftr new-signature
  (((var0 var1 var2) ((var var0) (atom? %integer))
    ((var var1) (var var0))
    ((var var2) (atom? %unsigned-integer)))))

(%annotate-function
  %minus new-signature
  (((var0 var1 var2) ((var var0) (atom? %number))
    ((var var1) (var var0))
    ((var var2) (var var0)))
   ;;    ((var0 var1 var2) ((var var0) (atom? %pointer))
   ;;                    ((var var1) (var var0))
   ;;                    ((var var2) (atom? %unsigned-integer)))
   ))

(%annotate-function
  %rem new-signature
  (((var0 var1 var2) ((var var0) (atom? %integer))
    ((var var1) (var var0))
    ((var var2) (var var0)))))

(%annotate-function
  %mult new-signature
  (((var0 var1 var2) ((var var0) (atom? %number))
    ((var var1) (var var0))
    ((var var2) (var var0)))))

(%annotate-function
  %neg new-signature
  (((var0 var1) ((var var0) (atom? %number))
    ((var var1) (var var0)))))

;; Has to be extended for all subtypes of singleton!
(%annotate-function
  %neq new-signature
  (((var0 var1 var2) ((var var0) (atom? %false))
    ((var var1) (atom? singleton))
    ((var var2) (var var1)))
   ((var0 var1 var2) ((var var0) (atom? top))
    ((var var1) (atom? (not singleton)))
    ((var var2) (atom? (not singleton))))))

(%annotate-function
  %not new-signature
  (((var0 var1) ((var var0) (atom? %integer))
    ((var var1) (var var0)))))

(%annotate-function
  %or new-signature
  (((var0 var1 var2) ((var var0) (atom? %integer))
    ((var var1) (var var0))
    ((var var2) (var var0)))))

(%annotate-function
  %plus new-signature
  (((var0 var1 var2) ((var var0) (atom? %number))
    ((var var1) (var var0))
    ((var var2) (var var0)))
   ;;    ((var0 var1 var2) ((var var0) (atom? %pointer))
   ;;                    ((var var1) (var var0))
   ;;                    ((var var2) (atom? %unsigned-integer)))
   ))

;;  (%annotate-function
;;   %ref new-signature  ;;treated special
;;   (((var0 var1) ((var var0) (atom? top))
;;               ((var var1) (atom? (or %unsigned-word-integer
;;                                     %signed-word-integer))))
;;    ((var0 var1) ((var var0) (atom? top))
;;               ((var var1) (atom? %object)))))

;;  (%annotate-function
;;   %rotatel new-signature
;;   (((var0 var1 var2) ((var var0) (atom? %integer))
;;                    ((var var1) (var var0))
;;                  ((var var2) (atom? %unsigned-integer)))))


;;  (%annotate-function
;;   %rotater new-signature
;;   (((var0 var1 var2) ((var var0) (atom? %integer))
;;                    ((var var1) (var var0))
;;                    ((var var2) (atom? %unsigned-integer)))))

;;  (%annotate-function  ;;treated special
;;   %select new-signature
;;   (((var0 var1 var2) ((var var0) (atom? top))
;;                    ((var var1) (atom? top))
;;                    ((var var2) (atom? top)))))

;;  (%annotate-function
;;   %setf-extract new-signature
;;   (((var0 var1 var2 var3) ((var var0) (atom? top))
;;                         ((var var1) (atom? top))
;;                         ((var var2) (atom? top))
;;                         ((var var3) (var var0)))))

;;  (%annotate-function
;;   %setf-ref new-signature
;;   (((var0 var1 var2 var3) ((var var0) (atom? top))
;;                         ((var var1) (atom? top))
;;                         ((var var2) (atom? top))
;;                         ((var var3) (var var0)))))

;;  (%annotate-function
;;   %setf-select new-signature
;;   (((var0 var1 var2 var3) ((var var0) (atom? top))
;;                         ((var var1) (atom? top))
;;                         ((var var2) (atom? top))
;;                         ((var var3) (var var0)))))

(%annotate-function
  %size-of-instance new-signature
  (((var0 var1) ((var var0) (atom? %signed-word-integer))
    ((var var1) (atom? top)))))

(%annotate-function
  %size-as-component new-signature
  (((var0 var1) ((var var0) (atom? %signed-word-integer))
    ((var var1) (atom? top)))))


(%annotate-function
  %xor new-signature
  (((var0 var1 var2) ((var var0) (atom? %integer))
    ((var var1) (var var0))
    ((var var2) (var var0)))))

(%annotate-function
  %setjmp new-signature
  (((var0 var1) ((var var0) (atom? %signed-word-integer))
    ((var var1) (atom? %jmpbuf)))))

(%annotate-function
  %longjmp new-signature
  (((var0 var1 var2) ((var var0) (atom? %void))
    ((var var1) (atom? %jmpbuf))
    ((var var2) (atom? %signed-word-integer)))))

;;  (%annotate-function
;;   %pointer-of-variable new-signature
;;   ; the only case, where pointer-of is needed is with jmp-bufs
;;   (((var0 var1) ((var var0) (atom? %pjmpbuf))
;;                 ((var var1) (atom? %jmpbuf)))))

;;;-----------------------------------------------------------------------------
)
;;;-----------------------------------------------------------------------------
