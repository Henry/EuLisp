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
;;; Title: basic functionality for strings
;;    everything symbol needs to be happy
;;;  Description:
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Ingo Mohr, E. Ulrich Kriegel
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule string-ii

  (import (tail
           string-i
           c-string-interface
           basic-string
           (only (allocate-on-multiple-size-card) mm-interface))

   syntax (tail)

   export (<string>
           string?
           duplicate-%string
           allocate-%string
           make-string
           string-pointer ;only for intern use for table
           string-append
           equal-string
           )
   )


;;;-----------------------------------------------------------------------------
;;; string?
;;;-----------------------------------------------------------------------------


(defun string? (object)
  (%instance-of? object <string>))

;;;-----------------------------------------------------------------------------
;;; string-append
;;;-----------------------------------------------------------------------------



(defun string-append (string1 string2)
  (%let* ((length1 %signed-word-integer
                   (strlen (string-pointer string1)))
          (new-string-ptr %string
                          (allocate-%string (%plus #%i1
                                                   (%plus length1
                                                          (strlen (string-pointer string2)))))))
         (strcpy new-string-ptr (string-pointer string1))
         (strcpy (%cast %string (%plus (%cast %signed-word-integer new-string-ptr) length1))
                 (string-pointer string2))
         (make-string new-string-ptr)
         ))

;;;-----------------------------------------------------------------------------
;;; equal-string
;;;-----------------------------------------------------------------------------


(defun equal-string (string1 string2)
  (%let ((length1 %signed-word-integer
                  (strlen (string-pointer string1)))
         (length2 %signed-word-integer
                  (strlen (string-pointer string2))))
        (if (%eq length1 length2)
            (if (%eq #%i0 (strcmp (string-pointer string1)
                                  (string-pointer string2)))
                string1
              ())
          ())))

;;;-----------------------------------------------------------------------------
;;; Type schemes for type inference
;;;-----------------------------------------------------------------------------

(%annotate-function
  equal-string new-signature
  (((var0 var1 var2)
    ((var var0) (atom? <object>))
    ((var var1) (atom? <string>))
    ((var var2) (atom? <string>)))))

(%annotate-function
  string? new-signature
  (((var0 var1)
    ((var var0) (atom? <string>))
    ((var var1) (atom? <string>)))
   ((var0 var1)
    ((var var0) (atom? <null>))
    ((var var1) (atom? (and <object> (not <string>)))))))

;;;-----------------------------------------------------------------------------
)  ;; End of module string-ii
;;;-----------------------------------------------------------------------------
