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
;;; Title: string-stack
;;;  Description:
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Rainer Rosenmuller
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------
(defmodule string-stack

  (import
   ;;------
   ((only (<class> <object>
                   %lt %plus %minus %extract %cast
                   %string %void
                   %signed-word-integer
                   %signed-byte-integer
                   %unsigned-byte-integer ;; vermutlich f %string f zugriffstypen
                   ) tail)
    ;;(only (allocate-%string) basic-class-init)
    (only (strcpy) c-string-interface)
    (only (make-string <string> duplicate-%string
                       allocate-%string) string-ii)
    )


   syntax
   ;;------
   (tail )

   export
   ;;------
   (    <string-stack>
        ?stack-string
        ?cur-index
        !cur-index
        ?last-index
        make-string-stack
        *buffer-1*
        *buffer-2*
        push-buffer
        pop-buffer
        clear-buffer
        getc-buffer
        ungetc-buffer)
   )

;;(%define-variable *buffer-1* %string (%literal %string #i255))
;;(%define-variable *buffer-2* %string (%literal %string #i255))

(%define-standard-class (<string-stack> <class>)
  <object>
  ((stack-string type %string
                 keyword stack-string
                 reader ?stack-string
                 writer !stack-string)
   (cur-index type %signed-word-integer default #%i0
              reader ?cur-index
              writer !cur-index)
   (last-index type %signed-word-integer
               keyword last-index
               reader ?last-index
               writer !last-index))
  constructor (make-string-stack stack-string last-index)
  ;; last index is one element shorter then the actually length (C string)
  allocation single-card
  representation pointer-to-struct)

;;(%define-variable *buffer-1* <string-stack>  ;; hier darf nur ein literal stehen
;;                  (make-string-stack (%literal %string #%i255 "?") #%i255))
;;(deflocal *buffer-1* ;<string-stack>
;;                  (make-string-stack (%literal %string #%i255 "?") #%i255))
;;! in generate-code-for-local-data erscheint als Lge :
;;! (%literal %signed-word-integer 255)
;;! frt zu fehler mit < !!

;; for read token
;;(deflocal *buffer-1*  ;;<string-stack>
;;  (make-string-stack (%literal %string 120 "?") #%i120))

;; for read token
;;(deflocal *buffer-2* ;<string-stack>
;;  (make-string-stack (%literal %string 120 "?") #%i120))

(deflocal *buffer-1*  ;;<string-stack>
  (make-string-stack (allocate-%string #%i120) #%i120))

;; for read token
(deflocal *buffer-2* ;<string-stack>
  (make-string-stack (allocate-%string #%i120) #%i120))

(%define-function (push-buffer %void)
  ((obj %signed-word-integer)
   (stack <string-stack>))
  (if (%lt (?cur-index stack) (?last-index stack))
      (progn
        (%setf (%extract (?stack-string stack) (?cur-index stack))
               (%cast %unsigned-byte-integer obj))
        (!cur-index stack (%plus #%i1 (?cur-index stack))))
    (progn (%setf (%extract (?stack-string stack) (?last-index stack)) #%B0)
           (!stack-string stack
                          (%let ((new-string-ptr %string
                                                 (allocate-%string (%plus (?last-index stack)
                                                                          (?last-index stack)))))
                                (strcpy new-string-ptr (?stack-string stack))
                                new-string-ptr))
           (!last-index stack (%plus (?last-index stack) (?last-index
                                                          stack)))
           (push-buffer obj stack)
           )))

(%define-function (pop-buffer %signed-word-integer)
  ((stack <string-stack>))
  (if (%lt (?cur-index stack) #%i1)
      #%i-1
    (progn
      (!cur-index stack (%minus (?cur-index stack) #%i1))
      (%cast %signed-word-integer
             (%extract (?stack-string stack) (?cur-index stack))))))

(%define-function (ungetc-buffer %void)
  ((obj %signed-word-integer)
   (stack <string-stack>))
  (if (%lt #%i0 (?cur-index stack))
      (!cur-index stack (%minus (?cur-index stack) #%i1))
    ()))

(%define-function (getc-buffer %signed-word-integer)
  ((stack <string-stack>))
  (if (%lt (?cur-index stack) (?last-index stack))
      (%let ((ch %signed-word-integer
                 (%cast %signed-word-integer
                        (%extract (?stack-string stack) (?cur-index stack)))))
            (!cur-index stack (%plus (?cur-index stack) #%i1))
            ch)
    #%i-1))

(%define-function (clear-buffer %void)
  ((stack <string-stack>))
  (!cur-index stack #%i0))
)
