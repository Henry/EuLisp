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
;;; Title:
;;;  Description:
;;;  Authors: Rainer Rosenmuller
;;;-----------------------------------------------------------------------------

(defmodule print
  (import ((only (<class>
                  <object>
                  %extract
                  %cast
                  %string
                  %void
                  %unsigned-byte-integer
                  %unsigned-word-integer
                  %signed-word-integer
                  %eq %neq %le %lt %gt
                  %plus %minus %rem %div
                  make-swi
                  <cons>
                  <null>
                  <list>
                  <symbol>
                  car
                  cdr
                  cons?
                  null?)
                 eulisp-kernel)
           (only (<file>
                  fprintf-3
                  stdout
                  %write-unit %write-string
                  ensure-open-character-output-stream
                  file-descriptor-pointer
                  <stream>
                  file-stream?
                  string-stream?
                  stream-string-stack
                  sprintf-3)
                 stream-i)
           (only (error
                  cerror
                  <condition>)
                 condition-i)
           (only (fprintf-3-double
                  sprintf-3-double)
                 c-stdio)
           stream-generic
           standard-generic-function
           (only (string?
                  string-pointer
                  <string>)
                 string-ii)
           (only (int?
                  <int>)
                 int-i)
           (only (double-float?
                  <double-float>
                  dble)
                 double-float-i)
           (only (convert-char-int
                  character?
                  <character>)
                 character)
           (only (vector?
                  primitive-vector-size
                  primitive-vector-ref
                  <vector>)
                 vector)
           (only ($char-string
                  $char-ascii-extension
                  $char-single-escape
                  $char-ascii-plus
                  $char-ascii-minus
                  $char-ascii-point
                  $char-newline
                  $char-ascii-d-l
                  $char-ascii-a-l
                  $char-ascii-b-l
                  $char-formfeed
                  $char-ascii-f-l
                  $char-return
                  $char-ascii-r-l
                  $char-ascii-tab
                  $char-ascii-t-l
                  $char-string-hex-l
                  $char-ascii-alert
                  $char-ascii-backspace
                  $char-ascii-delete
                  $char-ascii-vertical-tab
                  $char-ascii-l-l
                  $char-ascii-n-l
                  $char-ascii-v-l
                  $char-ascii-zero
                  $char-ascii-space
                  $char-ascii-e-l
                  $char-ascii-d-l
                  letter?
                  other?
                  peculiar-constituent?
                  normal-constituent?
                  extended-level-0-character?)
                 char-tables)
           (only (<string-stack>
                  *buffer-1*
                  push-buffer
                  pop-buffer
                  clear-buffer
                  ?cur-index
                  !cur-index
                  ?last-index
                  ?stack-string)
                 string-stack)
           (only (strlen)
                 c-string-interface))
   syntax (tail)
   export (<write-error>
           prin
           print
           sprint
           write
           output
           newline
           change-exponent-marker
           print-based-int-0   ; nicht el
           ))

;;;----------------------------------------------------------------------------
;;; <write-error>
;;;----------------------------------------------------------------------------
(%define-standard-class (<write-error> <class> )
  <condition>
  (
   (stream type <object> default () accessor stream
           keyword stream)
   (error-number  type <int>
                  default 77 accessor error-number
                  keyword error-number))
  representation pointer-to-struct
  allocation multiple-type-card)

;;;-----------------------------------------------------------------------------
;;; Write primitives
;;;-----------------------------------------------------------------------------
;;   (%define-function (%write-string %signed-word-integer)
;;                        ((stream <stream>)
;;                          (str %string))
;;     (if (file-stream? stream)
;;       (%let ((fd %unsigned-word-integer
;;                     (%cast %unsigned-word-integer
;;                            (file-descriptor-pointer stream))))
;;          (fprintf-3 fd (%literal %string () "%s")
;;                     (%cast %signed-word-integer str))
;;          )
;;       (if (string-stream? stream)
;;         (%let ((fd <string-stack> (stream-string-stack stream))
;;                   (length  %signed-word-integer (strlen str)))
;;             (push-string str #%i0 length fd)
;;             length)
;;         #%i0))
;;      )
;;
;;   (%define-function (push-string %void)
;;                     ((string %string)
;;                      (idx %signed-word-integer)
;;                      (length %signed-word-integer)
;;                      (string-stack <string-stack>))
;;     (if (%lt idx length)
;;       (progn (push-buffer
;;               (%cast %signed-word-integer
;;                      (%extract string idx)) string-stack)
;;              (push-string string (%plus idx #%i1) length string-stack))
;;       ()))

(%define-function (%write-hex %signed-word-integer)
  ((stream <stream>)
   (str <object>))
  (%let ((buf <string-stack> *buffer-1*))
        (clear-buffer buf)
        (%let* ((buf-str %string (?stack-string buf))
                (n-buf %signed-word-integer
                       (sprintf-3
                        buf-str
                        (%literal %string () "#<object %8x>")
                        (%cast %signed-word-integer str))))
               (%write-string stream buf-str)
               n-buf)))

(%define-function (%write-int %signed-word-integer)
  ((stream <stream>)
   (int %signed-word-integer))
  (if (file-stream? stream)
      (%let ((fd <file> (file-descriptor-pointer stream)))
            (fprintf-3 fd (%literal %string () "%d") int))
    (progn
      (clear-buffer (%cast <string-stack> *buffer-1*))
      (print-based-int-0 stream int #%i10 (%cast <string-stack> *buffer-1*))
      #%i1)))

(%define-function (%write-float %signed-word-integer)
  ((stream <stream>)
   (float <double-float>))
  (%let ((buf <string-stack> *buffer-1*))
        (clear-buffer buf)
        (%let* ((buf-str %string (?stack-string buf))
                (n-str %signed-word-integer
                       (sprintf-3-double
                        buf-str
                        (%literal %string () "%le")
                        (dble (%cast <double-float> float)))))
               ;;***HGW (change-exponent-marker buf-str)
               (%write-string stream buf-str)
               n-str)))

(%define-function (change-exponent-marker %void)
  ((string %string))
  (%let ((ch %unsigned-byte-integer (%extract string #%I0)))
        (if (%neq ch #%B0)
            (progn
              (if (%eq ch (%cast %unsigned-byte-integer $char-ascii-e-l))
                  (%setf (%extract string #%I0)
                         (%cast %unsigned-byte-integer $char-ascii-d-l))
                ())
              (change-exponent-marker
               (%cast %string (%plus (%cast %unsigned-word-integer string)
                                     #%I1))))
          ())))

;;(defun flush stream-list      in stream-i
;;  (if stream-list
;;      (let ((stream (car stream-list)))
;;        (if (ensure-open-character-output-stream stream)
;;          (fflush (file-descriptor-pointer stream))
;;          (progn ;(stream-error)
;;              (fflush (%cast %unsigned-word-integer c-stdout)))))
;;       (fflush (%cast %unsigned-word-integer c-stdout))))

;;(defun write-unit (unit . stream-list)
;;  (let ((obj (%cast %signed-word-integer
;;                    (make-swi (convert-char-int unit)))))
;;  (if stream-list
;;      (let ((stream (car stream-list)))
;;        (if (ensure-open-character-output-stream stream)
;;          (%write-unit (%cast %unsigned-word-integer
;;                                  (file-descriptor-pointer stream)) obj)
;;          (progn ;(stream-error)
;;              (%write-unit (%cast %unsigned-word-integer c-stdout) obj))))
;;       (%write-unit (%cast %unsigned-word-integer c-stdout) obj)))
;;  ())

;;;-----------------------------------------------------------------------------
;;;output
;;;-----------------------------------------------------------------------------
(defmethod output ((stream <stream>) (unit <object>))
  ;;(defun output (stream unit)
  (let ((obj (make-swi (convert-char-int unit))))
    (if (ensure-open-character-output-stream stream)
        (%write-unit stream obj)
      ()))
  unit)

;;;-----------------------------------------------------------------------------
;;; newline
;;;-----------------------------------------------------------------------------
(defun newline stream-list
  (if stream-list
      (let ((stream (car stream-list)))
        (if (ensure-open-character-output-stream stream)
            (%write-unit stream $char-newline)
          (progn ;(stream-error)
            (%write-unit stdout $char-newline))))
    (%write-unit stdout $char-newline))
  #\newline)

;;;-----------------------------------------------------------------------------
;;; prin
;;;-----------------------------------------------------------------------------
(defun prin (object . stream-list)
  (if stream-list
      (let ((stream (car stream-list)))
        (if (ensure-open-character-output-stream stream)
            (generic-print object stream)
          (progn ;(stream-error)
            (generic-print object stdout))))
    (generic-print object stdout)))

;; auf generic-print abbilden!
;; f string-streams nicht fd als %unsingned-word-intger definieren und
;; durchreichen (sondern als <object> besser stream durchreichen)

;; To depict generic prin-!
;; f string-stream not defined as fd %unsingned-word-intger
;; reach through (but better than <object> stream reach through)

;;   (%define-function (prin-1 <object>)
;;                     ((object <object>)
;;                      (stream <stream>))
;;      ;  (let ((obj-class (%class-of object))) ;fehlt class-of
;;      ;   (%write-hex stream object)
;;     (if (cons? object)  ;(eq obj-class <cons>)
;;       (prin-cons object stream)
;;       (if (null? object) ;(eq obj-class <null>)
;;         (%write-string stream (%literal %string () "()"))
;;         (if (int? object)
;;           (%write-int stream (make-swi (%cast <int> object)))
;;             (if (symbol? object)
;;                (%write-string stream (%select object <symbol> name))
;;              (if (string? object)
;;                (%write-string stream (string-pointer (%cast <string> object)))
;;    ;; vector
;;                (if (vector? object)
;;         ;; (prin-vector (%cast <vector> object) stream)
;;                     (generic-print (%cast <vector> object) stream)
;;       ;; character
;;                     (if (character? object)
;;                        (%write-unit stream
;;                                           (make-swi
;;                                       (convert-char-int (%cast <character> object))))
;;            ;;float
;;                        (if (double-float? object)
;;                           (%write-float stream (%cast <double-float> object))
;;               ;;             (%write-string stream (%literal %string () ".?."))
;;                         (%write-hex stream object)
;;                          )
;;                      )))))))
;;      object
;;       )

;;;-----------------------------------------------------------------------------
;;; generic-print
;;;-----------------------------------------------------------------------------
(defmethod generic-print ((object <object>) (stream <stream>))
  (%write-hex stream object) object)

(defmethod generic-print ((object <double-float>) (stream <stream>))
  (%write-float stream object) object)

(defmethod generic-print ((object <string>) (stream <stream>))
  (%write-string stream (string-pointer object)) object)

(defmethod generic-print ((object <symbol>) (stream <stream>))
  (%write-string stream (%select object <symbol> name))
  object)

(defmethod generic-print ((class <class>) (stream <stream>))
  (%write-string stream (string-pointer (%select class <class> class-name)))
 class)

(defmethod generic-print ((object <null>) (stream <stream>))
  (%write-string stream (%literal %string () "()")) ())

(defmethod generic-print ((object <int>) (stream <stream>))
  (%write-int stream (make-swi object)) object)

(defmethod generic-print ((object <character>) (stream <stream>))
  (%write-unit stream
               (make-swi (convert-char-int object)))
  object)

(defmethod generic-print ((object <cons>) (stream <stream>))
  (prin-cons object stream) object)

(%define-function (prin-cons <null>)
  ((object <cons>)
   (stream <stream>))
  (%write-string stream (%literal %string () "("))
  (generic-print (car object) stream)
  (prin-cons1 (cdr object) stream)
  (%write-string stream (%literal %string () ")"))
  ())

(%define-function (prin-cons1 <null>)
  ((object <object>)
   (stream <stream>))
  (if (cons? object)
      (progn (%write-string stream (%literal %string  () " "))
             (generic-print (car object) stream)
             (prin-cons1 (cdr object) stream))
    (if (null? object) ()
      (progn (%write-string stream (%literal %string () " . "))
             (generic-print object stream)
             ())))
  )

(defmethod generic-print ((object <vector>) (stream <stream>))
  (prin-vector object stream) object)

(%define-function (prin-vector <null>)
  ((object <vector>)
   (stream <stream>))
  (%write-string stream (%literal %string () "#("))
  (%let ((length %unsigned-word-integer (primitive-vector-size object)))
        (if (%neq length #%I0)
            (progn
              (generic-print (primitive-vector-ref object #%I0) stream)
              (prin-vector1 object length #%I1 stream))
          ()))
  (%write-string stream (%literal %string () ")"))
  ()
  )

(%define-function (prin-vector1 <null>)
  ((object <vector>)
   (length %unsigned-word-integer)
   (idx    %unsigned-word-integer)
   (stream <stream>))
  (if (%lt idx length)
      (progn (%write-string stream (%literal %string  () " "))
             (generic-print (primitive-vector-ref object idx) stream)
             (prin-vector1 object length (%plus idx #%I1) stream))
    ()))

;;;-----------------------------------------------------------------------------
;;; write
;;;-----------------------------------------------------------------------------
(defun write (object . stream-list)
  (if stream-list
      (let ((stream (car stream-list)))
        (if (ensure-open-character-output-stream stream)
            (generic-write object stream)
          (progn ;(stream-error)
            (generic-write object stdout))))
    (generic-write object stdout)))

;;;-----------------------------------------------------------------------------
;;; generite-write
;;;-----------------------------------------------------------------------------
(defmethod generic-write ((object <object>) (stream <stream>))
  (%write-hex stream object) object)

(defmethod generic-write ((object <double-float>) (stream <stream>))
  (%write-float stream object) object)

(defmethod generic-write ((object <null>) (stream <stream>))
  (%write-string stream (%literal %string () "()")) ())

(defmethod generic-write ((object <int>) (stream <stream>))
  (%write-int stream (make-swi object))
  object)

;;;-----------------------------------------------------------------------------

(defmethod generic-write ((object <symbol>) (stream <stream>))
  (write-symbol-1 object stream) object)

(%define-function (write-symbol-1 %void)
  ((object <symbol>)
   (stream <stream>))
  (write-symbol-2 (%select object <symbol>
                           name) stream)
  )

(%define-function (write-symbol-2 %void)
  ((object %string)
   (stream <stream>))
  (%let ((length %signed-word-integer (strlen object))
         (ch1 %signed-word-integer
              (%cast %signed-word-integer (%extract object #%I0))))
        (if (letter? ch1)
            (progn
              (%write-unit stream ch1)
              (write-symbol-nc stream object #%i1 length))
          (if (other? ch1)
              (write-symbol-2-other ch1 length object stream)
            (progn
              (%write-unit stream $char-single-escape)
              (%write-unit stream ch1)
              (write-symbol-nc stream object #%i1 length))))))

(%define-function (write-symbol-2-other %void)
  ((ch1 %signed-word-integer)
   (length %signed-word-integer)
   (object %string)
   (stream <stream>))
  (if (%eq ch1 $char-ascii-plus)
      (if (%eq length #%i1)
          (%write-unit stream ch1)
        (%let ((ch2 %signed-word-integer
                    (%cast %signed-word-integer (%extract object #%I1))))
              (if (peculiar-constituent? ch2)
                  ()
                (%write-unit stream $char-single-escape))
              (%write-unit stream ch1)
              (%write-unit stream ch2)
              (write-symbol-nc stream object #%i2 length)))
    (if (%eq ch1 $char-ascii-minus)
        (if (%eq length #%i1)
            (%write-unit stream ch1)
          (%let ((ch2 %signed-word-integer
                      (%cast %signed-word-integer (%extract object #%I1))))
                (if (peculiar-constituent? ch2)
                    ()
                  (%write-unit stream $char-single-escape))
                (%write-unit stream ch1)
                (%write-unit stream ch2)
                (write-symbol-nc stream object #%i2 length)))
      (if (%eq ch1 $char-ascii-point)
          (if (%eq length #%i1)
              (progn
                (%write-unit stream $char-single-escape)
                (%write-unit stream ch1))
            (%let ((ch2 %signed-word-integer
                        (%cast %signed-word-integer (%extract object #%I1))))
                  (if (peculiar-constituent? ch2)
                      ()
                    (%write-unit stream $char-single-escape))
                  (%write-unit stream ch1)
                  (%write-unit stream ch2)
                  (write-symbol-nc stream object #%i2 length)))
        (progn
          (%write-unit stream ch1)
          (write-symbol-nc stream object #%i1 length))
        ))))


(%define-function (write-symbol-nc %void)
  ((stream <stream>)
   (object %string)
   (idx %signed-word-integer)
   (length %signed-word-integer))
  (if (%lt idx length)
      (%let ((ch1 %signed-word-integer
                  (%cast %signed-word-integer (%extract object idx))))
            (if (normal-constituent? ch1)
                ()
              (%write-unit stream $char-single-escape))
            (%write-unit stream ch1)
            (write-symbol-nc stream object (%plus idx #%i1) length))
    ()))

;;;-----------------------------------------------------------------------------

(defmethod generic-write ((object <string>) (stream <stream>))
  (write-string-1 object stream) object)

(%define-function (write-string-1 %void)
  ((object <string>)
   (stream <stream>))
  ;;(%write-string stream (string-pointer (%cast <string> object)))
  (%write-unit stream $char-string)
  (write-string-2 stream (string-pointer object)
                  #%i0 (strlen (string-pointer object)))
  (%write-unit stream $char-string)
  )

(%define-function (write-string-2 %void)
  ((stream <stream>)
   (object %string)
   (idx %signed-word-integer)
   (length %signed-word-integer))
  (if (%lt idx length)
      (%let ((ch1 %signed-word-integer
                  (%cast %signed-word-integer (%extract object idx))))
            (if (extended-level-0-character? ch1)
                (progn
                  (if (%eq ch1 $char-string)
                      (%write-unit stream $char-single-escape)
                    (if (%eq ch1 $char-single-escape)
                        (%write-unit stream $char-single-escape)
                      ()))
                  (%write-unit stream ch1))
              (if (%eq ch1 $char-ascii-space) ; auf Wunsch des gro§en Blonden
                  (%write-unit stream $char-ascii-space)
                (progn
                  (%write-unit stream $char-single-escape)
                  (if (%eq ch1 $char-ascii-delete)
                      (%write-unit stream $char-ascii-d-l)
                    (if (%eq ch1 $char-ascii-alert)
                        (%write-unit stream $char-ascii-a-l)
                      (if (%eq ch1 $char-ascii-backspace)
                          (%write-unit stream $char-ascii-b-l)
                        (if (%eq ch1 $char-formfeed)
                            (%write-unit stream $char-ascii-f-l)
                          (if (%eq ch1 $char-newline)
                              (%write-unit stream $char-ascii-n-l)
                            (if (%eq ch1 $char-return)
                                (%write-unit stream $char-ascii-r-l)
                              (if (%eq ch1 $char-ascii-tab)
                                  (%write-unit stream $char-ascii-t-l)
                                (if (%eq ch1 $char-ascii-vertical-tab)
                                    (%write-unit stream $char-ascii-v-l)
                                  (progn
                                    (%write-unit stream $char-string-hex-l)
                                    (%write-unit stream $char-ascii-zero)
                                    (%write-unit stream $char-ascii-zero)
                                    (if (%le ch1 #%i15)
                                        (%write-unit stream $char-ascii-zero)
                                      ())
                                    (clear-buffer (%cast <string-stack> *buffer-1*))
                                    (print-based-int-0 stream
                                                       ch1 #%i16
                                                       (%cast <string-stack> *buffer-1*))
                                    ))))))))))))
            (write-string-2 stream object (%plus idx #%i1) length))
    ()))

(%define-function (print-based-int-0 %void)
  ((stream <stream>)
   (object %signed-word-integer)
   (base %signed-word-integer)
   (string-stack <string-stack>))
  (push-buffer (fig2char (%rem object base)) string-stack)
  (%let ((nobj %signed-word-integer (%div object base)))
        (if (%gt nobj #%i0)
            (print-based-int-0 stream nobj base string-stack)
          (print-based-int-01 stream string-stack))))

(%define-function (print-based-int-01 %void)
  ((stream <stream>)
   (string-stack <string-stack>))
  (%let ((ch %signed-word-integer
             (pop-buffer string-stack)))
        (if (%eq ch #%i-1)
            ()
          (progn (%write-unit stream ch)
                 (print-based-int-01 stream string-stack)))))

(%define-function (fig2char %signed-word-integer)
  ((fig %signed-word-integer))
  (if (%lt fig #%i10)
      (%plus fig $char-ascii-zero)
    (%plus fig (%minus $char-ascii-a-l #%i10))))

;;--------------------------------------------------------------------------

(defmethod generic-write ((object <vector>) (stream <stream>))
  (write-vector-1 object stream) object)

(%define-function (write-vector-1 <null>)
  ((object <vector>)
   (stream <stream>))
  (%write-string stream (%literal %string () "#("))
  (%let ((length %unsigned-word-integer (primitive-vector-size object)))
        (if (%neq length #%I0)
            (progn
              (generic-write (primitive-vector-ref object #%I0) stream)
              (write-vector-2 object length #%I1 stream))
          ()))
  (%write-string stream (%literal %string () ")"))
  ()
  )

(%define-function (write-vector-2 <null>)
  ((object <vector>)
   (length %unsigned-word-integer)
   (idx    %unsigned-word-integer)
   (stream <stream>))
  (if (%lt idx length)
      (progn (%write-string stream (%literal %string  () " "))
             (generic-write (primitive-vector-ref object idx) stream)
             (write-vector-2 object length (%plus idx #%I1) stream))
    ()))

;;--------------------------------------------------------------------------

(defmethod generic-write ((object <character>) (stream <stream>))
  (write-character-1 object stream) object)

(%define-function (write-character-1 %void)
  ((object <character>)
   (stream <stream>))
  (%write-unit stream $char-ascii-extension)
  (%write-unit stream $char-single-escape)
  (%let ((ch1 %signed-word-integer
              (make-swi (convert-char-int object))))
        (if (extended-level-0-character? ch1)
            (%write-unit stream ch1)
          (progn
            (if (%eq ch1 $char-ascii-delete)
                (%write-string stream (%literal %string () "delete"))
              (if (%eq ch1 $char-ascii-space)
                  (%write-string stream (%literal %string () "space"))
                (if (%eq ch1 $char-ascii-alert)
                    (%write-string stream (%literal %string () "alert"))
                  (if (%eq ch1 $char-ascii-backspace)
                      (%write-string stream (%literal %string () "backspace"))
                    (if (%eq ch1 $char-formfeed)
                        (%write-string stream (%literal %string () "formfeed"))
                      (if (%eq ch1 $char-newline)
                          (%write-string stream (%literal %string () "newline"))
                        (if (%eq ch1 $char-return)
                            (%write-string stream (%literal %string () "return"))
                          (if (%eq ch1 $char-ascii-tab)
                              (%write-string stream (%literal %string () "tab"))
                            (if (%eq ch1 $char-ascii-vertical-tab)
                                (%write-string stream (%literal %string () "vertical-tab"))
                              (progn
                                (%write-unit stream $char-string-hex-l)
                                (%write-unit stream $char-ascii-zero)
                                (%write-unit stream $char-ascii-zero)
                                (if (%le ch1 #%i15)
                                    (%write-unit stream $char-ascii-zero)
                                  ())
                                (clear-buffer (%cast <string-stack> *buffer-1*))
                                (print-based-int-0 stream
                                                   ch1 #%i16
                                                   (%cast <string-stack> *buffer-1*))
                                ))))))))))))))

;;---------------------------------------------------------------------------

(defmethod generic-write ((object <cons>) (stream <stream>))
  (write-cons-1 object stream) object)

(%define-function (write-cons-1 <null>)
  ((object <cons>)
   (stream <stream>))
  (%write-string stream (%literal %string () "("))
  (generic-write (car object) stream)
  (write-cons-2 (cdr object) stream)
  (%write-string stream (%literal %string () ")"))
  ()
  )

(%define-function (write-cons-2 <null>)
  ((object <object>)
   (stream <stream>))
  (if (cons? object)
      (progn (%write-string stream (%literal %string  () " "))
             (generic-write (car object) stream)
             (write-cons-2 (cdr object) stream))
    (if (null? object) ()
      (progn (%write-string stream (%literal %string () " . "))
             (generic-write object stream)
             ())))
  )

;;;-----------------------------------------------------------------------------
;;; print
;;;-----------------------------------------------------------------------------
(defun print (object . stream-list)
  (if stream-list
      (%let ((stream <stream> (%cast <stream> (car stream-list))))
            (if (ensure-open-character-output-stream stream)
                (print-1 object stream)
              (progn ;(stream-error)
                (print-1 object (%cast <stream> stdout)))))
    (print-1 object (%cast <stream> stdout)))
  object)

(%define-function (print-1 %void) ((object <object>) (stream <stream>))
  (generic-print object stream)
  (%write-unit stream $char-newline))

;; (%define-function (sprint <stream>)
;;   ((stream <stream>)
;;    (object <object>))
;;   (if (ensure-open-character-output-stream stream)
;;       (print-1 object stream)
;;     (progn                              ; (stream-error)
;;       (print-1 object (%cast <stream> stdout))))
;;   stream)

(%define-function (sprint <stream>) ((stream <stream>) . objects)
  (if objects
      (if (ensure-open-character-output-stream stream)
          (sprint-aux stream objects)
        (cerror <write-error> "Stream is not open for output" 'stream stream))
    ())
  (%write-unit stream $char-newline)
  stream)

;; (defun sprint-aux (stream objects)
;;   (if objects
;;       (progn
;;         (generic-print (car objects) stream)
;;         (sprint-aux stream (cdr objects)))
;;     ()))

(%define-function (sprint-aux <null>) ((stream <stream>) objects)
  (if objects
      (progn
        (generic-print (car objects) stream)
        (sprint-aux stream (cdr objects)))
    ()))

;;;-----------------------------------------------------------------------------
;;; Type schemes for type inference
;;;-----------------------------------------------------------------------------
;;(%annotate-function
;; %write-string new-signature
;; (((var0 var1 var2)
;;   ((var var0) (atom? %signed-word-integer))
;;   ((var var1) (atom? <stream>))
;;   ((var var2) (atom? %string)))))

;;(%annotate-function
;; push-string new-signature
;; (((var0 var1 var2 var3 var4)
;;   ((var var0) (atom? %void))
;;   ((var var1) (atom? %string))
;;   ((var var2) (atom? %signed-word-integer))
;;   ((var var3) (atom? %signed-word-integer))
;;   ((var var4) (atom? <string-stack>)))))

(%annotate-function
  %write-hex new-signature
  (((var0 var1 var2)
    ((var var0) (atom? %signed-word-integer))
    ((var var1) (atom? <stream>))
    ((var var2) (atom? <object>)))))

(%annotate-function
  %write-int new-signature
  (((var0 var1 var2)
    ((var var0) (atom? %signed-word-integer))
    ((var var1) (atom? <stream>))
    ((var var2) (atom? %signed-word-integer)))))

(%annotate-function
  %write-float new-signature
  (((var0 var1 var2)
    ((var var0) (atom? %signed-word-integer))
    ((var var1) (atom? <stream>))
    ((var var2) (atom? <double-float>)))))

(%annotate-function
  change-exponent-marker new-signature
  (((var0 var1)
    ((var var0) (atom? %void))
    ((var var1) (atom? %string)))))

(%annotate-function
  newline new-signature
  (((var0 var1)
    ((var var0) (atom? <object>))
    ((var var1) (atom? <list>)))))

(%annotate-function
  prin new-signature
  (((var0 var1 var2)
    ((var var0) (atom? <object>))
    ((var var1) (var var0))
    ((var var2) (atom? <list>)))))

(%annotate-function
  prin-cons new-signature
  (((var0 var1 var2)
    ((var var0) (atom? <null>))
    ((var var1) (atom? <cons>))
    ((var var2) (atom? <stream>)))))

(%annotate-function
  prin-cons1 new-signature
  (((var0 var1 var2)
    ((var var0) (atom? <null>))
    ((var var1) (atom? <object>))
    ((var var2) (atom? <stream>)))))

(%annotate-function
  prin-vector new-signature
  (((var0 var1 var2)
    ((var var0) (atom? <null>))
    ((var var1) (atom? <vector>))
    ((var var2) (atom? <stream>)))))

(%annotate-function
  prin-vector1 new-signature
  (((var0 var1 var2 var3 var4)
    ((var var0) (atom? <null>))
    ((var var1) (atom? <vector>))
    ((var var2) (atom? %unsigned-word-integer))
    ((var var3) (atom? %unsigned-word-integer))
    ((var var4) (atom? <stream>)))))

(%annotate-function
  write new-signature
  (((var0 var1 var2)
    ((var var0) (atom? <object>))
    ((var var1) (var var0))
    ((var var2) (atom? <list>)))))

;;(%annotate-function
;; write-1 new-signature
;; (((var0 var1 var2)
;;   ((var var0) (atom? <object>))
;;   ((var var1) (atom? <object>))
;;   ((var var2) (atom? <stream>)))))

(%annotate-function
  write-symbol-1 new-signature
  (((var0 var1 var2)
    ((var var0) (atom? %void))
    ((var var1) (atom? <symbol>))
    ((var var2) (atom? <stream>)))))

(%annotate-function
  write-symbol-2 new-signature
  (((var0 var1 var2)
    ((var var0) (atom? %void))
    ((var var1) (atom? %string))
    ((var var2) (atom? <stream>)))))

(%annotate-function
  write-symbol-nc new-signature
  (((var0 var1 var2 var3 var4)
    ((var var0) (atom? %void))
    ((var var1) (atom? <stream>))
    ((var var2) (atom? %string))
    ((var var3) (atom? %signed-word-integer))
    ((var var4) (atom? %signed-word-integer)))))

;;(%annotate-function
;; prin-1 new-signature
;; (((var0 var1 var2)
;;   ((var var0) (atom? <object>))
;;   ((var var1) (atom? <object>))
;;   ((var var2) (atom? <stream>)))))

(%annotate-function
  print new-signature
  (((var0 var1 var2)
    ((var var0) (atom? <object>))
    ((var var1) (var var0))
    ((var var2) (atom? <list>)))))

;;;-----------------------------------------------------------------------------
)  ;; End of module print
;;;-----------------------------------------------------------------------------
