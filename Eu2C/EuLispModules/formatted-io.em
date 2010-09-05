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
;;;  Title:
;;;  Description:
;;;  Authors: Rainer Rosenmuller
;;;-----------------------------------------------------------------------------

(defmodule formatted-io
  (import ((only (<class>
                  <list>
                  <object>
                  %cast
                  %signed-word-integer
                  %unsigned-word-integer
                  %signed-byte-integer
                  %unsigned-byte-integer
                  %void
                  %string
                  %double-float
                  %gt
                  %lt
                  %extract
                  %eq
                  %plus
                  %minus
                  %mult
                  make-swi
                  make-fpint
                  cons
                  car
                  cdr
                  cons?
                  eq)
                 tail)
           (only (error
                  <condition>)
                 condition-i)
           (only (int?)
                 int-i)
           (only ($standard-output
                  %write-unit
                  %read-unit
                  %peek-unit
                  ensure-open-character-output-stream
                  <stream>
                  <string-stream>
                  make-string-stream
                  convert-stream-string
                  %write-string)
                 stream-i)
           (only ($char-formfeed
                  $char-return
                  $char-newline
                  $char-tilde
                  $char-ascii-a-l
                  $char-ascii-%
                  $char-ascii-s-l
                  $char-ascii-b-l
                  $char-ascii-c-l
                  $char-ascii-d-l
                  $char-ascii-e-l
                  $char-ascii-f-l
                  $char-ascii-g-l
                  $char-ascii-o-l
                  $char-ascii-r-l
                  $char-ascii-t-l
                  $char-ascii-&
                  $char-ascii-page-seperator
                  $char-ascii-tab
                  $char-ascii-point
                  $char-string-hex-l
                  digit?
                  digit2figure10
                  char-class
                  *char-class-token*)
                 char-tables)
           (only (print-based-int-0
                  change-exponent-marker ;; %write-string
                  print) ;; nur fur test
                 print)
           (only (generic-prin
                  generic-write ) stream-generic)
           (only (symbol?)
                 symbol);; inserted by ak
           (only (null?)
                 basic-list-0)         ; inserted by ak
           (only (read
                  read-based-int1)
                 read)
           (only (integer?)
                 integer)
           (only (float?)
                 float-i)
           (only (<double-float>
                  make-dble
                  set-dble
                  dble)
                 double-float-i)
           (only (sscanf-3
                  sprintf-3-double)
                 c-stdio)
           (only (<character>
                  character?)
                 character)
           (only (*buffer-1*
                  <string-stack>
                  clear-buffer
                  push-buffer
                  ?stack-string
                  make-string-stack)
                 string-stack)
           (only (string?
                  string-pointer
                  <string>
                  make-string
                  allocate-%string
                  duplicate-%string)
                 string-ii)
           (only (strlen)
                 c-string-interface))
   syntax (tail)
   export (format
           scan))

(%define-standard-class (<scan-mismatch> <class> )
  <condition>
  ((format-string type <string> default ""
                  accessor format-string
                  keyword format-string)
   (input type <list> default ()
          accessor input
          keyword input))
  representation pointer-to-struct
  allocation multiple-type-card)

(%define-variable $temp-format-string-stream <string-stream>)

(setq $temp-format-string-stream
      (make-string-stream
       (make-string-stack (allocate-%string #%i128) #%i128)
       'output))

(defun format (stream fstring . args)
  (if stream
      (progn
        (if (symbol? stream) ; symbol? added by ak to avoid c-compiler warning
            (progn           ; if global optimization is on
              (if (null? (eq stream 't))
                  (error
                   "~Warning: the current output stream is designated by t")
                ())
              (format1 $standard-output
                       (string-pointer fstring)
                       #%i0
                       (strlen (string-pointer fstring))
                       args))
          (format1 stream
                   (string-pointer fstring)
                   #%i0
                   (strlen (string-pointer fstring))
                   args)))
    (progn
      (format1 $temp-format-string-stream
               (string-pointer fstring)
               #%i0
               (strlen (string-pointer fstring))
               args)
      (convert-stream-string $temp-format-string-stream))))

(defglobal *tilde-index* 0)

(%define-function (format1 <object>)
  ((stream <stream>)
   (fstring-c %string)
   (cur-index %signed-word-integer)
   (length %signed-word-integer)
   (args <object>))
  (if (%lt cur-index length)
      (%let ((ch %signed-word-integer
                 (%cast %signed-word-integer (%extract fstring-c cur-index))))
            (if (%eq ch $char-tilde)
                (progn
                  (dynamic-setq *tilde-index* (make-fpint cur-index))
                  (format2 stream fstring-c
                           (%plus #%i1 cur-index) length args #%i0 #%i0))
              (progn (%write-unit stream ch)
                     (format1 stream fstring-c
                              (%plus #%i1 cur-index) length args))))
    args)
  ) ;;args)           ;???????

(%define-function (format2 <object>)
  ((stream <stream>)
   (fstring-c %string)
   (cur-index %signed-word-integer)
   (length %signed-word-integer)
   (args <object>)
   (fp1-n %signed-word-integer)
   (fp2-m %signed-word-integer))
  (if (%lt cur-index length)
      (%let ((ch %signed-word-integer
                 (%cast %signed-word-integer (%extract fstring-c
                                                       cur-index))))
            (if (digit? ch)
                (format2 stream fstring-c (%plus #%i1 cur-index) length args
                         (%plus (%mult fp1-n #%i10)
                                (digit2figure10 ch))
                         fp2-m)
              (if (%eq ch $char-ascii-point)
                  (format2 stream
                           fstring-c
                           (%plus #%i1 cur-index) length args #%i0 fp1-n)
                (if (%eq ch $char-ascii-&)
                    (progn (%write-unit stream $char-newline)
                           (format1 stream
                                    fstring-c
                                    (%plus #%i1 cur-index) length args))
                  (if (%eq ch $char-ascii-t-l)
                      (progn (write-n-tabs stream fp1-n)
                             (format1 stream
                                      fstring-c
                                      (%plus #%i1 cur-index) length args))
                    (if (%eq ch $char-ascii-page-seperator)
                        (progn (%write-unit stream $char-formfeed)
                               (format1 stream
                                        fstring-c
                                        (%plus #%i1 cur-index) length args))
                      (if (%eq ch $char-ascii-%)
                          (progn (%write-unit stream $char-newline)
                                 (format1 stream fstring-c
                                          (%plus #%i1 cur-index) length args))
                        (if (%eq ch $char-tilde)
                            (progn (%write-unit stream $char-tilde)
                                   (format1 stream
                                            fstring-c
                                            (%plus #%i1 cur-index)
                                            length args))
                          (if (cons? args)
                              (format2-cons stream
                                            fstring-c
                                            cur-index length args fp1-n ch)
                            () ; no cons for argument using commands
                            )))))))))
    args))

(%define-function (format2-cons <object>)
  ((stream <stream>)
   (fstring-c %string)
   (cur-index %signed-word-integer)
   (length %signed-word-integer)
   (args <object>)
   (fp1-n %signed-word-integer)
   (ch %signed-word-integer))
  (if (%eq ch $char-ascii-a-l)
      (generic-prin (car args) stream )
    (if (%eq ch $char-ascii-s-l)
        (generic-write (car args) stream )
      (if (%eq ch $char-ascii-d-l)
          (generic-write (car args) stream ) ;write test int
        (if (%eq ch $char-ascii-b-l) ;test int
            (if (int? (car args))
                (progn (clear-buffer (%cast <string-stack> *buffer-1*))
                       (print-based-int-0 stream
                                          (make-swi (car args))
                                          #%i2
                                          (%cast <string-stack> *buffer-1*)))
              (generic-write (car args) stream ))
          (format2-cons-1 stream fstring-c cur-index length args fp1-n ch)))))
  (format1 stream fstring-c (%plus #%i1 cur-index) length (cdr args)))

(%define-function (format2-cons-1 %void)
  ((stream <stream>)
   (fstring-c %string)
   (cur-index %signed-word-integer)
   (length %signed-word-integer)
   (args <object>)
   (fp1-n %signed-word-integer)
   (ch %signed-word-integer))
  (if (%eq ch $char-ascii-c-l)
      (generic-write (car args) stream ) ;s.o. usw. write testchar
    (if (%eq ch $char-ascii-e-l)
        (format-float stream (car args) fstring-c cur-index)
      (if (%eq ch $char-ascii-f-l)
          (format-float stream (car args) fstring-c cur-index)
        (if (%eq ch $char-ascii-g-l)
            (format-float stream (car args) fstring-c cur-index)
          (if (%eq ch $char-ascii-o-l)
              (if (int? (car args))
                  (progn (clear-buffer (%cast <string-stack> *buffer-1*))
                         (print-based-int-0
                          stream
                          (make-swi (car args))
                          #%i8
                          (%cast <string-stack> *buffer-1*)))
                (generic-write (car args) stream ))
            (if (%eq ch $char-string-hex-l)
                (if (int? (car args))
                    (progn (clear-buffer (%cast <string-stack> *buffer-1*))
                           (print-based-int-0
                            stream
                            (make-swi (car args))
                            #%i16
                            (%cast <string-stack> *buffer-1*)))
                  (generic-write (car args) stream ))
              (if (%eq ch $char-ascii-r-l)
                  (if (int? (car args))
                      (progn (clear-buffer (%cast <string-stack> *buffer-1*))
                             (print-based-int-0
                              stream
                              (make-swi (car args))
                              fp1-n
                              (%cast <string-stack> *buffer-1*)))
                    (generic-write (car args) stream ))
                () ; bad command char
                ))))))))

(%define-function (format-float %void)
  ((stream <stream>)
   (float <double-float>)
   (fstring-c %string)
   (cur-idx %signed-word-integer))
  (%let ((buf <string-stack> *buffer-1*)
         (tilde-idx %signed-word-integer
                    (make-swi (dynamic *tilde-index*))))
        (clear-buffer buf)
        (%setf (%extract fstring-c tilde-idx)
               (%cast %unsigned-byte-integer $char-ascii-%))
        (%let* ((idx+ %signed-word-integer (%plus cur-idx #%i1))
                (ch-old %unsigned-byte-integer (%extract fstring-c idx+)))
               (%setf (%extract fstring-c idx+) #%B0)
               (%let* ((buf-str %string (?stack-string buf))
                       (n-str %signed-word-integer
                              (sprintf-3-double
                               buf-str
                               (%cast %string
                                      (%plus
                                       (%cast %signed-word-integer fstring-c)
                                       tilde-idx))
                               (dble (%cast <double-float> float)))))
                      ;;***HGW (change-exponent-marker buf-str)
                      (%write-string stream buf-str))
               (%setf (%extract fstring-c idx+) ch-old))
        (%setf (%extract fstring-c tilde-idx)
               (%cast %unsigned-byte-integer $char-tilde))))

(%define-function (write-n-tabs %void)
  ((stream <stream>)
   (n %signed-word-integer))
  (%write-unit stream $char-ascii-tab)
  (if (%gt n #%i1)
      (write-n-tabs stream (%minus n #%i1))
    ()))

(defglobal last-scan-string "")

(defglobal saved-cur-index 0)

(defun scan (fstring . stream-list)
  (dynamic-let ((last-scan-string fstring)
                (saved-cur-index 0))
               (let ((res
                      (if stream-list
                          (scan1 (car stream-list)
                                 (string-pointer fstring)
                                 #%i0
                                 (strlen (string-pointer fstring))
                                 )
                        (scan1 $standard-output
                               (string-pointer fstring)
                               #%i0
                               (strlen (string-pointer fstring))
                               ))))
                 (if (%gt (make-swi (dynamic saved-cur-index)) #%i0)
                     (error "scan mismatch" <scan-mismatch>
                            'format-string (dynamic fstring)
                            'input res)
                   res))))

(%define-function (scan1 <list>)
  ((stream <stream>)
   (fstring-c %string)
   (cur-index %signed-word-integer)
   (length %signed-word-integer))
  (if (%lt cur-index length)
      (%let ((ch %signed-word-integer
                 (%cast %signed-word-integer (%extract fstring-c cur-index))))
            (if (%eq ch $char-tilde)
                (scan2 stream fstring-c (%plus #%i1 cur-index) length #%i0)
              (scan1 stream fstring-c (%plus #%i1 cur-index) length)))
    ()))

(%define-function (scan2 <list>)
  ((stream <stream>)
   (fstring-c %string)
   (cur-index %signed-word-integer)
   (length %signed-word-integer)
   (fp1-n %signed-word-integer))
  (if (%lt cur-index length)
      (%let ((ch %signed-word-integer
                 (%cast %signed-word-integer (%extract fstring-c
                                                       cur-index))))
            (if (digit? ch)
                (scan2 stream fstring-c (%plus #%i1 cur-index) length
                       (%plus (%mult fp1-n #%i10)
                              (digit2figure10 ch)))
              (if (%eq ch $char-ascii-%)
                  (progn (scan1 stream
                                fstring-c (%plus #%i1 cur-index) length))
                (if (%eq ch $char-ascii-a-l)
                    (cons (read stream)
                          (scan1 stream
                                 fstring-c (%plus #%i1 cur-index) length))
                  (if (%eq ch $char-ascii-d-l)
                      (read-integer-0 stream
                                      #%i10 fstring-c
                                      (%plus #%i1 cur-index) length)
                    (if (%eq ch $char-ascii-b-l)
                        (read-integer-0 stream #%i2 fstring-c
                                        (%plus #%i1 cur-index) length)
                      (scan2-1 stream
                               fstring-c cur-index length fp1-n ch)))))))
    ()))

(%define-function (scan2-1 <list>)
  ((stream <stream>)
   (fstring-c %string)
   (cur-index %signed-word-integer)
   (length %signed-word-integer)
   (fp1-n %signed-word-integer)
   (ch %signed-word-integer))
  (if (%eq ch $char-ascii-c-l)
      (read-character-0 stream fstring-c
                        (%plus #%i1 cur-index) length)
    (if (%eq ch $char-ascii-e-l)
        (read-float-0 stream fp1-n ch fstring-c
                      (%plus #%i1 cur-index) length)
      (if (%eq ch $char-ascii-f-l)
          (read-float-0 stream fp1-n ch fstring-c
                        (%plus #%i1 cur-index) length)
        (if (%eq ch $char-ascii-o-l)
            (read-integer-0 stream #%i8 fstring-c
                            (%plus #%i1 cur-index) length)
          (if (%eq ch $char-string-hex-l)
              (read-integer-0 stream #%i16 fstring-c
                              (%plus #%i1 cur-index) length)
            (if (%eq ch $char-ascii-r-l)
                (read-integer-0 stream fp1-n fstring-c
                                (%plus #%i1 cur-index) length)
              ;; bad command char
              (progn (dynamic-setq saved-cur-index
                                   (make-fpint cur-index))
                     ()))))))))

(%define-function (read-integer-0 <list>)
  ((stream <stream>)
   (base %signed-word-integer)
   (fstring-c %string)
   (cur-index %signed-word-integer)
   (length %signed-word-integer))
  (let ((obj (%cast <object> (read-integer stream base))))
    (if (integer? obj)
        (cons obj (scan1 stream fstring-c cur-index length))
      (progn (dynamic-setq saved-cur-index
                           (make-fpint cur-index))
             (cons obj ())))))

(%define-function (read-integer <object>)
  ((stream <stream>)
   (base %signed-word-integer))
  (let ((obj (read-based-int1 stream base #%i0)))
    (%let ((ch-last %signed-word-integer (%peek-unit stream)))
          (if (%eq (char-class
                    *char-class-token*
                    (%cast %unsigned-word-integer ch-last)) #%b9) ;whitespace
              (%read-unit stream)
            ())
          obj)))

(%define-function (read-character-0 <list>)
  ((stream <stream>)
   (fstring-c %string)
   (cur-index %signed-word-integer)
   (length %signed-word-integer))
  (let ((obj (read-character stream)))
    (if (character? obj)
        (cons obj (scan1 stream fstring-c cur-index length))
      (progn (dynamic-setq saved-cur-index
                           (make-fpint cur-index))
             (cons obj ())))))

(%define-function (read-character <object>)
  ((stream <stream>))
  (let ((obj (read stream)))
    obj ))


(%define-function (read-float-0 <list>)
  ((stream <stream>)
   (float-length %signed-word-integer)
   (ch %signed-word-integer)
   (fstring-c %string)
   (cur-index %signed-word-integer)
   (length %signed-word-integer))
  (let ((obj (%cast <object> (read-float stream float-length ch))))
    (if (float? obj)
        (cons obj (scan1 stream fstring-c cur-index length))
      (progn (dynamic-setq saved-cur-index
                           (make-fpint cur-index))
             (cons obj ())))))

(%define-function (read-float <object>)
  ((stream <stream>)
   (length %signed-word-integer)
   (ch %signed-word-integer))
  (let ((obj (read-float-1 stream length #%i0 *buffer-1* ch)))
    obj))

(%define-function (read-float-1 <double-float>)
  ((stream <stream>)
   (length %signed-word-integer)
   (idx %signed-word-integer)
   (string-stack <string-stack>)
   (ch %signed-word-integer))
  (if (%gt length #%i0)
      (if (%gt length idx)
          (%let ((ch1 %signed-word-integer (%read-unit stream)))
                (if (%eq ch1 $char-ascii-d-l)
                    (push-buffer $char-ascii-e-l string-stack)
                  (push-buffer ch1 string-stack))
                (read-float-1 stream length (%plus idx #%i1) string-stack ch))
        (progn
          (push-buffer #%i0 string-stack)
          (%let ((float-pointer <double-float>
                                (make-dble (%cast %double-float
                                                  0))))
                (sscanf-3 (?stack-string string-stack)
                          (if (%eq ch $char-ascii-e-l)
                              (%literal %string () "%le")
                            (%literal %string () "%lf"))
                          (%cast %unsigned-word-integer float-pointer))
                (clear-buffer (%cast <string-stack> *buffer-1*))
                (set-dble float-pointer (dble float-pointer))
                float-pointer)))
    (%let ((ch1 %signed-word-integer (%peek-unit stream)))
          (if (%eq ch1 $char-ascii-d-l)
              (progn
                (%read-unit stream)
                (push-buffer $char-ascii-e-l string-stack)
                (read-float-1 stream length idx string-stack ch))
            (if (%eq (char-class
                      *char-class-token*
                      (%cast %unsigned-word-integer ch1)) #%b9) ;whitespace
                (progn
                  (%read-unit stream)
                  (read-float-1 stream #%i1 #%i1 string-stack ch))
              (progn
                (%read-unit stream)
                (push-buffer ch1 string-stack)
                (read-float-1 stream length idx string-stack ch)))))))

;;;-----------------------------------------------------------------------------
;;; type schemes for type inference
;;;-----------------------------------------------------------------------------

(%annotate-function
  format new-signature
  (((var0 var1 var2 var3)
    ((var var0) (atom? <list>))
    ((var var1) (atom? <object>))
    ((var var2) (atom? <string>))
    ((var var3) (atom? <list>)))))

(%annotate-function
  scan new-signature
  (((var0 var1 var2)
    ((var var0) (atom? <list>))
    ((var var1) (atom? <string>))
    ((var var2) (atom? <list>)))))

;;;-----------------------------------------------------------------------------
) ;; end of module
;;;-----------------------------------------------------------------------------
