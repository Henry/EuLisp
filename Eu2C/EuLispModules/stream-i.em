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
;;;  Authors: Horst Friedrich, Rainer Rosenmuller
;;;-----------------------------------------------------------------------------

(defmodule stream-i
  (import ((only (<class>
                  <object>
                  <symbol>
                  <null>
                  <int>
                  %cast
                  %signed-word-integer
                  %unsigned-word-integer
                  %eq
                  %lt
                  %gt
                  %member
                  %string
                  %plus
                  %minus
                  eq
                  cons
                  make-fpint
                  make-swi
                  %void
                  %extract)
                 tail)
           stream-ii
           standard-generic-function
           stream-generic
           (only (<function>)
                 function-i)
           (only (error
                  <condition>)
                 condition-i)
           (only (check-options
                  get-option)
                 option-lists)
           (only (<string>
                  string-pointer
                  make-string
                  duplicate-%string
                  allocate-%string)
                 string-ii)
           (only (<character>)
                 character)
           (rename ((FILE* <file>))
                   (only (FILE*)
                         c-stdio))
           (only (c-stdin
                  c-stdout
                  c-stderr
                  EOF
                  fgetc
                  fflush
                  ungetc
                  putc
                  fprintf-3
                  fscanf-3
                  sprintf-3
                  sscanf-3
                  open-fd
                  close-fd
                  ftell
                  fseek)
                 c-stdio)
           (only (getc-buffer
                  ungetc-buffer
                  clear-buffer
                  <string-stack>
                  push-buffer
                  ?stack-string
                  !cur-index
                  make-string-stack
                  pop-buffer
                  ?cur-index
                  ?last-index)
                 string-stack)
           (only ($char-eof
                  $char-newline
                  $char-ascii-space)
                 char-tables)
           (only (strlen)
                 c-string-interface)
           convert)
   syntax (tail
           (only (when
                  unless)
                 syntax-0))
   export (<stream>
           <file-stream>
           <char-file-stream>
           <string-stream>
           c-stdin
           c-stdout
           c-stderr
           %peek-unit
           %read-unit
           %write-unit
           fprintf-3
           sprintf-3
           fscanf-3
           sscanf-3
           EOF
           stream?
           file-stream?
           opened-streams
           open
           stdin
           stdout
           stderr
           fflush
           flush
           close
           ensure-open-character-input-stream
           %unread-unit stream-position
           set-stream-position
           end-of-stream?
           ensure-open-character-output-stream
           file-descriptor-pointer
           make-file-stream
           input-stream?
           output-stream?
           character-stream?
           open?
           stream-direction
           stream-opened
           ensure-open-input-stream
           ensure-open-stream
           convert-stream-string
           string-stream?
           stream-string-stack
           make-string-stream
           <stream-condition>
           <end-of-stream>
           <inappropriate-stream-position>
           make-string-input-stream
           make-string-output-stream
           converter
           convert
           stream-eos-action
           setf-stream-eos-action
           <file>
           %write-string
           line-position
           left-margin
           set-left-margin
           make-pp-line-position-stream
           pplp-line-position
           setf-pplp-line-position
           *not-eof-action*))

;;;-----------------------------------------------------------------------------
;;; Stream condition
;;;-----------------------------------------------------------------------------
(%define-standard-class (<stream-condition> <class> )
  <condition>
  ((stream type <object> default () accessor stream
           keyword stream))
  representation pointer-to-struct
  allocation multiple-type-card)

(%define-standard-class (<end-of-stream> <class>)
  <stream-condition>
  ()
  representation pointer-to-struct
  allocation multiple-type-card)

(%define-standard-class (<inappropriate-stream-position> <class>)
  <stream-condition>
  ((invalid-stream-position type <object>
                            default () accessor invalid-stream-position
                            keyword invalid-stream-position))
  representation pointer-to-struct
  allocation multiple-type-card)

(%define-standard-class (<file-stream> <class>)
  <stream>
  ((file-descriptor-pointer type <file>
                            reader file-descriptor-pointer
                            writer setf-file-descriptor-pointer
                            keyword file-descriptor-pointer)
   ;;                         (eos-action type <function>
   ;;                                     reader stream-eos-action
   ;;                                     writer setf-stream-eos-action
   ;;                                     default default-eos-action)
   )
  constructor (make-file-stream file-descriptor-pointer direction)
  predicate file-stream?
  allocation single-card
  representation pointer-to-struct)

(%define-standard-class (<char-file-stream> <class>)
  <file-stream>
  ((transaction-unit type <object>
                     default <character>))
  constructor (make-char-file-stream file-descriptor-pointer direction)
  predicate char-file-stream?
  allocation single-card
  representation pointer-to-struct)

(%define-standard-class (<string-stream> <class>)
  <stream>
  ((string-stack type <string-stack>
                 reader stream-string-stack
                 writer setf-stream-string-stack
                 keyword string-stack))
  constructor (make-string-stream string-stack direction)
  predicate string-stream?
  allocation single-card
  representation pointer-to-struct)

(defun make-string-input-stream (string)
  (let (( stream (make-string-stream
                  (make-string-stack
                   (string-pointer string)
                   (strlen (string-pointer string)))
                  'input)))
    (setf-stream-opened stream 'open)
    (setf-stream-transaction-unit stream <character>)
    (setf-stream-eos-action stream
                            (%cast <function> default-eos-action))
    stream))


(defun make-string-output-stream ()
  (let ((stream (make-string-stream
                 (make-string-stack (allocate-%string #%i128) #%i128)
                 'output)))
    (setf-stream-opened stream 'open)
    (setf-stream-transaction-unit stream <character>)
    stream))

(%define-standard-class (<pp-line-position-stream> <class>)
  <stream>
  ((pp-string-stack type <string-stack>
                    reader pplp-stream-string-stack
                    writer setf-pplp-stream-string-stack
                    default (make-string-stack
                              (allocate-%string #%i512) #%i512))
   (line-position type %signed-word-integer
                  reader pplp-line-position
                  writer setf-pplp-line-position
                  default #%i0)
   (left-margin   type %signed-word-integer
                  reader pplp-left-margin
                  writer setf-pplp-left-margin
                  default #%i0)
   (bcur-position type %signed-word-integer
                  reader pplp-bcur-position
                  writer setf-pplp-bcur-position
                  default #%i0)
   (blength       type %signed-word-integer
                  reader pplp-blength
                  writer setf-pplp-blength
                  default #%i512)
   (out-stream type <stream>
               reader pplp-out-stream
               keyword pplp-out-stream))
  constructor (make-pp-line-position-stream0 pplp-out-stream)
  predicate pp-line-position-stream?
  allocation single-card
  representation pointer-to-struct)

(%define-function (make-pp-line-position-stream <pp-line-position-stream>)
  ((stream <stream>))
  (let ((nstr (make-pp-line-position-stream0 stream)))
    (setf-stream-direction nstr (stream-direction stream))
    (setf-stream-transaction-unit nstr (stream-transaction-unit stream))
    (setf-stream-opened nstr (stream-opened stream))
    nstr))

(defmethod close ((stream <pp-line-position-stream>))
  (setf-stream-opened stream 'closed)
  (close (pplp-out-stream stream))
  ())

(defmethod flush ((stream <pp-line-position-stream>))
  (%let ((buffer <string-stack> (pplp-stream-string-stack stream)))
        (push-buffer #%i0 buffer)
        (%write-string (pplp-out-stream stream)
                       (?stack-string buffer))
        (clear-buffer buffer)
        (setf-pplp-bcur-position stream #%i0)
        (flush (pplp-out-stream stream))))

(defmethod (converter <string>) ((stream <pp-line-position-stream>))
  (convert-pplp-stream-string stream))

(defun convert-pplp-stream-string (stream)
  ;; (flush stream)
  (%let ((buffer <string-stack> (pplp-stream-string-stack stream)))
        (push-buffer #%i0 buffer)
        (%write-string (pplp-out-stream stream)
                       (?stack-string buffer))
        (clear-buffer buffer)
        (setf-pplp-bcur-position stream #%i0))
  ((converter <string>) (pplp-out-stream stream)))

(%define-function (%write-unit-special %signed-word-integer)
  ((stream <stream>)
   (ch %signed-word-integer))
  (%let ((lpos %signed-word-integer (pplp-line-position stream))
         (bcpos %signed-word-integer (pplp-bcur-position stream))
         (blength %signed-word-integer (pplp-blength stream))
         (fd <string-stack> (pplp-stream-string-stack stream)))
        (if (%eq bcpos blength)
            ;; write-buffer
            (progn

              ;;(%write-string stdout (%literal %string () "<<<-"))
              ;;(%write-string stdout (?stack-string fd))
              ;;(%write-string stdout (%literal %string () "->>>"))

              (%write-string (pplp-out-stream stream) (?stack-string fd))
              (clear-buffer fd)
              (setf-pplp-bcur-position stream #%i0))
          ())
        (push-buffer ch fd)
        (setf-pplp-line-position stream (%plus lpos #%i1))
        (setf-pplp-bcur-position stream (%plus bcpos #%i1))
        (if (%eq ch $char-newline)
            (progn
              (setf-pplp-line-position stream #%i0)
              (%write-unit-n-times stream $char-ascii-space
                                   (pplp-left-margin stream))
              ;;(%plus #%i1 (pplp-left-margin stream)))
              ;;? plus 1 ist immer 1 zeichen zu viel
              ;;? ohne   ist immer 1 zeichen zu wenig
              )
          ())
        ch))

(%define-function (%write-unit-n-times %void)
  ((stream <stream>)
   (ch %signed-word-integer)
   (n %signed-word-integer))
  (if (%gt n #%i0)
      (progn
        (%write-unit stream ch)
        (%write-unit-n-times stream ch (%minus n #%i1)))
    ()))


(%define-function (line-position %signed-word-integer)
  ((stream <stream>))
  (if (pp-line-position-stream? stream)
      (pplp-line-position stream)
    #%i0))

(%define-function (left-margin %signed-word-integer)
  ((stream <stream>))
  (if (pp-line-position-stream? stream)
      (pplp-left-margin stream)
    #%i0))

(%define-function (set-left-margin %signed-word-integer)
  ((stream <stream>)
   (lm %signed-word-integer))
  (if (pp-line-position-stream? stream)
      (setf-pplp-left-margin stream lm)
    #%i0))


(%define-function (%write-string %signed-word-integer)
  ((stream <stream>)
   (str %string))
  (if (file-stream? stream)
      (%let ((fd <file> ;;%unsigned-word-integer
                 ;;(%cast %unsigned-word-integer
                 (file-descriptor-pointer stream))) ;;)
            (fprintf-3 fd (%literal %string () "%s")
                       (%cast %signed-word-integer str))
            )
    (if (string-stream? stream)
        (%let ((fd <string-stack> (stream-string-stack stream))
               (length  %signed-word-integer (strlen str)))
              (push-string str #%i0 length fd)
              length)
      (if (pp-line-position-stream? stream)
          (%write-string-special stream str #%i0 (strlen str))
        #%i0))))

(%define-function (%write-string-special %signed-word-integer)
  ((stream <stream>)
   (string %string)
   (idx %signed-word-integer)
   (length %signed-word-integer))
  (if (%lt idx length)
      (progn
        (%write-unit-special stream
                             (%cast %signed-word-integer
                                    (%extract string idx)))
        (%write-string-special stream string (%plus idx #%i1) length))
    length))

(%define-function (push-string %void)
  ((string %string)
   (idx %signed-word-integer)
   (length %signed-word-integer)
   (string-stack <string-stack>))
  (if (%lt idx length)
      (progn (push-buffer
              (%cast %signed-word-integer
                     (%extract string idx)) string-stack)
             (push-string string (%plus idx #%i1) length string-stack))
    ()))

(deflocal stdin
  (let (( stream (make-char-file-stream (%cast <file> c-stdin)
                                        'input)))
    (setf-stream-opened stream 'open)
    (setf-stream-transaction-unit stream <character>)
    (setf-stream-eos-action stream
                            (%cast <function> default-eos-action))
    stream))

(deflocal stdout
  (let ((stream (make-file-stream (%cast <file> c-stdout)
                                  'output)))
    (setf-stream-opened stream 'open)
    (setf-stream-transaction-unit stream <character>)
    stream))

(deflocal stderr
  (let ((stream (make-file-stream (%cast <file> c-stderr)
                                  'output)))
    (setf-stream-opened stream 'open)
    (setf-stream-transaction-unit stream <character>)
    stream) )

(%define-function (%peek-unit %signed-word-integer)
  ((stream <stream>))
  (if (file-stream? stream)
      (%let ((fd <file> ;;%unsigned-word-integer
                 ;;(%cast %unsigned-word-integer
                 (file-descriptor-pointer stream))) ;;)
            (%let* ((ch %signed-word-integer (fgetc fd)))
                   (if (%eq ch #%i-1)
                       (make-swi
                        (%cast <int>
                               ((stream-eos-action stream) stream)))
                     ; EOS (%cast %signed-word-integer $char-eof)
                     (progn
                       (ungetc ch fd)
                       ch))
                   ))
    (if (string-stream? stream)
        (%let ((fd <string-stack> (stream-string-stack stream)))
              (%let* ((ch %signed-word-integer (getc-buffer fd)))
                     (if (%eq ch #%i-1)
                         (make-swi
                          (%cast <int>
                                 ((stream-eos-action stream) stream)))
                       ; EOS (%cast %signed-word-integer $char-eof)
                       (progn
                         (ungetc-buffer ch fd)
                         ch))))
      #%i0)))

(%define-function (%read-unit %signed-word-integer)
  ((stream <stream>))
  (if (file-stream? stream)
      (%let ((fd <file> ;;%unsigned-word-integer
                 ;;(%cast %unsigned-word-integer
                 (file-descriptor-pointer stream))) ;;)
            (%let* ((ch %signed-word-integer (fgetc fd)))
                   (if (%eq ch #%i-1)
                       (make-swi
                        (%cast <int>
                               ((stream-eos-action stream) stream)))
                     ; EOS (%cast %signed-word-integer $char-eof)
                     ch))
            )
    (if (string-stream? stream)
        (%let ((fd <string-stack> (stream-string-stack stream)))
              (%let* ((ch %signed-word-integer (getc-buffer fd)))
                     (if (%eq ch #%i-1)
                         (make-swi
                          (%cast <int>
                                 ((stream-eos-action stream) stream)))
                       ; EOS (%cast %signed-word-integer $char-eof)
                       (progn
                         ch))))
      #%i0))
  )

(%define-function (%unread-unit %signed-word-integer)
  ((stream <stream>)
   (ch %signed-word-integer))
  (if (file-stream? stream)
      (%let ((fd <file> ;;%unsigned-word-integer
                 ;;(%cast %unsigned-word-integer
                 (file-descriptor-pointer stream))) ;;)
            (ungetc ch fd))
    ;;     ((.. EOF))
    (if (string-stream? stream)
        (%let ((fd <string-stack> (stream-string-stack stream)))
              (ungetc-buffer ch fd)
              #%i0)
      #%i0)))


(%define-function (%write-unit %signed-word-integer)
  ((stream <stream>)
   (ch %signed-word-integer))
  (if (file-stream? stream)
      (%let ((fd <file> ;;%unsigned-word-integer
                 ;; (%cast %unsigned-word-integer
                 (file-descriptor-pointer stream))) ;;)
            (putc ch fd))
    (if (string-stream? stream)
        (%let ((fd <string-stack> (stream-string-stack stream)))
              (push-buffer ch fd)
              #%i0)
      (if (pp-line-position-stream? stream)
          (%write-unit-special stream ch)
        #%i0))))


(deflocal opened-streams ())

(defun open (handle . inilist)
  (check-options  () '(input output update append eos-action) () inilist)
  (let ((lvdir (if (%member 'input inilist) 'input
                 (if (%member 'output inilist) 'output
                   'input)))
        (lvtrans-ini <character>)
        (lvpos-ini ())
        (lvupdate (%member 'update inilist))
        (lvappend (%member 'append inilist))
        (lveos (%cast <function> (get-option 'eos-action inilist
                                             default-eos-action))))
    (let ((stream
           (make-char-file-stream
            (%cast <file>
                   (open-fd (string-pointer handle)
                            (if (eq lvdir 'input)
                                (%literal %string () "r")
                              (if (eq 'output lvdir)
                                  (if lvupdate
                                      (%literal %string () "w+")
                                    (if lvappend
                                        (%literal %string () "a")
                                      (%literal %string () "w")))
                                (%literal %string () "r")))))
            lvdir)))
      (if (%eq #%i0 (%cast %signed-word-integer
                           (file-descriptor-pointer stream)))
          (setf-stream-opened stream ())
        (setf-stream-opened stream 'open))
      ;;(setf-stream-direction stream lvdir)
      (setf-stream-transaction-unit stream lvtrans-ini)
      (setf-stream-positionable stream lvpos-ini)
      (setf-stream-eos-action stream (%cast <function> lveos))
      (setq opened-streams (cons stream opened-streams))
      stream)))

(deflocal *not-eof-action* ())

(defun default-eos-action (stream)
  (if *not-eof-action*
      (%cast <object> (make-fpint $char-eof))
    (progn
      (error <end-of-stream>
             "end of stream" 'stream stream)
      ())))


(defun input-stream? (obj)
  (if (stream? obj)
      (if ;(inputstream? (stream-direction obj))
          (eq 'input (stream-direction obj))
          obj
        ())
    ()))

(defun output-stream? (obj)
  (if (stream? obj)
      (if ;(outputstream? (stream-direction obj))
          (eq 'output (stream-direction obj))
          obj
        ())
    ()))

(defgeneric character-stream? ((object <object>)))
(defmethod character-stream? ((object <object>)) ())
(defmethod character-stream? ((object <stream>))
  (if (eq <character> (stream-transaction-unit object))
      object
    ()))

(defmethod close ((stream <stream>))
  (setf-stream-opened stream 'closed)
  ())

(defmethod close ((stream <file-stream>))
  (setf-stream-opened stream 'closed)
  (close-fd ;;(%cast %unsigned-word-integer
   (file-descriptor-pointer stream)) ;;)
  ())

(defun open? (stream)
  (let ((lvstate (stream-opened stream)))
    (if (eq 'open lvstate)
        stream
      ())))

(defun ensure-open-stream (stream)
  (let ((lvstate (stream-opened stream)))
    (if (eq 'open lvstate)
        stream
      ;;(error <stream-condition>
      ;;       "Attempt to use stream ~A wich is ~A."
      ;;       stream lvstate)
      ())))

(defun ensure-open-input-stream (stream)
  (if (ensure-open-stream stream)
      (if (input-stream? stream)
          stream
        (progn
          ;;(error <stream-condition>
          ;;       "Attempt to use stream ~A as input-stream." stream)
          ()))
    ()))

(defun ensure-open-output-stream (stream)
  (if (ensure-open-stream stream)
      (if (output-stream? stream)
          stream
        (progn ;;(error <stream-condition>
          ;;            "Attempt to use stream ~A as input-stream." stream)
          ()))
    ()))

(defun ensure-open-character-input-stream (stream)
  (if (ensure-open-input-stream stream)
      (if (character-stream? stream)
          stream
        (progn ;;(error <stream-condition>
          ;;            "Attempt to use stream ~A as character-input-stream." stream)
          ()))
    ()))

(defun ensure-open-character-output-stream (stream)
  (if (ensure-open-output-stream stream)
      (if (character-stream? stream)
          stream
        (progn ;;(error <stream-condition>
          ;;            "Attempt to use stream ~A as character-input-stream." stream)
          ()))
    ()))

(defmethod flush ((stream <stream>))
  ())

(defmethod flush ((stream <file-stream>))
  (if (ensure-open-output-stream stream)
      (if (%eq #%i0 (%cast %signed-word-integer
                           (fflush ;;(%cast %unsigned-word-integer
                            (file-descriptor-pointer stream)))) ;;)
          't
        ())
    ()))

(defmethod stream-position ((stream <stream>))
  (if (ensure-open-stream stream)
      (if (file-stream? stream)
          (make-fpint (%cast %signed-word-integer
                             (ftell ;;(%cast %unsigned-word-integer
                              (file-descriptor-pointer stream)))) ;;)
        (if (string-stream? stream)
            (%let ((fd <string-stack> (stream-string-stack stream)))
                  (make-fpint (?cur-index fd)))
          ()))
    ()))

(defmethod (setter stream-position) ((stream <stream>) (pos <object>))
  (set-stream-position stream pos))

(defun set-stream-position (stream pos)
  (if (ensure-open-stream stream)
      (if (file-stream? stream)
          (if (eq pos 'stream-end)
              (if (%eq #%i-1 (%cast %signed-word-integer
                                    (fseek ;;(%cast %unsigned-word-integer
                                     (file-descriptor-pointer stream) ;;)
                                     #%i0 #%i2)))
                  (error <stream-condition>
                         "set stream position falled" 'stream stream
                         'invalid-stream-position pos)
                't)
            (if (%eq #%i-1 (%cast %signed-word-integer
                                  (fseek ;;(%cast %unsigned-word-integer
                                   (file-descriptor-pointer stream) ;;)
                                   (make-swi pos)
                                   #%i0)))
                (error <stream-condition>
                       "set stream position falled" 'stream stream
                       'invalid-stream-position pos)
              't))
        (if (string-stream? stream)
            (%let ((fd <string-stack> (stream-string-stack stream)))
                  (if (eq pos 'stream-end)
                      (progn (!cur-index fd (?last-index fd)) 't)
                    (%let ((ipos %signed-word-integer (make-swi pos)))
                          (if (%lt ipos (?last-index fd))
                              (progn (!cur-index fd ipos) 't)
                            (error <stream-condition>
                                   "set stream position falled" 'stream stream
                                   'invalid-stream-position pos)))))
          ()))
    ()))

(defun end-of-stream? (stream)
  (if (file-stream? stream)
      (%let ((lvpos %signed-word-integer
                    (ftell ;;(%cast %unsigned-word-integer
                     (file-descriptor-pointer stream)))) ;;)
            (fseek ;;(%cast %unsigned-word-integer
             (file-descriptor-pointer stream) ;;)
             #%i0
             #%i2)
            (%let ((lvend-pos %signed-word-integer
                              (ftell ;;(%cast %unsigned-word-integer
                               (file-descriptor-pointer stream)))) ;;)
                  (if (%eq lvpos lvend-pos)
                      't
                    (progn (fseek ;;(%cast %unsigned-word-integer
                            (file-descriptor-pointer stream) ;;)
                            lvpos #%i0)
                           ()))))
    (if (string-stream? stream)
        (%let ((fd <string-stack> (stream-string-stack stream)))
              (if (%lt (?cur-index fd ) (?last-index fd))
                  't
                ()))
      ())))

(defmethod (converter <string>) ((stream <stream>))
  (convert-stream-string stream))

(defun convert-stream-string (stream)
  (if (string-stream? stream)
      (progn
        (push-buffer #%i0 (stream-string-stack stream))
        (let ((string
               (make-string
                (duplicate-%string
                 (?stack-string (stream-string-stack stream))))))
          (!cur-index
           (stream-string-stack stream)
           #%i0)
          string))
    ()))

;;(%define-abstract-class (<stream-properties> <abstract-class>)
;;                        <object>
;;       ())
;;
;;(%define-abstract-class (<stream-direction> <abstract-class>)
;;                        <stream-properties>
;;       ())

;;(%define-standard-class (<input-stream> <class>)
;;                       <stream-direction>
;;        ((mode-string type %string default
;;                                   (%literal %string () "r")
;;                        accessor ?mode-string)) ;rr
;;           predicate inputstream?
;;        ;;(dummy type %unsigned-word-integer))
;;        ; constructor
;;        constructor (make-input-stream)
;;        allocation single-card
;;        representation pointer-to-struct)
;;
;;(deflocal input-stream
;;                  (make-input-stream))   ;z.Z fehler in asm
;;              ;;%define-c. nur mit lit.
;;              ;;defconst in lzs2mzs
;;(%define-standard-class (<output-stream> <class>)
;;                       <stream-direction>
;;        ((mode-string type %string default
;;                            (%literal %string () "w")
;;                        accessor ?mode-string))
;;(dummy type %unsigned-word-integer))
;;             predicate outputstream?
;;        constructor (make-output-stream)
;;        allocation single-card
;;        representation pointer-to-struct)
;;
;;(deflocal output-stream
;;                  (make-output-stream))   ;z.Z s.o.
;;
;;(%define-standard-class (<io-stream> <class>)
;;                       <stream-direction>
;;        ((dummy type %unsigned-word-integer))
;;      ((mode-string default (%literal %string () "r+")
;;                            accessor ?mode-string))
;;        ; constructor ???
;;             predicate iostream?
;;             allocation single-card
;;        representation pointer-to-struct)
;;
;;(%define-abstract-class (<stream-unit> <abstract-class>)
;;                       <stream-properties>
;;                       ())
;;
;;(%define-standard-class (<character-stream> <class>)
;;                       <stream-unit>
;;        ((dummy type %unsigned-word-integer))
;;        ; constructor
;;        constructor (make-character-stream)
;;             predicate characterstream?
;;        allocation single-card
;;        representation pointer-to-struct)
;;
;;(%define-standard-class (<binary-stream> <class>)
;;                       <stream-unit>
;;        ((dummy type %unsigned-word-integer))
;;        ; constructor
;;        constructor (make-binary-stream)
;;             predicate binarystream?
;;        allocation single-card
;;        representation pointer-to-struct)

;;(deflocal $character-stream
;;                  (make-character-stream))
;;(defgeneric open ((stream <stream>) (handle <object>) (inilist <list>)))

;;(defmethod open ((stream <file-stream>) (handle <string>) (inilist <list>))..)
;;(defun open-98 (stream handle . inilist)
;;  (check-options  () '(direction transaction-unit positionable) () inilist)
;;  (let ((lvdirection (stream-direction stream))
;;        (lvtransaction-unit ())
;;        (lvpositionable (stream-positionable stream)))
;;    (let ((lvdir-ini (get-option 'direction inilist lvdirection))
;;          (lvtrans-ini (get-option 'transaction-unit inilist lvtransaction-unit))
;;          (lvpos-ini (get-option 'positionable inilist lvpositionable)))
;;      ;          (if (stream-direction? (cadr inilist))
;;      ;            (cerror <stream-condition> "Missmatch in the argument list of open"))
;;      ;            (if (stream-unit? (cadr inilist))
;;      ;              (cerror <stream-condition> "Missmatch in the argument list of open"))
;;      (setf-file-descriptor-pointer stream
;;            (open-fd (string-pointer handle)
;;                     (%cast %string (?mode-string lvdirection))))
;;      (if (%eq #%I0 (file-descriptor-pointer stream))
;;        (setf-stream-opened stream ())
;;        (setf-stream-opened stream 'open))
;;      (setf-stream-direction stream lvdir-ini)
;;      (setf-stream-transaction-unit stream lvtrans-ini)
;;      (setf-stream-positionable stream lvpos-ini)
;;      (setq opened-streams (cons stream opened-streams))))
;;      stream)

;;(defun io-stream? (obj) ; nicht notwendig
;;  (if (stream? obj)
;;    (if (iostream? (stream-direction obj))
;;      obj
;;      ())
;;    ()))

;;(defun binary-stream? (obj)
;;  (if (stream? obj)
;;    (if (binarystream? (stream-transaction-unit obj))
;;      obj
;;      (if (eq <int> (stream-transaction-unit obj))
;;        obj
;;        ()))
;;    ()))

;;;-----------------------------------------------------------------------------
;;; Type schemes for type inference
;;;-----------------------------------------------------------------------------
;; still to annotate:

;;(%annotate-function
;; input-stream? new-signature
;; (((var0 var1)
;;   ((var var0) (atom? (and <object> (not <null>))))
;;   ((var var1) (atom? <input-stream>)))
;;  ((var0 var1)
;;   ((var var0) (atom? <null>))
;;   ((var var1) (atom? (and <object> (not <input-stream>)))))))
;;
;;(%annotate-function
;; output-stream? new-signature
;; (((var0 var1)
;;   ((var var0) (atom? (and <object> (not <null>))))
;;   ((var var1) (atom? <output-stream>)))
;;  ((var0 var1)
;;   ((var var0) (atom? <null>))
;;   ((var var1) (atom? (and <object> (not <output-stream>)))))))

;;(%annotate-function
;; input extend-signature
;; (((var0 var1)
;;   ((var var0) (atom? <character>))
;;   ((var var1) (atom? <character-stream>)))
;;  ((var0 var1)
;;   ((var var0) (atom? <object>))
;;   ((var var1) (atom? (and <stream> (not <character-stream>)))))))

(%annotate-function
  read-char new-signature
  (((var0 var1)
    ((var var0) (atom? <object>))
    ((var var1) (atom? <stream>)))))

;;(%annotate-function
;; putback-char new-signature
;; (((var0 var1 var2)
;;   ((var var0) (atom? <symbol>))
;;   ((var var1) (atom? <character-stream>))
;;   ((var var2) (atom? <character>)))
;;  ((var0 var1 var2)
;;   ((var var0) (atom? <symbol>))
;;   ((var var1) (atom? (and <stream> (not <character-stream>))))
;;   ((var var2) (atom? (and <object> (not <character>)))))))

(%annotate-function
  putback-char new-signature
  (((var0 var1 var2)
    ((var var0) (atom? <symbol>))
    ((var var1) (atom? <stream>))
    ((var var2) (atom? <object>)))))

(%annotate-function
  read-line new-signature
  (((var0 var1)
    ((var var0) (atom? <string>))
    ((var var1) (atom? <stream>)))))

(%annotate-function
  %write-string new-signature
  (((var0 var1 var2)
    ((var var0) (atom? %signed-word-integer))
    ((var var1) (atom? <stream>))
    ((var var2) (atom? %string)))))

(%annotate-function
  push-string new-signature
  (((var0 var1 var2 var3 var4)
    ((var var0) (atom? %void))
    ((var var1) (atom? %string))
    ((var var2) (atom? %signed-word-integer))
    ((var var3) (atom? %signed-word-integer))
    ((var var4) (atom? <string-stack>)))))

(%annotate-function
  ensure-open-stream new-signature
  (((var0 var1)
    ((var var0) (atom? <null>))
    ((var var1) (atom? <stream>)))
   ((var0 var1)
    ((var var0) (atom? <stream>))
    ((var var1) (var var0)))))

(%annotate-function
  input-stream? new-signature
  (((var0 var1)
    ((var var0) (atom? <null>))
    ((var var1) (atom? <object>)))
   ((var0 var1)
    ((var var0) (atom? <stream>))
    ((var var1) (var var0)))))

(%annotate-function
  ensure-open-input-stream new-signature
  (((var0 var1)
    ((var var0) (atom? <null>))
    ((var var1) (atom? <stream>)))
   ((var0 var1)
    ((var var0) (atom? <stream>))
    ((var var1) (var var0)))))

(%annotate-function
  ensure-open-character-input-stream new-signature
  (((var0 var1)
    ((var var0) (atom? <null>))
    ((var var1) (atom? <stream>)))
   ((var0 var1)
    ((var var0) (atom? <stream>))
    ((var var1) (var var0)))))

(%annotate-function
  convert-stream-string new-signature
  (((var0 var1)
    ((var var0) (atom? <null>))
    ((var var1) (atom? <object>)))
   ((var0 var1)
    ((var var0) (atom? <string>))
    ((var var1) (atom? <string-stream>)))))

(%annotate-function
  default-eos-action new-signature
  (((var0 var1)
    ((var var0) (atom? <null>))
    ((var var1) (atom? <object>)))))

;;;-----------------------------------------------------------------------------
)  ;; End of module stream-i
;;;-----------------------------------------------------------------------------
