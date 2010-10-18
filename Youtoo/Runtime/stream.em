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
;;; Title: streams
;;;  Library: level1
;;;  Authors: Julian Padget, Andreas Kind
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule stream
  (syntax (_macros)
   import (telos
           stream1
           stream2
           socket
           dynamic
           condition
           lock
           convert
           collect
           list
           string
           fpi)
   expose (stream1
           stream2
           socket)
   export (connect
           sflush
           flush
           sprintf
           fprintf
           swrite
           write
           sprint
           print
           sprint-one-char
           sprint-char
           print-char
           nl
           *int-size-in-decimal-digits*
           *double-size-in-decimal-digits*
           output-list-contents
           print-string
           print-address
           make-space
           file-lookup))

;;;------------------------------------------------------------------------
;;; Stream Manipulation Functions
;;;------------------------------------------------------------------------
(defun connect (source sink . options)
  (generic-connect source sink options))

(defmethod generic-connect ((stream1 <stream>) (stream2 <stream>) options)
  (let ((mode (stream-mode stream1)))
    (cond
      ((eq mode 'r)
       ((setter stream-source) stream1 stream2))
      ((eq mode 'w)
       ((setter stream-sink) stream1 stream2))
      ((eq mode 'a)
       ((setter stream-sink) stream1 stream2))
      (t
       ((setter stream-source) stream1 stream2)
       ((setter stream-sink) stream1 stream2))))
  ())

(defmethod reconnect ((stream1 <stream>) (stream2 <stream>))
  ((setter stream-source) stream2 (stream-source stream1))
  ((setter stream-source) stream1 ())
  ((setter stream-sink) stream2 (stream-sink stream1))
  ((setter stream-sink) stream1 ())
  ())

(defmethod disconnect ((stream <stream>))
  ((setter stream-source) stream ())
  ((setter stream-sink) stream ())
  ())

(defmethod disconnect ((fstream <file-stream>))
  (let ((sink (stream-sink fstream))
        (source stream-source fstream))
    (if (file-control-block? sink)
        (progn
          (flush-buffer fstream)
          (if (int-binary= (eul_close (control-block-descriptor sink)) 0) ()
            (error <stream-condition> (strerror) value: fstream)))
      ())
    (if (file-control-block? source)
        (if (int-binary= (eul_close (control-block-descriptor source)) 0) ()
          (error <stream-condition> (strerror) value: fstream))
      ())
    (with-lock *open-file-streams*-lock
               (setq *open-file-streams*
                     (list-remove fstream *open-file-streams*)))
    (call-next-method)))

(defmethod disconnect ((x <socket>))
  (if (int-binary= (eul_close (socket-descriptor x)) 0)
      (with-lock *open-file-streams*-lock
                 (setq *open-file-streams*
                       (list-remove x *open-file-streams*)))
    (error <stream-condition> (strerror) value: x)))

;;;------------------------------------------------------------------------
;;; Buffer Management
;;;------------------------------------------------------------------------
(defmethod fill-buffer ((stream <stream>))
  (let* ((source (stream-source stream))
         (sink (stream-sink stream))
         (sink-buf (control-block-buffer sink)))
    (if (null? sink-buf)
        0
      (progn
        ((setter control-block-buffer) source (reverse-list sink-buf))
        ((setter control-block-buffer) sink ())
        ;; return non-zero
        1))))

(defmethod fill-buffer ((fstream <file-stream>))
  (let* ((fcb (stream-source fstream))
         (n (control-block-buffer-size fcb))
         (i (read-into-buffer (control-block-descriptor fcb)
                              (control-block-buffer fcb)
                              n)))
    ((setter control-block-buffer-cnt) fcb i)
    ((setter control-block-buffer-pos) fcb
     (int-binary- (int-binary/ n 2) 1))
    i))

(defmethod fill-buffer ((sstream <string-stream>))
  (let ((str-list (string-stream-string-list sstream)))
    (if (null? str-list)
        0
      (let ((scb (stream-source sstream)))
        ((setter string-stream-string-list) sstream (cdr str-list))
        ((setter control-block-buffer) scb (car str-list))
        ((setter control-block-buffer-cnt) scb 0)
        ;; return non-zero
        1))))

(defmethod flush-buffer ((stream <stream>))
  (let* ((source (stream-source stream))
         (sink (stream-sink stream))
         (source-buf (control-block-buffer source))
         (sink-buf (control-block-buffer sink)))
    ((setter control-block-buffer) source
     (append source-buf (reverse sink-buf)))
    ((setter control-block-buffer) sink ())
    t))

(defmethod flush-buffer ((fstream <file-stream>))
  ;; Write buffer to sink and reset
  (let* ((fcb (stream-sink fstream))
         (i (eul_write (control-block-descriptor fcb)
                       (control-block-buffer fcb)
                       (control-block-buffer-pos fcb))))
    (if (int-binary= i -1)
        ;; indicates failure
        ()
      (progn
        ((setter control-block-buffer-pos) fcb 0)
        fstream))))

(defmethod flush-buffer ((sstream <string-stream>))
  ;; Append buffer to the list of strings and reset
  (let* ((scb (stream-sink sstream))
         (pos (control-block-buffer-pos scb))
         (str (substring (control-block-buffer scb) 0 pos)))
    ((setter string-stream-string-list) sstream
     (append (string-stream-string-list sstream) (list str)))
    ((setter control-block-buffer-pos) scb 0)
    ((setter control-block-buffer) scb
     (make <string> size: (control-block-buffer-size scb))))
  sstream)

(defmethod end-of-stream ((stream <stream>))
  (error <end-of-stream> "end of stream" value: stream))

(defmethod end-of-stream ((fstream <file-stream>))
  (disconnect fstream)
  (call-next-method))

;;;------------------------------------------------------------------------
;;; Read Operations
;;;------------------------------------------------------------------------
(defmethod generic-read ((stream <stream>) eos-error? eos-value)
  ((stream-read-action stream) stream eos-error? eos-value))

(defmethod generic-read ((fstream <file-stream>) eos-error? eos-value)
  (let ((fcb (stream-source fstream)))
    (if (and (int-binary= (control-block-buffer-cnt fcb) 0)
             (int-binary= (fill-buffer fstream) 0))
        (if eos-error? (end-of-stream fstream) eos-value)
      (let* ((cnt (control-block-buffer-cnt fcb))
             (pos (control-block-buffer-pos fcb))
             (new-pos (int-binary+ pos 1)))
        ((setter control-block-buffer-cnt) fcb (int-binary- cnt 1))
        ((setter control-block-buffer-pos) fcb new-pos)
        (string-ref (control-block-buffer fcb) new-pos)))))

(defmethod generic-read ((sstream <string-stream>) eos-error? eos-value)
  (let ((scb (stream-source sstream)))
    (if (and (int-binary= (control-block-buffer-cnt scb)
                          (string-size (control-block-buffer scb)))
             (int-binary= (fill-buffer sstream) 0))
        (if eos-error? (end-of-stream sstream) eos-value)
      (let* ((cnt (control-block-buffer-cnt scb))
             (c (string-ref (control-block-buffer scb) cnt)))
        ((setter control-block-buffer-cnt) scb (int-binary+ cnt 1))
        c))))

;;;------------------------------------------------------------------------
;;; Write and print
;;;------------------------------------------------------------------------
(defun swrite-all (stream args)
  (if args
      (do1-list
       (lambda (x)
         (if (object? x)
             (generic-write x stream)
           (progn
             (print-string "#<C: " 5 stream)
             (print-address x stream)
             (sprint-one-char stream #\>))))
       args)
    ())
  stream)

(defun swrite (stream . args)
  (swrite-all stream args))

(defun write args
  (swrite-all stdout args))

(defun sprint-all (stream args)
  (if args
      (do1-list
       (lambda (x)
         (if (object? x)
             (generic-print x stream)
           (progn
             (print-string "#<C: " 5 stream)
             (print-address x stream)
             (sprint-one-char stream #\>))))
       args)
    ())
  stream)

(defun sprint (stream . args)
  (sprint-all stream args))

(defun print args
  (sprint-all stdout args))

(defun sflush (stream)
  (flush-buffer stream))

(defun flush ()
  (flush-buffer stdout))

;;***HGW this should be just a character
;; but at the moment Youtoo does not write characters correctly
;; so the .i file would be incorrect
;;(defconstant nl '#\\n)
(defconstant nl #\\n)

;;;------------------------------------------------------------------------
;;; Some low level functions to get things printed on <file-stream>s
;;;------------------------------------------------------------------------
(defun sprint-char (stream char . times)
  (if times
      (sprint-char* stream char (car times))
    (sprint-one-char stream char)))

(defun print-char (char . times)
  (if times
      (sprint-char* stdout char (car times))
    (sprint-one-char stdout char)))

(defun sprint-one-char (stream char)
  (let* ((scb (stream-sink stream))
         (pos (control-block-buffer-pos scb))
         (new-pos (int-binary+ pos 1))
         (bufsiz (control-block-buffer-size scb)))
    ((setter string-ref) (control-block-buffer scb) pos char)
    ((setter control-block-buffer-pos) scb new-pos)
    (if (or (eql char #\\n)
            (null? (int-binary< new-pos bufsiz)))
        (flush-buffer stream)
      ())))

(defun sprint-char* (stream char times)
  (let* ((scb (stream-sink stream))
         (bufsiz (control-block-buffer-size scb)))
    (labels
     ((loop (i)
            (if (int-binary< i times)
                (let* ((pos (control-block-buffer-pos scb))
                       (new-pos (int-binary+ pos 1)))
                  ((setter string-ref) (control-block-buffer scb) pos char)
                  ((setter control-block-buffer-pos) scb new-pos)
                  (if (or (eql char #\\n)
                          (null? (int-binary< new-pos bufsiz)))
                      (flush-buffer stream)
                    ())
                  (loop (int-binary+ i 1)))
              ())))
     (loop 0))))

(defun make-space (stream n)
  ;; There must be at least one byte more available than requested
  (let* ((scb (stream-sink stream))
         (bufsiz (control-block-buffer-size scb))
         (pos (control-block-buffer-pos scb))
         (left (int-binary- bufsiz pos)))
    (if (int-binary< n left) t
      (progn
        (flush-buffer stream)
        ;; Buffer size greater than n?
        (int-binary< n bufsiz)))))

;; Will be 16 for 64 bit addresses
;(defconstant *address-size-in-hex-digits* 8)

;; 2^64-1 = 18446744073709551615 (20 digits) + 1 for sign
(defconstant *int-size-in-decimal-digits* 21)

;; No idea: write a program to print HUGE_VAL?
(defconstant *double-size-in-decimal-digits* 25)

(defun sprintf (c-format-str x)
  (let* ((buf (make <string> size: *double-size-in-decimal-digits*))
         (n (eul_sprintf buf 0 c-format-str x)))
    (substring buf 0 n)))

(defun fprintf (s c-format-str x)
  (let* ((buf (make <string> size: *double-size-in-decimal-digits*))
         (n (eul_sprintf buf 0 c-format-str x))
         (str (substring buf 0 n)))
    (print-string str n s)))

(defun print-address (x s) (fprintf s "0x%08X" x))

(defun print-string (str n s)
  (let* ((scb (stream-sink s))
         (bufsiz (control-block-buffer-size scb))
         (max (int-binary- bufsiz 1)))
    (labels
     ((loop (i)
            (let ((m (int-binary- n i)))
              ;; m chars still need to be printed
              (if (int-binary< 0 m)
                  (let ((flag (make-space s m))
                        (pos (control-block-buffer-pos scb))
                        (buf (control-block-buffer scb)))
                    (if flag
                        (progn
                          ;; str fits into buffer
                          (eul_sprintf_string buf pos m i "%s" str)
                          ((setter control-block-buffer-pos) scb
                           (int-binary+ pos m)))
                      (progn
                        ;; str does not fit into buffer
                        (eul_sprintf_string buf pos max i "%s" str)
                        ((setter control-block-buffer-pos) scb
                         (int-binary+ pos max))
                        (loop (int-binary+ i max)))))
                ()))))
     (loop 0)
     (if (eql (string-ref str (int-binary- n 1)) #\\n)
         (flush-buffer s)
       ())
     str)))

(defun output-list-contents (l fun s)
  (labels
   ((loop (ll)
          (fun (car ll) s)
          (cond ((cons? (cdr ll))
                 (sprint-one-char s #\ )
                 (loop (cdr ll)))
                (t
                 (cdr ll)))))
   (loop l)))

;;;-----------------------------------------------------------------------------
;;; File lookup with path
;;;-----------------------------------------------------------------------------
(defextern eul-file-lookup (<string> ptr) ptr "eul_file_lookup")
(defun file-lookup (name . dirs)
  (eul-file-lookup (convert name <string>) dirs))

;;;-----------------------------------------------------------------------------
)  ;; End of module stream
;;;-----------------------------------------------------------------------------
