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
;;;  Library: level1
;;;  Authors: Julian Padget, Andreas Kind
;;; Description: streams
;;;-----------------------------------------------------------------------------
(defmodule stream3
  (syntax (_telos0)
   import (telos integer collect list character string vector
                 float stream1 stream format))

;;;-----------------------------------------------------------------------------
;;; Default generic write/prin method
;;;-----------------------------------------------------------------------------
(defmethod generic-write (x (s <stream>))
  (if (stream-write-action s)
      ((stream-write-action s) x s)
    ()))

(defmethod generic-print (x (s <stream>))
  ;; Default generic-print == generic-write
  (generic-write x s))

;;;------------------------------------------------------------------------
;;; Write to buffered-stream (e.g. file-stream, string-stream)

;;;------------------------------------------------------------------------
(defmethod generic-write (x (s <buffered-stream>))
  (prin-string "#<" 2 s)
  (let ((name (symbol-name (class-name (class-of x)))))
    (prin-string name (string-size name) s))
  (prin-string ": " 2 s)
  (prin-address x s)
  (sprin-one-char s #\>)
  x)

(defmethod generic-write ((x <null>) (s <buffered-stream>))
  (prin-string "()" 2 s)
  ())

(defmethod generic-write ((x <symbol>) (s <buffered-stream>))
  (let ((str (symbol-name x)))
    ;; should also check for start comment!
    (if (all? graph? str)
        (generic-print str s)
      (progn
        (sprin-one-char s #\|)
        (generic-print str s)
        (sprin-one-char s #\|))))
  x)

(defmethod generic-write ((x <keyword>) (s <buffered-stream>))
  (let ((name (symbol-name x)))
    (prin-string name (string-size name) s))
  (sprin-one-char s #\:)
  x)

(defmethod generic-write ((x <integer>) (s <buffered-stream>))
  (fprintf s "%i" x) s)

(defmethod generic-write ((x <double>) (s <buffered-stream>))
  (fprintf s "%f" x) s)

;  (defmethod generic-write ((x <integer>) (s <buffered-stream>))
;    (make-space s *int-size-in-decimal-digits*)
;    (let* ((scb (stream-sink s))
;          (pos (control-block-buffer-pos scb))
;          (n (eul_sprintf_int (control-block-buffer scb) pos "%i" x)))
;      ((setter control-block-buffer-pos) scb (int-binary+ pos n)))
;    x)
;  (defmethod generic-write ((x <double>) (s <buffered-stream>))
;    (make-space s *double-size-in-decimal-digits*)
;    (let* ((scb (stream-sink s))
;          (pos (control-block-buffer-pos scb))
;          (n (eul_sprintf_double (control-block-buffer scb) pos "%f" x)))
;      ((setter control-block-buffer-pos) scb (int-binary+ pos n)))
;    x)

(defmethod generic-write ((x <function>) (s <buffered-stream>))
  (sformat s "#<~a: ~a>" (class-name (class-of x)) (function-name x)))

(defmethod generic-write ((vec <vector>) (s <buffered-stream>))
  (let ((n (vector-size vec)))
    (cond ((int-binary= n 0)
           (prin-string "#()" 3 s))
          ((int-binary= n 1)
           (prin-string "#(" 2 s)
           (swrite s (vector-ref vec 0))
           (sprin-one-char s #\) ))
          (t
           (let ((m (int-binary- n 1))
                 (i 0))
             (prin-string "#(" 2 s)
             (labels
              ((loop ()
                     (and (int-binary< i m)
                          (progn
                            (swrite s (vector-ref vec i))
                            (sprin-one-char s #\  )
                            (setq i (int-binary+ i 1))
                            (loop)))))
              (loop)
              (swrite s (vector-ref vec m))
              (sprin-one-char s #\) ))))))
  vec)

(defmethod generic-write ((l <cons>) (s <buffered-stream>))
  (sprin-one-char s #\( )
  (let ((tail (output-list-contents l generic-write s)))
    (if (null? tail) ()
      (progn
        (prin-string " . " 3 s)
        (swrite s tail))))
  (sprin-one-char s #\) )
  l)

(defmethod generic-write ((c <character>) (s <buffered-stream>))
  ;; Certain characters should be interpreted
  (sprin-one-char s #\# )
  (sprin-one-char s #\\\ )
  (sprin-one-char s c)
  c)

(defmethod generic-write ((x <string>) (s <buffered-stream>))
  ;; Certain characters should be interpreted
  (sprin-one-char s #\")
  (prin-string x (string-size x) s)
  (sprin-one-char s #\")
  x)

(defmethod generic-write ((x <file-stream>) (s <buffered-stream>))
  (let ((sink (stream-sink x))
        (source (stream-source x))
        (mode (stream-mode x)))
    (sformat s "#<~a: ~a>"
             (class-name (class-of x))
             (if (eq mode 'r)
                 (if (file-control-block? source)
                     (control-block-file-name source)
                   "*unconnected*")
               (if (file-control-block? sink)
                   (control-block-file-name sink)
                 "*unconnected*")))
    x))

(defmethod generic-write ((x <connection>) (s <buffered-stream>))
  (sformat s "#<~a: ~a:~a>"
           (class-name (class-of x))
           (connection-host x)
           (connection-port x))
  x)

(defmethod generic-write ((x <socket>) (s <buffered-stream>))
  (sformat s "#<~a: ~a:~a>"
           (class-name (class-of x))
           (socket-host x)
           (socket-port x))
  x)

;;;-----------------------------------------------------------------------------
;;; Prin on file streams; default same as write
;;;-----------------------------------------------------------------------------
(defmethod generic-print ((vec <vector>) (s <buffered-stream>))
  (let ((n (vector-size vec)))
    (cond ((int-binary= n 0)
           (prin-string "#()" 3 s))
          ((int-binary= n 1)
           (prin-string "#(" 2 s)
           (sprin s (vector-ref vec 0))
           (sprin-one-char s #\) ))
          (t
           (let ((m (int-binary- n 1))
                 (i 0))
             (prin-string "#(" 2 s)
             (labels
              ((loop ()
                     (and (int-binary< i m)
                          (progn
                            (sprin s (vector-ref vec i))
                            (sprin-one-char s #\  )
                            (setq i (int-binary+ i 1))
                            (loop)))))
              (loop)
              (sprin s (vector-ref vec m))
              (sprin-one-char s #\) ))))))
  vec)

(defmethod generic-print ((l <cons>) (s <buffered-stream>))
  (sprin-one-char s #\( )
  (let ((tail (output-list-contents l generic-print s)))
    (if (null? tail) ()
      (progn
        (prin-string " . " 3 s)
        (sprin s tail))))
  (sprin-one-char s #\) )
  l)

(defmethod generic-print ((c <character>) (s <buffered-stream>))
  ;    (if (graph? c)
  (sprin-one-char s c)
  ;      (generic-write c s))
  c)

(defmethod generic-print ((x <symbol>) (s <buffered-stream>))
  (generic-print (symbol-name x) s)
  x)

;  (defmethod generic-print ((x <symbol>) (s <buffered-stream>))
;    (let ((str (symbol-name x)))
;      (prin-string str (string-size str) s)
;      x))

(defmethod generic-print ((str <string>) (s <buffered-stream>))
  (prin-string str (string-size str) s)
  str)

(defmethod generic-print ((x <class>) (s <buffered-stream>))
  (let ((class-name (symbol-name (or (class-name x) 'anonymous))))
    ;         (meta-class-name (symbol-name (class-name (class-of x)))))
    ;      (sprin-one-char s #\#)
    (sprin-one-char s #\<)
    ;      (prin-string meta-class-name (string-size meta-class-name) s)
    ;      (sprin-one-char s #\:)
    ;      (sprin-one-char s #\  )
    (prin-string class-name (string-size class-name) s)
    (sprin-one-char s #\>))
  x)

;;;-----------------------------------------------------------------------------
)  ;; End of module
;;;-----------------------------------------------------------------------------
