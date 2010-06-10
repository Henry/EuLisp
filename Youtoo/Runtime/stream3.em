;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: level1
;;;  Authors: Julian Padget, Andreas Kind
;;; Description: streams
;;;-----------------------------------------------------------------------------
(defmodule stream3
  (syntax (_telos0)
   import (telos integer collect list character string vector
           float stream1 stream))

;;;-----------------------------------------------------------------------------
;;; Default generic write/prin method
;;;-----------------------------------------------------------------------------
  (defmethod generic-write (x (s <stream>))
    (if (stream-write-action s)
        ((stream-write-action s) x s)
      ()))

  (defmethod generic-prin (x (s <stream>))
    ;; Default generic-prin == generic-write
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
    (prin-one-char #\> s)
    x)

  (defmethod generic-write ((x <null>) (s <buffered-stream>))
    (prin-string "()" 2 s)
    ())

  (defmethod generic-write ((x <symbol>) (s <buffered-stream>))
    (let ((str (symbol-name x)))
      ;; should also check for start comment!
      (if (allp graphp str)
          (generic-prin str s)
        (progn
          (prin-one-char #\| s)
          (generic-prin str s)
          (prin-one-char #\| s))))
    x)

  (defmethod generic-write ((x <keyword>) (s <buffered-stream>))
    (let ((name (symbol-name x)))
      (prin-string name (string-size name) s))
    (prin-one-char #\: s)
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
    (format s "#<~a: ~a>" (class-name (class-of x)) (function-name x)))

  (defmethod generic-write ((vec <vector>) (s <buffered-stream>))
    (let ((n (vector-size vec)))
      (cond ((int-binary= n 0)
             (prin-string "#()" 3 s))
            ((int-binary= n 1)
             (prin-string "#(" 2 s)
             (write (vector-ref vec 0) s)
             (prin-one-char #\) s))
            (t
             (let ((m (int-binary- n 1))
                   (i 0))
               (prin-string "#(" 2 s)
               (labels
                ((loop ()
                       (and (int-binary< i m)
                            (progn
                              (write (vector-ref vec i) s)
                              (prin-one-char #\  s)
                              (setq i (int-binary+ i 1))
                              (loop)))))
                (loop)
                (write (vector-ref vec m) s)
                (prin-one-char #\) s))))))
    vec)

  (defmethod generic-write ((l <cons>) (s <buffered-stream>))
    (prin-one-char #\( s)
    (let ((tail (output-list-contents l generic-write s)))
      (if (null? tail) ()
        (progn
          (prin-string " . " 3 s)
          (write tail s))))
    (prin-one-char #\) s)
    l)

  (defmethod generic-write ((c <character>) (s <buffered-stream>))
    ;; Certain characters should be interpreted
    (prin-one-char #\# s)
    (prin-one-char #\\\ s)
    (prin-one-char c s)
    c)

  (defmethod generic-write ((x <string>) (s <buffered-stream>))
    ;; Certain characters should be interpreted
    (prin-one-char #\" s)
    (prin-string x (string-size x) s)
    (prin-one-char #\" s)
    x)

  (defmethod generic-write ((x <file-stream>) (s <buffered-stream>))
    (let ((sink (stream-sink x))
          (source (stream-source x))
          (mode (stream-mode x)))
      (format s "#<~a: ~a>"
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
    (format s "#<~a: ~a:~a>"
            (class-name (class-of x))
            (connection-host x)
            (connection-port x))
    x)

  (defmethod generic-write ((x <socket>) (s <buffered-stream>))
    (format s "#<~a: ~a:~a>"
            (class-name (class-of x))
            (socket-host x)
            (socket-port x))
    x)

;;;-----------------------------------------------------------------------------
;;; Prin on file streams; default same as write
;;;-----------------------------------------------------------------------------
  (defmethod generic-prin ((vec <vector>) (s <buffered-stream>))
    (let ((n (vector-size vec)))
      (cond ((int-binary= n 0)
             (prin-string "#()" 3 s))
            ((int-binary= n 1)
             (prin-string "#(" 2 s)
             (prin (vector-ref vec 0) s)
             (prin-one-char #\) s))
            (t
             (let ((m (int-binary- n 1))
                   (i 0))
               (prin-string "#(" 2 s)
               (labels
                ((loop ()
                       (and (int-binary< i m)
                            (progn
                              (prin (vector-ref vec i) s)
                              (prin-one-char #\  s)
                              (setq i (int-binary+ i 1))
                              (loop)))))
                (loop)
                (prin (vector-ref vec m) s)
                (prin-one-char #\) s))))))
    vec)

  (defmethod generic-prin ((l <cons>) (s <buffered-stream>))
    (prin-one-char #\( s)
    (let ((tail (output-list-contents l generic-prin s)))
      (if (null? tail) ()
        (progn
          (prin-string " . " 3 s)
          (prin tail s))))
    (prin-one-char #\) s)
    l)

  (defmethod generic-prin ((c <character>) (s <buffered-stream>))
    ;    (if (graphp c)
    (prin-one-char c s)
    ;      (generic-write c s))
    c)

  (defmethod generic-prin ((x <symbol>) (s <buffered-stream>))
    (generic-prin (symbol-name x) s)
    x)

  ;  (defmethod generic-prin ((x <symbol>) (s <buffered-stream>))
  ;    (let ((str (symbol-name x)))
  ;      (prin-string str (string-size str) s)
  ;      x))

  (defmethod generic-prin ((str <string>) (s <buffered-stream>))
    (prin-string str (string-size str) s)
    str)

  (defmethod generic-prin ((x <class>) (s <buffered-stream>))
    (let ((class-name (symbol-name (or (class-name x) 'anonymous))))
      ;         (meta-class-name (symbol-name (class-name (class-of x)))))
      ;      (prin-one-char #\# s)
      (prin-one-char #\< s)
      ;      (prin-string meta-class-name (string-size meta-class-name) s)
      ;      (prin-one-char #\: s)
      ;      (prin-one-char #\  s)
      (prin-string class-name (string-size class-name) s)
      (prin-one-char #\> s))
    x)

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
