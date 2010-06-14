;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: level1
;;;  Authors: Julian Padget, Andreas Kind
;;; Description: streams
;;;-----------------------------------------------------------------------------
(defmodule stream
  (syntax (_macros)
   import (telos stream1 stream2 socket dynamic condition lock convert
           collect list string fpi)
   expose (stream1 stream2 socket)
   export (connect flush newline sprintf fprintf
           write prin print prin-one-char prin-char
           *int-size-in-decimal-digits* *double-size-in-decimal-digits*
           output-list-contents
           prin-string prin-address make-space file-lookup
           print-all sprint-all))

;;;------------------------------------------------------------------------
;;; Stream Manipulation Functions
;;;------------------------------------------------------------------------
  (defun connect (source sink . options)
    (generic-connect source sink options))

  (defmethod generic-connect ((s1 <stream>) (s2 <stream>) options)
    (let ((mode (stream-mode s1)))
      (cond
       ((eq mode 'r)
        ((setter stream-source) s1 s2))
       ((eq mode 'w)
        ((setter stream-sink) s1 s2))
       ((eq mode 'a)
        ((setter stream-sink) s1 s2))
       (t
        ((setter stream-source) s1 s2)
        ((setter stream-sink) s1 s2))))
    ())

  (defmethod reconnect ((s1 <stream>) (s2 <stream>))
    ((setter stream-source) s2 (stream-source s1))
    ((setter stream-source) s1 ())
    ((setter stream-sink) s2 (stream-sink s1))
    ((setter stream-sink) s1 ())
    ())

  (defmethod disconnect ((s <stream>))
    ((setter stream-source) s ())
    ((setter stream-sink) s ())
    ())

  (defmethod disconnect ((fs <file-stream>))
    (let ((sink (stream-sink fs))
          (source stream-source fs))
      (if (file-control-block? sink)
          (progn
            (flush-buffer fs)
            (if (int-binary= (eul_close (control-block-descriptor sink)) 0) ()
              (error (strerror) <stream-condition> value: fs)))
        ())
      (if (file-control-block? source)
          (if (int-binary= (eul_close (control-block-descriptor source)) 0) ()
            (error (strerror) <stream-condition> value: fs))
        ())
      (with-lock *open-file-streams*-lock
                 (setq *open-file-streams*
                       (list-remove fs *open-file-streams*)))
      (call-next-method)))

  (defmethod disconnect ((x <socket>))
    (if (int-binary= (eul_close (socket-descriptor x)) 0)
        (with-lock *open-file-streams*-lock
                   (setq *open-file-streams*
                         (list-remove x *open-file-streams*)))
      (error (strerror) <stream-condition> value: x)))

;;;------------------------------------------------------------------------
;;; Buffer Management
;;;------------------------------------------------------------------------
  (defmethod fill-buffer ((s <stream>))
    (let* ((source (stream-source s))
           (sink (stream-sink s))
           (sink-buf (control-block-buffer sink)))
      (if (null? sink-buf)
          0
        (progn
          ((setter control-block-buffer) source (reverse-list sink-buf))
          ((setter control-block-buffer) sink ())
          ;; return non-zero
          1))))

  (defmethod fill-buffer ((fs <file-stream>))
    (let* ((fcb (stream-source fs))
           (n (control-block-buffer-size fcb))
           (i (read-into-buffer (control-block-descriptor fcb)
                                (control-block-buffer fcb)
                                n)))
      ((setter control-block-buffer-cnt) fcb i)
      ((setter control-block-buffer-pos) fcb
       (int-binary- (int-binary/ n 2) 1))
      i))

  (defmethod fill-buffer ((ss <string-stream>))
    (let ((str-list (string-stream-string-list ss)))
      (if (null? str-list)
          0
        (let ((scb (stream-source ss)))
          ((setter string-stream-string-list) ss (cdr str-list))
          ((setter control-block-buffer) scb (car str-list))
          ((setter control-block-buffer-cnt) scb 0)
          ;; return non-zero
          1))))

  (defmethod flush-buffer ((s <stream>))
    (let* ((source (stream-source s))
           (sink (stream-sink s))
           (source-buf (control-block-buffer source))
           (sink-buf (control-block-buffer sink)))
      ((setter control-block-buffer) source
       (append source-buf (reverse sink-buf)))
      ((setter control-block-buffer) sink ())
      t))

  (defmethod flush-buffer ((fs <file-stream>))
    ;; Write buffer to sink and reset
    (let* ((fcb (stream-sink fs))
           (i (eul_write (control-block-descriptor fcb)
                         (control-block-buffer fcb)
                         (control-block-buffer-pos fcb))))
      (if (int-binary= i -1)
          ;; indicates failure
          ()
        (progn
          ((setter control-block-buffer-pos) fcb 0)
          fs))))

  (defmethod flush-buffer ((ss <string-stream>))
    ;; Append buffer to the list of strings and reset
    (let* ((scb (stream-sink ss))
           (pos (control-block-buffer-pos scb))
           (str (substring (control-block-buffer scb) 0 pos)))
      ((setter string-stream-string-list) ss
       (append (string-stream-string-list ss) (list str)))
      ((setter control-block-buffer-pos) scb 0)
      ((setter control-block-buffer) scb
       (make <string> size: (control-block-buffer-size scb))))
    ss)

  (defmethod end-of-stream ((s <stream>))
    (error "end of stream" <end-of-stream> value: s))

  (defmethod end-of-stream ((fs <file-stream>))
    (disconnect fs)
    (call-next-method))

;;;------------------------------------------------------------------------
;;; Read Operations
;;;------------------------------------------------------------------------
  (defmethod generic-read ((s <stream>) eos-error? eos-value)
    ((stream-read-action s) s eos-error? eos-value))

  (defmethod generic-read ((fs <file-stream>) eos-error? eos-value)
    (let ((fcb (stream-source fs)))
      (if (and (int-binary= (control-block-buffer-cnt fcb) 0)
               (int-binary= (fill-buffer fs) 0))
          (if eos-error? (end-of-stream fs) eos-value)
        (let* ((cnt (control-block-buffer-cnt fcb))
               (pos (control-block-buffer-pos fcb))
               (new-pos (int-binary+ pos 1)))
          ((setter control-block-buffer-cnt) fcb (int-binary- cnt 1))
          ((setter control-block-buffer-pos) fcb new-pos)
          (string-ref (control-block-buffer fcb) new-pos)))))

  (defmethod generic-read ((ss <string-stream>) eos-error? eos-value)
    (let ((scb (stream-source ss)))
      (if (and (int-binary= (control-block-buffer-cnt scb)
                            (string-size (control-block-buffer scb)))
               (int-binary= (fill-buffer ss) 0))
          (if eos-error? (end-of-stream ss) eos-value)
        (let* ((cnt (control-block-buffer-cnt scb))
               (c (string-ref (control-block-buffer scb) cnt)))
          ((setter control-block-buffer-cnt) scb (int-binary+ cnt 1))
          c))))

;;;------------------------------------------------------------------------
;;; Write and print
;;;------------------------------------------------------------------------
  (defun write (x . ss)
    (match-let ss ((s stdout))
               (if (object? x)
                   (generic-write x s)
                 (progn
                   (prin-string "#<C: " 5 s)
                   (prin-address x s)
                   (prin-one-char #\> s))))
    x)

  (defun prin (x . ss)
    (match-let ss ((s stdout))
               (if (object? x)
                   (generic-prin x s)
                 (progn
                   (prin-string "#<C: " 5 s)
                   (prin-address x s)
                   (prin-one-char #\> s))))
    x)

  (defun print (x . ss)
    (match-let ss ((s stdout))
               (if (object? x)
                   (progn
                     (generic-prin x s)
                     (prin-one-char #\\n s))
                 (progn
                   (prin-string "#<C: " 5 s)
                   (prin-address x s)
                   (prin-string ">\n" 2 s))))
    x)

  (defun flush ss
    (match-let ss ((s stdout))
               (flush-buffer s)))

  (defun newline ss
    (match-let ss ((s stdout))
               (prin-one-char #\\n s)))

  (defun sprin-all (s . args)
    (if args
        (do1-list
          (lambda (x)
            (if (object? x)
                (generic-prin x s)
              (progn
                (prin-string "#<C: " 5 s)
                (prin-address x s)
                (prin-one-char #\> s))))
          (car args))
      ())
    s)

  (defun sprint-all (s . args)
    (apply sprin-all s args)
    (prin-one-char #\\n s))

  (defun prin-all args
    (sprin-all stdout args))

  (defun print-all args
    (sprint-all stdout args))

;;;------------------------------------------------------------------------
;;; Some low level functions to get things printed on <file-stream>s
;;;------------------------------------------------------------------------
  (defun prin-char (c . ss)
    (match-let ss
               ((s stdout)
                (times 1))
               (if (int-binary= times 1)
                   (prin-one-char c s)
                 (prin-char* c s times))))

  (defun prin-one-char (c s)
    (let* ((scb (stream-sink s))
           (pos (control-block-buffer-pos scb))
           (new-pos (int-binary+ pos 1))
           (bufsiz (control-block-buffer-size scb)))
      ((setter string-ref) (control-block-buffer scb) pos c)
      ((setter control-block-buffer-pos) scb new-pos)
      (if (or (eql c #\\n)
              (null? (int-binary< new-pos bufsiz)))
          (flush-buffer s)
         ())))

  (defun prin-char* (char s times)
    (let* ((scb (stream-sink s))
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
                        (flush-buffer s)
                      ())
                    (loop (int-binary+ i 1)))
                ())))
       (loop 0))))

  (defun make-space (s n)
    ;; There must be at least one byte more available than requested
    (let* ((scb (stream-sink s))
           (bufsiz (control-block-buffer-size scb))
           (pos (control-block-buffer-pos scb))
           (left (int-binary- bufsiz pos)))
      (if (int-binary< n left) t
        (progn
          (flush-buffer s)
          ;; Buffer size greater than n?
          (int-binary< n bufsiz)))))

  ;; Will be 16 for 64 bit addresses
  (defconstant *address-size-in-hex-digits* 8)

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
      (prin-string str n s)))

  (defun prin-address (x s) (fprintf s "0x%08X" x))

  (defun prin-string (str n s)
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
                   (prin-one-char #\  s)
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
  )  ;; end of module
;;;-----------------------------------------------------------------------------
