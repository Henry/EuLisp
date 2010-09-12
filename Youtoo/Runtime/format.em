;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'Youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: level1
;;;  Authors: Andreas Kind, Julian Padget
;;;  Description: formatted output (a first attempt!)
;;;-----------------------------------------------------------------------------
(defmodule format
  (syntax (_macros)
   import (telos collect fpi list string character stream condition let-cc
                 socket lock convert dynamic)
   export (sformat format fmt cerror))

;;;-----------------------------------------------------------------------------
;;; Formatted output
;;;-----------------------------------------------------------------------------
(defextern format-info (<string>) ptr "eul_format_info")

(defun sformat (s str . args)
  (let ((scb (stream-sink s)))
    (labels
     ((loop (info l)
            ;(if (null? info)
            ;(error "bad format string ~s" str)
            (let* ((j (car info))
                   (n (progn (setq info (cdr info)) (car info)))
                   (c (progn (setq info (cdr info)) (car info))))
              ;; following lines are inlined prin-string function
              (make-space s n)
              (let ((pos (control-block-buffer-pos scb))
                    (buf (control-block-buffer scb)))
                (eul_sprintf_string buf pos n j "%s" str)
                ((setter control-block-buffer-pos) scb
                 (int-binary+ pos n)))
              (if (eq c #\\x0000) l
                (cond ((or (eq c #\a) (eq c #\d))
                       (if (null? l)
                           (error "bad format string ~s" str)
                         (let ((x (car l)))
                           (if (object? x)
                               (generic-print x s)
                             (sprin s x))
                           (loop (cdr info) (cdr l)))))
                      ((eq c #\s)
                       (if (null? l)
                           (error "bad format string ~s" str)
                         (let ((x (car l)))
                           (if (object? x)
                               (generic-write x s)
                             (swrite s x))
                           (loop (cdr info) (cdr l)))))
                      ((eq c #\%)
                       (sprin-one-char s #\\n)
                       (loop (cdr info) l))
                      ((or (eq c #\x) (eq c #\o)
                           (eq c #\f) (eq c #\e) (eq c #\g))
                       (if (null? l)
                           (error "bad format string ~s" str)
                         (progn
                           (fprintf
                            s
                            (concatenate "%" (character-as-string c))
                            (car l))
                           (loop (cdr info) (cdr l)))))
                      ((eq c #\~)
                       (sprin s #\~)
                       (loop (cdr info) l))
                      (t
                       (loop (cdr info) l)))))))
     (let ((res (loop (reverse-list (format-info str)) args)))
       (sflush s)
       res))))

(defun format (str . args)
  (apply sformat stdout str args))

;; Format the arguments as a string
;; for use with the n-ary print function for example:
;; (print "test n = " (fmt "~d" n) " only")
(defun fmt (str . args)
  (let* ((ss (make <string-stream>))
         (scb (stream-sink ss)))
    (apply sformat ss str args)
    (labels
     ((loop (ll res)
            (if (null? ll) res
              (loop (cdr ll) (string-append res (car ll))))))
     ;; should be optimized by allocating one big string and then
     ;; filling it
     (string-append (loop (string-stream-string-list ss) "")
                    (substring
                     (control-block-buffer scb) 0
                     (control-block-buffer-pos scb))))))

;;;-----------------------------------------------------------------------------
;;; Formatted error messages
;;;-----------------------------------------------------------------------------
;; error already defined in module boot
(setq *error*
      (named-lambda error (str class . rest)
                    (if (and (class? class) (subclass? class <condition>))
                        (signal (apply make class message: str rest) ())
                      ;; Not EuLisp but very comfortable
                      (signal
                       (make <condition> message: (apply fmt str class rest))
                       ()))))

(defun cerror (str class . rest)
  (if (and (class? class) (subclass? class <condition>))
      (let/cc k (signal (apply make class message: str rest) k))
    ;; Not EuLisp but very comfortable
    (let/cc k (signal
               (make <condition> message: (apply fmt str class rest))
               k))))

;;;-----------------------------------------------------------------------------
;;; Socket connections
;;;-----------------------------------------------------------------------------
(defmethod initialize ((x <connection>) inits)
  (call-next-method)
  (let ((s (init-list-ref inits socket:))
        host port fd)
    (if (socket? s)
        (progn
          ;; leads to accept
          (setq host (socket-host s))
          (setq port (socket-port s))
          ((setter connection-host) x host)
          ((setter connection-port) x port)
          (setq fd (eul_socket_accept (socket-descriptor s))))
      (progn
        ;; leads to connect
        (setq host (connection-host x))
        (setq port (connection-port x))
        (setq fd (eul_make_connection host (convert port <string>) "tcp"))))
    ;; error handling
    (if (int-binary= fd -1)
        (error (strerror) <stream-condition> value: x)
      (if (int-binary< fd -1)
          (error (eul_socket_strerror fd) <stream-condition> value: x)
        ()))
    (let* ((file-name (fmt "~a:~a" host port))
           (fcb1 (make <file-control-block>
                       file-name: file-name
                       mode: 'r
                       descriptor: fd))
           (fcb2 (make <file-control-block>
                       file-name: file-name
                       mode: 'w
                       descriptor: fd)))
      ((setter stream-source) x fcb1)
      ((setter stream-sink) x fcb2)
      (with-lock *open-file-streams*-lock
                 (setq *open-file-streams*
                       (cons x *open-file-streams*)))
      x)))

;;;-----------------------------------------------------------------------------
)  ;; end of module
;;;-----------------------------------------------------------------------------
