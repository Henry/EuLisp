;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: level1
;;;  Authors: Andreas Kind, Julian Padget
;;; Description: formatted output (a first attempt!)

;;;-----------------------------------------------------------------------------
(defmodule format
  (syntax (_telos0)
   import (telos collect fpi list string stream)
   export (format))

;;;-----------------------------------------------------------------------------
;;; Formatted output
;;;-----------------------------------------------------------------------------
  (defmethod format ((s <null>) (str <string>) . args)
    (let* ((ss (make <string-stream>))
           (scb (stream-sink ss)))
      (apply format ss str args)
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

  (defmethod format ((s <symbol>) (str <string>) . args)
    (apply format stdout str args))

  (defmethod format ((s <int>) (str <string>) . args)
    (let ((stream (if (int-binary= s 2) stderr stdout)))
      (apply format stream str args)))

  (defextern format-info (<string>) ptr "eul_format_info")

  (defmethod format ((s <stream>) (str <string>) . args)
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
                                  (generic-prin x s)
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
                          (prin-one-char #\\n s)
                          (loop (cdr info) l))
                         ((eq c #\x)
                          (if (null? l)
                              (error "bad format string ~s" str)
                            (progn
                              (fprintf s "%x" (car l))
                              (loop (cdr info) (cdr l)))))
                         ((eq c #\o)
                          (if (null? l)
                              (error "bad format string ~s" str)
                            (progn
                              (fprintf s "%o" (car l))
                              (loop (cdr info) (cdr l)))))
                         ((eq c #\~)
                          (sprin s #\~)
                          (loop (cdr info) l))
                         (t
                           (loop (cdr info) l)))))))
        (let ((res (loop (reverse-list (format-info str)) args)))
          (flush s)
          res))))

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
