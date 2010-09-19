;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: mpi
;;;  Authors: Andreas Kind
;;; Description: Message Passing Interface (MPI)
;;;  Compilation
;;    youtoo -ar mpis -l level1
;;;-----------------------------------------------------------------------------
(defmodule mpis
  (syntax (macros)
   import (level1 serial)
   export (<mpi-stream> mpi-stream? mpi-stream-host mpi-stream-rank
                        local-mpi-stream?))

;;;-----------------------------------------------------------------------------
;;; MPI stream class
;;;-----------------------------------------------------------------------------
(defclass <mpi-stream> <stream>
  ((host accessor: mpi-stream-host)
   (rank accessor: mpi-stream-rank)
   (object-stream accessor: mpi-stream-object-stream)
   (tag accessor: mpi-stream-tag))
  predicate: mpi-stream?)

(defmethod initialize ((x <mpi-stream>) inits)
  (call-next-method)
  (if (< *mpi-rank-counter* *mpi-max-rank*)
      (let ((os (make <object-stream> mode: 'rw)))
        (if (= *mpi-rank-counter* *mpi-local-rank*)
            ((setter mpi-stream-host) x *mpi-local-host*)
          ((setter mpi-stream-host) x '*remote*))
        ((setter mpi-stream-rank) x *mpi-rank-counter*)
        (setq *mpi-rank-counter* (+ *mpi-rank-counter* 1))
        ;; mpi streams are always r/w; probably not used anyway
        ;;((setter stream-mode) x 'rw)
        ((setter mpi-stream-object-stream) x os)
        ((setter mpi-stream-tag) x t)
        (connect os x)
        ((setter mpi-stream-tag) x ()))
    (error <condition> "exceeded specified number of mpi stream"))
  x)

(defmethod generic-print ((x <mpi-stream>) (s <stream>))
  (sformat s "#<~a: ~a ~a>"
           (class-name (class-of x))
           (mpi-stream-host x)
           (mpi-stream-rank x)))

(defmethod disconnect ((x <mpi-stream>))
  (eul-mpi-finalize)
  ;;(reset x)
  (call-next-method))

(defmethod reset ((x <mpi-stream>))
  (reset (mpi-stream-object-stream x)))

;;;-----------------------------------------------------------------------------
;;; Writing
;;;-----------------------------------------------------------------------------
(defmethod generic-write ((x <int>) (s <mpi-stream>))
  (let* ((rank (mpi-stream-rank s))
         (tag (mpi-stream-tag s))
         (res (eul-mpi-send x rank tag)))
    (if (null? res)
        (error <condition> (fmt "mpi write error with ~a" x))
      s)))

(defmethod generic-write ((x <double>) (s <mpi-stream>))
  (let* ((rank (mpi-stream-rank s))
         (tag (mpi-stream-tag s))
         (res (eul-mpi-send x rank tag)))
    (if (null? res)
        (error <condition> (fmt "mpi write error with ~a" x))
      s)))

(defmethod generic-write ((x <character>) (s <mpi-stream>))
  (let* ((rank (mpi-stream-rank s))
         (tag (mpi-stream-tag s))
         (res (eul-mpi-send x rank tag)))
    (if (null? res)
        (error <condition> (fmt "mpi write error with ~a" x))
      s)))

(defmethod generic-write ((x <string>) (s <mpi-stream>))
  (let* ((rank (mpi-stream-rank s))
         (tag (mpi-stream-tag s))
         (res (eul-mpi-send x rank tag)))
    (if (null? res)
        (error <condition> (fmt "mpi write error with ~a" x))
      s)))

(defmethod generic-write ((x <object>) (s <mpi-stream>))
  (let ((os (mpi-stream-object-stream s)))
    ((setter mpi-stream-tag) s t)
    (generic-write x os)
    ((setter mpi-stream-tag) s ())
    s))

;;;-----------------------------------------------------------------------------
;;; Reading
;;;-----------------------------------------------------------------------------
(defmethod generic-read ((s <mpi-stream>) eos-error? eos-value)
  (let* ((rank (mpi-stream-rank s))
         (tag (mpi-stream-tag s))
         (info (eul-mpi-probe rank () eos-error?))
         (sformat stderr "generic-read msg-tag: ~a\n" info)
         (if (null? info)
             (if (eq eos-error? t)
                 (end-of-stream s)
               eos-value)
           (let ((objp (vector-ref info 0))
                 (msg-tag (vector-ref info 1))
                 (n (vector-ref info 2)))
             (if (null? tag)
                 (if (null? objp)
                     (let ((res (eul-mpi-receive rank msg-tag n)))
                       (if (null? res)
                           (error <condition> "mpi read error")
                         res))
                   (let ((os (mpi-stream-object-stream s)))
                     ((setter mpi-stream-tag) s t)
                     (let ((res (generic-read os eos-error? eos-value)))
                       ((setter mpi-stream-tag) s ())
                       res)))
               (let ((res (eul-mpi-receive rank msg-tag n)))
                 (if (null? res)
                     (error <condition> "mpi read error")
                   res))))))))

;;;-----------------------------------------------------------------------------
;;; Foreign functions
;;;-----------------------------------------------------------------------------
(defextern eul-mpi-send (ptr <int> ptr) ptr "eul_mpi_send")
(defextern eul-mpi-receive (ptr ptr <int>) ptr "eul_mpi_receive")
(defextern eul-mpi-probe (ptr ptr ptr) ptr "eul_mpi_probe")
(defextern eul-mpi-finalize () <int> "MPI_Finalize")
(defextern eul-mpi-initialize () ptr "eul_mpi_initialize")

;;;-----------------------------------------------------------------------------
;;; Mpi initialization
;;;  *mpi-info* is initialized with id, nprocs and the processor name
;;;-----------------------------------------------------------------------------
(defconstant *mpi-info* (eul-mpi-initialize))
(defconstant *mpi-local-rank* (vector-ref *mpi-info* 0))
(defconstant *mpi-max-rank* (vector-ref *mpi-info* 1))
(defconstant *mpi-local-host* (vector-ref *mpi-info* 2))

(deflocal *mpi-rank-counter* 0)

(defun local-mpi-stream? (x)
  (= (mpi-stream-rank x) *mpi-local-rank*))

;;;-----------------------------------------------------------------------------
)  ;; End of module
;;;-----------------------------------------------------------------------------
