;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;; -----------------------------------------------------------------------
;;;                     EuLisp System 'youtoo'
;;; -----------------------------------------------------------------------
;;;  Library: level1 (EuLisp Language Level1 Implementation)
;;;  Authors: Andreas Kind, Julian Padget
;;;  Description: locks
;;; -----------------------------------------------------------------------
(defmodule lock
  (syntax (_telos0)
   import (telos thread)
   export (<semaphore> semaphore-counter semaphorep
           <lock> lockp lock unlock))
;;; --------------------------------------------------------------------
;;; Class <lock>
;;; --------------------------------------------------------------------
  (defclass <semaphore> ()
    ((counter default: 0 accessor: semaphore-counter keyword: counter:))
    predicate: semaphorep
    predicate: lockp)
  (deflocal <lock> <semaphore>)
;;; --------------------------------------------------------------------
;;; Lock
;;; --------------------------------------------------------------------
  (defgeneric lock (lk))
  (defmethod lock ((lk <lock>))
    (labels
     ((loop ()
            (or (test-and-set-lock lk)
                (progn
                  (thread-reschedule)
                  (loop)))))
     (loop)))
  (defopencoded test-and-set-lock (lk) (test-and-set-lock))
;;; --------------------------------------------------------------------
;;; Unlock
;;; --------------------------------------------------------------------
  (defgeneric unlock (lk))
  (defmethod unlock ((lk <lock>))
    ((setter semaphore-counter) lk 0)
    lk)
)  ; end of module
