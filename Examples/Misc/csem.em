;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: Misc
;;;  Authors: Andreas Kind, Chris Murphy
;;; Description: counting semaphores (see also dphil.em)
;;;  Compilation
;;    youtoo -c csem -l level1
;;;-----------------------------------------------------------------------------
(defmodule csem
  (syntax (macros)
   import (level1)
   export (<csemaphore> cwait csignal))

;;;-----------------------------------------------------------------------------
;;; The counting semaphore class
;;;-----------------------------------------------------------------------------
(defclass <csemaphore> <semaphore>
  ((counter-lock default: (make <lock>)
                 accessor: counter-lock)
   (mutex-lock default: (make <lock>)
               accessor: mutex-lock)))

;;;-----------------------------------------------------------------------------
;;; Exclude thread until the count rises above zero
;;;-----------------------------------------------------------------------------
(defun cwait (csem)
  (lock (mutex-lock csem))
  (unlock (counter-lock csem))
  (if (= (semaphore-counter csem) 0)
      (lock (counter-lock csem))
    ())
  (unlock (mutex-lock csem))
  (lock (counter-lock csem))
  (lock (mutex-lock csem))
  ((setter semaphore-counter) csem (- (semaphore-counter csem) 1))
  (unlock (mutex-lock csem)))

;;;-----------------------------------------------------------------------------
;;; Indicate departure of a process from a critical region
;;;-----------------------------------------------------------------------------
(defun csignal (csem)
  (lock (mutex-lock csem))
  ((setter semaphore-counter) csem (+ (semaphore-counter csem) 1))
  (unlock (counter-lock csem))
  (unlock (mutex-lock csem)))

;;;-----------------------------------------------------------------------------
)  ;; end of module
;;;-----------------------------------------------------------------------------
