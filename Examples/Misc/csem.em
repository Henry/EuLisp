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
;;; Title: counting semaphores (see also dphil.em)
;;;  Authors: Andreas Kind, Chris Murphy
;;;  Maintainer: Henry G. Weller
;;;  Compilation
;;    youtoo -c csem -l level-1
;;;-----------------------------------------------------------------------------

(defmodule csem
  (syntax (macros)
   import (level-1)
   export (<csemaphore>
           cwait
           csignal))

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
)  ;; End of module csem
;;;-----------------------------------------------------------------------------
