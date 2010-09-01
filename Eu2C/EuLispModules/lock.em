;;; Copyright 1994-2010 Fraunhofer ISST
;;; Copyright 2010 Henry G. Weller
;;;-----------------------------------------------------------------------------
;;  This file is part of
;;; ---                           EuLisp System 'Eu2C'
;;;-----------------------------------------------------------------------------
;;
;;  Eu2C is free software: you can redistribute it and/or modify it under the
;;  terms of the GNU General Public License version 2 as published by the Free
;;  Software Foundation.
;;
;;  Eu2C is distributed in the hope that it will be useful, but WITHOUT ANY
;;  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;  details.
;;
;;  You should have received a copy of the GNU General Public License along with
;;  this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;-----------------------------------------------------------------------------
;;;  Title: semaphores as part of the thread implementation
;;;  Description:
;;;  Authors: Jens Bimberg
;;;-----------------------------------------------------------------------------
(defmodule lock
  (import ((only (<T-LST>
                  make-T-LST
                  First
                  <thread>)
                 thread-ii)
    (only (wait-in-T-LST
           TL-empty
           TL-revolve
           TL-on-top
           TL-leave-first
           The-Sequential-Set
           thread-yield)
          thread-i)
    (only (hold-threads
           cont-threads)
          thread-b)
    (only (%instance-of-p
           %signed-word-integer
           <class>
           <object>)
          tail)
    (only (eq)
          basic-compare)
    (only (<symbol>)
          basic-symbol))
   export (<lock>
           lock?
           lock
           unlock))

(%define-standard-class (<lock> <class>)
  <object>
  ((state type <symbol>
          reader state writer set-state default 'unlocked)
   (waiters type <T-LST>
            accessor waiters default (make-T-LST)))
  constructor (make-lock) representation pointer-to-struct
  allocation multiple-type-card )

(defun lock? (obj)
  (%instance-of-p obj <lock>))

(%define-function (lock <lock>)
  ((l <lock>))
  (hold-threads)
  (if (eq (state l) 'unlocked)
      (set-state l 'locked)
    (wait-in-T-LST (waiters l)))
  (cont-threads)
  l)

(%define-function (unlock <lock>)
  ((l <lock>))
  (hold-threads)
  (if (TL-empty (waiters l))
      (set-state l 'unlocked)
    (let ((thread (First The-Sequential-Set)))
      (TL-revolve The-Sequential-Set)
      (TL-on-top The-Sequential-Set (TL-leave-first (waiters l)))
      (thread-yield thread (First The-Sequential-Set))))
  (cont-threads)
  l)
)
