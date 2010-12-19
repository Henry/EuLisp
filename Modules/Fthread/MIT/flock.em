;;; Copyright 1996 A. Kind & University of Bath
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
;;; Title: UI locks for EuLisp
;;;  Library: fthread (foreign thread interface)
;;;  Authors: Andreas Kind, Liam Wickins
;;;  Maintainer: Henry G. Weller
;;;  Compilation
;;    see fthread.em
;;;-----------------------------------------------------------------------------

(defmodule flock
  (syntax (syntax-1)
   import (level-1
           fcsem)
   export (<thread-lock>
           thread-lock?
           lock-handle))

;;;-----------------------------------------------------------------------------
;;; Lock classes
;;;  EuLisp locks cannot be implemented with foreign mutexes because in
;;;  order to unlock a foreign UI mutex (and this different in EuLisp) the
;;;  mutex  must be locked and the calling thread must be the one
;;;  that last locked the mutex (the owner).
;;;
;;;  Locks with UI semantics are provided with class <thread-lock>.
;;;-----------------------------------------------------------------------------
;; UI locks are now default
(setq <lock> <csemaphore>)

(defclass <thread-lock> ()
  ((handle accessor: lock-handle))
  predicate: thread-lock?)

(defmethod initialize ((lk <thread-lock>) keywords)
  (call-next-method)
  ((setter lock-handle) lk (eul_lock_create))
  lk)

(defextern eul_lock_create () ptr)

;;;-----------------------------------------------------------------------------
;;; lock
;;;-----------------------------------------------------------------------------
;(defmethod lock ((lk <csemaphore>))
;  (cwait lk))

(defmethod lock ((lk <thread-lock>))
  (eul_lock (lock-handle lk))
  lk)

(defextern eul_lock (ptr) ptr "pthread_mutex_lock")

;;;-----------------------------------------------------------------------------
;;; unlock
;;;-----------------------------------------------------------------------------
;(defmethod unlock ((lk <csemaphore>))
;  (csignal lk))

(defmethod unlock ((lk <thread-lock>))
  (eul_unlock (lock-handle lk))
  lk)

(defextern eul_unlock (ptr) ptr "pthread_mutex_unlock")

;;;-----------------------------------------------------------------------------
)  ;; End of module flock
;;;-----------------------------------------------------------------------------
