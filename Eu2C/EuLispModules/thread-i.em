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
;;; Title: Basic thread functions for use in modules thread and lock
;;;  Authors: Jens Bimberg
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule thread-i
  (import (object-0
           thread-ii
           thread-b
           (only (cons
                  car
                  cdr
                  null?
                  t)
                 list)
           (only (eq)
                 compare)
           (only (setter
                  <function>)
                 function)
           (only (signal
                  error
                  <thread-condition>
                  <wrong-condition-class>
                  <domain-condition>)
                 condition)
           (only (<object>
                  <class>
                  %cast
                  %void
                  %instance-of?)
                 tail))
   syntax ((only (when
                  unless)
                 syntax-0))
   export (TL-empty
           TL-single
           TL-on-top
           TL-on-end
           TL-revolve
           TL-leave-first
           TL-queue-on-top
           wait-in-T-LST
           thread-yield
           The-Sequential-Set))

(deflocal The-Sequential-Set (make-T-LST))

;;;-----------------------------------------------------------------------------
;;; moving threads around in T-LST's
;;  test whether a T-LST is empty; in that case First is NIL
;;;-----------------------------------------------------------------------------

(%define-function (TL-empty <object>)
  ((TL <T-LST>))
  (null? (First TL)))

;; test whether there's only one thread in a T-LST; in that case First and
;; Last point to the same thread. This Function may fail, if the T-LST is empty

(%define-function (TL-single <object>)
  ((TL <T-LST>))
  (eq (First TL) (Last TL)))

(%define-function (TL-leave-first <thread>)
  ((TL <T-LST>))  ;must not be empty!!
  (%let ((thread <thread> (%cast <thread> (First TL))))
        (set-First TL (next thread))
        thread))

(%define-function (TL-on-top %void)
  ((TL <T-LST>) (thread <thread>))
  (when (TL-empty TL) (set-Last TL thread))
  (set-next thread (First TL))
  (set-First TL thread))

(%define-function (TL-on-end %void)
  ((TL <T-LST>) (thread <thread>))
  (if (TL-empty TL)
      (set-First TL thread)
    (set-next (%cast <thread> (Last TL)) thread))
  (set-Last TL thread)
  (set-next thread ()))

(%define-function (TL-queue-on-top %void)
  ((TL <T-LST>) (TL1 <T-LST>))
  (unless (TL-empty TL1)
          (when (TL-empty TL) (set-Last TL (Last TL1)))
          (set-next (%cast <thread> (Last TL1)) (First TL))
          (set-First TL (First TL1))))

(%define-function (TL-queue-on-end %void)
  ((TL <T-LST>) (TL1 <T-LST>))
  (unless (TL-empty TL1)
          (if (TL-empty TL)
              (set-First TL (First TL1))
            (set-next (%cast <thread> (Last TL)) (First TL1)))
          (set-Last TL (Last TL1))
          (set-next (%cast <thread> (Last TL1)) ())))

(%define-function (TL-revolve %void)
  ((TL <T-LST>)) ; must not be empty
  (set-next (%cast <thread> (Last TL)) (First TL))
  (set-Last TL (First TL))
  (set-First TL (next (%cast <thread> (First TL))))
  (set-next (%cast <thread> (Last TL)) ()))

;;;-----------------------------------------------------------------------------
;;; signal condition on thread
;;;-----------------------------------------------------------------------------

;; first store the old value of signal to a local variable
(deflocal normal-signal signal)

(%define-function (raise-condition %void)
  ((c <object>))
  (cont-threads)  ; should be interruptable, migth not return
  (normal-signal (car c) (cdr c))
  (hold-threads))

(%define-function (raise-conditions %void)
  ((thread <thread>))
  (unless (null? (condqueue thread))
          (let ((c (car (condqueue thread))))
            (set-condqueue thread (cdr (condqueue thread)))
            (raise-condition c)
            (raise-conditions thread))))

(defun register-condition-1 (queue condpair)
  (if (null? (cdr queue))
      ((setter cdr) queue condpair)
    (register-condition-1 (cdr queue) condpair)))

(defun register-condition (thread condpair)
  (if (null? (condqueue thread))
      (set-condqueue thread condpair)
    (register-condition-1 (condqueue thread) condpair)))

(defun signal-with-threads (condition continuation . thread)
  (if (null? thread)
      (normal-signal condition continuation)
    (if (%instance-of? condition <thread-condition>)
        (if (thread? (car thread))
            (let ((condpair (cons
                             (cons condition continuation) ())))
              (hold-threads)
              (register-condition (car thread) condpair)
              (cont-threads) ())
          (error <domain-condition>
                 "Optional argument to signal not a thread"))
      (error <wrong-condition-class>
             "No thread condition"
             'current-thread (First The-Sequential-Set)
             'condition condition))))

;; now set the new signal function as new value for signal

(setq signal signal-with-threads)

;;;-----------------------------------------------------------------------------
;;; yield to another thread
;;;-----------------------------------------------------------------------------
(%define-function (thread-yield %void)
  ((from <thread>) (to <thread>))
  (save-dynamics (saved-dynamics from))
  ;; give control away
  (m-thread-yield (m-thread to))
  ;; got control back, this is the ONLY such point!!
  (restore-dynamics (saved-dynamics from))
  (unless (null? (condqueue from))
          (raise-conditions from)))

;;;-----------------------------------------------------------------------------
;;; wait in a T-LST
;;;-----------------------------------------------------------------------------
(%define-function (wait-in-T-LST  %void)
  ((TL <T-LST>))
  (hold-threads)
  (let ((thread (TL-leave-first The-Sequential-Set)))
    (TL-on-end TL thread)
    (thread-yield thread (First The-Sequential-Set)))
  (cont-threads))

;;;-----------------------------------------------------------------------------
)  ;; End of module thread-i
;;;-----------------------------------------------------------------------------
