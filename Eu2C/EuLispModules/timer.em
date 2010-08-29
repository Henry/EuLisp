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
;;;  Title: timer interrupt handling
;;;  Description:
;;    interface to unix interval timers and signals
;;    enables creating a real and a virtual timer
;;    (set-real-timer x fun) (set-virtual-timer x fun)
;;    both timers fire any x microseconds and then call fun
;;    used by threads to reschedule
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Jens Bimberg
;;;-----------------------------------------------------------------------------
(defmodule timer
  (import
   ((only (%signed-word-integer
           <class>
           <object>
           %void
           make-swi
           %cast
           %sighandler)
          tail)
    (only (<integer>) integer)
    (only (<symbol>) symbol)
    (only (eq) compare)
    (only (<function>
           function-address)
          function-i))
   c-import (<sys/time.h>
             <signal.h>)
   export (set-interval-timer
           set-timer))

; interval-timers are declared in <sys/time.h>, here's what they're like:

(%define-standard-class (itimer <class>)
  <object>
  ((interv-sec  type %signed-word-integer default #%i0
                writer set-inter-sec)
   (interv-usec type %signed-word-integer default #%i0
                writer set-inter-usec)
   (cur-sec     type %signed-word-integer default #%i0
                writer set-current-sec)
   (cur-usec    type %signed-word-integer default #%i0
                writer set-current-usec))
  constructor (make-itimer)
  representation pointer-to-struct
  allocation multiple-type-card)

; the following are the declarations of the timer's names
; and of the system call used to set the timers.

(%declare-external-variable itimer-real %signed-word-integer
  language c external-name |ITIMER_REAL|)

(%declare-external-variable itimer-virtual %signed-word-integer
  language c external-name |ITIMER_VIRTUAL|)

(%declare-external-function (c-setitimer %signed-word-integer)
  ((which %signed-word-integer)
   (value  itimer)
   (ovalue itimer))
  language c external-name |setitimer|)

; now for the installation of the signal handlers; first the declaration
; of the function to set a signal handler

(%declare-external-function (c-signal %sighandler)
  ((sig %signed-word-integer)
   (handler %sighandler))
  language c external-name |signal|)

; the following are the numbers of the signals we want to catch

(%declare-external-variable sig-alarm %signed-word-integer
  language c external-name |SIGALRM|)

(%declare-external-variable sig-vt-alarm %signed-word-integer
  language c external-name |SIGVTALRM|)

; that's all with the interface to the C-side. Now for our own functions.
; it seems sensefull to hide the C things completely and give only the chance
; to set signal handler and timer at once
; the function must therefor take the interval, the function (signal handler)
; and one of two values dependent from which timer to call

; two static instances of the class itimer

(deflocal    real-timer (make-itimer))
(deflocal virtual-timer (make-itimer))

(%define-function (set-interval-timer %void)
  ((which <symbol>)
   (interval <integer>)
   (handler <function>))
  (if (eq which 'real)
      (progn
        (c-signal sig-alarm (%cast %sighandler (function-address handler)))
        (set-current-usec  real-timer (make-swi interval))
        (set-inter-usec real-timer (make-swi interval))
        (c-setitimer itimer-real real-timer
                     (%cast itimer #%i0)))
    (progn
      (c-signal sig-vt-alarm (%cast %sighandler (function-address handler)))
      (set-current-usec  virtual-timer (make-swi interval))
      (set-inter-usec virtual-timer (make-swi interval))
      (c-setitimer itimer-virtual virtual-timer
                   (%cast itimer #%i0)))))

; function to set a timer to fire one time
; the interval time used to reload the timer after expiration will not be
; changed, therefore there's no need to do any reparation afterwards
; the caller is responsible to set the c-signal-handler

(%define-function (set-timer %void)
  ((which <symbol>)
   (sec  %signed-word-integer)
   (usec %signed-word-integer))
  (if (eq which 'real)
      (progn
        (set-current-sec  real-timer  sec)
        (set-current-usec real-timer usec)
        (c-setitimer itimer-real real-timer
                     (%cast itimer #%i0)))
    (progn
      (set-current-sec  virtual-timer  sec)
      (set-current-usec virtual-timer usec)
      (c-setitimer itimer-virtual virtual-timer
                   (%cast itimer #%i0)))))

;;;-----------------------------------------------------------------------------
)
;;;-----------------------------------------------------------------------------
