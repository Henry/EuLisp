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
;;; Title: internal event handling mechanisms
;;;  Description:
;;    Defines the generic function wait and the double float constant
;;    ticks-per-second
;;;  Authors: Jens Bimberg
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule event-i
  (import ((only (<class>
                  %signed-word-integer
                  make-swi
                  %gt
                  %lt
                  %eq
                  %plus
                  %minus
                  %cast
                  <object>
                  %void)
                 tail)
           (only (*
                  /
                  binary-mod)
                 number)
           (only (<integer>)
                 integer)
           (only (t)
                 basic-list)
           (only (set-timer)
                 timer))
   syntax ((only (when
                  unless)
                 syntax-i))
   export (wait
           get-timer
           timeout-expired
           ticks-per-second
           time-lt
           get-this-time
           sleep-until))

(defconstant ticks-per-second 100.0)
(defconstant ticks-ps-fixnum 100)
(defconstant usec-per-tick (/ 1000000 ticks-ps-fixnum))

(%define-standard-class (struct-timeval <class>)
  <object>
  ((tv-sec type %signed-word-integer
           reader tv-sec writer setf-tv-sec)
   (tv-usec type %signed-word-integer
            reader tv-usec writer setf-tv-usec))
  constructor (make-struct-timeval)
  representation pointer-to-struct
  allocation multiple-type-card)

(deflocal local-tp (make-struct-timeval))

(%declare-external-function (gettime %void)
  ((tp struct-timeval)
   (tzp <object>))
  language c external-name |gettimeofday|)

;; The generic function wait holds the current thread until timeout did expire
;; or a condition connected with object did occure, whichever comes first
(defgeneric wait (object timeout))

;; a couple of functions to be used in methods of wait
;; get an object to describe the time after timeout ticks
(%define-function (get-timer struct-timeval)
  ((timeout <integer>))
  (let ((sec  (make-swi (/   timeout ticks-ps-fixnum)))
        (usec (make-swi (* (binary-mod timeout ticks-ps-fixnum)
                           usec-per-tick)))
        (tp (make-struct-timeval)))
    (gettime tp (%cast <object> #%i0))
    (setf-tv-sec  tp (%plus (tv-sec  tp) sec))
    (setf-tv-usec tp (%plus (tv-usec tp) usec))
    (when (%gt (tv-usec tp) #%i1000000)
          (setf-tv-usec tp (%minus (tv-usec tp) #%i1000000))
          (setf-tv-sec tp (%plus (tv-sec tp) #%i1)))
    tp))

;; test whether the time described by a timer object did expire
(%define-function (timeout-expired <object>)
  ((tp struct-timeval))
  (gettime local-tp (%cast <object> #%i0))
  (if (%gt (tv-sec local-tp) (tv-sec tp))
      t
    (if (%eq (tv-sec local-tp) (tv-sec tp))
        (if (%gt (tv-usec local-tp) (tv-usec tp))
            t
          ())
      ())))

;; compare two timer objects
(defun time-lt (timer1 timer2)
  (if (%lt (tv-sec timer1) (tv-sec timer2))
      t
    (if (%eq (tv-sec timer1) (tv-sec timer2))
        (if (%lt (tv-usec timer1) (tv-usec timer2))
            t
          ())
      ())))

;; get a timer object to describe the current time
;; since current time changes this object should not be stored to long
(defun get-this-time ()
  (gettime local-tp (%cast <object> #%i0))
  local-tp)

;; pause until a time according to a timer object
(%declare-external-function (c-pause %void) ()
  language C external-name |pause|)

(defun sleep-until (timer)
  (gettime local-tp (%cast <object> #%i0))
  (let ((sec  (%minus (tv-sec  timer) (tv-sec  local-tp)))
        (usec (%minus (tv-usec timer) (tv-usec local-tp))))
    (when (%lt usec #%i0)
          (setq usec (%plus usec #%i1000000))
          (setq sec  (%minus sec #%i1)))
    (if (%gt sec #%i0)
        (progn (set-timer 'real sec usec) (c-pause))
      (busy-wait timer))
    ()))

(defun busy-wait (timer)
  (if (time-lt (get-this-time) timer)
      (busy-wait timer)
    ()))

;;;-----------------------------------------------------------------------------
)  ;; End of module event-i
;;;-----------------------------------------------------------------------------
