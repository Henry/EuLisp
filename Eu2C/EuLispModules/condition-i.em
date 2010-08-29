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
;;;  Title: eulisp conditions
;;;  Description:
;;    Provides condition classes and a function most-saimple-error-handler which
;;    should work above apply-level-1
;;    most-simple-error-handler is called with 2 $strings, message and name-of-condition-class
;;;  Notes:
;;    $make-function contains a dummy make which in fact is only an allocate
;;    must be reset as soon as possible to the original value of make
;;    $signal-function contains a very simple version of signal which calls the
;;    function bound to $default-signal-handler
;;    as soon as dynamic is available, $signal-function has to be reset to genuine
;;    signal calling function bound on $dynamic-default-signal-handler
;;;  Problems:
;;    tested in feel, does work for one-threaded applications only
;;    for multiple threaded apps handler must be explicitely established
;;    and deestablished, respectively
;;    0.99 specifies not all conditions, no description is given for
;;    <no-converter>
;;    <stream-condition>
;;    some conditions are only given in text, not in the condition index
;;    J. Dalton signal returns: signal static does nothing because there is no mean to
;;    catch signals at that level
;;;  Authors: E. Ulrich Kriegel
;;;-----------------------------------------------------------------------------

(defmodule condition-i
  (import (tail
           (only (function-address)
                 function-i)
           basic-condition)
   syntax (tail
           syntax-0
           condition-ii)
   c-import (<stdlib.h>
             <stdio.h>
             <signal.h>
             "c-runtime.h")
   export (<condition>
           <execution-condition>
           <typecheck-error>
           <telos-condition>
           <no-applicable-method>
           <arithmetic-condition>
           ;;functions and macros
           c-exit
           signal ; now set to a faked signal
           condition-message)
   expose ((only (error)
                 basic-condition)))

;;;-----------------------------------------------------------------------------
;;; definition of condition class
;;;-----------------------------------------------------------------------------
(%define-standard-class (<condition> <class>)
  <object>
  ((message type <object> default () keyword message
            accessor condition-message)
   (continuation type <object>  default () keyword
                 continuation
                 accessor continuation ))
  predicate conditionp
  representation pointer-to-struct
  constructor (make-condition )
  allocation multiple-type-card)

;;;-----------------------------------------------------------------------------
;;; <execution-condition>
;;;-----------------------------------------------------------------------------
(define-condition-class <execution-condition> <condition> )

(%define-standard-class (<typecheck-error> <class>)
  <execution-condition>
  ((object type <object> keyword object accessor object)
   (class-list type <list> keyword class-list accessor class-list))
  allocation multiple-type-card
  representation pointer-to-struct)
(setq $<typecheck-error> <typecheck-error>)

;;;-----------------------------------------------------------------------------
;;; <arithmetic-condition>
;;;-----------------------------------------------------------------------------
(%define-standard-class (<arithmetic-condition> <class> )
  <condition>
  ((operator type <object> default () keyword operator accessor operator)
   (operand-list type <list>
                 default ()
                 keyword operand-list
                 accessor operand-list))
  representation pointer-to-struct
  allocation multiple-type-card)

;;;-----------------------------------------------------------------------------
;;; <telos-condition>
;;;-----------------------------------------------------------------------------
(define-condition-class <telos-condition> <condition> )

(%define-standard-class (<no-applicable-method> <class> )
  <telos-condition>
  ((generic type <object> default ()
            keyword generic accessor generic)
   (arguments type <object> default ()
              keyword arguments accessor arguments))
  representation pointer-to-struct
  allocation multiple-type-card)
(setq $<no-applicable-method> <no-applicable-method>)

;;;-----------------------------------------------------------------------------
;;;External function for primitive i/o and exit
;;;-----------------------------------------------------------------------------
(%declare-external-function (c-exit %void)
  ((status %signed-word-integer))
  language C
  external-name |exit|)

(%declare-external-function (c-puts %void)
  ((str %string))
  language C
  external-name |puts|)

;;;-----------------------------------------------------------------------------
;;; define primitive templates for signal and make
;;;-----------------------------------------------------------------------------
(defun %allocate (class)
  (%funcall (%class-allocator class)))

(defun error-handler-with-puts (condition continuation)
  (c-puts (string-pointer (condition-message condition)))
  (c-puts (%literal %string () "Sorry, but this is the end ;-)"))
  (c-exit #%i2)
  ())


(defun signal-static (condition continuation . thread)
  ;;threads not yet handled
  )

(deflocal signal signal-static)

(%declare-external-function (c-signal %sighandler)
  ((signal %signed-word-integer)
   (handler %sighandler))
  language C
  external-name |signal|)

(%declare-external-variable fpe-signal-code
  %signed-word-integer language C
  external-name |SIGFPE|)
(deflocal arithmetic-condition-instance (%allocate <arithmetic-condition>))

(%define-function (apply-fpe-handler %signed-word-integer)
  ((sig %signed-word-integer))
  ;;everytime, a FPE is signalled, this signal handler from C calls the
  ;;corresponding EuLisp signal handler with an instance of
  ;;<arithmetic-exception> and without thread and continuation
  ;;at first, signal has to be reestablished
  (c-signal fpe-signal-code
            (%cast %sighandler (function-address apply-fpe-handler)))
  ((setter condition-message) arithmetic-condition-instance
   "Floating Point Exception")
  (signal arithmetic-condition-instance ())
  ;; returning sig to make type-inference happy
  sig)

;;;-----------------------------------------------------------------------------
;;; Specification signals which has to be handled by c-signal and the
;; transformed to a signal call
;; IMHO, there should be a means that generated C code will use the actual
;; signal masks from <signal.h>
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;; Establish signal handler
;;;-----------------------------------------------------------------------------
(c-signal fpe-signal-code
          (%cast %sighandler (function-address apply-fpe-handler)))

;;;-----------------------------------------------------------------------------
;;; Error
;;;-----------------------------------------------------------------------------
(defun error-with-signal-static (error-message condition-class . init-args)
  (let ((condition (%allocate  condition-class)))
    ((setter condition-message) condition error-message)
    (signal-static condition ())
    (error-handler-with-puts condition ())))

(setq error error-with-signal-static)

;;;-----------------------------------------------------------------------------
)
;;;-----------------------------------------------------------------------------
