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
;;;  Title: interface to the c-part of threads
;;;  Description:
;;    interface to the lower part of threads (implemented in c)
;;    it is desired never to change this interface, since this migth require
;;    changes in thread, thread-i, lock and event
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Jens Bimberg
;;;-----------------------------------------------------------------------------
(defmodule thread-b
  (import ((only (%void <object> %function %signed-word-integer) tail))
   c-import ("thread.h")
   export (thread-schedule hold-threads cont-threads
                           m-thread-init m-thread-create m-thread-yield)
   )

(%declare-external-variable thread-schedule %signed-word-integer
  language c
  external-name |thread_schedule|)

(%declare-external-function (hold-threads %void)
  () language c external-name |DISABLE_SCHEDULE|)

(%declare-external-function (cont-threads %void)
  () language c external-name |ENABLE_SCHEDULE|)

(%declare-external-function (m-thread-init <object>)
  ()
  language c external-name |m_thread_init|)

(%declare-external-function (m-thread-create <object>)
  ((fun %function) (args <object>))
  language c external-name |m_thread_create|)

(%declare-external-function (m-thread-yield %void)
  ((m-thread <object>))
  language c external-name |m_thread_yield|)

)
