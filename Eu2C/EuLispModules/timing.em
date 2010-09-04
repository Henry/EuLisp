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
;;;  Title: timer interface routines
;;;  Authors: E. Ulrich Kriegel
;;;-----------------------------------------------------------------------------

(defmodule timing
  (import (level-0x)
   syntax (level-0x)
   c-import ("timing.h")
   export (start-timer timer xalloc-info time))

(%declare-external-function (start-timer <c.long>)
  ()
  language C
  external-name |start_timer|)

(%declare-external-function (timer <c.long>)
  ((s <c.string>))
  language C
  external-name |timer|)

(%declare-external-function (xalloc-info <c.void>)
  ()
  language C
  external-name |xalloc_info|)

(defmacro time (form . messg)
  (let ((std-fmt "\nuser time %.2f system time %.2f sum %.2f"))
    `(let (res)
       (if (,%eq (,start-timer) (%literal ,<c.long> 0))
           (progn
             (setq res ,form)
             (if (,%eq (,timer ,(if (null? messg)
                                    `(,c.string ,std-fmt)
                                  `(,c.string ,(car messg))))
                       (%literal ,<c.long> 0))
                 res
               ()))
         ()))))

;;;-----------------------------------------------------------------------------
)
;;;-----------------------------------------------------------------------------
