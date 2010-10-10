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
;;; Title: Test object streams
;;;  Library: serial
;;;  Authors: Andreas Kind
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule test3
  (syntax (syntax-0)
   import (level-0
           serial))

(defmethod generic-connect ((s1 <stream>) (s2 <stream>) options)
  (let ((mode (init-list-ref options mode: 'r)))
    (cond ((eq mode 'r)
           ((setter stream-source) s1 s2))
          ((eq mode 'w)
           ((setter stream-sink) s1 s2))
          ((eq mode 'a)
           ((setter stream-sink) s1 s2))
          (t
           ((setter stream-source) s1 s2)
           ((setter stream-sink) s1 s2)))
    ()))

(defun my-deserialize ()
  (let ((x (deserialize)))
    (spprint x stderr)
    (cond ((generic-function? x)
           (x 42 "abc")
           (x 1.23 'foo))
          ((function? x)
           (print (x 1 2 3 4) nl)
           ())
          ((thread? x)
           (thread-reschedule x)))))

(my-deserialize)

;;;-----------------------------------------------------------------------------
)  ;; End of module test3
;;;-----------------------------------------------------------------------------
