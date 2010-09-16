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
;;; Title: thread internals to be known by signal
;;;  Description:
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Jens Bimberg
;;;-----------------------------------------------------------------------------
(defmodule thread-ii
  (import
   (object-0 ;apply-level-1
    (only (hold-threads cont-threads) thread-b)
    (only (null? <list>) basic-list-0)
    (only (t) basic-list)
    (only (eq) basic-compare)
    (only (<function>) function-i)
    (only (<object> <class> %cast %void %instance-of? %pjmpbuf
                    %signed-word-integer) tail)
    (only (<dynamic> unwind stop-unwind-before continue-at
                     top-dynamic global-dynamic letcc-result) letcc)
    )
   syntax
   ((only (when unless) syntax-0)
    )
   export
   (<thread> thread? make-thread
             state value function args next waiters condqueue m-thread
             set-state set-value set-args set-next set-condqueue set-m-thread
             tmp-locks set-tmp-locks
             saved-dynamics save-dynamics restore-dynamics
             <T-LST> make-T-LST First Set-First Last Set-Last
             )
   )

(%define-standard-class (<T-LST> <class>)
  <object>
  ((First
    reader First writer set-First
    default ())
   (Last
    reader Last writer set-Last))
  constructor (make-T-LST)
  representation pointer-to-struct
  allocation multiple-type-card)

(%define-standard-class (<saved-dynamics> <class>)
  <object>
  ((s-unwind type %pjmpbuf
             reader s-unwind writer set-s-unwind
             default (%cast %pjmpbuf #%i0))
   (s-stop-unwind-before type %pjmpbuf
                         reader s-stop-unwind-before
                         writer set-s-stop-unwind-before
                         default (%cast %pjmpbuf #%i0))
   (s-continue-at type %pjmpbuf
                  reader s-continue-at writer set-s-continue-at
                  default (%cast %pjmpbuf #%i0))
   (s-top-dynamic type <dynamic>
                  reader s-top-dynamic writer set-s-top-dynamic
                  default global-dynamic)
   (s-letcc-result type <object>
                   reader s-letcc-result writer set-s-letcc-result
                   default (%cast <object> #%i0)))
  constructor (make-saved-dynamics)
  representation pointer-to-struct
  allocation multiple-type-card)

(%define-standard-class (<thread> <class>)
  <object>
  ((state
    reader state
    writer set-state
    default 'new)
   (value
    reader value writer set-value)
   (function type <function>
             reader function
             keyword init-function)
   (args
    reader args writer set-args)
   (next
    reader next writer set-next
    default ())
   (waiters type <T-LST>
            reader waiters writer set-waiters
            default (make-T-LST))
   (condqueue
    reader condqueue writer set-condqueue
    default ())
   (saved-dynamics type <saved-dynamics>
                   reader saved-dynamics
                   default (make-saved-dynamics))
   (tmp-locks
    reader tmp-locks writer set-tmp-locks
    default ())
   (m-thread
    reader m-thread writer set-m-thread))
  constructor (make-thread init-function)
  representation pointer-to-struct
  allocation multiple-type-card)

(defun thread? (obj) (%instance-of? obj <thread>))


(%define-function (save-dynamics %void) ((d <saved-dynamics>))
  (set-s-unwind             d unwind)
  (set-s-stop-unwind-before d stop-unwind-before)
  (set-s-continue-at        d continue-at)
  (set-s-top-dynamic        d top-dynamic)
  (set-s-letcc-result       d letcc-result))

(%define-function (restore-dynamics %void) ((d <saved-dynamics>))
  (setq unwind             (s-unwind             d))
  (setq stop-unwind-before (s-stop-unwind-before d))
  (setq continue-at        (s-continue-at        d))
  (setq top-dynamic        (s-top-dynamic        d))
  (setq letcc-result       (s-letcc-result       d)))
)
