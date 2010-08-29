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
;;;-----------------------------------------------------------------------------

(defmodule c-interface
  (import (tail)
   syntax (tail)
   expose ((rename ((make-swi c.long)
                    (make-fpint lisp.fpint))
                   (only (make-swi make-fpint) basic-number))
           (rename ((make-string lisp.string)
                    (string-pointer c.string))
                   (only (make-string string-pointer) string-i))
           (rename ((%signed-word-integer <c.long>)
                    (%unsigned-word-integer <c.unsigned-long>)
                    (%signed-byte-integer <c.char>)
                    (%single-float <c.float>)
                    (%double-float <c.double>)
                    (%string <c.string>)
                    (%void <c.void>))
                   (only (%signed-word-integer
                          %unsigned-word-integer
                          %signed-byte-integer
                          %single-float
                          %double-float
                          %string
                          %void)
                         tail))
           (only (%cast
                  %funcall

                  %eq
                  %neq
                  %gt
                  %lt
                  %ge
                  %le

                  %plus
                  %minus
                  %neg
                  %mult
                  %div
                  %rem
                  %abs

                  %and
                  %or
                  %xor
                  %not

                  %citos
                  %citod
                  %cstoi
                  %cstod
                  %cdtoi
                  %cdtos
                  )
                 tail))

   ))
