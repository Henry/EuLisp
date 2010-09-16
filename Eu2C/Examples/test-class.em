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
;;; Title: Example 'test-class'
;;;  Description:
;;    Basic tests of class construction, initialisation and slot access.
;;;  Authors: Henry G. Weller
;;;-----------------------------------------------------------------------------
(defmodule test-class
  (import (level-0
           (only (<pointer-to-void>)
                 pointer-to-void))
   syntax (level-0
           ))

(defclass <tower> <object>
  (
   ;;(hmm reader hmm keyword hmm default 10)
   (id reader tower-id keyword id)
   (blocks accessor tower-blocks)
   )
  )

;(deflocal str (make <string> characters "hmm"))
;(initialize str 'characters "hmm")
;(deflocal str (make-string "hmm"))
;(print (string-pointer str))

(deflocal hmm1 (make <tower>))
;; (hmm hmm1)
;; (print (hmm hmm1))

;; (deflocal hqz 2)
;; (print hqz)

;; (deflocal hqz: 1)
;; (print hqz:)

;;;-----------------------------------------------------------------------------
)  ;; end of module
;;;-----------------------------------------------------------------------------
