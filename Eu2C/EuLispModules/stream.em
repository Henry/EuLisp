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
;;;  Title:
;;;  Description:
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Rainer Rosenmuller
;;;-----------------------------------------------------------------------------
(defmodule stream

  (import
   ;;------
   (tail stream-i read print)

   syntax
   ;;------
   (tail)

   export
   ;;------
   (
    ;; <stream-properties> <stream-direction> ; nicht in el0.99
    <stream>
    <file-stream> ; nicht in el0.99
    <char-file-stream>
    <string-stream>
    ;; end-of-stream s.o. condition
    ;; converter to string s.o.
    convert-stream-string ; fur converter to string
    ;; <input-stream> <output-stream> ; nicht in el0.99
    ;; <io-stream> <stream-unit>  ; nicht in el0.99
    ;; input-stream output-stream ; nicht in el0.99
    read ; nicht in el0.99
    prin open close
    streamp file-stream-p
    input-stream-p output-stream-p ; nicht in el0.99
    ;; io-stream-p  ; nicht in el0.99
    character-stream-p
    ;; binary-stream-p  ; nicht in el0.99
    open-p ; nicht in el0.99
    flush print
    ;; write-unit read-unit peek-unit ; nicht in el0.99
    standard-input-stream standard-output-stream
    standard-error-stream
    stream-position
    set-stream-position ; nicht in el0.99, aber fur setter stream-position
    end-of-stream-p input uninput read-line
    output newline write
    <stream-condition>
    <end-of-stream>
    <inappropriate-stream-position>
    make-string-input-stream make-string-output-stream
    converter
    convert
    <syntax-error>
    )
   )

;;;-----------------------------------------------------------------------------
;;; Type schemes for type inference
;;;-----------------------------------------------------------------------------

;; still to annotate:

(%annotate-function
  streamp new-signature
  (((var0 var1)
    ((var var0) (atom (and <object> (not <null>))))
    ((var var1) (atom <stream>)))
   ((var0 var1)
    ((var var0) (atom <null>))
    ((var var1) (atom (and <object> (not <stream>)))))))

(%annotate-function
  file-stream-p new-signature
  (((var0 var1)
    ((var var0) (atom (and <object> (not <null>))))
    ((var var1) (atom <file-stream>)))
   ((var0 var1)
    ((var var0) (atom <null>))
    ((var var1) (atom (and <object> (not <file-stream>)))))))

;;(%annotate-function
;; character-stream-p new-signature
;; (((var0 var1)
;;   ((var var0) (atom (and <object> (not <null>))))
;;   ((var var1) (atom <character-stream>)))
;;  ((var0 var1)
;;   ((var var0) (atom <null>))
;;   ((var var1) (atom (and <object> (not <character-stream>)))))))


)
