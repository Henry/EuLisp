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
;;;  Authors: Rainer Rosenmuller
;;;-----------------------------------------------------------------------------

(defmodule stream
  (import (tail
           stream-i
           read
           print)
   syntax (tail)
   export (<stream>
           <file-stream>
           <char-file-stream>
           <string-stream>
           convert-stream-string
           read
           prin
           open
           close
           stream?
           file-stream?
           input-stream?
           output-stream?
           character-stream?
           open?
           flush
           print
           stream-position
           set-stream-position
           end-of-stream?
           read-char
           putback-char
           read-line
           output
           newline write
           <stream-condition>
           <end-of-stream>
           <inappropriate-stream-position>
           make-string-input-stream
           make-string-output-stream
           converter
           convert
           <read-error>
           stdin
           stdout
           stderr))

;;;-----------------------------------------------------------------------------
;;; Type schemes for type inference
;;;-----------------------------------------------------------------------------
(%annotate-function
  stream? new-signature
  (((var0 var1)
    ((var var0) (atom? (and <object> (not <null>))))
    ((var var1) (atom? <stream>)))
   ((var0 var1)
    ((var var0) (atom? <null>))
    ((var var1) (atom? (and <object> (not <stream>)))))))

(%annotate-function
  file-stream? new-signature
  (((var0 var1)
    ((var var0) (atom? (and <object> (not <null>))))
    ((var var1) (atom? <file-stream>)))
   ((var0 var1)
    ((var var0) (atom? <null>))
    ((var var1) (atom? (and <object> (not <file-stream>)))))))

;;;-----------------------------------------------------------------------------
)  ;; End of modules stream
;;;-----------------------------------------------------------------------------
