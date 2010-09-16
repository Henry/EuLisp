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
;;; Title:
;;;  Description:
;;;  Authors: Rainer Rosenmuller
;;;-----------------------------------------------------------------------------

(defmodule stream-generic
  (import (tail
           apply
           standard-generic-function
           (only (<stream>)
                 stream-ii))
   syntax (tail)
   export (close
           flush
           stream-position
           read-char
           putback-char
           output
           read-line
           generic-print
           generic-write))

(defgeneric close ((stream <stream>)))
(defgeneric flush ((stream <stream>)))
(defgeneric stream-position ((stream <stream>)))
(defgeneric (setter stream-position) ((stream <stream>) (object <object>)))
(defgeneric read-char ((stream <stream>)))
(defgeneric putback-char ((stream <stream>) (object <object>)))
(defgeneric output ((stream <stream>) (object <object>)))
(defgeneric read-line ((stream <stream>)))
(defgeneric generic-print ((object <object>) (stream <stream>)))
(defgeneric generic-write ((object <object>) (stream <stream>)))

;;;-----------------------------------------------------------------------------
;;; Type schemes for type inference
;;;-----------------------------------------------------------------------------
(%annotate-function
  close new-signature
  (((var0 var1)
    ((var var0) (atom? <object>))
    ((var var1) (atom? <stream>)))))

(%annotate-function
  flush new-signature
  (((var0 var1)
    ((var var0) (atom? <object>))
    ((var var1) (atom? <stream>)))))

(%annotate-function
  stream-position new-signature
  (((var0 var1)
    ((var var0) (atom? (or <null> <integer>)))
    ((var var1) (atom? <stream>)))))

(%annotate-function
  output new-signature
  (((var0 var1 var2)
    ((var var0) (atom? <object>))
    ((var var1) (atom? <stream>))
    ((var var2) (var var0)))))

;; read-line needs <string> ! Moved to stream-i.am.

(%annotate-function
  generic-write new-signature
  (((var0 var1 var2)
    ((var var0) (atom? <object>))
    ((var var1) (var var0))
    ((var var2) (atom? <stream>)))))

(%annotate-function
  generic-print new-signature
  (((var0 var1 var2)
    ((var var0) (atom? <object>))
    ((var var1) (var var0))
    ((var var2) (atom? <stream>)))))

;;;-----------------------------------------------------------------------------
)  ;; End of module stream-generic
;;;-----------------------------------------------------------------------------
