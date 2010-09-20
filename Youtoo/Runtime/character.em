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
;;;  Library: level1
;;;  Authors: Andreas Kind, Julian Padget
;;; Description: characters
;;;-----------------------------------------------------------------------------
(defmodule character
  (syntax (_telos0)
   import (telos compare collect string)
   export (<character> character? uppercase? lowercase?
                       alphap alnump digit? graph?
                       as-lowercase as-uppercase
                       character-as-string))

;;;-----------------------------------------------------------------------------
;;; Class <character>
;;;-----------------------------------------------------------------------------
(defprimclass <character> character-class (<object>) ())

;;;-----------------------------------------------------------------------------
;;; Comparison
;;;-----------------------------------------------------------------------------
(defmethod binary< ((c1 <character>) (c2 <character>))
  (cond
    ((and (uppercase? c1) (uppercase? c2))
     (int-binary< (character-as-int c1) (character-as-int c2)))
    ((and (lowercase? c1) (lowercase? c2))
     (int-binary< (character-as-int c1) (character-as-int c2)))
    ((and (digit? c1) (digit? c2))
     (int-binary< (character-as-int c1) (character-as-int c2)))
    (t ())))

;;;-----------------------------------------------------------------------------
;;; Upper-/lower case
;;;-----------------------------------------------------------------------------
(defgeneric as-lowercase (x))
(defgeneric as-uppercase (x))
(defmethod as-lowercase ((c <character>)) (character-as-lowercase c))
(defmethod as-uppercase ((c <character>)) (character-as-uppercase c))

(defun uppercase? (c) (and (isupper c) c))
(defun lowercase? (c) (and (islower c) c))
(defun alnump     (c) (and (isalnum c) c))
(defun alphap     (c) (and (isalpha c) c))
(defun digit?     (c) (and (isdigit c) c))
(defun graph?     (c) (and (isgraph c) c))

(defextern character-as-lowercase (<character>) <character> "tolower")
(defextern character-as-uppercase (<character>) <character> "toupper")
(defextern isalpha   (<character>) boolean)
(defextern isalnum   (<character>) boolean)
(defextern isupper   (<character>) boolean)
(defextern islower   (<character>) boolean)
(defextern isdigit   (<character>) boolean)
(defextern isgraph   (<character>) boolean)

;;;-----------------------------------------------------------------------------
;;; Conversion
;;;-----------------------------------------------------------------------------
(defgeneric (converter <character>) (x))

(defun character-as-string (x)
  (make <string> size: 1 fill-value: x))

;;;-----------------------------------------------------------------------------
)  ;; End of module
;;;-----------------------------------------------------------------------------
