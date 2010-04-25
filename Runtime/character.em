;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: level1
;;;  Authors: Andreas Kind, Julian Padget
;;; Description: characters
;;;-----------------------------------------------------------------------------
(defmodule character
  (syntax (_telos0)
   import (telos compare collect string)
   export (<character> characterp uppercasep lowercasep
           alphap alnump digitp graphp
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
     ((and (uppercasep c1) (uppercasep c2))
      (int-binary< (character-as-int c1) (character-as-int c2)))
     ((and (lowercasep c1) (lowercasep c2))
      (int-binary< (character-as-int c1) (character-as-int c2)))
     ((and (digitp c1) (digitp c2))
      (int-binary< (character-as-int c1) (character-as-int c2)))
     (t ())))

;;;-----------------------------------------------------------------------------
;;; Upper-/lower case
;;;-----------------------------------------------------------------------------
  (defgeneric as-lowercase (x))
  (defgeneric as-uppercase (x))
  (defmethod as-lowercase ((c <character>)) (character-as-lowercase c))
  (defmethod as-uppercase ((c <character>)) (character-as-uppercase c))

  (defun uppercasep (c) (and (isupper c) c))
  (defun lowercasep (c) (and (islower c) c))
  (defun alnump     (c) (and (isalnum c) c))
  (defun alphap     (c) (and (isalpha c) c))
  (defun digitp     (c) (and (isdigit c) c))
  (defun graphp     (c) (and (isgraph c) c))

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
  )  ;; end of module
;;;-----------------------------------------------------------------------------