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
;;;  Title: very basic condition and error signalling facility
;;;-----------------------------------------------------------------------------

(defmodule basic-condition
  (import (%tail
           apply-level-1
           string-i)
   syntax (%tail )
   c-import (<stdio.h>
             <stdlib.h>)
   export (error
           cerror
           typecheck-error
           no-applicable-method-error
           $<no-applicable-method>
           $<typecheck-error>))

;;;-----------------------------------------------------------------------------
;;; declare external functions to realize error
;;;-----------------------------------------------------------------------------
(%declare-external-function (c-exit %void)
  ((status %signed-word-integer))
  language C
  external-name |exit|)

(%declare-external-function (c-puts %signed-word-integer)
  ((status %string))
  language C
  external-name |puts|)

;;;-----------------------------------------------------------------------------
;;; define basic error and bind error and cerror
;;;-----------------------------------------------------------------------------
(defun basic-error (condition-class message . init-args)
  (c-puts (%literal %string () "Simple Error signalled:"))
  (c-puts (string-pointer message))
  (c-puts (%literal %string () "->exit 2"))
  (c-exit #%i2)
  ;; to make type inference happy
  message)

(deflocal error basic-error)
(deflocal cerror basic-error)

;;;-----------------------------------------------------------------------------
;;; define constants for conditions
;;;-----------------------------------------------------------------------------
(deflocal $<no-applicable-method> ())
(deflocal $<typecheck-error> ())

;;;-----------------------------------------------------------------------------
;;; define functions calling error
;;;-----------------------------------------------------------------------------
(defun no-applicable-method-error (gf . args)
  (error $<no-applicable-method> "No applicable Methods found"
         'generic gf
         'arguments args)
  ;; to make type-inference happy
  gf)

(defun typecheck-error (object class-list)
  (error $<typecheck-error> "Typecheck-Error"
         'object object
         'class-list class-list))

;;;-----------------------------------------------------------------------------
)  ;; End of module basic-condition
;;;-----------------------------------------------------------------------------
