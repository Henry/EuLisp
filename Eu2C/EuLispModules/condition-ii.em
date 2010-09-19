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
;;; Title: EuLisp Level-0 condition module
;;;  Description:
;;    Provides the <condition> superclass and the defcondition macro.
;;    defcondition corresponds to defclass with <condition> as the
;;    default superclass and the superclass checked to be a <condition>.
;;;  Authors: E. Ulrich Kriegel and Henry G. Weller
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule condition-ii
  (import (tail)
   syntax (tail)
   export (<condition>
           condition-message
           defcondition-error-string
           %subclass?))

;;;-----------------------------------------------------------------------------
;;; <condition>
;;;-----------------------------------------------------------------------------
(%define-standard-class (<condition> <class>)
  <object>
  ((message type <object>
            default ()
            keyword message:
            accessor condition-message)
   ;; (continuation type <object>
   ;;               default ()
   ;;               keyword continuation:
   ;;               accessor continuation)
   )
  predicate condition?
  representation pointer-to-struct
  allocation multiple-type-card)

;;;-----------------------------------------------------------------------------
;;; defcondition
;;;-----------------------------------------------------------------------------
(defconstant defcondition-error-string
  "Superclass in defcondition is not a subclass of <condition>")

(defmacro defcondition (condition-class-name
                        super-class-name
                        slots . class-options)
  `(progn (if (%subclass? ,(or super-class-name '<condition>) <condition>)
              ()
            (error <condition>
                   defcondition-error-string))
          (%define-standard-class
            (,condition-class-name <class>)
            ,(or super-class-name '<condition>)
            ,slots
            representation pointer-to-struct
            allocation multiple-type-card
            ,@class-options)))

;;;-----------------------------------------------------------------------------
)  ;; End of module condition-ii
;;;-----------------------------------------------------------------------------
