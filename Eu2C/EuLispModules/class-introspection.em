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
;;;  Authors: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule class-introspection
  (import (tail
           basic-string)
   syntax (basic-syntax)
   export (class-name
           class-precedence-list))

;;;-----------------------------------------------------------------------------
;;; class-name
;;;-----------------------------------------------------------------------------
(%define-function (class-name <string>) ((class <class>))
  (make-string (duplicate-%string (string-pointer
                                   (%select class <class> class-name)))))

;;;-----------------------------------------------------------------------------
;;; class-precedence-list
;;;-----------------------------------------------------------------------------
(%define-function (class-precedence-list <cons>)
  ((cl <class>))
  (%select cl <class> class-precedence-list))

;;;-----------------------------------------------------------------------------
)  ;; end of module class-introspection
;;;-----------------------------------------------------------------------------
