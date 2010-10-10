;;; Copyright 1995 Tracy Gardner & University of Bath
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
;;; Title: Definitions for main ops5 module
;;;  Library: ops5
;;;  Authors: Tracy Gardner
;;;  Maintainer: Henry G. Weller
;;;  Description:
;;    OPS5 for EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------

(defmodule ops5-def
  (syntax (syntax-0
           macros-tag)
   import (level-0
           basic
           cond-el-gf
           wm
           conflict)
   export (ce-manager
           set-ce-manager
           cr-manager
           set-cr-manager
           wm-manager
           set-wm-manager
           <ops5-system>))

(print "### ops5-def" nl)

(defclass <ops5-system> ()
  ((ce-manager
    reader:  ce-manager
    writer:  set-ce-manager)
   (cr-manager
    default: (make-cr-manager 'mea)
    reader:  cr-manager
    writer:  set-cr-manager)
   (wm-manager
    default: (make-wm-manager)
    reader:  wm-manager
    writer:  set-wm-manager)))

;;;-----------------------------------------------------------------------------
)  ;; End of module ops5-def
;;;-----------------------------------------------------------------------------
