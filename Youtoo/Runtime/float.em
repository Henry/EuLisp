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
;;; Title: floating point numbers
;;;  Library: level-1
;;;  Authors: Andreas Kind, Julian Padget
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule float
  (syntax (_telos0)
   import (telos
           number)
   export (<float>
           float?
           <double-float>
           double-float?
           ceiling
           floor
           round
           truncate))

;;;-----------------------------------------------------------------------------
;;; Class <float> and <double-float>
;;;-----------------------------------------------------------------------------
(defclass <float> <number>
  () abstract?: t predicate: float?)

(defprimclass <double-float> double-float-class (<float>) ()
              predicate: double-float?)

;;;-----------------------------------------------------------------------------
;;; Around floats
;;;-----------------------------------------------------------------------------
(defgeneric ceiling ((x <float>)))
(defgeneric floor ((x <float>)))
(defgeneric round ((x <float>)))
(defgeneric truncate ((x <float>)))
(defgeneric (converter <double-float>) (x))

;;;-----------------------------------------------------------------------------
)  ;; End of module float
;;;-----------------------------------------------------------------------------
