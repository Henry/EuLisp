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
;;; Description: floating point numbers
;;;-----------------------------------------------------------------------------
(defmodule float
  (syntax (_telos0)
   import (telos number)
   export (<float> float? <double-float> <double> double?
                   ceiling floor round truncate))

;;;-----------------------------------------------------------------------------
;;; Class <float> and <double-float>
;;;-----------------------------------------------------------------------------
(defclass <float> <number>
  () abstract?: t predicate: float?)

(defprimclass <double> double-class (<float>) ()
              predicate: double?)

(defconstant <double-float> <double>)

;;;-----------------------------------------------------------------------------
;;; Around floats
;;;-----------------------------------------------------------------------------
(defgeneric ceiling ((x <float>)))
(defgeneric floor ((x <float>)))
(defgeneric round ((x <float>)))
(defgeneric truncate ((x <float>)))
(defgeneric (converter <double>) (x))

;;;-----------------------------------------------------------------------------
)  ;; End of module
;;;-----------------------------------------------------------------------------
