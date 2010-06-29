;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
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
  (defclass <float> (<number>)
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
  )  ;; end of module
;;;-----------------------------------------------------------------------------
