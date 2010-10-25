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
;;; Title: handler
;;;  Library: level1
;;;  Authors: Andreas Kind, Julian Padget
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule handler
  (syntax (_telos0)
   import (telos
           callback
           convert
           convert1
           fpi
           float
           string
           collect)
   export (<handler>
           handle handler?
           <int*>
           <double*>
           <string*>))

;;;-----------------------------------------------------------------------------
;;; Handle foreign C pointers
;;;-----------------------------------------------------------------------------
(defclass <handler> <object>
  ((handle accessor: handle keyword: handle:))
  predicate: handler?)

;;;-----------------------------------------------------------------------------
;;; Class <int*>
;;;-----------------------------------------------------------------------------
(defprimclass <int*> fpi-ref-class (<handler>) ()
              predicate: int*?)

(defgeneric (converter <int*>) (x))

(defmethod (converter <int*>) ((x <fpi>))
  (eul_fpi_as_eul_fpi_ref x))

(defmethod (converter <fpi>) ((x <int*>))
  (eul_fpi_ref_as_c_int x))

(defmethod element ((x <int*>) (i <fpi>))
  (eul_fpi_ref_ref x i))

(defmethod (setter element) ((x <int*>) (i <fpi>) y)
  (eul_fpi_ref_set x i y))

(defextern eul_fpi_as_eul_fpi_ref (ptr) ptr)
(defextern eul_fpi_ref_as_c_int (ptr) <fpi>)
(defextern eul_fpi_ref_ref (<int*> <fpi>) <fpi> "eul_c_vector_ref")
(defextern eul_fpi_ref_set (<int*> <fpi> <fpi>) <fpi> "eul_c_vector_set")

;;;-----------------------------------------------------------------------------
;;; Class <double*>
;;;-----------------------------------------------------------------------------
(defprimclass <double*> double-ref-class (<handler>) ()
              predicate: double*?)

(defgeneric (converter <double*>) (x))

(defmethod (converter <double*>) ((x <double>))
  (eul_double_as_eul_double_ref x))

(defmethod (converter <double>) ((x <double*>))
  (eul_double_ref_as_c_double x))

(defmethod element ((x <double*>) (i <fpi>))
  (eul_double_ref_ref x i))

(defmethod (setter element) ((x <double*>) (i <fpi>) y)
  (eul_double_ref_set x i y))

(defextern eul_double_as_eul_double_ref (ptr) ptr)
(defextern eul_double_ref_as_c_double (ptr) <double>)
(defextern eul_double_ref_ref (<double*> <fpi>) <double> "eul_c_vector_ref")
(defextern eul_double_ref_set (<double*> <fpi> <double>) <double>
           "eul_c_vector_set")

;;;-----------------------------------------------------------------------------
;;; Class <string*>
;;;-----------------------------------------------------------------------------
(defprimclass <string*> string-ref-class (<handler>) ()
              predicate: string*?)

(defgeneric (converter <string*>) (x))

(defmethod (converter <string*>) ((x <string>))
  (eul_string_as_eul_string_ref x))

(defmethod (converter <string>) ((x <string*>))
  (eul_string_ref_as_c_string x))

(defmethod element ((x <string*>) (i <fpi>))
  (eul_string_ref_ref x i))

(defmethod (setter element) ((x <string*>) (i <fpi>) y)
  (eul_string_ref_set x i y))

(defextern eul_string_as_eul_string_ref (ptr) ptr)
(defextern eul_string_ref_as_c_string (ptr) <string>)
(defextern eul_string_ref_ref (<string*> <fpi>) <string> "eul_c_vector_ref")
(defextern eul_string_ref_set (<string*> <fpi> <string>) <string>
           "eul_c_vector_set")

;;;-----------------------------------------------------------------------------
)  ;; End of module handler
;;;-----------------------------------------------------------------------------
