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
;;; Title: EuLisp Level-0 module collection
;;;  Description:
;;    Collection gives the functionality described in A.2.
;;    Collection consist of list, string, vector and table.
;;;  Authors: Winfried Heicking
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule collection
  (import (tail
           eulisp-kernel
           (only (collection?
                  sequence?)
                 collection-generic)
           (only (<vector>)
                 vector)
           (only (<string>)
                 string)
           (only (<table>)
                 collection-table))
   syntax (tail)
   export (collection?
           sequence?)
   expose (collection-list
           collection-string
           collection-vector
           collection-table
           collection-convert))

(defmethod collection? ((object <table>)) t)

(defmethod collection? ((object <string>)) t)

(defmethod collection? ((object <list>)) t)

(defmethod collection? ((object <vector>)) t)

(defmethod collection? (object) ())

(defmethod sequence? ((object <string>)) t)

(defmethod sequence? ((object <list>)) t)

(defmethod sequence? ((object <vector>)) t)

(defmethod sequence? (object) ())

;;;-----------------------------------------------------------------------------
) ;; End of module collection
;;;-----------------------------------------------------------------------------
