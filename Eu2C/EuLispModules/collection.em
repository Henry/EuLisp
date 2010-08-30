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
;;;  Title: collection consist of list, string, vector, table,
;;;  Description: collection gives the functionality described in A.2
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Winfried Heicking
;;;-----------------------------------------------------------------------------

(defmodule collection
  (import (tail
           eulisp-kernel
           (only (collectionp
                  sequencep)
                 collection-generic)
           (only (<vector>)
                 vector)
           (only (<string>)
                 string)
           (only (<table>)
                 collection-table))
   syntax (tail)
   export (collectionp
           sequencep)
   expose (collection-list
           collection-string
           collection-vector
           collection-table
           collection-convert))

(defmethod collectionp ((object <table>)) t)

(defmethod collectionp ((object <string>)) t)

(defmethod collectionp ((object <list>)) t)

(defmethod collectionp ((object <vector>)) t)

(defmethod collectionp (object) ())

(defmethod sequencep ((object <string>)) t)

(defmethod sequencep ((object <list>)) t)

(defmethod sequencep ((object <vector>)) t)

(defmethod sequencep (object) ())

;;;-----------------------------------------------------------------------------
)  ;; end of module
;;;-----------------------------------------------------------------------------
