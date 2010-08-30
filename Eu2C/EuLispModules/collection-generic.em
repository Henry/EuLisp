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
;;;  Description: definitions of defgeneric for collections
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Winfried Heicking
;;;-----------------------------------------------------------------------------

(defmodule collection-generic

  (import
   (tail
    standard-generic-function)

   syntax
   (tail
    syntax-0)

   export
   (accumulate
    accumulate1
    anyp
    collectionp
    concatenate
    do
    element
    ;;element1
    emptyp
    fill
    map
    member
    reverse
    sequencep
    size

    ;;now the aux methods
    construct-collection-info
    construct-result
    take-next-elt
    anyp-with-two-args
    concat-with-two-args
    do-with-two-args
    map-with-two-args
    )
   )

(defgeneric accumulate (function object collection))

(defgeneric accumulate1 (function collection))

(defgeneric anyp (function collection . more-collection))

(defgeneric collectionp (object))

(defgeneric concatenate (collection . more-collection))

(defgeneric do (function collection . more-collection))

(defgeneric element (collection key))

(defgeneric (setter element) ((collection <object>)
                              (key <object>)
                              (value <object>)))

(defgeneric emptyp (collection))

(defgeneric fill (collection object . keys))

(defgeneric map (function collection . more-collection))

(defgeneric member (object collection . test))

(defgeneric reverse (collection))

(defgeneric sequencep (object))

(defgeneric size (collection))

;;;aux methods

(defgeneric construct-collection-info (collection))

(defgeneric construct-result (collection result))

(defgeneric take-next-elt (element collection))

(defgeneric anyp-with-two-args (function collection1 collection2))

(defgeneric concat-with-two-args (collection1 collection2))

(defgeneric do-with-two-args (function collection1 collection2))

(defgeneric map-with-two-args (function collection1 collection2 result))

)