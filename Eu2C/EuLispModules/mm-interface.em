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
;;; Title: Interface to memory management system
;;;  Notes:
;;    <pointer-to-void> as defconstant represented, exclude when in %tail
;;;  Problems:
;;    %pointer-to-void
;;;  Authors: E. Ulrich Kriegel
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule mm-interface
  (import ((rename ((%class <class>)
                    (%object <object>)
                    (%tail-class <tail-class>)
                    )
                   %tail)
           (rename ((%unsigned-word-integer <card-descriptor>))
                   %tail)
           (rename ((%unsigned-word-integer <type-descriptor>))
                   %tail)
           (rename ((%unsigned-word-integer <pointer-to-trace-function>))
                   %tail)
           (rename ((<pointer-to-void> <void*>))
                   pointer-to-void))
   syntax (%tail)
   c-import ("xalloc_user.h" "c-runtime.h")
   export (;;allocation primitives
           allocate-on-single-card
           allocate-on-multiple-type-card
           allocate-on-multiple-size-card

           ;;dealing with descriptors
           make-type-descriptor ;generates tdsc
           make-card-descriptor ;generate cdsc
           set-type-descriptor  ;set tdsc
           set-card-descriptor  ;set cdsc

           ;;introspection
           object-class ; class of an object
           object-size  ; size of an object (in bytes)
           object-type-descriptor ;tdsc assigned to an object


           ;;handling of root set
           initialize-root-set
           add-object-to-root-set
           add-card-to-root-set
           delete-object-from-root-set
           delete-card-from-root-set
           garbage-collection

           ;;auxiliary
           decode-card-type

           ;;           ;;type definitions
           ;;           <card-descriptor>
           ;;           <type-descriptor>


           ;;external variables for describing heap region
           WordLength
           LowerHeapLimit
           UpperHeapLimit

           ;;constants for MTSS,STSS,STMS
           mtss
           stms
           stss


           ;;trace functions
           trace-pair ; traces a pair
           trace-pointer ; traces a pointer
           trace-nothing ; traces nothing
           trace-general-object ; traces a general object

           ;;functions setting mm-type and mm-card during initialitzation
           set-class-mm-type
           set-class-mm-card
           class-mm-type
           class-mm-card

           ;; end export
           ))

;;;-----------------------------------------------------------------------------
;;; Used datatypes
;;;-----------------------------------------------------------------------------
(%declare-external-class (<int*> <tail-class>) ()
  ()
  representation pointer-to-void
  language c
  type-identifier |int*|)

(%declare-external-class (<int**> <tail-class>) ()
  ()
  representation pointer-to-void
  language c
  type-identifier |int**|)

;;;-----------------------------------------------------------------------------
;;; provide max-used-card-descriptor and max-used-type-descriptor
;;;-----------------------------------------------------------------------------
(%provide-compiler-info
 ;; 2 is used for %string (see basic-string.am)
 max-used-type-descriptor 2
 max-used-card-descriptor 2
 )

;; now the compiler starts with these values after adding 1 when computing
;; type/card-descriptors statically

;;;-----------------------------------------------------------------------------
;;;
;;;-----------------------------------------------------------------------------
;; allocate chunks on single size single type cards
;; input card-descriptor
(%declare-external-variable WordLength %unsigned-byte-integer
  language c
  external-name |BYTES_PER_WORD|)

(%declare-external-variable LowerHeapLimit <int*>
  language c
  external-name |HEAPBEGIN|)

(%declare-external-variable UpperHeapLimit <int*>
  language C
  external-name |HEAPEND|)

;;MTSS,STMS,STSS
(%declare-external-variable mtss %signed-byte-integer
  language c
  external-name |MTSS|)

(%declare-external-variable stms %signed-byte-integer
  language c
  external-name |STMS|)
(%declare-external-variable stss %signed-byte-integer
  language c
  external-name |STSS|)

(%declare-external-function (allocate-on-single-card <void*>)
  ((card <card-descriptor>))
  language c
  external-name |xalloc_stss|)

;;allocate chunks on multiple type single size cards
;;input card and type-descriptor
(%declare-external-function (allocate-on-multiple-type-card  <void*>)
  ((card <card-descriptor>)
   (type <type-descriptor>))
  language c
  external-name |xalloc_mtss|)

;;allocate chunks on single size multiple type cards
;;input is card-descriptor and size
(%declare-external-function (allocate-on-multiple-size-card <void*>)
  ((card <card-descriptor>)
   (size %signed-word-integer))
  language c
  external-name |xalloc_stms|)


;;creates a type descriptor using user type and marking function
(%declare-external-function (make-type-descriptor <type-descriptor>)
  ((user-type %unsigned-word-integer)
   (mark-function %function))
  language c
  external-name |describe_type|)

;;creates a card descriptor with card type object size and type descriptor
(%declare-external-function (make-card-descriptor <card-descriptor>)
  ((card-type %signed-byte-integer)
   (object-size %signed-word-integer)
   (type-descriptor <type-descriptor>))
  language c
  external-name |describe_card|)

;;inscribe a type-descriptor in mm-bookkeeping system
(%declare-external-function (set-type-descriptor <type-descriptor>)
  ((type-descriptor <type-descriptor>)
   (user-type %unsigned-word-integer)
   (mark-function %function))
  language c
  external-name |set_type_descriptor|)
;; returns type descriptor assigned to an object

(%declare-external-function (object-type-descriptor <type-descriptor>)
  ((object <int*>))
  language c
  external-name |get_type_descriptor|)

;;inscribe a card-descriptor in mm bookkeeping system
(%declare-external-function (set-card-descriptor <card-descriptor>)
  ((card-descriptor <card-descriptor>)
   (card-type %signed-byte-integer)
   (object-size %signed-word-integer)
   (type-descriptor <type-descriptor>))
  language c
  external-name |set_card_descriptor|)

;;returns object type from object address
(%declare-external-function (object-class %unsigned-word-integer)
  ((pointer <object>))
  language c
  external-name |get_type|)

;;returns object size given object adress
(%declare-external-function (object-size %signed-word-integer)
  ;;%unsigned-word-integer)
  ((pointer <void*>))
  language c
  external-name |get_object_size|)

(%declare-external-function (initialize-root-set %void)
  ()
  language c
  external-name |initialize_root_set|)

(%declare-external-function (add-object-to-root-set %void)
  ((pointer <int**>))
  language c
  external-name |add_object_to_root_set|)

(%declare-external-function (add-card-to-root-set %void)
  ((pointer <int**>))
  language c
  external-name |add_card_to_root_set|)

(%declare-external-function (delete-object-from-root-set %void)
  ((pointer <int**>))
  language c
  external-name |delete_object_from_root_set|)

(%declare-external-function (delete-card-from-root-set %void)
  ((pointer <int**>))
  language c
  external-name |delete_card_from_root_set|)

(%declare-external-function (garbage-collection %void)
  ()
  language c
  external-name |force_garbage_collection|)

(%declare-external-function (decode-card-type %unsigned-byte-integer)
  ((card-descriptor <card-descriptor>))
  language c
  external-name |decode_card_type|)

;;trace functions
(%declare-external-function (trace-pair %void)
  ((ptr <void*>)
   (size %signed-word-integer))
  language c
  external-name |trace_pair|)
(%declare-external-function (trace-nothing %void)
  ((ptr <void*>)
   (size %signed-word-integer))
  language c
  external-name |trace_nothing|)

(%declare-external-function (trace-general-object %void)
  ((ptr <void*>)
   (size %signed-word-integer))
  language c
  external-name |trace_all|)
(%declare-external-function (trace-pointer %void)
  ((ptr <void*>))
  language c
  external-name |trace_pointer|)

;;;-----------------------------------------------------------------------------
;;; Interface to class initialization
;;;-----------------------------------------------------------------------------
(%define-function (set-class-mm-type %void)
  ((class <class>)
   (mm-type-val %unsigned-word-integer))
  (%setf (%select class <class> mm-type)
         (%cast %signed-word-integer mm-type-val)))

(%define-function (set-class-mm-card %void)
  ((class <class>)
   (mm-card-val %unsigned-word-integer))
  (%setf (%select class <class> mm-card)
         (%cast %signed-word-integer mm-card-val)))

(%define-function (class-mm-type %unsigned-word-integer)
  ((class <class>))
  (%cast %unsigned-word-integer
         (%select class <class> mm-type)))

(%define-function (class-mm-card %unsigned-word-integer)
  ((class <class>))
  (%cast %unsigned-word-integer
         (%select class <class> mm-card)))

;;;-----------------------------------------------------------------------------
;;; Init of c-runtime-system and set root-set
;;;-----------------------------------------------------------------------------
(%declare-external-function (c-runtime-init %void)
  ()
  language C
  external-name |c_runtime_init|)

(c-runtime-init)

;;;-----------------------------------------------------------------------------
;;; make links to the compiler, especially to the module mm-interface.em
;;;-----------------------------------------------------------------------------
(%annotate-function trace-pair
  is-special-function trace-pair)

(%annotate-function trace-pointer
  is-special-function trace-pointer)

(%annotate-function trace-nothing
  is-special-function trace-nothing)

(%annotate-function trace-general-object
  is-special-function trace-general-object)

(%annotate-function make-type-descriptor
  is-special-function make-type-descriptor)

(%annotate-function make-card-descriptor
  is-special-function make-card-descriptor)

(%annotate-function set-type-descriptor
  is-special-function set-type-descriptor)

(%annotate-function set-card-descriptor
  is-special-function set-card-descriptor)

(%annotate-function set-class-mm-type
  is-special-function set-class-mm-type)

(%annotate-function set-class-mm-card
  is-special-function set-class-mm-card)

(%annotate-function class-mm-type
  is-special-function class-mm-type)

(%annotate-function class-mm-card
  is-special-function class-mm-card)

(%annotate-function allocate-on-single-card
  is-special-function allocate-on-single-card)

(%annotate-function allocate-on-multiple-type-card
  is-special-function allocate-on-multiple-type-card)

(%annotate-function allocate-on-multiple-size-card
  is-special-function allocate-on-multiple-size-card)

(%annotate-binding mtss is-special-binding mtss)
(%annotate-binding stms is-special-binding stms)
(%annotate-binding stss is-special-binding stss)

(%annotate-class <void*> is-special-class <pointer-to-void>)

;;;-----------------------------------------------------------------------------
)  ;; End of module mm-interface
;;;-----------------------------------------------------------------------------
