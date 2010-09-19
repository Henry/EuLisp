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
;;; Title: introspection functions
;;;  Notes:
;;    %class-of must be changed with introduction of static cards
;;;  Problems:
;;    %class-of explicitely assumed that it is called for valid lisp data only
;;;  Authors: E. Ulrich Kriegel
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------
(defmodule tail-introspection
  (import ((rename ((%class <class>)
                    (%object <object>)
                    (%tail-class <tail-class>))
                   %tail)
           (only (typecheck-error) basic-condition)
           ti-sys-signatures     ;; this allows declaration of signatures
           (only (LowerHeapLimit
                  UpperHeapLimit
                  WordLength
                  object-class
                  object-size) mm-interface)
           pointer-to-void
           basic-list-0
           basic-number)
   syntax (%tail)
   export (%class-of
           %instance-of?
           %subclass?
           typecheck
           %vector-class-instance-size))

;;;-----------------------------------------------------------------------------
;;; Define accessors for static information
;;;-----------------------------------------------------------------------------
(%define-tail-class (<static-object-tag>  <tail-class>)
  ((tag type <class> accessor static-class-of))
  representation pointer-to-struct
  allocation single-card)

(%define-tail-class (<static-vector-size>  <tail-class>)
  ((length type %unsigned-word-integer
           accessor static-vector-size))
  representation pointer-to-struct
  allocation single-card)

;;;-----------------------------------------------------------------------------
;;; Do the job
;;;-----------------------------------------------------------------------------
#-(:int :big)
(%define-function (%class-of <class> )
  ((obj <object>))
  (if (%gt (%cast %unsigned-word-integer obj)
           (%cast %unsigned-word-integer UpperHeapLimit))
      ;; neg fixnum
      <int>
    (if (%gt (%literal %signed-word-integer #x10000)
             (%cast %signed-word-integer obj))
        ;; pos fixnum
        <int>
      (if (%gt (%cast %signed-word-integer obj)
               (%cast %signed-word-integer LowerHeapLimit))
          ;; dynamic objects
          (%cast <class> (object-class obj))
        (static-class-of
         (%cast <static-object-tag>
                (%minus (%cast %unsigned-word-integer obj)
                        (%cast %unsigned-word-integer WordLength))))))))

#+(:int :big)
(%define-function (%class-of <class> )
  ((obj <object>))
  (if (%eq (%and #%i1 (%cast %signed-word-integer obj)) #%i1)
      <int>
    (if (%gt (%cast %signed-word-integer obj)
             (%cast %signed-word-integer LowerHeapLimit))
        ;; dynamic object
        (%cast <class> (object-class obj))
      (static-class-of
       (%cast <static-object-tag>
              (%minus (%cast %unsigned-word-integer obj)
                      (%cast %unsigned-word-integer WordLength)))))))

(%define-function (%vector-class-instance-size %unsigned-word-integer)
  ((obj <object>))
  (if (%gt (%cast %signed-word-integer obj)
           (%cast %signed-word-integer LowerHeapLimit))
      ;; dynamic allocated vector class instance
      (%cast %unsigned-word-integer
             (object-size (%cast <pointer-to-void> obj)))
    (static-vector-size
     (%cast <static-vector-size>
            (%minus (%cast %unsigned-word-integer obj)
                    #%I8)))))

(%define-function (%instance-of? <object>)
  ((instance <object>)
   (class <class>))
  (%let ((class-of-instance <class> (%class-of instance)))
        (if (%eq (%cast %unsigned-word-integer
                        class-of-instance)
                 (%cast %unsigned-word-integer class))
            't ; old: instance
          (if (%member class (%select class-of-instance
                                      <class>
                                      class-precedence-list))
              't ; old: instance
            ())
          )))

(%define-function (%subclass? <object>)
  ((c1 <class>)
   (c2 <class>))
  (if (%member c2 (%select c1 <class> class-precedence-list))
      't
    ()))

(%annotate-function %subclass? is-special-function subclass?)

;;;-----------------------------------------------------------------------------
;;; Type Check
;;;-----------------------------------------------------------------------------
(%define-function (typecheck <object>)
  ((object <object>)
   (class-list <list>))
  (typecheck1 (%class-of object) object class-list))

(%define-function (typecheck1 <object>)
  ((class <class>)
   (object <object>)
   (class-list <list>))
  (if (null? class-list)
      (typecheck-error object class-list)
    (if (%subclass? class (car class-list))
        object
      (typecheck1 class object (cdr class-list)))))

;;;-----------------------------------------------------------------------------
)
;;;-----------------------------------------------------------------------------
