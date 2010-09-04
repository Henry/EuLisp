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
;;;  Title: module for the definition of tail data types
;;;  Description:
;;    the definition functions for tail data types
;;;  Documentation:
;;    see in the APPLY-paper TAIL:eine getypte implementationssprache fuer APPLY
;;;  Notes:
;;;  Authors: Winfried Heicking, E. Ulrich Kriegel
;;;-----------------------------------------------------------------------------
;;;begin module whc-definitions

#module whc-definitions
(import (representation
         accessors
         level-1
         lzs
         mzs
         mm-initialize
         lzs-mop
         machine-description; for the whole machine-description
         debugging
         (only (case
                cadr
                dolist
                make-instance
                setf
                warn)
               common-lisp))
 syntax (level-1
         apply-standard
         debugging)
 export (%pointer
         %pointer-to-struct
         %pointer-to-vector))


(defmethod ?byte-length-as-component ((pobj <class-def>))
  (if (?representation pobj)
      (?byte-length-as-component1 (?representation pobj))
    (progn
      (cl:error "error ~a von <class-def> hat keine representation" pobj)
      4)))

(defmethod ?byte-length-as-component ((pobj <%representation>))
  (?byte-length-as-component1 pobj))

(defgeneric ?byte-length-as-component1 (pobj))

(defmethod ?byte-length-as-component1 ((pobj <%pointer>))
  (?byte-length (?representation %signed-word-integer))
  )

(defmethod ?byte-length-as-component1 ((pobj t))
  (?byte-length pobj)
  )


(defmethod ?byte-length-of-instance ((obj <%representation>))
  (?byte-length obj))

(defmethod ?byte-length-of-instance (obj)
  (cl:error "?byte-length-of representation called for a
non-representation-object ~s" obj))


;;;-----------------------------------------------------------------------------
;;; !!!!!
;;;-----------------------------------------------------------------------------
(defmethod ~compute-representation ((class-object <class-def>)
                                    representation-spec
                                    allocation-spec)
  ;;  ; representation-spec (symbol) = %pointer-to-struct | %pointer-to-void ...
  ;;  ; if representation-spec is () then take the value from the superclass
  (let ((representation-object
         (if representation-spec
             (funcall (case representation-spec
                            (es::pointer-to-struct #'make-pointer-to-struct)
                            (es::pointer-to-vector #'make-pointer-to-vector)
                            (es::pointer-to-void #'make-pointer-to-void)
                            (es::direct #'make-direct)
                            (t (lambda ()
                                 (warn "unknown representation: ~S " representation-spec))))
                      class-object )
           (copy-representation
            (~class-representation (cadr (~class-precedence-list
                                          class-object))) class-object ))))

    ;;the following step is necessary in order to solve a cycle with
    ;;class initialization and creation of constructors
    ;;set raw data in representation slots
    ;;~compute-runtime-initialization will  retrieve that values
    ;;and call mm-initialize
    (setf (?allocation representation-object) allocation-spec)
    representation-object))


(defgeneric copy-representation (representation class ))

(defmethod copy-representation
  ((representation <%pointer-to-struct>) class)
  (make-pointer-to-struct class ))

(defmethod copy-representation
  ((representation <%pointer-to-vector>) class)
  (make-pointer-to-vector class ))


(defmethod copy-representation
  ((representation <%pointer-to-void>) class)
  (make-pointer-to-void class ))

(defmethod copy-representation
  ((representation <%direct>) class)
  (make-direct class ))



(defun make-pointer-to-struct (class-obj)
  (let ((byte-length 0)
        (maximum-alignment 0))
    (dolist (slot-descr (~class-slot-descriptions class-obj))
            (when (> (?alignment (?representation (?type slot-descr)))
                     maximum-alignment)
                  (setq maximum-alignment (?alignment (?representation (?type
                                                                        slot-descr)))))
            (setq byte-length (+ byte-length (?byte-length-as-component (?representation (?type slot-descr))))))
    (make-instance
     <%pointer-to-struct>
     :byte-length byte-length
     :alignment maximum-alignment; *UK* 03.01.94
     )
    ))


(defun make-pointer-to-void (class-obj)
  (make-instance <%pointer-to-void>))


(defun make-direct (class-obj)
  (make-instance
   <%direct>
   :alignment (?alignment (?representation %unsigned-word-integer))
   :byte-length
   (?byte-length (?representation
                  (?type (car (~class-slot-descriptions class-obj)))))
   ))


(defun make-pointer-to-vector (class-obj)
  (let* ((length-slot (~vector-class-instance-length class-obj))
         (length  (if length-slot
                      length-slot
                    0)))
    (make-instance
     <%pointer-to-vector>
     :alignment (?alignment (?representation %signed-word-integer))
     :byte-length (compute-byte-length*
                   length
                   (?representation (~vector-class-element-type class-obj)))
     )))


(defgeneric compute-byte-length* (length representation))


(defmethod compute-byte-length* (length (representation <%pointer>))
  (* length (?byte-length (?representation %unsigned-word-integer))))


(defmethod compute-byte-length* (length (representation <%machine-type>))
  (* length (?byte-length representation)))



(defmethod compute-byte-length* ((length <null>) representation)
  ())

#module-end
