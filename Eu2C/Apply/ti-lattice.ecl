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
;;;  Title: Lattice and Lattice Types Used for Type Inference
;;;  Description:
;;    Implementation of an complementary lattice with intersection (meet), union
;;    (join) and complement of lattice types. Lattice types are used in atomic type
;;    expressions (-> ti-exprs).
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Andreas Kind
;;;-----------------------------------------------------------------------------

#module ti-lattice
(import (lzs
         lzs-modules
         lzs-mop
         tail-module
         el2lzs-rules
         el2lzs-error
         expand-literal
         apply-funs
         ti
         ti-codes
         messages
         (only (acons
                assoc
                format
                sort
                delete
                mapc
                mapcar
                dolist
                append)
               common-lisp))
 syntax (ti
         el2lzs-main)
 export (<lattice-type>
         ?name ?code ?class ?supertypes ?subtypes
         ?strategic ?compound ?write-access-stamp ?atomic-expr
         <lattice-type-with-literals> ?literals
         <lattice> ?top ?bottom ?table ?latest-write-access-stamp
         *the-lattice* initialize-lattice
         *strategic-lattice-types*
         *top* *top-code*
         *bottom* *bottom-code*
         *%void* *%void-code*
         *%false* *%false-code*
         *%object* *%object-code*
         *%integer* *%integer-code*
         *%function* *%function-code*
         *%class* *%class-code*
         *fpi-list* *fpi-list-code*
         *<null>* *<null>-code*
         *<function>* *<function>-code*
         *<fpi>* *<fpi>-code*
         get-lattice-type get-lattice-code
         get-strategic-lattice-type get-lattice-type-class
         def-sys-lattice-type def-strategic-lattice-type
         ~compute-lattice-type compute-normalized-lattice-type
         join-lattice-types meet-lattice-types complement-lattice-type-code
         meet-lattice-types? lattice-subtype?
         top-lattice-type? bottom-lattice-type?
         eq-lattice-type copy-lattice-type
         expand-all-lattice-type-literals
         find-lattice-type-for-literal
         set-write-access-stam? new-write-access-stamp
         convert-to-super-non-compound-type
         trans-lattice-type-list))

;;;-----------------------------------------------------------------------------
;;; TYPE LATTICE CLASSES
;;;-----------------------------------------------------------------------------

;; Structure of types in the type lattice.
(defstandardclass <lattice-type> ()
  (name :accessor :initarg :initform ())
  (class :accessor :initarg :initform ())
  (code :accessor :initarg)
  (atomic-expr :accessor :initarg :initform ())
  (subtypes :accessor :initarg :initform ())
  (supertypes :accessor :initarg :initform ())
  (strategic :accessor :initarg :initform ())
  (compound :accessor :initarg :initform ())
  (write-access-stamp :accessor :initarg :initform ()))

;; Structure of types in the type lattice.
(defstandardclass <lattice-type-with-literals> (<lattice-type>)
  (literals :accessor :initarg :initform ()))

;; Structure of the lattice used for type inference.
(defstandardclass <lattice> ()
  (top :accessor :initarg :initform ())
  (bottom :accessor :initarg :initform ())
  (table :accessor :initarg :initform ())
  (latest-write-access-stamp :accessor :initform 0))

;; The global type inference lattice.
(deflocal *the-lattice* (make <lattice>))
(deflocal *strategic-lattice-types* ()) ; a-list

;;; A-list to map from some expanded literals to lattice-types. For
;;; example, 1 -> fpi-one and 0.0 df-zero.
(deflocal *lattice-type-literals* ()) ; a-list

;; Here come some ofter used lattice types with their codes; they have to be
;; updated every time a new lattice type is added to *the-lattice*.
(deflocal *top* ())
(deflocal *top-code* ())
(deflocal *bottom* ())
(deflocal *bottom-code* ())
(deflocal *%void* ())
(deflocal *%void-code* ())
(deflocal *%object* ())
(deflocal *%object-code* ())
(deflocal *%function* ())
(deflocal *%function-code* ())
(deflocal *%class* ())
(deflocal *%class-code* ())
(deflocal *%integer* ())
(deflocal *%integer-code* ())
(deflocal *%false* ())
(deflocal *%false-code* ())
(deflocal *fpi-list* ())
(deflocal *fpi-list-code* ())
(deflocal *<null>* ())
(deflocal *<null>-code* ())
(deflocal *<function>* ())
(deflocal *<function>-code* ())
(deflocal *<fpi>* ())
(deflocal *<fpi>-code* ())

;;; Initializing the lattice; called when loading this file and in
;;; apply-compiler for reset.
(defun initialize-lattice ()
  (ti-format t "~%Initializing the lattice ...")
  (reset-used-codes)
  (setf (?latest-write-access-stamp *the-lattice*) 0)
  (setq *strategic-lattice-types* ())
  (setq *lattice-type-literals* ())
  ;; Add least upper (top) and greatest lower types (bottom) to lattice.
  (setq *top* (make <lattice-type> :name ^top :code (next-code) :strategic t))
  (setq *bottom* (make <lattice-type> :name ^bottom :code 0 :strategic t))
  (setf (?top *the-lattice*) *top*)
  (setf (?bottom *the-lattice*) *bottom*)
  (setf (?subtypes *top*) (list *bottom*))
  (setf (?supertypes *bottom*) (list *top*))
  (setq *strategic-lattice-types*
        (acons ^top *top* *strategic-lattice-types*))
  (setq *strategic-lattice-types*
        (acons ^bottom *bottom* *strategic-lattice-types*))
  (ti-format t " done."))

;;;-----------------------------------------------------------------------------
;;; WRITE ACCESSES OF COMPOUND TYPES
;;;-----------------------------------------------------------------------------

(defun new-write-access-stamp ()
  (let ((new-stamp (+ (?latest-write-access-stamp *the-lattice*) 1)))
    (setf (?latest-write-access-stamp *the-lattice*) new-stamp)
    new-stamp))

(defun set-write-access-stam? (lattice-type)           ;<lattice-type>
  (let ((new-stamp (new-write-access-stamp)))
    (setf (?write-access-stamp lattice-type) new-stamp)
    (set-subtype-write-access-stamps lattice-type new-stamp)))

(defun set-subtype-write-access-stamps (lattice-type   ;<lattice-type>
                                        new-stamp)     ;<spint>
  (dolist (subtype (?subtypes lattice-type))
          (setf (?write-access-stamp subtype) new-stamp)
          (set-subtype-write-access-stamps subtype new-stamp)))

(defun copy-lattice-type (lattice-type) ;<lattice-type>
  (make <lattice-type>
        :name (?name lattice-type)
        :class (?class lattice-type)
        :code (?code lattice-type)
        :supertypes (?supertypes lattice-type)
        :subtypes (?subtypes lattice-type)
        :write-access-stamp (?write-access-stamp lattice-type)
        :strategic (?strategic lattice-type)
        :compound (?compound lattice-type)))

;; Get the next super lattice type that is not compound and copy all slot
;; values to the given lattice type.
(defun convert-to-super-non-compound-type (lattice-type) ;<lattice-type>
  (let ((non-compound-type (find-super-non-compound-type lattice-type)))
    (if *use-compound-types*
        (format t "~%convert ~A to ~A"
                (?name lattice-type)
                (?name non-compound-type)))
    non-compound-type))

(defun find-super-non-compound-type (lattice-type) ;<lattice-type>
  (let* ((supers (?supertypes lattice-type))
         (type (member-with-args (lambda (x)
                                   (null? (?compound x))) supers)))
    (if type (car type)
      (find-super-non-compound-type (car supers)))))

;;;-----------------------------------------------------------------------------
;;; ACCESSING LATTICE TYPES
;;;-----------------------------------------------------------------------------

;;; Return a list of lattice types corresponding to a list of type names.
(defun as-lattice-types (identifiers)
  (if (null? identifiers)
      (list *top*)
    (mapcar #'as-lattice-type identifiers)))

;; Answer the corresponding class (if available) or a lattice type to an
;; identifier.
(defun get-class-or-lattice-type (identifier)
  (choose-class-or-lattice-type identifier
                                (dynamic-let ((error-if-no-lexical-found ()))
                                             (find-in-lex-env identifier))
                                (get-strategic-lattice-type identifier)))

;;; Answer a lattice type using get-class-or-lattice-type.
;;; Compound lattice types are copied with write-access-stamp set to ().
;;; Necessary for ti-signature::def-descr.
(defun get-lattice-type (obj)
  (let ((lattice-type (as-lattice-type-check (get-class-or-lattice-type obj))))
    (if (?compound lattice-type)
        (let ((new-lattice-type (copy-lattice-type lattice-type)))
          (setf (?write-access-stamp new-lattice-type) ())
          new-lattice-type)
      lattice-type)))

;;; Specialization of GF defined in ti-lattice.
(defun get-lattice-code (name)
  (?code (as-lattice-type-check (get-class-or-lattice-type name))))

;;; Answer the strategic lattice type identified by obj.
(defun get-strategic-lattice-type (name)
  (let ((entry (assoc name *strategic-lattice-types*)))
    (if entry
        (let ((lattice-type (cdr entry)))
          (if (?strategic lattice-type)
              lattice-type
            ()))
      ())))

;; Answer a lattice type to a class or lattice type.
(defgeneric as-lattice-type (obj))

(defmethod as-lattice-type (obj)
  (get-lattice-type obj))

(defmethod as-lattice-type ((obj <class-def>))
  (?lattice-type obj))

(defmethod as-lattice-type ((obj <lattice-type>))
  obj)

(defgeneric as-lattice-type-check (obj))

(defmethod as-lattice-type-check (obj)
  (write-message ^warning "type scheme defined with unknown type ~A" obj) ; *IM* 01.03.94
  (ti-error)
  *top*)

(defmethod as-lattice-type-check ((obj <lattice-type>))
  obj)

(defmethod as-lattice-type-check ((obj <class-def>))
  (?lattice-type obj))

;;;-----------------------------------------------------------------------------
;;; Remove all subtype-/supertype-links between the elements of two type-lists.
(defun remove-lattice-links-between (lattice-supertypes lattice-subtypes)
  (dolist (subtype lattice-subtypes)
          (dolist (lattice-type lattice-supertypes)
                  (remove-lattice-subtype lattice-type subtype)))
  (dolist (supertype lattice-supertypes)
          (dolist (lattice-type lattice-subtypes)
                  (remove-lattice-supertype lattice-type supertype))))

(defun remove-lattice-subtype (lattice-type subtype)
  (delete subtype (?subtypes lattice-type)))

(defun remove-lattice-supertype (lattice-type supertype)
  (delete supertype (?supertypes lattice-type)))

;;;-----------------------------------------------------------------------------
;;; Add subtype-/supertype-links between the elements of two type-lists.
(defun add-lattice-links-between (lattice-supertypes lattice-subtypes)
  (dolist (subtype lattice-subtypes)
          (dolist (lattice-type lattice-supertypes)
                  (add-lattice-subtype lattice-type subtype)))
  (dolist (supertype lattice-supertypes)
          (dolist (lattice-type lattice-subtypes)
                  (add-lattice-supertype lattice-type supertype))))

(defun add-lattice-subtype (lattice-type subtype)
  (setf (?subtypes lattice-type)
        (cons subtype (?subtypes lattice-type))))

(defun add-lattice-supertype (lattice-type supertype)
  (setf (?supertypes lattice-type)
        (cons supertype (?supertypes lattice-type))))

;;; Tell all supertypes of a new subtype.
(defun update-supertypes (new-lattice-type lattice-supertypes)
  (dolist (type lattice-supertypes)
          (setf (?code type)
                (join-codes (?code type) (?code new-lattice-type)))
          (let ((expr (?atomic-expr type)))
            (if expr
                (setf (?code expr) (?code type))))
          (update-supertypes new-lattice-type (?supertypes type))))

(defun update-lattice-constants ()
  ;; pre-defined lattice types
  (setq *top-code* (?code *top*))
  (setq *bottom-code* (?code *bottom*))
  (setq *%integer-code* (and *%integer* (?code *%integer*)))
  (setq *%false-code* (and *%false* (?code *%false*)))
  ;; system lattice types
  (setq *%function-code* (and *%function* (?code *%function*)))
  (setq *%void-code* (and *%void* (?code *%void*)))
  (setq *%object* (?lattice-type %object))
  (setq *%object-code* (and *%object* (?code *%object*)))
  (setq *%class* (?lattice-type %class))
  (setq *%class-code* (and *%class* (?code *%class*)))
  ;; standard lattice types (see apply-funs.em)
  (setq *<null>* (and <null>-class (?lattice-type <null>-class)))
  (setq *<null>-code* (and *<null>* (?code *<null>*)))
  (setq *<function>* (and <function>-class (?lattice-type <function>-class)))
  (setq *<function>-code* (and *<function>* (?code *<function>*)))
  (setq *<fpi>* (and <fpi>-class (?lattice-type <fpi>-class)))
  (setq *<fpi>-code* (and *<fpi>* (?code *<fpi>*)))
  ;; strategic lattice types
  (setq *fpi-list* (get-strategic-lattice-type ^fpi-list))
  (setq *fpi-list-code* (and *fpi-list* (?code *fpi-list*))))

;;; Answer a class for system, strategic, and standard lattice types.
(defun get-lattice-type-class (lattice-type) ;<lattice-type>
  (let ((class (?class lattice-type)))
    (if class class
      (let ((classes-of-supers
             (mapcar #'get-lattice-type-class (?supertypes lattice-type))))
        (if classes-of-supers
            (min-class classes-of-supers)
          ())))))

(defun min-class (classes)
  (let ((min-class (car classes)))
    (dolist (class (cdr classes))
            (if (subclass? class min-class)
                (setq min-class class)))
    min-class))

;;(defun min-class (classes)
;;  (min-class1 (cdr classes) (car classes)))
;;
;;(defun min-class1 (classes min-class)
;;  (if classes
;;      (let ((first-class (car classes)))
;;      (min-class1 (cdr classes)
;;                         (if (subclass? first-class min-class)
;;                             first-class
;;                           min-class)))
;;    min-class))

(defun subclass? (class1 class2)
  (member class2 (~class-precedence-list class1)))

;;; Add a lattice type at a specified position into a lattice.
(defun add-lattice-type-between (new-lattice-type ;<lattice-type>
                                 supertype-names
                                 subtype-names)
  (ti-format t "~%Add lattice type ~A." (?name new-lattice-type))
  (setf (?code new-lattice-type) (next-code))
  (let ((supers (as-lattice-types supertype-names))
        (subs (as-lattice-types subtype-names)))
    (remove-lattice-links-between supers subs)
    (add-lattice-links-between supers (list new-lattice-type))
    (add-lattice-links-between (list new-lattice-type) subs)
    (update-supertypes new-lattice-type supers)
    (update-lattice-constants)))

;;;-----------------------------------------------------------------------------
;;; FILLING THE LATTICE
;;;-----------------------------------------------------------------------------

;;; Read in the external describtion of a lattice.
(defun def-sys-lattice-type (type-def)
  (let* ((name (car type-def))
         (supers (car (cdr type-def)))
         (subs (car (cdr (cdr type-def))))
         (class (find-lexical name $tail-module)) ;lookup for TAIL class
         (new-lattice-type (make <lattice-type> :name name :class class)))
    (add-lattice-type-between new-lattice-type supers subs)
    (if class
        (setf (?lattice-type class) new-lattice-type)
      (write-message ^warning "~A should be defined in module %tail" name)) ; *IM* 01.03.94
    new-lattice-type))

;;; Read in the external describtion of a lattice.
(defun def-strategic-lattice-type (type-def)
  (let* ((name (car type-def))
         (supers (car (cdr type-def)))
         (subs (car (cdr (cdr type-def))))
         (new-lattice-type (make <lattice-type>
                                 :name name
                                 :class ()
                                 :strategic t)))
    (add-to-list-of-strategic-types new-lattice-type)
    (add-lattice-type-between new-lattice-type supers subs)
    new-lattice-type))

;;; Adding strategic lattice types to the lattice; the last argument if
;;; not () is a list with a flag if type is compound and some literal values,
;;; which are later expanded.
(defun add-strategic-lattice-type (name supers subs compound&values)
  (let* ((is-compound (if compound&values
                          (eq ^t (car compound&values))
                        ()))
         (values (if compound&values (cdr compound&values) ()))
         (new-lattice-type  (if values
                                (make <lattice-type-with-literals>
                                      :name name
                                      :strategic t
                                      :compound is-compound
                                      :literals values)
                              (make <lattice-type>
                                    :name name
                                    :strategic t
                                    :compound is-compound))))
    (add-to-list-of-strategic-types new-lattice-type)
    (add-lattice-type-between new-lattice-type supers subs)))

(defun add-to-list-of-strategic-types (lattice-type)
  (setq *strategic-lattice-types*
        (acons (?name lattice-type) lattice-type *strategic-lattice-types*)))

(defun expand-all-lattice-type-literals ()
  (mapc (lambda (entry)
          (expand-lattice-type-literals (cdr entry)))
        *strategic-lattice-types*))

(defgeneric expand-lattice-type-literals (lattice-type))

(defmethod expand-lattice-type-literals ((lattice-type <lattice-type>)))

(defmethod expand-lattice-type-literals ((lattice-type <lattice-type-with-literals>))
  (let ((literals (?literals lattice-type)))
    (if literals
        (mapc (lambda (literal)
                (setf *lattice-type-literals*
                      (acons (expand-literal literal) lattice-type
                             *lattice-type-literals*)))
              literals))))

(defun find-lattice-type-for-literal (literal)
  (cdr (assoc literal *lattice-type-literals*)))

;;; Adding non-strategic lattice types to the lattice.
;;; Corresponding generic function is defined in lzs-mop.
(defmethod ~compute-lattice-type ((class <class-def>)
                                  supers lattice-supers)
  (let ((new-lattice-type (make <lattice-type>
                                :name (?identifier class)
                                :class class))
        (subs (list ^bottom)))
    (if (and (cons? supers) (null? (car supers)))
        (setq supers (list *top*)))
    (add-lattice-type-between new-lattice-type
                              (append supers lattice-supers)
                              subs)
    new-lattice-type))

(defmethod ~compute-lattice-type ((class <tail-class-def>)
                                  supers lattice-supers)
  (if (and (null? supers) (null? lattice-supers))
      (call-next-method class () (list ^%struct))
    (call-next-method)))

;;;-----------------------------------------------------------------------------
;;; MEET/JOIN/COMPLEMENT OF LATTICE TYPES
;;;-----------------------------------------------------------------------------

;;; Answer a normalized intersection of two lattice types.
(defun meet-lattice-types (lattice-type1  ;<lattice-type>
                           lattice-type2) ;<lattice-type>
  (let ((code1 (?code lattice-type1))
        (code2 (?code lattice-type2)))
    (cond ((subcode? code1 code2) lattice-type1)
          ((subcode? code2 code1) lattice-type2)
          (t (ti-format t "~%Warning: can't meet lattice types")
             (ti-error)
             *top*))))

;;; Answer a normalized union of two lattice types.
(defun join-lattice-types (lattice-type1  ;<lattice-type>
                           lattice-type2) ;<lattice-type>
  (let ((code1 (?code lattice-type1))
        (code2 (?code lattice-type2)))
    (cond ((subcode? code1 code2) lattice-type2)
          ((subcode? code2 code1) lattice-type1)
          (t
           (let ((joins1
                  (mapcar (lambda (supertype)
                            (join-lattice-types supertype lattice-type2))
                          (?supertypes lattice-type1)))
                 (joins2
                  (mapcar (lambda (supertype)
                            (join-lattice-types supertype lattice-type1))
                          (?supertypes lattice-type2))))
             (cond (joins1
                    (min-lattice-type joins1))
                   (joins2
                    (min-lattice-type joins2))
                   (t
                    (ti-format t "~%Warning: can't join lattice types")
                    (ti-error)
                    *top*)))))))

(defun min-lattice-type (lattice-types)
  (let ((min-type (car lattice-types)))
    (dolist (type (cdr lattice-types))
            (if (lattice-subtype? type min-type)
                (setq min-type type)))
    min-type))

;;(defun min-lattice-type (lattice-types)
;;  (min-lattice-type1 (cdr lattice-types) (car lattice-types)))
;;
;;(defun min-lattice-type1 (lattice-types min-type)
;;  (if lattice-types
;;      (let ((first-type (car lattice-types)))
;;      (min-lattice-type1 (cdr lattice-types)
;;                         (if (lattice-subtype? first-type min-type)
;;                             first-type
;;                           min-type)))
;;    min-type))

;;; Answer complement code of a lattice type.
(defun complement-lattice-type-code (lattice-type) ;<lattice-type>
  (complement-code (?code lattice-type)))

;;; Answer whether all subtypes of first arg meet with a given lattice-type.
(defun meet-all-subtypes? (supertype     ;<lattice-type>
                            lattice-type) ;<lattice-type>
  (null? (member-with-args (lambda (subtype)
                            (null? (meet-lattice-types? subtype lattice-type)))
                          (?subtypes supertype))))

;;; Answer the normalized complement of a lattice type.
(defun normalized-complement-lattice-type (lattice-type) ;<lattice-type>
  *top*)

(defun compute-normalized-lattice-type (name)
  (if (cons? name)
      (let ((op-symbol (car name))
            (arg-def (cdr name)))
        (cond ((eq ^not op-symbol)
               (normalized-complement-lattice-type
                (compute-normalized-lattice-type (car arg-def))))
              ((eq ^and op-symbol)
               (meet-lattice-types
                (compute-normalized-lattice-type (car arg-def))
                (compute-normalized-lattice-type (car (cdr arg-def)))))
              ((eq ^or op-symbol)
               (join-lattice-types
                (compute-normalized-lattice-type (car arg-def))
                (compute-normalized-lattice-type (car (cdr arg-def)))))
              (t
               (ti-format t "~%Warning: no correct atomic type definition")
               *top*)))
    name))

;;;-----------------------------------------------------------------------------
;;; TESTING
;;;-----------------------------------------------------------------------------

;;; Answer whether a lattice type denotes the bottom type.
(defun bottom-lattice-type? (lattice-type) ;<lattice-type>
  (eq-code? (?code lattice-type) *bottom-code*))

;;; Answer whether a lattice type denotes the top type.
(defun top-lattice-type? (lattice-type) ;<lattice-type>
  (eq-code? (?code lattice-type) *top-code*))

;;; Answer whether the intersection of two lattice types is not *bottom-type*.
(defun meet-lattice-types? (lattice-type1  ;<lattice-type>
                             lattice-type2) ;<lattice-type>
  (meet-codes? (?code lattice-type1)(?code lattice-type2)))

;;; Answer whether one lattice type is a subtype of another.
(defun lattice-subtype? (lattice-type1  ;<lattice-type>
                          lattice-type2) ;<lattice-type>
  (subcode? (?code lattice-type1) (?code lattice-type2)))

;;; Answer whether two lattice types are equal.
(defun eq-lattice-type (lattice-type1   ;<lattice-type>
                        lattice-type2)  ;<lattice-type>
  (eq-code? (?code lattice-type1) (?code lattice-type2)))

;;; Answer whether the class corresponding to a lattice type is exported.
(defun exported? (lattice-type) ;<lattice-type>
  (let ((class (?class lattice-type)))
    (if class
        (?exported class)
      ())))

;;;-----------------------------------------------------------------------------
;;; %define-lattice-type (came form el2lzs-classes.em)
;;;-----------------------------------------------------------------------------

(defun trans-lattice-type-list (lattice-types)
  (mapcar #'get-class-or-lattice-type lattice-types))

(deftranssyn (%define-lattice-type name supers subs . values)
  (whole-form))

;; transmod is not necessary, because no global binding must be created

(deftransdef (%define-lattice-type name supers subs . values)
  (with-defining-form
   (add-strategic-lattice-type name
                               (trans-lattice-type-list supers)
                               (trans-lattice-type-list subs)
                               values)
   ()))

(defgeneric choose-class-or-lattice-type (identifier binding lattice-type))

(defmethod choose-class-or-lattice-type (identifier
                                         (class <class-def>)
                                         (lattice-type <null>))
  class)

(defmethod choose-class-or-lattice-type (identifier (class <null>)
                                                    lattice-type)
  lattice-type)

(defmethod choose-class-or-lattice-type (identifier (class <class-def>)
                                                    lattice-type)
  (error-class-equal-lattice-type identifier)
  class)

(defmethod choose-class-or-lattice-type (identifier
                                         (class <null>)
                                         (lattice-type <null>))
  (error-neither-class-nor-lattice-type identifier))

#module-end
