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
;;;  Title: initialize classes for memory management system
;;;  Notes:
;;    1. at the moment the slot gc-tracer belongs to the class, not to the representation
;;    this should be changed in the near future and all calls to (?gc-tracer class)
;;    should be changed into (?gc-tracer representation-object)
;;    2. length parameter in representation pointer-to-vector is assumed to be of type %unsigned-word-integer
;;    3. we are going to generate mm-types if none are given - there might be minor
;;    problems with synchronization in case of module compilation
;;    4. the allocator function is generated only for representation
;;    pointer-to-struct. this should be changed in the future to
;;    a. allocator only for instances of <structure-class>
;;    b. for all classes if this is needed
;;;  Requires:
;;;  Problems:
;;    1. any changes of type <pointer-to-void> must be done conformely with changes in
;;    the generation of trace functions for <%pointer-to-struct>-thingies.
;;    2. now there is a cast to %unsigned-word-integer which should be replaced with an
;;    type %pointer....
;;    3. ~compute constructor:
;;    the generated let expr needs a genuine variable alloc
;;    what about constructors if a user writes its own intialization
;;    representation direct : it is assumed that classes with such a
;;    representation have only one slot !!!!
;;    4. problem with direct
;;    5. ensure-vector-length hack
;;    6. %size-of only correct code for pointer-to-struct and pointer-to-vector
;;    to use %size-of, constructors for multiple-type-card-allocated objects have
;;    use the cds stored in the class !!!
;;    what about dynamically calculated card and type descriptors?
;;    Ensure that init-frorms of "library module" is executed first!!!!!!!!!
;;    estimated-size in canonize-mm-card methods is not necessaryly machine
;;    independent
;;    there should be an option which defines the use of that estimation
;;    7. card-type-code contains lzs-object
;;    8. all lexical env's replaced by empty lists
;;    9. %string is initialized by dummy-initialization -> the file basic-string.am
;;    contains the initialization with card-descriptor 11   <<<<<<<<< Attention
;;    therefore last-used-card-descriptor is set to 11 in init-mm-initialize
;;;  Authors: E. Ulrich Kriegel
;;;-----------------------------------------------------------------------------

#module mm-initialize
(import (eulisp0
         mm-initialize-syntax ;for constants last-used-tds...
         compiler-conditions
         (only (?configuration-value)
               configuration)
         lzs-mop
         lzs
         representation
         accessors
         el2lzs
         (only (get-option
                check-options)
               option-lists)
         (only (complete-function
                add-function)
               el2lzs-rules)
         (only (<basic-class-def>
                <%pointer-to-struct>
                <%pointer-to-vector>
                <%pointer>
                <%direct>
                <%string>
                %string
                ) machine-description)
         tail-module
         apply-funs
         (only (?byte-length-of-instance
                ?byte-length-as-component)
               representation)
         (only (set-predicate-signature)
               type-inference)
         (rename ((mapcan cl:mapcan)
                  (mapcar cl:mapcar)
                  (make-instance cl:make-instance)
                  (find cl:find)
                  (vector cl:vector)
                  (member cl:member)
                  (not cl:not))
                 (only (mapcan
                        mapcar
                        make-instance
                        find
                        vector
                        member
                        not)
                       common-lisp))
         expand-literal)
 syntax (eulisp1
         mm-initialize-syntax
         (rename
          ((push cl:push)
           (incf cl:incf)
           (cond cl:cond))
          common-lisp))
 export (mm-initialize
         init-mm-initialize))


;;;-----------------------------------------------------------------------------
;;; list of class init forms - must be reset in ~compute-runtime-initialization
;;;-----------------------------------------------------------------------------
(defvar *class-initialization-forms* ())

;;definitions and init-forms
;;;-----------------------------------------------------------------------------
;;; Definition of used compiler-conditions
;;;-----------------------------------------------------------------------------

(define-compiler-condition <wrong-allocation-argument> (<condition>)
  "The allocation argument ~A in class ~A is not defined" :argument :class )

(define-compiler-condition <no-allocation-argument> (<condition>)
  "Neither class ~A nor its superclass ~A have a allocation argument." :class :super )

(define-compiler-condition <wrong-initialization-argument-length-for-vector-class> (<condition>)
  "The vector class ~A is defined with an initvalue of ~A for length and can ~
therefore not have a length argument for the constructor function" :class
:length)

(define-compiler-condition <missing-length-argument> (<condition>)
  "The vector class ~A is defined without  an initvalue and no init-value is~
given in the constructor form" :class )

;;;-----------------------------------------------------------------------------
;;; Constants
;;;-----------------------------------------------------------------------------


(defconstant $dummy-class-mm-type (literal-instance %signed-word-integer -1))
(defconstant $dummy-class-mm-card (literal-instance %signed-word-integer -1))

;;$dynamic-class-mm-card-for-vector-classes is used to signal the need for
;;creating card descriptors in constructor functions during class definition and allocation
;;if no length is given and if allocation is not on multiple-size-cards

(defconstant $dynamic-class-mm-card-for-vector-classes (literal-instance %signed-word-integer -10))

;;;-----------------------------------------------------------------------------
;;; Variables, initial values for accumulator vars are set in init-mm-initialize
;;;-----------------------------------------------------------------------------------

(deflocal generated-mm-types ())
(deflocal initialized-classes ())

;;Using %size-of there it is not possible to calculate the size of instances
;;during apply compilation -- therefore all multiple-type-card-instances have to
;;use card-descriptors which are created dynamically during runtime

(deflocal multiple-type-card-descriptors ())
(deflocal multiple-size-card-descriptors ());; default for class %string



;;;-----------------------------------------------------------------------------
;;; Assumtion: mm-options not specified in %define-xx-class are set to ()
;;; mm-type specified ->
;;;     corresponding card-descriptor is already used ->
;;;             generate '(set-type-descriptor mm-type (?gc-tracer representation))
;;;     corresponding card-descriptor not yet used ->
;;;                     constructor can be generated with mm-type and mm-card values
;;;             generate '(set-type-descriptor mm-type (?gc-tracer representation))
;;;                      '(set-card-descriptor card-type size mm-type)
;;;                     constructor can be generated with mm-type and mm-card values
;;; mm-type not specified ->
;;;     card and type descriptors must be generated dynamically at run-time
;;;     corresponding class slots must be set
;;;     constructor has to use values of mm-card and mm-type class slots
;;;     generate '(let* ((mm-type (make-type-descriptor class gc-tracer))
;;;                      (mm-card (make-card-descriptor card-type size mm-type)))
;;;                   (set-mm-type-in-class)
;;;                   (set-mm-card-in-class))
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;; Reset accumulator vars, called by the apply compiler
;;;-----------------------------------------------------------------------------

(defun init-mm-initialize()
  (setq generated-mm-types ())
  (setq initialized-classes ())

  (setq multiple-type-card-descriptors
        ())
  (setq multiple-size-card-descriptors
        ())
  )


;;;-----------------------------------------------------------------------------
;;;
;;;-----------------------------------------------------------------------------

(defun dummy-initialization-for
  (class representation-object)
  (setf(?mm-type representation-object) $dummy-class-mm-type)
  (setf(?mm-card representation-object) $dummy-class-mm-card)
  (setf(?allocation representation-object) ^multiple-type-card)
  (setf(?gc-tracer class) ^unknown)
  (setf(?allocator class) ^unknown)
  )

(defun mm-initialize-standard (class representation-object allocation)
  (cl:push class initialized-classes)
  ;;canonize gc-tracer and canonize-allocation must be done inside
  ;;mm-initialize-using-representation because of representation direct does not
  ;;have both
  ;;generate mm-type if none is given
  ;;be careful in case of module compilation

  (let ((mm-type (if (dynamic *static-mm-type*)
                     (progn
                       (setq max-used-type-descriptor (+ 1
                                                         max-used-type-descriptor))
                       ;; *UK* 21.06.94 not sure whether or not the following will
                       ;; ever be used
                       (cl:push (cons max-used-type-descriptor class) generated-mm-types)
                       max-used-type-descriptor)
                   ;;use dynamically estimated mm-type
                   ()
                   )))

    (mm-initialize-using-representation representation-object class allocation mm-type))
  )

(defgeneric mm-initialize
  (class representation-object allocation))

(defmethod mm-initialize
  ((class <imported-class>) representation-object allocation)
  ;;insert dummy values
  (dummy-initialization-for class representation-object))

;;the following method have to be activated if the method for <imported-class> is
;;changed (see above)
;;(defmethod mm-initialize
;;           ((class <basic-class-def>) representation-object allocation )
;;  ;;insert dummy values
;;  (dummy-initialization-for class representation-object))

(defmethod mm-initialize
  ((class <abstract-class-def>) representation-object allocation)
  (dummy-initialization-for class representation-object))

(defmethod mm-initialize
  ((class <tail-class-def>) representation-object allocation )
  (if allocation
      (mm-initialize-standard class representation-object allocation)
    (dummy-initialization-for class representation-object)))

(defmethod mm-initialize
  ((class <standard-class-def>) (representation-object <%direct>)
   allocation )
  ;;prevent <direct> from handling with mm-type
  (dummy-initialization-for  class representation-object))

(defmethod mm-initialize
  ((class <standard-class-def>) representation-object allocation)
  (mm-initialize-standard class representation-object allocation))

(defun canonize-gc-tracer
  (class representation-object )
  ;;computes gc-tracer for a class and fills slot gc-tracer
  ;; should become true in the future
  ;; (setf(?gc-tracer representation-object) (generate-trace-code  representation-object class))
  (setf(?gc-tracer class) (generate-trace-code representation-object class )))

(defun canonize-mm-type
  (class representation-object mm-type)
  (setf (?mm-type representation-object) (literal-instance %signed-word-integer
                                                           mm-type))
  ;;add default (set-type-descriptor mm-type class (?gc-tracer
  ;;representation-object))
  (cl:push (cl:make-instance <app>
                             :function set-type-descriptor
                             :arg-list (list
                                        (literal-instance %unsigned-word-integer mm-type)
                                        (cl:make-instance <app>
                                                          :function %cast
                                                          :arg-list (list
                                                                     %unsigned-word-integer
                                                                     class))
                                        (?gc-tracer class); *IM* 10.03.94
                                        ))
           (dynamic *class-initialization-forms*))
  )

(defun canonize-allocation (class representation-object allocation)
  ;;if no representation is given lookup in the superclass
  ;;returns allocation

  (setq allocation
        (if allocation
            ;;one step evaluation of allocation symbols using configuration table
            (?configuration-value allocation)
          ;;use configuration of superclass which is already evaluated
          (?allocation (~class-representation
                        (car (cdr (~class-precedence-list class)))))))
  (if allocation
      (setf (?allocation representation-object) allocation)
    (compiler-error <no-allocation-argument> () :class (?identifier class)
                    :super  (?identifier (car (cdr (~class-precedence-list class))))))

  allocation)

(defun create-runtime-mm-defaults
  (representation-object class allocation)
  (create-runtime-mm-type-default representation-object class allocation)
  (create-runtime-mm-card-default representation-object class allocation))

(defmethod mm-initialize-using-representation
  ((representation-object <%direct>) class allocation mm-type)
  (dummy-initialization-for class representation-object))

(defmethod mm-initialize-using-representation
  ((representation-object <%pointer-to-struct>) class allocation
   mm-type)
  ;;create gc-tracer and store them in representation object
  (canonize-gc-tracer class representation-object)
  ;;find allocation either by evaluating configuration info or by looking into
  ;;the super
  (setq allocation (canonize-allocation class representation-object allocation))
  (if mm-type
      (progn
        (canonize-mm-type class representation-object mm-type)
        (canonize-mm-card class representation-object allocation mm-type))
    (create-runtime-mm-defaults  representation-object class allocation))
  )


(defmethod mm-initialize-using-representation
  ((representation-object <%pointer-to-vector>) class allocation mm-type)
  (canonize-gc-tracer class representation-object)
  (setq allocation (canonize-allocation class representation-object allocation))
  (if (and mm-type (or (cl:not (null? (~vector-class-instance-length class)))
                       (eq allocation ^multiple-size-card)))
      (progn
        (canonize-mm-type class representation-object mm-type)
        (canonize-mm-card class representation-object allocation mm-type))
    (create-runtime-mm-defaults  representation-object class allocation))
  )


(defmethod create-runtime-cdscr-default
  (class cdscr cardtype size tdscr)
  ;;size is assumed to be an lzs-expression with %size-of or an literal
  (cl:push (cl:make-instance <app>
                             :function set-card-descriptor
                             :arg-list (list
                                        (literal-instance %unsigned-word-integer cdscr)
                                        cardtype
                                        size
                                        (literal-instance %unsigned-word-integer tdscr)
                                        ))
           (dynamic *class-initialization-forms*))

  )

;;;-----------------------------------------------------------------------------
;;; Generation of type and card descriptors during runtime
;;;-----------------------------------------------------------------------------

;;if no value for mm-type is given, it should be created during run time

(defmethod create-runtime-mm-type-default
  ((representation-object <%pointer>) class allocation)
  ;;actions to be done
  ;;set dummy values into mm-slots of representation-object to have no unbound slots
  (setf(?mm-type representation-object) $dummy-class-mm-type)
  ;;generation of typ descriptor
  ;;inscribe corresponding values into class-object
  ;;all constructors have to use the values from that class object
  ;;generate type-descriptor and sets mm-type
  (cl:push (cl:make-instance <app>
                             :function set-class-mm-type
                             :arg-list (list
                                        class
                                        (cl:make-instance <app>
                                                          :function make-type-descriptor
                                                          :arg-list (list
                                                                     (cl:make-instance <app>
                                                                                       :function %cast
                                                                                       :arg-list (list
                                                                                                  %unsigned-word-integer
                                                                                                  class))
                                                                     (cl:make-instance <app>
                                                                                       :function %cast
                                                                                       :arg-list (list
                                                                                                  %function
                                                                                                  (?gc-tracer class)))))))
           (dynamic *class-initialization-forms*)))


(defun card-type-code
  (allocation)
  ;;returns the bindings for STSS,MTSS,STMS
  (cl:make-instance <var-ref>
                    :var (if (eq allocation ^multiple-type-card)
                             $mtss
                           (if (eq allocation ^multiple-size-card)
                               $stms
                             $stss)
                           ))
  )

(defmethod create-runtime-mm-card-default
  ((representation-object <%pointer-to-struct>) class allocation)
  ;;actions to be done
  ;;set dummy values into mm-slots of representation-object to have no unbound values
  (setf(?mm-card representation-object) $dummy-class-mm-card)
  ;;generation card descriptor
  ;;inscribe corresponding values into class-object
  ;;all constructors have to use the values from that class object
  ;;generate card descriptor using mm-type from class and
  ;;then set mm-card
  (cl:push (cl:make-instance <app>
                             :function set-class-mm-card
                             :arg-list (list
                                        class
                                        (cl:make-instance <app>
                                                          :function make-card-descriptor
                                                          :arg-list (list (card-type-code allocation); *UK* 10.01.94
                                                                          (cl:make-instance <app>
                                                                                            :function %size-of-instance
                                                                                            :arg-list (list class))
                                                                          (cl:make-instance <app>
                                                                                            :function class-mm-type
                                                                                            :arg-list (list class))))))
           (dynamic *class-initialization-forms*))
  )

(defun ~vector-class-instance-byte-length (class allocation)
  ;;returns either #%i0 or an corresponding lzs-expression with %size-of or
  ;;value of $dynamic-class-mm-card-for-vector-classes
  (let ((size (~vector-class-instance-length class)))
    (if (null? size)
        (if (eq allocation ^multiple-size-card)
            (literal-instance %signed-word-integer 0)
          $dynamic-class-mm-card-for-vector-classes)
      (cl:make-instance <app>
                        :function %mult
                        :arg-list (list (literal-instance %signed-word-integer size)
                                        (cl:make-instance <app>
                                                          :function %size-as-component
                                                          :arg-list (list (~vector-class-element-type
                                                                           class))))))))

(defmethod create-runtime-mm-card-default
  ((representation-object <%pointer-to-vector>) class allocation)
  ;;actions to be done
  ;;set dummy values into mm-slots of representation-object to have no unbound slots
  ;;generation of card descriptor if ~vector-class-instance-length is not () or
  ;;if allocation is multiple-size-card
  ;;inscribe corresponding values into class-object
  ;;if ~vector-class-instance-length returns () ansd allocation is not multiple-size-card
  ;;no card descriptor can be generated. So every time a constructor is called,
  ;;there will be a call to make-card-descriptor which generates one if there is none
  ;;all constructors have to use the values from that class object
  ;;generate card descriptor using mm-type from class and
  ;;then set mm-card

  (let ((size (~vector-class-instance-byte-length class allocation)))
    (if (eq size $dynamic-class-mm-card-for-vector-classes)
        ;;signal that there is a vector-class without known length
        (setf(?mm-card representation-object) $dynamic-class-mm-card-for-vector-classes)

      ;;there is a length defined as initvalue or vector is allocated on multiple
      ;;size card
      (progn
        ;;set dummy values into mm-slots of representation-object to have no unbound slots
        (setf(?mm-card representation-object) $dummy-class-mm-card)
        (cl:push (cl:make-instance <app>
                                   :function set-class-mm-card
                                   :arg-list (list
                                              class
                                              (cl:make-instance <app>
                                                                :function make-card-descriptor
                                                                :arg-list (list (card-type-code allocation); *UK* 10.01.94
                                                                                size
                                                                                (cl:make-instance <app>
                                                                                                  :function class-mm-type
                                                                                                  :arg-list (list class))))))
                 (dynamic *class-initialization-forms*)))))
  )

;;;-----------------------------------------------------------------------------
;;; canonize-mm... methods try to estimate descriptors during compilation time
;;;-----------------------------------------------------------------------------

(defgeneric canonize-mm-card (class representation-object allocation mm-type))

(defmethod canonize-mm-card (class (representation-object <%pointer-to-vector>) allocation mm-type)
  ;;assume that allocation and mm-type are set
  ;;mm-type not set -> must be generated during run time
  ;;creation of card-descriptors
  (let ((estimated-size (+ (?alignment representation-object)
                           (?byte-length-of-instance representation-object)))
        (size (~vector-class-instance-byte-length class allocation))
        (ctype (card-type-code allocation)))
    (cond
      ((eq allocation ^single-card)
       ;;handle single card type descriptors

       (cl:incf max-used-card-descriptor )
       (setf (?mm-card representation-object)
             (literal-instance %signed-word-integer max-used-card-descriptor))
       (create-runtime-cdscr-default class max-used-card-descriptor
                                      ctype
                                      size
                                      mm-type))
      ;;handle multiple type card descriptors
      ;;multiple type card descriptors is something like an option list with
      ;;estimated-size and corresponding card descriptor
      ;;this calculatiion is not necessaryly machine-independent
      ;;alignment is added to estimated size to place objects with
      ;;non-standard-alignments on proper mtss cards
      ((eq allocation ^multiple-type-card)
       (canonize-multiple-card-descriptors class representation-object size
                                           mm-type ctype estimated-size multiple-type-card-descriptors))
      ((eq allocation  ^multiple-size-card)
       ;;special treatent for vector with unknown size
       ;;size () would result in %iNIL in set-mm-card
       ;; ~vector-class-instance-byte-length is always not null, so there is no
       ;; need to test length
       (canonize-multiple-card-descriptors class representation-object size mm-type ctype class multiple-size-card-descriptors))
      (t (compiler-error <wrong-allocation-argument> () :argument allocation
                         :class (?identifier class))))))

(defmethod canonize-mm-card
  (class representation-object allocation mm-type)
  ;;assume that allocation and mm-type are set
  ;;mm-type not set -> must be generated during run time
  ;;creation of card-descriptors
  (let ((estimated-size (+ (?alignment representation-object)
                           (?byte-length-of-instance representation-object)))
        ;;estimated-size is the size of instance in bytes
        ;;this calculatiion is not necessaryly machine-independent
        ;;alignment is added to estimated size to place objects with
        ;;non-standard-alignments on proper mtss cards
        (ctype (card-type-code allocation))
        (size (cl:make-instance <app>
                                :function %size-of-instance
                                :arg-list (list class))))
    (cond
      ((eq allocation ^single-card)
       ;;handle single card type descriptors

       (cl:incf max-used-card-descriptor )
       (setf (?mm-card representation-object)
             (literal-instance %signed-word-integer max-used-card-descriptor))
       (create-runtime-cdscr-default class max-used-card-descriptor
                                      ctype
                                      size
                                      mm-type))
      ;;handle multiple type card descriptors
      ;;multiple type card descriptors is something like an option list with
      ;;estimated-size and corresponding card descriptor
      ((eq allocation ^multiple-type-card)
       (canonize-multiple-card-descriptors class representation-object size mm-type ctype estimated-size multiple-type-card-descriptors))
      ((eq allocation  ^multiple-size-card)
       (canonize-multiple-card-descriptors class representation-object size mm-type ctype class multiple-size-card-descriptors))
      (t (compiler-error <wrong-allocation-argument> () :argument allocation
                         :class (?identifier class))))))


;;;-----------------------------------------------------------------------------
;;; Generation of trace functions
;;;-----------------------------------------------------------------------------

(defgeneric pointer-representation-p(representation))

(defmethod pointer-representation-p
  ((representation <%pointer>))
  t)
(defmethod pointer-representation-p
  (representation)
  ())

(defgeneric generate-trace-code (representation-instance class))

(defmethod generate-trace-code
  ((representation-instance <%pointer-to-struct>)
   class )
  ;;returns a symbol naming the trace function or a lambda list
  ;;containing (trace (%select object slot-name))-forms
  (let* ((slot-descrs (~class-slot-descriptions class))
         (len (length slot-descrs))
         (ptr ^ptr)
         (trace-calls (cl:mapcan (lambda(slot)
                                   (if (pointer-representation-p
                                        (?representation (~slot-description-type slot)))
                                       (list `(,trace-pointer (,%cast
                                                               ,<pointer-to-void>
                                                               (,^%select ,ptr ,class; *UK* 20.01.94
                                                                          ,(~slot-description-name
                                                                            slot)))))
                                     (); no trace necessary
                                     )) slot-descrs))
         )
    (let ((le (length trace-calls)))
      (cond
        ;;nothing to do - object does not contain pointer
        ((= le 0)
         (%function-literal trace-nothing))
        ;;someting like a pair
        ((and (= le 2)(= len 2))
         (%function-literal trace-pair))
        ((and (= le len) (> le 1))
         (%function-literal trace-general-object))
        (t (%function-literal
            (add-function
             (complete-function
              (cl:make-instance <global-fun>
                                :range-and-domain (cl:vector %void class %signed-word-integer)
                                :identifier (list ^trace (?identifier class))
                                )
              (list ptr ^length)
              (if (= le 1)
                  (car trace-calls)
                (cons ^progn trace-calls))
              ();; empty env
              ;;(?lex-env (find-module ^mm-interface))
              ))))))))




(defmethod generate-trace-code
  ((representation-instance <%pointer-to-vector>)
   class )
  (if (pointer-representation-p (?representation (~vector-class-element-type class)))
      ;;since all elements have the same type, use trace-general-object
      (%function-literal trace-general-object)
    ;;noting to do here
    (%function-literal trace-nothing)))


(defmethod generate-trace-code
  ((representation-instance <%direct>) class)
  (%function-literal trace-nothing))

;;;-----------------------------------------------------------------------------
;;; generation of forms used for the creation of constructors
;;;-----------------------------------------------------------------------------

(defun convert-literal-for-allocation
  (li)
  ;;mm-type and mm-card are stored as %signed-word-integer
  ;;allocation functions require %unsigned-word-integer
  ;;so there is a neet for conversion
  (make-literal-instance %unsigned-word-integer
                         (?value-list li)))

(defmethod generate-allocation-code
  ((representation <%pointer-to-struct>) class parameters)
  (let* (
         (mm-type (?mm-type representation))
         (mm-card (?mm-card representation))
         (allocation (?allocation representation))
         ;;(size (?byte-length-of-instance representation))
         (tds (if (eq mm-type $dummy-class-mm-type)
                  `(,class-mm-type ,class)
                (convert-literal-for-allocation mm-type) ))
         (cds (if (eq $dummy-class-mm-card mm-card)
                  `(,class-mm-card ,class)
                (convert-literal-for-allocation mm-card))))
    (if (eq allocation ^single-card)
        `(,allocate-on-single-card ,cds)
      (if (eq allocation ^multiple-type-card)
          `(,allocate-on-multiple-type-card ,cds ,tds)
        (if (eq allocation ^multiple-size-card)
            ;; `(,allocate-on-multiple-size-card ,cds ,(literal-instance
            ;;                                          %signed-word-integer size))
            `(,allocate-on-multiple-size-card ,cds (,%size-of-instance ,class))
          (compiler-error <wrong-allocation-argument> () :argument allocation
                          :class (?identifier class)))))))

;;;-----------------------------------------------------------------------------
;;; Special treatment needed if no length is specified and allocation is one of
;;; single-card or multiple type card - the only chance I see is to generate a
;;; call to make-card-descriptor which looks in the mm-database whether or not a
;;; corresponding descriptor already exists - otherwise it creates one.
;;;-----------------------------------------------------------------------------

;;(defun ensure-vector-length
;;       (representation-instance class parameters)
;;  (let ((initial-length (~vector-class-instance-length class ))
;;        (length-parameter (cl:find ^length parameters)))
;;    (cond ((and length-parameter initial-length)
;;           (compiler-error
;;            <wrong-initialization-argument-length-for-vector-class> ()
;;                           :class class :length initial-length))
;;          ((and (null? initial-length) (cl:not length-parameter))
;;           (compiler-error <missing-length-argument> () :class class))
;;          (initial-length (literal-instance %signed-word-integer (?byte-length-of-instance representation-instance)))
;;;;a hack to get true size
;;          (initial-length
;;           (literal-instance %signed-word-integer (* initial-length
;;                                                       (?byte-length-as-component  (?representation
;;                                                                                (~vector-class-element-type class)) ))))
;;          (length-parameter
;;           `(,%mult ,(literal-instance %signed-word-integer (?byte-length-as-component  (?representation
;;                                                                                        (~vector-class-element-type class)) ))
;;                     (,%cast ,%signed-word-integer ,^length)))
;;          )
;;    ))

(defun ensure-vector-length
  (representation-instance class parameters)
  ;;
  ;;representation * class * parameters -> %unsigned-word-integer
  ;;
  (let ((initial-length (~vector-class-instance-length-literal class ))
        (length-parameter (cl:find ^length parameters)))
    (cond ((and length-parameter initial-length)
           (compiler-error
            <wrong-initialization-argument-length-for-vector-class> ()
            :class class :length initial-length))
          ((and (null? initial-length) (cl:not length-parameter))
           (compiler-error <missing-length-argument> () :class class))
          (initial-length
           `(,%mult (,%cast ,%unsigned-word-integer (,%size-as-component  ,(~vector-class-element-type class) ))
                    initial-length))
          (length-parameter
           `(,%mult (,%cast ,%unsigned-word-integer (,%size-as-component  ,(~vector-class-element-type class)))
                    (,%cast ,%unsigned-word-integer ,^length)))
          )
    ))

(defmethod generate-allocation-code
  ((representation <%pointer-to-vector>) class parameters)
  (let* (
         (mm-type (?mm-type representation))
         (mm-card (?mm-card representation))
         (allocation (?allocation representation))
         (size (ensure-vector-length representation class parameters))
         (ctype (card-type-code allocation)); *UK* 10.01.94
         (tds (if (eq mm-type $dummy-class-mm-type)
                  `(,class-mm-type ,class )
                (convert-literal-for-allocation mm-type) ))
         (cds (if (eq $dummy-class-mm-card mm-card)
                  `(,class-mm-card ,class)
                (if (eq $dynamic-class-mm-card-for-vector-classes mm-card)
                    `(,make-card-descriptor ,ctype ,size ,tds)
                  (convert-literal-for-allocation mm-card)))))
    (if (eq allocation ^single-card)
        `(,allocate-on-single-card ,cds)
      (if (eq allocation ^multiple-type-card)
          `(,allocate-on-multiple-type-card ,cds ,tds)
        (if (eq allocation ^multiple-size-card)
            `(,allocate-on-multiple-size-card ,cds (,%cast ,%signed-word-integer ,size))
          (compiler-error <wrong-allocation-argument> () :argument allocation
                          :class (?identifier class)))))))


;;;-----------------------------------------------------------------------------
;;; generation of constructors and predicates
;;;-----------------------------------------------------------------------------

(defmethod ~compute-constructor
  ((class <abstract-class-def>) parameters)
  ())

(defmethod ~compute-constructor
  ((class <standard-class-def>) parameters)
  (~compute-constructor-using-representation (?representation class) class
                                             parameters))


(defmethod ~compute-constructor-using-representation
  ((representation-object <%direct>)
   class parameters)
  ;;the number of arguments (length of parameters) should be exactly 1 and must
  ;;name the only slot
  ;;the constructor function only makes a type conversion from slot type to the
  ;;class for which the constructor is created
  (add-function
   (complete-function
    (cl:make-instance <constructor-fun>
                      :range-and-domain
                      (cl:vector class
                                 (~slot-description-type (car (~class-slot-descriptions
                                                               class))));;superclasses may not
                      ;;have slots
                      :keywords parameters
                      :constructor-for class)
    parameters         ;; lambda-list
    `(,%cast ,class ,(car parameters)); body
    ();;empty env
    ;;(?lex-env (find-module ^mm-interface))
    )))



(defmethod ~compute-constructor-using-representation
  ((representation-object <%pointer-to-struct>)
   class parameters)
  ;;parameters which doesn't name a slot keyword are ignored
  (let* ((slot-descriptions (~class-slot-descriptions class))
         (access-list
          (cl:mapcar (lambda(slot)
                       (let ((slot-name (~slot-description-name slot))
                             (keyword (~slot-description-keyword slot))
                             (default-function (~slot-description-default-function slot)))
                         (if (member keyword parameters)
                             ;;keyword of slot is contained in parameters
                             (list ^%setf (list ^%select ^alloc class slot-name) ; *UK* 20.01.94
                                   keyword)
                           (if default-function
                               ;;there is a default-function
                               (list ^%setf (list ^%select ^alloc class slot-name) ; *UK* 20.01.94
                                     (list default-function))
                             ;;do nothing
                             ()))))
                     slot-descriptions))
         )
    (add-function
     (complete-function
      (cl:make-instance <constructor-fun>
                        :range-and-domain
                        (apply #'cl:vector class
                               (cl:mapcar (lambda (keyword)
                                            (~slot-description-type
                                             (cl:find keyword slot-descriptions
                                                      :key #'~slot-description-keyword)))
                                          parameters))
                        :keywords parameters
                        :constructor-for class)
      parameters         ;; lambda-list
      `(,^let ((,^alloc (,%cast ,class ,(generate-allocation-code
                                         representation-object class parameters))))
              (,^progn
               ,@access-list
               ,^alloc))  ;; body
      ;;(?lex-env (find-module ^mm-interface))
      (); empty env
      )))
  )

(defmethod ~compute-constructor-using-representation
  ((representation-object <%pointer-to-vector>)
   class parameters)
  ;;the possible parameters for a vector constructor are element and length
  ;;length must be given if the slot length has no default
  ;;length must not be given if the slot length has an default
  ;;element specifies the initial value for all vector elements
  ;;element can be omitted in which case the vector is leaved uninitialized
  ;;if element is given then an default-function is generated which runs over the
  ;;whole vector and initializes its elements with the argument element
  (let* ((slot-descriptions (~class-slot-descriptions class))
         (number-of-slots (if (~vector-class-instance-length-literal class)
                              (~vector-class-instance-length-literal class)
                            `(,%cast ,%unsigned-word-integer ,^length)))
         (init-value (if (member ^element parameters)
                         ^element
                       (if (~vector-class-element-default-function class)
                           (list (~vector-class-element-default-function class))
                         ())))
         (init-fun () ))
    ;;making init-fun
    (if init-value
        (progn
          (setq init-fun (cl:make-instance <global-fun>
                                           :range-and-domain (cl:vector %void class
                                                                        %unsigned-word-integer
                                                                        (~vector-class-element-type class))
                                           :identifier (list ^init (?identifier class))
                                           ))
          (add-function
           (complete-function
            init-fun
            ;; lambda-list
            ^(instance length value)
            ;; body

            `(,^if (,%gt ,^length ,(literal-instance %unsigned-word-integer 0))
                   (,^progn (,^%setf ,^length (,%minus ,^length
                                                       ,(literal-instance %unsigned-word-integer 1)))
                            (,^%setf(,^%extract ,^instance ,^length)
                                    ,^value)
                            (,init-fun ,^instance ,^length ,^value))
                   ())
            (tail-environment)
            )))
      ())
    ;;still to do initialization
    (add-function
     (complete-function
      (cl:make-instance <constructor-fun>
                        :range-and-domain
                        (apply #'cl:vector class
                               (cl:mapcar (lambda (slot-name)
                                            (~slot-description-type
                                             (cl:find slot-name slot-descriptions
                                                      :key #'~slot-description-name)))
                                          parameters))
                        :keywords parameters
                        :constructor-for class)
      parameters         ;; lambda-list

      (if init-fun
          `(,^let ((,^alloc (,%cast ,class ,(generate-allocation-code
                                             representation-object class parameters))))
                  (,^progn

                   (,init-fun ,^alloc ,number-of-slots ,init-value)

                   ,^alloc))
        `(,%cast ,class ,(generate-allocation-code
                          representation-object class parameters)))
      ();;empty env
      ))
    ))



;;;-----------------------------------------------------------------------------
;;; installing allocator function in a class
;;;-----------------------------------------------------------------------------

(defgeneric canonize-allocator (class representation))

(defmethod canonize-allocator (class representation)
  ;;dummy-method
  ())
(defmethod canonize-allocator ((class <tail-class-def>) (representation <%pointer-to-struct>))
  (if (eq (?allocation representation) ^unknown)
      ()
    (add-function
     (complete-function
      (cl:make-instance <global-fun>
                        :range-and-domain (cl:vector class)
                        :identifier (list ^allocate (?identifier class)))
      ()             ;; no parameters
      `(,%cast ,class
               ,(generate-allocation-code representation class ()))
      ();;empty env
      ;;(?lex-env (find-module ^mm-interface))
      ))))

(defmethod canonize-allocator (class (representation <%pointer-to-struct>))
  (add-function
   (complete-function
    (cl:make-instance <global-fun>
                      :range-and-domain (cl:vector class)
                      :identifier (list ^allocate (?identifier class)))
    ()             ;; no parameters
    `(,%cast ,class
             ,(generate-allocation-code representation class ()))
    ();;empty env
    ;;(?lex-env (find-module ^mm-interface))
    )))

;;;-----------------------------------------------------------------------------
;;; Predicates
;;;-----------------------------------------------------------------------------

(defmethod ~compute-predicate ((class <standard-class-def>))
  (let ((pred (add-function
               (complete-function
                (cl:make-instance <predicate-fun>
                                  :range-and-domain (cl:vector %object %object))
                ^(object)              ;; lambda-list
                `(,%instance-of? ,^object ,class)    ; body
                ()))))
    ;;set signature for predicate
    (set-predicate-signature pred class)
    pred))


(defmethod ~compute-predicate
  ((class <tail-class-def>))
  ())


;;;-----------------------------------------------------------------------------
;;; Reader and Writer
;;;-----------------------------------------------------------------------------

(defmethod ~compute-slot-reader
  (class slot effective-slots)
  (~compute-slot-reader-using-representation (?representation class) class slot
                                             effective-slots))
;;reader for direct values is only a cast to the slot-type
(defmethod ~compute-slot-reader-using-representation
  ((representation-object <%direct>)
   class slot effective-slots)
  (add-function
   (complete-function (cl:make-instance <slot-accessor-fun>
                                        :slot slot
                                        :range-and-domain (cl:vector (~slot-description-type slot)
                                                                     class))
                      ^(object)
                      `(,%cast  ,(~slot-description-type slot) ,^object)
                      (tail-environment))))

(defmethod ~compute-slot-reader-using-representation
  ((representation-object <%pointer-to-struct>)
   class slot effective-slots)
  (add-function
   (complete-function (cl:make-instance <slot-accessor-fun>
                                        :slot slot
                                        :range-and-domain (cl:vector (~slot-description-type slot)
                                                                     class))
                      ^(object)
                      `(,^%select ,^object ,class ,(~slot-description-name slot)); *UK* 20.01.94
                      (tail-environment))))

(defmethod ~compute-slot-reader-using-representation
  ((representation-object <%pointer-to-vector>)
   class slot effective-slots)
  (if (eq ^element (~slot-description-name slot))
      ;;reader for element = access fun for vectors
      (add-function
       (complete-function (cl:make-instance <slot-accessor-fun>
                                            :slot slot
                                            :range-and-domain (cl:vector (~slot-description-type slot)
                                                                         class %unsigned-word-integer))
                          ^(object index)
                          `(,%extract ,^object ,^index)
                          (tail-environment)))
    ;;reader for length = call to object-size which is imported in mm-initialize
    (add-function
     (complete-function (cl:make-instance <slot-accessor-fun>
                                          :slot slot
                                          :range-and-domain (cl:vector %unsigned-word-integer
                                                                       class))
                        ^(object)
                        ;;                        `(,%cast ,%unsigned-word-integer
                        ;;                                (,%div  (,%vector-class-instance-size ,^object)
                        ;;                                        ,(literal-instance %unsigned-word-integer (?byte-length-as-component  (?representation
                        ;;
                        ;;                                                                           (~vector-class-element-type class))))))
                        `(,%div  (,%vector-class-instance-size ,^object)
                                 (,%cast ,%unsigned-word-integer (,%size-as-component ,(~vector-class-element-type class))))
                        (tail-environment)))))



(defmethod ~compute-slot-writer
  (class slot effective-slots)
  (~compute-slot-writer-using-representation (?representation class) class slot
                                             effective-slots))


(defmethod ~compute-slot-writer-using-representation
  ((representation-object <%direct>)
   class slot effective-slots)
  ;; a writer for classes with representation direct isn't possible
  ())

(defmethod ~compute-slot-writer-using-representation
  ((representation-object <%pointer-to-struct>)
   class slot effective-slots)
  (add-function
   (complete-function (cl:make-instance <slot-accessor-fun>
                                        :slot slot
                                        :range-and-domain (cl:vector (~slot-description-type slot)
                                                                     class
                                                                     (~slot-description-type slot)))
                      ^(object new-value)
                      `(,^%setf (,^%select ,^object ,class; *UK* 20.01.94
                                           ,(~slot-description-name slot))
                                ,^new-value)
                      (tail-environment))))


(defmethod ~compute-slot-writer-using-representation
  ((representation-object <%pointer-to-vector>)
   class slot effective-slots)

  (if (eq ^element (~slot-description-name slot))
      (add-function
       (complete-function (cl:make-instance <slot-accessor-fun>
                                            :slot slot
                                            :range-and-domain (cl:vector (~slot-description-type slot)
                                                                         class %unsigned-word-integer
                                                                         (~slot-description-type slot)))
                          ^(object index new-value)
                          `(,^%setf (,%extract ,^object ,^index)
                                    ,^new-value)
                          (tail-environment)))
    ;;writer for length cannot be constructed
    ()
    ))

;;;-----------------------------------------------------------------------------
;;; ~compute-runtime-initialization initializes representation
;;;-----------------------------------------------------------------------------
(defmethod ~compute-runtime-initialization (class)
  (dynamic-let ((*class-initialization-forms* ()))
               (let ((representation-object  (?representation class)))
                 (mm-initialize class
                                representation-object
                                (?allocation representation-object)))
               (dynamic *class-initialization-forms*)))


;;;-----------------------------------------------------------------------------
;;; ~compute-allocator
;;;-----------------------------------------------------------------------------

(defmethod ~compute-allocator(class)
  (canonize-allocator class (?representation class)))

;;;-----------------------------------------------------------------------------
;;; general information about representation
;;;-----------------------------------------------------------------------------


(defun info()
  (mm-describe initialized-classes))

(defun mm-describe(l)
  (if (null? l)
      ()
    (progn
      (let* ((class (car l))
             (representation (?representation class))
             (mm-type (car (?value-list(?mm-type representation))))
             (mm-card (car (?value-list (?mm-card representation))))
             (allocation (symbol-name (?allocation representation))))
        (cl:format t "~%Class ~a has ~%~3Trepresentation ~a ~%~3Tmm-type: ~A ~%~3Tmm-card: ~A
~%~3Tallocation: ~a" (symbol-name (?identifier class))
(cl:class-name (cl:class-of representation)) mm-type mm-card allocation))
(mm-describe (cdr l)))))

#module-end
