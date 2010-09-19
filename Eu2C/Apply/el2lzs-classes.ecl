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
;;; Title: Transformation of class definitions into LZS
;;;  Description:
;;;  Authors: Ingo Mohr
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

#module el2lzs-classes
(import (level-1
         el2lzs-rules
         el2lzs-error
         option-lists
         pair-ext
         list-ext
         tail-module
         lzs-mop
         quasiquote
         (only (call)
               lzs-eval)
         (only (get-defined-lattice-type
                add-lattice-type
                trans-lattice-type-list)
               ti-lattice)
         (only (mapcar
                nconc
                mapc
                warn
                list*
                reverse
                vector
                append)
               common-lisp))
 syntax (level-1
         el2lzs-main
         (only (prog1)
               common-lisp))
 export (get-class-or-lattice-type
         get-lattice-type))

;;;-----------------------------------------------------------------------------
;;; TS (transsyn)
;;;-----------------------------------------------------------------------------
(deftranssyn (%define-standard-class class-spec superclass
               slot-specs . class-options)
  (with-defining-form
   (unless (check-options ()
                          ^(keywords representation allocation
                                     direct-super-lattice-types)
                          ^(constructor predicate)
                          class-options)
           (error-bad-class-options class-options))
   (transsyn-slot-specs slot-specs)
   (whole-form)))

(deftranssyn (%define-tail-class class-spec
               slot-specs . class-options)
  (with-defining-form
   (unless (check-options ()
                          ^(keywords representation allocation
                                     direct-super-lattice-types)
                          ^(constructor)
                          class-options)
           (error-bad-class-options class-options))
   (transsyn-slot-specs slot-specs)
   (whole-form)))

(deftranssyn (%define-abstract-class class-spec superclass
               slot-specs . class-options)
  (with-defining-form
   (unless (check-options ()
                          ^(keywords direct-super-lattice-types)
                          ^(predicate)
                          class-options)
           (error-bad-class-options class-options))
   (transsyn-slot-specs slot-specs)
   (whole-form)))

(deftranssyn (%define-metaclass class-spec superclass
               slot-specs . class-options)
  (with-defining-form
   (unless (check-options ()
                          ^(keywords representation allocation
                                     direct-super-lattice-types)
                          ^(constructor predicate)
                          class-options)
           (error-bad-class-options class-options))
   (transsyn-slot-specs slot-specs)
   (whole-form)))

(defun transsyn-slot-specs (slot-specs)
  (mapc (lambda (slot-spec)
          (check-options () ^(default type keyword) ^(reader writer accessor)
                         (cdr slot-spec))
          (replace-option-value #'transsyn ^default (cdr slot-spec)))
        slot-specs))

;;;-----------------------------------------------------------------------------
;;; TM (transmod)
;;;-----------------------------------------------------------------------------

(deftransmod (%define-standard-class class-spec superclass
               slot-specs . class-options)
  (transmod-%define-class <standard-class-def>
                          class-spec superclass
                          slot-specs class-options))

(deftransmod (%define-tail-class class-spec
               slot-specs . class-options)
  (transmod-%define-class <tail-class-def>
                          class-spec ()
                          slot-specs class-options))

(deftransmod (%define-abstract-class class-spec superclass
               slot-specs . class-options)
  (transmod-%define-class <abstract-class-def>
                          class-spec superclass
                          slot-specs class-options))

(deftransmod (%define-metaclass class-spec superclass
               slot-specs . class-options)
  (transmod-%define-class <metaclass-def>
                          class-spec superclass
                          slot-specs class-options))

(defun transmod-%define-class (compiler-class class-spec superclass
                                              slot-specs class-options)
  (let* ((ID (first class-spec))
         (class (make-instance compiler-class :identifier ID)))
    (add-class class)
    (nconc (list class)
           (transmod-slot-specs slot-specs)
           (transmod-class-options class-options))))

(defun transmod-slot-specs (slot-specs)
  (let ((accessor-bindings ()))
    (mapc (lambda (slot-spec)
            (map-option-list
             (lambda (key value)
               (when (member key ^(reader writer accessor))
                     (push (add-const (make-instance <defined-named-const>
                                                     :identifier value))
                           accessor-bindings)))
             (cdr slot-spec)     ; the slot option list
             ))
          slot-specs)
    accessor-bindings))

(defun transmod-class-options (class-options)
  (let ((functions ()))
    (map-option-list
     (lambda (key value)
       (cond ((eq key ^predicate)
              (push (add-const (make-instance <defined-named-const>
                                              :identifier value))
                    functions))
             ((eq key ^constructor)
              (push (add-const (make-instance <defined-named-const>
                                              :identifier (car value)))
                    functions))
             (t ())))
     class-options)
    functions))

;;;-----------------------------------------------------------------------------
;;; TD (transdef)
;;;-----------------------------------------------------------------------------

(deftransdef (%define-standard-class class-spec superclass
               slot-specs . class-options)
  (with-defining-form
   (transdef-%define-class class-spec superclass slot-specs class-options)))

(deftransdef (%define-tail-class class-spec
               slot-specs . class-options)
  (with-defining-form
   (transdef-%define-class class-spec () slot-specs class-options)))

(deftransdef (%define-abstract-class class-spec superclass
               slot-specs . class-options)
  (with-defining-form
   (transdef-%define-class class-spec superclass slot-specs class-options)))

(deftransdef (%define-metaclass class-spec superclass
               slot-specs . class-options)
  (with-defining-form
   (transdef-%define-class class-spec superclass slot-specs class-options)))

(defun transdef-%define-class (class-spec superclass slot-specs class-options)
  ;; This function must fill the class definition and the function definitions
  ;; for the predicate, the constructor and for all slot accessors
  (let* ((id (first class-spec))
         (metaclass (second class-spec))
         (class-def (find-in-lex-env id))
         (keywords (get-option ^keywords class-options ()))
         (supers (if superclass
                     (list (find-in-lex-env superclass))
                   ())))
    (setf (?class class-def) (find-in-lex-env metaclass))
    (mapc #'make-defined-sym keywords)
    (~initialize class-def
                 (list ^name id
                       ^direct-superclasses supers
                       ^direct-slots (mapcar
                                      (lambda (s)
                                        (make-slot-spec class-def s))
                                      slot-specs)
                       ^direct-keywords (append
                                         (slot-keywords slot-specs)
                                         keywords)
                       ^representation (get-option
                                        ^representation class-options ())
                       ^allocation (get-option ^allocation class-options ())
                       ^direct-super-lattice-types
                       (trans-lattice-type-list
                        (get-option ^direct-super-lattice-types
                                    class-options ()))))
    (bind-slot-accessors class-def slot-specs)
    (let ((init-forms
           (reverse (~compute-runtime-initialization class-def))))
      ;; allocator and constructors can be created only
      ;; after ~compute-runtime-initialization
      (compute&bind-class-functions class-def class-options)
      init-forms)
    ))

(defun bind-slot-accessors (class-def slot-specs)
  (mapc (lambda (slot-spec)
          (let ((slot (~find-slot class-def (car slot-spec))))
            (map-option-list
             (lambda (key value)
               (bind-slot-accessor key value slot))
             (cdr slot-spec))
            (name-slot-accessors slot)))
        slot-specs))

(defun bind-slot-accessor (accessor-type name slot)
  (let ((accessor ()))
    (cond ((eq accessor-type ^reader)
           (setq accessor (~slot-slot-reader slot)))
          ((eq accessor-type ^accessor)
           (setq accessor (~slot-slot-reader slot))
           (setf (?setter accessor)
                 (~slot-slot-writer slot))
           (unless (?identifier (?setter accessor))
                   (setf (?identifier (?setter accessor)) (list ^setter name))))
          ((eq accessor-type ^writer)
           (setq accessor (~slot-slot-writer slot))))
    (when accessor
          (setf (?value (find-in-lex-env name)) accessor)
          ;; the following installs a listed identifier to avoid that two
          ;; objects have the same name (a constant with accessor as value has
          ;; already the same name)
          (unless (?identifier accessor)
                  (setf (?identifier accessor) (list name)))
          )))

(defun name-slot-accessors (slot)
  ;;installs default names for accessors not explicitely named
  (when (and (~slot-slot-reader slot)
             (null? (?identifier (~slot-slot-reader slot))))
        (setf (?identifier (~slot-slot-reader slot))
              (list ^reader (~slot-name slot)
                    ^of (?identifier (?slot-of slot))
                    )))
  (when (and (~slot-slot-writer slot)
             (null? (?identifier (~slot-slot-writer slot))))
        (setf (?identifier (~slot-slot-writer slot))
              (list ^setter
                    (~slot-name slot)
                    ^of
                    (?identifier (?slot-of slot))
                    ))))

(defun compute&bind-class-functions (class-def class-options)
  ;; ATTN: the allocator has to be created before any constructor
  ;;       is computed
  (setf (?allocator class-def) (~compute-allocator class-def))
  (map-option-list
   (lambda (key value)
     (bind-class-function key value class-def))
   class-options))

(defun bind-class-function (function-type spec class)
  (let (function name)
    (cond ((eq function-type ^predicate)
           (setq function (~compute-predicate class))
           (setq name spec))
          ((eq function-type ^constructor)
           (setq function (~compute-constructor class (cdr spec)))
           (setq name (car spec))))
    (when function
          (setf (?value (find-in-lex-env name))
                function)
          ;; the following installs a listed identifier to avoid that two
          ;; objects have the same name
          (unless (?identifier function)
                  (setf (?identifier function) (list name)))
          )))

(defun make-slot-spec (class slot-spec)
  (let* ((name (car slot-spec))
         (type-option (find-option ^type (cdr slot-spec) ()))
         (type (and type-option (find-in-lex-env (car type-option))))
         (default-option (find-option ^default (cdr slot-spec) ()))
         (keyword-option (find-option ^keyword (cdr slot-spec) ()))
         (reader-option (or (find-option ^reader (cdr slot-spec) ())
                            (find-option ^accessor (cdr slot-spec) ())))
         (writer-option (or (find-option ^writer (cdr slot-spec) ())
                            (find-option ^accessor (cdr slot-spec) ())))
         )
    (nconc (list ^name name
                 ^reader reader-option  ; used as a flag only for tail classes
                 ^writer writer-option) ; used as a flag only for tail classes
           (when type-option
                 (list ^type type))
           (when default-option
                 (list ^default-function
                       (create-slot-init-function class name type
                                                  (car default-option))))
           (when keyword-option
                 (make-defined-sym (car keyword-option))
                 (list ^keyword
                       (car keyword-option))))))

(defun create-slot-init-function (class slot-name slot-type default)
  (let ((init-fun
         (make-instance <slot-init-fun>
                        :identifier (list ^init slot-name
                                          ^of (?identifier class)))))
    (when slot-type
          (setf (?range-and-domain init-fun) (vector slot-type)))
    (add-function          ;; install it as a global function
     (complete-function init-fun
                        ()
                        default
                        (dynamic lex-env)))))

(defun slot-keywords (slot-specs)
  (if (null? slot-specs) ()
    (let ((keyword-option (find-option ^keyword (cdar slot-specs) ())))
      (if keyword-option
          (cons (car keyword-option)
                (slot-keywords (cdr slot-specs)))
        (slot-keywords (cdr slot-specs))))))

;;;-----------------------------------------------------------------------------
;;; %declare-external-class
;;;-----------------------------------------------------------------------------
(deftranssyn (%declare-external-class class-spec superclasses
               slot-specs . class-options)
  (with-defining-form
   (unless (check-options ^(type-identifier
                            representation)
                          ^(converter
                            object-identifier
                            direct-super-lattice-types
                            keywords
                            language)
                          ()
                          class-options)
           (error-bad-class-options class-options))
   (transsyn-x-slot-specs slot-specs)
   (whole-form)))

(defun transsyn-x-slot-specs (slot-specs)
  (mapc (lambda (slot-spec)
          (check-options ()
                         ^(type keyword c-identifier)
                         ()
                         (cdr slot-spec)))
        slot-specs))

(deftransmod (%declare-external-class class-spec superclasses
               slot-specs . class-options)
  (let* ((ID (first class-spec))
         (class (make-instance <imported-class> :identifier ID)))
    (add-class class)
    (list class)))

(deftransdef (%declare-external-class class-spec superclasses
               slot-specs . class-options)
  ;; the class option 'language' isn't used yet
  (with-defining-form
   (let* ((id (first class-spec))
          (metaclass (second class-spec))
          (class-def (find-in-lex-env id))
          (supers (mapcar #'find-in-lex-env superclasses)))
     (setf (?class class-def) (find-in-lex-env metaclass))
     (setf (?code-identifier class-def)
           (get-option ^object-identifier class-options ()))
     (setf (?type-identifier class-def)
           (get-option ^type-identifier class-options ()))
     (~initialize class-def
                  (list ^name id
                        ^direct-superclasses supers
                        ^effective-slots
                        (mapcar (lambda (s)
                                  (make-x-slot-spec class-def s))
                                slot-specs)
                        ^direct-keywords (append
                                          (slot-keywords slot-specs)
                                          (get-option ^keywords class-options ()))
                        ^direct-super-lattice-types
                        (trans-lattice-type-list
                         (get-option ^direct-super-lattice-types class-options
                                     ()))
                        ^converter (trans (get-option ^converter class-options ()))
                        ^representation (get-option ^representation class-options ())
                        ))
     ; no initialization form is needed
     ())))

(defun make-x-slot-spec (class slot-spec)
  (let* ((name (car slot-spec))
         (type-option (find-option ^type (cdr slot-spec) ()))
         (type (and type-option (find-in-lex-env (car type-option))))
         (keyword-option (find-option ^keyword (cdr slot-spec) ()))
         (c-identifier-option (find-option ^c-identifier (cdr slot-spec) ()))
         )
    (nconc (list ^name name)
           (when type-option
                 (list ^type type))
           (when keyword-option
                 (list ^keyword
                       (car keyword-option)))
           (when c-identifier-option
                 (list ^c-identifier (car c-identifier-option))))))

;;;-----------------------------------------------------------------------------
#module-end
;;;-----------------------------------------------------------------------------
