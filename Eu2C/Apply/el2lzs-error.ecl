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
;;;  Title: Signalling errors, warnings and infos for specific situations detected by
;;    the frontend
;;;  Description:
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Ingo Mohr
;;;-----------------------------------------------------------------------------

#module el2lzs-error
(import (eulisp1
         lzs
         accessors
         configuration; only to get initialization of dynamic *[system-]info-level*
         binding
         debugging
         messages
         (only (first
                second
                mapcar)
               common-lisp))
 syntax (eulisp1
         apply-standard)
 export (*frontend-errors*
         frontend-message
         new-frontend-error
         reset-frontend-errors
         eval-fun
         eval-args)
 ;; ---- stuff for bindings ; should be temporary
 export (<binding>
         finally-refered-object
         get-lzs-object))

;;;-----------------------------------------------------------------------------
;;; general
;;;-----------------------------------------------------------------------------

(deflocal *frontend-errors* 0)

(defun new-frontend-error ()
  (setq *frontend-errors* (+ 1 *frontend-errors*)))

(defun reset-frontend-errors ()
  (setq *frontend-errors* 0))

(defvar current-defining-form ())

(defun frontend-message (key message . args)
  (when (eq key ^error) (new-frontend-error))
  (apply #'write-message key message args))

(defvar eval-fun ())
(defvar eval-args ())

;;;-----------------------------------------------------------------------------
;;; used by el2lzs-main
;;;-----------------------------------------------------------------------------

(export
 error-bad-c-import-spec
 error-bad-module-directive
 error-no-lexical-binding
 error-name-clash
 error-bad-old-style-syntax-import
 info-old-style-module-header
 error-invalid-syntax
 error-redefinition-of-top-lexical
 error-redefinition-of-imported-lexical
 warning-non-existent-binding-in-interface
 warning-binding-in-import-and-syntax
 )

(defvar error-if-no-lexical-found t)

(defun error-no-lexical-binding (id)
  (when (dynamic error-if-no-lexical-found)
        (frontend-message ^error "no lexical binding found for ~A" id))
  ())

(defun error-bad-c-import-spec (file)
  (frontend-message ^error "bad filespec in c-import directive: ~S" file))

(defun error-bad-module-directive (key value)
  (frontend-message ^error "bad module directive: ~A ~@[~A~]" key value))

(defun error-bad-old-style-syntax-import (import-spec)
  (frontend-message ^error "bad old style syntax import specification: ~A" import-spec))

(defgeneric error-name-clash (binding1 binding2))
(defmethod error-name-clash (binding1 binding2)
  (frontend-message ^error "name clash: attempt to import ~A ~@
                         from modules ~A and ~A"
                    (?identifier binding1)
                    (?module-id binding1)
                    (?module-id binding2)))
(defmethod error-name-clash ((binding1 <binding>) (binding2 <binding>))
  (frontend-message ^error "name clash: attempt to import under ~A~@
                         ~A from ~A and ~@
                         ~A from ~A"
                    (?identifier binding2)
                    (?identifier (finally-refered-object binding1))
                    (?module-id (finally-refered-object binding1))
                    (?identifier (finally-refered-object binding2))
                    (?module-id (finally-refered-object binding2))
                    ))
(defmethod error-name-clash ((binding1 <binding>) binding2)
  (frontend-message ^error "name clash: attempt to import under ~A~@
                         ~A from ~A and ~@
                         ~A from ~A"
                    (?identifier binding2)
                    (?identifier (finally-refered-object binding1))
                    (?module-id (finally-refered-object binding1))
                    (?identifier binding2)
                    (?module-id binding2)
                    ))
(defmethod error-name-clash (binding1 (binding2 <binding>))
  (error-name-clash binding2 binding1))

(defun warning-non-existent-binding-in-interface (identifier import-spec)
  (frontend-message ^warning
                    "~A is not available in interface specification:~%~A"
                    identifier import-spec))

(defun warning-binding-in-import-and-syntax (identifier import-spec)
  (frontend-message ^warning
                    "~A is both import and syntax in interface specification:~
                  ~%~A~
                  ~%using import only"
                    identifier import-spec))

(defun info-old-style-module-header (module-def)
  (frontend-message ^warning "old style module header: ~A"
                    (list (first module-def)
                          (second module-def) ^|...|)))

(defun error-invalid-syntax (syntax expr)
  (frontend-message ^error "invalid syntax for '~(~A~)' in~%~(~A~)"
                    (car syntax) expr))

(defun error-redefinition-of-top-lexical (object)
  (frontend-message ^error
                    "redefinition of top lexical binding: ~A"
                    (?identifier object)))

(defun error-redefinition-of-imported-lexical (object)
  (frontend-message ^error
                    "redefinition of imported lexical binding: ~A"
                    (?identifier object)))

;;;-----------------------------------------------------------------------------
;;; used by el2lzs-rules
;;;-----------------------------------------------------------------------------

(export
 error-invalid-assignment
 error-invalid-slot-name)

(defun error-invalid-assignment (location)
  (frontend-message ^error
                    "invalid destination for assignment: ~A" location))

(defun error-invalid-slot-name (class-id slot-id)
  (frontend-message ^error
                    "invalid slot name ~A for accessing class ~A"
                    slot-id class-id))

;;;-----------------------------------------------------------------------------
;;; used by el2lzs-load
;;;-----------------------------------------------------------------------------
(export
 error-invalid-module-definition
 warning-differing-names-for-module-and-file
 info-loading-module
 info-module-loaded
 error-cannot-find-file
 error-cannot-load-file
 error-cannot-open-file)

(defun error-invalid-module-definition (object path)
  (frontend-message ^error "invalid module definition in ~A" path))

(defun warning-differing-names-for-module-and-file (module-name path)
  (frontend-message ^warning "modulename ~A differs from filename ~A"
                    module-name
                    path))

(defvar *load-level* 0)

(defun info-loading-module (path)
  (frontend-message ()
                    "~[~;.~:;~%;~V@{.~}loading module ~A~]"
                    (dynamic *info-level*)
                    (- (dynamic *load-level*) 1) path))

(defun info-module-loaded (module-name)
  (frontend-message ()
                    "~[~;~;~;~%;~V@{.~}apply module ~A loaded~]"
                    (dynamic *info-level*)
                    (- (dynamic *load-level*) 1)
                    module-name))

(defun error-cannot-find-file (name)
  (frontend-message ^error "can't find Apply module ~A" name))

(defun error-cannot-load-file (path)
  (frontend-message ^error "can't load Apply module ~A" path))

(defun error-cannot-open-file (path)
  (frontend-message ^error "can't open Apply module ~A" path))

;;;-----------------------------------------------------------------------------
;;; used by el2lzs-classes
;;;-----------------------------------------------------------------------------

(export
 error-bad-class-options
 error-class-equal-lattice-type
 error-neither-class-nor-lattice-type
 error-redefinition-of-inherited-keyword
 error-invalid-gf-for-add-method)

(defun error-bad-class-options (options)
  (frontend-message ^error "bad class option in ~A" options))

(defun error-class-equal-lattice-type (id)
  (frontend-message ^error
                    "A class and a lattice type with the name ~A are found.~
                  Using the class." id))

(defun error-neither-class-nor-lattice-type (id)
  (frontend-message
   ^error
   "Neither a class nor a lattice type with the name ~A are found."
   id))

(defun error-invalid-gf-for-add-method (x)
  (frontend-message ^error
                    "~A was given to add-method instead of a generic function"
                    x))

;;;-----------------------------------------------------------------------------
;;; used by el2lzs-generic
;;;-----------------------------------------------------------------------------

(export
 warn-defgeneric-options-not-analyzed
 error-no-converter
 error-no-setter
 error-class-required-in-converter-spec
 error-function-required-in-setter-spec
 error-invalid-generic-function-spec
 error-converter-redefinition
 error-setter-redefinition
 )

(defun warn-defgeneric-options-not-analyzed (options)
  (frontend-message ^warn
                    "options in defgeneric not yet analyzed: ~A"
                    options))

(defun error-no-converter (class-def)
  (frontend-message ^error
                    "no converter available for class ~(~A [~A]~)"
                    (?identifier class-def)
                    (?module-id class-def)))

(defun error-no-setter (fun)
  (frontend-message ^error
                    "no converter available for function ~(~A [~A]~)"
                    (?identifier fun)
                    (?module-id fun)))

(defun error-class-required-in-converter-spec (gf-spec)
  (frontend-message ^error
                    "a class is required in converter specification ~(~A~)"
                    gf-spec))

(defun error-function-required-in-setter-spec (gf-spec)
  (frontend-message ^error
                    "a function is required in setter specification ~(~A~)"
                    gf-spec))

(defun error-invalid-generic-function-spec (gf-spec)
  (frontend-message ^error
                    "invalid key in generic function specification ~(~A~)"
                    gf-spec))

(defun error-converter-redefinition (class-def)
  (frontend-message ^error
                    "attempt to redefine previously defined converter for ~(~A [~A]~)"
                    (?identifier class-def)
                    (?module-id class-def)))

(defun error-setter-redefinition (fun)
  (frontend-message ^error
                    "attempt to redefine previously defined setter for ~(~A [~A]~)"
                    (?identifier fun)
                    (?module-id fun)))

;;;-----------------------------------------------------------------------------
;;; used by eval
;;;-----------------------------------------------------------------------------

(export
 eval-error-expecting-function
 eval-error-cannot-interpret-function
 eval-error-variable-without-value
 eval-error-too-few-arguments
 eval-error-too-many-arguments
 eval-error-undefined-interpreter
 eval-error-special-form-not-implemented)

(defun eval-error-expecting-function (obj)
  (frontend-message ^error
                    "a function must be given instead of ~A~%during evaluation of~%~A"
                    obj
                    (eval-expr))
  ())

(defun eval-error-cannot-interpret-function (fun)
  (frontend-message ^error
                    "cannot interpret function ~A~%during evaluation of~%~A"
                    (?identifier fun)
                    (eval-expr))
  ())

(defun eval-error-variable-without-value (var)
  (frontend-message ^error
                    "global binding ~A has an expression as initial value~%during evaluation of~%~A"
                    (?identifier var)
                    (eval-expr))
  ())

(defun eval-error-too-few-arguments ()
  (frontend-message ^error "too few arguments~%during evaluation of~%~A"
                    (eval-expr)))

(defun eval-error-too-many-arguments ()
  (frontend-message ^error "too many arguments~%during evaluation of~%~A"
                    (eval-expr)))

(defun eval-error-undefined-interpreter (identifier)
  (frontend-message ^error
                    "undefined interpreter ~A"
                    identifier))

(defun eval-error-special-form-not-implemented (id)
  (frontend-message ^error
                    "Special form '~A' not yet implemented.~
                  ~%during evaluation of~%~A"
                    id
                    (eval-expr)))

(defun eval-expr ()
  (cons (if (fun? (dynamic eval-fun))
            (?identifier (dynamic eval-fun))
          (dynamic eval-fun))
        (dynamic eval-args)))

;;;-----------------------------------------------------------------------------
;;; used by annotate
;;;-----------------------------------------------------------------------------

(export
 error-invalid-object-for-annotate
 error-invalid-key-for-annotate)

(defun error-invalid-object-for-annotate (type)
  (frontend-message ^error
                    "a ~A is expected"
                    type))

(defun error-invalid-key-for-annotate (key)
  (frontend-message ^error
                    "~A is an invalid key" key))

;;;-----------------------------------------------------------------------------
;;; used by apply-funs
;;;-----------------------------------------------------------------------------

(export
 error-invalid-special-specification
 error-invalid-key-for-%provide-compiler-info)

(defun error-invalid-special-specification (keyword type)
  (frontend-message ^warning
                    "~A is an invalid specification for annotation ~(~A~)"
                    type keyword))

(defun error-invalid-key-for-%provide-compiler-info (keyword)
  (frontend-message ^error
                    "~A is an invalid key"
                    keyword))

;;;-----------------------------------------------------------------------------
;;;
;;;-----------------------------------------------------------------------------

(export
 error-non-congruent-lambda-lists
 error-incompatible-method-domain
 error-method-domain-clash)

(defun error-non-congruent-lambda-lists (method gf)
  (frontend-message ^error
                    "non congruent lambda lists between generic function ~A~
                     ~%and its method ~A"
                    (?identifier gf)
                    (mapcar #'?identifier (?domain method))))

(defun error-incompatible-method-domain (method gf)
  (frontend-message ^error
                    "incompatible domain between generic function ~A~
                     ~%with signature ~A~
                     ~%and its method ~A"
                    (?identifier gf)
                    (mapcar #'?identifier (?domain gf))
                    (mapcar #'?identifier (?domain method))))

(defun error-method-domain-clash (method gf)
  (frontend-message ^error
                    "method redefinition for generic function ~A~
                     ~%method signature is ~A"
                    (?identifier gf)
                    (mapcar #'?identifier (?domain method))))

#module-end
