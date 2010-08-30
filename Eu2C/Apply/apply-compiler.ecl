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
;;;  Title: The Main Part of the APPLY-Compiler
;;;  Description:
;;;  Authors:
;;;-----------------------------------------------------------------------------

#module apply-compiler
(import (level-1-eulisp
         lzs
         mzs-to-lzs
         (only (configurationp init-configuration-table)
               configuration)
         (only (ti-initialize update-all-type-expr-codes)
               ti-init)
         (only (expand-all-lattice-type-literals)
               ti-lattice)
         (only (ti-print-statistics)
               ti)
         (only (module-env $tail-module find-module load-module)
               el2lzs)
         (only (reset-%tail)
               tail-module)
         (only (reset-environments load-if-module)
               el2lzs-main) ; because importing from el2lzs doesn't function
         (only (export-objects)
               export)
         (only (reset-literals)
               expand-literal)
         (only (reset-literal-expanders)
               el2lzs-literals)
         (only (*frontend-errors* reset-frontend-errors)
               el2lzs-error)
         (only (create-predefined-standard-classes)
               lzs-mop)
         (only (initialize-predefined-standard-classes
                initialize-predefined-standard-classes-part-2
                handle-symbols
                )
               lzs-class-init)
         (only (reset-code-identifier)
               code-identifier)
         (only (init-mm-initialize)
               mm-initialize)
         (only (reset-generic-dispatch set-discriminating-functions)
               generic-dispatch)
         (only (set-apply-objects
                set-apply-level-1-objects
                set-apply-level-2-objects)
               apply-funs)
         (only (*compilation-type* *basic-system*)
               predicates)
         (only (?identifier)
               accessors)
         (only (mapc
                format
                unintern
                import
                find-package
                intern
                funcall
                symbol-function
                find-symbol
                string
                string-equal
                find-package)
               common-lisp))
 syntax (level-1-eulisp
         (only (return-from when)
               common-lisp))
 export (compile-application compilation-state))

(deflocal compilation-state ())

(defmacro call-module-function (module-name function-name . args)
  `(funcall (symbol-function (find-symbol ,(string function-name)
                                          (find-package ,(string module-name))))
            ,@args))

;;;-----------------------------------------------------------------------------
;;; main function to compile
;;;-----------------------------------------------------------------------------

(defun compile-application (module-name . basic-system)
  (when basic-system (setq basic-system (car basic-system)))
  (compile :application module-name basic-system))

(defun compile-basic-system (module-name . basic-system)
  (when basic-system (setq basic-system (car basic-system)))
  (compile :basic-system module-name basic-system))

(defun load-basic-system (compilation-type basic-system)
  ;; the name of the basic system -- if given -- is converted into a symbol to
  ;; make it sure that the default load path is used
  ;; the basic system is a string if taken from the Lisp-command-line
  (when (stringp basic-system)
        (setq basic-system (make-symbol basic-system)))

  (if *basic-system*
      ;; check the names for equality

      (check-basic-system-equality basic-system *basic-system*)

    ;; otherwise the basic system was already loaded
    ;; this is the case if a dump is created with a preloaded basic system
    (progn
      (setq *compilation-type* compilation-type)
      (setq *basic-system* basic-system) ; the initial value, will be updated later
      ;; to a module object

      ;;initialize configuration-table
      (init-configuration-table)

      (when *basic-system*
            ;; if a basic system is used
            ;; these variables must be set to () independent of the value given in the
            ;; configuration file
            (dynamic-setq *static-mm-type* ())
            (dynamic-setq *static-mm-card* ()))


      (format t "~%--- resetting the compiler...")
      (reset-compiler)

      (format t "~%--- loading basic modules")
      (load-basic-modules basic-system) ; sets *basic-system*
      t ; say that the basic system is ok
      )))

(defun compile (compilation-type module-name basic-system)
  ;; module-name may be a symbol, a string interpreted as a load path or the empty
  ;; string (on Mac only) where a file dialog appears

  (and

   (load-basic-system compilation-type basic-system)

   (let (module)          ;;module contains the main module


     (when (eq compilation-state ^init)
           (return-from compile module-env))

     (format t "~%--- loading application modules")
     (setq module (load-application-modules module-name))

     (when (> *frontend-errors* 0)
           (return-from compile ()))

     (format t "~%--- handle symbol environment...")
     (handle-symbols module module-env)

     (format t "~%--- computing discriminating functions...")
     (computing-discriminating-functions module-env)

     (format t "~%--- marking all exported bindings...")
     (export-objects module module-env)

     ;; now the deflocal-variable module-env (module el2lzs) contains the list of
     ;; all direct or indirect loaded application modules

     (format t "~%--- converting to MZS ")

     ;; Signatures are read in before all defined types are included into the
     ;; lattice. Thus before inference starts the type expression codes have to
     ;; be updated.
     (update-all-type-expr-codes)

     ;; After the literal expander are defined the collected literals in the
     ;; lattice type definitions (%define-latticef-type ...) can be expanded.
     (expand-all-lattice-type-literals)

     (call-module-function lzs2mzs lzs2mzs module-env)

     (when (eq compilation-state ^mzs)
           (return-from compile module-env))

     ;; remember: now the variable module contains a list

     (format t "~%--- converting MZS to LZS...")
     (mzs2lzs-4-modules module-env)
     ;; (call-module-function mzs2lzs-4-modules mzs-to-lzs module-env)
     (format t "~%--- generating C-code")
     (call-module-function code-generator generate-code
                           module module-env)

     (format t "~%--- end of compilation ---~%")

     ;; Print out type inference statistics if debugging::ti-verbose is set.
     (ti-print-statistics)

     module-env
     "end of compilation"
     )))

;;;-----------------------------------------------------------------------------
;;;
;;;-----------------------------------------------------------------------------

(defun reset-compiler ()
  (reset-frontend-errors)
  (reset-%tail)
  (create-predefined-standard-classes)
  (ti-initialize)        ;; initialization of the type inference
  (reset-environments)
  (reset-literals)
  (reset-literal-expanders)
  (reset-code-identifier)
  (reset-generic-dispatch)
  (initialize-predefined-standard-classes)
  )

(defun load-basic-modules (basic-system)
  ;; the result is the module describing the basic system or () if compilation is
  ;; done from scratch
  (dynamic-let ((*info-level* (dynamic *system-info-level*)))
               (if (null? basic-system)
                   (progn
                     (setq *basic-system* ())
                     (load-module ^apply-level-1)            ; load the most basic things
                     (set-apply-level-1-objects)             ; provide basic things to the compiler

                     (init-mm-initialize)
                     (initialize-predefined-standard-classes-part-2)

                     (load-module ^apply-level-2)            ; load def of <cons>, now predicates can be constructed
                     (set-apply-level-2-objects)             ; provide secons layer of basic things
                     ;;(load-module ^tail)                    ; load modules apply and tail
                     (set-apply-objects (load-module ^apply)); binds some variables with objects
                     ;; available in apply.am)
                     ()
                     )
                 (progn
                   (setq *basic-system* (load-if-module basic-system))
                   (set-apply-level-1-objects)
                   (set-apply-level-2-objects)
                   (set-apply-objects *basic-system*)
                   )
                 )))

(defun load-application-modules (module-name)
  (load-module module-name))

(defun computing-discriminating-functions (module-env)
  (mapc #'set-discriminating-functions module-env))

(defun check-basic-system-equality (basic-system-id preloaded-basic-system)
  (if (string-equal (string basic-system-id)
                    (string (?identifier preloaded-basic-system)))
      t
    (progn
      (format t "~%*** ERROR ***~
                 Eu2C with preloaded basic system ~A ~
                 cannot provide the required basic system ~A"
              (?identifier preloaded-basic-system)
              basic-system-id)
      )))


#module-end
