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
;;;  Title:
;;;  Description:
;;;  Authors:
;;;-----------------------------------------------------------------------------
(in-package cl-user)

;;;-----------------------------------------------------------------------------
;;; apply packages for compilation
;;;-----------------------------------------------------------------------------
(defvar *apply-packages*
  '((:eulisp el-modules
             eulisp-kernel
             character
             collection
             condition-0
             condition
             compare
             double
             null
             number
             pair
             spint
             el-stream
             string
             symbol
             table
             vector
             level-0
             pair-ext
             list-ext
             dynamic
             control1
             level-1
             class-ext)
    (:lisp defstandardclass
           simple-programming)
    (:zs   accessors
           lzs
           mzs)
    (:general debugging
              option-lists
              compiler-conditions
              configuration)
    (:frontend el2lzs-basic
               el2lzs-error
               el2lzs-load
               el2lzs-main
               lzs-modules
               el2lzs

               lzs-mop
               whc-classes
               expand-literal
               ;;whc-aux
               whc-basic-data-types
               rr-md-read
               ;;machine-description
               tail-module
               apply-funs
               eval
               whc-definitions
               representation

               el2lzs-rules
               el2lzs-classes
               el2lzs-generic
               el2lzs-literals

               annotate

               standard-mop
               standard-init
               mm-initialize
               lzs-class-init)
    (:generic inline-method
              generic-dispatch)
    (:ti ti
         ti-codes
         ti-lattice

         ti-exprs
         ti-meet-join
         ti-eqs

         name-of-fun

         ti-write
         ti-copy
         ti-unify
         ti-signature
         ti-const
         type-inference

         ti-init
         type-propagation)
    (:mzs side-effects-h
          context
          analyse-h
          progn-context
          type-propagation
          side-effects
          lzs-to-mzs-fun
          function-call-context
          gutter
          inline
          function-call
          if-form
          move
          letstar-form
          setq-form
          arg-context
          function-label
          join-label-context
          switch-context
          test-context
          void-context
          cleartypes
          lzs2mzs)
    (:codegen mzs-to-lzs
              code-identifier
              c-typing
              c-data
              c-code-syntax
              c-code)
    (:top code-generator
          generate-header-file
          generate-def-file
          apply-compiler)
    )) ; end of *apply-packages*

;;;-----------------------------------------------------------------------------
;;; compile-apply
;;;-----------------------------------------------------------------------------
(defun compile-apply (&rest packages)
  (load-apply :lisp)
  (unless packages
    (setq packages (mapcar #'car *apply-packages*)))
  (mapc #'(lambda (package)
            (mapc #'compile-module
                  (cdr (assoc package *apply-packages*))))
        packages))

(defun compile-module (name)
  (let ((file (some #'(lambda (path)
                        (or
                         (probe-file
                          (merge-pathnames
                           (make-pathname :name (string-downcase
                                                 (string name))
                                          :type "ecl")
                           path))
                         (probe-file
                          (merge-pathnames
                           (make-pathname :name (string-downcase
                                                 (string name))
                                          :type "lisp")
                           path))))
                    *eulisp-module-search-path*)))
    (and file (compile-file file :verbose t))))

;;;-----------------------------------------------------------------------------
;;  End of compile-apply
;;;-----------------------------------------------------------------------------
