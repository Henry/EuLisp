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
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors:
;;;-----------------------------------------------------------------------------
(in-package COMMON-LISP-USER)

;;;-----------------------------------------------------------------------------
;;; definition of APPLY load states
;;;-----------------------------------------------------------------------------
(defconstant $load-states
  `(
    ;; (key entry...)
    ;; entry = pathname    (load pathname)
    ;; entry = symbol      (require-eulisp-module symbol)
    ;; entry = list        (eval list)

    (:eulisp ,(make-pathname :directory `(,@$applyroot "EclModules")
                             :name "el-modules")
             level-1)
    (:lisp ,(make-pathname :directory `(,@$applyroot "Apply")
                           :name "defstandardclass")
           ,(make-pathname :directory `(,@$applyroot "Apply")
                           :name "simple-programming")
           )
    (:zs accessors
         lzs
         mzs)
    (:tail el2lzs
           el2lzs-rules
           el2lzs-classes
           el2lzs-generic
           el2lzs-literals
           standard-mop
           standard-init
           lzs-class-init
           annotate
           whc-definitions ; initialization
           mm-initialize ; initialization
           )
    (:ti type-inference)
    (:mzs   lzs2mzs)
    ;;(:asm mzs2asm)
    (:full code-generator
           apply-compiler)
    )) ; end of defconstant load-states

;;;-----------------------------------------------------------------------------

(defun load-apply (&optional (key (caar (last $load-states))))
  (if (member key $load-states :key #'car)
      (load-apply-states key $load-states)
    (error "undefined load-state for APPLY: ~A" key)))

(defun load-apply-states (key states)
  (cond ((null states) nil)
        ((eq key (caar states))
         (load-state key (cdar states)))
        (t
         (load-state (caar states) (cdar states))
         (load-apply-states key (cdr states)))))

(defun load-state (key load-specifications)
  (unless (get key 'load-apply)
    (mapc #'apply-load load-specifications)
    (setf (get key 'load-apply) t)
    ))

(defmethod apply-load (load-specification)
  (eval load-specification))

(defmethod apply-load ((load-specification pathname))
  (load load-specification))

(declaim (ftype function require-eulisp-module))

(defmethod apply-load ((load-specification symbol))
  (require-eulisp-module load-specification))
