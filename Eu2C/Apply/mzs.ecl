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
;;; Title:
;;;  Description:
;;;  Authors:
;;;-----------------------------------------------------------------------------

#module mzs
(import (level-0
         apply-standard
         accessors
         lzs)
 syntax (level-0
         apply-standard)
 expose (accessors))

(defun make-supers (supers)
  (if (null? supers) '(<mzs-object>)
    (list (make-eulisp-class-id (car supers)))))

(defmacro def-mzs-object (name super . slots)
  `(progn
     (defstandardclass
       ,(make-eulisp-class-id name)
       ,(make-supers super)
       ,@(make-structure-and-annotation-slots slots)
       :predicate)
     (export
      ,(make-eulisp-class-id name)
      ,(make-predicate-name name))))


;;;-----------------------------------------------------------------------------

(defstandardclass <mzs-object> () )     ; the top node

(export <mzs-object> mzs-object?)

;;;-----------------------------------------------------------------------------
;;; definition of basic blocks
;;;-----------------------------------------------------------------------------

(def-mzs-object block (mzs-object)
  (in-label  :initform ())              ; label or branch
  (out-label :initform ())              ; lable or ()
  (body :initform ())        ; list of function-call or move's
  (interface :initform ())   ; list of move's
  (result :initform ())  ;; a void, local-variable, test, switch,
  ;; return or a goto.
  :Annotations
  ;;-----------
  env;; environment of variables
  (t-path :initform ()) ;; type-pathes
  ;; deleted from *hf*
  ;;  read-glocs            ;; environment of reads of global
  ;;                        ;; locations
  ;;  write-glocs             ;; environment of set's of global
  ;;                        ;; locations
  (in-binding :initform ())           ;; for register-allocation
  (out-binding :initform ())
  (kill-binding :initform ())
  (code-identifier :initform ())       ; identifier for code generation
  (successor-blocks :initform ())
  (else-interface :initform ())
  )

;;;-----------------------------------------------------------------------------
;; definition of branches
;;;-----------------------------------------------------------------------------
;; these are defined in statements !!
;;;-----------------------------------------------------------------------------
;;(def-mzs-object test (mzs-object)
;; function               ;; a predicate (%eq, %neq etc. )
;; block                  ;; in which block
;; var-descr              ;; argument and result variables (or
;;                        ;; constances)
;; type-descr             ;; the general type-descriptor
;; type-descr-s           ;; a list of type-descriptors
;; then-block             ;; a block
;; else-block             ;; a block
;; used-places            ;; for register-allocation
;; killed-places
;; :Annotations
;;  ;;-----------
;;)
;;
;;(def-mzs-object switch (mzs-object)
;;  vlaue                 ;; a variable
;;  block                 ;; in which block
;; var-descr              ;; argument and result variables (or
;;                        ;; constances)
;; type-descr             ;; the general type-descriptor
;; type-descr-s           ;; a list of type-descriptors
;; (out-blocks)           ;; list of (case-label block)
;; used-places            ;; for register allocation
;; killed-places
;; :Annotations
;;  ;;;----------
;;)

;;;-----------------------------------------------------------------------------
;; definition of the labels: join-label, zykl-label and function-label
;;;-----------------------------------------------------------------------------

(def-mzs-object label (mzs-object)     ; the superclass of join-label and zykl-label
  in-block;;; list of all income lblock's
  out-block             ;; a block
  (place :initform ())  ;; an addressexpression of this label
  (code-identifier :initform ())       ; identifier for code generation
  :Annotations
  ;;-----------
  )

(def-mzs-object join-label (label)
  context;;; context of all income block's
  :Annotations
  ;;-----------
  (rebind-vars :initform ())            ; lists of local variables
  env-level             ;; the level of the var-environment
  ;; deleted from *hf*
  ;;  read-glocs-level      ;; the level of global read from location
  ;;  set-glocs-level       ;; the level of global set to location
  )

(def-mzs-object function-label (mzs-object)
  function;;; the current function
  start-block            ;; a block
  end-blocks             ;; list of blocks
  :Annotations
  ;;-----------
  env-level             ;; the level of the var-environment
  ;;  deleted from *hf*
  ;;  read-glocs-level      ;; the level of global read from location
  ;;  set-glocs-level       ;; the level of global set to location
  (code-identifier :initform ())       ; identifier for code generation
  )

(def-mzs-object zykl-label (label)
  :Annotations
  ;;-----------
  env-level             ;; the level of the var-environment
  ;;  deleted from *hf*
  ;;  read-glocs-level      ;; the level of global read from location
  ;;  set-glocs-level       ;; the level of global set to location
  type-descr-s          ;; for join of variables
  )

;;;-----------------------------------------------------------------------------
;; typedescriptor's
;;;-----------------------------------------------------------------------------

(def-mzs-object type-descr (mzs-object)
  stat   ;; the reference to the statement
  t-descr-before        ;; link
  type-vars             ;; type var substitutions
  type-vec              ;; a vector of type vars of the
  ;; length arg-num + 1. type-vec[0] = the
  ;; result-type
  (new-type-vars :initform ())         ; refs to sharper type var substitutions
  (type-spec :initform 0) ; where are improves in the type-vec
  :Annotations
  ;;-----------
  )

(def-mzs-object act-type-descr (type-descr)
  actual  ;; will be set by each inference-step
  error-spec             ;; where are type-errors
  :Annotations
  ;;-----------
  (t-descr-after :initform ())
  )

(def-mzs-object formal-type-descr (type-descr)
  :Annotations
  ;;-----------
  )

(def-mzs-object recursive-type-descr (formal-type-descr)
  :Annotations
  ;;-----------
  )

;;;-----------------------------------------------------------------------------
;; var-descr - a descriptor of arguemnt's and the result
;;;-----------------------------------------------------------------------------

(def-mzs-object var-descr (mzs-object)
  var-vec ;; a vector of variables and constances
  constant-counter       ;; the number of constants in var-vec
  :Annotations
  ;;-----------
  )

;;;-----------------------------------------------------------------------------
;; mzs-statements
;;;-----------------------------------------------------------------------------

(def-mzs-object statement (mzs-object)
  (block :initform ())   ;; the block of this statement
  (used-places :initform ())            ; for register-allocation
  (killed-places :initform ())
  (ready :initform ())   ;; for register-allocation *rr*
  :Annotations
  ;;-----------
  )

(def-mzs-object goto (statement)
  label
  :Annotations
  ;;-----------
  arg-num ;; for recursive function-application
  var-descr;;; before optimation
  type-descr              ;; add for global inference *hf* 16.02
  type-descr-s            ;; add for global inference *hf* 16.02
  )

(def-mzs-object value-statement (statement)
  ;; the superclass of return and switch
  value
  (type :initform ())    ;; the general type of this value
  type-descr-s           ;; types in different pathes
  :Annotations
  ;;-----------
  (read-glocs :initform ())
  )

(def-mzs-object return (value-statement)
  ;; deleted form *hf*
  ;; (last-call :initform ())             ; the last function-call
  :Annotations
  ;;-----------
  ;; deleted form *hf*
  ;; function
  pathes
  )

(def-mzs-object switch (value-statement)
  out-blocks              ;; a list of (case-label block)
  :Annotations
  ;;-----------
  )


(def-mzs-object function-statement (statement)
  ;; the superclass of all statements with
  ;; function and the funcall-statement
  arg-num ;; the number of arguments
  var-descr              ;; a variablen-descriptor
  type-descr             ;; the general type-descriptor
  type-descr-s            ;; list of typedescriptore
  used-places            ;; for register-allocation
  killed-places
  :Annotations
  ;;-----------
  (write-glocs :initform ())
  (read-glocs :initform ())
  )

(def-mzs-object function-call (function-statement)
  ;; suberclass for call, last-call, funcall, asm, last-asm
  :Annotations
  ;;-----------
  )

(def-mzs-object call (function-call)
  function;;; the called function
  :Annotations
  ;;-----------
  )

(def-mzs-object last-call (function-call)
  function;;; the called function
  :Annotations
  ;;-----------
  )

(def-mzs-object funcall (function-call)
  value   ;; the variable (or constant) with des
  ;; function address
  (closure-call :initform ())          ; flag for the closure-call
  value-type
  :Annotations
  ;;-----------
  )

(def-mzs-object asm (function-call)
  function;;; the called function, a TAIL-function
  ;; (coded in special-sys-fun)
  :Annotations
  ;;-----------
  )

(def-mzs-object last-asm (function-call)
  function;;; the last-called function, a TAIL-function
  ;; (coded in special-sys-fun)
  :Annotations
  ;;-----------
  )

(def-mzs-object move (function-statement)
  ;; with the implicit move-function
  :Annotations
  ;;-----------
  )

(def-mzs-object test (function-statement)
  function;;; the predicate, a TAIL-function (coded
  ;; in special-sys-fun)
  then-block             ;; a block
  else-block             ;; a block
  :Annotations
  ;;-----------
  then-type-descr-s
  else-type-descr-s
  (divided :initform ()) ;; to indicate a complex inline
  )

;;;-----------------------------------------------------------------------------
;; temporary variables
;;;-----------------------------------------------------------------------------

(def-mzs-object tempvar (mzs-object)
  tnr ;; for printing of temp-vars
  (link :initform ())    ;; the link to used and set-statements
  (place :initform ())   ;; for register-allocation
  :Annotations
  ;;-----------
  (code-identifier :initform ())       ; identifier for code generation
  )

;;;-----------------------------------------------------------------------------
;; context to analyse form
;;
;; a call, last-call, funcall, asm, last-asm,  join-label, function-label, the
;; test-statement and the switch statement are context's also
;;
;;;-----------------------------------------------------------------------------

(def-mzs-object arg (mzs-object)
  num ;; the number of the argument
  application            ;; a function-statement
  :Annotations
  ;;-----------
  )

(def-mzs-object progn (mzs-object)
  context ;; the context of the last progn-form
  :Annotations
  ;;-----------
  )

(def-mzs-object void (mzs-object)      ; the context of all progn-forms but the
  ;; last
  :Annotations
  ;;-----------
  )

;;;-----------------------------------------------------------------------
;; places  *rr*
;;;-----------------------------------------------------------------------

(def-mzs-object place (mzs-object)
  :Annotations
  ;;-----------
  )

(def-mzs-object rplace (place)
  rclass rnumber
  :Annotations
  ;;-----------
  )

(def-mzs-object dplace (place)
  value
  :Annotations
  ;;-----------
  )

(def-mzs-object mplace (place)
  basis
  offset
  data-type
  :Annotations
  ;;-----------
  )

(def-mzs-object address-expr1 (mplace)
  :Annotations
  ;;-----------
  )

(def-mzs-object temp-place (address-expr1)
  :Annotations
  ;;-----------
  )

(def-mzs-object address-expr1-identifier (address-expr1)
  :Annotations
  ;;-----------
  )

(def-mzs-object address-of-identifier (mplace)
  :Annotations
  ;;-----------
  )

(def-mzs-object address-expr2 (mplace)
  :Annotations
  ;;-----------
  )

(def-mzs-object address-exprn (mplace)
  :Annotations
  ;;-----------
  )

(def-mzs-object %direct-value-place (dplace)
  :Annotations
  ;;-----------
  )

;;;-----------------------------------------------------------------------------
#module-end  ;; End of module mzs
;;;-----------------------------------------------------------------------------
