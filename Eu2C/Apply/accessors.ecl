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
;;;  Authors: Horst Friedrich
;;;-----------------------------------------------------------------------------

#module accessors

(import ((except (concatenate) eulisp0)
         (only (intern concatenate string)
               common-lisp))
 syntax (eulisp0))

(defmacro defaccessors aclist
  `(progn ,@(da1 aclist)))

(defun da1 (aclist)
  (if (null? aclist) ()
    (let ((el (car aclist)))
      `(,(mk!acc el)
        ,(ex!acc el)
        ,(mk?acc el)
        ,(ex?acc el)
        ,@(da1 (cdr aclist)))
      )))

(defun mk!acc (sym)
  `(defgeneric ,(intern (concatenate 'cl:string "!" (string sym)))
     (arg what)))

(defun mk?acc (sym)
  `(defgeneric ,(intern (concatenate 'cl:string "?" (string sym)))
     (arg)))

(defun ex!acc (sym)
  `(export ,(intern (concatenate 'cl:string "!" (string sym)))))

(defun ex?acc (sym)
  `(export ,(intern (concatenate 'cl:string "?" (string sym)))))


(defaccessors ; --- structure slots of LZS ---
  allow-other-keys
  arg
  arg-list
  body
  case-list
  class
  class-def-list
  constant-value
  cont
  converter
  direct-slots
  domain
  else
  exported
  first-form
  form
  form-list
  fun
  fun-list
  function
  init
  init-list
  keyword
  default-function
  instance
  key-list
  location
  method-class
  method-list
  name
  named-const-list
  opt-list
  options
  otherwise
  package
  params
  pred
  rest
  setter
  slot
  supers
  suppl
  sym
  sym-list
  tagbody
  tagged-form-list
  then
  toplevel-forms
  value
  value-list
  var
  var-list
  )

(defaccessors ; --- annotation slots of LZS ---
  accessor
  actual
  allocator
  applications
  arg-num
  c-imports
  calls
  class-precedence-list
  closure
  closure-vars
  code-identifier
  constructor-for
  constructors
  copy-fun
  definition
  discriminating-fun
  discrimination-arguments
  discrimination-depth
  effective-slots
  equal-pred
  expanded
  expanded-literal
  exports
  fread-gloc
  function-label
  function-type
  fwrite-gloc
  gc-not-needed
  gc-tracer
  generic-fun
  identifier
  keywords
  initial-value
  initialization
  initvalue
  inline
  interpreter
  label
  language
  lattice-type
  lex-env
  link
  match-list
  method-lookup-fun
  module
  moves
  get-slot-value
  set-slot-value
  offset
  pass
  place
  predicate
  protocol-type
  range-and-domain
  read-gloc
  read-gloc-list
  read-glocs
  read-stats
  reader
  rec-calls
  reduce
  representation
  signature
  slot-of
  specializes
  statement-status
  subclasses
  syntactic
  syntax-env
  syntax-exports
  sys-glocs
  tests
  type
  type-descr
  type-descr-s
  type-identifier
  type-list
  used-runtime-modules
  used-syntax-modules
  var-descr
  write-gloc
  write-gloc-list
  write-glocs
  write-stats
  writer
  )

(defaccessors ; --- structure slots of MZS ---
  ;;NOTE that some structure slots of the MZS have the same name as annotation
  ;;slots of the LZS
  application
  basis
  block
  closure-call
  constant-counter
  context
  data-type
  else-block
  end-blocks
  error-spec
  in-block
  in-label
  interface
  killed-places
  new-type-vars
  num
  out-block
  out-blocks
  out-label
  rclass
  ready
  result
  rnumber
  start-block
  stat
  t-descr-before
  then-block
  tnr
  type-spec
  type-vars
  type-vec
  used-places
  value-type
  var-vec
  )

(defaccessors ; --- annotation slots of MZS ---
  divided
  else-interface
  else-type-descr-s
  env
  env-level
  in-binding
  kill-binding
  out-binding
  pathes
  rebind-vars
  successor-blocks
  t-descr-after
  t-path
  then-type-descr-s
  )

#module-end ;end of module accessors
