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
;;;  Description:
;;    try to allocate static data
;;;  Notes:
;;    til now adjustment array is put directly in front of static data
;;    all adresses in this array til 0xffffffff are considered as root addresses
;;;  Authors: E. Ulrich Kriegel
;;;-----------------------------------------------------------------------------

#module static-allocation
(import ((except (format)
                 eulisp0)
         accessors ;; ??for missing ?mm-type
         (rename ((push cl:push)
                  (dolist cl:dolist)
                  (format cl:format)
                  (cadr cl:cadr)
                  (cddr cl:cddr)
                  (mapc cl:mapc))
                 (only (push
                        dolist
                        format
                        cadr
                        cddr
                        mapc)
                       common-lisp))
         (only (expand-literal)
               expand-literal)
         (only (~vector-class-element-type)
               lzs-mop)
         (only (<class-def>)
               lzs)
         (only (<stream>)
               el-stream)
         (only (asm-identifier
                c-identifier
                type-identifier)
               code-identifier)
         compiler-conditions
         (only (<%pointer-to-vector>
                <%pointer-to-struct>
                <%machine-type>
                ?actually-byte-length)
               machine-description)
         asm-ops)
 syntax (eulisp0
         dynamic)
 export (static-allocate
         write-static-cards
         write-instance-imports
         write-global-defs
         add-root-object
         add-root-label  ;;soon obsolete used in whc-gen-code only
         add-root-variable
         initialize-static-data-code-collectors))


;;; Macros

(defmacro extract-class-label-from-literal
  (literal-instance)
  `(asm-identifier (expand-literal (?class ,literal-instance))))

;; the following definitions were inherited from codegen-data where there are also
;; local definitions

(defmacro with-label (label comment . body)
  `(dynamic-let ((*label* ,label)
                 (*label-comment* ,comment))
                ,@body))

(defmacro with-comment (comment . body)
  `(dynamic-let ((*comment* ,comment)) ,@body))

(defmacro with-new-alignment body
  `(dynamic-let ((*alignment* 1)
                 (*align* ()))
                ,@body))




(define-compiler-condition <root-set-overflow>(<condition>)
  "The number of generated root labels ~s exceeds ~s" :generated :defined)


;;Hackers version by e.u.kriegel


(deflocal static-asm-code ())
(deflocal root-objects ())
(deflocal root-variables ())
;;reseting code collectors

(defun initialize-static-data-code-collectors()
  (setq static-asm-code ())
  (setq root-objects ())
  (setq root-variables ()))
;;generation of class labels is postponed until code will be written
;;literal instance is consed in front of list of code strings
(defun static-allocate
  (literal-instance list-of-strings representation-instance)
  (cl:push (cons representation-instance (cons literal-instance list-of-strings))
           static-asm-code)
  representation-instance)

;;add new top level object to the root set
(defun add-root-object(label)
  (cl:push label root-objects))
(defun add-root-label(label)
  (cl:push label root-objects))
(defun add-root-variable(label)
  (cl:push label root-variables))

(defgeneric write-static-data (representation literal-instance code))

(defmethod write-static-data
  ((representation <%pointer-to-vector>) literal-instance code)
  ;;write out a static object with rep pointer-to-vector
  (with-new-alignment
   ;;write length in bytes
   (.word (* (car (?value-list literal-instance))
             (?actually-byte-length (?representation
                                     (~vector-class-element-type (?class literal-instance))))))
   ;;write class label
   (.word (extract-class-label-from-literal literal-instance)))
  ;;write data code
  (write-code code))

(defmethod write-static-data
  ((representation <%machine-type>) literal-instance code)
  ;;write data code only
  (write-code code))

(defmethod write-static-data
  (representation literal-instance code)
  ;;write class label
  (with-new-alignment
   (.word (extract-class-label-from-literal literal-instance)))
  ;;write data code
  (write-code code))

(defun write-code (code)
  (cl:format (dynamic code-output) "~{~a~}" code))

(defun write-static-cards ()
  ;; (dynamic code-output) should be bound to a stream for code output
  (let ((len (+ 1 (length root-objects)
                (length root-variables))))
    (if (> len #x4000)
        (compiler-error <root-set-overflow> :defined #x4000 :generated len)
      (Progn
       ;;put adjustment-array in front of static data
       ;;static variable root must be dereferenced to get roots
       (.seg ^data)
       (with-new-alignment
        (.global "_StaticVariableRoot")
        (with-label "_StaticVariableRoot" "table of root variables for GC"
                    (.skip 0) ; to get a label if no root variable exists
                    (cl:mapc #'.word root-variables))
        ;;static object roots
        (.global "_StaticObjectRoot")
        (with-label "_StaticObjectRoot" "table of addresses of root objects for GC"
                    (.skip 0) ; to get a label if no root object exists
                    (cl:mapc #'.word root-objects))
        ;;write end mark
        (.align 4)
        (.global "_StaticRootEnd")
        (with-label "_StaticRootEnd" ()
                    (.skip (- #x10000 (* len 4))));;  ??
        (.global "_StaticLiteralBegin")
        (with-label "_StaticLiteralBegin" ()
                    (.skip 0))
        )
       (cl:dolist (entry static-asm-code)
                  (write-static-data (car entry) (cl:cadr entry) (cl:cddr entry)))

       (.global "_StaticLiteralEnd")
       (with-label "_StaticLiteralEnd" ()
                   (.skip 0))
       ))))

(defun write-global-defs ()
  (cl:mapc (lambda (inst-descr)
             (.global (asm-identifier (cl:cadr inst-descr))))
           static-asm-code))

(defun write-instance-imports (stream)
  (cl:mapc (lambda (inst-descr)
             (let ((inst (cl:cadr inst-descr)))
               (cl:format stream "extern ~A ~A;~%"
                          (instance-type-spec (?representation (?class inst)) (?class inst))
                          (c-identifier inst))))
           static-asm-code))

(defgeneric instance-type-spec (representation class))
(defmethod instance-type-spec ((representation <%pointer-to-struct>) class)
  (cl:format () "struct ~A" (type-identifier class)))
(defmethod instance-type-spec ((representation <%pointer-to-vector>) class)
  (type-identifier (~vector-class-element-type class)))

;;(write-static-cards t)

#module-end
