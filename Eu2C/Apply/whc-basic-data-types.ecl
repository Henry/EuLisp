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
;;; Title: macros and aux functions to read in the basic data types
;;;  Description:
;;    macros and aux functions to read in the basic data types
;;    first the machine data types of the machine,
;;    define-machine-data-types reads in the list of machine data types
;;    (from the machine-file)
;;    and set these to the Constant machine-dates
;;;  Documentation:
;;    see in the APPLY-paper TAIL:eine getypte implementationssprache fuer APPLY
;;;  Notes:
;;    here with define-tail for the aux-data-types too
;;;  Authors: Winfried Heicking
;;;-----------------------------------------------------------------------------

#module whc-basic-data-types
(import (representation
         lzs
         level-0
         (only (get-option)
               option-lists)
         (only (<tail-class-def>)
               lzs-mop)
         (only (dolist
                assoc
                cadr
                make-instance
                mapcan
                intern
                setf)
               common-lisp)
         class-ext
         whc-aux)
 syntax (level-0
         apply-standard
         (only (setf)
               common-lisp)
         el2lzs)       ; only define-tail is needed
 export (<basic-class-def>
         <%integer>
         <%signed>
         <%unsigned>
         <%float>
         <%pointer>
         <%pointer-to-struct>
         <%pointer-to-vector>
         <%pointer-to-void>
         <%aux-type>
         <%representation>
         <%machine-type>
         <%direct>)
 export (define-machine-data-types
          define-aux-data-types   ;data types whithout export
          define-tail-sys-functions ;tail functions with export
          define-tail-aux-sys-functions;tail aux functions without export
          define-special-sys-funs ; taken from rr-md-read
          ))

(defmacro define-machine-data-types machine-datas
  (let (classes)
    (setq classes
          (mapcan
           (lambda (li)
             (list
              `(deflocal ,(car li)
                 (make-instance
                  <%machine-type>
                  :byte-size ,(get-option :byte-size (cdr li) ())
                  :alignment ,(get-option :alignment (cdr li) ())))))
           machine-datas))
    `(progn ,@classes)))



;;define-basic-data-types takes the predifined basic-data-types from
;;the machine file, generates for each data type a class (there are the
;;classes %unsigned-word-integer) and makes for this class one
;;instance. This instance is set to value of the class name (the value
;;%unsigned-word-integer is a instance of the class %unsigned-word-integer)

;;define-basic-data-types like in TAIL


;;;read-in the tail functions like %ref...
;;;in define-tail with export export (like the real data types)

(defun make-instance-symbol (sym)
  (intern (string-append "$" (symbol-name sym))))


(defmacro define-tail-sys-functions funs
  (let (progbody)
    (dolist (fun funs)
            (setq progbody
                  (cons
                   `(define-tail ,fun
                      export
                      (special-sys-fun)
                      :protocol-type ()
                      :arg-num ()
                      :match-list ()
                      :inline ())
                   progbody))
            )
    `(progn ,@progbody)))


(defmacro define-tail-aux-sys-functions funs
  (let (progbody)
    (dolist (fun funs)
            (setq progbody
                  (cons
                   `(define-tail ,fun
                      ()
                      (special-sys-fun)
                      :protocol-type ()
                      :arg-num ()
                      :match-list ()
                      :inline ())
                   progbody))
            )
    `(progn ,@progbody)))


(defmacro define-basic-data-types  basic-data-types
  (let (classes)
    (setq classes
          (mapcan (lambda (li)
                    (list
                     `(define-tail
                        ,(car li)
                        export (basic-class-def)
                        :representation
                        ,(get-option :machine-type (cdr li) ()))))
                  basic-data-types))
    `(progn ,@classes)))


(defmacro define-special-sys-funs  description-list
  (let ((funs  (mapcan (lambda (li)
                         (list
                          `(define-tail
                             ,(car li)
                             export (special-sys-fun)
                             :arg-num ,(get-option :arg-num (cdr li) ())
                             :inline ())))
                       description-list)))
    `(progn ,@funs)))

;;;-----------------------------------------------------------------------------
#module-end
;;;-----------------------------------------------------------------------------
