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
;;;  Title: Code Generator for Data
;;;  Description:
;;    This modules provides the generation of code for global Variables, named
;;    constants and literals. Only assembly code for the SPARC processor is generated
;;    which can be processed by the SPARC-Assembler from SUN.
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors:
;;;-----------------------------------------------------------------------------

#module c-data
(import ((except (format) eulisp1)
         list-ext
         el2lzs-literals
         code-identifier
         c-typing
         machine-description
         expand-literal
         lzs accessors
         lzs-mop
         predicates
         (only (configuration?)
               configuration)
         (only (format
                mapc
                make-list
                char
                subseq
                char-code
                find
                princ
                alphanumericp
                remove
                reverse
                mapcan
                nconc
                make-instance)
               common-lisp))

 syntax (eulisp1
         (only (with-output-to-string
                push
                dolist)
               common-lisp))

 export (generate-c-data
         get-instance
         reset-c-data
         make-c-string
         disable-gc))

;;;-----------------------------------------------------------------------------
;;; variables and reset
;;;-----------------------------------------------------------------------------

(deflocal *structure-roots* ())
(deflocal *vector-roots* ())
(defvar code-output t)

(defun reset-c-data ()
  (setq *structure-roots* ())
  (setq *vector-roots* ()))

;;;-----------------------------------------------------------------------------
;;; code output
;;;-----------------------------------------------------------------------------

(defun write-data (format . args)
  (apply #'format (dynamic code-output) format args))

;;;-----------------------------------------------------------------------------
;;; generate-c-data
;;;-----------------------------------------------------------------------------

(defun generate-c-data ()
  (setq *literals*
        (reverse (remove (expand-literal ()) *literals*)))
  (when (configuration? ':int ':small)
        (write-data "~%SMALL_INT_SKIP;"))
  (mapc (lambda (lit)
          (unless (imported-p (?unexpanded lit))
                  (literal-declaration lit
                                       (?class lit)
                                       (?representation (?class lit)))))
        *literals*)
  (mapc (lambda (literal)
          (unless (imported-p (?unexpanded literal))
                  (literal-definition literal
                                      (?class literal)
                                      (?representation (?class literal)))))
        *literals*)
  (write-data "~2%static void *STRUCTURE_ROOTS[~A] = {~{~%~A,~} 0};"
              (+ (length *structure-roots*) 1)
              *structure-roots*)
  (write-data "~2%static struct VECTOR_ROOTS{int length; void **pointer;}VECTOR_ROOTS[~A] ~
               = {~{~%~A,~} {0, 0}};~%"
              (+ (length *vector-roots*) 1)
              *vector-roots*))

;;;-----------------------------------------------------------------------------
;;; disabling gc for literals
;;;-----------------------------------------------------------------------------

(defgeneric disable-gc (literal))

(defmethod disable-gc (literal) ())

(defmethod disable-gc ((class <defined-class>))
  (disable-gc (expand-literal class)))

(defmethod disable-gc ((literal <literal-instance>))
  (unless (?gc-not-needed literal)
          (setf (?gc-not-needed literal) t)
          (mapc #'disable-gc (?value-list literal))))

;;;-----------------------------------------------------------------------------
;;; collection of root addresses for GC
;;;-----------------------------------------------------------------------------

(defun add-structure-root (literal slot)
  (unless (?gc-not-needed literal)
          (push (format () "~:/EXPR/.~A"
                        literal
                        (c-identifier slot))
                *structure-roots*)))

(defun add-vector-root (length literal)
  (unless (?gc-not-needed literal)
          (push (format () "{~A, (void**)~/EXPR/}"
                        length
                        literal)
                *vector-roots*)))

;;;-----------------------------------------------------------------------------
;;; Declaring Literals
;;;-----------------------------------------------------------------------------

(defgeneric literal-declaration (literal class representation))

(defmethod literal-declaration (literal class
                                        (representation <%pointer-to-struct>))
  (write-data "~%~:[static ~;~]S_LITERAL(~A, ~A);"
              (exported-for-lisp? (?unexpanded literal))
              (type-identifier (?class literal))
              (c-identifier literal)))

(defmethod literal-declaration (literal class
                                        (representation <%pointer-to-vector>))
  (write-data "~%~:[static ~;~]V_LITERAL(~A, ~A, ~A);"
              (exported-for-lisp? (?unexpanded literal))
              (type-identifier (~vector-class-element-type class))
              (c-identifier literal)
              (get-length-of-vector-literal literal class)))

(defmethod literal-declaration (literal (class <tail-class-def>)
                                        representation)
  ())

(defmethod literal-declaration (literal (class <basic-class-def>)
                                        representation)
  ())

(defmethod literal-declaration (literal class representation)
  ())

;;;-----------------------------------------------------------------------------
;;; Defining Literals (with initial value)
;;;-----------------------------------------------------------------------------

(defgeneric literal-definition (literal class representation))

(defmethod literal-definition (literal class representation)
  ())

(defmethod literal-definition (literal class (representation <%direct>))
  ;; direct can be used only for class mappings to basic classes like
  ;; %signed-word-integer
  (let ((class (~slot-description-type (car (~class-slot-descriptions class)))))
    (literal-definition literal class (?representation class))))

(defmethod literal-definition (literal class
                                       (representation <%pointer-to-struct>))

  (write-data "~%~:[static ~;~]LITERAL(~A) = {STAG(~:/EXPR/), {~{~:/EXPR/~^, ~}}};"
              (exported-for-lisp? (?unexpanded literal))
              (c-identifier literal)
              class
              (get-structure-components
               literal
               (?value-list literal)
               (~class-slot-descriptions class))))

(defmethod literal-definition (literal (class <tail-class-def>)
                                       (representation <%pointer-to-struct>))
  (write-data "~%struct ~A ~A = {~{~:/EXPR/~^, ~}};"
              (type-identifier class)
              (c-identifier literal)
              (get-structure-components
               literal
               (?value-list literal)
               (~class-slot-descriptions class))))

(defmethod literal-definition (literal class
                                       (representation <%pointer-to-vector>))
  (let* ((length (get-length-of-vector-literal literal class))
         (element-type (~vector-class-element-type class))
         (components (get-vector-components
                      length
                      (second (?value-list literal))    ; the components
                      element-type)))
    (if (string? components)
        (write-data "~%~:[static ~;~]LITERAL(~A) = {VTAG(~A, ~:/EXPR/), \"~A\"};"
                    (exported-for-lisp? (?unexpanded literal))
                    (c-identifier literal)
                    length
                    class
                    components)
      (progn
        (when (is-pointer element-type)
              (add-vector-root length literal))
        (write-data "~%~:[static ~;~]LITERAL(~A) = {VTAG(~A*sizeof(~A), ~:/EXPR/), {~{~:/EXPR/~^, ~}}};"
                    (exported-for-lisp? (?unexpanded literal))
                    (c-identifier literal)
                    length
                    (type-identifier element-type)
                    class
                    components)))))

;;tail vectors are stored like lisp vectors to make the length available
;;(defmethod literal-definition (literal (class <tail-class-def>)
;;                                       (representation <%pointer-to-vector>))
;;  (let* ((length (get-length-of-vector-literal literal class))
;;         (element-type (~vector-class-element-type class))
;;         (components (get-vector-components
;;                      length
;;                      (second (?value-list literal))    ; the components
;;                      element-type)))
;;    (if (string? components)
;;      (write-data "~%~A ~A = \"~A\";"
;;                  (type-identifier class)
;;                  (c-identifier literal)
;;                  components)
;;      (progn
;;        (when (is-pointer element-type)
;;          (add-vector-root length literal))
;;        (write-data "~%~A ~A[~A] = {~{~:/EXPR/~^, ~}};"
;;                    (type-identifier (~vector-class-element-type class))
;;                    (c-identifier literal)
;;                    length
;;                    components)))))

(defmethod literal-definition (literal (class <%string>)
                                       representation)
  ())

(defconstant $unknown-initializer
  (make-literal-instance
   %unsigned-word-integer
   '(0)))

(defun get-structure-components (literal values slots)
  (if (null? values) ()
    (progn
      (when (is-pointer (~slot-description-type (car slots)))
            (add-structure-root literal (car slots)))
      (cons (if (eq (car values) ^unknown)
                $unknown-initializer
              (type-expr-for-c (~slot-description-type (car slots))
                               (car values)))
            (get-structure-components literal (cdr values) (cdr slots))))))

(defun get-length-of-vector-literal (literal class)
  (setf (first (?value-list literal))
        (or (first (?value-list literal))     ; the length spec in literal
            (~vector-class-instance-length class) ; defined length in class
            (length (second (?value-list literal))) ;the length of the given sequence
            )))

(defgeneric get-vector-components (length components class))

(defmethod get-vector-components (length components class)
  (cond ((= length 0) ())
        ((null? components)
         ())
        ((null? (cdr components))
         (make-list length
                    :initial-element (type-expr-for-c class
                                                      (car components))))
        (t
         (cons (type-expr-for-c class
                                (car components))
               (get-vector-components (- length 1)
                                      (cdr components)
                                      class)))))

(defmethod get-vector-components (length (components <string>)
                                         (class <%unsigned-byte-integer>))
  (cond ((< length (length components))
         (string-code (subseq components 0 length) length #\space))
        ((= length (length components))
         (string-code components length #\space))
        (t
         (string-code components length
                      (char components (- (length components) 1))))))

;;;-----------------------------------------------------------------------------
;;; Strings
;;;-----------------------------------------------------------------------------

(defun string-code (string length padchar)
  (convert-to-c-string (format () "~V,,,VA" length padchar string)))

(defun convert-to-c-string (string)
  (with-output-to-string (c-string)
                         (cl:map ()
                                 (lambda (char)
                                   (princ (cond ((alphanumericp char) char)
                                                ((find char " !#$%&'()*+,-./:;<=>?@[]^_`{|}~")
                                                 char)
                                                ((eq char #\0) "\\a")
                                                ((eq char #\backspace) "\\b")
                                                ((eq char #\page) "\\f")
                                                ((eq char #\newline) "\\n")
                                                ((eq char #\return) "\\r")
                                                ((eq char #\tab) "\\t")
                                                ((eq char #\?) "\\?")
                                                ((eq char #\') "\\'")
                                                ((eq char #\") "\\\"")
                                                ((eq char #\\) "\\\\")
                                                (t (format () "\\~3,'0O" (char-code char))))
                                          c-string))
                                 string)))

(defun make-c-string (literal)
  (get-vector-components (get-length-of-vector-literal literal (?class literal))
                         (second (?value-list literal))
                         (~vector-class-element-type (?class literal))))

#module-end
