;;; Copyright 1997 A. Kind & University of Bath
;;; Copyright 2010 Henry G. Weller
;;;-----------------------------------------------------------------------------
;;  This file is part of
;;; ---                         EuLisp System 'Youtoo'
;;;-----------------------------------------------------------------------------
;;
;;  Youtoo is free software: you can redistribute it and/or modify it under the
;;  terms of the GNU General Public License version 2 as published by the Free
;;  Software Foundation.
;;
;;  Youtoo is distributed in the hope that it will be useful, but WITHOUT ANY
;;  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;  details.
;;
;;  You should have received a copy of the GNU General Public License along with
;;  this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;-----------------------------------------------------------------------------
;;; Title: defining form 'defclass'
;;;  Library: telos (The EuLisp Object System -- TELOS)
;;;  Authors: Russell Bradford, Andreas Kind
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule mop-defcl0
  (syntax (boot0)
   import (level1))

;;;-----------------------------------------------------------------------------
;;; Syntax: (defclass name supers (slots) {keywords}*), where
;; name is a symbol,
;; supers is {() | class | ({class}*)},
;; slots is {symbol | (symbol {slot-keywords}*)}, and
;; keywords and slot-keywords are {key val}. Allowable keywords include
;; class:               the class of the class begin defined
;; keywords:            a list of the allowable keywords for this class
;; predicate:           a predicate function for this class
;; constructor:         a constructor function for this class
;; print-function:      a function to be added as a method to generic-print
;;                          to print an instance
;; The predicate: and constructor: keywords can be repeated.
;; Allowable slot-keywords include
;; reader:              a symbol to name a reader for this slot
;; writer:              a symbol to name a writer for this slot
;; accessor:            a symbol to name a reader for this slot; a writer
;;                      for this slot will be installed as the setter of
;;                      the reader
;; keyword:             a symbol to be the keyword for the slot
;; default:             an initial value for the slot
;; The reader:, writer:, and accessor: keywords can be repeated.
;;;-----------------------------------------------------------------------------
(defmacro defclass (cl-name supers slots . keywords)
  (let ((real-name (strip-<> cl-name)))
    `(progn
       (defconstant ,cl-name
         (make ,(find-key class: keywords '<simple-class>)
               name: ',real-name
               direct-superclasses:
               (list ,@(or (if (list? supers) supers (list supers))
                           '(<object>)))
               direct-slots: (list ,@(do-direct-slotds slots))
               direct-keywords:
               ',(append (find-key keywords: keywords ())
                         (find-slot-keywords slots))
               ,@(filter-keywords keywords
                                  '(keywords: predicate:
                                              class: constructor:
                                              print-function:))))
       ,@(do-accessors cl-name slots)
       ,@(do-predicates cl-name keywords)
       ,@(do-constructors cl-name keywords)
       ,@(do-printfn cl-name keywords)
       ,cl-name)))

;;;-----------------------------------------------------------------------------
;;; Define primitive class
;;;-----------------------------------------------------------------------------
(defmacro defprimclass (cl-name internal-name supers slots . keywords)
  (let ((real-name (strip-<> cl-name)))
    `(progn
       (defconstant ,cl-name (get-global-register ,internal-name))
       ((setter primitive-class-of) ,cl-name <simple-class>)
       (initialize ,cl-name
                   (list name: ',real-name
                         direct-superclasses:
                         ;(list ,@(or supers '(<object>)))
                         (list ,@(or (if (list? supers) supers (list supers))
                                     '(<object>)))
                         direct-slots:
                         (list ,@(do-direct-slotds slots))
                         direct-keywords:
                         ',(append (find-key keywords: keywords ())
                                   (find-slot-keywords slots))
                         ,@(filter-keywords keywords
                                            '(keywords: predicate:
                                                        class: constructor:
                                                        print-function:))))
       ,@(do-accessors cl-name slots)
       ,@(do-predicates cl-name keywords)
       ,@(do-constructors cl-name keywords)
       ,@(do-printfn cl-name keywords)
       ,cl-name)))

;;;-----------------------------------------------------------------------------
;;; Strip-<>
;;;-----------------------------------------------------------------------------
(defun strip-<> (sym)
  (let ((str (symbol-name sym)))
    (if (eq (string-ref str 0) #\<)
        (make-symbol (substring str 1 (- (string-size str) 1)))
      sym)))

;;;-----------------------------------------------------------------------------
;;; Auxilary functions
;;;-----------------------------------------------------------------------------
(defun do-direct-slotds (slots)
  (cond ((null? slots) ())
        ((atom? (car slots))
         (cons `(list name: ',(car slots))
               (do-direct-slotds (cdr slots))))
        (t (let* ((*absent* '(absent))
                  (initf (find-key default: (cdar slots) *absent*))
                  (inita (find-key keyword: (cdar slots) *absent*)))
             (cons `(list name: ',(caar slots)
                          ,@(if (eq initf *absent*)
                                ()
                              `(default: (lambda () ,initf)))
                          ,@(if (eq inita *absent*)
                                ()
                              `(keyword: ,inita))
                          ,@(filter-keywords
                             (cdar slots)
                             '(default: accessor: keyword: reader: writer:)))
                   (do-direct-slotds (cdr slots)))))))

(defun do-accessors (name slots)
  (labels
   ((loop (ll res i)
          (if (null? ll) res
            (let ((slot (car ll)))
              (if (atom? slot)
                  (loop (cdr ll) res (- i 1))
                (loop (cdr ll)
                      (append (do-accessor name
                                           (car slot) (cdr slot) i)
                              res)
                      (- i 1)))))))
   (loop slots () (- (list-size slots) 1))))

(defun do-accessor (name slotname inits i)
  (cond ((null? inits) ())
        ((eq (car inits) accessor:)
         (let ((acc (cadr inits)))
           (concatenate
            (do-reader acc name slotname i)
            (do-writer acc name slotname i)
            (do-accessor name slotname (cddr inits) i))))
        ((eq (car inits) reader:)
         (let ((acc (cadr inits)))
           (append (do-reader acc name slotname i)
                   (do-accessor name slotname (cddr inits) i))))
        ((eq (car inits) writer:)
         (let ((acc (cadr inits)))
           (append (do-writer acc name slotname i t)
                   (do-accessor name slotname (cddr inits) i))))
        (t (do-accessor name slotname (cddr inits) i))))

;  (defun do-reader (acc name slotname i)
;    (list (list 'defun acc '(x)
;               (list (list 'opencoded-lambda '(o i)
;                           (list 'binding-ref '? name)
;                           '(primitive-relative-ref))
;                     'x i))
;      (list 'declare-inline acc)))

(defun do-reader (acc name slotname i)
  `((defun ,acc (x)
      ((opencoded-lambda (o i)
                         (binding-ref ? ,name)
                         (primitive-relative-ref))
       x ,i))
    (declare-inline ,acc)))

;  (defun do-reader (acc name slotname i)
;    `((defconstant ,acc '(()))  ; class ,name not yet initialized!
;      (setq ,acc (slot-reader (find-slot ,name ',slotname)) t)
;      (if (generic-function? ,acc)
;         (setq ,acc (generic-function-discriminating-function ,acc) t)
;       ())))

(defun do-writer (acc name slotname i . no-accessor)
  (if (null? no-accessor)
      `((defun (setter ,acc) (x v)
          ((opencoded-lambda (x i v)
                             (binding-ref ? ,name)
                             (set-primitive-relative-ref))
           x ,i v)))
    `((defun ,acc (x v)
        ((opencoded-lambda (x i v)
                           (binding-ref ? ,name)
                           (set-primitive-relative-ref))
         x ,i v))
      (declare-inline ,acc))))

;  (defun do-writer (acc name slotname i . no-accessor)
;    (if (null? no-accessor)
;       `(((setter setter) ,acc (slot-writer (find-slot ,name ',slotname)))
;         (if (generic-function? (setter ,acc))
;             ((setter setter) ,acc
;              (generic-function-discriminating-function (setter ,acc)))
;           ()))
;      `((defconstant ,acc '(()))       ; class ,name not yet initialized!
;       (setq ,acc (slot-writer (find-slot ,name ',slotname)) t)
;       (if (generic-function? ,acc)
;           (setq ,acc (generic-function-discriminating-function ,acc) t)
;         ()))))

(defun do-predicates (name keywords)
  (cond ((null? keywords) ())
        ((eq (car keywords) predicate:)
         (let ((pred (cadr keywords)))
           (append `((defgeneric ,pred ((x <object>))
                       method: (((x <object>)) ())
                       method: (((x ,name)) x)))
                   (do-predicates name (cddr keywords)))))
        (t (do-predicates name (cddr keywords)))))

(defun do-constructors (name keywords)
  (cond ((null? keywords) ())
        ((eq (car keywords) constructor:)
         (let ((con (car (cdr keywords))))
           (cons
            (if (atom? con)
                `(defun ,con inits
                   (apply make ,name inits))
              (let ((params (map (lambda (x) (gensym)) (cdr con))))
                `(defun ,(car con) ,params
                   (make ,name
                         ,@(labels
                            ((loop (l1 l2 res)
                                   (if (null? l1) res
                                     (loop
                                      (cdr l1)
                                      (cdr l2)
                                      (append res
                                              (list (car l1) (car l2)))))))
                            (loop (cdr con) params ()))))))
            (do-constructors name (cddr keywords)))))
        (t (do-constructors name (cddr keywords)))))

(defun do-printfn (name keywords)
  (let ((fun (find-key print-function: keywords ())))
    (if (null? fun) ()
      `((defmethod generic-print ((obj ,name) str)
          (,fun obj str))))))

;;;-----------------------------------------------------------------------------
;;; Find slot keywords
;;;-----------------------------------------------------------------------------
(defun find-slot-keywords (slots)
  (labels
   ((loop (ll res)
          (if (null? ll) res
            (let ((s (car ll)))
              (if (atom? s)
                  (loop (cdr ll) res)
                (let ((key (find-key keyword: (cdr s) ())))
                  (if (null? key)
                      (loop (cdr ll) res)
                    (loop (cdr ll) (cons key res)))))))))
   (loop slots ())))

;;;-----------------------------------------------------------------------------
)  ;; End of module mop-defcl0
;;;-----------------------------------------------------------------------------
