;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: comp (EuLisp to Bytecode Compiler -- EuLysses)
;;;  Authors: Andreas Kind, Keith Playford
;;; Description: macro to define abstract syntax tree
;;;-----------------------------------------------------------------------------
(defmodule _sx-obj0
  (syntax (macros)
   import (level1))

;;;-----------------------------------------------------------------------------
;;;  All syntax objects are defined with this macro
;;;-----------------------------------------------------------------------------
(defmacro def-syntax-obj (cl-name super slots . options)
  (let* ((str (symbol-name cl-name))
         (pred-str (concatenate
                    (substring str 1 (- (string-size str) 1))
                    "?"))
         (pred-name (make <symbol> name: pred-str))
         (export-names ()))
    (labels
     ((def-syntax-slots (struct-name slots)
                        (if slots
                            (let* ((str (symbol-name struct-name))
                                   (pre-str (concatenate
                                             (substring str 1
                                                        (- (string-size str) 1))
                                             "-"))
                                   (slot-name (symbol-name (car slots)))
                                   (keywd (make <keyword> name: slot-name))
                                   (reader-name
                                    (make <symbol>
                                          name: (concatenate pre-str slot-name "?")))
                                   (writer-name
                                    (make <symbol>
                                          name: (concatenate pre-str slot-name "!"))))
                              (setq export-names
                                    (cons reader-name (cons writer-name export-names)))
                              (cons (list (car slots)
                                          keyword: keywd
                                          default: ()
                                          reader: reader-name
                                          writer: writer-name)
                                    (def-syntax-slots struct-name (cdr slots))))
                          ())))
     ;; labels body ...
     `(progn
        (defclass ,cl-name ,super
          ,(def-syntax-slots cl-name slots)
          predicate: ,pred-name
          ,@options)
        (export ,cl-name ,pred-name ,@export-names)))))

;;;-----------------------------------------------------------------------------
;;;  Register a new node to the actual module
;;;-----------------------------------------------------------------------------
(defmacro new-node (node kind . first?)
  (let ((reader-name (make <symbol>
                           name: (fmt "module-~as?" (cadr kind))))
        (writer-name (make <symbol>
                           name: (fmt "module-~as!" (cadr kind)))))
    (if first?
        `(let ((m (dynamic *actual-module*)))
           (,writer-name m (cons ,node (,reader-name m))))
      `(let ((m (dynamic *actual-module*)))
         (,writer-name m (tconc (,reader-name m) ,node))))))

;;;-----------------------------------------------------------------------------
)  ;; End of module
;;;-----------------------------------------------------------------------------
