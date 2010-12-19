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
;;; Title: macro to define abstract syntax tree
;;;  Library: comp (EuLisp to Bytecode Compiler -- EuLysses)
;;;  Authors: Andreas Kind, Keith Playford
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule _sx-obj0
  (syntax (syntax-1)
   import (level-1))

;;;-----------------------------------------------------------------------------
;;;  All syntax objects are defined with this macro
;;;-----------------------------------------------------------------------------
(defsyntax def-syntax-obj (cl-name super slots . options)
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
(defsyntax new-node (node kind . first?)
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
)  ;; End of module sx-obj0
;;;-----------------------------------------------------------------------------
