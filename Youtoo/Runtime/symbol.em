;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: level1
;;;  Authors: Andreas Kind, Julian Padget
;;; Description: symbols
;;;-----------------------------------------------------------------------------
(defmodule symbol
  (syntax (_telos0)
   import (telos convert compare collect character string table fpi)
   export (<symbol> <keyword> <name> symbolp keywordp name
           gensym symbol-name keyword-name symbol-exists?))

;;;-----------------------------------------------------------------------------
;;; Symbol and keyword initialization
;;;-----------------------------------------------------------------------------
  (defmethod initialize ((sym <symbol>) inits)
    (call-next-method)
    (eul_init_symbol sym))

  (defmethod initialize ((key <keyword>) inits)
    (call-next-method)
    (eul_init_keyword key))

  (defextern eul_init_symbol (ptr) ptr)
  (defextern eul_init_keyword (ptr) ptr)

;;;-----------------------------------------------------------------------------
;;; Gensym
;;;-----------------------------------------------------------------------------
  (deflocal *symbol-count* 0)

  (defun gensym strs
    (let* ((prestr (if strs (car strs) "G00"))
           (sym (make-symbol
                 (string-append prestr (int-as-string *symbol-count*)))))
      (setq *symbol-count* (int-binary+ *symbol-count* 1))
      sym))

;;;-----------------------------------------------------------------------------
;;; More predicates ...
;;;-----------------------------------------------------------------------------
  (defun keywordp (x)
    (eq (class-of x) <keyword>))

  (defun symbol-exists? (str)
    (table-ref (get-global-register symbols) str))

  (defgeneric namep (x)
    method: ((x))
    method: (((x <name>)) t))

;;;-----------------------------------------------------------------------------
;;; Comparison
;;;-----------------------------------------------------------------------------
  (defmethod binary< ((sym1 <symbol>) (sym2 <symbol>))
    (int-binary< (string-compare (symbol-name sym1)
                                 (symbol-name sym2)) 0))

;;;-----------------------------------------------------------------------------
;;; Concatenation
;;;-----------------------------------------------------------------------------
  (defmethod concatenate ((x <name>) . cs)
    (labels
     ((loop (ccs str)
            (if (null? ccs)
                (make (class-of x) name: str)
              (loop (cdr ccs)
                    (string-append str (convert (car ccs) <string>))))))
     (loop cs (name x))))

;;;-----------------------------------------------------------------------------
;;; Conversion
;;;-----------------------------------------------------------------------------
  (defgeneric (converter <symbol>) (x))

  (defmethod (converter <symbol>) ((str <string>))
    (make <symbol> name: str))

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
