;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: comp (EuLisp to Bytecode Compiler -- EuLysses))
;;;  Authors: Andreas Kind, Keith Playford
;;; Description: reading sources
;;;-----------------------------------------------------------------------------
(defmodule p-read
  (syntax (_macros _i-aux0)
   import (i-all sx-obj)
   export (read-source-file))

;;;-----------------------------------------------------------------------------
;;; Read source file
;;;-----------------------------------------------------------------------------
(defun read-source-file (module-name)
  (setq *pass* 'read)
  (let ((file-name (as-source-file-name module-name)))
    (with-input-file-of-path (stream file-name dir *load-path*)
                             (notify0 "  Reading sources from ~a~a~a.em ..."
                                      dir *delimiter* module-name)
                             (setq *tmp-load-dir* dir)
                             (read-s-expression stream))))

;;;-----------------------------------------------------------------------------
)  ;; end of module
;;;-----------------------------------------------------------------------------
