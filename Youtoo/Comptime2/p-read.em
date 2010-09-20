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
)  ;; End of module
;;;-----------------------------------------------------------------------------
