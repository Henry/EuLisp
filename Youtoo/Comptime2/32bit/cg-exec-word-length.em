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
;;;  Library: comp (EuLisp to Bytecode Compiler -- EuLysses)

;;;  Authors: Andreas Kind, Keith Playford
;;;  Maintainer: Henry G. Weller
;;; Title: executes compiled code
;;;-----------------------------------------------------------------------------
(defmodule cg-exec-word-length
  (syntax (_syntax-1 _i-aux0)
   import (i-all i-modify p-env sx-obj sx-node cg-state cg-asm cg-interf
                 i-ffi ex-expr cg-dld)
   export (bytevector open-bytevector write-next-bv-binding-ref
                      write-next-bv-byte))

;;;-----------------------------------------------------------------------------
;;; Write bytevector
;;;-----------------------------------------------------------------------------
(deflocal *bv* ())
(deflocal *bv-index* 0)
(defun bytevector ()
  *bv*)
(defun open-bytevector (n)
  (notify0 "  open-bytevector ~a" n)
  (setq *bv* (make <string> size: n))
  (setq *bv-index* 0))
(defopencoded bytevector-set (bv i x) (set-byte-vector-ref))
(defextern write-next-bv-binding-ref1 (ptr <fpi> <string> <fpi>) ptr
           "eul_write_next_bv_binding_ref1")

(defun write-next-bv-binding-ref (module-name-str index)
  (notify0 "  write-next-bv-binding-ref ~a ~a ~a"
           *bv-index* module-name-str index)
  (write-next-bv-binding-ref1 *bv* *bv-index* module-name-str index)
  (setq *bv-index* (+ *bv-index* 4)))

(defun write-next-bv-byte (x)
  ((setter string-ref) *bv* *bv-index* (fpi-as-character x))
  (setq *bv-index* (+ *bv-index* 1)))

;;;-----------------------------------------------------------------------------
)  ;; End of module cg-exec-word-length
;;;-----------------------------------------------------------------------------
