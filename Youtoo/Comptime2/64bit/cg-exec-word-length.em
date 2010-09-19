;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: comp (EuLisp to Bytecode Compiler -- EuLysses)

;;;  Authors: Andreas Kind, Keith Playford
;;; Description: executes compiled code
;;;-----------------------------------------------------------------------------
(defmodule cg-exec-word-length
  (syntax (_macros _i-aux0)
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
  (notify0 "  open-bytevector ~a" (* n 2))
  (setq *bv* (make <string> size: (* n 2) fill-value: #\0))
  (setq *bv-index* 0))
(defopencoded bytevector-set (bv i x) (set-byte-vector-ref))
(defextern write-next-bv-binding-ref1 (ptr <int> <string> <int>) ptr
           "eul_write_next_bv_binding_ref1")

(defun write-next-bv-binding-ref (module-name-str index)
  (notify0 "  write-next-bv-binding-ref ~a ~a ~a"
           *bv-index* module-name-str index)
  (write-next-bv-binding-ref1 *bv* *bv-index* module-name-str index)
  (setq *bv-index* (+ *bv-index* 8)))

(defun write-next-bv-byte (x)
  ((setter string-ref) *bv* *bv-index* (int-as-character x))
  ((setter string-ref) *bv* (+ *bv-index* 1) (int-as-character 0))
  (setq *bv-index* (+ *bv-index* 2)))

;;;-----------------------------------------------------------------------------
)  ;; End of module
;;;-----------------------------------------------------------------------------
