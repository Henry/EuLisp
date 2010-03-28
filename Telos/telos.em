;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;; -----------------------------------------------------------------------
;;;                     EuLisp System 'youtoo'
;;; -----------------------------------------------------------------------
;;;  Library: telos (The EuLisp Object System -- TELOS)
;;;  Authors: Russel Bradford, Andreas Kind
;;;  Description: The EuLisp Object System (TELOS)
;;;  Attention: Update Comptime2/cg-dld.em if telos modules change!
;;; -----------------------------------------------------------------------
(defmodule telos
  (expose ((rename ((+ int-binary+)
                    (- int-binary-)
                    (* int-binary*)
                    (/ int-binary/)
                    (% int-binary%)
                    (mod int-binary-mod)
                    (= int-binary=)
                    (< int-binary<)
                    (prin primitive-prin)
                    (print primitive-print)
                    (stdout primitive-stdout)
                    (stderr primitive-stderr)
                    (substring substring1)
                    (equal list-equal))
                    boot)
           mop-access
           mop-alloc
           mop-class
           mop-defcl
           (rename ((generic-format format)) mop-gf)
           mop-init
           mop-inspect
           mop-key
           mop-meth
           mop-prim))
)  ; end of module