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
;;; Title: The EuLisp Object System (TELOS)
;;;  Library: telos (The EuLisp Object System -- TELOS)
;;;  Authors: Russel Bradford, Andreas Kind
;;;  Maintainer: Henry G. Weller
;;;  Attention: Update Comptime2/cg-dld.em if telos modules change!
;;;-----------------------------------------------------------------------------

(defmodule telos
  (expose ((rename ((+ int-binary+)
                    (- int-binary-)
                    (* int-binary*)
                    (/ int-binary/)
                    (% int-binary%)
                    (mod int-binary-mod)
                    (= int-binary=)
                    (< int-binary<)
                    (sprin primitive-sprin)
                    (prin primitive-prin)
                    (sprint primitive-sprint)
                    (print primitive-print)
                    (format primitive-format)
                    (stdout primitive-stdout)
                    (stderr primitive-stderr)
                    (substring substring1)
                    (equal list-equal))
                   boot)
           mop-access
           mop-alloc
           mop-class
           mop-defcl
           mop-gf
           mop-init
           mop-inspect
           mop-key
           mop-meth
           mop-prim))

;;;-----------------------------------------------------------------------------
)  ;; End of module telos
;;;-----------------------------------------------------------------------------
