;;; Copyright 1994-2010 Fraunhofer ISST
;;; Copyright 2010 Henry G. Weller
;;;-----------------------------------------------------------------------------
;;  This file is part of
;;; ---                           EuLisp System 'Eu2C'
;;;-----------------------------------------------------------------------------
;;
;;  Eu2C is free software: you can redistribute it and/or modify it under the
;;  terms of the GNU General Public License version 2 as published by the Free
;;  Software Foundation.
;;
;;  Eu2C is distributed in the hope that it will be useful, but WITHOUT ANY
;;  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;  details.
;;
;;  You should have received a copy of the GNU General Public License along with
;;  this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;-----------------------------------------------------------------------------
;;; Title:
;;;  Description:
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors:
;;;-----------------------------------------------------------------------------
(in-package #:cl-user)

(load "Apply/environment.lisp")

(defun load-from-apply-dir (dir file)
  (load (merge-pathnames
         (make-pathname :directory
                        (if dir
                            (append $applyroot (list dir))
                          $applyroot))
         file)))

(declaim (ftype function load-apply))

(let ((system-name
       (caddr
        #+ :sbcl sb-ext:*posix-argv*
        #+ :cmu ext:*command-line-strings*
        )))
  (load-from-apply-dir "Apply" "load-apply.lisp")
  (load-apply)
  (load-from-apply-dir "Apply" "interaction.lisp")
  (dump-apply system-name))
(quit)

;; -----------------------------------------------------------------------------
;;; End of boot2.lisp
;; -----------------------------------------------------------------------------
