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
;;; Title: predicates on files, modules and libraries
;;;  Library: comp (EuLisp to Bytecode Compiler -- EuLysses))
;;;  Authors: Andreas Kind, Keith Playford
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule i-modify
  (syntax (_macros
           _i-aux0)
   import (i-all)
   export (file-newer?
           file-exist?
           external-file-exist?
           module-modified?
           C-module-modified?
           absolute-file-name
           library-newer?))

;;;-----------------------------------------------------------------------------
;;; File predicates
;;;-----------------------------------------------------------------------------
(defun file-exist? (name)
  (or (*get-load-dir* name)
      (let ((info (apply file-lookup name *load-path*)))
        (and info
             (let ((dir (cdr info)))
               ((setter *get-load-dir*) name dir)
               dir)))))

(defun external-file-exist? (name)
  (let ((info (apply file-lookup name *C-library-load-path*)))
    (if info (cdr info) ())))

(defun file-newer? (name1 name2)
  (let ((info1 (apply file-lookup name1 *load-path*))
        (info2 (apply file-lookup name2 *load-path*)))
    (if info1
        (if info2
            (eul-file-newer? (car info1) (car info2))
          name1)
      (error <condition> (fmt "no such file ~a" name1)))))

(defextern eul-file-newer? (<string> <string>) ptr "eul_file_newer_p")

;;;-----------------------------------------------------------------------------
;;; Module sources modified (compared with .[ihc] files)
;;;-----------------------------------------------------------------------------
(defun module-modified? (module-name)
  (or (file-newer?
       (as-source-file-name module-name)
       (as-interface-file-name module-name))
      (file-newer?
       (as-source-file-name module-name)
       (as-included-C-file-name module-name))
      (file-newer?
       (as-source-file-name module-name)
       (as-C-file-name module-name))))

;;;-----------------------------------------------------------------------------
;;; Library interface file modified (compared with .a file)
;;;-----------------------------------------------------------------------------
(defun library-newer? (lib-name module-name) ())
;    (let ((file-name1 (as-C-library-interface-file-name lib-name))
;         (file-name2 (as-C-library-file-name module-name))
;         (tmp-load-path *load-path*)
;         (result ()))
;      (setq *load-path* *C-library-load-path*)
;      (setq result
;           (and (file-exist? file-name1)
;                (file-exist? file-name2)
;                (file-newer? file-name1 file-name2)))
;      (setq *load-path* tmp-load-path)
;      result))

;;;-----------------------------------------------------------------------------
;;; C linkable module file modified (compared with .o file)
;;;-----------------------------------------------------------------------------
(defun C-module-modified? (module-name)
  (let ((name1 (as-C-file-name module-name))
        (name2 (as-compiled-C-file-name module-name)))
    (file-newer? name1 name2)))

(defun absolute-file-name (file-name)
  (let ((info (apply file-lookup file-name *load-path*)))
    (if info
        (car info)
      (ct-error "" "file ~a does not exist" file-name))))

;;;-----------------------------------------------------------------------------
)  ;; End of module i-modify
;;;-----------------------------------------------------------------------------
