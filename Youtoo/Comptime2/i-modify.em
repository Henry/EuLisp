;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: comp (EuLisp to Bytecode Compiler -- EuLysses))
;;;  Authors: Andreas Kind, Keith Playford
;;; Description: predicates on files, modules and libraries
;;;-----------------------------------------------------------------------------
(defmodule i-modify
  (syntax (_macros _i-aux0)
   import (i-all)
   export (file-newer? file-exist? external-file-exist?
                       module-modified? C-module-modified? absolute-file-name
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
      (error "no such file ~a" name1))))

(defextern eul-file-newer? (<string> <string>) ptr "eul_file_newer_p")

;;;-----------------------------------------------------------------------------
;;; Module sources modified (compared with .i-file)
;;;-----------------------------------------------------------------------------
(defun module-modified? (module-name)
  (or *recompile*
      (let ((name1 (as-source-file-name module-name))
            (name2 (as-interface-file-name module-name)))
        (file-newer? name1 name2))))

;;;-----------------------------------------------------------------------------
;;; Library interface file modified (compared with .a-file)
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
;;; C linkable module file modified (compared with .bc-file)
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
)  ;; end of module
;;;-----------------------------------------------------------------------------
