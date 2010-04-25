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
   export (file-newer-p file-exist-p external-file-exist-p
           module-modified-p C-module-modified-p absolute-file-name
           library-newer-p))

;;;-----------------------------------------------------------------------------
;;; File predicates
;;;-----------------------------------------------------------------------------
  (defun file-exist-p (name)
    (or (*get-load-dir* name)
        (let ((info (apply file-lookup name *load-path*)))
          (and info
               (let ((dir (cdr info)))
                 ((setter *get-load-dir*) name dir)
                 dir)))))

  (defun external-file-exist-p (name)
    (let ((info (apply file-lookup name *C-library-load-path*)))
      (if info (cdr info) ())))

  (defun file-newer-p (name1 name2)
    (let ((info1 (apply file-lookup name1 *load-path*))
          (info2 (apply file-lookup name2 *load-path*)))
      (if info1
          (if info2
              (eul-file-newer-p (car info1) (car info2))
            name1)
        (error "no such file ~a" name1))))

  (defextern eul-file-newer-p (<string> <string>) ptr "eul_file_newer_p")

;;;-----------------------------------------------------------------------------
;;; Module sources modified (compared with .i-file)
;;;-----------------------------------------------------------------------------
  (defun module-modified-p (module-name)
    (or *recompile*
        (let ((name1 (as-source-file-name module-name))
              (name2 (as-interface-file-name module-name)))
          (file-newer-p name1 name2))))

;;;-----------------------------------------------------------------------------
;;; Library interface file modified (compared with .a-file)
;;;-----------------------------------------------------------------------------
  (defun library-newer-p (lib-name module-name) ())
  ;    (let ((file-name1 (as-C-library-interface-file-name lib-name))
  ;         (file-name2 (as-C-library-file-name module-name))
  ;         (tmp-load-path *load-path*)
  ;         (result ()))
  ;      (setq *load-path* *C-library-load-path*)
  ;      (setq result
  ;           (and (file-exist-p file-name1)
  ;                (file-exist-p file-name2)
  ;                (file-newer-p file-name1 file-name2)))
  ;      (setq *load-path* tmp-load-path)
  ;      result))

;;;-----------------------------------------------------------------------------
;;; C linkable module file modified (compared with .bc-file)
;;;-----------------------------------------------------------------------------
  (defun C-module-modified-p (module-name)
    (let ((name1 (as-C-file-name module-name))
          (name2 (as-compiled-C-file-name module-name)))
      (file-newer-p name1 name2)))

   (defun absolute-file-name (file-name)
     (let ((info (apply file-lookup file-name *load-path*)))
       (if info
           (car info)
         (ct-error "" "file ~a does not exist" file-name))))

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
