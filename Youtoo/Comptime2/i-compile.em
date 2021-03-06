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
;;; Title: compiling/linking
;;;  Library: comp (EuLisp to Bytecode Compiler -- EuLysses)
;;;  Authors: Andreas Kind, Keith Playford
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule i-compile
  (syntax (_syntax-1
           _i-aux0)
   import (i-all
           i-modify
           p-read
           p-env
           p-parse
           sx-obj
           sx-write
           ex-module
           ex-body
           cg-gen
           cg-asm
           cg-dld
           cg-interf
           cg-link
           cg-exec)
   export (compile
           interactive-compile
           link
           check-stop))

;;;-----------------------------------------------------------------------------
;;; Compile (read, parse, encode, assemble, write C-module and interface
;;;-----------------------------------------------------------------------------
(defun compile (module-name)
  (notify "Compiling module ~a ..." module-name)
  (setq *tmp-source-file-name* module-name)
  (let ((syntax-exprs ())
        (code-state ())
        (asm-state ())
        (C-state ()))
    (setq syntax-exprs (read-source-file module-name))
    (check-stop)
    (let ((m (parse-module syntax-exprs)))
      (dynamic-let ((*actual-module* m))
                   (check-stop)
                   (setq code-state (encode m ()))
                   (check-stop)
                   (setq asm-state (assemble m code-state))
                   (check-stop)
                   (setq C-state (decode m asm-state))
                   (check-stop)
                   (write-C-module-file m C-state)
                   (check-stop)
                   (write-interface-file m)
                   (notify "... module ~a compiled." module-name)
                   m))))

(defun interactive-compile (sexpr)
  (notify0 "Interactive compiling ...")
  (let* ((m (interactive-parse sexpr))
         (code-state (interactive-encode m))
         (asm-state (assemble m code-state)))
    (execute m asm-state)))

;;;-----------------------------------------------------------------------------
;;; Link
;;;-----------------------------------------------------------------------------
(defun link (module-name)
  (check-stop)
  (and (or *create-C-module* *create-C-library* *stand-alone*)
       (cond (*create-C-library*
              (create-C-library module-name))
             (*stand-alone*
              (create-stand-alone-application module-name)))))

(defun create-stand-alone-application (module-name)
  (notify "Creating stand-alone application of module ~a ..." module-name)
  (compile-C-file module-name)
  (compile-C-file-aux (make-symbol (as-C-hook-name module-name)))
  (notify "  Linking ~a.o with imports using ~a ..."
          module-name *C-ld*)
  (let* ((hook (absolute-file-name
                (as-C-hook-object-file-name module-name)))
         (vm (vm-link-string))
         (fff (fff-link-string))
         (ffl (ffl-link-string))
         (str (link-string module-name))
         (dir (or *object-dir*
                  (file-exist? (as-compiled-C-file-name module-name))))
         (dest (destination-link-string module-name dir))
         (libs (full-C-library-link-string))
         (gc (gc-link-string))
         (lib-dirs (full-C-library-dir-link-string))
         (sys-str (string-append-with-space
                   *C-ld* *C-cc-flags* "-o" dest str hook *main-link-string*
                   fff lib-dirs ffl vm libs gc *C-cc-libs*)))
    (notify0 sys-str)
    (if (= (system sys-str) 0)
        (if (null? *strip-stand-alone*) ()
          (if (= (system (string-append "strip " dest)) 0) ()
            (ct-error () "executable ~a can't be stipped correctly"
                      module-name)))
      (ct-error () "module ~a can't be linked correctly" module-name))))

(defun create-C-library (module-name)
  (notify "Creating library of module ~a ..." module-name)
  ;; Note that the global *dest-file-name* is cached and
  ;; temporarily set to () so that it is used for the output name
  ;; of the library not the intermediate object files.
  (let ((dest-file-name *dest-file-name*))
    (setq *dest-file-name* ())
    (compile-C-file module-name)
    (setq *dest-file-name* dest-file-name))
  (let* ((str (link-string module-name))
         (fff (fff-link-string))
         (dir (file-exist? (as-compiled-C-file-name module-name)))
         (dest (destination-library-link-string module-name dir))
         (sys-str1 (string-append-with-space *C-ar* dest str fff))
         (sys-str2  (string-append-with-space *C-ranlib* dest)))
    (notify0 sys-str1)
    (or (zero? (system sys-str1))
        (ct-error -1 "library ~a can't be created correctly" dest))
    (notify0 sys-str2)
    (or (zero? (system sys-str2))
        (ct-error -1 "archive ~a can't be converted correctly" dest)))
  (create-library-interface-file module-name))

(defun compile-C-file (module-name)
  (let* ((module (get-module module-name))
         (full-import (if module
                          (module-all-used-module-names? module)
                        (get-full-import-names module-name)))
         (lib-names (get-library-names))
         (import (binary- full-import lib-names)))
    (do1-list compile-C-file-aux import)
    (compile-C-file-aux module-name)))

(defun compile-C-file-aux (module-name)
  (let ((file-name (as-C-file-name module-name))
        (hook-sym (concatenate *tmp-start-source-file-name* '_)))
    (if (and (or (eq module-name *tmp-start-source-file-name*)
                 (eq module-name hook-sym)
                 (null? *no-recompile*))
             (C-module-modified? module-name))
        (let* ((abs-file-name (absolute-file-name file-name))
               (dir (file-exist? (as-C-file-name module-name)))
               (dest (destination-object-string module-name dir))
               (dest-dir (destination-object-dir dir))
               (sys-str (string-append-with-space
                         *C-cc* *C-cc-flags* "-o" dest "-c" abs-file-name)))
          (notify "  Compiling ~a using ~a ..." file-name *C-cc*)
          (notify0 sys-str)
          (mkdir dest-dir)
          (or (zero? (system sys-str))
              (ct-error -2 "file ~a can't be compiled correctly"
                        file-name)))
      (notify "  Module file ~a need not be recompiled." file-name))))

;;;-----------------------------------------------------------------------------
;;; Subsequent loading and compiling
;;; Written as method to avoid mutual module imports.
;;;-----------------------------------------------------------------------------
(defmethod compile-module (name)
  (if (member1-list name '(telos level-1 math))
      (load-module-interface name t)
    (dynamic-let ((*indent* (fmt "  ~a" (dynamic *indent*))))
                 (compile name))))

;;;-----------------------------------------------------------------------------
;;; Stop after pass
;;;-----------------------------------------------------------------------------
(defun check-stop ()
  (and *pass*
       (eq *pass* *stop-after-pass*)
       (progn
         (setq *stop-after-pass* ())
         (ct-exit))))

;;;-----------------------------------------------------------------------------
)  ;; End of module i-compile
;;;-----------------------------------------------------------------------------
