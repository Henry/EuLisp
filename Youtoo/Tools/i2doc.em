;;; Copyright 2003 T. Kurt Bond
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
;;; Title: Generate module interfaces documentation: bindings.txt
;;;  Library: Tools
;;;  Authors: T. Kurt Bond
;;;  Maintainer: Henry G. Weller
;;;  Description:
;;    Organization of the file bindings.txt:
;;      Header
;;      0. Legend
;;      1. Bindings in level1 (Lib.${ARCH}/liblevel1.i)
;;      2. Bindings in macros (Runtime/macros.i)
;;      3. Bindings in eval (Lib.${ARCH}/libeval.i)
;;;-----------------------------------------------------------------------------

(defmodule i2doc
  (syntax (macros)
   import (level1)
   export (main))

(defextern strftime (<string>) <string> "eul_strftime")

(deflocal note-renaming ())
(deflocal internal-sort t)
(deflocal print-header t)
(deflocal saved-bindings '())
(deflocal header-index 0)
(deflocal line (make <string> size: 75 fill-value: #\-))

(defun moduleize (filename)
  (let* ((len (size filename))
         (start (if (binary= "lib" (substring filename 0 3)) 3 0))
         (end (if (binary= ".i" (substring filename (- len 2) len))
                  (- len 2)
                len)))
    (substring filename start end)))

(defun assq (key alist)
  (if (null? alist)
      '()
    (let ((first (car alist))
          (rest (cdr alist)))
      (if (eq key (car first))
          (cdr first)
        (assq key rest)))))

(defun process-export (export)
  (let* ((name (assq 'name export))
         (pos (assq 'pos export))
         (origin (assq 'origin export))
         (omodule (and origin (car origin)))
         (oname (and origin (cdr origin)))
         (class (assq 'class export)))
    (let* ((renamed (if (and note-renaming (not (eq name oname)))
                        " !!!" "")))
      (when (and pos (not (eq class 'ff)))
            (let ((s (fmt "  ~a ~a ~a ~a~a"
                          name pos omodule oname renamed)))
              (if internal-sort
                  (setq saved-bindings (cons saved-bindings))
                (print s nl)))))))

(defun process-file (filename)
  (let* ((module (with-input-file (f filename)
                                  (read-s-expression f)))
         (specs (caddr module))
         (export-clause (member 'export specs))
         (exports (if export-clause (cadr export-clause) ()))
         )
    (when print-header
          (format "\n~a\n~d. Bindings in ~a\n~a\n\n"
                  line header-index (moduleize filename) line))
    (setq header-index (+ header-index 1))
    (do process-export exports)
    (when internal-sort
          (do print (sort saved-bindings))
          (setq saved-bindings '()))
    ))

(defun yesno (bool) (if bool "yes" "no"))

(deflocal usage-string
  `("usage: i2doc [options] interfacefile.i ..."
    "where options are:"
    ,(fmt "-h\tPrint header? (default: ~a)" (yesno print-header))
    ,(fmt "-r\tNote renamings? (default: ~a" (yesno note-renaming))
    ,(fmt "-s\tUse internal sort? (default: ~a)" (yesno internal-sort))
    ))

(defun usage ()
  (do (lambda (msg) (sprint stderr msg nl)) usage-string)
  (sflush stderr)
  (exit 1))

(defun parse-args (args)
  (if (null? args)
      '()
    (let ((first (car args))
          (rest  (cdr args)))
      (cond
        ((binary= first "-s")            ;Sort
         (setq internal-sort (not internal-sort))
         (parse-args rest))
        ((binary= first "-h")            ;Header
         (setq print-header (not print-header))
         (parse-args rest))
        ((binary= first "-r")            ;Note renamings
         (setq note-renaming (not note-renaming))
         (parse-args rest))
        ((or (binary= first "-?")
             (binary= first "-help")
             (binary= first "--help"))
         (usage))
        (t                             ;Not known option, so must be filename
         args)))))

(defun print-first-header ()
  (print "youtoo Functions" nl)
  (format "  Generated ~a\n" (strftime "%d %B %Y, %X"))
  (print "" nl)
  (print line nl)
  (print "0. Legend" nl)
  (print line nl)
  (print "" nl)
  (print "  The binding entries are listed in the following format:" nl)
  (print "" nl)
  (print "    NAME INDEX_IN_ORIGINAL_MODULE ORIGINAL_MODULE_NAME ORIGINAL_NAME" nl)
  (print "" nl)
  (print "  Global variables begin with * and end with * (e.g. *foo*)." nl)
  (print "  Classes begin with < and end with > (e.g. <foo>)." nl)
  (print "" nl)
  (print "  Not all of the level1 and macro functions are mentioned in the" nl)
  (print "  EuLisp Definition. Please, consult the EuLisp Definition for further" nl)
  (print "  details." nl)
  (setq header-index 1))

(defun main (args)
  (let ((filenames (parse-args args)))
    (unless filenames                 ; must have at least one file.
            (usage))
    (when print-header
          (print-first-header))
    (do process-file filenames)))

(main (cdr ((converter <list>) *argv*))) ;get rid of program name.

;;;-----------------------------------------------------------------------------
)  ;; End of module i2doc
;;;-----------------------------------------------------------------------------
