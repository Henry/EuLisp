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
;;;  Title: the cover for the code-generation parts
;;;  Description:
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Ingo Mohr
;;;-----------------------------------------------------------------------------

#module code-generator
(import
 (level-1-eulisp
  (only (?configuration configuration?) configuration)
  accessors lzs
  predicates
  c-code
  c-data
  code-identifier
  expand-literal
  (only (reset-code-identifier extend-identifier-table) code-identifier)
  generate-header-file
  generate-def-file
  debugging
  (only (*print-circle* *print-pretty*
                        mapc reverse
                        open make-pathname string-downcase string format)
        common-lisp))

 syntax (level-1-eulisp)

 export (generate-code))

(defun generate-c-file (main-module modules)
  (dynamic-let ((code-output
                 (if (code-debug) t
                   (open (make-pathname :name (string-downcase
                                               (string
                                                (?identifier main-module)))
                                        :type "c")
                         :direction :output :if-exists :new-version)))
                )
               (unwind-protect
                   (generate-c-code main-module modules)
                 (unless (eq (dynamic code-output) t)
                         (close (dynamic code-output))))))

(defun generate-inst-file (main-module modules)
  (reset-c-data)
  (dynamic-let ((code-output
                 (if (code-debug) t
                   (open (make-pathname :name (string-downcase
                                               (string
                                                (?identifier main-module)))
                                        :type "inst")
                         :direction :output :if-exists :new-version)))
                )
               (unwind-protect
                   (generate-c-data)
                 (unless (eq (dynamic code-output) t)
                         (close (dynamic code-output))))))

(defun generate-h-file (main-module modules)
  (dynamic-let ((code-output
                 (if (code-debug) t
                   (open (make-pathname :name (string-downcase
                                               (string
                                                (?identifier main-module)))
                                        :type "h")
                         :direction :output :if-exists :new-version)))
                )
               (unwind-protect
                   (generate-header-file main-module modules)
                 (unless (eq (dynamic code-output) t)
                         (close (dynamic code-output))))))

(defun generate-def-file (main-module modules)
  (dynamic-let ((code-output
                 (if (code-debug) t
                   (open (make-pathname :name (string-downcase
                                               (string
                                                (?identifier main-module)))
                                        :type "def")
                         :direction :output :if-exists :new-version)))
                )
               (unwind-protect
                   (generate-module-def main-module modules)
                 (unless (eq (dynamic code-output) t)
                         (close (dynamic code-output))))))

(defun generate-code (main-module modules)
  (let ((*print-circle* ())
        (*print-pretty* ())
        )
    ;; to get the definition order of classes reverse the module list such that
    ;; the top module is the last one and the most basic is the first one
    (setq modules (reverse modules))

    (reset-code-identifier)
    (reset-c-code)
    (mapc #'collect-c-identifiers modules)
    (map-modules #'expand-literal #'?class-def-list modules)
    (map-modules #'expand-literal #'?sym-list modules)
    ;; all functions are needed as objects if they are exported for Lisp
    (map-modules (lambda (fun)
                   (when (exported-for-lisp? fun)
                         (expand-literal fun)))
                 #'?fun-list modules)
    (name-objects main-module modules)
    (when (?exports main-module)
          (format t "~%~(~A~).h..." (?identifier main-module))
          (generate-h-file main-module modules))
    (format t "~%~(~A~).c..." (?identifier main-module))
    (generate-c-file main-module modules)
    (format t "~%~(~A~).inst..." (?identifier main-module))
    (generate-inst-file main-module modules)
    (unless (eq *compilation-type* :application)
            (format t "~%~(~A~).def..." (?identifier main-module))
            (generate-def-file main-module modules))
    ))

(defun name-objects (main-module modules)
  (mapc #'name-global-object modules) ; naming the module objects
  (name-global-object ())
  (mapc (if (eq *compilation-type* :application)
            #'name-exported-object ; do special naming for C-interface
          #'name-global-object)
        (?exports main-module))
  (mapc #'name-global-objects modules)
  )

(defun name-global-objects (module)
  (when (?toplevel-forms module)
        (name-global-object (?toplevel-forms module)))
  (mapc #'name-global-object (?class-def-list module))
  (mapc (lambda (con)
          (when (or (null? (fun-p (?value con)))
                    (function-needed-p (?value con))
                    (imported-p (?value con)))
                (name-global-object con)))
        (?named-const-list module))
  (mapc (lambda (fun)
          (when (or (function-needed-p fun)
                    (imported-p fun))
                (name-global-object fun)))
        (?fun-list module))
  (mapc #'name-global-object (?var-list module))
  (mapc #'name-global-object (?sym-list module))
  )

(defun collect-c-identifiers (module)
  (labels ((add-c-identifier (object)
                             (when (?code-identifier object)
                                   (extend-identifier-table (?code-identifier object)))))
          (mapc #'add-c-identifier (?fun-list module))
          (mapc #'add-c-identifier (?class-def-list module))
          (mapc #'add-c-identifier (?named-const-list module))
          (mapc #'add-c-identifier (?var-list module))
          (mapc #'add-c-identifier (?sym-list module))
          ))
#module-end ; code-generator
