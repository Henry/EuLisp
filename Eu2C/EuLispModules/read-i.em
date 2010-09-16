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
;;  The function supplied to set-dispatch-macro-characte has the arguments:
;;          stream
;;          char    the subchar
;;
;;  (set-dispatch-macro-character #\# % read-tail-short-literals)
;;
;;  (defun read-tail-short-literals (stream subchar)
;;    (let ((subsubchar (read-char stream))
;;          (object (read stream)))
;;      (if (equal subsubchar #\i)
;;        (progn (print "make signed-word-integer from:")
;;               (print object))
;;        (progn (print "other from:")
;;               (print object)))))
;;;  Authors: Rainer Rosenmuller
;;;-----------------------------------------------------------------------------

(defmodule read-i
  (import (tail
           apply
           (only (equal)
                 compare)
           (only (<character>)
                 character)
           (only (print)
                 print))
   syntax (tail)
   export (get-dispatch-macro-character
           set-dispatch-macro-character))

(deflocal
  *extension-macro-table*
  ;;(make <table>)
  ())

(defun set-dispatch-macro-character
  (disp-char subchar function . readtable-list)
  (if readtable-list
      (print "there is no readtable in Eulisp, argument will be ignored")
    ())
  (if (equal #\# disp-char)
      (progn
        ;;((setter element) *extension-macro-table* subchar function)
        (setq *extension-macro-table*
              (cons (cons subchar function)
                    *extension-macro-table*))
        )
    (print "there is no other dispatch macro character (as #\#) in Eulisp, no changes")
    ))

(defun get-dispatch-macro-character
  (disp-char subchar . readtable-list)
  ;;(element *extension-macro-table* subchar)
  (assoc-char-eql *extension-macro-table* subchar))

(defun assoc-char-eql (plist subchar)
  (if plist
      (if (equal (%cast <character>
                        (car (%cast <cons>
                                    (car (%cast <cons> plist)))))
                 (%cast <character> subchar))
          (cdr (car plist))
        (assoc-char-eql (cdr plist) subchar))
    ()))

;;;-----------------------------------------------------------------------------
)  ;; End of module read-i
;;;-----------------------------------------------------------------------------
