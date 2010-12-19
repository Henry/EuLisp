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
;;; Title: stream syntax functions
;;;  Library: level-1
;;;  Authors: Julian Padget, Andreas Kind
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule _stream0
  (syntax (_boot0)
   import (level-1))

;;;-----------------------------------------------------------------------------
;;; (with-lock (lock-valued-expression) form*)
;;;-----------------------------------------------------------------------------
(defsyntax with-lock (lock . body)
  (let ((the-lock (gensym)))
    `(let ((,the-lock ,lock))
       (unwind-protect
           (progn (lock ,the-lock) ,@body)
         (unlock ,the-lock)))))

;;;-----------------------------------------------------------------------------
;;; (with-source (identifier expression) form*)
;;;  Temporarily reconnects source of identifier to expression
;;;-----------------------------------------------------------------------------
(defsyntax with-source (decl . body)
  (let ((the-source (gensym)))
    `(let ((,the-source (source ,(car decl))))
       (reconnect ,(car (cdr decl)) ,(car decl))
       (unwind-protect
           (progn ,@body)
         (reconnect ,the-source ,(car decl))))))

;;;-----------------------------------------------------------------------------
;;; (with-source (identifier expression) form*)
;;;  Temporarily reconnects sink of identifier to expression
;;;-----------------------------------------------------------------------------
(defsyntax with-sink (decl . body)
  (let ((the-source (gensym)))
    `(let ((,the-source (source ,(car decl))))
       (reconnect ,(car (cdr decl)) ,(car decl))
       (unwind-protect
           (progn ,@body)
         (reconnect ,the-source ,(car decl))))))

;;;-----------------------------------------------------------------------------
;;;  (with-input-file (var-and-file-name)  form*)
;;;  Temporarily binds var with input stream
;;;-----------------------------------------------------------------------------
(defsyntax with-input-file (var-and-file-name . body)
  (let ((s (car var-and-file-name))
        (file-name (car (cdr var-and-file-name)))
        (res (gensym)))
    `(let ((,s (make <file-stream> file-name: ,file-name))
           (,res ()))
       (unwind-protect (setq ,res (progn ,@body))
         (disconnect ,s))
       ,res)))

;;;-----------------------------------------------------------------------------
;;;  (with-output-file (var-and-file-name)  form*)
;;;  Temporarily connects stdout to file-name
;;;-----------------------------------------------------------------------------
(defsyntax with-output-file (var-and-file-name . body)
  (let ((s (car var-and-file-name))
        (file-name (car (cdr var-and-file-name)))
        (res (gensym)))
    `(let ((,s (make <file-stream> file-name: ,file-name mode: 'w))
           (,res ()))
       (unwind-protect (setq ,res (progn ,@body))
         (disconnect ,s))
       ,res)))

;;;-----------------------------------------------------------------------------
;;; (with-input-file-of-path (decl) form*)
;;;  Open input file with path
;;;-----------------------------------------------------------------------------
(defsyntax with-input-file-of-path (decl . body)
  (let ((s (car decl))
        (name (car (cdr decl)))
        (dir (car (cdr (cdr decl))))
        (path (car (cdr (cdr (cdr decl)))))
        (info (gensym))
        (file-name (gensym))
        (res (gensym)))
    `(let ((,info (apply file-lookup ,name ,path)))
       (if (null? ,info)
           (error <condition>
                   (fmt "No such file or directory ~a in ~a" ,name ,path))
         (let ((,file-name (car ,info))
               (,dir (cdr ,info)))
           (with-input-file (,s ,file-name) ,@body))))))

;;;-----------------------------------------------------------------------------
)  ;; End of module stream
;;;-----------------------------------------------------------------------------
