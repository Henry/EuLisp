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
;;;  Title: Signalling Compiler Messages
;;;  Description:
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Ingo Mohr
;;;-----------------------------------------------------------------------------

#module messages
(import (level-0
         configuration
         lzs
         accessors
         (only (format
                *standard-output*)
               common-lisp))
 syntax (level-0
         dynamic)
 export (write-message
         write-message-conditional))

(defvar *current-module* ())

(deflocal *line-length* 75)

(defun write-message (key message . args)
  (write-msg key message args))

(defun write-message-conditional (level key format . args)
  (unless (< (dynamic *info-level*) level)
          (write-msg key format args)))

(defun write-msg (key message args)
  (when key
        (let ((header
               (format () "~A~@[ in module '~(~A~)'~]"
                       key
                       (if (dynamic *current-module*)
                           (?identifier (dynamic *current-module*))
                         ()))))
          (format *standard-output*
                  "~%--- ~A ~V{-~}~%~
               ~@[--- in form ~((~2{~A ~} ...)~)~%~]"
                  header
                  (- *line-length* (length header) 5)
                  '(())
                  (dynamic current-defining-form)
                  )))
  (format *standard-output* "~?" message args)
  (when key (format *standard-output* "~%~V{-~}" *line-length* '(()))))

#module-end
