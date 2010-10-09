;;; Copyright 2005 T. Kurt Bond
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
;;; Title: Macros for b2h
;;;  Library: Tools
;;;  Authors: T. Kurt Bond
;;;  Maintainer: Henry G. Weller
;;;  Description:
;;    See b2h.em
;;;-----------------------------------------------------------------------------

(defmodule b2h-aux
  (syntax (macros)
   import (level1))

(defmacro with-input-file* (file-name . body)
  (let ((res (gensym))
        (s (gensym))
        (orig-source (gensym))
        (orig-sink (gensym)))
    `(let ((,s (make <file-stream> file-name: ,file-name mode: 'r))
           (,res ())
           (,orig-source (stream-source stdin))
           (,orig-sink (stream-sink stdin)))
       (reconnect ,s stdin)
       (unwind-protect (setq ,res (progn ,@body))
         ((setter stream-source) stdin ,orig-source)
         ((setter stream-sink) stdin ,orig-sink))
       ,res)))

;;;-----------------------------------------------------------------------------
)  ;; End of module b2h-aux
;;;-----------------------------------------------------------------------------
