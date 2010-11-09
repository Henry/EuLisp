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
;;; Title: Extras for match
;;;  Library: match
;;;  Authors: Andreas Kind
;;;  Maintainer: Henry G. Weller
;;;  Description:
;;    See match.em
;;;-----------------------------------------------------------------------------

(defmodule extras
  (syntax (syntax-0)
   import (level-0)
   export (end-of-stream?))

;; It's a lot simple to do (until (end-of-stream? s) (read s))
;; than to use exceptions or the eof-object argument to catch end of file.
(defgeneric end-of-stream? (stream))

(defmethod end-of-stream? ((s <stream>))
  (let ((scb (stream-source s)))
    (and (null? (control-block-buffer scb))
         (fpi-binary= (fill-buffer s) 0))))

(defmethod end-of-stream? ((ss <string-stream>))
  (let ((scb (stream-source ss)))
    (and (fpi-binary= (control-block-buffer-cnt scb)
                      (string-size (control-block-buffer scb)))
         (fpi-binary= (fill-buffer ss) 0)
         t)))

(defmethod end-of-stream? ((fs <file-stream>))
  (let ((fcb (stream-source fs)))
    (and (fpi-binary= (control-block-buffer-cnt fcb) 0)
         (fpi-binary= (fill-buffer fs) 0)
         t)))

;;;-----------------------------------------------------------------------------
)  ;; End of module extras
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;; Testing
;;;-----------------------------------------------------------------------------

(deflocal ss (make <string-stream> string: "this"))
(deflocal scb (stream-source ss))
(control-block-buffer-cnt scb)
(control-block-buffer-pos scb)
(string-size (control-block-buffer scb))
(end-of-stream? ss)
(deflocal fs (make <file-stream> file-name: "/dev/null" mode: 'r))
(with-output-file (f "/tmp/end-of-stream-test.dat")
                  (print "this" f nl))
(deflocal fs (make <file-stream> file-name: "/tmp/end-of-stream-test.dat"))
(let loop ((i 0)) (cond ((< i 5) (read-char fs) (loop (+ i 1)))))
(end-of-stream? fs)

;;;-----------------------------------------------------------------------------
