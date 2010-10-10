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
;;; Title: fork child process and control i/o with file stream
;;;  Library: pipe
;;;  Authors: Rob Simmons, Andreas Kind
;;;  Maintainer: Henry G. Weller
;;;  Compilation:
;;    make touch
;;    make
;;;-----------------------------------------------------------------------------

(defmodule pipe
  (syntax (syntax-0)
   import (level-0)
   export (<pipe>
           pipe-process))

;;;------------------------------------------------------------------------
;;; The pipe class
;;;------------------------------------------------------------------------
(defclass <pipe> <file-stream>
  ((process accessor: pipe-process keyword: process: required?: t))
  predicate: pipe?)

(defmethod initialize ((x <pipe>) inits)
  (call-next-method)
  (let* ((name (convert (pipe-process x) <string>))
         (fds (eul-fork-child name)))
    (if (integer? fds)
        (error <stream-condition>
               (eul-pipe-strerror fds) value: x)
      (let* ((fcb1 (make <file-control-block>
                         file-name: name
                         mode: 'r
                         descriptor: (vector-ref fds 1)))
             (fcb2 (make <file-control-block>
                         file-name: name
                         mode: 'w
                         descriptor: (vector-ref fds 2))))
        ((setter stream-source) x fcb1)
        ((setter stream-sink) x fcb2)
        ;;((setter stream-mode) x 'rw)
        ;; Set the childs process id
        ((setter pipe-process) x (vector-ref fds 0))
        (with-lock *open-file-streams*-lock
                   (setq *open-file-streams*
                         (cons x *open-file-streams*)))
        x))))

(defmethod disconnect ((x <pipe>))
  (let ((child-process-id (pipe-process x)))
    (if (integer? child-process-id)
        ;; Quit the child
        (eul-kill child-process-id 3)
      ()))
  ;; Closing file descriptors sometimes causes problems ...
  (call-next-method))

;;;-----------------------------------------------------------------------------
;;; External functions from eul-pipe.c
;;;-----------------------------------------------------------------------------
(defextern eul-fork-child (<string>) ptr  "eul_fork_child")
(defextern eul-pipe-strerror (<int>) <string> "eul_pipe_strerror")
(defextern eul-kill (<int> <int>) <int> "kill")

;;;-----------------------------------------------------------------------------
)  ;; End of module pipe
;;;-----------------------------------------------------------------------------
