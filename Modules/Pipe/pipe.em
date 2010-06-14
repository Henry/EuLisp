;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: pipe
;;;  Authors: Rob Simmons, Andreas Kind
;;; Description: fork child process and control i/o with file stream
;;;  Compilation
;;    make touch
;;    make
;;;-----------------------------------------------------------------------------
(defmodule pipe
  (syntax (macros)
   import (level1)
   export (<pipe> pipe-process))

;;;------------------------------------------------------------------------
;;; The pipe class
;;;------------------------------------------------------------------------
  (defclass <pipe> (<file-stream>)
    ((process accessor: pipe-process keyword: process: requiredp: t))
    predicate: pipe?)

  (defmethod initialize ((x <pipe>) inits)
    (call-next-method)
    (let* ((name (convert (pipe-process x) <string>))
           (fds (eul-fork-child name)))
      (if (integer? fds)
          (error (eul-pipe-strerror fds) <stream-condition> value: x)
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
  )  ;; end of module
;;;-----------------------------------------------------------------------------
