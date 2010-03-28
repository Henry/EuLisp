;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;; -----------------------------------------------------------------------
;;;                     EuLisp System 'youtoo'
;;; -----------------------------------------------------------------------
;;;  Library: level1 (EuLisp Language Level1 Implementation)
;;;  Authors: Julian Padget, Andreas Kind
;;;  Description: sockets
;;; -----------------------------------------------------------------------
(defmodule socket
  (syntax (_macros)
   import (telos lock condition convert dynamic stream1 stream2 string)
   export (<socket> socket-port socket-host socket-descriptor
           socket-queue-size
           <connection> connection-host connection-port connectionp))
;;; --------------------------------------------------------------------
;;; Sockets
;;; --------------------------------------------------------------------
  (defclass <socket> ()
    ((port accessor: socket-port keyword: port: default: 4711)
     (host accessor: socket-host keyword: host: default: (hostname))
     (descriptor accessor: socket-descriptor)
     (queue-size accessor: socket-queue-size keyword: queue-size: default: 5))
    predicate: socketp)
  (defmethod initialize ((x <socket>) inits)
    (call-next-method)
    (let* ((port (convert (socket-port x) <string>))
           (n (socket-queue-size x))
           (fd (eul_make_socket port "tcp" n)))
      (if (int-binary= fd -1)
          (error (strerror) <stream-condition> value: x)
        (if (int-binary< fd -1)
            (error (eul_socket_strerror fd) <stream-condition> value: x)
          (progn
            ((setter socket-descriptor) x fd)
            (with-lock *open-file-streams*-lock
                       (setq *open-file-streams*
                             (cons x *open-file-streams*)))
            x)))))
;;; --------------------------------------------------------------------
;;; Socket connections
;;; --------------------------------------------------------------------
  (defclass <connection> (<file-stream>)
    ((host accessor: connection-host keyword: host: default: (hostname))
     (port accessor: connection-port keyword: port: default: 4711))
    keywords: (socket:)
    predicate: connectionp)
  (defmethod initialize ((x <connection>) inits)
    (call-next-method)
    (let ((s (init-list-ref inits socket:))
          host port fd)
      (if (socketp s)
          (progn
            ;; leads to accept
            (setq host (socket-host s))
            (setq port (socket-port s))
            ((setter connection-host) x host)
            ((setter connection-port) x port)
            (setq fd (eul_socket_accept (socket-descriptor s))))
        (progn
          ;; leads to connect
          (setq host (connection-host x))
          (setq port (connection-port x))
          (setq fd (eul_make_connection host (convert port <string>) "tcp"))))
      ;; error handling
      (if (int-binary= fd -1)
          (error (strerror) <stream-condition> value: x)
        (if (int-binary< fd -1)
            (error (eul_socket_strerror fd) <stream-condition> value: x)
          ()))
      (let* ((file-name (format () "~a:~a" host port))
             (fcb1 (make <file-control-block>
                         file-name: file-name
                         mode: 'r
                         descriptor: fd))
             (fcb2 (make <file-control-block>
                         file-name: file-name
                         mode: 'w
                         descriptor: fd)))
        ((setter stream-source) x fcb1)
        ((setter stream-sink) x fcb2)
        (with-lock *open-file-streams*-lock
                   (setq *open-file-streams*
                         (cons x *open-file-streams*)))
        x)))
)  ; end of module
