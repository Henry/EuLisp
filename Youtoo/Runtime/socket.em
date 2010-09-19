;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: level1
;;;  Authors: Julian Padget, Andreas Kind
;;; Description: sockets
;;;-----------------------------------------------------------------------------
(defmodule socket
  (syntax (_macros)
   import (telos lock condition convert dynamic stream1 stream2 string)
   export (<socket> socket? socket-port socket-host socket-descriptor
                    socket-queue-size
                    <connection> connection-host connection-port connection?))

;;;-----------------------------------------------------------------------------
;;; Sockets
;;;-----------------------------------------------------------------------------
(defclass <socket> ()
  ((port accessor: socket-port keyword: port: default: 4711)
   (host accessor: socket-host keyword: host: default: (hostname))
   (descriptor accessor: socket-descriptor)
   (queue-size accessor: socket-queue-size keyword: queue-size: default: 5))
  predicate: socket?)

(defmethod initialize ((x <socket>) inits)
  (call-next-method)
  (let* ((port (convert (socket-port x) <string>))
         (n (socket-queue-size x))
         (fd (eul_make_socket port "tcp" n)))
    (if (int-binary= fd -1)
        (error <stream-condition>
                (strerror) value: x)
      (if (int-binary< fd -1)
          (error <stream-condition>
                 (eul_socket_strerror fd) <stream-condition> value: x)
        (progn
          ((setter socket-descriptor) x fd)
          (with-lock *open-file-streams*-lock
                     (setq *open-file-streams*
                           (cons x *open-file-streams*)))
          x)))))

;;;-----------------------------------------------------------------------------
;;; Socket connections
;;;-----------------------------------------------------------------------------
(defclass <connection> <file-stream>
  ((host accessor: connection-host keyword: host: default: (hostname))
   (port accessor: connection-port keyword: port: default: 4711))
  keywords: (socket:)
  predicate: connection?)

;;;-----------------------------------------------------------------------------
)  ;; End of module
;;;-----------------------------------------------------------------------------
