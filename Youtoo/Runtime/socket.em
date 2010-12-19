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
;;; Title: sockets
;;;  Library: level-1
;;;  Authors: Julian Padget, Andreas Kind
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule socket
  (syntax (_syntax-1)
   import (telos
           lock
           condition
           convert
           dynamic
           stream1
           stream2
           string)
   export (<socket>
           socket?
           socket-port
           socket-host
           socket-descriptor
           socket-queue-size
           <connection>
           connection-host
           connection-port
           connection?))

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
    (if (fpi-binary= fd -1)
        (error <stream-condition>
                (strerror) value: x)
      (if (fpi-binary< fd -1)
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
)  ;; End of module socket
;;;-----------------------------------------------------------------------------
