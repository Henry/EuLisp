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
;;; Title: posix stream interface
;;;  Library: level1
;;;  Authors: Julian Padget, Andreas Kind
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule stream1
  (import (telos)
   export (O_RDONLY
           O_WRONLY
           O_RDWR
           O_APPEND
           O_NONBLOCK
           O_CREAT
           O_TRUNC
           O_EXCL
           mode-table
           eul_open
           eul_close
           eul_read
           eul_write
           eul_make_socket
           eul_socket_accept
           eul_make_connection
           eul_sprintf
           eul_sprintf_string
           strerror
           eul_socket_strerror
           ntok
           read-into-buffer
           hostname))

;;;-----------------------------------------------------------------------------
;;; Posix codes
;;;-----------------------------------------------------------------------------
(defextern get-posix-codes () ptr "eul_posix_codes")
(defconstant *posix-codes* (get-posix-codes))
(defconstant O_RDONLY (primitive-ref *posix-codes* 0))
(defconstant O_WRONLY (primitive-ref *posix-codes* 1))
(defconstant O_RDWR (primitive-ref *posix-codes* 2))
(defconstant O_APPEND (primitive-ref *posix-codes* 3))
(defconstant O_NONBLOCK (primitive-ref *posix-codes* 4))
(defconstant O_CREAT (primitive-ref *posix-codes* 5))
(defconstant O_TRUNC (primitive-ref *posix-codes* 6))
(defconstant O_EXCL (primitive-ref *posix-codes* 7))
(defconstant mode-table
  (list 'r O_RDONLY
        'w (int-binary+ O_WRONLY (int-binary+ O_CREAT O_TRUNC))
        'a (int-binary+ O_WRONLY O_APPEND)
        '|r+| O_RDWR
        '|w+| O_RDWR
        '|a+| (int-binary+ O_RDWR O_APPEND)))

;;;-----------------------------------------------------------------------------
;;; C interface
;;;-----------------------------------------------------------------------------
(defextern eul_open (<string> <fpi> <fpi>) <fpi> "open")
(defextern eul_close (<fpi>) <fpi> "close")
(defextern eul_read (<fpi> <string> <fpi>) <fpi> "read")
(defextern eul_write (<fpi> <string> <fpi>) <fpi> "write")
(defextern eul_sprintf (<string> <fpi> <string> ptr) <fpi>)
(defextern eul_sprintf_string (<string> <fpi> <fpi> <fpi> <string> <string>) <fpi>)
(defextern eul_make_socket (<string> <string> <fpi>) <fpi>)
(defextern eul_socket_accept (<fpi>) <fpi>)
(defextern eul_make_connection (<string> <string> <string>) <fpi>)
(defextern eul_socket_strerror (<fpi>) <string>)
(defextern eul_strerror () <string>)

(defun strerror () (eul_strerror))

;;;-----------------------------------------------------------------------------
;;; Tokenizer
;;;-----------------------------------------------------------------------------
(defextern ntok (ptr ptr) ptr "ntok")
(defextern read-into-buffer (<fpi> <string> <fpi>) <fpi> "read_into_buffer")

;;;-----------------------------------------------------------------------------
;;; Hostname
;;;-----------------------------------------------------------------------------
(defextern eul_hostname () <string>)
(defun hostname () (eul_hostname))

;;;-----------------------------------------------------------------------------
)  ;; End of module stream1
;;;-----------------------------------------------------------------------------
