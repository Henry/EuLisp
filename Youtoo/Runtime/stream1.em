;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: level1
;;;  Authors: Julian Padget, Andreas Kind
;;; Description: posix stream interface
;;;-----------------------------------------------------------------------------
(defmodule stream1
  (import (telos)
   export (O_RDONLY O_WRONLY O_RDWR O_APPEND O_NONBLOCK
           O_CREAT O_TRUNC O_EXCL mode-table
           eul_open eul_close eul_read eul_write
           eul_make_socket eul_socket_accept eul_make_connection
           eul_sprintf eul_sprintf_string
           strerror eul_socket_strerror
           ntok read-into-buffer hostname))

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
  (defextern eul_open (<string> <int> <int>) <int> "open")
  (defextern eul_close (<int>) <int> "close")
  (defextern eul_read (<int> <string> <int>) <int> "read")
  (defextern eul_write (<int> <string> <int>) <int> "write")
  (defextern eul_sprintf (<string> <int> <string> ptr) <int>)
  (defextern eul_sprintf_string (<string> <int> <int> <int> <string> <string>) <int>)
  (defextern eul_make_socket (<string> <string> <int>) <int>)
  (defextern eul_socket_accept (<int>) <int>)
  (defextern eul_make_connection (<string> <string> <string>) <int>)
  (defextern eul_socket_strerror (<int>) <string>)
  (defextern eul_strerror () <string>)

  (defun strerror () (eul_strerror))

;;;-----------------------------------------------------------------------------
;;; Tokenizer
;;;-----------------------------------------------------------------------------
  (defextern ntok (ptr ptr) ptr "ntok")
  (defextern read-into-buffer (<int> <string> <int>) <int> "read_into_buffer")

;;;-----------------------------------------------------------------------------
;;; Hostname
;;;-----------------------------------------------------------------------------
  (defextern eul_hostname () <string>)
  (defun hostname () (eul_hostname))

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
