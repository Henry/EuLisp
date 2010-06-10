;;; sending stuff over sockets

(defmodule sexpr

    (import (level0 socket))

  (defconstant null-tag 0)
  (defconstant cons-tag 1)
  (defconstant fpi-tag 2)
  (defconstant double-tag 3)
  (defconstant symbol-tag 4)
  (defconstant string-tag 5)
  (defconstant vector-tag 6)
  (defconstant char-tag 7)
  (defconstant structure-tag 8)

  (defcondition <send-error> <general-error>)
  (defcondition <recv-error> <general-error>)

  (defgeneric send (obj port))

  (defmethod send (obj port)
    (error "don't know how to send object"
           <send-error>
           value: obj))

  (defmethod send ((obj <null>) port)
    (send-int port null-tag))

  (defmethod send ((obj <cons>) port)
    (send-int port cons-tag)
    (send (car obj) port)
    (send (cdr obj) port))

  (defmethod send ((obj <fpi>) port)
    (send-int port fpi-tag)
    (send-int port obj))

  (defmethod send ((obj <double-float>) port)
    (send-int port double-tag)
    (send-float port obj))

  (defmethod send ((obj <symbol>) port)
    (send-int port symbol-tag)
    (send-string port (convert obj <string>)))

  (defmethod send ((obj <string>) port)
    (send-int port string-tag)
    (send-string port obj))

  (defmethod send ((obj <vector>) port)
    (send-int port vector-tag)
    (send-vector obj port))

  (defmethod send ((obj <char>) port)
    (send-int port char-tag)
    (send-int port (convert obj <integer>)))

  (defmethod send ((obj <structure>) port)
    (send-int port structure-tag)
    (send-structure obj port))

  (defun send-vector (vec port)
    (send-int port (vector-length vec))
    (do (lambda (elt) (send elt port)) vec))

  (defun recv (port)
    (let ((tag (recv-int port)))
      (cond ((= tag null-tag) ())
            ((= tag cons-tag)
             (let ((car (recv port))
                   (cdr (recv port)))
               (cons car cdr)))
            ((= tag fpi-tag) (recv-int port))
            ((= tag double-tag) (recv-float port))
            ((= tag symbol-tag) (convert (recv-string port) <symbol>))
            ((= tag string-tag) (recv-string port))
            ((= tag vector-tag) (recv-vector port))
            ((= tag char-tag) (convert (recv-int port) <integer>))
            ((= tag structure-tag) (recv-structure port))
            (t (error "unknown tag type in recv"
                      <recv-error>
                      value: tag)))))

  (defun recv-vector (port)
    (let* ((len (recv-int port))
           (vec (make-vector len)))
      (labels ((loop (n)
                     (when (< n len)
                           ((setter vector-ref) vec n (recv port))
                           (loop (+ n 1)))))
              (loop 0))
      vec))

  (export <send-error> <recv-error> send recv)

  )
