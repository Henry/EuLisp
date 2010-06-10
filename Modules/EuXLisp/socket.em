;;; this code written by David Halls
;;; Euscheme code Copyright (c) 1994 Russell Bradford

;;; to use this the interpreter must have been compiled with -DSOCK

(defmodule socket
    (import (root setter convert))
  (export <socket> socket-fd socketp make-socket connect bind
   listen accept set-block set-nonblock reuse noreuse close-socket
   shutdown peeraddr peerport sockaddr sockport host->ip
   ip->host fd-zero-read fd-set-read fd-isset-read select-read
   fd-zero-write fd-set-write fd-isset-write select-write
   send-int recv-int send-float recv-float send-string recv-string
   port-fd port-unbuffered port-block-buffered port-line-buffered)

  (defclass <socket> ()
    ((fd keyword: fd:
         accessor: socket-fd
         default: nil))
    constructor: (socket-constructor fd:)
    predicate: socketp)

  (define (make-socket)
          (socket-constructor (socket-socket)))

  (define-generic (connect s name port))

  (define-method (connect (s <socket>) (name <string>) (port <integer>))
                 (socket-connect (socket-fd s) name port))

  (define-generic (bind s port))

  (define-method (bind (s <socket>) (port <integer>))
                 (socket-bind (socket-fd s) port))

  (define-generic (listen s backlog))

  (define-method (listen (s <socket>) (backlog <integer>))
                 (socket-listen (socket-fd s) backlog))

  (define-generic (accept s))

  (define-method (accept (s <socket>))
                 (socket-constructor (socket-accept (socket-fd s))))

  (define-generic (set-block s))

  (define-method (set-block (s <socket>))
                 (socket-block (socket-fd s)))

  (define-generic (set-nonblock s))

  (define-method (set-nonblock (s <socket>))
                 (socket-nonblock (socket-fd s)))

  (define-generic (reuse s))

  (define-method (reuse (s <socket>))
                 (socket-reuse (socket-fd s)))

  (define-generic (noreuse s))

  (define-method (noreuse (s <socket>))
                 (socket-noreuse (socket-fd s)))

  (define-generic (close-socket s))

  (define-method (close-socket (s <socket>))
                 (socket-close (socket-fd s)))

  (define-generic (shutdown s how))

  (define-method (shutdown (s <socket>) (how <integer>))
                 (socket-shutdown (socket-fd s) how))

  (define-generic (peeraddr s))

  (define-method (peeraddr (s <socket>))
                 (socket-peeraddr (socket-fd s)))

  (define-generic (peerport s))

  (define-method (peerport (s <socket>))
                 (socket-peerport (socket-fd s)))

  (define-generic (sockaddr s))

  (define-method (sockaddr (s <socket>))
                 (socket-sockaddr (socket-fd s)))

  (define-generic (sockport s))

  (define-method (sockport (s <socket>))
                 (socket-sockport (socket-fd s)))

  (define-generic (host->ip name))

  (define-method (host->ip (name <string>))
                 (socket-host-to-ip name))

  (define-generic (ip->host ip))

  (define-method (ip->host (ip <string>))
                 (socket-ip-to-host ip))

  (define-generic (converter->port obj))

  (define-method (converter->port (obj <socket>))
                 (socket-convert-to-port (socket-fd obj)))

  ((setter converter) <port> converter->port)

  (define (fd-zero-read)
          (socket-fd-zero-read))

  (define-generic (fd-set-read obj))

  (define-method (fd-set-read (port <port>))
                 (socket-fd-set-read (port-fd port)))

  (define-method (fd-set-read (s <socket>))
                 (socket-fd-set-read (socket-fd s)))

  (define-method (fd-set-read (fd <integer>))
                 (socket-fd-set-read fd))

  (define-generic (fd-isset-read obj))

  (define-method (fd-isset-read (port <port>))
                 (socket-fd-isset-read (port-fd port)))

  (define-method (fd-isset-read (s <socket>))
                 (socket-fd-isset-read (socket-fd s)))

  (define-method (fd-isset-read (fd <integer>))
                 (socket-fd-isset-read fd))

  (define-generic (select-read descriptors timeout))

  (define-method (select-read (descriptors <list>) (timeout <integer>))
                 (fd-zero-read)
                 (map-list fd-set-read descriptors)
                 (if (socket-select-read timeout)
                     (letrec
                       ((loop (lambda (d)
                                (if (null? d)
                                    nil
                                  (if (fd-isset-read (car d))
                                      (cons (car d) (loop (cdr d)))
                                    (loop (cdr d)))))))
                       (loop descriptors))
                   nil))

  (define (fd-zero-write)
          (socket-fd-zero-write))

  (define-generic (fd-set-write obj))

  (define-method (fd-set-write (port <port>))
                 (socket-fd-set-write (port-fd port)))

  (define-method (fd-set-write (s <socket>))
                 (socket-fd-set-write (socket-fd s)))

  (define-method (fd-set-write (fd <integer>))
                 (socket-fd-set-write fd))

  (define-generic (fd-isset-write obj))

  (define-method (fd-isset-write (port <port>))
                 (socket-fd-isset-write (port-fd port)))

  (define-method (fd-isset-write (s <socket>))
                 (socket-fd-isset-write (socket-fd s)))

  (define-method (fd-isset-write (fd <integer>))
                 (socket-fd-isset-write fd))

  (define-generic (select-write descriptors timeout))

  (define-method (select-write (descriptors <list>) (timeout <integer>))
                 (fd-zero-write)
                 (map-list fd-set-write descriptors)
                 (if (socket-select-write timeout)
                     (letrec
                       ((loop (lambda (d)
                                (if (null? d)
                                    nil
                                  (if (fd-isset-write (car d))
                                      (cons (car d) (loop (cdr d)))
                                    (loop (cdr d)))))))
                       (loop descriptors))
                   nil))

  (define-generic (send-int port i))

  (define-method (send-int (port <port>) (i <integer>))
                 (port-xdr-send-int port i))

  (define-generic (recv-int port))

  (define-method (recv-int (port <port>))
                 (port-xdr-recv-int port))

  (define-generic (send-float port f))

  (define-method (send-float (port <port>) (f <float>))
                 (port-xdr-send-float port f))

  (define-generic (recv-float port))

  (define-method (recv-float (port <port>))
                 (port-xdr-recv-float port))

  (define-generic (send-string port s))

  (define-method (send-string (port <port>) (s <string>))
                 (port-xdr-send-string port s))

  (define-generic (recv-string port))

  (define-method (recv-string (port <port>))
                 (port-xdr-recv-string port))

  (define-method (binary= (port <port>) (fd <integer>))
                 (= (port-fd port) fd))

  (define-method (binary= (fd <integer>) (port <port>))
                 (= fd (port-fd port)))

  (define-method (binary= (s <socket>) (fd <integer>))
                 (= (socket-fd s) fd))

  (define-method (binary= (fd <integer>) (s <socket>))
                 (= fd (socket-fd s)))

  (define-method (binary= (port <port>) (s <socket>))
                 (= (port-fd port) (socket-fd s)))

  (define-method (binary= (s <socket>) (port <port>))
                 (= (socket-fd s) (port-fd port)))

  )
