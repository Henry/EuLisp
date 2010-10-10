;;; Copyright 1994 Russell Bradford
;;; Copyright 2010 Henry G. Weller
;;;-----------------------------------------------------------------------------
;;  This file is part of
;;; ---                           EuLisp System 'EuXLisp'
;;;-----------------------------------------------------------------------------
;;
;;  EuXLisp is free software: you can redistribute it and/or modify it under the
;;  terms of the GNU General Public License version 2 as published by the Free
;;  Software Foundation.
;;
;;  EuXLisp is distributed in the hope that it will be useful, but WITHOUT ANY
;;  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;  details.
;;
;;  You should have received a copy of the GNU General Public License along with
;;  this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;-----------------------------------------------------------------------------
;;; Title: Socket interface for EuXLisp
;;;  Authors: David Halls, Russell Bradford
;;;  Maintainer: Henry G. Weller
;;;  Description:
;;    to use this the interpreter must have been compiled with -DSOCK
;;;-----------------------------------------------------------------------------

(defmodule socket
  (import (root
           setter
           convert)
   export (<socket>
           socket-fd
           socket?
           make-socket
           connect bind
           listen
           accept
           set-block
           set-nonblock
           reuse
           noreuse
           close-socket
           shutdown
           peeraddr
           peerstream
           sockaddr
           sockstream
           host->ip
           ip->host
           fd-zero-read
           fd-set-read
           fd-isset-read
           select-read
           fd-zero-write
           fd-set-write
           fd-isset-write
           select-write
           send-int
           recv-int
           send-float
           recv-float
           send-string
           recv-string
           stream-fd
           stream-unbuffered
           stream-block-buffered
           stream-line-buffered))

(defclass <socket> ()
  ((fd keyword: fd:
       accessor: socket-fd
       default: nil))
  constructor: (socket-constructor fd:)
  predicate: socket?)

(define (make-socket)
        (socket-constructor (socket-socket)))

(define-generic (connect s name stream))

(define-method (connect (s <socket>) (name <string>) (stream <integer>))
               (socket-connect (socket-fd s) name stream))

(define-generic (bind s stream))

(define-method (bind (s <socket>) (stream <integer>))
               (socket-bind (socket-fd s) stream))

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

(define-generic (peerstream s))

(define-method (peerstream (s <socket>))
               (socket-peerstream (socket-fd s)))

(define-generic (sockaddr s))

(define-method (sockaddr (s <socket>))
               (socket-sockaddr (socket-fd s)))

(define-generic (sockstream s))

(define-method (sockstream (s <socket>))
               (socket-sockstream (socket-fd s)))

(define-generic (host->ip name))

(define-method (host->ip (name <string>))
               (socket-host-to-ip name))

(define-generic (ip->host ip))

(define-method (ip->host (ip <string>))
               (socket-ip-to-host ip))

(define-generic (converter->stream obj))

(define-method (converter->stream (obj <socket>))
               (socket-convert-to-stream (socket-fd obj)))

((setter converter) <stream> converter->stream)

(define (fd-zero-read)
        (socket-fd-zero-read))

(define-generic (fd-set-read obj))

(define-method (fd-set-read (stream <stream>))
               (socket-fd-set-read (stream-fd stream)))

(define-method (fd-set-read (s <socket>))
               (socket-fd-set-read (socket-fd s)))

(define-method (fd-set-read (fd <integer>))
               (socket-fd-set-read fd))

(define-generic (fd-isset-read obj))

(define-method (fd-isset-read (stream <stream>))
               (socket-fd-isset-read (stream-fd stream)))

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

(define-method (fd-set-write (stream <stream>))
               (socket-fd-set-write (stream-fd stream)))

(define-method (fd-set-write (s <socket>))
               (socket-fd-set-write (socket-fd s)))

(define-method (fd-set-write (fd <integer>))
               (socket-fd-set-write fd))

(define-generic (fd-isset-write obj))

(define-method (fd-isset-write (stream <stream>))
               (socket-fd-isset-write (stream-fd stream)))

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

(define-generic (send-int stream i))

(define-method (send-int (stream <stream>) (i <integer>))
               (stream-xdr-send-int stream i))

(define-generic (recv-int stream))

(define-method (recv-int (stream <stream>))
               (stream-xdr-recv-int stream))

(define-generic (send-float stream f))

(define-method (send-float (stream <stream>) (f <float>))
               (stream-xdr-send-float stream f))

(define-generic (recv-float stream))

(define-method (recv-float (stream <stream>))
               (stream-xdr-recv-float stream))

(define-generic (send-string stream s))

(define-method (send-string (stream <stream>) (s <string>))
               (stream-xdr-send-string stream s))

(define-generic (recv-string stream))

(define-method (recv-string (stream <stream>))
               (stream-xdr-recv-string stream))

(define-method (binary= (stream <stream>) (fd <integer>))
               (= (stream-fd stream) fd))

(define-method (binary= (fd <integer>) (stream <stream>))
               (= fd (stream-fd stream)))

(define-method (binary= (s <socket>) (fd <integer>))
               (= (socket-fd s) fd))

(define-method (binary= (fd <integer>) (s <socket>))
               (= fd (socket-fd s)))

(define-method (binary= (stream <stream>) (s <socket>))
               (= (stream-fd stream) (socket-fd s)))

(define-method (binary= (s <socket>) (stream <stream>))
               (= (socket-fd s) (stream-fd stream)))

;;;-----------------------------------------------------------------------------
)  ;; End of module socket
;;;-----------------------------------------------------------------------------
