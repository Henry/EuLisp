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
;;; Title: Object streams
;;;  Library: serial
;;;  Authors: Andreas Kind
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule serial
  (syntax (syntax-0)
   import (level-0)
   export (<object-stream>
           object-stream-cache
           object-stream-cache-index
           *serial-standard-modules*
           serialize
           deserialize
           reset
           ;; used in eul-serial.c
           eul-serial-read-bytes
           eul-serial-error
           eul-serial-make-class
           eul-serial-make-state
           eul-serial-make-instance
           eul-serial-allocate-instance
           eul-serial-initialize-instance
           eul-serial-make-file-stream))

;;;-----------------------------------------------------------------------------
;;; Define standard EuLisp modules
;;; This should reflect full-import in liblevel-0.i as default
;;;-----------------------------------------------------------------------------
(deflocal *serial-standard-modules*
  #(level-0
    telos
    boot1
    boot
    mop-defcl
    mop-meth
    mop-gf
    mop-inspect
    mop-init
    mop-class
    mop-key
    mop-prim
    mop-access
    mop-alloc
    bit condition
    event
    thread
    dynamic
    let-cc
    callback
    string
    convert
    copy
    integer
    number
    fpi
    collect
    compare
    character
    float
    stream3
    vector
    stream
    stream1
    lock
    stream2
    socket
    list
    format
    convert1
    table1
    table
    handler
    random
    symbol
    read))

(defun serialize (x . ss)
  (let ((s (if ss (car ss) stdout))
        (os (make <object-stream> mode: 'w)))
    (connect os s)
    (swrite os x)
    (disconnect os)))

(defun deserialize ss
  (let ((s (if ss (car ss) stdin))
        (os (make <object-stream> mode: 'r)))
    (connect os s)
    (let ((res (read os)))
      (disconnect os)
      res)))

;;;------------------------------------------------------------------------
;;; Object stream class
;;;------------------------------------------------------------------------
(defclass <object-stream> <stream>
  ((cache accessor: object-stream-cache default: #(() ()))
   (cache-index accessor: object-stream-cache-index default: 0))
  predicate: object-stream?)

(defprimclass <bytevector> byte-vector-class (<string>)
              ())

(defmethod generic-print ((x <bytevector>) (s <stream>))
  (sformat s "#<bytevector: ~a>" (string-size x)))

(defmethod generic-write ((x <bytevector>) (s <stream>))
  (sformat s "#<bytevector: ~a>" (string-size x)))

;;;-----------------------------------------------------------------------------
;;; Reset
;;;-----------------------------------------------------------------------------
(defmethod reset ((x <object-stream>))
  (let ((sink (stream-sink x)))
    (reset (object-stream-cache x))
    ((setter object-stream-cache-index) x 0)
    (generic-print TC_RESET sink)
    x))

;;;------------------------------------------------------------------------
;;; Tags and constants
;;;------------------------------------------------------------------------
(defconstant *serial-constants* (eul_serial_initialize))
(defextern eul_serial_initialize () ptr)
(defconstant STREAM_MAGIC (vector-ref *serial-constants* 0))
(defconstant STREAM_VERSION (vector-ref *serial-constants* 1))
(defconstant TC_NULL (vector-ref *serial-constants* 2))
(defconstant TC_REFERENCE (vector-ref *serial-constants* 3))
(defconstant TC_CLASS (vector-ref *serial-constants* 4))
(defconstant TC_OBJECT (vector-ref *serial-constants* 5))
(defconstant TC_STRING (vector-ref *serial-constants* 6))
(defconstant TC_VECTOR (vector-ref *serial-constants* 7))
(defconstant TC_KEYWORD (vector-ref *serial-constants* 8))
(defconstant TC_STREAM (vector-ref *serial-constants* 9))
(defconstant TC_STATE (vector-ref *serial-constants* 10))
(defconstant TC_RESET (vector-ref *serial-constants* 11))
(defconstant TC_SELF (vector-ref *serial-constants* 12))
(defconstant TC_FUNCTION (vector-ref *serial-constants* 14))
(defconstant TC_BYTEVECTOR (vector-ref *serial-constants* 15))
(defconstant TC_INT (vector-ref *serial-constants* 16))
(defconstant TC_DOUBLE (vector-ref *serial-constants* 17))
(defconstant TC_SYMBOL (vector-ref *serial-constants* 18))
(defconstant TC_CHAR (vector-ref *serial-constants* 19))
(defconstant TC_CONS (vector-ref *serial-constants* 20))

;;;------------------------------------------------------------------------
;;; Reading from object streams
;;;------------------------------------------------------------------------
(defmethod generic-connect ((os <object-stream>) (s <stream>) options)
  (call-next-method)
  (let ((mode (stream-mode os)))
    (if (eq mode 'r)
        (let* ((header (eul-serial-read-header os () (eos-default-value)))
               (magic (car header))
               (version (cdr header)))
          (if (and (= magic STREAM_MAGIC)
                   (= version STREAM_VERSION))
              ()
            (sformat stderr "*** WARNING: bad header ~a\n" header))
          (debug-format stderr "magic: ~x version: ~x\n" magic version))
      (let ((sink (stream-sink os))
            (magic-data (eul-serial-short-data STREAM_MAGIC))
            (version-data (eul-serial-short-data STREAM_VERSION)))
        (generic-print magic-data sink)
        (generic-print version-data sink)
        ((setter object-stream-cache) os (make <table>))))
    ()))

(defmethod flush-buffer ((os <object-stream>))
  (if (eq (stream-mode os) 'r) ()
    (flush-buffer (stream-sink os))))

(defmethod generic-read ((os <object-stream>) eos-error? eos-value)
  (eul-serial-read-object os eos-error? eos-value))

(defextern eul-serial-read-header (ptr ptr ptr) ptr "eul_serial_read_header")
(defextern eul-serial-read-object (ptr ptr ptr) ptr "eul_serial_read_object")

(defun eul-serial-error (s str . args)
  (apply error str args))

;;;------------------------------------------------------------------------
;;; Writing to object streams
;;;------------------------------------------------------------------------
(defmethod generic-write ((x <null>) (os <object-stream>))
  (let ((sink (stream-sink os)))
    (generic-print TC_NULL sink)))

(defmethod generic-write ((x <string>) (os <object-stream>))
  (if (prev-object x os) os
    (let ((sink (stream-sink os))
          (data (eul-serial-short-data (string-size x))))
      (generic-print TC_STRING sink)
      (new-handle x os)
      (generic-print data sink)
      (generic-print x sink)
      os)))

(defmethod generic-write ((x <symbol>) (os <object-stream>))
  (let* ((sink (stream-sink os))
         (str (symbol-name x))
         (data (eul-serial-short-data (string-size str))))
    (generic-print TC_SYMBOL sink)
    (generic-print data sink)
    (generic-print str sink)
    os))

(defmethod generic-write ((x <keyword>) (os <object-stream>))
  (let* ((sink (stream-sink os))
         (str (keyword-name x))
         (data (eul-serial-short-data (string-size str))))
    (generic-print TC_KEYWORD sink)
    (generic-print data sink)
    (generic-print str sink)
    os))

(defmethod generic-write ((x <fpi>) (os <object-stream>))
  (let ((sink (stream-sink os))
        (data (eul-serial-int-data x)))
    (generic-print TC_INT sink)
    (generic-print data sink)
    os))

(defmethod generic-write ((x <double>) (os <object-stream>))
  (let ((sink (stream-sink os))
        (data (eul-serial-double-data x)))
    (generic-print TC_DOUBLE sink)
    (generic-print data sink)
    os))

(defmethod generic-write ((x <character>) (os <object-stream>))
  (let ((sink (stream-sink os)))
    (generic-print TC_CHAR sink)
    (generic-print x sink)
    os))

(defmethod generic-write ((x <cons>) (os <object-stream>))
  (if (prev-object x os) os
    (let ((sink (stream-sink os)))
      (generic-print TC_CONS sink)
      (new-handle x os)
      (generic-write (car x) os)
      (generic-write (cdr x) os))))

(defmethod generic-write ((x <vector>) (os <object-stream>))
  (if (prev-object x os) os
    (let ((sink (stream-sink os))
          (data (eul-serial-int-data (vector-size x))))
      (debug-format stderr "vector size: ~a\n" (vector-size x))
      (generic-print TC_VECTOR sink)
      (new-handle x os)
      (generic-print data sink)
      (do (lambda (elem) (generic-write elem os)) x)
      os)))

(defmethod generic-write ((x <simple-class>) (os <object-stream>))
  (if (prev-object x os) os
    (let* ((sink (stream-sink os))
           (supers (class-direct-superclasses x))
           (slot-descrs
            (map (lambda (slot)
                   (let ((name (slot-name slot))
                         (required? (slot-required? slot)))
                     (if required?
                         (list name: name
                               ;; Just following the convention ...
                               ; accessor:
                               ; (concatenate (class-name x) '- name)
                               keyword:
                               (slot-keyword slot)

                               ;; default:
                               ; (slot-default slot)
                               required?:
                               (slot-required? slot))
                       (list name: name
                             ;; Just following the convention ...
                             ; accessor:
                             ; (concatenate (class-name x) '- name)
                             keyword:
                             (slot-keyword slot)

                             ;; default:
                             ; (slot-default slot)
                             ))))
                 (select (lambda (s)
                           (null? (any? (lambda (super)
                                          (member s (class-slots super)))
                                        supers)))
                         (class-slots x)))))
      (generic-print TC_CLASS sink)
      (generic-write (class-name x) os)
      (generic-write supers os)
      (generic-write slot-descrs os)
      (new-handle x os)))
  os)

(defmethod generic-write ((x <object>) (os <object-stream>))
  (if (prev-object x os) os
    (let* ((sink (stream-sink os))
           (cl (class-of x))
           inits)
      (generic-print TC_OBJECT sink)
      (new-handle x os)
      (generic-write cl os)
      (do (lambda (slot)
            (setq inits (cons (slot-keyword slot) inits))
            (setq inits (cons ((slot-reader slot) x) inits)))
          (class-slots cl))
      (generic-write (reverse inits) os)))
  os)

(defmethod generic-write ((x <simple-thread>) (os <object-stream>))
  (if (prev-object x os) os
    (let* ((sink (stream-sink os))
           (cl (class-of x))
           (inits (make <vector> size: 6)))
      (generic-print TC_OBJECT sink)
      (new-handle x os)
      (generic-write cl os)
      ((setter vector-ref) inits 0 (thread-error-handlers x))
      ((setter vector-ref) inits 1 (thread-dynamic-variables x))
      ((setter vector-ref) inits 2 (thread-continuation x))
      ((setter vector-ref) inits 3 (thread-state x))
      ((setter vector-ref) inits 4 (thread-returned? x))
      ((setter vector-ref) inits 5 (thread-return-value x))
      (debug-format stderr "!!!Thread: ~a\n" inits)
      (generic-write inits os)))
  os)

(defmethod generic-write ((x <simple-function>) (os <object-stream>))
  (if (prev-object x os) os
    (let* ((sink (stream-sink os))
           (code (eul-serial-lambda-data x))
           (data (eul-serial-int-data (size code)))
           (refs (reverse (eul-lambda-refs x *serial-standard-modules*))))
      (generic-print TC_FUNCTION sink)
      (generic-write (function-name x) os)
      (generic-write (function-domain x) os)
      (generic-write (setter x) os)
      (generic-write (simple-function-environment x) os)
      (generic-print data sink)
      (generic-print code sink)
      (new-handle x os)
      (generic-write refs os))))

(defmethod generic-write ((x <bytevector>) (os <object-stream>))
  (let ((sink (stream-sink os))
        (data1 (eul-serial-short-data (string-size x)))
        (data2 (eul-serial-bytevector-data x))
        (refs (reverse (eul-bytevector-refs x *serial-standard-modules*))))
    (generic-print TC_BYTEVECTOR sink)
    (generic-print data1 sink)
    (generic-print data2 sink)
    (debug-format stderr "!!!References: ~a\n" refs)
    (generic-write refs os)))

(defmethod generic-write ((x <state>) (os <object-stream>))
  (let ((sink (stream-sink os)))
    (generic-print TC_STATE sink)
    (generic-write (state-value-stack x) os)
    (generic-write (state-value-stack-size x) os)
    (serialize-context-stack (state-context-stack x) os sink 0)
    (generic-write (state-context-stack-size x) os)))

(defun serialize-context-stack (vec os s i)
  (let ((n (vector-size vec)))
    (if (= i 0)
        (let ((data (eul-serial-int-data n))
              (x (vector-ref vec 0)))
          (generic-print TC_VECTOR s)
          (generic-print data s)
          (if (null? x)
              (generic-write () os)
            (serialize-context-stack x os s 0))
          (serialize-context-stack vec os s (+ i 1)))
      (if (< i n)
          (let ((next-meths (vector-ref vec i))
                (pc (vector-ref vec (+ i 1)))
                (env (vector-ref vec (+ i 2)))
                (fun (vector-ref vec (+ i 3))))
            (if (function? fun)
                (setq pc (eul-serial-relative-pc fun pc))
              ())
            ;; Write the frame with relative program counter
            (generic-write next-meths os)
            (generic-write pc os)
            (generic-write env os)
            (generic-write fun os)
            (serialize-context-stack vec os s (+ i 4)))
        ()))))

(defmethod generic-write ((x <object-stream>) (os <object-stream>))
  (if (eq x os)
      (generic-print TC_SELF (stream-sink x))
    (call-next-method)))

(defmethod generic-print ((x <file-stream>) (os <object-stream>))
  (if (prev-object x os) os
    (let* ((sink (stream-sink x))
           (source (stream-source x))
           (mode (stream-mode x))
           (file-name (if (eq mode 'r)
                          (if (file-control-block? source)
                              (control-block-file-name source)
                            ())
                        (if (file-control-block? sink)
                            (control-block-file-name sink)
                          ()))))
      (debug-format stderr "write file stream with mode: ~a file-name: ~a"
                    mode file-name)
      (generic-print TC_STREAM (stream-sink os))
      (generic-write mode os)
      (generic-write file-name os)
      (new-handle x os)
      os)))

(defextern eul-serial-short-data (<fpi>) ptr "eul_serial_short_data")
(defextern eul-serial-int-data (<fpi>) ptr "eul_serial_int_data")
(defextern eul-serial-double-data (ptr) ptr "eul_serial_double_data")
(defextern eul-serial-lambda-data (ptr) ptr "eul_serial_lambda_data")
(defextern eul-lambda-refs (ptr ptr) ptr "eul_lambda_refs")
(defextern eul-serial-bytevector-data (ptr) ptr "eul_serial_bytevector_data")
(defextern eul-bytevector-refs (ptr ptr) ptr "eul_bytevector_refs")
(defextern eul-serial-relative-pc (ptr ptr) <fpi> "eul_serial_relative_pc")

;;;-----------------------------------------------------------------------------
;;; The impossible things ...
;;;-----------------------------------------------------------------------------
(defmethod generic-write ((x <handler>) (os <object-stream>))
  (error <condition>
         (fmt "cannot write ~a to stream ~a" x os)))

(defmethod generic-write ((x <thread>) (os <object-stream>))
  (error <condition>
         (fmt "cannot write ~a to stream ~a" x os)))

;;;------------------------------------------------------------------------
;;; Object caching
;;;------------------------------------------------------------------------
(defun new-handle (x os)
  (debug-format stderr "new-handle: ~a\n" x)
  (let ((tab (object-stream-cache os))
        (i (object-stream-cache-index os)))
    (debug-format stderr ">>> new handle: ~a\n" i)
    (sformat stderr "*** CACHE PUT (write) ~a for ~a\n" i x)
    ((setter object-stream-cache-index) os (+ i 1))
    ((setter table-ref) tab x i)))

(defun prev-object (x os)
  (debug-format stderr "prev-object: ~a\n" x)
  (let* ((tab (object-stream-cache os))
         (i (table-ref tab x)))
    (if (null? i) ()
      (let ((sink (stream-sink os))
            (data (eul-serial-int-data i)))
        (debug-format stderr "<<< old handle: ~a\n" i)
        (sformat stderr "*** CACHE GET (write) ~a for ~a\n" i x)
        (generic-print TC_REFERENCE sink)
        (generic-print data sink)
        os))))

;;;------------------------------------------------------------------------
;;; Creating classes and instances
;;;------------------------------------------------------------------------
(defun eul-serial-make-class (name super slot-descs)
  (debug-format stderr "make-class ~a ~a ~a\n" name super slot-descs)
  (or
   (get-class name)
   (progn
     (debug-format stderr "make <simple-class> ~a ~a ~a\n"
                   name super slot-descs)
     (let ((keys (map (lambda (desc)
                        (init-list-ref desc keyword:))
                      slot-descs)))
       (make <simple-class>
             name: name
             direct-superclasses: (or super (list <object>))
             direct-slots: slot-descs
             direct-keywords: keys)))))

(defun get-class (name)
  (labels
   ((loop (cl)
          (if (eq (class-name cl) name)
              cl
            (any? loop (class-direct-subclasses cl)))))
   (loop <object>)))

(defun eul-serial-make-state (value-stack value-stack-size
                                          context-stack context-stack-size)
  (debug-format stderr "make-state: ~a ~a ~a ~a\n"
                value-stack value-stack-size
                context-stack context-stack-size)
  (make <state>
        value-stack: value-stack
        value-stack-size: value-stack-size
        context-stack: context-stack
        context-stack-size: context-stack-size))

(defun eul-serial-make-file-stream (mode file-name)
  (debug-format stderr "make-file-stream: ~a ~a\n" mode file-name)
  (cond ((binary= file-name "stdin")
         stdin)
        ((binary= file-name "stdout")
         stdout)
        ((binary= file-name "stderr")
         stderr)
        (t
         (make <file-stream> file-name: file-name mode: mode))))

(defun eul-serial-make-instance (cl inits)
  (debug-format stderr "make-instance: ~a ~a\n" cl inits)
  (cond ((eq cl <character>)
         (init-list-ref inits value:))
        ((eq cl <fpi>)
         (init-list-ref inits value:))
        ((eq cl <number>)
         (init-list-ref inits value:))
        ((eq cl <string>)
         (init-list-ref inits value:))
        ((eq cl <object>)
         ())
        (t
         (apply make cl inits))))

(defun eul-serial-allocate-instance (cl)
  (debug-format stderr "allocate-instance: ~a\n" cl)
  (cond ((eq cl <character>)
         ())
        ((eq cl <fpi>)
         ())
        ((eq cl <string>)
         ())
        ((eq cl <symbol>)
         ())
        (t
         (allocate cl ()))))

(defun eul-serial-initialize-instance (x inits)
  (debug-format stderr "initialize-instance: ~a ~a\n" x inits)
  (if (simple-thread? x)
      (let ((handlers (vector-ref inits 0))
            (dyn-vars (vector-ref inits 1))
            (k (vector-ref inits 2))
            (state (vector-ref inits 3))
            (flag (vector-ref inits 4))
            (val (vector-ref inits 5)))
        (debug-format
         stderr "initialize <simple-thread>: ~a ~a ~a ~a ~a ~a\n"
         handlers dyn-vars k state flag val)
        ((setter thread-error-handlers) x handlers)
        ((setter thread-dynamic-variables) x dyn-vars)
        ((setter thread-continuation) x k)
        ((setter thread-state) x state)
        ((setter thread-returned?) x flag)
        ((setter thread-return-value) x val)
        x)
    (initialize x inits)))

;;;------------------------------------------------------------------------
;;; Reading bytes
;;;------------------------------------------------------------------------
(deflocal *position* 0)

(defun eul-serial-read-bytes (os n eos-error? eos-value)
  (let ((s (stream-source os))
        (str (make <string> size: n)))
    (labels
     ((loop (i)
            (if (< i n)
                (let ((c (generic-read s eos-error? eos-value)))
                  (if (eq c eos-value)
                      (end-of-stream s)
                    (progn
                      ((setter string-ref) str i c)
                      (loop (+ i 1)))))
              str)))
     (loop 0)
     (setq *position* (+ *position* n))
     str)))

;;;------------------------------------------------------------------------
;;; For debugging only ...
;;;------------------------------------------------------------------------
(deflocal *debug* ())

(defun debug-format (s str . args)
  (if *debug*
      (apply sformat s str args)
    ()))

;;;-----------------------------------------------------------------------------
;;; To be removed (see also generic-connect!)
;;;-----------------------------------------------------------------------------
;;  (defgeneric select (f c . cs))
;;  (defmethod select ((fun <function>) (c <list>) . cs)
;;    (if (null? cs)
;;      (select-list fun c)
;;      (call-next-method)))
;;  (defun select-list (pred l . args)
;;    (labels
;;     ((loop (ll res)
;;          (if (null? ll)
;;              (reverse-list res)
;;            (let ((x (car ll)))
;;              (if (apply pred x args)
;;                  (loop (cdr ll) (cons x res))
;;                (loop (cdr ll) res))))))
;;     (loop l ())))
;;  (defmethod size (c)
;;    (vector-size c))

(defmethod generic-print ((c <character>) (s <buffered-stream>))
  ;    (if (graph? c)
  (sprint-one-char s c)
  ;      (generic-write c s))
  c)

;;;-----------------------------------------------------------------------------
)  ;; End of module serial
;;;-----------------------------------------------------------------------------
