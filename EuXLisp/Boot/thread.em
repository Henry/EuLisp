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
;;;  Title: EuLisp Level-0 thread module
;;;-----------------------------------------------------------------------------

(defmodule thread
  (import (root
           telos
           telosint
           condition
           macros)
   export (<thread>
           <simple-thread>
           make-thread
           thread?
           thread-reschedule
           current-thread
           thread-kill
           thread-queue
           current-thread
           thread-start
           thread-value
           thread-state
           <thread-condition>
           <thread-error>
           <thread-general-error>
           <thread-already-started>
           <lock>
           <simple-lock>
           make-lock
           lock?
           lock
           unlock
           <lock-condition>
           <lock-error>
           wait
           <wait-condition>
           <wait-error>
           let/cc
           with-handler
           unwind-protect
           <wrong-condition-class>
           signal
           error
           cerror))

(define (no-thread op . args)
        no-state:)

;; cf xsint.c, current_continuation and restore_continuation
;; for index of state
(defclass <thread> ()

  ((self keyword: self:
         reader: get-self
         writer: set-self
         default: no-thread)
   (cont keyword: cont:
         reader: get-cont
         writer: set-cont)
   (locks reader: get-locks
          writer: set-locks
          default: ())
   (state reader: get-state           ; handlers and uwps
          writer: set-state
          default: (cons () ()))
   (signals reader: get-signals
            writer: set-signals
            default: ()))

  ;    constructor: (%make-thread self: cont:)
  predicate: thread?
  abstract?: t)

(defclass <simple-thread> <thread> ())

;; avoid calling make, which calls mkthread
(define (%make-thread self cont)
        (initialize (allocate <simple-thread> ()) (list self: self cont: cont)))

;; (make <thread> function: foo)
(define (mkthread inits)
        (let ((initfn (find-key function: inits ())))
          (if (null? initfn)
              (raise-telos-error
               "missing keyword function: in make <simple-thread>"
               inits))
          (make-thread initfn)))

(table-set! builtin-make-table <simple-thread> mkthread)

(define (get-handlers thr)
        (car (get-state thr)))

(define (set-handlers thr val)
        (set-state thr (cons val (cdr (get-state thr))))) ; not rplaca

(define (get-uwps thr)
        (cdr (get-state thr)))

(define (set-uwps thr val)
        (set-state thr (cons (car (get-state thr)) val))) ; not rplacd

(define-method (generic-write (thr <thread>) stream)
               (let ((self (get-self thr))
                     (locks (list-size (get-locks thr)))
                     (signals (get-signals thr)))
                 (%display "#<" stream)
                 (display-class-name thr stream)
                 (%display " " stream)
                 (generic-write self stream)
                 (%display " " stream)
                 (generic-write (self state:) stream)
                 (if (> locks 0)
                     (progn
                       (%display " " stream)
                       (generic-print locks stream)
                       (%display " lock" stream)
                       (if (> locks 1) (%display "s" stream))))
                 (if (not (null? signals))
                     (%display " signals pending" stream))
                 (%display ">" stream)
                 thr))

(define-method (generic-print (thr <thread>) stream)
               (generic-write thr stream))

(define (make-uwp-frame cont)
        (cons cont ()))

(define (get-uwp-frame-cc uwpf) (car uwpf))
(define (set-uwp-frame-cc uwpf val) (set-car! uwpf val))

(define (get-uwp-frame-uwps uwpf) (cdr uwpf))
(define (set-uwp-frame-uwps uwpf val) (set-cdr! uwpf val))

(defcondition <thread-condition> <condition>
  ()
  abstract?: t)

(defcondition <thread-error> <thread-condition>
  ((value reader: thread-error-value
          keyword: value:))
  abstract?: t)

(defcondition <thread-general-error> <thread-error> ())
(defcondition <thread-already-started> <thread-error> ())

(define (nconc a b)
        (if (null? a)
            b
          (progn
            (set-cdr! (last-pair a) b)
            a)))

;; delete first occurence
(define (delq x l)
        (cond ((null? l) ())
              ((eq x (car l)) (cdr l))
              (else (cons (car l) (delq x (cdr l))))))

(deflocal current-self ())

(deflocal r-t-r-q ())

(define (queue-empty?) (null? r-t-r-q))

;;(define (add-thread-to-queue thread)
;;        (setq r-t-r-q (nconc r-t-r-q (list thread))))

;; generate a little non-determinism by random insertion into queue
(define (add-thread-to-queue thread)
        (cond ((null? r-t-r-q)
               (setq r-t-r-q (list thread)))
              ((= (random 2) 0)             ; often put at end
               (setq r-t-r-q (nconc r-t-r-q (list thread))))
              (else
               (insert-thread-in-queue      ; not at start of queue
                r-t-r-q (cdr r-t-r-q) thread (random (list-size r-t-r-q))))))

(define (insert-thread-in-queue before after thread n)
        (if (= n 0)
            (set-cdr! before (cons thread after))
          (insert-thread-in-queue (cdr before) (cdr after) thread (- n 1))))

(define (remove-thread-from-queue thread)
        (setq r-t-r-q (delq thread r-t-r-q)))

(define (pop-thread-from-queue)
        (if (queue-empty?)
            (no-more-threads)
          (let ((thread (car r-t-r-q)))
            (setq r-t-r-q (cdr r-t-r-q))
            (setq current-self thread)
            thread)))

(define (pop-and-call-thread)
        (let* ((new (pop-thread-from-queue))
               (signals (get-signals new)))
          (if (null? signals)
              ((get-cont new) t)
            (progn
              (set-signals new ())
              (for-each                    ; deliver waiting signals
               (lambda (s)
                 (call/cc                  ; (let/cc here (signal ...))
                  (lambda (here)
                    (push-uwp-frame here)
                    (pop-uwp-frame
                     (signal
                      (car s)
                      (cond ((condition? (cdr s)) (cdr s))
                            ((null? (cdr s)) ())
                            (t (unwrap-cc here))))))))
               signals)
              ((get-cont new) t)))))

(deflocal die-when-no-more-threads ())

(define (no-more-threads)
        (newline)
        (if die-when-no-more-threads
            (progn
              (prin "No more runnable threads. Bye...")
              (newline)
              (exit))
          (progn
            (%display "No more runnable threads. Restarting...")
            (newline)
            (set-cont current-self ())     ; tidy up
            (set-locks current-self ())
            (set-state current-self (list () (list ())))
            (set-signals current-self ())
            ((get-self current-self) reset-state:)
            (reset))))

;;   (define (no-more-threads)
;;           (newline)
;;           (prin "No more runnable threads. Bye...")
;;           (newline)
;;           (exit))

;;   (define (no-more-threads)
;;           (newline)
;;           (%display "No more runnable threads. Restarting...")
;;           (newline)
;;           (set-cont current-self ())          ; tidy up
;;           (set-locks current-self ())
;;           (set-state current-self (list () (list ())))
;;           (set-signals current-self ())
;;           ((get-self current-self) reset-state:)
;;           (reset))

;; capture and save current continuation, pass to a continuation in the
;; queue
;; this is expensive as call/cc uses a lot of memory
(define (thread-reschedule)
        (if (queue-empty?)
            t
          (call/cc
           (lambda (cc)
             (set-cont current-self cc)
             (add-thread-to-queue current-self)
             (pop-and-call-thread)))))

(define (make-thread fun)
        (let ((thread (%make-thread (%%make-thread fun) ())))
          (set-uwps thread (list (make-uwp-frame ())))
          thread))

(define (%%make-thread fun)
        (let ((ready? ())
              (started? ())
              (killed? ())
              (value ()))
          (letrec ((self
                    (lambda (op . args)
                      (cond ((eq op ready?:) ready?)
                            ((eq op value:)
                             (cond (ready? value)
                                   ((not started?)
                                    (error <thread-general-error>
                                           "thread-value on unstarted thread"
                                           value: self))
                                   (killed?
                                    (error <thread-general-error>
                                           "thread-value on killed thread"
                                           value: self))
                                   ((eq self (get-self (current-thread)))
                                    (error <thread-general-error>
                                           "thread-value on self"
                                           value: self))
                                   (else
                                    (thread-reschedule)
                                    (self value:))))
                            ((eq op start:)
                             (cond (killed?
                                    (error <thread-general-error>
                                           "thread start on killed thread"
                                           value: self))
                                   (started?
                                    (error <thread-already-started>
                                           "attempt to start running thread"
                                           value: self))
                                   (else
                                    (call/cc
                                     (lambda (cc)
                                       (set-cont (car args) cc)
                                       (add-thread-to-queue (car args))))
                                    (if started?
                                        (progn
                                          (setq value (apply fun (car (cdr args))))
                                          (release-locks (car args))
                                          (setq ready? t)
                                          (pop-and-call-thread))
                                      (setq started? t))
                                    self)))
                            ((eq op kill:)
                             (setq killed? t)
                             (release-locks (car args)))
                            ((eq op state:)
                             (cond (killed? dead:)
                                   (ready? ready:)
                                   (started? started:)
                                   (else limbo:)))
                            ((eq op reset-state:)
                             (setq ready? ())
                             (setq started? t)
                             (setq killed? ())
                             (setq value ()))
                            (else (error <thread-general-error>
                                         "unknown thread operation"
                                         value: op))))))
                  self)))

(define (thread-kill thread)
        (if (thread? thread)
            (progn
              ((get-self thread) kill: thread)
              (remove-thread-from-queue thread)
              (if (eq thread current-self)
                  (pop-and-call-thread))
              t)
          (error <thread-general-error>
                 "not a thread in thread-kill"
                 value: thread)))

(define (thread-queue) r-t-r-q)

(define (clear-queue) (setq r-t-r-q ()))

(define (current-thread) current-self)

(deflocal toplevel-thread
  (let ((killed? ()))
    (define (interactive-thread op . args)
            (cond ((eq op ready?:) ())
                  ((eq op kill:)
                   (setq killed? t)
                   (release-locks (car args)))
                  ((eq op state:)
                   (if killed? dead: started:))
                  ((eq op value:)
                   (cond (killed?
                          (error <thread-general-error>
                                 "thread-value on killed thread"
                                 value: interactive-thread))
                         ((eq toplevel-thread (current-thread))
                          (error <thread-general-error>
                                 "thread-value on self"
                                 value: interactive-thread))
                         (else
                          (thread-reschedule)
                          (interactive-thread value:))))
                  ((eq op start:)
                   (error <thread-already-started>
                          "attempt to start running thread"
                          value: interactive-thread))
                  ((eq op reset-state:)
                   (setq killed? ()))
                  (else (error <thread-general-error>
                               "unknown thread operation"
                               value: op))))
    (let ((thread (%make-thread interactive-thread ())))
      (set-uwps thread (list (make-uwp-frame ())))
      thread)))

(setq current-self toplevel-thread)

(define (thread-start thread . args)
        (if (thread? thread)
            (progn
              ((get-self thread) start: thread args)
              thread)
          (error <thread-general-error>
                 "not a thread in thread-start"
                 value: thread)))

(define (thread-value thread)
        (if (thread? thread)
            ((get-self thread) value:)
          (error <thread-general-error>
                 "not a thread in thread-value"
                 value: thread)))

(define (thread-state thread)
        (if (thread? thread)
            ((get-self thread) state:)
          (error <thread-general-error>
                 "not a thread in thread-state"
                 value: thread)))

;; locks

(defclass <lock> ()

  ((owner reader: lock-owner
          writer: set-lock-owner!
          default: ())
   (queue reader: lock-queue
          writer: set-lock-queue!
          default: ())
   (value reader: lock-value
          writer: set-lock-value!
          default: 1))

  predicate: lock?
  abstract?: t)

(defclass <simple-lock> <lock>
  ()
  constructor: (make-lock))

(define-method (generic-write (l <lock>) stream)
               (%display "#<" stream)
               (display-class-name l stream)
               (%display " " stream)
               (generic-write (lock-queue l) stream)
               (%display " value " stream)
               (generic-write (lock-value l) stream)
               (if (= (lock-value l) 0)
                   (progn
                     (%display " owner " stream)
                     (generic-write (lock-owner l) stream)))
               (%display ">" stream)
               l)

(defcondition <lock-condition> <condition>
  ()
  abstract?: t)

(defcondition <lock-error> <lock-condition>
              ((value default: "no-value")))

(define (add-thread-to-lock-queue lock thread)
        (set-lock-queue!
         lock
         (nconc (lock-queue lock) (list thread))))

(define (thread-reschedule-lock lock)
        (call/cc
         (lambda (cc)
           (set-cont current-self cc)
           (add-thread-to-lock-queue lock current-self)
           (pop-and-call-thread))))

(define (lockit l)
        (if (= (lock-value l) 1)
            (let ((current (current-thread)))
              (set-lock-value! l 0)
              (set-lock-owner! l current)
              (set-locks current (cons l (get-locks current))))
          (thread-reschedule-lock l)))

(define (unlockit l)
        (let ((owner (lock-owner l)))
          (if owner (set-locks owner (delq l (get-locks owner))))
          (if (cons? (lock-queue l))
              (let ((thread (car (lock-queue l)))) ; someone waiting
                (set-lock-queue! l (cdr (lock-queue l)))
                (set-lock-owner! l thread)
                (set-locks thread (cons l (get-locks thread)))
                (if (eq (thread-state thread) dead:)       ; died wile waiting
                    (unlockit l)
                  (add-thread-to-queue thread)))
            (progn                          ; no-one waiting
              (set-lock-owner! l ())
              (set-lock-value! l 1)))))

(define (release-locks thread)
        (let ((locks (get-locks thread)))
          (if locks
              (let ((len (list-size locks)))
                (for-each unlockit (get-locks thread))
                (%display ";; warning: thread finished holding ")
                (%display len)
                (%display " lock")
                (if (> len 1) (%display "s"))
                (%display " -- released")
                (newline)))))

(define (lock l)
        (if (lock? l)
            (lockit l)
          (error <lock-error>
                 "not a lock in LOCK"
                 value: l))
        l)

(define (unlock l)
        (if (lock? l)
            (unlockit l)
          (error <lock-error>
                 "not a lock in LOCK"
                 value: l))
        l)

;; waiting

(defcondition <wait-condition> <condition>
  ()
  abstract?: t)

(defcondition <wait-error> <wait-condition>
              ((value default: "no-value")))

;; ()       poll
;; t       suspend until ready
;; time     wait
(define-method (wait (thread <thread>) (time <object>))
               (cond ((null? time)                 ; poll
                      (if (eq (thread-state thread) ready:)
                          thread
                        ()))
                     ((eq time t)                ; suspend until ready
                      (thread-value thread)
                      thread)
                     ((and (number? time)
                           (>= time 0))
                      (thread-timeout thread time))
                     (t (error <wait-error>
                               "not a valid timeout in wait"
                               value: time))))

(define (thread-timeout thread time)
        (let ((state (thread-state thread))
              (interval (round time)))
          (cond ((eq state ready:) thread)
                ((not (eq state started:))
                 (error <wait-error>
                        "waiting on non-running thread"
                        value: thread))
                (t (timeout-loop (+ (vector-ref (cpu-time) 0) interval)
                                 (lambda () (eq (thread-state thread) ready:)))
                   (if (eq (thread-state thread) ready:)
                       thread
                     ())))))

(define (timeout-loop timeout test)
        (if (and (< (vector-ref (cpu-time) 0) timeout)
                 (not (test)))
            (progn
              (thread-reschedule)
              (timeout-loop timeout test))))

(define-method (wait (str <stream>) (time <object>))
               (cond ((null? time) (char-ready? str))
                     ((eq time t) (stream-suspend str))
                     (t (timeout-loop (+ (vector-ref (cpu-time) 0) (round time))
                                      (lambda () (char-ready? str)))
                        (char-ready? str))))

(define (stream-suspend str)
        (if (char-ready? str)
            t
          (progn
            (thread-reschedule)
            (stream-suspend str))))

;; error handlers

(define (establish-uwp cleanups)
        (dprint (list "establish uwp" cleanups))
        (let* ((thread (current-thread))
               (frame (car (get-uwps thread))))
          (set-uwp-frame-uwps
           frame
           (cons cleanups (get-uwp-frame-uwps frame)))))

(define (disestablish-uwp value cleanups)
        (dprint (list "disestablish uwp" value cleanups))
        (let* ((thread (current-thread))
               (frame (car (get-uwps thread)))
               (uwps (get-uwp-frame-uwps frame)))
          (if (null? uwps)
              (print "*** no uwp to disestablish")
            (if (not (eq (car uwps) cleanups))
                (print "*** out of sync in uwps")))
          (set-uwp-frame-uwps frame (cdr uwps))
          ((car uwps))                      ; run after forms
          value))

(define (establish-handler fun)
        (dprint (list "establish handler" fun))
        (let ((thread (current-thread)))
          (set-handlers thread (cons fun (get-handlers thread)))))

(define (disestablish-handler value handler)
        (dprint (list "disestablish handler" value handler))
        (let* ((thread (current-thread))
               (handlers (get-handlers thread)))
          (if (or (null? handlers) (not (eq handler (car handlers))))
              (print "*** out of sync in handlers")
            (set-handlers thread (cdr handlers))))
        value)

(defcondition <wrong-condition-class> <thread-error> ())

(define (signal condition resume . thread)
        (let ((current (current-thread)))
          (if (or (null? thread) (eq (car thread) current))
              (current-thread-signal condition resume current)
            (if (not (subclass? (class-of condition) <thread-condition>))
                (error  <wrong-condition-class>
                        "must be a subclass of <thread-condition> in signal"
                       value: condition)
              (other-thread-signal condition resume (car thread))))))

(define (current-thread-signal condition resume current)
        (if (null? (get-handlers current))
            (progn
              (default-handler condition resume)
              (print "*** somehow returned from default handler")
              (if (not (eq current toplevel-thread))
                  (thread-kill current)))
          (let* ((handlers (get-handlers current))
                 (handler (car handlers)))
            (dprint (list "pop and call handler" handler))
            (set-handlers current (cdr handlers))
            (call-handler handler condition resume current)
            (current-thread-signal condition resume current))))

;;   (call-next-handler) requires hidden args, c.f. defmethod,
;;   namely condition and resume---defhandler?
;;   or the use of dynamic variables

(define (call-handler handler condition resume current)
        (dprint (list "handling" handler condition resume))
        (handler condition
                 (lambda val
                   (if (function? resume)
                       (if (null? val)
                           (resume)
                         (resume (car val)))
                     (error <general-error>
                            "attempt to resume non-continuation"
                            value: resume)))))

(define (other-thread-signal condition resume thread)
        (let ((signals (get-signals thread)))
          (if (null? signals)
              (set-signals thread (list (cons condition resume)))
            (set-cdr! (last-pair signals)
                      (list (cons condition resume)))))
        t)

(define (unwrap-cc cc)
        (lambda z
          (unwrap-uwps cc)
          (cond ((null? z) (cc))
                ((null? (cdr z)) (cc (car z)))
                (t (error <general-error>
                          "too many args passed to continuation"
                          value: z)))))

(defmacro let/cc (name . body)
  `(call/cc (lambda (cc)
              (push-uwp-frame cc)
              (pop-uwp-frame
               (let ((,name (unwrap-cc cc)))
                 ,@body)))))

(define (push-uwp-frame cc)
        (dprint (list "push uwp frame" cc))
        (let ((thread (current-thread)))
          (set-uwps thread (cons (make-uwp-frame cc)
                                 (get-uwps thread)))))

(define (pop-uwp-frame result)
        (dprint (list "pop uwp frame"))
        (let ((thread (current-thread)))
          (set-uwps thread (cdr (get-uwps thread))))
        result)

(define (unwrap-uwps cc)
        (dprint (list "unwrap uwps" cc))
        (let* ((thread (current-thread))
               (frames (get-uwps thread))
               (frame (car frames))
               (cont (get-uwp-frame-cc frame)))
          (if (null? (cdr frames))          ; toplevel, keep last frame
              (exec-uwps frame)
            (progn
              (set-uwps thread (cdr frames))
              (exec-uwps frame)
              (if (eq cc cont)
                  ()                       ; done
                (unwrap-uwps cc))))))

(define (unwrap-and-reset)
        (dprint (list "unwrap and reset"))
        (unwrap-uwps ())
        (if (eq (current-thread) toplevel-thread)
            (reset)
          (thread-kill (current-thread))))

(define (exec-uwps frame)
        (let ((uwps (get-uwp-frame-uwps frame)))
          (if (null? uwps)
              ()
            (progn
              (set-uwp-frame-uwps frame (cdr uwps))
              ((car uwps))
              (exec-uwps frame)))))

;; don't uwp disestablish, as any non-local exit will reset state itself
(defmacro with-handler (fun . body)
  `(let ((handler ,fun))
     (establish-handler handler)
     (disestablish-handler (progn ,@body) handler)))

(defmacro unwind-protect (protected . afterforms)
  `(let ((cleanups (lambda () ,@afterforms)))
     (establish-uwp cleanups)
     (disestablish-uwp ,protected cleanups)))

(define (error condclass message . opts)
        (signal (apply make condclass message: message opts) ()))

;;   (define (cerror condclass message . opts)
;;           (let/cc resume
;;             (signal (apply make condclass message: message opts) resume)))

(define (cerror condclass message . opts)
        (call/cc (lambda (cc)
                   (push-uwp-frame cc)
                   (pop-uwp-frame
                    (signal
                     (apply make condclass message: message opts)
                     (unwrap-cc cc))))))

;; debugging support

(define (debug cc condition)
        (print "\nDebug loop.  Type help: for help")
        (frame-where *xlframe* cc condition)
        (setq current-print-depth 0)
        (if (null? cc)
            (let ((cleanups (lambda () (inc-depth -1))))
              (inc-depth 1)
              (establish-uwp cleanups)
              (disestablish-uwp
               (debug-loop *xlframe* cc condition)
               cleanups))
          (progn
            (push-uwp-frame cc)
            (pop-uwp-frame
             (let ((cleanups (lambda () (inc-depth -1))))
               (inc-depth 1)
               (establish-uwp cleanups)
               (disestablish-uwp
                (debug-loop *xlframe* (unwrap-cc cc) condition)
                cleanups))))))

(deflocal *debug-depth* 0)
(deflocal *debug-rl* ())

(define (inc-depth inc)
        (setq *debug-depth* (+ *debug-depth* inc)))

(define (debug-loop frameptr cc condition)
        (if (not *debug-rl*) ;; If readline is not used print prompt
            (prin "[error" *debug-depth* "] " (current-module) ">"))
        (let ((op (read)))
          (if (eq op **EOF**)
              (if (null? cc)
                  (unwrap-and-reset)
                (cc)))
          (debug-loop
           (cond ((keyword? op)
                  (debug-op frameptr cc condition op ()))
                 ((and (cons? op) (keyword? (car op)))
                  (debug-op frameptr cc condition (car op) (cdr op)))
                 (t (write ((compile op (frame-env frameptr))))
                    (newline)
                    frameptr))
           cc condition)))

(define (debug-op frameptr cc cd op args)
        (let ((fn (table-ref op-table op)))
          (if (null? fn)
              (progn
                (print op)
                frameptr)
            (apply fn frameptr cc cd args))))

(deflocal op-table (make-table eq))

(define (help frameptr cc cd . args)
        (print "Debug loop.")
        (print "top:                                return to top level")
        (print "resume:  or  (resume: val)          resume from error")
        (print "bt:                                 backtrace")
        (print "locals:                             local variables")
        (print "cond:                               current condition")
        (print "up:  or  (up: n)                    up one or n frames")
        (print "down:  or  (down: n)                down one or n frames")
        (print "where:                              current function")
        (newline)
        frameptr)

(table-set! op-table help: help)

(define (debug-return frameptr cc cd . args)
        (unwrap-and-reset))

(table-set! op-table top: debug-return)

(define (resume frameptr cc cd . args)
        (if (null? cc)
            (error <general-error>
                   "attempt to resume from a non-continuable error"
                   value: cc))
        (if (null? args)
            (cc)
          (cc (car args))))

(table-set! op-table resume: resume)

(define (debug-backtrace frameptr cc cd . args)
        (backtrace frameptr)
        frameptr)

(table-set! op-table bt: debug-backtrace)

(define (locals frameptr cc cd . args)
        (letrec ((loop (lambda (e)
                         (locals-loop (vector-ref (%CAR e) 0) (%CAR e) 1)
                         (if (%CDR e) (loop (%CDR e))))))
                (loop (frame-env frameptr)))
        (newline)
        frameptr)

(define (locals-loop syms vals index)
        (if (cons? syms)
            (progn
              (prin (car syms))
              (prin ": ")
              (indent (string-size (symbol->string (car syms))))
              (print (vector-ref vals index))
              (locals-loop (cdr syms) vals (+ index 1)))))

(define (indent n)
        (if (< n 15)
            (progn
              (prin " ")
              (indent (+ n 1)))))

(table-set! op-table locals: locals)

(table-set! op-table up: frame-up)

(table-set! op-table down: frame-down)

(define (frame-where frameptr cc cd . args)
        (prin "Broken at ")
        (print (frame-fun frameptr))
        (newline)
        frameptr)

(table-set! op-table where: frame-where)

(define (frame-cond frameptr cc cd . args)
        (print cd)
        (newline)
        frameptr)

(table-set! op-table cond: frame-cond)

;;;-----------------------------------------------------------------------------
)  ;; End of module thread
;;;-----------------------------------------------------------------------------
