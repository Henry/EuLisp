;; A simple Linda implementation
;; RJB March 92
;; EuScheme version RJB Nov 94

;; (make-linda-pool)
;; (linda-out pool tag . values)
;; (linda-in pool tag . pattern)
;; (linda-in? pool tag . pattern)
;; (linda-read pool tag . pattern)
;; (linda-read? pool tag . pattern)
;; (linda-eval fun . args)

;; tag is a constant symbol (not matched)

;; linda-in? and linda-read? are non-blocking versions of linda-in
;; and linda-read, returning () if no matching tuple, t otherwise.

;; the pattern (? var) matches anything, and assigns that value to var
;; the pattern ? matches anything, and discards the value
;; tags, and any other patterns are matched literally

;; e.g.
;; (setq pp (make-linda-pool))
;; (linda-out pp 'foo 1 2)
;; (linda-read pp 'foo ? (? x))      setqs x to 2
;; (linda-read? pp 'foo 1 2 3)       returns ()
;; (linda-read pp 'foo 1 2 3)        suspends

(defmodule eulinda
  (import (level0)
   export (<linda-pool> make-linda-pool linda-in linda-read
                        linda-out linda-evallinda-in? linda-read?
                        linda-in-tuple linda-read-tuple
                        linda-in?-tuple linda-read?-tuple
                        linda-tuple-value ?
                        print-linda-pool tril))

(deflocal trace-linda? ())

(defun tril (x) (setq trace-linda? x))

(defclass <linda-pool> ()
  ((lock default: (make-lock)
         accessor: linda-pool-lock)
   (tuple-table default: (make-linda-tuple-table)
                accessor: linda-pool-tuple-table))
  constructor: make-linda-pool
  predicate: pool?)

(defmethod generic-write ((p <linda-pool>) s)
  (sformat s "#<linda-pool: ~s>"
           (linda-pool-tuple-table p))
  p)

(defmethod generic-print ((p <linda-pool>) s)
  (sformat s "#<linda-pool: ~s>"
           (linda-pool-tuple-table p))
  p)

(defun print-linda-pool (pool)
  (format "Linda pool:~%")
  (map
   (lambda (v) (format "~a " v))
   (linda-pool-tuple-table pool))
  (format "~%"))

(defun tidy-pattern (pat)
  (cond ((null? pat) ())
        ((eq (car pat) '?)
         (cons '? (tidy-pattern (cdr pat))))
        ((and (cons? (car pat))
              (eq (caar pat) '?))
         (cons '? (tidy-pattern (cdr pat))))
        (t (cons (car pat) (tidy-pattern (cdr pat))))))

(defun do-setqs-aux (pattern n)
  (cond ((null? pattern) ())
        ((and (cons? (car pattern))
              (eq (caar pattern) '?))
         (cons `(setq ,(cadar pattern) (vector-ref *tuple* ,n))
               (do-setqs-aux (cdr pattern) (+ n 1))))
        (t (do-setqs-aux (cdr pattern) (+ n 1)))))

(defun do-setqs (pattern)
  (do-setqs-aux pattern 0))

(defmacro linda-in (pool tag . pattern)
  `(let ((*tuple* (convert (linda-tuple-value
                            (linda-in-tuple ,pool ,tag
                                            ,@(tidy-pattern pattern)))
                           <vector>)))
     ,@(do-setqs pattern)
     *tuple*))

(defun linda-in-tuple (pool tag . pattern)
  (when trace-linda? (format ";; in-ing ~a ~a~%" tag pattern))
  (let ((val (linda-in/read pool tag (tuple tag pattern) in-match)))
    (when trace-linda?
          (format ";; in'd ~a~%" val))
    val))

(defmacro linda-in? (pool tag . pattern)
  `(let ((*result* (linda-in?-tuple ,pool ,tag
                                    ,@(tidy-pattern pattern))))
     (if (null? *result*)
         ()
       (let ((*tuple* (convert (linda-tuple-value *result*) <vector>)))
         ,@(do-setqs pattern)
         t))))

(defun linda-in?-tuple (pool tag . pattern)
  (when trace-linda? (format ";; in?-ing ~a ~a~%" tag pattern))
  (let ((val (linda-in/read? pool tag (tuple tag pattern) in-match)))
    (when trace-linda?
          (format ";; in?'d ~a~%" val))
    val))

(defmacro linda-read (pool tag . pattern)
  `(let ((*tuple* (convert (linda-tuple-value
                            (linda-read-tuple ,pool ,tag
                                              ,@(tidy-pattern pattern)))
                           <vector>)))
     ,@(do-setqs pattern)
     *tuple*))

(defun linda-read-tuple (pool tag . pattern)
  (when trace-linda? (format ";; reading ~a ~a~%" tag pattern))
  (let ((val (linda-in/read pool tag (tuple tag pattern) read-match)))
    (when trace-linda?
          (format ";; read ~a~%" val))
    val))

(defmacro linda-read? (pool tag . pattern)
  `(let ((*result* (linda-read?-tuple ,pool ,tag
                                      ,@(tidy-pattern pattern))))
     (if (null? *result*)
         ()
       (let ((*tuple* (convert (linda-tuple-value *result*) <vector>)))
         ,@(do-setqs pattern)
         t))))

(defun linda-read?-tuple (pool tag . pattern)
  (when trace-linda? (format ";; read?-ing ~a ~a~%" tag pattern))
  (let ((val (linda-in/read? pool tag (tuple tag pattern) read-match)))
    (when trace-linda?
          (format ";; read?'d ~a~%" val))
    val))

(defun linda-in/read (pool tag pattern matchfn)
  (check-pool pool)
  (check-tag tag)
  (let ((llock (linda-pool-lock pool))
        (match ()))
    (lock llock)
    (unwind-protect
        (setq match (matchfn pool tag pattern))
      (unlock llock))
    (if (null? match)
        (progn
          (when trace-linda?
                (format ";; suspending~%"))
          (thread-reschedule)
          (when trace-linda?
                (format ";; retrying ~a ~a~%" tag
                        (linda-tuple-value pattern)))
          (linda-in/read pool tag pattern matchfn))
      match)))

(defun linda-in/read? (pool tag pattern matchfn)
  (check-pool pool)
  (check-tag tag)
  (let ((llock (linda-pool-lock pool))
        (match ()))
    (lock llock)
    (unwind-protect
        (setq match (matchfn pool tag pattern))
      (unlock llock))
    (if (null? match)
        ()
      match)))

(defun linda-out (pool tag . rest)
  (when trace-linda? (format ";; out ~a ~a~%" tag rest))
  (check-pool pool)
  (check-tag tag)
  (let ((llock (linda-pool-lock pool))
        (tup (tuple tag rest)))
    (lock llock)
    (unwind-protect
        (linda-out-tuple pool tag tup)
      (unlock llock))
    (thread-reschedule)
    tup))

(defun check-pool (p)
  (unless (pool? p)
          (error <bad-type>
                 "not a pool"
                 value: p
                 expected-type: <linda-pool>)))

(defun check-tag (tag)
  (when (or (eq tag '?)
            (and (cons? tag)
                 (eq (car tag) '?)))
        (error <general-error>
               "constant tag only"
               value: tag)))

(defun make-linda-tuple-table ()
  (make-table eql))

(defclass linda-tuple ()
  ((tag keyword: tag:
        reader: linda-tuple-tag)
   (value keyword: value:
          reader: linda-tuple-value))
  constructor: (tuple tag: value:))

(defmethod generic-write ((lt linda-tuple) s)
  (sformat s "#<linda-tuple: ~a ~a>"
           (linda-tuple-tag lt)
           (linda-tuple-value lt))
  lt)

(defmethod generic-print ((lt linda-tuple) s)
  (sformat s "#<linda-tuple: ~a ~a>"
           (linda-tuple-tag lt)
           (linda-tuple-value lt))
  lt)

;; delete one occurence of obj from lis: always one there
(defun delete1 (obj lis)
  (cond ((atom? lis) lis)
        ((binary= obj (car lis))
         (cdr lis))
        (t (cons (car lis) (delete1 obj (cdr lis))))))

(defun in-match (pool tag pattern-tuple)
  (let* ((table (linda-pool-tuple-table pool))
         (vallist (table-ref table tag))
         (val (match-in-list (linda-tuple-value pattern-tuple) vallist)))
    (unless (null? val)
            (let ((new (delete1 val vallist)))
              (if (null? new)
                  (table-delete table tag)
                ((setter table-ref) table tag new))))
    val))

;;   (defun read-match (pool tag pattern-tuple)
;;     (let* ((table (linda-pool-tuple-table pool))
;;            (vallist (table-ref table tag)))
;;       (match-in-list (linda-tuple-value pattern-tuple) vallist)))

;; try to be fair about picking a match
(defun read-match (pool tag pattern-tuple)
  (let* ((table (linda-pool-tuple-table pool))
         (vallist (table-ref table tag))
         (vals (match-all-in-list
                (linda-tuple-value pattern-tuple) vallist)))
    (cond ((null? vals)
           ())
          ((null? (cdr vals))
           (car vals))
          (t (element vals (random (length vals)))))))

(defun match-in-list (pat vallist)
  (cond ((null? vallist) ())
        ((matchit pat (linda-tuple-value (car vallist))) (car vallist))
        (t (match-in-list pat (cdr vallist)))))

(defun match-all-in-list (pat vallist)
  (cond ((null? vallist) ())
        ((matchit pat (linda-tuple-value (car vallist)))
         (cons (car vallist) (match-all-in-list pat (cdr vallist))))
        (t (match-all-in-list pat (cdr vallist)))))

(defun matchit (pat val)
  (cond ((null? pat) (null? val))       ; must be equal length
        ((null? val) ())
        ((binary= (car pat) (car val))
         (matchit (cdr pat) (cdr val)))
        ((eq (car pat) '?)
         (matchit (cdr pat) (cdr val)))
        (t ())))

(defun nconc (a b)
  (if (null? a)
      b
    (progn
      ((setter cdr) (last-pair a) b)
      a)))

; putting tuple at end allows weak fairness on tuple selection
; for a given tag
(defun linda-out-tuple (pool tag tuple)
  (let* ((table (linda-pool-tuple-table pool))
         (val (table-ref table tag)))
    ((setter table-ref) table tag (nconc val (list tuple)))
    tuple))

(defun linda-eval (fun . args)
  (when trace-linda?
        (format ";; eval ~a~%" fun))
  (apply thread-start (make-thread fun) args))

; a convenient fiddle
(defconstant ? '?)

)
