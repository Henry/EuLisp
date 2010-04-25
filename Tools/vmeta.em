;;;; vmeta.em -- verbose META in EuLisp.  A work in progress.      -*- lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  You can also find this code at http://access.mountain.net/~tkb/software.html
;;;
;;;  This code was inspired by Henry G. Baker's article "Pragmatic
;;;  Parsing in Common Lisp", ACM LISP Pointers IV,2 (April-June
;;;  1991),3-15.  However, any errors are mine; inefficencies too.  There
;;;  have been some minor changes and additions from the code in that
;;;  article. You can download the original paper from
;;;  <URL:ftp://ftp.netcom.com/pub/hb/hbaker/home.html>.
;;;
;;;  Comments to tkb@access.mountain.net.
;;;
;;; Description of forms:
;;;
;;;    match exp => generalized-boolean
;;;
;;;      This form applies the matching expression `exp' to the value of
;;;      `sequence', starting at `index' and ending at `end'.  It returns
;;;      true if the expression matched.
;;;
;;;      The user must lexically bind `sequence' to the sequence to parse,
;;;      `index' to where the parse should start, and `end' to where the
;;;      parse should end.  The implementation of the matching languag
;;;      uses those variables for its own purposes, so the user should
;;;      not assign to them (or even reference them if they do not know
;;;      what they are doing.).
;;;
;;;      Example:
;;;
;;;     (deftype alphanumeric () '(satisfies alphanumericp))

;;;     (deftype not-space () '(not (eql #\space)))
;;;     (defun match-imap-status (sequence
;;;                               &optional (index 0) (end (length sequence))

;;;                               &aux id status rest)
;;;            (and
;;;             (match [%(id ^(@alphanumeric 1 nil))

;;;                     #\space
;;;                     %(status ^(@not-space 1 nil))

;;;                     {[#\space %(rest ^(@character))] []}])
;;;             (values t id status rest)))

;;;     (match-imap-status "a001 OK Interesting response.")
;;;     ;;=> T ; "a001" ; "OK" ; "Interesting response."
;;;        (match-imap-status "bogusdatahere")

;;;        ;;=> NIL
;;;
;;;    match-expr s ([start [end]]) exp => generalized-boolean
;;;
;;;      This form binds `sequence' to the value of s and applies the
;;;      matching expression `exp' to `sequence'.  If `start' is
;;;      specified `index' is bound to its value; otherwise `index' is
;;;      bound to zero.  If `end' is specified, `end' is bound to its
;;;      value; otherwise `end' is bound to the length of the sequence.
;;;
;;;      Example:
;;;
;;;        (let (id status rest)

;;;       (and
;;;        (match-expr "a002 BAD Another interesting response." ()

;;;                    [%(id ^(@alphanumeric 1 nil))
;;;                      #\space
;;;                      %(status ^(@not-space 1 nil))

;;;                      {[#\space %(rest ^(@character))] []}])
;;;        (values t id status rest)))

;;;         ;;=> T ; "a002" ; "BAD" ; "Another interesting response."
;;;
;;;
;;;
;;; Description of matching expression syntax:
;;;
;;;    Non-terminals are in lower case.  The symbol `...' indicates zero or
;;;    more of the preceding expression.
;;;
;;;    [exp ...]
;;;    (SEQ exp ...)

;;;     Matches every one of `exp ...'.
;;;
;;;    {exp ...]
;;;    (ALT exp ...)

;;;     Matches exactly one of `exp ...'
;;;
;;;    @typepred
;;;    @(typepred var)

;;;    (type typepred)
;;;    (type typepred var)

;;;     Matches if, given the current `element', (typep element typepred).
;;;     If `var' was specified, it is assigned the value of `element'.
;;;
;;;    $exp
;;;    ^(exp)

;;;    ^(exp min)
;;;    ^(exp min max)

;;;    (star exp)
;;;    (star exp min)

;;;    (star exp min max)
;;;
;;;     Matches zero or more of `exp'.  If min is specified by itself,
;;;     matches exactly `min' of `exp'.  If min and max are specified,
;;;     matches at least `min' and at most `max' of `exp'.  Min and max
;;;     can be NIL, in which case they do not limit (Thus, (star exp 3 nil)

;;;     matches at least 3 of `exp', and possibly more.)
;;;
;;;    (not exp)

;;;     Doesn't match if `exp' does.
;;;
;;;    %(var exp)

;;;    (name var exp)
;;;     Matches `exp' and sets `var' to the matching subsequence.
;;;
;;;    (push var exp)

;;;        Matches `exp' and pushes the matching subsequence onto `var'.
;;;
;;;    (end)

;;;     Matches if at the end of the sequence..
;;;
;;;  Note:
;;;
;;;    Despite the use of `sequence' as a variable name, I haven't
;;;    actually tried using this to parse anything but lists.  At least
;;;    the `name' ecase branch, `match-literal' and `match-type' would
;;;    have to change for that.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmodule vmeta
  (syntax (macros vmeta-aux)
   import (level1 nuseq))

;;; Implementation
  ;; I dropped back to a fully parenthesized form so I could work with
  ;; the internals and not have to worry about the read-table/syntax
  ;; issues at the same time.  When I finished with that I added the
  ;; syntax from the paper back, setting read macro characters to translate
  ;; into the parenthesized form.  That way I can use this in Lisps that don't
  ;; have read macros, but have the terse and convenient syntax in
  ;; those that do.
  (defmacro iter (inits test . body)
    (let ((loop (gensym)))
      `(let ,loop (,@(map (lambda (initform)
                            (let ((var (car initform))
                                  (init (cadr initform))
                                  (step (caddr initform)))
                              (list var init)))
                          inits))
            (cond (,(car test)
                   ,@(if (null (cdr test))
                         '(nil)
                       (cdr test)))
                  (t
                   ,@body
                   (,loop ,@(map (lambda (initform)
                                  (let ((var (car initform))
                                        (init (cadr initform))
                                        (step (caddr initform)))
                                    step))
                                inits)))
                  ))))

  (defmacro incf (n)
    `(setq ,n (+ ,n 1)))

  ;; Baker's match, for strings.
  (defmacro match-literal (x)
    (cond
     ((characterp x)
      `(when (and (< index end) (equal (element sequence index) ',x))
         (incf index)))
     ((stringp x)
      `(let ((old-index index))         ; 'old-index' is a *lexical* variable.
         (or (and ,@(map (lambda (c) `(match-literal ,c)) (convert x <list>)))
             (progn (setq index old-index) nil))))))

  ;; Doesn't work in EuLisp!!!  ???
  ;; Baker's match-type, for strings.
  (defmacro match-type (x v)
    `(when (and (< index end) (,x (element sequence index)))
       ,(if v `(setq ,v (element sequence index)) ())
       (incf index)))

  ;; Note: in Youtoo use () instead of nil! since "(eq 'nil '()) => ()".
  ;; This is only so complicated because we allow
  ;;   (star x) to match zero or more Xs,
  ;;   (star x min) to match exactly MIN Xs,
  ;;   (star x min nil) to match at least MIN Xs,
  ;;   (star x nil max) to match at most MAX Xs, and
  ;;   (star x min max) to match at least MIN and at most MAX Xs.
  (defun match-star (min-specified max-specified min max matcher x)
    (cond
     ;; No min or max specified, match any number.
     ((not (or min max))
                                        ;    (format stderr "match any number: ~s\n" x)
      `(not (iter () ((not ,matcher)))))
     ;; Min specified, but no max specified, so
     ;; match exactly min.
     ((and min (not max-specified))
                                        ;    (format stderr "match exactly ~s: ~s\n" min x)
      `(iter ((i 0 (+ i 1)))
             ((or (> i ,min) (not ,matcher))
              (and (= i ,min)))))
     ;; Min specified, but max is nil, so match
     ;; at least min.
     ((and min max-specified (not max))
                                        ;    (format stderr "match at least ~s: ~s\n" min x)
      `(iter ((i 0 (+ 1 i)))
             ((not ,matcher)
              (>= i ,min))))
     ;; Min specified, max specified, so match
     ;; at least min (none if min was null) and at
     ;; most most max.
     ((and min-specified max-specified max)
                                        ;    (format stderr "match at least ~s and at most ~s: ~s\n" min max x)
      (when (not min)
        (setq min 0))
      `(iter ((i 0 (+ 1 i)))
             ((or (= i ,max) (not ,matcher))
              (and (>= i ,min) (<= i ,max)))))
     (t
      (error "~s is not a valid star form." x))))

  ;; Baker's compileit.
  ;; * I thought about adding (start), like (end) below, but since this doesn't
  ;;   do searching it might not be useful.
  ;; * esc tries to have no effect on the matching, which requires keeping
  ;;   track of whether we are in a sequence or not.  Pred is just like
  ;;   Baker's `!'.
  ;; * I wonder if it is worth it to worry about name capture?
  (defun compile-match (x)
    (labels ((helper (x in-seq-p)
                     (cond
                      ((and (listp x) (symbolp (car x)))
                       (ecase (car x)
                         (esc `(progn ,@(cdr x) ,in-seq-p))
                         (pred (cadr x))
                         (seq `(and ,@(map (lambda (x) (helper x t))
                                           (cdr x))))
                         (alt `(or ,@(map (lambda (x) (helper x nil))
                                          (cdr x))))
                         ;; This is complicated by the fact that @typepred results in
                         ;; (type . typepred)
                         (type
                                        ;                (format stderr "~s" x)
                          (if (symbolp (cdr x))
                              `(match-type ,(cdr x) nil)
                            `(match-type ,(cadr x) ,(if (> 2 (size x))
                                                        (caddr x)
                                                      ()))))
                         ;; Ugh.  This would be simple if we only allowed (star x):
                         ;;   (star `(not (iter () ((not (helper (cadr x) in-seq-p))))))
                         (star
                          (let* ((len (size x))
                                 (min-specified (and (> len 2))); had  (caddr x)
                                 (max-specified (and (> len 3))); had  (cadddr x)
                                 (min (if min-specified (caddr x) ()))
                                 (max (if max-specified (cadddr x) ())))
                                        ;                  (format stderr "min-s: ~s max-s: ~s min: ~s max: ~s x: ~s\n"
                                        ;                          min-specified max-specified min max x)
                                        ;                  (if (null max-specified)
                                        ;                      (print "max-specified null")
                                        ;                    (print "max-speicified not null"))
                                        ;                  (if (symbolp max-specified)
                                        ;                      (print "max-specified symbolp")
                                        ;                    (print "max-specified not symbolp"))
                                        ;                  (format stderr "class-of max-specified: ~a\n"
                                        ;                          (class-of max-specified))
                            (match-star min-specified   ; min-specified
                                        max-specified  ; max-specified
                                        min  ; min
                                        max ; max
                                        (helper (cadr x) in-seq-p) ;matcher
                                        x             ; for error messages.
                                        )))
                         (not `(not ,(helper (cadr x) in-seq-p)))
                         (name `(let* ((start index)
                                       (val ,(helper (caddr x) in-seq-p))
                                       (last index))
                                  (when val
                                    (setq ,(cadr x) (subseq sequence start last)))
                                  val))
                         (push `(let* ((start index)
                                       (val ,(helper (caddr x) in-seq-p))
                                       (last index))
                                  (when val
                                    (push (subseq sequence start last)
                                          ,(cadr x)))
                                  val))
                         (end '(= index end))
                         ))
                      (t `(match-literal ,x)))))
      (helper x ())))

  ;; Baker's matchit.
  (defmacro match (x) (compile-match x))

  ;; This allows using match an expression without wrapping it in a
  ;; function.
  (defmacro match-expr (sequence expr)
    `(let* ((sequence ,sequence)
            (index 0)
            (end   (size sequence)))
       ,(compile-match expr)))

  (defun show-expr  (sequence expr)
    (write
     `(let* ((sequence ,sequence)
             (index 0)
             (end   (size sequence)))
        ,(compile-match expr)))
    (newline))

  (defmacro doit (sequence . forms)
    `(let* ((sequence ,sequence)
            (index 0)
            (end (size sequence)))
       (write (progn ,@forms))
       (newline)
       (newline)
       t))

  (defmacro testit (expr expected)
    (let ((res (gensym)))
      `(let ((,res ,expr))
         (format t "expression: ~s\n    result: ~s\n  expected: ~s\n"
                 ',expr ,res ,expected))))

  (defmacro push (item place)
    `(setq ,place (cons ,item ,place)))

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
