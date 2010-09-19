;;;-----------------------------------------------------------------------------
;;;                 OPS5 for EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;; File   : reader-vars.em
;;; Date   : 17 Jul 1995
;;; Author : Tracy Gardner (tag@maths.bath.ac.uk)
;;; Description: Functions to handle reading variables.
;;;-----------------------------------------------------------------------------
(defmodule reader-vars
  (syntax (macros macros-tag)
   import (level1 basic ops-out))

(print "### reader-vars")

;;;-----------------------------------------------------------------------------
;;; get-join-vars
; Construct  (join-vars . new-prod) where new prod is the production
; with all occurences of {x <<x x>> x} split into two symbols.
; *** Needs updating to handle condition element variables
;;;-----------------------------------------------------------------------------
(defun get-join-vars (prod-rest)
  ;;(sformat ops-out "get-join-vars: ~a~%" prod-rest)
  ;; loop to determine join variables
  (let ((res (labels ((loop (rest vars join-vars new-prod)
                            ;;(format "next elt: ~a~%" (car rest))
                            (cond
                              ((eql '--> (car rest))
                               (cons join-vars (append new-prod rest)))
                              ((eql (car rest) '-)
                               (loop (cdr rest) vars join-vars
                                     (append new-prod (list '-))))
                              ((is-ops5-var (car rest))
                               (loop (cddr rest) vars join-vars
                                     (append new-prod (list (car rest) (cadr rest)))))
                              ((eql (car rest) '})
                              (loop (cdr rest) vars join-vars
                                    (append new-prod (list '}))))
                      ((eql (car rest) '{)
                            (if (is-ops5-var (cadr rest))
                                (progn
                                  (loop (cddr rest) vars join-vars
                                        (append new-prod (list (car rest)
                                                               (cadr rest)))))
                              (loop (cdr rest) vars join-vars
                                    (append new-prod (list '{)))))
                            ((eql (size (car rest)) 1) ;; No tests
                             (loop (cdr rest) vars join-vars
                                   (append new-prod (list (car rest)))))
                            (t
                             (let* ((new-ce (split-symbols (car rest)))
                                    (all-vars (get-vars (car new-ce) vars join-vars)))
                               (loop (cdr rest) (car all-vars) (cdr all-vars)
                                     (append new-prod new-ce)))))))
                     (loop prod-rest () () ()))))
    ;;(print res)
    res))

(defun split-symbols (ce)
  ;;(sformat ops-out "split-symbols: ~a~%" ce)
  (labels
   ((loop (rest)
          (cond
            ((null? rest)
             ())
            (t
             (append (if (number? (car rest)) (list (car rest))
                       (split-symbol (car rest)))
                     (loop (cdr rest)))))))
   (list (loop ce))))

;;;-----------------------------------------------------------------------------
;;; split-symbol
; Used to determine whether a token contains a {, }, << or >> symbol
; (in which case the token must be split up, eg {<w> -> { <w> )
;;;-----------------------------------------------------------------------------
(defun split-symbol (x)
  ;;(sformat ops-out "split-symbol: ~a~%" x)
  (let ((ssres (let* ((x-as-string (symbol-name x))
                      (len (size x-as-string))
                      (first (element x-as-string 0)))
                 (if (eql len 1)
                     (list x)
                   (cond
                     ((eql first #\{)
                      (let ((sym1 (make-symbol (convert first <string>)))
                            (sym2 (make-symbol (make-new-string x-as-string 1))))
                        ;;(sformat ops-out "s1: ~a  s2: ~a~%" sym1 sym2)
                        (list sym1 sym2)))
                     ((and (eql first #\<) (eql (element x-as-string 1) #\<))
                      (if (eql len 2)
                          (list x)
                        (let ((sym1 (make-symbol (make-new-string x-as-string 0 1)))
                              (sym2 (make-symbol (make-new-string x-as-string 2))))
                          ;;(sformat ops-out "s1: ~a  s2: ~a~%" sym1 sym2)
                          (list sym1 sym2))))
                     (t
                      (let ((last (element x-as-string (- len 1))))
                        ;;(sformat ops-out "last: ~a~%" last)
                        (cond
                          ((eql last #\})
                           (let ((sym1 (make-symbol (make-new-string
                                                     x-as-string 0 (- len 2))))
                                 (sym2 (make-symbol (convert last <string>))))
                             ;;(sformat ops-out "s1: ~a  s2: ~a~%" sym1 sym2)
                             (list sym1 sym2)))
                          ((and (eql last #\>) (eql (element x-as-string (- len 2)) #\>))
                           (if (eql len 2)
                               (list x)
                             (let ((sym1 (make-symbol (make-new-string x-as-string
                                                                       0 (- len 3))))
                                   (sym2 (make-symbol (make-new-string x-as-string
                                                                       (- len 2)))))
                               ;;(sformat ops-out "s1: ~a  s2: ~a~%" sym1 sym2)
                               (list sym1 sym2))))
                          (t (list x))))))))))
    ;;(sformat ops-out "ssres: ~a~%" ssres)
    ssres))

;;;-----------------------------------------------------------------------------
;;; make-new-string
; make a string from a substring of another string
; first is the index of the first char to include
; last is the index of the last string to include (if last is
; omitted include rest of string)
;;;-----------------------------------------------------------------------------
(defun make-new-string (old-str first . last)
  ;;(sformat ops-out "old: ~a first: ~a last: ~a~%" old-str first last)
  (let ((end (if last (+ (car last) 1) (size old-str)))
        (init-str (make <string> size: 0)))
    (labels
     ((loop (indx new-str)
            (cond
              ((eql indx end) new-str)
              (t (loop (+ indx 1)
                       (concatenate new-str
                                    (convert
                                     (element old-str indx) <string>)))))))
     (loop first init-str))))

;;;-----------------------------------------------------------------------------
;;; get-vars
;;;-----------------------------------------------------------------------------
(defun get-vars (cond-el vars join-vars)
  ;;(sformat ops-out "get-vars: ~a ~a ~a ~%" cond-el vars join-vars)
  (cond
    ((null? cond-el)
     (cons vars join-vars))
    ((binary= (car cond-el) '})
    (get-vars (cdr cond-el) vars join-vars))
  ((binary= (cadr cond-el) '{)
            (get-vars (cddr cond-el) vars join-vars))
   ((eql (cadr cond-el) '<<)
    (get-vars (cdr (member '>> cond-el))
              vars join-vars))
   ;; ((binary= (car cond-el) '{)
   ;;  (get-vars (cddr cond-el) vars join-vars))
   ((eql (car cond-el) '<<)
    (get-vars (cdr (member '>> cond-el)) vars join-vars))
   ((is-attrib (car cond-el))
    (if (is-ops5-pred (cadr cond-el))
        (sort-vars (caddr cond-el) (cdddr cond-el) vars join-vars)
      (sort-vars (cadr cond-el) (cddr cond-el) vars join-vars)))
   (t
    (if (is-ops5-pred (car cond-el))
        (sort-vars (cadr cond-el) (cddr cond-el) vars join-vars)
      (sort-vars (car cond-el) (cdr cond-el) vars join-vars)))))

(defun sort-vars (new-var rest vars join-vars)
  ;;(sformat ops-out "sort-vars: ~a ~a ~a ~a~%" new-var rest vars join-vars)
  ;;(sformat ops-out "not member: ~a~%" (null? (member new-var vars)))
  (cond
    ((is-constant new-var)
     ;;(print "constant")
     (get-vars rest vars join-vars))
    ((member new-var vars)
     ;;(print "join-var")
     (get-vars rest vars
               (if (member new-var join-vars)
                   join-vars
                 (cons new-var join-vars))))
    (t
     ;;(print "non-join var")
     (get-vars rest (cons new-var vars) join-vars))))

(defun is-constant (x)
  ;;(format "is-constant: ~a~%" x)
  (if (number? x) t
    (let* ((name (symbol-name x))
           (first (element name 0))
           (same  (eql first #\<)))
      (null? same))))

(defun is-attrib (x)
  ;;(sformat ops-out "x: ~a~a~%" x (class-of x))
  (let ((name (symbol-name x)))
    (eql (element (symbol-name x) 0)
         #\\x005e )))

(defun is-ops5-pred (x)
  (member x '(< <= > >= <> = <=>) binary=))

(defun is-ops5-var (x)
  (if (or (list? x) (number? x)) ()
    (and (eql (element (symbol-name x) 0) #\<)
         (eql (element (symbol-name x) (- (size (symbol-name x)) 1)) #\>))))

(export get-join-vars
 is-constant is-attrib is-ops5-pred is-ops5-var)

;;;-----------------------------------------------------------------------------
)  ;; End of module
;;;-----------------------------------------------------------------------------
