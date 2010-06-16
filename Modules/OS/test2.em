(defmodule test2
  (syntax (macros)
   import (level1 serial))

;;;-----------------------------------------------------------------------------
;;; Test values
;;;-----------------------------------------------------------------------------
  (defclass <foo> ()
    ((u accessor: foo-u keyword: u: default: 34)
     (v accessor: foo-v keyword: v: default: 35)))

  (defclass <bar> (<foo>)
    ((r accessor: bar-r keyword: r: default: 36)))

  (defun baz x (print (reverse x)))
  (defun boo x x)

  (defgeneric boz (x y))
  (defmethod boz ((x <int>) (y <string>))
    (format t "method1: ~a ~a\n" x y))
  (defmethod boz ((x <double>) (y <symbol>))
    (format t "method1: ~a ~a\n" x y))

;;;-----------------------------------------------------------------------------
;;; Serialization tests
;;;-----------------------------------------------------------------------------
  (defun my-deserialize ()
    (pprint (deserialize)))

  ;;  (defun Deserialize ss
  ;;    (let ((s (if ss (car ss) stdin))
  ;;        (os (make <object-stream> mode: 'r)))
  ;;      (pprint os stderr)
  ;;      (connect os s)
  ;;      (pprint os stderr)
  ;;      (let ((res (read os)))
  ;;      (disconnect os)
  ;;      res)))

  (defun my-serialize (type fs)
    (let* ((os (make <object-stream> mode: 'w)))
      (pprint os stderr)
      (connect os fs)
      (pprint os stderr)
      (cond
       ((binary= type "null")
        (swrite os ()))
       ((binary= type "int")
        (swrite os 42))
       ((binary= type "double")
        (swrite os 42.123))
       ((binary= type "char")
        (swrite os #\x))
       ((binary= type "string")
        (swrite os "Hello world!"))
       ((binary= type "symbol")
        (swrite os 'foo))
       ((binary= type "keyword")
        (swrite os bar:))
       ((binary= type "cons")
        (swrite os '(42 43)))
       ((binary= type "vector")
        (swrite os #(42 #\x)))
       ((binary= type "object")
        (swrite os (make <bar>)))
       ((binary= type "function1")
        (swrite os list))
       ((binary= type "function2")
        (swrite os +))
       ((binary= type "function3")
        (swrite os baz))
       ((binary= type "function4")
        (swrite os boo))
       ((binary= type "function5")
        (swrite os binary+))
       ((binary= type "function6")
        (swrite os boz))
       ((binary= type "thread")
        (let ((thr (make <current-thread> function: (lambda (thr)
                                              (pprint thr stderr)
                                              (sprint stderr 42)
                                              (swrite os thr)
                                              (sprint stderr 43)))))
          (thread-start thr thr)
          (thread-value thr)))
       (t
        (format stderr "*** ERROR: unknown type ~a\n" type)
        (swrite os ())))
      (flush os)
      (disconnect os)))
  (if (< *argc* 2)
      (my-deserialize)
    (let ((type (vector-ref *argv* 1)))
      (my-serialize type stdout)))

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
