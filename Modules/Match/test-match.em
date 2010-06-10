(defmodule test-match
  (syntax (macros match test-match-macros)
   import (level1 match-support))

  (defun f1 (l)
    (match l
      (() 'nil)
      (1  'one)
      ("hello" 'hello)
      ((1 2) 'const-1-2)
      ((('z c d) a b) (list 'nested-z a b c d))
      (('z . b) (list 'z-prefix b))
      ((a b) (list 'var-a-b a b))
      ((a . b) (list 'var-a.b a b))
      (#(a b) (list 'vect-a-b a b))
      (#(#(c d) a b) (list 'vect-nested a b c d))
      (#(a b c ...) (list 'vect-ellip a b c))
      (_ 'anything-else)))

  (defconstant fact
    (match-lambda
     (1 1)
     (n (* n (fact (- n 1))))))

  (defconstant read
    (match-lambda*
     (()
      '(read default-port default-eof-object))
     ((port)
      `(read ,port default-eof-object))
     ((port eof-object)
      `(read ,port ,eof-object))))

  (defun main ()
    (print-test (f1 '()))
    (print-test (f1 1))
    (print-test (f1 '(1)))
    (print-test (f1 '(1 2)))
    (print-test (f1 '(a b)))
    (print-test (f1 '(a b c d e f)))
    (print-test (f1 "hello"))
    (print-test (f1 '(z 2)))
    (print-test (f1 '((z 3 4) 1 2)))
    (print-test (f1 '((z 3 4 5) 1 2)))
    (print-test (f1 #(1 2)))
    (print-test (f1 #(1 2 3 4 5)))
    (print-test (f1 #(#(3 4) 1 2)))
    (print-test (f1  (fact 6)))
    (print-test (f1  (read)))
    (print-test (f1  (read 'port)))
    (print-test (f1  (read 'port 'eof-object)))
    (print-test (f1 (match '(let ((a 1)
                                  (b 2)
                                  (c 3))
                              (m a)
                              (m b)
                              (m c)
                              (list a b c))
                           (('let ((a b) ...) body ...)
                            (list vars: a  vals: b body: body))
                           (_ 'no-match))))
    (print-test (match '(let3 ((a b c) (d e f)) body1 body2)
                  (('let3 ((a b c) ...) body ...) (list (list a b c) body))
                  (_ 'no-match)))
    )

  (main)

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
