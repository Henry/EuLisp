;;; Verbose META in EuLisp.
;;;-----------------------------------------------------------------------------
;;; Test application
;;;-----------------------------------------------------------------------------
(defmodule test-vmeta
  (syntax (macros vmeta)
   import (level1))

  (defmethod binary= ((c1 <character>) (c2 <character>)) (equal c1 c2))

  (defun digit (c)
    (and (characterp c)
         (<= #\0 c #\9)))

  (defun whitespace (c)
    (and (characterp c)
         (or (= c #\	)
             (= c #\ ))))

  (defun letter (c)
    (and (characterp c)
         (or (<= #\a c #\z)
             (<= #\A c #\Z))))

  (defun identifier (c)
    (or (letter c)
        (digit c)
        (= #\- c)))

  (let (name args number)
    (match-expr
      "(def-bytecode write-object (x)	66 (in obj1 obj2) (out obj))"
      (seq
        (star (type whitespace))
        "(def-bytecode"
        (star (type whitespace))
        (name name (seq (type letter)
                        (star (type identifier))))
        (star (type whitespace))
        "("
        (star (seq
                (push args (star (type identifier) 1 ()))
                (star (alt (type whitespace)))))
        ")"
        (star (alt (type whitespace)))
        (name number (seq (star (type digit) 1 ())))))
    (format t "~a ~a ~a ~%" name args number))

  (defun match-test (sequence)
    (let ((index 0)
          (end (size sequence)))
      (let (name args number)
        (match
          (seq
            (star (type whitespace))
            "(def-bytecode"
            (star (type whitespace))
            (name name (seq (type letter)
                            (star (type identifier))))
            (star (type whitespace))
            "("
            (star (seq
                    (push args (star (type identifier) 1 ()))
                    (star (alt (type whitespace)))))
            ")"
            (star (alt (type whitespace)))
            (name number (seq (star (type digit) 1 ())))))
        (format t "~a ~a ~a ~%" name args number))
      ))

  (match-test "(def-bytecode write-object (x)	66 (in obj1 obj2) (out obj))")

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
