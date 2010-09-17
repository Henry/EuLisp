;;; modular arithmetic

(defmodule modular
  (import (level-0)
   export (mod <modular-error> <modular-number> <no-modular-inverse>))

(defclass <modular-number> ()
  ((value keyword: value:
          accessor: modular-number-value)
   (modulus keyword: modulus:
            accessor: modular-number-modulus))
  constructor: (make-modular-number value: modulus:)
  predicate: modular-number?)

(defmethod generic-write ((a <modular-number>) stream)
  (generic-print "#<" stream)
  (generic-print (modular-number-value a) stream)
  (generic-print " mod " stream)
  (generic-print (modular-number-modulus a) stream)
  (generic-print ">" stream)
  a)

(defun mod (a n)
  (make-modular-number
   (if (< a 0) (+ n a) a)
   n))

(defcondition  <modular-error> ())

(defcondition <modular-argument-mismatch>
              <modular-error>
              arg1 0
              arg2 0)

(defun modular-mismatch (moda modb)
  (error <modular-argument-mismatch>
         "mismatch moduli in modular +"
         arg1: moda
         arg2: modb))

(defmethod binary+ ((a <modular-number>) (b <modular-number>))
  (let ((moda (modular-number-modulus a))
        (modb (modular-number-modulus b)))
    (if (= moda modb)
        (mod
         (remainder (+ (modular-number-value a)
                       (modular-number-value b))
                    moda)
         moda)
      (modular-mismatch moda modb))))

(defmethod binary- ((a <modular-number>) (b <modular-number>))
  (let ((moda (modular-number-modulus a))
        (modb (modular-number-modulus b)))
    (if (= moda modb)
        (mod
         (remainder (- (modular-number-value a)
                       (modular-number-value b))
                    moda)
         moda)
      (modular-mismatch moda modb))))

(defmethod unary- ((a <modular-number>))
  (let ((moda (modular-number-modulus a)))
    (mod (- moda (modular-number-value a)) moda)))

(defmethod binary* ((a <modular-number>) (b <modular-number>))
  (let ((moda (modular-number-modulus a))
        (modb (modular-number-modulus b)))
    (if (= moda modb)
        (mod
         (remainder (* (modular-number-value a)
                       (modular-number-value b))
                    moda)
         moda)
      (modular-mismatch moda modb))))

(defmethod binary/ ((a <modular-number>) (b <modular-number>))
  (let ((moda (modular-number-modulus a))
        (modb (modular-number-modulus b)))
    (if (= moda modb)
        (binary* a (unary/ b))
      (modular-mismatch moda modb))))

(defmethod unary/ ((a <modular-number>))
  (let ((moda (modular-number-modulus a)))
    (mod (inverse-mod-n (modular-number-value a) moda) moda)))

(defcondition <no-modular-inverse>
              <modular-error>
              value 0
              modulus 0)

(defun inverse-mod-n (a n)
  (if (> (gcd a n) 1)
      (error <no-modular-inverse>
             "no modular inverse"
             value: a
             modulus: n)
    (let ((ans (gcd-cofactors a n 1 0 0 1)))
      (car ans))))

(defun gcd-cofactors (a b cfa1 cfa2 cfb1 cfb2)
  (if (= b 0)
      (cons cfa1 cfa2)
    (let ((q (quotient a b)))
      (gcd-cofactors
       b
       (- a (* q b))
       cfb1
       cfb2
       (- cfa1 (* q cfb1))
       (- cfa2 (* q cfb2))))))

)
