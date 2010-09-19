;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: telos (The EuLisp Object System -- TELOS)
;;;  Authors: Russel Bradford, Andreas Kind
;;; Description: handling generic functions
;;;-----------------------------------------------------------------------------
(defmodule mop-gf
  (syntax (_boot0 _mop-gf0)
   import (boot mop-prim mop-class mop-inspect)
   export (gf-reset-cache the-method-lookup-function
                          initialize finalize allocate make
                          error-no-applicable-methods sig=
                          discriminating-domain make-generic-function))

;;;-----------------------------------------------------------------------------
;;; Initialize, allocate and make
;;;-----------------------------------------------------------------------------
(defgeneric initialize ((obj <object>) inits))
((setter generic-function-method-cache) initialize (make-vector 34))
(defgeneric finalize ((obj <object>)))
(defgeneric allocate ((cl <class>) inits))

(defun make (cl . keywords)
  (initialize (allocate cl keywords) keywords))

;;;-----------------------------------------------------------------------------
;;; Make generic function
;;;-----------------------------------------------------------------------------
(defun make-generic-function
  (name domain gf-class method-class method-inits keywords)
  (if (and (eq gf-class <simple-generic-function>)
           (eq method-class <simple-method>)
           (null? method-inits)
           (null? keywords))
      (primitive-make-generic-function name domain)
    (apply make
           gf-class
           name: name
           domain: domain
           method-class: method-class
           method-keywords: method-inits
           keywords)))

(defun primitive-make-generic-function (name domain)
  (let ((gf (primitive-allocate <simple-generic-function> gf-size)))
    ((setter function-name) gf name)
    ((setter generic-function-domain) gf domain)
    ((setter generic-function-method-class) gf <simple-method>)
    ((setter generic-function-method-lookup-function)
     gf (named-lambda primitive-method-lookup-function values
                      (the-method-lookup-function gf values domain)))
    ((setter generic-function-discriminating-function)
     gf (compute-primitive-discriminating-function gf))
    gf))

;;;-----------------------------------------------------------------------------
;;; Disciminating function
;;;-----------------------------------------------------------------------------
(defun compute-primitive-discriminating-function (gf)
  (named-lambda
   primitive-discriminating-function values
   (let* ((method-cache (generic-function-method-cache gf))
          (value-dom (vector-ref method-cache 0))
          (method-cache-index (vector-ref method-cache 1))
          (lookup (generic-function-method-lookup-function gf))
          (appl-meths (apply lookup values))
          (meth-funs (map1-list method-function appl-meths)))
     (if (null? meth-funs)
         (error-no-applicable-methods gf values)
       (let ((entry (cons value-dom meth-funs)))
         ((setter vector-ref) method-cache method-cache-index entry)
         (set-global-register next-methods (cdr meth-funs))
         ;; Apply the function of the most specific applicable method
         (apply (car meth-funs) values))))))

(defun error-no-applicable-methods (gf values)
  (error () "no applicable methods for ~a\n    arguments: ~a\n    classes: ~a"
         (function-name gf) values (map1-list class-of values)))

;;;-----------------------------------------------------------------------------
;;; Method lookup
;;;-----------------------------------------------------------------------------
(defun the-method-lookup-function (gf values domain)
  (let ((value-dom (vector-ref (generic-function-method-cache gf) 0))
        ;;(value-dom (discriminating-domain values domain))
        )
    (sort-list
     (select-methods value-dom (generic-function-methods gf))
     (lambda (md1 md2)
       (sig<= (method-domain md1) (method-domain md2) value-dom)))))

(defun discriminating-domain (values domain)
  (labels
   ((loop (l1 l2 res)
          (if (null? l2)
              (reverse-list res)
            (if (car l2)
                (loop (cdr l1) (cdr l2) (cons (class-of (car l1)) res))
              ;; non-discriminating arg
              (loop (cdr l1) (cdr l2))))))
   (loop values domain ())))

(defun select-methods (value-dom meths)
  (labels
   ((loop (l res)
          (if (null? l)
              res
            (let ((meth (car l)))
              (if (sig-applicable? value-dom (method-domain meth))
                  (loop (cdr l) (cons meth res))
                (loop (cdr l) res))))))
   (loop meths ())))

(defun sig-applicable? (val-dom meth-dom)
  ;; Assume equal length
  (let ((arity (vector-size meth-dom)))
    (labels
     ((loop (i)
            (if (< i arity)
                (let ((meth-cl (vector-ref meth-dom i)))
                  (if meth-cl
                      (let ((val-cl (vector-ref val-dom i)))
                        (if (cpl-subclass? val-cl meth-cl)
                            (loop (+ i 1))
                          ()))
                    (loop (+ i 1))))
              t)))
     (let ((res (loop 0)))
       res))))

;;;-----------------------------------------------------------------------------
;;; Range and domain
;;;-----------------------------------------------------------------------------
(defun sig= (dom1 dom2)
  ;; Assume equal length
  (let ((n (vector-size dom1)))
    (labels
     ((loop (i)
            (if (< i n)
                ;; Also case of non-discriminating arg
                (and (eq (vector-ref dom1 i)
                         (vector-ref dom2 i))
                     (loop (+ i 1)))
              t)))
     (loop 0))))

(defun sig<= (dom1 dom2 val-dom)
  ;; Assume equal length
  (let ((n (vector-size dom1)))
    (labels
     ((loop (i)
            (if (< i n)
                (let ((cl1 (vector-ref dom1 i))
                      (cl2 (vector-ref dom2 i)))
                  (if (eq cl1 cl2)
                      ;; Also case of non-discriminating arg
                      (loop (+ i 1))
                    (labels
                     ((cpl-loop (cpl)
                                ;; Is cl1 before cl2 in cpl of class of arg?
                                (let ((cl (car cpl)))
                                  (if (eq cl1 cl) t
                                    (if (eq cl2 cl) ()
                                      (cpl-loop (cdr cpl)))))))
                     (cpl-loop (class-precedence-list
                                (vector-ref val-dom i))))))
              t)))
     (loop 0))))

;;;-----------------------------------------------------------------------------
;;; Mehthod caching
;;;-----------------------------------------------------------------------------
(defun gf-reset-cache (gf)
  (let ((vec (generic-function-method-cache gf)))
    (if vec
        (let ((n (vector-size vec)))
          (labels
           ((loop (i)
                  (if (< i n)
                      (progn
                        ((setter vector-ref) vec i ())
                        (loop (+ i 1)))
                    vec)))
           (loop 0)))
      ;          ((setter generic-function-method-cache) gf
      ;           (make-vector (vector-size vec)))
      ())))

;;;-----------------------------------------------------------------------------
;;; Error and warning (using simple format)
;;;-----------------------------------------------------------------------------
(setq *error*
      (named-lambda error (str . args)
                    (format 2 "*** ERROR [level1]: ")
                    (apply format 2 str args)
                    (format 2 "\n")
                    (format 2 "***    See Backtrace? (y/n) ")
                    (if (eq (getchar) #\y) (backtrace) ())
                    (exit)))

(setq *warning*
      (named-lambda warning (str . args)
                    (format 2 "*** WARNING [level1]: ")
                    (apply format 2 str args)
                    (format 2 "\n")))

;;;-----------------------------------------------------------------------------
)  ;; End of module
;;;-----------------------------------------------------------------------------
