;; match0.em -- macros for match.em
(defmodule match0
  (syntax (macros)
   import (level1))

  ;; I should really look at Scheme/*.em for better versions???
  ;; This really should be using an equivalent to eqv, not eq.
  (defmacro case (exp . cases)
    (let ((case-val (gensym)))
      `(let ((else t) (,case-val ,exp))
         (cond ,@(map (lambda (x)
                        (if (and (symbol? (car x)) (eq (car x) 'else))
                            x
                          `((eq (quote ,(caar x)) ,case-val) ,(cadr x))))
                      cases)))))

  (defmacro begin body `(progn ,@body))

  (defmacro letrec (inits . body)
    `(let (,@(map (lambda (init) `(,(car init) '())) inits))
       ,@(map (lambda (init) `(setq ,(car init) ,(cadr init))) inits)
       ,@body))

  (defmacro set! args `(setq ,@args))

  (defmacro define (name . body)
    (if (consp name)
        `(deflocal ,(car name) (lambda ,(cdr name) ,@body))
      `(deflocal ,name ,@body)))

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
