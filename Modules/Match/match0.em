;; match0.em -- macros for match.em
(defmodule match0
  (syntax (macros)
   import (level1))

  (defmacro case (exp . cases)
    (let ((case-val (gensym)))
      `(let ((else t) (,case-val ,exp))
         (cond ,@(map (lambda (x)
                        (if (and (symbol? (car x)) (eql (car x) 'else))
                            x
                          `((eql (quote ,(caar x)) ,case-val) ,(cadr x))))
                      cases)))))

  (defmacro letrec (inits . body)
    `(let (,@(map (lambda (init) `(,(car init) '())) inits))
       ,@(map (lambda (init) `(setq ,(car init) ,(cadr init))) inits)
       ,@body))

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
