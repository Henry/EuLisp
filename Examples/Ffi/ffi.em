(defmodule ffi
  (syntax (syntax-0)
   import (level-0))

(defextern ext_get_struct () ptr)
(defextern ext_print_struct (ptr) ptr)
(deflocal structs '())

(let loop ((n 0))
     (cond ((< n 50)
            (let ((s (ext_get_struct)))
              (format "struct #~d~%" n)
              (print "  ")
              (ext_print_struct s)
              (print "  ")
              (write s)
              (setq structs (cons s structs))
              (loop (+ n 1))))
           (t
            (print "*** Finished creating" nl))))

(do (lambda (s) (ext_print_struct s)) structs)

;;;-----------------------------------------------------------------------------
)  ;; End of module
;;;-----------------------------------------------------------------------------
