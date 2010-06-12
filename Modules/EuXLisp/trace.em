(defmodule trace
    (import (level0)
     export (trace untrace))

  (defconstant trace-table (make-table eq))

  (defmacro trace (name)
    `(progn
       (setq old ,name)
       (defun ,name args
         (format t "-> ~s    ~s~%" ',name args)
         (let ((result (apply old args)))
           (format t "~s ->    ~s~%" ',name result)
           result))
       ((setter table-ref) trace-table ,name old)
       ',name))

  (defmacro untrace (name)
    `(let ((old (table-ref trace-table ,name)))
       (if old
           (progn
             (setq ,name old)
             ((setter table-ref) trace-table ,name ())
             ',name)
         #f)))

  )
