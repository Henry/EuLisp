(defmodule test3
  (syntax (syntax-0)
   import (level-0 serial))

(defmethod generic-connect ((s1 <stream>) (s2 <stream>) options)
  (let ((mode (init-list-ref options mode: 'r)))
    (cond ((eq mode 'r)
           ((setter stream-source) s1 s2))
          ((eq mode 'w)
           ((setter stream-sink) s1 s2))
          ((eq mode 'a)
           ((setter stream-sink) s1 s2))
          (t
           ((setter stream-source) s1 s2)
           ((setter stream-sink) s1 s2)))
    ()))

(defun my-deserialize ()
  (let ((x (deserialize)))
    (spprint x stderr)
    (cond ((generic-function? x)
           (x 42 "abc")
           (x 1.23 'foo))
          ((function? x)
           (print (x 1 2 3 4) nl)
           ())
          ((thread? x)
           (thread-reschedule x)))))

(my-deserialize)

;;;-----------------------------------------------------------------------------
)  ;; End of module
;;;-----------------------------------------------------------------------------
