;;; -*- lisp -*-
(defmodule b2h-aux
  (syntax (macros)
   import (level1))
  (defmacro with-input-file* (file-name . body)
    (let ((res (gensym))
          (s (gensym))
          (orig-source (gensym))
          (orig-sink (gensym)))
      `(let ((,s (make <file-stream> file-name: ,file-name mode: 'r))
             (,res ())
             (,orig-source (stream-source stdin))
             (,orig-sink (stream-sink stdin)))
         (reconnect ,s stdin)
         (unwind-protect (setq ,res (progn ,@body))
           ((setter stream-source) stdin ,orig-source)
           ((setter stream-sink) stdin ,orig-sink))
         ,res)))
) ; end of b2h-aux.em
