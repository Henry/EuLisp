;;;-----------------------------------------------------------------------------
;;; ---                  EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;; Library: string-perls
;;; Description: String processing test module
;;;  Compilation: make
;;;  Run: make run
;;;-----------------------------------------------------------------------------
(defmodule string-perls
  (syntax (macros)
   import (level1))

;;;-----------------------------------------------------------------------------
;;; Split
;;;-----------------------------------------------------------------------------
  (defextern eul_split_string (<string> <string>) ptr)

  (defun split-string (str delim)
    (reverse-list! (eul_split_string str delim)))

  (print (split-string
           "Split this string into words,honestly:no really" " ,;:"))

  0 ; Return success

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
