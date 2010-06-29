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

  (deflocal str "Split this string into words,honestly:no really")
  (deflocal delim " ,;:")
  (format "Splitting \"~a\" at delimiters \"~a\"~%" str delim)
  (print (split-string str delim))

;;;-----------------------------------------------------------------------------
;;; Match
;;;-----------------------------------------------------------------------------
  (defextern eul_match_string (<string> <string>) ptr)
  (defun match-string (str regex)
    (eul_match_string str regex))

  (setq str "From: an.email@address.org\\n")
  (deflocal regex "^From: ([^@]+)@([^\\n]+)")
  (print (match-string str regex))

;;;-----------------------------------------------------------------------------
;;; Match-all
;;;-----------------------------------------------------------------------------
  (defextern eul_match_all_string (<string> <string>) ptr)
  (defun match-all-string (str regex)
    (eul_match_all_string str regex))

  (setq str "a b a b a ")
  (setq regex " a ")
  (print (match-all-string str regex))

;;;-----------------------------------------------------------------------------
;;; Return Success
;;;-----------------------------------------------------------------------------

  0

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
