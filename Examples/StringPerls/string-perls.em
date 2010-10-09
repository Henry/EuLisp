;;; Copyright 2010 Henry G. Weller
;;;-----------------------------------------------------------------------------
;;  This file is part of
;;; ---                         EuLisp System 'Youtoo'
;;;-----------------------------------------------------------------------------
;;
;;  Youtoo is free software: you can redistribute it and/or modify it under the
;;  terms of the GNU General Public License version 2 as published by the Free
;;  Software Foundation.
;;
;;  Youtoo is distributed in the hope that it will be useful, but WITHOUT ANY
;;  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;  details.
;;
;;  You should have received a copy of the GNU General Public License along with
;;  this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;-----------------------------------------------------------------------------
;;; Title: String processing test module
;;;  Authors: Henry G. Weller
;;;  Maintainer: Henry G. Weller
;;;  Library: string-perls
;;;  Compilation:
;;    make
;;;  Run:
;;    make run
;;;-----------------------------------------------------------------------------

(defmodule string-perls
  (syntax (syntax-0)
   import (level-0))

;;;-----------------------------------------------------------------------------
;;; Split
;;;-----------------------------------------------------------------------------
(defextern eul_split_string (<string> <string>) ptr)
(defun split-string (str delim)
  (reverse-list (eul_split_string str delim)))

(deflocal str "Split this string into words,honestly:no really")
(deflocal delim " ,;:")
(format "Splitting \"~a\" at delimiters \"~a\"~%" str delim)
(print (split-string str delim) nl)

;;;-----------------------------------------------------------------------------
;;; Match
;;;-----------------------------------------------------------------------------
(defextern eul_match_string (<string> <string>) ptr)
(defun match-string (str regex)
  (eul_match_string str regex))

(setq str "From: an.email@address.org\\n")
(deflocal regex "^From: ([^@]+)@([^\\n]+)")
(print (match-string str regex) nl)

;;;-----------------------------------------------------------------------------
;;; Match-all
;;;-----------------------------------------------------------------------------
(defextern eul_match_all_string (<string> <string>) ptr)
(defun match-all-string (str regex)
  (eul_match_all_string str regex))

(setq str "a b a b a ")
(setq regex " a ")
(print (match-all-string str regex) nl)

;;;-----------------------------------------------------------------------------
;;; Return Success
;;;-----------------------------------------------------------------------------

0

;;;-----------------------------------------------------------------------------
)  ;; End of module string-perls
;;;-----------------------------------------------------------------------------
