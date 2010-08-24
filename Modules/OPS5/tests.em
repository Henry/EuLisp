;;;-----------------------------------------------------------------------------
;;;                 OPS5 for EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;; File   : tests.em
;;; Date   : 17 Jul 1995
;;; Author : Tracy Gardner (tag@maths.bath.ac.uk)
;;; Description: Functions for handling OPS5 tests.
;;;-----------------------------------------------------------------------------
(defmodule tests
  (syntax (macros macros-tag)
   import (level1 basic))

(defun test-attrib (x) (car x))
(defun test-pred   (x) (cadr x))

(defun test-value  (x) (caddr x))
(defun test-var    (x) (caddr x))

(export test-attrib test-pred test-value test-var)

;;;-----------------------------------------------------------------------------
)  ;; end of module
;;;-----------------------------------------------------------------------------
