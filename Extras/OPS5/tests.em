;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;;                 OPS5 for EuLisp System 'youtoo'
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; File   : tests.em
;;; Date   : 17 Jul 1995
;;; Author : Tracy Gardner (tag@maths.bath.ac.uk)
;;; Desc.  : Functions for handling OPS5 tests.
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(defmodule tests
;;; Uncomment this block to run under youtoo ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BEGIN_YOUTOO
(syntax (macros macros-tag) 
import (level1 basic)) 
;;; END_YOUTOO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Uncomment this block to run under euscheme ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BEGIN_EUSCHEME
;;  (import (level0))
;;; END_EUSCHEME
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A test has the format (attrib pred value) 
;;                    or (attrib pred (value1 value2 ...))
;;                    or (attrib pred var)
;; depending on the type of the test
; (defconstant test-attrib car)
  (defun test-attrib (x) (car x))
  (defun test-pred   (x) (cadr x))
  (defun test-value  (x) (caddr x))
  (defun test-var    (x) (caddr x))
 (export test-attrib test-pred test-value test-var)
) ;; module: tests
