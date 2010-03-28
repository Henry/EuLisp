;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;; -----------------------------------------------------------------------
;;;                     EuLisp System 'youtoo'
;;; -----------------------------------------------------------------------
;;;  Library: level1 (EuLisp Language Level1 Implementation)
;;;  Authors: Andreas Kind, Julian Padget
;;;  Description: conversion
;;; -----------------------------------------------------------------------
(defmodule convert
  (syntax (_telos0)
   import (telos condition)
   export (converter convert))
;;; --------------------------------------------------------------------
;;; Converter function
;;; --------------------------------------------------------------------
  (defun convert (x cl) ((converter cl) x))
)  ; end of module
