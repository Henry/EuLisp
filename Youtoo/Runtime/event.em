;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: level1
;;;  Authors: Andreas Kind, Julian Padget
;;; Description: events not yet implemented
;;;-----------------------------------------------------------------------------
(defmodule event
  (syntax (_telos0)
   import (telos)
   export (wait ticks-per-second))

;;;-----------------------------------------------------------------------------
;;; Wait (timeout should be <integer>)
;;;-----------------------------------------------------------------------------
  (defgeneric wait (x timeout))

  (defmethod wait ((x <object>) (timeout <object>))
    (error "wait not yet implemented"))

  (defextern eul_ticks_per_second () <int>)
  (defconstant ticks-per-second (eul_ticks_per_second))

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
