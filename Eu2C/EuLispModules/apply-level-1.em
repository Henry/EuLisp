;;; Copyright 1994-2010 Fraunhofer ISST
;;; Copyright 2010 Henry G. Weller
;;;-----------------------------------------------------------------------------
;;  This file is part of
;;; ---                           EuLisp System 'Eu2C'
;;;-----------------------------------------------------------------------------
;;
;;  Eu2C is free software: you can redistribute it and/or modify it under the
;;  terms of the GNU General Public License version 2 as published by the Free
;;  Software Foundation.
;;
;;  Eu2C is distributed in the hope that it will be useful, but WITHOUT ANY
;;  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;  details.
;;
;;  You should have received a copy of the GNU General Public License along with
;;  this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;-----------------------------------------------------------------------------
;;;  Title: The most basic dynamically loaded things for the compiler
;;;  Description:
;;    This module contains all basic stuff for the compiler which is not hard-wired
;;    but is needed for
;;    - defining classes
;;    - ???
;;    This module is loaded explicitely by the compiler before any other module is
;;    required. The following things are available for this module and the modules it
;;    uses:
;;    - definition of simple functions
;;    - constant and variable definitions
;;    - definition of external functions, constants and variables
;;    - predefined classes of TAIL and standard literals
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Ingo Mohr
;;;-----------------------------------------------------------------------------


(defmodule apply-level-1

  (import (%tail
           mm-interface)
   syntax (%tail)

   expose ((rename ((%class <class>)
                    (%object <object>)
                    (%tail-class <tail-class>)
                    (%abstract-class <abstract-class>)
                    )
                   %tail)
           pointer-to-void)

   )

)
