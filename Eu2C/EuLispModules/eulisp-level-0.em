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
;;;  Title: Expose all Modules comprising eulisp-level-0
;;;  Description:
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Rainer Rosenmuller, Winfried Heicking, Ingo Mohr
;;;-----------------------------------------------------------------------------

(defmodule eulisp-level-0
  (syntax (syntax-0)
   import (character
           collection
           compare
           condition
           convert
           copy
           double-float
           elementary-functions
           ;;***HGW event ; For thread support
           int
           formatted-io
           function
           list
           ;;***HGW lock ; For thread support
           ;;null ;contained in list
           number
           integer
           float
           object-0
           stream
           string
           symbol
           syntax-0
           table
           ;;***HGW thread ; For thread support
           vector)

   expose (character
           collection
           compare
           condition
           convert
           copy
           double-float
           elementary-functions
           ;;***HGW event ; For thread support
           int
           formatted-io
           function
           list
           ;;***HGW lock ; For thread support
           ;;null ;contained in list
           number
           integer
           float
           object-0
           stream
           string
           symbol
           syntax-0
           table
           ;;***HGW thread ; For thread support
           vector))

(%annotate-function characterp interpreter characterp)
(%annotate-function as-lowercase interpreter as-lowercase)
(%annotate-function as-uppercase interpreter as-uppercase)
(%annotate-function accumulate interpreter accumulate)
(%annotate-function accumulate1 interpreter accumulate1)
(%annotate-function anyp interpreter anyp)
(%annotate-function collectionp interpreter collectionp)
(%annotate-function concatenate interpreter concatenate)
(%annotate-function do interpreter do)
(%annotate-function element interpreter element)
(%annotate-function emptyp interpreter emptyp)
(%annotate-function fill interpreter fill)
(%annotate-function map interpreter map)
(%annotate-function member interpreter member)
(%annotate-function reverse interpreter reverse)
(%annotate-function sequencep interpreter sequencep)
(%annotate-function size interpreter size)
(%annotate-function eql interpreter eql)
(%annotate-function equal interpreter equal)
(%annotate-function = interpreter =)
(%annotate-function < interpreter <)
(%annotate-function max interpreter max)
(%annotate-function min interpreter min)
;;(%annotate-function convert interpreter convert);convert is macro in collection
(%annotate-function shallow-copy interpreter shallow-copy)
(%annotate-function deep-copy interpreter deep-copy)
(%annotate-function double-float-p interpreter double-float-p)
(%annotate-function floatp interpreter floatp)
(%annotate-function ceiling interpreter ceiling)
(%annotate-function floor interpreter floor)
(%annotate-function round interpreter round)
(%annotate-function int-p interpreter int-p)
(%annotate-function format interpreter format)
(%annotate-function integerp interpreter integerp)
(%annotate-function evenp interpreter evenp)
(%annotate-function oddp interpreter oddp)
(%annotate-function consp interpreter consp)
(%annotate-function atom? interpreter atom?)
(%annotate-function numberp interpreter numberp)
(%annotate-function + interpreter +)
(%annotate-function - interpreter -)
(%annotate-function * interpreter *)
(%annotate-function / interpreter /)
(%annotate-function % interpreter %)
(%annotate-function gcd interpreter gcd)
(%annotate-function lcm interpreter lcm)
(%annotate-function abs interpreter abs)
(%annotate-function zerop interpreter zerop)
(%annotate-function negate interpreter negate)
(%annotate-function signum interpreter signum)
(%annotate-function positivep interpreter positivep)
(%annotate-function negativep interpreter negativep)
(%annotate-function streamp interpreter streamp)
(%annotate-function close interpreter close)
(%annotate-function print interpreter print)
(%annotate-function stringp interpreter stringp)
(%annotate-function binary< interpreter binary<)
(%annotate-function make-symbol interpreter make-symbol)
(%annotate-function symbol? interpreter symbol?)
(%annotate-function gensym interpreter gensym)
(%annotate-function symbol-name interpreter symbol-name)
(%annotate-function symbol-exists-p interpreter symbol-exists-p)
(%annotate-function tablep interpreter tablep)
(%annotate-function clear-table interpreter clear-table)

;;;-----------------------------------------------------------------------------
)  ;;  eof eulisp-level-0
;;;-----------------------------------------------------------------------------
