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
;;; Title: Expose all Modules comprising level-0
;;;  Description:
;;;  Authors: Rainer Rosenmuller, Winfried Heicking, Ingo Mohr
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule level-0
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

(%annotate-function character? interpreter character?)
(%annotate-function as-lowercase interpreter as-lowercase)
(%annotate-function as-uppercase interpreter as-uppercase)
(%annotate-function accumulate interpreter accumulate)
(%annotate-function accumulate1 interpreter accumulate1)
(%annotate-function any? interpreter any?)
(%annotate-function collection? interpreter collection?)
(%annotate-function concatenate interpreter concatenate)
(%annotate-function do interpreter do)
(%annotate-function element interpreter element)
(%annotate-function empty? interpreter empty?)
(%annotate-function fill interpreter fill)
(%annotate-function map interpreter map)
(%annotate-function member interpreter member)
(%annotate-function reverse interpreter reverse)
(%annotate-function sequence? interpreter sequence?)
(%annotate-function size interpreter size)
(%annotate-function eql interpreter eql)
(%annotate-function equal interpreter equal)
(%annotate-function = interpreter =)
(%annotate-function < interpreter <)
(%annotate-function max interpreter max)
(%annotate-function min interpreter min)
(%annotate-function shallow-copy interpreter shallow-copy)
(%annotate-function deep-copy interpreter deep-copy)
(%annotate-function double-float? interpreter double-float?)
(%annotate-function float? interpreter float?)
(%annotate-function ceiling interpreter ceiling)
(%annotate-function floor interpreter floor)
(%annotate-function round interpreter round)
(%annotate-function int? interpreter int?)
(%annotate-function format interpreter format)
(%annotate-function integer? interpreter integer?)
(%annotate-function even? interpreter even?)
(%annotate-function odd? interpreter odd?)
(%annotate-function cons? interpreter cons?)
(%annotate-function atom? interpreter atom?)
(%annotate-function number? interpreter number?)
(%annotate-function + interpreter +)
(%annotate-function - interpreter -)
(%annotate-function * interpreter *)
(%annotate-function / interpreter /)
(%annotate-function % interpreter %)
(%annotate-function gcd interpreter gcd)
(%annotate-function lcm interpreter lcm)
(%annotate-function abs interpreter abs)
(%annotate-function zero? interpreter zero?)
(%annotate-function negate interpreter negate)
(%annotate-function signum interpreter signum)
(%annotate-function positive? interpreter positive?)
(%annotate-function negative? interpreter negative?)
(%annotate-function stream? interpreter stream?)
(%annotate-function close interpreter close)
(%annotate-function print interpreter print)
(%annotate-function string? interpreter string?)
(%annotate-function binary< interpreter binary<)
(%annotate-function make-symbol interpreter make-symbol)
(%annotate-function symbol? interpreter symbol?)
(%annotate-function gensym interpreter gensym)
(%annotate-function symbol-name interpreter symbol-name)
(%annotate-function symbol-exists? interpreter symbol-exists?)
(%annotate-function table? interpreter table?)
(%annotate-function clear-table interpreter clear-table)

;;;-----------------------------------------------------------------------------
)  ;; End of module level-0
;;;-----------------------------------------------------------------------------
