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
;;;  Title: Mashine-description of Register, Location and Parameter-passing
;;;  Description:
;;    the definition functions for tail data types
;;;  Documentation:
;;    see in the APPLY-paper machine description
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: E. Ulrich Kriegel, Ingo Mohr, Rainer Rosenmuller, Winfried Heicking
;;;-----------------------------------------------------------------------------

#module machine-description

(import (level-0
         representation
         lzs
         mzs
         whc-basic-data-types
         ;;rr-md-read
         accessors
         el2lzs
         expand-literal
         (rename ((error cl:error))
                 (only (error)
                       common-lisp)))
 syntax (level-1
         apply-standard
         whc-basic-data-types
         ;;rr-md-read
         lzs-modules)
 export (<basic-class-def>
         <%aux-type>
         <%representation>
         <%machine-type>
         <%struct>
         <%vector>
         <%pointer>
         <%pointer-to-struct>
         <%pointer-to-vector>
         <%pointer-to-void>
         <%direct>
         <%function>
         <%sighandler>
         ?alignment)
 expose ((only
          (define-tail-sys-functions
            define-tail-aux-sys-functions)
          whc-basic-data-types)))


#+ :word32 (define-machine-data-types
             (%byte
              :byte-length 1
              :alignment 1
              )

             (%half-word
              :byte-length 2
              :alignment 2
              )

             (%word
              :byte-length 4
              :alignment 4
              )

             (%double-word
              :byte-length 8
              :alignment 8
              )

             (%quad-word
              :byte-length 16
              :alignment 8
              )
             )


#+ :word64 (define-machine-data-types
             (%byte
              :byte-length 1
              :alignment 1)

             (%half-word
              :byte-length 4
              :alignment 4)

             (%word
              :byte-length 8
              :alignment 8)

             (%double-word
              :byte-length 16
              :alignment 8)
             )


(define-basic-data-types

  (%signed-word-integer
   :machine-type %word)

  (%unsigned-word-integer
   :machine-type %word)

  (%signed-half-integer
   :machine-type %half-word)

  (%unsigned-half-integer
   :machine-type %half-word)

  (%signed-byte-integer
   :machine-type %byte)

  (%unsigned-byte-integer
   :machine-type %byte)

  #+ :word32 (%single-float
              :machine-type %word)

  #+ :word64 (%single-float
              :machine-type %half-word)

  #+ :word32 (%double-float
              :machine-type %double-word)

  #+ :word64 (%double-float
              :machine-type %word)

  #+ :word32 (%extended-float
              :machine-type %quad-word)

  #+ :word64 (%extended-float
              :machine-type %double-word)

  (%void
   ;;!!!to give any representation for void !!!
   :machine-type %word)

  (%function
   :machine-type %word)

  (%sighandler
   :machine-type %word)

  (%string
   :machine-type %byte)

  (%jmpbuf
   ;; this isn't true, but the length should not be used
   :machine-type %word)

  (%pjmpbuf
   :machine-type %word))


(define-special-sys-funs
  (%setjmp :arg-num 1)

  (%longjmp :arg-num 2)

  (%plus :arg-num 2)

  (%minus :arg-num 2)

  (%neg :arg-num 1)

  (%mult :arg-num 2)

  (%div :arg-num 2)

  (%abs :arg-num 1)

  (%rem :arg-num 2)

  (%not :arg-num 1)

  (%and :arg-num 2)

  (%or :arg-num 2)

  (%xor :arg-num 2)

  (%lshiftl :arg-num 2)

  (%ashiftl :arg-num 2)

  (%lshiftr :arg-num 2)

  (%ashiftr :arg-num 2)

  (%sign-extend :arg-num 1)

  (%zero-extend :arg-num 1)

  (%citos :arg-num 1)
  (%citod :arg-num 1)
  (%citox :arg-num 1)

  (%cstoi :arg-num 1)
  (%cstod :arg-num 1)
  (%cstox :arg-num 1)

  (%cdtoi :arg-num 1)
  (%cdtos :arg-num 1)
  (%cdtox :arg-num 1)

  (%cxtoi :arg-num 1)
  (%cxtos :arg-num 1)
  (%cxtod :arg-num 1)

  (%eq :arg-num 2)


  (%neq :arg-num 2)


  (%gt :arg-num 2)

  (%lt :arg-num 2)

  (%ge :arg-num 2)
  (%le :arg-num 2)
  )

#module-end
