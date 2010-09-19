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
;;; Title: Compiler Interface to Basic Module %tail
;;;  Description:
;;    the definition functions for tail data types
;;;  Documentation:
;;    see in the APPLY-paper machine description
;;;  Authors: Winfried Heicking
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

#module tail-module
(import (accessors
         level-0
         LZS
         MZS
         machine-description; for the whole machine-description
         debugging
         el2lzs
         expand-literal
         (only (*basic-system*)
               predicates)
         (only (make-instance
                 vector
                 append
                 delete-if-not
                 last butlast)
               common-lisp))
 syntax (level-0
         apply-standard
         debugging
         machine-description)
 export (;;accesses for data types
         ;;!!!the following (and other symbols,
         ;;see define-tail-sys-functions and
         ;;define-tail-aux-sys-functions below)
         ;;are exported automatically by define-tail!!!
         ;;%ref %extract %select %preselect %view
         ;;%size-of-instance %size-as-component %pointer-of

         %byte-integer
         %half-integer
         %word-integer
         %unsigned-byte-integer
         %signed-byte-integer
         %unsigned-half-integer
         %signed-half-integer
         %unsigned-word-integer
         %signed-word-integer
         %direct-integer
         %signed-direct-integer
         %single-float
         %double-float
         %extended-float
         %void
         %direct
         %function
         %sighandler
         %string
         %jmpbuf
         %pjmpbuf

         <%byte-integer>
         <%half-integer>
         <%word-integer>
         <%unsigned-byte-integer>
         <%signed-byte-integer>
         <%unsigned-half-integer>
         <%signed-half-integer>
         <%unsigned-word-integer>
         <%signed-word-integer>
         <%direct-integer>
         <%signed-direct-integer>
         <%single-float>
         <%double-float>
         <%extended-float>
         <%void>
         <%direct>
         <%function>
         <%sighandler>
         <%string>
         <%jmpbuf>
         <%pjmpbuf>

         %goto %label
         %make-proc-epilog
         %make-proc-prolog
         %make-data-segment
         %make-code-segment
         %make-commentline %call
         %jmpl %move-block %move
         %le %ge %lt %gt
         %neq %eq %cxtod %cxtos
         %cxtoi %cdtox %cdtos
         %cdtoi %cstox %cstod
         %cstoi %citox %citod
         %citos %zero-extend
         %sign-extend %sqrt %abs
         %rotater %rotatel %ashiftr
         %lshiftr %ashiftl %lshiftl
         %xor %or %and %not
         %rem %div %mult %neg
         %minus %plus
         %setjmp %longjmp

         <%goto> <%label>
         <%make-proc-epilog>
         <%make-proc-prolog>
         <%make-data-segment>
         <%make-code-segment>
         <%make-commentline> <%call>
         <%jmpl> <%move-block> <%move>
         <%le> <%ge> <%lt> <%gt>
         <%neq> <%eq> <%cxtod> <%cxtos>
         <%cxtoi> <%cdtox> <%cdtos>
         <%cdtoi> <%cstox> <%cstod>
         <%cstoi> <%citox> <%citod>
         <%citos> <%zero-extend>
         <%sign-extend> <%sqrt> <%abs>
         <%rotater> <%rotatel> <%ashiftr>
         <%lshiftr> <%ashiftl> <%lshiftl>
         <%xor> <%or> <%and> <%not>
         <%rem> <%div> <%mult> <%neg>
         <%minus> <%plus>
         <%setjmp> <%longjmp>
         )
 export (reset-%tail
         add-toplevel-forms-for-tail-module)
 expose ((only ($tail-module)
               el2lzs)) ; export without import
 )

;;;in define-tail mit export export (like the real data types)
;;;to export from the tail module application
;;;but for this module define-tail exports always

(define-tail-sys-functions
  %ref
  %extract
  %select
  %preselect
  %view
  %size-of-instance
  %size-as-component
  %cast
  %funcall
  ;;??%setf

  ;; %setjmp
  ;; %longjmp

  ;; now export the following from tail!!
  %pointer-of-variable
  %pointer-of-function
  %pointer-of-extract
  %pointer-of-select

  %setf-variable
  %setf-ref
  %setf-extract
  %setf-select
  %setf-view
  %setf-cast)

;;;-----------------------------------------------------------------------------
;;; resetting module %tail
;;;-----------------------------------------------------------------------------
(defun reset-%tail ()
  (setf (?fun-list $tail-module)
        (delete-if-not #'special-sys-fun? (?fun-list $tail-module)))
  ;;?var-list is set by set-init-fun-for-tail-module
  (setf (?named-const-list $tail-module) ())
  (setf (?sym-list $tail-module) ())
  (setf (?syntax-exports $tail-module) ())
  (setf (?syntax-env $tail-module) ())
  (setf (?exports $tail-module)
        (append (?class-def-list $tail-module)
                (?fun-list $tail-module)))
  (setf (?lex-env $tail-module) (?exports $tail-module))
  (set-init-fun-for-tail-module))

(defconstant $unsigned-0
  (make-literal-instance %unsigned-word-integer '(0)))
(defconstant $unsigned-1
  (make-literal-instance %unsigned-word-integer '(1)))

(defun set-init-fun-for-tail-module ()
  ;;NOTE SIDE EFFECT:
  ;;sets the variable list of %tail
  (if *basic-system*
      (progn
        (setf (?var-list $tail-module) ())
        (setf (?toplevel-forms $tail-module) ()))
    (let ((initflag (make-instance <global-static>
                      :identifier ^basic-initialization-done
                      :module $tail-module
                      :type %unsigned-word-integer
                      :initial-value $unsigned-0)))
      (setf (?var-list $tail-module) (list initflag))
      (setf (?toplevel-forms $tail-module)
            (make-instance <global-fun>
              :range-and-domain (vector %unsigned-word-integer)
              :params (make-instance <params>)
              :body
              (make-instance <if-form>
                :pred
                (make-instance <app>
                  :function %eq
                  :arg-list (list (make-instance <var-ref> :var initflag)
                                  $unsigned-0))
                :then
                (make-instance <progn-form>
                  :form-list (list (make-instance <setq-form>
                                     :location (make-instance <var-ref>
                                                 :var initflag)
                                     :form $unsigned-1)))
                :else
                $unsigned-1)
              :exported ()
              :identifier "basic-initialization"
              :module $tail-module)))))

(defun add-toplevel-forms-for-tail-module (forms)
  (unless *basic-system*
          (let ((progn (?then (?body (?toplevel-forms $tail-module)))))
            (setf (?form-list progn)
                  (append (butlast (?form-list progn))
                          forms
                          (last (?form-list progn)))))))

;;;-----------------------------------------------------------------------------
#module-end
;;;-----------------------------------------------------------------------------
