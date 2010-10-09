;;; Copyright 1997 A. Kind & University of Bath
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
;;; Title: class initialization
;;;  Library: telos (The EuLisp Object System -- TELOS)
;;;  Authors: Russel Bradford, Andreas Kind
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule mop-init
  (syntax (_boot0)
   import (boot
           mop-prim
           mop-class)
   export (init-class
           compute-class-codes))

;;;-----------------------------------------------------------------------------
;;; Initialize classes
;;;-----------------------------------------------------------------------------
(defun init-class (cl name isize supers subs keys cpl absp)
  ((setter class-name) cl name)
  ((setter class-instance-length) cl isize)
  ((setter class-direct-superclasses) cl supers)
  ((setter class-direct-subclasses) cl subs)
  ((setter class-slots) cl ())
  ((setter class-keywords) cl keys)
  ((setter class-precedence-list) cl (cons cl cpl))
  ((setter class-abstract?) cl absp))
(declare-inline init-class)

(init-class <object> 'object object-size
            ()                         ; supers
            (list <class> <method>     ; subs
                  <slot>
                  <function> <name>)
            ()                         ; keywords
            ()                         ; cpl
            t)                         ; absp

(init-class <metaclass> 'metaclass class-size
            (list <simple-class>)      ; supers
            ()                         ; subs
            class-keywordz             ; keywords
            (list <simple-class>
                  <object>)            ; cpl
            t)                         ; absp

(init-class <class> 'class class-size
            (list <object>)            ; supers
            (list <simple-class>       ; subs
                  <function-class>)
            class-keywordz             ; keywords
            (list <object>)            ; cpl
            t)                         ; absp

(init-class <simple-class> 'simple-class class-size
            (list <class>)             ; supers
            (list <metaclass>)         ; subs
            class-keywordz             ; keywords
            (list <class> <object>)    ; cpl
            ())                        ; absp

(init-class <function-class> 'function-class class-size
            (list <class>)             ; supers
            ()                         ; subs
            class-keywordz             ; keywords
            (list <class> <object>)    ; cpl
            ())                        ; absp

(init-class <function> 'function function-size
            (list <object>)            ; supers
            (list <simple-function>
                  <generic-function>)  ; subs
            ()                         ; keywords
            (list <object>)            ; cpl
            t)                         ; absp

(init-class <simple-function> 'simple-function sf-size
            (list <function>)          ; supers
            ()                         ; subs
            ()                         ; keywords
            (list <function> <object>) ; cpl
            ())                        ; absp

(init-class <generic-function> 'generic-function gf-size
            (list <function>)          ; supers
            (list
             <simple-generic-function>); subs
            gf-keywords                ; keywords
            (list <function> <object>) ; cpl
            t)                         ; absp

(init-class <simple-generic-function> 'simple-generic-function gf-size
            (list <generic-function>)  ; supers
            ()                         ; subs
            gf-keywords                ; keywords
            (list <generic-function>
                  <function> <object>) ; cpl
            ())                        ; absp

(init-class <method> 'method method-size
            (list <object>)            ; supers
            (list <simple-method>)     ; subs
            method-keywords            ; keywords
            (list <object>)            ; cpl
            t)                         ; absp

(init-class <simple-method> 'simple-method method-size
            (list <method>)            ; supers
            ()                         ; subs
            method-keywords            ; keywords
            (list <method> <object>)   ; cpl
            ())                        ; absp

(init-class <slot> 'slot sd-size
            (list <object>)            ; supers
            (list <local-slot>)        ; subs
            sd-keywords                ; keywords
            (list <object>)            ; cpl
            t)                         ; absp

(init-class <local-slot> 'local-slot lsd-size
            (list <slot>)              ; supers
            ()                         ; subs
            lsd-keywords               ; keywords
            (list <slot> <object>)     ; cpl
            ())                        ; absp

(init-class <name> 'name name-size
            (list <object>)            ; supers
            (list <symbol> <keyword>)  ; subs
            name-keywords              ; keywords
            (list <object>)            ; cpl
            t)                         ; absp

(init-class <symbol> 'symbol name-size
            (list <name>)              ; supers
            ()                         ; subs
            name-keywords              ; keywords
            (list <name> <object>)     ; cpl
            ())                        ; absp

(init-class <keyword> 'keyword name-size
            (list <name>)              ; supers
            ()                         ; subs
            name-keywords              ; keywords
            (list <name> <object>)     ; cpl
            ())                        ; absp

(init-class <list> 'list object-size
            (list <object>)            ; supers
            (list <cons> <null>)       ; subs
            ()                         ; keywords
            (list <object>)            ; cpl
            t)                         ; absp

(init-class <null> 'null object-size
            (list <list>)              ; supers
            ()                         ; subs
            ()                         ; keywords
            (list <list> <object>)     ; cpl
            ())                        ; absp

(init-class <cons> 'cons cons-size
            (list <list>)              ; supers
            ()                         ; subs
            cons-keywords              ; keywords
            (list <list> <object>)     ; cpl
            ())                        ; absp

;;;-----------------------------------------------------------------------------
;;; Compute class codes
;;;-----------------------------------------------------------------------------
(defun compute-class-codes ()
  (labels
   ((loop (cl min)
          (let ((code ((setter class-code) cl (cons (car min) ()))))
            (do1-list (lambda (subcl)
                        (loop subcl min))
                      (class-direct-subclasses cl))
            ((setter cdr) code (car min))
            ((setter car) min (+ (car min) 1)))))
   (loop <object> (cons 0 ()))))
(compute-class-codes)

;;;-----------------------------------------------------------------------------
)  ;; End of module mop-init
;;;-----------------------------------------------------------------------------
