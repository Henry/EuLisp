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
;;; Title:
;;;  Description:
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Horst Friedrich
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------
(defmodule function

  (import (apply-level-1
           basic-list ;
           function-i
           )
   syntax (apply-level-1
           basic-syntax)
   export (<function>
           make-function
           function-closure-vars
           function-address
           function-arg-descr
           function-name
           apply
           ;;<function-class>
           setter
           %apply3       ;; for standard-generic-function
           error-no-setter-defined-handler ;variable
           error-no-setter-defined      ;function, for standard-generic-function
           )
   )

;;;-----------------------------------------------------------------------------
;;; not defined functions
;;;-----------------------------------------------------------------------------
(%define-function (too-many-arguments <null>)
  ((foo <function>)
   (arg-descr %signed-word-integer)
   (arg-counter %signed-word-integer)
   (args <cons>))
  ;; a dummy definition
  (if (%eq arg-descr #%i0)
      (too-many-arguments foo arg-descr arg-counter args)
    ())
  ;;  (error <range-condition> foo arg-descr arg-counter args)
  )

(%define-function (wrong-argument-number <null>)
  ((foo <function>)
   (arg-descr %signed-word-integer)
   (arg-counter %signed-word-integer)
   (args <list>))
  ;; a dummy definition
  (if (%eq arg-descr #%i0)
      (wrong-argument-number foo arg-descr arg-counter args)
    ())
  ;;(error <range-condition> foo arg-descr arg-counter args))
  )

;;;-----------------------------------------------------------------------------
;;; needed at EuLisp level 1
;;;-----------------------------------------------------------------------------
;; (%define-metaclass (<function-class> <class>) ;; <class> ist die Metaklasse
;;                    <object>    ;; <object> ist die
;;        ;; Superklasse ; ;
;;                    ()          ;; no new slots
;;        ;; representation pointer-to-void ; ;
;;                    )

(%define-literal-expansion function
  `(%literal ,<function>
             closure-vars ()
             arg-descr (%literal ,%signed-word-integer ,argument-descriptor)
             address ,function-pointer
             name (%literal ,%string () ,name)
             setter ,(if setter setter error-no-setter-defined)))

;;;-----------------------------------------------------------------------------
;;; stuff for setter
;;;-----------------------------------------------------------------------------

(%define-variable error-no-setter-defined-handler %function
  (%function error-no-setter-defined-default-handler))

(defun error-no-setter-defined (reader-function)
  (%funcall error-no-setter-defined-handler reader-function))

(defun error-no-setter-defined-default-handler (reader-function)
  (%cast <object> #%i0))

;;-------------------------------------------------------------------
;; %apply1, %apply2, %apply3, %apply4, ... used the apply-compiler to
;; translate an apply-call.
;; Example: (apply foo x) -> (apply1 foo x)
;;          (apply foo a b c d) -> (apply4 foo a b c d)
;;-------------------------------------------------------------------
(defun apply  (foo  . args)
  (%apply1 foo args))

(%define-function (%apply1 <object>) ((foo <function>) (args <list>))
  (if (null? args) (%funcall0 foo)
    (%let ((arg-descr %signed-word-integer (function-arg-descr foo)))
          (if (%eq arg-descr #%i-1)
              (let ((cl-vars (function-closure-vars foo)))
                (if cl-vars (%funcall (function-address foo) args cl-vars)
                  (%funcall (function-address foo) args)))
            (if (%eq arg-descr #%i0)
                (wrong-argument-number foo arg-descr
                                       (%list-size args) args)
              (apply>=1 foo arg-descr (car args) (cdr args))))))
  )


(%define-function (%apply2 <object>)
  ((foo <function>) (arg1 <object>) (args <list>))

  (if (null? args) (%funcall1 foo arg1)
    (%let ((arg-descr %signed-word-integer (function-arg-descr foo)))
          (if (%eq arg-descr #%i-2)
              (let ((cl-vars (function-closure-vars foo)))
                (if cl-vars (%funcall (function-address foo) arg1 args cl-vars)
                  (%funcall (function-address foo) arg1 args)))
            (if (%gt arg-descr #%i-2)
                (if (%lt arg-descr #%i2)
                    (apply>-2 foo arg-descr (cons arg1 args))
                  (apply>=2 foo arg-descr arg1 (car args) (cdr args)))
              (apply>=2 foo arg-descr arg1 (car args) (cdr args))))))
  )

(%define-function (%apply3 <object>)
  ((foo <function>) (arg1 <object>) (arg2 <object>)
   (args <list>))

  (if (null? args) (%funcall2 foo arg1 arg2)
    (%let ((arg-descr %signed-word-integer (function-arg-descr foo)))
          (if (%eq arg-descr #%i-3)
              (let ((cl-vars (function-closure-vars foo)))
                (if cl-vars (%funcall (function-address foo) arg1 arg2 args cl-vars)
                  (%funcall (function-address foo) arg1 arg2 args)))
            (if (%gt arg-descr #%i-3)
                (if (%lt arg-descr #%i3)
                    (apply>-3 foo arg-descr arg1 (cons arg2 args))
                  (apply>=3 foo arg-descr arg1 arg2 (car args) (cdr args)))
              (apply>=3 foo arg-descr arg1 arg2 (car args) (cdr args))))))
  )

(%define-function (%apply4 <object>)
  ((foo <function>) (arg1 <object>) (arg2 <object>)
   (arg3 <object>)   (args <list>))

  (if (null? args) (%funcall3 foo arg1 arg2 arg3)
    (%let ((arg-descr %signed-word-integer (function-arg-descr foo)))
          (if (%eq arg-descr #%i-4)
              (let ((cl-vars (function-closure-vars foo)))
                (if cl-vars (%funcall (function-address foo) arg1 arg2 arg3 args cl-vars)
                  (%funcall (function-address foo) arg1 arg2 arg3 args)))
            (if (%gt arg-descr #%i-4)
                (if (%lt arg-descr #%i4)
                    (apply>-4 foo arg-descr arg1 arg2 (cons arg3 args))
                  (apply>=4 foo arg-descr arg1 arg2 arg3
                            (car args) (cdr args)))
              (apply>=4 foo arg-descr arg1 arg2 arg3
                        (car args) (cdr args))))))
  )

(%define-function (%apply5 <object>)
  ((foo <function>) (arg1 <object>) (arg2 <object>)
   (arg3 <object>)   (arg4 <object>) (args <list>))

  (if (null? args) (%funcall4 foo arg1 arg2 arg3 arg4)
    (%let ((arg-descr %signed-word-integer (function-arg-descr foo)))
          (if (%eq arg-descr #%i-5)
              (let ((cl-vars (function-closure-vars foo)))
                (if cl-vars (%funcall (function-address foo)
                                      arg1 arg2 arg3 arg4 args cl-vars)
                  (%funcall (function-address foo)
                            arg1 arg2 arg3 arg4 args)))
            (if (%gt arg-descr #%i-5)
                (if (%lt arg-descr #%i5)
                    (apply>-5 foo arg-descr arg1 arg2 arg3 (cons arg4 args))
                  (apply>=5 foo arg-descr arg1 arg2 arg3 arg4
                            (car args) (cdr args)))
              (apply>=5 foo arg-descr arg1 arg2 arg3 arg4
                        (car args) (cdr args))))))
  )

(%define-function (%apply6 <object>)
  ((foo <function>) (arg1 <object>) (arg2 <object>)
   (arg3 <object>)   (arg4 <object>) (arg5 <object>)
   (args <list>))

  (if (null? args) (%funcall5 foo arg1 arg2 arg3 arg4 arg5)
    (%let ((arg-descr %signed-word-integer (function-arg-descr foo)))
          (if (%eq arg-descr #%i-6)
              (let ((cl-vars (function-closure-vars foo)))
                (if cl-vars (%funcall (function-address foo)
                                      arg1 arg2 arg3 arg4 arg5 args cl-vars)
                  (%funcall (function-address foo)
                            arg1 arg2 arg3 arg4 arg5 args)))
            (if (%gt arg-descr #%i-6)
                (if (%lt arg-descr #%i6)
                    (apply>-6 foo arg-descr
                              arg1 arg2 arg3 arg4 (cons arg5 args))
                  (apply>=6 foo arg-descr arg1 arg2 arg3 arg4 arg5
                            (car args) (cdr args)))
              (apply>=6 foo arg-descr arg1 arg2 arg3 arg4 arg5
                        (car args) (cdr args))))))
  )

(%define-function (%apply7 <object>)
  ((foo <function>) (arg1 <object>) (arg2 <object>)
   (arg3 <object>)   (arg4 <object>) (arg5 <object>)
   (arg6 <object>) (args <list>))

  (if (null? args) (%funcall6 foo arg1 arg2 arg3 arg4 arg5 arg6)
    (%let ((arg-descr %signed-word-integer (function-arg-descr foo)))
          (if (%eq arg-descr #%i-7)
              (let ((cl-vars (function-closure-vars foo)))
                (if cl-vars (%funcall (function-address foo)
                                      arg1 arg2 arg3 arg4 arg5 arg6 args cl-vars)
                  (%funcall (function-address foo)
                            arg1 arg2 arg3 arg4 arg5 arg6 args)))
            (if (%gt arg-descr #%i-7)
                (if (%lt arg-descr #%i7)
                    (apply>-7 foo arg-descr
                              arg1 arg2 arg3 arg4 arg5 (cons arg6 args))
                  (apply>=7 foo arg-descr arg1 arg2 arg3 arg4 arg5 arg6
                            (car args) (cdr args)))
              (apply>=7 foo arg-descr arg1 arg2 arg3 arg4 arg5 arg6
                        (car args) (cdr args))))))
  )

(%define-function (%apply8 <object>)
  ((foo <function>) (arg1 <object>) (arg2 <object>)
   (arg3 <object>)   (arg4 <object>) (arg5 <object>)
   (arg6 <object>)   (arg7 <object>) (args <list>))

  (if (null? args) (%funcall7 foo arg1 arg2 arg3 arg4 arg5 arg6 arg7)
    (%let ((arg-descr %signed-word-integer (function-arg-descr foo)))
          (if (%eq arg-descr #%i-8)
              (let ((cl-vars (function-closure-vars foo)))
                (if cl-vars (%funcall (function-address foo)
                                      arg1 arg2 arg3 arg4 arg5 arg6 arg7 args cl-vars)
                  (%funcall (function-address foo)
                            arg1 arg2 arg3 arg4 arg5 arg6 arg7 args)))
            (if (%gt arg-descr #%i-8)
                (if (%lt arg-descr #%i8)
                    (apply>-8 foo arg-descr
                              arg1 arg2 arg3 arg4 arg5 arg6 (cons arg7 args))
                  (apply>=8 foo arg-descr arg1 arg2 arg3 arg4 arg5 arg6 arg7
                            (car args) (cdr args)))
              (apply>=8 foo arg-descr arg1 arg2 arg3 arg4 arg5 arg6 arg7
                        (car args) (cdr args))))))
  )

(%define-function (apply>=1 <object>)
  ((foo <function>)
   (arg-descr %signed-word-integer)
   (arg1 <object>) (args <list>))

  (if (%eq arg-descr #%i-2)
      (let ((cl-vars (function-closure-vars foo)))
        (if cl-vars (%funcall (function-address foo) arg1 args cl-vars)
          (%funcall (function-address foo) arg1 args)))
    (if (null? args)
        (if (%eq arg-descr #%i1)
            (let ((cl-vars (function-closure-vars foo)))
              (if cl-vars (%funcall (function-address foo) arg1 cl-vars)
                (%funcall (function-address foo) arg1)))
          (wrong-argument-number foo arg-descr #%i1 (list arg1)))
      (apply>=2 foo arg-descr arg1 (car args) (cdr args))))
  )

(%define-function (apply>=2 <object>)
  ((foo <function>)
   (arg-descr %signed-word-integer)
   (arg1 <object>) (arg2 <object>) (args <list>))

  (if (%eq arg-descr #%i-3)
      (let ((cl-vars (function-closure-vars foo)))
        (if cl-vars (%funcall (function-address foo) arg1 arg2 args cl-vars)
          (%funcall (function-address foo) arg1 arg2 args)))
    (if (null? args)
        (if (%eq arg-descr #%i2)
            (let ((cl-vars (function-closure-vars foo)))
              (if cl-vars (%funcall (function-address foo) arg1 arg2 cl-vars)
                (%funcall (function-address foo) arg1 arg2)))
          (wrong-argument-number foo arg-descr #%i2 (list arg1 arg2)))
      (apply>=3 foo arg-descr arg1 arg2 (car args) (cdr args))))
  )

(%define-function (apply>=3 <object>)
  ((foo <function>)
   (arg-descr %signed-word-integer)
   (arg1 <object>) (arg2 <object>)
   (arg3 <object>) (args <list>))

  (if (%eq arg-descr #%i-4)
      (let ((cl-vars (function-closure-vars foo)))
        (if cl-vars (%funcall (function-address foo) arg1 arg2 arg3 args cl-vars)
          (%funcall (function-address foo) arg1 arg2 arg3 args)))
    (if (null? args)
        (if (%eq arg-descr #%i3)
            (let ((cl-vars (function-closure-vars foo)))
              (if cl-vars (%funcall (function-address foo) arg1 arg2 arg3 cl-vars)
                (%funcall (function-address foo) arg1 arg2 arg3)))
          (wrong-argument-number foo arg-descr #%i3
                                 (list arg1 arg2 arg3)))
      (apply>=4 foo arg-descr arg1 arg2 arg3 (car args) (cdr args))))
  )

(%define-function (apply>=4 <object>)
  ((foo <function>)
   (arg-descr %signed-word-integer)
   (arg1 <object>) (arg2 <object>) (arg3 <object>)
   (arg4 <object>) (args <list>))

  (if (%eq arg-descr #%i-5)
      (let ((cl-vars (function-closure-vars foo)))
        (if cl-vars (%funcall (function-address foo)
                              arg1 arg2 arg3 arg4 args cl-vars)
          (%funcall (function-address foo)
                    arg1 arg2 arg3 arg4 args)))
    (if (null? args)
        (if (%eq arg-descr #%i4)
            (let ((cl-vars (function-closure-vars foo)))
              (if cl-vars (%funcall (function-address foo) arg1 arg2 arg3 arg4 cl-vars)
                (%funcall (function-address foo) arg1 arg2 arg3 arg4)))
          (wrong-argument-number foo arg-descr #%i4
                                 (list arg1 arg2 arg3 arg4)))
      (apply>=5 foo arg-descr arg1 arg2 arg3 arg4 (car args) (cdr args))))
  )

(%define-function (apply>=5 <object>)
  ((foo <function>)
   (arg-descr %signed-word-integer)
   (arg1 <object>) (arg2 <object>) (arg3 <object>)
   (arg4 <object>) (arg5 <object>) (args <list>))

  (if (%eq arg-descr #%i-6)
      (let ((cl-vars (function-closure-vars foo)))
        (if cl-vars (%funcall (function-address foo)
                              arg1 arg2 arg3 arg4 arg5 args cl-vars)
          (%funcall (function-address foo)
                    arg1 arg2 arg3 arg4 arg5 args)))
    (if (null? args)
        (if (%eq arg-descr #%i5)
            (let ((cl-vars (function-closure-vars foo)))
              (if cl-vars (%funcall (function-address foo)
                                    arg1 arg2 arg3 arg4 arg5 cl-vars)
                (%funcall (function-address foo)
                          arg1 arg2 arg3 arg4 arg5)))
          (wrong-argument-number foo arg-descr #%i5
                                 (list arg1 arg2 arg3 arg4 arg5)))
      (apply>=6 foo arg-descr
                arg1 arg2 arg3 arg4 arg5 (car args) (cdr args))))
  )

(%define-function (apply>=6 <object>)
  ((foo <function>)
   (arg-descr %signed-word-integer)
   (arg1 <object>) (arg2 <object>) (arg3 <object>)
   (arg4 <object>) (arg5 <object>) (arg6 <object>)
   (args <list>))

  (if (%eq arg-descr #%i-7)
      (let ((cl-vars (function-closure-vars foo)))
        (if cl-vars (%funcall (function-address foo)
                              arg1 arg2 arg3 arg4 arg5 arg6 args cl-vars)
          (%funcall (function-address foo)
                    arg1 arg2 arg3 arg4 arg5 arg6 args)))
    (if (null? args)
        (if (%eq arg-descr #%i6)
            (let ((cl-vars (function-closure-vars foo)))
              (if cl-vars (%funcall (function-address foo)
                                    arg1 arg2 arg3 arg4 arg5 arg6 cl-vars)
                (%funcall (function-address foo)
                          arg1 arg2 arg3 arg4 arg5 arg6)))
          (wrong-argument-number foo arg-descr #%i6
                                 (list arg1 arg2 arg3 arg4 arg5 arg6)))
      (apply>=7 foo arg-descr
                arg1 arg2 arg3 arg4 arg5 arg6 (car args) (cdr args))))
  )

(%define-function (apply>=7 <object>)
  ((foo <function>)
   (arg-descr %signed-word-integer)
   (arg1 <object>) (arg2 <object>) (arg3 <object>)
   (arg4 <object>) (arg5 <object>) (arg6 <object>)
   (arg7 <object>) (args <list>))

  (if (%eq arg-descr #%i-8)
      (let ((cl-vars (function-closure-vars foo)))
        (if cl-vars (%funcall (function-address foo)
                              arg1 arg2 arg3 arg4 arg5 arg6 arg7 args cl-vars)
          (%funcall (function-address foo)
                    arg1 arg2 arg3 arg4 arg5 arg6 arg7 args)))
    (if (null? args)
        (if (%eq arg-descr #%i7)
            (let ((cl-vars (function-closure-vars foo)))
              (if cl-vars (%funcall (function-address foo)
                                    arg1 arg2 arg3 arg4 arg5 arg6 arg7 cl-vars)
                (%funcall (function-address foo)
                          arg1 arg2 arg3 arg4 arg5 arg6 arg7)))
          (wrong-argument-number foo arg-descr #%i7
                                 (list arg1 arg2 arg3 arg4 arg5 arg6 arg7)))
      (apply>=8 foo arg-descr
                arg1 arg2 arg3 arg4 arg5 arg6 arg7
                (car args) (cdr args))))
  )

(%define-function (apply>=8 <object>)
  ((foo <function>)
   (arg-descr %signed-word-integer)
   (arg1 <object>) (arg2 <object>) (arg3 <object>)
   (arg4 <object>) (arg5 <object>) (arg6 <object>)
   (arg7 <object>) (arg8 <object>) (args <list>))

  (if (%eq arg-descr #%i-8)
      (let ((cl-vars (function-closure-vars foo)))
        (if cl-vars (%funcall (function-address foo)
                              arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 args cl-vars)
          (%funcall (function-address foo)
                    arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 args)))
    (if (null? args)
        (if (%eq arg-descr #%i8)
            (let ((cl-vars (function-closure-vars foo)))
              (if cl-vars (%funcall (function-address foo)
                                    arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 cl-vars)
                (%funcall (function-address foo)
                          arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8)))
          (wrong-argument-number
           foo arg-descr #%i8
           (list arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8)))
      (too-many-arguments foo arg-descr (%plus #%i8 (%list-size args))
                          (cons arg1
                                (cons arg2
                                      (cons arg3
                                            (cons arg4
                                                  (cons arg5
                                                        (cons
                                                         arg6
                                                         (cons
                                                          arg7
                                                          (cons
                                                           arg8
                                                           args)))))))))))
  )


(%define-function (apply>-2 <object>)
  ((foo <function>)
   (arg-descr %signed-word-integer)
   (args <list>))

  (if (%eq arg-descr #%i-1)
      (let ((cl-vars (function-closure-vars foo)))
        (if cl-vars (%funcall (function-address foo) args cl-vars)
          (%funcall (function-address foo) args)))
    (wrong-argument-number foo arg-descr (%list-size args) args))
  )

(%define-function (apply>-3 <object>)
  ((foo <function>)
   (arg-descr %signed-word-integer)
   (arg1 <object>) (args <list>))

  (if (%eq arg-descr #%i-2)
      (let ((cl-vars (function-closure-vars foo)))
        (if cl-vars (%funcall (function-address foo) arg1 args cl-vars)
          (%funcall (function-address foo) arg1 args)))
    (apply>-2 foo arg-descr (cons arg1 args)))
  )

(%define-function (apply>-4 <object>)
  ((foo <function>)
   (arg-descr %signed-word-integer)
   (arg1 <object>) (arg2 <object>) (args <list>))

  (if (%eq arg-descr #%i-3)
      (let ((cl-vars (function-closure-vars foo)))
        (if cl-vars (%funcall (function-address foo) arg1 arg2 args cl-vars)
          (%funcall (function-address foo) arg1 arg2 args)))
    (apply>-3 foo arg-descr arg1 (cons arg2 args)))
  )

(%define-function (apply>-5 <object>)
  ((foo <function>)
   (arg-descr %signed-word-integer)
   (arg1 <object>) (arg2 <object>)
   (arg3 <object>) (args <list>))

  (if (%eq arg-descr #%i-4)
      (let ((cl-vars (function-closure-vars foo)))
        (if cl-vars (%funcall (function-address foo) arg1 arg2 arg3 args cl-vars)
          (%funcall (function-address foo) arg1 arg2 arg3 args)))
    (apply>-4 foo arg-descr arg1 arg2 (cons arg3 args)))
  )

(%define-function (apply>-6 <object>)
  ((foo <function>)
   (arg-descr %signed-word-integer)
   (arg1 <object>) (arg2 <object>) (arg3 <object>)
   (arg4 <object>) (args <list>))

  (if (%eq arg-descr #%i-5)
      (let ((cl-vars (function-closure-vars foo)))
        (if cl-vars (%funcall (function-address foo)
                              arg1 arg2 arg3 arg4 args cl-vars)
          (%funcall (function-address foo)
                    arg1 arg2 arg3 arg4 args)))
    (apply>-5 foo arg-descr arg1 arg2 arg3 (cons arg4 args)))
  )

(%define-function (apply>-7 <object>)
  ((foo <function>)
   (arg-descr %signed-word-integer)
   (arg1 <object>) (arg2 <object>) (arg3 <object>)
   (arg4 <object>) (arg5 <object>) (args <list>))

  (if (%eq arg-descr #%i-6)
      (let ((cl-vars (function-closure-vars foo)))
        (if cl-vars (%funcall (function-address foo)
                              arg1 arg2 arg3 arg4 arg5 args cl-vars)
          (%funcall (function-address foo)
                    arg1 arg2 arg3 arg4 arg5 args)))
    (apply>-6 foo arg-descr arg1 arg2 arg3 arg4 (cons arg5 args)))
  )

(%define-function (apply>-8 <object>)
  ((foo <function>)
   (arg-descr %signed-word-integer)
   (arg1 <object>) (arg2 <object>) (arg3 <object>)
   (arg4 <object>) (arg5 <object>) (arg6 <object>)
   (args <list>))

  (if (%eq arg-descr #%i-7)
      (let ((cl-vars (function-closure-vars foo)))
        (if cl-vars (%funcall (function-address foo)
                              arg1 arg2 arg3 arg4 arg5 arg6 args cl-vars)
          (%funcall (function-address foo)
                    arg1 arg2 arg3 arg4 arg5 arg6 args)))
    (apply>-7 foo arg-descr arg1 arg2 arg3 arg4 arg5 (cons arg6 args)))
  )

(%define-function (apply>-9 <object>)
  ((foo <function>)
   (arg-descr %signed-word-integer)
   (arg1 <object>) (arg2 <object>) (arg3 <object>)
   (arg4 <object>) (arg5 <object>) (arg6 <object>)
   (arg7 <object>) (args <list>))

  (if (%eq arg-descr #%i-8)
      (let ((cl-vars (function-closure-vars foo)))
        (if cl-vars (%funcall (function-address foo)
                              arg1 arg2 arg3 arg4 arg5 arg6 arg7 args cl-vars)
          (%funcall (function-address foo)
                    arg1 arg2 arg3 arg4 arg5 arg6 arg7 args)))
    (apply>-8 foo arg-descr arg1 arg2 arg3 arg4 arg5 arg6 (cons arg7 args)))
  )

(%define-function (apply>-10 <object>)
  ((foo <function>)
   (arg-descr %signed-word-integer)
   (arg1 <object>) (arg2 <object>) (arg3 <object>)
   (arg4 <object>) (arg5 <object>) (arg6 <object>)
   (arg7 <object>) (arg8 <object>) (args <list>))

  (if (%eq arg-descr #%i-9)
      (let ((cl-vars (function-closure-vars foo)))
        (if cl-vars (%funcall (function-address foo)
                              arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 args cl-vars)
          (%funcall (function-address foo)
                    arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 args)))
    (apply>-9 foo arg-descr arg1 arg2 arg3 arg4 arg5 arg6 arg7 (cons arg8 args)))
  )

;;---------------------------------------------------------------

;;--------------------------------------------------------------------
;; %funcall0, %funcall1, ... used the apply-compiler to translate
;; an function call with an expression (and not a constant-binding)
;; in the first position.
;; Example: (defun foo (baz) .. (baz) ...) -> ...(%funcall0 baz) ...
;; (defun foo (arg fbaz) ... (fbaz arg) ...)
;;        -> ... (%funcall1 fbaz arg) ...
;;--------------------------------------------------------------------

(%define-function (%funcall0 <object>)
  ((foo <function>))

  (%let ((arg-descr %signed-word-integer
                    (function-arg-descr foo)))
        (if (%eq arg-descr #%i0)
            (let ((cl-vars (function-closure-vars foo)))
              (if cl-vars (%funcall (function-address foo) cl-vars)
                (%funcall (function-address foo) )))
          (if (%eq arg-descr #%i-1)
              (let ((cl-vars (function-closure-vars foo)))
                (if cl-vars (%funcall (function-address foo) () cl-vars)
                  (%funcall (function-address foo) ())))
            (wrong-argument-number foo arg-descr #%i0 ()))))
  )

(%define-function (%funcall1 <object>)
  ((foo <function>) (arg1 <object>))

  (%let ((arg-descr %signed-word-integer
                    (function-arg-descr foo)))
        (if (%eq arg-descr #%i1)
            (let ((cl-vars (function-closure-vars foo)))
              (if cl-vars (%funcall (function-address foo) arg1 cl-vars)
                (%funcall (function-address foo) arg1)))
          (apply>-3 foo arg-descr arg1 ())))
  )

(%define-function (%funcall2 <object>)
  ((foo <function>) (arg1 <object>) (arg2 <object>))

  (%let ((arg-descr %signed-word-integer
                    (function-arg-descr foo)))
        (if (%eq arg-descr #%i2)
            (let ((cl-vars (function-closure-vars foo)))
              (if cl-vars (%funcall (function-address foo) arg1 arg2 cl-vars)
                (%funcall (function-address foo) arg1 arg2)))
          (apply>-4 foo arg-descr arg1 arg2 ())))
  )

(%define-function (%funcall3 <object>)
  ((foo <function>) (arg1 <object>) (arg2 <object>)
   (arg3 <object>))

  (%let ((arg-descr %signed-word-integer
                    (function-arg-descr foo)))
        (if (%eq arg-descr #%i3)
            (let ((cl-vars (function-closure-vars foo)))
              (if cl-vars (%funcall (function-address foo) arg1 arg2 arg3 cl-vars)
                (%funcall (function-address foo) arg1 arg2 arg3)))
          (apply>-5 foo arg-descr arg1 arg2 arg3 ())))
  )

(%define-function (%funcall4 <object>)
  ((foo <function>) (arg1 <object>) (arg2 <object>)
   (arg3 <object>)  (arg4 <object>))

  (%let ((arg-descr %signed-word-integer
                    (function-arg-descr foo)))
        (if (%eq arg-descr #%i4)
            (let ((cl-vars (function-closure-vars foo)))
              (if cl-vars (%funcall (function-address foo) arg1 arg2 arg3 arg4 cl-vars)
                (%funcall (function-address foo) arg1 arg2 arg3 arg4)))
          (apply>-6 foo arg-descr arg1 arg2 arg3 arg4 ())))
  )

(%define-function (%funcall5 <object>)
  ((foo <function>) (arg1 <object>) (arg2 <object>)
   (arg3 <object>)  (arg4 <object>) (arg5 <object>))

  (%let ((arg-descr %signed-word-integer
                    (function-arg-descr foo)))
        (if (%eq arg-descr #%i5)
            (let ((cl-vars (function-closure-vars foo)))
              (if cl-vars (%funcall (function-address foo) arg1 arg2 arg3 arg4 arg5 cl-vars)
                (%funcall (function-address foo) arg1 arg2 arg3 arg4 arg5)))
          (apply>-7 foo arg-descr arg1 arg2 arg3 arg4 arg5 ())))
  )

(%define-function (%funcall6 <object>)
  ((foo <function>) (arg1 <object>) (arg2 <object>)
   (arg3 <object>)  (arg4 <object>) (arg5 <object>)
   (arg6 <object>))

  (%let ((arg-descr %signed-word-integer
                    (function-arg-descr foo)))
        (if (%eq arg-descr #%i6)
            (let ((cl-vars (function-closure-vars foo)))
              (if cl-vars (%funcall (function-address foo)
                                    arg1 arg2 arg3 arg4 arg5 arg6 cl-vars)
                (%funcall (function-address foo)
                          arg1 arg2 arg3 arg4 arg5 arg6)))
          (apply>-8 foo arg-descr arg1 arg2 arg3 arg4 arg5 arg6 ())))
  )

(%define-function (%funcall7 <object>)
  ((foo <function>) (arg1 <object>) (arg2 <object>)
   (arg3 <object>)  (arg4 <object>) (arg5 <object>)
   (arg6 <object>)  (arg7 <object>))

  (%let ((arg-descr %signed-word-integer
                    (function-arg-descr foo)))
        (if (%eq arg-descr #%i7)
            (let ((cl-vars (function-closure-vars foo)))
              (if cl-vars (%funcall (function-address foo)
                                    arg1 arg2 arg3 arg4 arg5 arg6 arg7 cl-vars)
                (%funcall (function-address foo)
                          arg1 arg2 arg3 arg4 arg5 arg6 arg7)))
          (apply>-9 foo arg-descr arg1 arg2 arg3 arg4 arg5 arg6 arg7 ())))
  )

(%define-function (%funcall8 <object>)
  ((foo <function>) (arg1 <object>) (arg2 <object>)
   (arg3 <object>)  (arg4 <object>) (arg5 <object>)
   (arg6 <object>)  (arg7 <object>) (arg8 <object>))

  (%let ((arg-descr %signed-word-integer
                    (function-arg-descr foo)))
        (if (%eq arg-descr #%i8)
            (let ((cl-vars (function-closure-vars foo)))
              (if cl-vars (%funcall (function-address foo)
                                    arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 cl-vars)
                (%funcall (function-address foo)
                          arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8)))
          (apply>-10 foo arg-descr arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 ()))))

;;;-----------------------------------------------------------------------------
;;; providing special objects to the compiler
;;;-----------------------------------------------------------------------------

(%annotate-function apply   is-special-function apply)
(%annotate-function %apply1 is-special-function apply)
(%annotate-function %apply2 is-special-function apply)
(%annotate-function %apply3 is-special-function apply)
(%annotate-function %apply4 is-special-function apply)
(%annotate-function %apply5 is-special-function apply)
(%annotate-function %apply6 is-special-function apply)
(%annotate-function %apply7 is-special-function apply)
(%annotate-function %apply8 is-special-function apply)

(%annotate-function %funcall0 is-special-function funcall)
(%annotate-function %funcall1 is-special-function funcall)
(%annotate-function %funcall2 is-special-function funcall)
(%annotate-function %funcall3 is-special-function funcall)
(%annotate-function %funcall4 is-special-function funcall)
(%annotate-function %funcall5 is-special-function funcall)
(%annotate-function %funcall6 is-special-function funcall)
(%annotate-function %funcall7 is-special-function funcall)
(%annotate-function %funcall8 is-special-function funcall)

;;;-----------------------------------------------------------------------------
)  ;; End of module function
;;;-----------------------------------------------------------------------------
