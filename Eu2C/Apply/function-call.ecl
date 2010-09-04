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
;;;  Title: a call to a function
;;;  Description:
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Horst Friedrich
;;;-----------------------------------------------------------------------------

#module function-call

(import ((except (format) level-1)
         SIMPLE-PROGRAMMING
         lzs
         mzs
         lzs-mop
         accessors
         context
         analyse-h ; make-vector and vector-ref
         type-propagation
         progn-context
         vector
         lzs-to-mzs-fun
         function-call-context
         (only (error format) common-lisp)
         type-inference
         side-effects
         gutter
         name-of-fun
         inline
         debugging
         tail-module ; %cast
         configuration ; nothing imported, only to initialize (dynamic *inline*)
         )

 syntax (level-1)

 export (call-a-function ; fun arg-list last
         )
 )

(defun call-a-function (fun arg-list last read-glocs)
  ;;
  ;; fun = <global-fun>, <local-fun>, <imported-fun>, <special-sys-fun>,
  ;; <global-generic-fun>, <local-generic-fun>, imported-generic-fun>, <var-ref>,
  ;; <cont>, <defined-named-constant>, <imported-named-constant>
  ;;
  ;; global-fun - normal function call last-call
  ;; local-fun - closure-call possible last-call
  ;; imported-function - closure-call possible last-call
  ;; special-sys-fun - last-asm
  ;; global-generic-fun - last-call
  ;; local-generic-fun - closure-possible last-call
  ;; imported-generic-fun - closure possible last-call
  ;; var-ref - funcall
  ;; cont - goto ??
  ;;  defined-named-const, imported-named-const - funcall
  (if (named-const? fun)
      (setq fun (?value fun))
    (if (var-ref? fun) (setq fun (?var fun)) ()))
  (let* ((arg-num (length arg-list))
         (call
          (cond ((fun? fun)
                 (let ((rglocs
                        (if (?fread-gloc fun)
                            (balance-side-effects1
                             (?glocs (?fread-gloc fun)) read-glocs)
                          read-glocs))
                       (wglocs
                        (if (?fwrite-gloc fun)
                            (?glocs (?fwrite-gloc fun)) ())))
                   (if (special-sys-fun? fun)
                       (if last
                           (make <last-asm> :function fun
                                 :read-glocs rglocs
                                 :write-glocs wglocs)
                         (make <asm> :function fun
                               :read-glocs rglocs
                               :write-glocs wglocs))
                     (if last
                         (make <last-call> :function fun
                               :read-glocs rglocs
                               :write-glocs wglocs)
                       (make <call> :function fun
                             :read-glocs rglocs
                             :write-glocs wglocs))
                     ))
                 )
                ;; var = <local-static>, <global-static>, <imported-static>, <dynamic>
                ((local-static? fun)
                 (make <funcall> :value fun :closure-call t
                       :read-glocs (balance-side-effects1
                                    (?glocs *funcall-fread-gloc*)
                                    read-glocs)
                       :write-glocs (?glocs *funcall-fwrite-gloc*)))
                ((var? fun)
                 ;; global-read
                 (make <funcall> :value fun :closure-call t
                       :read-glocs (balance-side-effects1
                                    (?glocs *funcall-fread-gloc*)
                                    read-glocs)
                       :write-glocs (?glocs *funcall-fwrite-gloc*)))
                (t (error " ~s is no function" fun))))
         (result (make <tempvar> :tnr (dynamic *counter*)))
         (var-vec (make-vector (+ arg-num 1)))
         (inline ()) )
    (setf (dynamic *counter*) (+ (dynamic *counter*) 1))
    (setf (vector-ref var-vec  0) result)
    (setf (?arg-num call) arg-num)
    (setf (?var-descr call) (make <var-descr>
                                  :var-vec var-vec
                                  :constant-counter 0))
    (setf (?type-descr call) (general-var-actual-descr arg-num))
    (setf (?type-descr-s call) ())
    ;; fill the var-descr
    (l2m-call call arg-list)
    ;; constant propagation
    ;; fill type-descr-s
    (if (eq fun %cast)
        (progn (setf (vector-ref (?var-vec (?var-descr call))
                                 1)
                     (make <cast>
                           :type
                           (vector-ref (?var-vec (?var-descr call))
                                       1))))
      ())

    (let ((typedescrs
           (make-actual-type-descr (dynamic typepathes)
                                   call
                                   (?var-descr call)
                                   arg-num
                                   ())))
      ;; rename local-static-variable
      (if (and (funcall? call) (local-static? (?value call)))
          (let ((tempvar (rename (?value call))))
            (setq fun  tempvar)
            (setf (?value call) fun))
        (progn
          ;; analyse called function first
          (lzs2mzs-fun fun)
          ;; set the inline-flag
          (if (inline-able fun)
              (setq inline t)
            ;; add annotations to called function
            (setf (?applications fun)
                  (cons call (?applications fun))))))
      ;; make a type - inference
      (setq typedescrs
            (inference fun typedescrs))
      (if (and (generic-fun? fun) *actual-method-subset*
               (null? (cdr *actual-method-subset*))) ; only one method
          (progn
            ;;           (format t "M")
            (setq fun (?fun (car *actual-method-subset*)))
            (setf (?function call) fun)
            (if (inline-able fun) (setq inline t) (setq inline ())))
        ())
      ;; add the type-descriptors
      (setf (?type-descr-s call) typedescrs)
      (setf (dynamic typepathes) typedescrs)
      ;; inline or not
      (if inline
          (progn
            (inline-information fun)
            (link-var-vec var-vec call arg-num)
            (setq result (inline-a last fun var-vec result))
            (unlink-var-vec var-vec call 0 arg-num)
            result)
        ;; link variable
        (let ((curblock (dynamic block)))
          (link-var-vec var-vec call arg-num)
          (if (funcall? call)
              (link-funcall-variable (?value call) call) ())
          ;; add the statement to the Block
          (setf (?block call) curblock)
          (setf (?body curblock)
                (append-stat (?body curblock) call))
          ;; add annotation to the function
          (cond ((null? (funcall? call))
                 (setf (dynamic calls)
                       (cons call (dynamic calls)))))
          ;; result
          result)))))

(defun unlink-var-vec (var-vec call nr arg-num)
  (if (> nr arg-num) var-vec
    (let ((var (vector-ref var-vec nr)))
      (if (or (local-static? var)
              (tempvar? var))
          (setf (?link var) (unlink-var-vec1 (?link var) call))
        ())
      (unlink-var-vec var-vec call (+ nr 1) arg-num))))

(defun unlink-var-vec1 (link call)
  (if link
      (if (eq (car (car link)) call)
          (unlink-var-vec1 (cdr link) call)
        (cons (car link) (unlink-var-vec1 (cdr link) call)))
    ()))


(defun inline-information (foo)
  (format t "i")
  ;;  (let ((fun (analysed-fun)))
  ;;    (format t "~% -------------------------------------------")
  ;;    (format t "~% in ~a function ~a:"
  ;;            (funtype-of fun) (name-of fun))
  ;;    (format t "~% inline of ~a function ~a"
  ;;            (funtype-of foo) (name-of foo))
  ;;    (format t "~% -------------------------------------------"))
  )

;; inlining is controlled by the dynamic variable *inline* which is set by the
;; configuration 'inline' and which may contain the following values:
;; () - no inlining at all
;; 0   - only inlining of slot-accessors and slot-default-functions if they meet the
;;       requirement of (dynamic *inline*) = 1
;; n   - inlining takes place if the "complexity" of the function is less than n
;; (dynamic *inline*) is defined in the module 'configuration'

(defun inline-able (fun)
  (if (global-fun? fun)
      (progn (lzs2mzs-fun fun)
             (if (and (dynamic *inline*)
                      (> (?pass fun) 2))
                 (let* ((f-label (?function-label fun))
                        (start-block (?start-block f-label))
                        (end-blocks  (?end-blocks f-label))
                        (calls (?calls fun)))
                   (if (and (eq start-block (car end-blocks))
                            (null? (cdr end-blocks)))
                       (if (or (module-init-fun? (analysed-fun))
                               (eq (dynamic *inline*) 0))
                           (and (or (slot-accessor-fun? fun)
                                    (slot-init-fun? fun))
                                (only-asm-stats-small calls))
                         (only-asm-stats-big calls))
                     ()))
               () ))
    ()))

(defun only-asm-stats-small (stats)
  (let ((nr (only-asm-stats1 stats 0)))
    (if (< nr 1) t ())))

(defun only-asm-stats-big (stats)
  (let ((nr (only-asm-stats1 stats 0)))
    (if (< nr (dynamic *inline*)) t ())))

(defun only-asm-stats1 (stats n)
  (if (null? stats) n
    (let ((stat (car stats)))
      (if (or (last-asm? stat)
              (asm? stat)
              (and (or (call? stat) (last-call? stat))
                   (constructor-fun? (?function stat))))
          (only-asm-stats1 (cdr stats) n)
        (only-asm-stats1 (cdr stats) (+ n 1))
        ))))

#module-end
