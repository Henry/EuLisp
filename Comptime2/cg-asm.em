;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;; -----------------------------------------------------------------------
;;;                     EuLisp System 'youtoo'
;;; -----------------------------------------------------------------------
;;;  Library: comp (EuLisp to Bytecode Compiler -- EuLysses))
;;;  Authors: Andreas Kind, Keith Playford
;;;  Description: assembling
;;; -----------------------------------------------------------------------
(defmodule cg-asm
  (syntax (_macros _i-aux0 _sx-obj0)
   import (i-all sx-obj cg-bycode1 cg-bycode2 cg-state op-peep op-peep-r)
   export (assemble))
;;; --------------------------------------------------------------------
;;; Assemble module
;;; --------------------------------------------------------------------
  (defglobal *with-long-jumps* ())
  (defun assemble (module code-state)
    (labels
     ((loop (states res)
            (if (null states) res
              (let* ((state (car states))
                     (handle (code-state-handle? state))
                     (name (code-state-binding-name? state))
                     (asm (code-state-asm? state))
                     (new-res (loop (code-state-enclosed-code? state) res))
                     (bv-state (assemble-function asm handle name)))
                (loop (cdr states) (cons bv-state new-res))))))
      (with-ct-handler "assembler error" module
        (notify "  Assembling module ~a ..." (module-name? module))
        (setq *pass* 'assemble)
        (let* ((bv-states (loop (list code-state) ())))
          (make <asm-state>
                init-bytevector: (car bv-states)
                bytevectors: (reverse (cdr bv-states)))))))
;;; --------------------------------------------------------------------
;;; Assemble function
;;; --------------------------------------------------------------------
  (defun assemble-function (code handle name)
    (let* ((table (make-access-table))
           (state (make-asm-function-state handle name)))
      (do1-list (lambda (i)
                  (assemble-instruction i state table))
                (peep-hole-optimize code))
      (if (resolve-label-refs table)
          (progn
            (asm-function-state-code!
             state (reverse (asm-function-state-code? state)))
            state)
        (dynamic-let ((*with-long-jumps* t))
          (notify0 "  Re-assembling function ~a because of long jump" name)
          (assemble-function code handle name)))))
  (defun assemble-instruction (l state table)
    (notify0 "    Instruction: ~a" l)
    (let ((name (car l))
          (args (cdr l)))
      (cond
       ((eq name 'label)
        (register-label-loc table (car args) (asm-function-state-pc? state)))
       ((eq name 'binding-ref)
        (put-bc state 'binding-ref)
        (put-fix state `(BINDING ,@args)))
       ((eq name 'set-binding-ref)
        (put-bc state 'set-binding-ref)
        (put-fix state `(BINDING ,@args)))
       ((eq name 'set-and-get-binding-ref)
        (put-bc state 'set-and-get-binding-ref)
        (put-fix state `(BINDING ,@args)))
       ((eq name 'code-vector-ref)
        (put-bc state 'static-ref)
        (put-fix state `(CODE-VECTOR ,@args)))
       ((eq name 'call-foreign-function)
        (let* ((name (car args)))
          (put-bc state 'call-foreign-function)
          (put-fix state `(FF ,name))))
       (t
        (assemble-instruction-default name args state table)))))
  (defun assemble-instruction-default (name args state table)
    (if (and (null (dynamic *with-long-jumps*))
             (branchp name))
        (assemble-branch name args state table)
      (let* ((code (put-bc state name))
             (formals (bytecode-args? code)))
        (if (null formals) ()
          (let ((formal1 (car formals)))
            (cond
             ((eq formal1 'label)
              (let* ((loc (- (asm-function-state-pc? state) 1))
                     (ref (cons name loc)))
                ;; Refs are resolved later
                (register-label-ref table (car args) ref)
                (put-fix state ref)))
             ((eq formal1 'static)
              (put-fix state `(STATIC ,(car args))))
             ((eq formal1 'reg)
              (put-byte
               state (or (get-register (car args))
                         (ct-serious-warning 0 "no register ~a" (car args)))))
             ((eq formal1 'byte)
              (put-bytes state args))
             ((eq formal1 'label)
               (let* ((loc (- (asm-function-state-pc? state) 1))
                      (ref (cons name loc)))
                 (register-label-ref table (car args) ref)
                 (put-fix state ref)))
             (t
              (do1-list (lambda (v) (put-fix state v)) args))))))))
  (defun branchp (x)
    (member1-list x '(branch-true branch-nil branch)))
  (defun assemble-branch (name args state table)
    ;; Label ref can be modified after labels are resolved
    (let* ((loc (+ (asm-function-state-pc? state) 1))
           (ref (cons name loc)))
      ;; Refs are resolved later
      (register-label-ref table (car args) ref)
      (put-branch-bytes state ref)))
;;; --------------------------------------------------------------------
;;; Resolve and register labels
;;; Label table entry looks like this: (label-pc (ref-pc1) (ref-pc2) ...)
;;; --------------------------------------------------------------------
  (defun register-label-loc (table label pc)
    (let ((x (table label)))
      (if x
          (if (null (car x))
              ((setter car) x pc)
            (error "label multiply defined" <condition>))
        (let ((fun (setter table)))
          (fun label (list pc))))))
  (defun register-label-ref (table label ref)
    (notify0 "    Register label ~a ref ~a" label ref)
    (let ((x (table label)))
      (if x
          ((setter cdr) x (cons ref (cdr x)))
        ;; label refered before it is defined
        (let ((fun (setter table)))
          (fun label (list () ref))))))
  (defun resolve-label-refs (table)
    (notify0 "    Resolve labels")
      (let/cc k
        (if (dynamic *with-long-jumps*)
            (access-table-do
             (lambda (label entry)
               ;; Attention -- label is ptr to C string!
               (let ((loc (car entry))
                     (refs (cdr entry)))
                 (do (lambda (ref)
                       (let* ((ref-loc (cdr ref))
                              (offset (- loc ref-loc))
                              (bytes (fix-as-4-bytes offset)))
                         (notify0 "   .Resolving ~a to ~a" ref offset)
                         ((setter car) ref (car bytes))
                         ((setter cdr) ref (cdr bytes))))
                     refs)))
             table)
          (access-table-do
           (lambda (label entry)
             ;; Attention -- label is ptr to C string!
             (let ((pc (car entry))
                   (refs (cdr entry)))
               (do1-list
                (lambda (ref)
                  (let* ((name (car ref))
                         (ref-pc (cdr ref))
                         (offset (- pc ref-pc)))
                    (cond ((< offset -512)
                           (notify0 "jump too long: ~a" offset)
                           (k ()))
                          ((< offset -255)
                           ((setter car) ref
                            (get-branch-code name "-long-neg"))
                           ((setter cdr) ref (list (- (- 0 offset) 256))))
                          ((< offset 0)
                           ((setter car) ref
                            (get-branch-code name "-neg"))
                           ((setter cdr) ref (list (- 0 offset))))
                          ((< 511 offset)
                           (notify0 "jump too long: ~a" offset)
                           (k ()))
                          ((< 255 offset)
                           ((setter car) ref
                            (get-branch-code name "-long-pos"))
                           ((setter cdr) ref (list (- offset 256))))
                          (t
                           ((setter car) ref
                            (get-branch-code name "-pos"))
                           ((setter cdr) ref (list offset))))))
                refs)))
           table))
        table))
  (defun get-branch-code (x str)
    (let ((name (symbol-name x)))
      (bytecode-code? (get-bytecode (make-symbol (string-append name str))))))
;;; --------------------------------------------------------------------
;;; Put fixints (4 bytes) and single bytes into the bytevector
;;; --------------------------------------------------------------------
  (defun put-fix (state x)
    (align state)
    (if (integerp x)
        (put-bytes state (fix-as-4-bytes x))
      (let ((code (asm-function-state-code? state)))
        (asm-function-state-pc! state (+ (asm-function-state-pc? state) 4))
        (asm-function-state-code! state (cons x code)))))
  (defun put-bc (state name)
    (let ((bc (or (get-bytecode name)
                  (ct-serious-warning
                   (get-bytecode 'noop) "no bytecode ~a" name))))
      (put-byte state (bytecode-code? bc))
      bc))
  (defun put-branch-bytes (state ref)
    (notify0 "      PUT BRANCH BYTES: ~a" ref)
    (asm-function-state-pc! state (+ (asm-function-state-pc? state) 2))
    (asm-function-state-code! state
                              (cons ref (asm-function-state-code? state))))
  (defun put-byte (state x)
    (notify0 "      PUT BYTE: ~a" x)
    (let ((code (asm-function-state-code? state)))
      (asm-function-state-pc! state (+ (asm-function-state-pc? state) 1))
      (asm-function-state-code! state (cons x code))))
  (defun put-bytes (state l)
    (labels
     ((loop (ll i code)
            (if (null ll)
                (progn
                  (asm-function-state-pc! state i)
                  (asm-function-state-code! state code))
              (loop (cdr ll) (+ i 1) (cons (car ll) code)))))
     (loop l
           (asm-function-state-pc? state)
           (asm-function-state-code? state))))
;;; --------------------------------------------------------------------
;;; Alignment
;;; --------------------------------------------------------------------
  (defun align (state)
    (labels
     ;; Padding
     ((loop (i l)
            (if (= i 0) l
              (loop (- i 1) (cons 0 l)))))
     (let* ((pc (asm-function-state-pc? state))
            (code (asm-function-state-code? state))
            (rem (% pc 4)))
       (if (= rem 0) ()
         (let ((m (- 4 rem)))
           (asm-function-state-code! state (loop m code))
           (asm-function-state-pc! state (+ pc m)))))))
;;; --------------------------------------------------------------------
;;; Fixnum as list of bytes
;;; --------------------------------------------------------------------
  (defun fix-as-4-bytes (x)
    (if (< x 0)
        (neg-fix-bytes-aux (- (- 0 x) 1) 3 ())
      (pos-fix-bytes-aux x 3 ())))
  (defun pos-fix-bytes-aux (x pos res)
    (if (= pos 0) (cons x res)
      (let ((rem (% x 256))
            (div (/ x 256)))
        (pos-fix-bytes-aux div (- pos 1) (cons rem res)))))
  (defun neg-fix-bytes-aux (x pos res)
    (if (= pos 0) (cons (- 255 x) res)
      (let ((rem (% x 256))
            (div (/ x 256)))
        (neg-fix-bytes-aux div (- pos 1) (cons (- 255 rem) res)))))
)  ; end of module