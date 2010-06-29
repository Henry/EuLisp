;;; EuLisp system 'youtoo'
;;;   Interface file for module condition

(definterface condition
  (import (telos thread dynamic let-cc)
   syntax (_macros)
   full-import (mop-alloc mop-access mop-prim mop-key mop-class mop-init mop-inspect mop-gf mop-meth mop-defcl boot boot1 telos event thread dynamic let-cc)
   export (
    ((name . pop-error-handlers) (pos . 6) (origin dynamic . pop-error-handlers))
    ((name . <general-condition>) (pos . 5) (origin condition . <general-condition>) (class . constant))
    ((name . error) (pos . 12) (origin boot . error))
    ((name . <condition>) (pos . 8) (origin condition . <condition>) (class . constant))
    ((name . signal) (pos . 2) (origin condition . signal))
    ((name . *default-error-handler*) (pos . 3) (origin condition . *default-error-handler*))
    ((name . condition-message) (pos . 6) (origin condition . condition-message) (inline (G00795 (static-ref0) (binding-ref ? <condition>) (primitive-relative-ref))) (setter (G00797 (stack-ref 1) (static-ref0) (stack-ref 2) (binding-ref ? <condition>) (set-primitive-relative-ref) (nobble 2))))
    ((name . output-condition-contents) (pos . 4) (origin condition . output-condition-contents))
    ((name . push-error-handler) (pos . 5) (origin dynamic . push-error-handler))
    ((name . condition?) (pos . 7) (origin condition . condition?))
   )
   local-literals (
    (top-level . 47)
    (signal . 46)
    (output-condition-contents . 45)
    (condition-message . 44)
    ("
*** ERROR [~a]: ~a
" . 41)
    ("    ~a: ~a
" . 39)
    (call/ep-lambda . 36)
    (anonymous . 34)
    (|(method condition?)| . 31)
    (|(setter condition-message)| . 30)
    (default-error-handler . 29)
    (condition? . 28)
    ((arguments:) . 27)
    (general-condition . 26)
    (arguments: . 25)
    (arguments . 24)
    ((message:) . 23)
    (direct-keywords: . 22)
    (direct-slots: . 21)
    (direct-superclasses: . 20)
    (condition . 19)
    (message: . 18)
    (keyword: . 17)
    (message . 16)
    (name: . 15)
    ("***    See Backtrace? (y/n) " . 13)
    ("***    Do you want to continue? (y/n) " . 12)
   )
   literals (
   )
))
