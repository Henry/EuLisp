;; some non-EuLisp stuff to stop Feel users whinging

(defmodule standard
  (import (level0)
   export (subst nconc make-symbol symbol-name while for ++ -- defstruct))

(expose level0)

;; subst a for b in c
(defun subst (a b c)
  (if (list? c)
      (map (lambda (e) (if (binary= b e) a e)) c)
    (error <bad-type>
           "trying to to subst into a non-list"
           value: c
           expected-type: <list>)))

(defun nconc (a b)
  (if (null? a)
      b
    (progn ((setter cdr) (last-pair a) b)
           a)))

(defun make-symbol (string)
  (convert string <symbol>))

(defun symbol-name (symbol)
  (convert symbol <string>))

(defmacro for (init condition inc . body)
  `(progn
     ,init
     (while ,condition
       ,@body
       ,inc)))

(defmacro ++ (sym . inc)
  `(setq ,sym (+ ,sym ,(if (null? inc) 1 (car inc)))))

(defmacro -- (sym . inc)
  `(setq ,sym (- ,sym ,(if (null? inc) 1 (car inc)))))


(defmacro defstruct (name superclass slot-descriptions . class-options)
  `(defclass ,name
     (,(if (null? superclass) '<object> superclass))
     ,(map (lambda (slot)
             (if (atom? slot)
                 slot
               (map (lambda (sym)
                      (cond ((eq sym 'accessor) accessor:)
                            ((eq sym 'reader) reader:)
                            ((eq sym 'writer) writer:)
                            ((eq sym 'initform) default:)
                            ((eq sym 'initarg) keyword:)
                            (t sym)))
                    slot)))
           slot-descriptions)
     ,@(map (lambda (sym)
              (cond ((eq sym 'constructor) constructor:)
                    ((eq sym 'predicate) predicate:)
                    (t sym)))
            class-options)))

)
