;;;-----------------------------------------------------------------------------
;;;                 OPS5 for EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;; File: action.em
;;; Date: 20 Jul 1995
;;; Author: Tracy Gardner (tag@maths.bath.ac.uk)
;;; Description: Action stuff
;;;-----------------------------------------------------------------------------
(defmodule action
    (syntax (macros macros-tag)
     import (level1 basic reader-vars reader-ce
                    prod action-gf wm wm-gf conflict ops-out))

  (defun accept () (read-s-expression stdin))

;;;-----------------------------------------------------------------------------
;;; Action Classes
;;;-----------------------------------------------------------------------------
  (defclass <make-action> (<action>)
    ((mk-wme-name
      keyword: mk-wme-name:
      reader:  mk-wme-name)
     (mk-attribs
      keyword: mk-attribs:
      reader:  mk-attribs))
    constructor: (make-make-action mk-wme-name: mk-attribs:))

  (defclass <remove-action> (<action>)
    ((rm-elt-desigs
      keyword: rm-elt-desigs:
      reader:  rm-elt-desigs))
    constructor: (make-remove-action rm-elt-desigs:))

  (defclass <modify-action> (<action>)
    ((md-attribs
      keyword: md-attribs:
      reader:  md-attribs)
     (md-elt-desig
      keyword: md-elt-desig:
      reader:  md-elt-desig))
    constructor: (make-modify-action md-elt-desig: md-attribs:))

  (defclass <write-action> (<action>)
    ((wr-data
      keyword: wr-data:
      reader:  wr-data))
    constructor: (make-write-action wr-data:))

  (defclass <bind-action> (<action>)
    ((bd-var
      keyword: bd-var:
      reader:  bd-var)
     (bd-val
      keyword: bd-val:
      reader:  bd-val))
    constructor: (make-bind-action bd-var: bd-val:))

;;;-----------------------------------------------------------------------------
;;; Execute Actions
;;;-----------------------------------------------------------------------------

  (defmethod execute ((action <make-action>) pi
                      wm-manager ce-manager cr-manager)
    ;;(format ops-out "*** make-action: ~a~%" (mk-wme-name action))
    (let* ((class (mk-wme-name action))
           (a-vals (mk-attribs action))
           (new-attrib-vals
            (labels
             ((set-attribs (alist newlist)
                 (cond
                  ((null alist) newlist)
                  (t (set-attribs (cddr alist)
                                  (cons (cons (make-attrib (car alist))
                                              (rhs-val (cadr alist) pi))
                                        newlist))))))
             (set-attribs a-vals ()))))
      (insert-wme wm-manager ce-manager cr-manager class new-attrib-vals)))

  ;; Process rhs values (such a variables, compute, etc)
  (defun rhs-val (val pi)
    ;;(print "rhs-val")
    (if (atom val)
        (if (is-ops5-var val)
            (apply-binding (pi-bindings pi) val)
          val)
      (cond
       ((eql (car val) 'crlf)
        (print "") ())
       ((eql (car val) 'accept)
        (let ((res (accept))) res))
       ((eql (car val) 'compute)
        (compute (cdr val) pi))
       ((eql (car val) 'tabto)
        (make <string> size: (cadr val)))
       (t (format ops-out "Function ~a not yet supported~%" (car val))))))

;;;-----------------------------------------------------------------------------
;;; Compute
;;  Compute expression wrt production instantiation
;;;-----------------------------------------------------------------------------
  (defun compute (exprn pi)
   ;; (print compute)
   ;; (print exprn)
    (let ((arg1 (rhs-val (car exprn) pi))
          (op   (cadr exprn))
          (arg2 (rhs-val (caddr exprn) pi)))
      (cond
       ((eql op '+) (+ arg1 arg2))
       ((eql op '-) (- arg1 arg2))
       ((eql op '*) (* arg1 arg2))
       ((eql op '/) (/ arg1 arg2))
       ((eql op '%) (binary-mod arg1 arg2))
       (t (format ops-out "Unknown operator in compute: ~a~%" op)))))

  (defun apply-binding (bindings apply-to)
   ;; (format t "apply-to: ~a bindings: ~a~%" apply-to bindings)
    (cdr (assoc apply-to bindings)))

  (defmethod execute ((action <remove-action>) pi
                      wm-manager ce-manager cr-manager)
    ;;(print "*** remove-action")
    (let ((ce-ts (ce-ts-list pi))
          (ce-nums (rm-elt-desigs action)))
      (labels ((loop (elt-desigs)
                     (cond
                      ((null elt-desigs))
                      (t (let ((ce-des (car elt-desigs)))
                           (remove-wme wm-manager ce-manager cr-manager
                                       (if (is-ops5-var ce-des)
                                           (rhs-val ce-des pi)
                                         (get-ts ce-des (pi-prod pi) ce-ts))))
                         (loop (cdr elt-desigs)))))))))

  (defmethod execute ((action <modify-action>) pi
                      wm-manager ce-manager cr-manager)
    ;;(format ops-out  "*** modify-action ~a~%" action)
    (let* ((ce-des (md-elt-desig action))
           (ce-ts  (ce-ts-list pi))
           (tstamp (if (is-ops5-var ce-des)
                       (rhs-val ce-des pi)
                     (get-ts ce-des (pi-prod pi) ce-ts)))
           (wme (unless (null tstamp)
                        (remove-wme wm-manager ce-manager cr-manager tstamp)))
           (class (wme-class-name wme))
           (prev-attrib-vals (wme-attrib-vals wme))
           (new-attrib-vals
            (labels
             ((set-attribs (alist newlist)
                 ;;(print alist)
                 (cond
                  ((null alist) newlist)
                  (t (set-attribs (cddr alist)
                                  (cons (cons (make-attrib (car alist))
                                               (rhs-val (cadr alist) pi))
                                        newlist))))))
             (set-attribs (md-attribs action) ())))
           (a-vals (accumulate
                    (lambda (a x)
                      (if (null (assoc (car x) a))
                          (cons x a)
                        a))
                    new-attrib-vals
                    prev-attrib-vals)))
      ;;(format ops-out  "inserting new wme: ~a ~a ~a~%" class
        ;;    prev-attrib-vals a-vals)
      (insert-wme wm-manager ce-manager cr-manager class a-vals)))

  (defmethod execute ((action <write-action>) pi
                      wm-manager ce-manager cr-manager)
    ;;(format ops-out "*** write-action: ")
    (do
     (lambda (x)
       (let ((val (rhs-val x pi)))
         (when val (format ops-out "~a " val))))
     (wr-data action)))

  (defmethod execute ((action <bind-action>) pi
                      wm-manager ce-manager cr-manager)
    (let ((var (bd-var action))
          (val (rhs-val (bd-val action) pi)))
      (set-pi-bindings pi (cons (cons var val) (pi-bindings pi)))))

   (export make-make-action make-remove-action make-modify-action
           make-write-action make-bind-action make-halt-action)

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
