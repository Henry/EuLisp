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
;;;  Title: Computation of Method Lookup and Generic Dispatch at Compile Time
;;;  Description:
;;    Assumptions:
;;    1. No method is added at runtime
;;    2. No subclass is added at runtime
;;;  Documentation:
;;;  Notes:
;;    Some definitions, originally defined by Ingo Mohr, are commented out and
;;    are replaced by a bit different version written by Hortst Friedrich.
;;;  Requires:
;;;  Problems: error methods and gf with no methods
;;;  Authors: Ingo Mohr
;;    extended by Horst Friedrich
;;;-----------------------------------------------------------------------------

#module generic-dispatch
(import (level-0
         apply-standard
         accessors
         lzs
         lzs-mop
         el2lzs-rules
         tail-module ; %funcall
         apply-funs ; %class-of no-applicable-method-error
         inline-method
         ;; expand-literal ; hack for method-errors !!!
         (only (stable-sort
                find-if
                make-instance
                assoc
                mapc
                mapcar
                append
                error
                funcall)
               common-lisp))
 syntax (level-1)
 export (reset-generic-dispatch
         set-discriminating-functions
         set-std-discr-fun
         map-to-std-discr-funs))

;;;-----------------------------------------------------------------------------
;;; some predicates to ask for the metaclass of classes
;;;-----------------------------------------------------------------------------
;;; should be provided generally in a separate module

(defun abstract-class? (class-def)
  (eq (?class class-def) %abstract-class))

(defun tail-class? (class-def)
  (eq (?class class-def) %tail-class))

;;;-----------------------------------------------------------------------------
;;; set-discriminating-functions and ~set-discriminating-function
;;;-----------------------------------------------------------------------------

(defun set-discriminating-functions (module)
  (dynamic-let ((*current-module* module))
               (mapc #'~set-discriminating-function
                     (?fun-list module))))

(defmethod ~set-discriminating-function ((fun <fun>))
  ;;do nothing for non-generic functions
  ())

(defmethod ~set-discriminating-function ((gf <generic-fun>))
  ;;called after loading application modules and before analyzing functions
  ;;this means that all statically collected methods are stored in the
  ;;generic-fun, but a call of add-method at runtime may add additional methods
  ;;
  ;;ATTENTION: (dynamic *current-module*) must be set to the module where the
  ;;generic function was defined
  ;;
  (setf (?discrimination-arguments gf)
        (~compute-discrimination-arguments gf))
  (setf (?discrimination-depth gf)
        (~compute-discrimination-depth gf))
  (setf (?discriminating-fun gf)
        (~compute-discriminating-function
         gf
         (~generic-function-domain gf)
         ()             ;; the lookup-fn is ignored in the
         ;; default case
         (~generic-function-methods gf)))
  )

;;the following method is not needed for the standard-level-0-case
;;(defmethod ~compute-method-lookup-function (gf domain))

;;;-----------------------------------------------------------------------------
;;; ~compute-discrimination-arguments
;;;-----------------------------------------------------------------------------

(defun null-list (dom)
  (if dom (cons () (null-list (cdr dom))) ()))

(defun discr-args (dom1 rest-methods d-args)
  (if rest-methods
      (let ((dom2 (~method-domain (car rest-methods))))
        (calc-d-args dom1 dom2 d-args)
        (discr-args dom2 (cdr rest-methods) d-args))
    d-args))

(defun  calc-d-args (dom1 dom2 d-args)
  (if dom1
      (if (eq (car dom1) (car dom2))
          (calc-d-args (cdr dom1) (cdr dom2) (cdr d-args))
        (progn
          (setf (car d-args) t)
          (calc-d-args (cdr dom1) (cdr dom2) (cdr d-args))))
    ()))

(defmethod ~compute-discrimination-arguments ((gf <generic-fun>))
  (let ((discr-arguments (null-list (~generic-function-domain gf)))
        (methods (~generic-function-methods gf)))
    (if methods
        (if (cdr methods)
            (discr-args (~method-domain (car methods)) (cdr methods)
                        discr-arguments)
          (discr-args (~generic-function-domain gf) methods
                      discr-arguments))
      discr-arguments)))

;;;-----------------------------------------------------------------------------
;;; ~compute-discrimination-depth
;;;-----------------------------------------------------------------------------
(defun discr-depth (d-args count)
  (if d-args
      (if (car d-args)
          (discr-depth (cdr d-args) (+ count 1))
        (discr-depth (cdr d-args) count))
    count))

(defmethod ~compute-discrimination-depth ((gf <generic-fun>))
  (discr-depth (?discrimination-arguments gf) 0))

;;;-----------------------------------------------------------------------------
;;; few-methods?
;;;-----------------------------------------------------------------------------

(defconstant $maximum-of-few-methods 8)
(defconstant $maximum-of-decisions 12)

(defglobal error-methods ())

(defun few-methods? (gf methods) ; *hf* domain removed
  (let ((da (?discrimination-arguments gf))
        (do (~generic-function-domain gf)))
    (if methods
        ;; (if (cdr methods) ; only one method
        (if (<= (length methods) $maximum-of-few-methods)
            (progn
              (dynamic-setq error-methods ())
              (let ((tree
                     (compute-decision-tree da do methods
                                            ;;(sort-methods da do gf methods)
                                            )))
                (if (or (method-def? tree)
                        (< (length tree) $maximum-of-decisions))
                    (progn
                      (if (dynamic error-methods)
                          (dynamic-setq error-methods
                                        (mk-error-method gf))
                        ())
                      tree)
                  ())))
          ()) ; more than few method
      ;; (car methods)) ; only one method
      ())) ; no methods
  )


;;(defun add-error-method (dom gf lst)
;;  (nconcat lst (list (cons dom)
;;                     (mk-error-method dom gf)))
;;  lst)

(defun mk-error-method (gf)
  (make-instance <method-def>
                 :fun
                 (make-instance <global-fun>
                                :identifier (list (?identifier gf))
                                :params (?params gf)
                                :range-and-domain (?range-and-domain gf)
                                :arg-num (compute-arg-descr (?params gf))
                                :inline ()
                                :body (error-body gf))))

;; !!!! hack
(defun error-body (gf)
  (make-instance <app>
                 :function no-applicable-method-error
                 :arg-list (cons gf
                                 (make-var-refs (?params gf))))
  )

;;;-----------------------------------------------------------------------------
;;; compute-decision-tree
;;;-----------------------------------------------------------------------------
;;;  The algorithm to compute the decision tree assumes that no class and no
;;;  method is added at runtime.

(defun compute-decision-tree (d-args gf-dom methods)
  (let ((tree (cons () ())))
    (if (compute-decision-tree1 0 d-args gf-dom methods tree)
        (cdr tree)
      ()))
  )

(defun compute-decision-tree1 (arg-nr d-args gf-dom methods tree)
  ;; find the next descriminating argument
  ;; (if tree ; decition depth was to large
  (if d-args
      (if (car d-args)
          (progn
            (compute-next-tree-level
             methods arg-nr
             (car gf-dom)
             () ; methods with method-dom = gf-dom
             tree) ; with (cdr tree) = ()
            ;; ? is the next level too large
            (if (cdr tree)
                (if (go-in-next-levels (cdr tree)
                                       (+ arg-nr 1)
                                       (cdr d-args)
                                       (cdr gf-dom))
                    tree ())
              ()))
        (compute-decision-tree1 (+ arg-nr 1) (cdr d-args)
                                (cdr gf-dom) methods tree))
    (progn
      (setf (cdr tree)
            (select-the-most-specific-method methods))
      tree))
  ;;    ())
  )

(defun go-in-next-levels (tree arg-nr d-args gf-dom)
  (if tree
      (let* ((item (car tree))
             (methods (cdr item)))
        (if (eq methods ^error) t
          (progn
            (setf (cdr item) ())
            (if (compute-decision-tree1
                 arg-nr d-args gf-dom methods item)
                (go-in-next-levels (cdr tree) arg-nr d-args gf-dom)
              ()))))
    t))


(defun compute-next-tree-level
  (methods arg-nr gf-dom gf-dom-meth tree)
  (if methods
      (let* ((meth (car methods))
             (m-dom (nth-method-domain arg-nr meth)))
        (if (eq m-dom gf-dom)
            ;; oh, a general method found
            (compute-next-tree-level (cdr methods) arg-nr gf-dom
                                     (cons meth gf-dom-meth)
                                     tree)
          (progn
            (insert-method m-dom meth tree)
            (compute-next-tree-level (cdr methods)
                                     arg-nr gf-dom gf-dom-meth tree))))
    (if (< (length (cdr tree)) $maximum-of-decisions)
        (progn
          ;; complete the tree with default or error methods
          (if (eq gf-dom %object)
              (if gf-dom-meth
                  (progn
                    (insert-methods-in-all-subtrees
                     gf-dom-meth (cdr tree))
                    (setf (cdr tree)
                          (nconcat (cdr tree)
                                   (list (cons t ; default-methods
                                               gf-dom-meth)))))
                (progn
                  (dynamic-setq error-methods t)
                  (setf (cdr tree)
                        (nconcat (cdr tree)
                                 (list (cons t ; default-methods
                                             ^error))))
                  ))
            (if gf-dom-meth
                (progn
                  (insert-methods-in-all-subtrees
                   gf-dom-meth (cdr tree))
                  (if (are-all-subclasses-in-tree
                       gf-dom tree)
                      (select-a-default-class tree)
                    (setf (cdr tree)
                          (nconcat (cdr tree)
                                   (list (cons t ; default-methods
                                               gf-dom-meth))))))
              (if (are-all-subclasses-in-tree
                   gf-dom tree)
                  (select-a-default-class tree)
                (progn
                  (dynamic-setq error-methods t)
                  (setf (cdr tree)
                        (nconcat (cdr tree)
                                 (list (cons t ; default-methods
                                             ^error))))))))
          tree)
      (progn
        (setf (cdr tree) ())
        tree)))
  )
(defun insert-method (m-dom meth tree)
  ;; Inserts a methods into a decision tree. The problems of this task were the
  ;; insertion of error methods and methods for abstract classes which must work
  ;; for all instantiable subclasses.
  (if (abstract-class? m-dom)
      (insert-method-loop (~class-subclasses m-dom) meth tree)
    (let ((item (assoc m-dom (cdr tree))))
      (if item
          (setf (cdr item)
                (cons meth (cdr item)))
        (setf (cdr tree)
              (cons (list m-dom meth)
                    (cdr tree))))
      (insert-method-loop (~class-subclasses m-dom) meth tree))
    ))

(defun insert-method-loop (dom-lst meth tree)
  (if dom-lst
      (progn
        (insert-method (car dom-lst)
                       meth tree)
        (insert-method-loop (cdr dom-lst) meth tree))
    tree))

(defun insert-methods-in-all-subtrees (methods tree)
  (if methods
      (progn
        (insert-method-in-all-subtrees (car methods) tree)
        (insert-methods-in-all-subtrees (cdr methods) tree))
    tree))

(defun insert-method-in-all-subtrees (method tree)
  (if tree
      (let ((item (car tree)))
        (setf (cdr item)
              (cons method
                    (cdr item)))
        (insert-method-in-all-subtrees method (cdr tree)))
    ()))

(defun are-all-subclasses-in-tree (gf-dom tree)
  (let ((n (length (cdr tree))))
    (if (= (sum-of-all-subclasses gf-dom) n)
        t ())))

(defun sum-of-all-subclasses (dom)
  (if (abstract-class? dom)
      (sum-of-all-subclasses-list (~class-subclasses dom))
    (+ 1 (sum-of-all-subclasses-list (~class-subclasses dom)))))

(defun sum-of-all-subclasses-list (lst)
  (if lst
      (sum-of-all-subclasses-list1
       (cdr lst)
       (sum-of-all-subclasses (car lst)))
    0))

(defun sum-of-all-subclasses-list1 (lst count)
  (if lst
      (sum-of-all-subclasses-list1
       (cdr lst)
       (+ count (sum-of-all-subclasses (car lst))))
    count))

(defun select-a-default-class (tree)
  ;; nothing specific known
  tree)


(defun nth-method-domain (n method)
  (nth-ele n (~method-domain method)))

(defun nth-ele (n li)
  (if (eq n 0) (car li)
    (nth-ele (- n 1) (cdr li))))

(defun select-the-most-specific-method (ml)
  (if (cdr ml)
      (s-m-s-m (car ml) (cdr ml))
    (car ml)))

(defun s-m-s-m (s-m ml)
  (if ml
      (let ((m (car ml)))
        (if (more-specific? (~method-domain m) (~method-domain s-m))
            (s-m-s-m m (cdr ml))
          (s-m-s-m s-m (cdr ml))))
    s-m))

(defun nconcat (l1 l2)
  (if l1 (if l2 (if (cdr l1)
                    (progn
                      (nconcat1 (cdr l1) l2)
                      l1)
                  (progn
                    (setf (cdr l1) l2)
                    l1))
           l1)
    l2))

(defun nconcat1 (l1 l2)
  (if (cdr l1)
      (nconcat1 (cdr l1) l2)
    (setf (cdr l1) l2)))

;;;-----------------------------------------------------------------------------
;;; gf-with-closure?
;;;-----------------------------------------------------------------------------


(defun gf-with-closure? (gf methods)
  ())

(defun gf-with-next-method? (gf methods)
  ())

;;;-----------------------------------------------------------------------------
;;; compute-std-discrfun-4-few-methods
;;;-----------------------------------------------------------------------------

(defun compute-std-discrfun-4-few-methods (gf few-methods)
  (if (method-def? few-methods) (?fun few-methods)
    ;; few-methods is a dedecision tree
    (dynamic-let ((in-generic-fun gf)
                  (next-method?arams (?params gf)))
                 (let* ((params (?params gf))
                        (d-vars (?discrimination-arguments gf))
                        (d-args (make-d-var-refs d-vars (?var-list params)))
                        (args
                         ;;              (if (?rest params)
                         ;;                     (make-var-refs-rest params)
                         ;;                     (make-var-refs params))
                         (if (?rest params)
                             (append (?var-list params) (list (?rest params)))
                           (?var-list params)))
                        (d-class-vars
                         (copy-to-class-vars d-args))
                        (body (make-instance <let*-form>
                                             :var-list (entrefs-var-list d-class-vars)
                                             :init-list (add-class-of d-args)
                                             :body (compute-if-cascade d-class-vars
                                                                       few-methods
                                                                       args)
                                             )))
                   (add-function
                    (make-instance <discriminating-fun>
                                   :identifier (list (?identifier gf))
                                   :params params
                                   :range-and-domain (?range-and-domain gf)
                                   :arg-num (compute-arg-descr params)
                                   :inline ()
                                   :body body
                                   :signature (?signature gf)
                                   ))))))

(defun entrefs-var-list (ref-lst)
  (if ref-lst
      (cons (?var (car ref-lst))
            (entrefs-var-list (cdr ref-lst)))
    ()))

(defun copy-to-class-vars (var-ref-lst)
  (if var-ref-lst
      (cons (make-instance <var-ref>
                           :var (make-instance <local-static>
                                               :identifier (list ^class-of
                                                                 (?identifier (?var (car var-ref-lst))))))
            (copy-to-class-vars (cdr var-ref-lst)))
    ()))

(defun add-class-of (var-list)
  (if var-list
      (cons
       (make-instance <app> :function %class-of
                      :arg-list (list (car var-list)))
       (add-class-of (cdr var-list)))
    ()))

(defun compute-if-cascade (cl-varl tree args)
  (if (cons? tree)
      (if (cdr tree)
          (let* ((cl-m (car tree))
                 (class (car cl-m))
                 (pred (make-instance <app>
                                      :function %eq ; ***test***
                                      :arg-list (list (car cl-varl) class)))
                 (then (compute-if-cascade (cdr cl-varl)
                                           (cdr cl-m)
                                           args))
                 (else (compute-if-cascade cl-varl (cdr tree) args)))
            (make-instance <if-form>
                           :pred pred
                           :then then
                           :else else))
        (compute-if-cascade (cdr cl-varl) (cdr (car tree)) args))
    (if (eq tree ^error)
        (inline-method (dynamic error-methods) args)
      (inline-method tree;; tree is a <method-def>
                     args))))
;;    (let* ((fun (?fun tree)) ; tree is a <method-def>
;;           (params (?params fun)))
;;      (if (?rest params)
;;        (progn
;;          (setf (?var-list params)
;;                (nconcat (?var-list params)
;;                         (list (?rest params))))
;;          (setf (?rest params) ())
;;          (setf (?arg-num fun) (compute-arg-descr params)))
;;        ())
;;      (make-instance <app>
;;        :function fun
;;        :arg-list args)



;;;-----------------------------------------------------------------------------
;;; Installing and Finding Standard Discriminating Functions
;;;-----------------------------------------------------------------------------

(deflocal *std-discr-fun-table* ())
;;*std-discr-fun-table* is a sorted list of descriptions of standard dispatching
;;functions; the order in this list is as follows:
;;without closure before with closure
;;without next-method before with next-method
;;fewer required parameters first
;;no rest parameter before rest parameter

(defstandardclass <std-discr-fun-descr> ()
  (discr-fun :reader :initarg)
  (required :reader :initarg)
  (rst :reader :initarg) ;; 0 or 1 for 'no' resp. 'yes'
  (next-method :reader :initarg)        ; 0 or 1 for 'no' resp. 'yes'
  (closure :reader :initarg))           ; 0 or 1 for 'no' resp. 'yes'

(defun reset-generic-dispatch ()
  (setq *std-discr-fun-table* ()))

(defun map-to-std-discr-funs (fun)
  (mapc (lambda (fun-descr)
          (funcall fun (?discr-fun fun-descr)))
        *std-discr-fun-table*))

(defun get-std-discr-fun (req next-method closure)
  (setq next-method (if next-method 1 0))
  (setq closure (if closure 1 0))
  (let ((descr
         (find-if (lambda (descr)
                    (and
                     (or (= (?rst descr) 1)
                         (= req (?required descr)))
                     (<= next-method (?next-method descr))
                     (<= closure (?closure descr))
                     ))
                  *std-discr-fun-table*)))
    (if descr (?discr-fun descr)
      (error "standard discriminating function missing for ~
                ~A argument~:P, with~[out~;~] next-method, with~[out~;~] closure"
             req
             next-method
             closure))))

(defun set-std-discr-fun (fun keyword description)
  (set-std-discr-fun1 fun
                      (member ^next-method description)
                      (member ^closure description)))

(defun set-std-discr-fun1 (fun next-method closure)
  (setq *std-discr-fun-table*
        (stable-sort     ;; to make sure that replacing goes right
         (cons (make-instance <std-discr-fun-descr>
                              :discr-fun fun
                              :next-method (if next-method 1 0)
                              :closure (if closure 1 0)
                              :required (- (length (?var-list (?params fun))) 1)
                              ;; the first argument is the generic function object which must
                              ;; not considered here
                              :rst (if (?rest (?params fun)) 1 0))
               *std-discr-fun-table*)
         (lambda (descr1 descr2)
           (or
            (and (< (?required descr1) (?required descr2))
                 (<= (?rst descr1) (?rst descr2))
                 (<= (?next-method descr1) (?next-method descr2))
                 (<= (?closure descr1) (?closure descr2)))
            (and (< (?rst descr1) (?rst descr2))
                 (<= (?next-method descr1) (?next-method descr2))
                 (<= (?closure descr1) (?closure descr2)))
            (and (< (?next-method descr1) (?next-method descr2))
                 (<= (?closure descr1) (?closure descr2)))
            (< (?closure descr1) (?closure descr2))
            )))))

;;;-----------------------------------------------------------------------------
;;; compute-std-discrfun
;;;-----------------------------------------------------------------------------

(defun compute-std-discrfun (gf closure next-method)
  (let* ((params (?params gf))
         (d-vars (?discrimination-arguments gf))
         (dd (?discrimination-depth gf))
         ;; (args (make-var-refs (?params gf))
         (body
          (make-instance <app>
                         :function %funcall
                         :arg-list
                         (cons
                          (make-instance <app>
                                         :function (get-std-discr-fun dd next-method closure)
                                         :arg-list (cons gf (make-d-var-refs d-vars
                                                                             (?var-list params))))
                          (if (?rest params)
                              (make-var-refs-rest params)
                            (make-var-refs params)))
                         )))
    ;;    (when (?rest params)
    ;;      (error "generic functions with rest parameter not yet implemented"))
    (add-function
     (make-instance <discriminating-fun>
                    :identifier (list (?identifier gf))
                    :params params
                    :range-and-domain (?range-and-domain gf)
                    :arg-num (compute-arg-descr params)
                    :inline t
                    :body body
                    :signature (?signature gf)
                    ))))

(defun make-d-var-refs (d-vars varl)
  (if d-vars
      (if (car d-vars)
          (cons (make-instance <var-ref> :var (car varl))
                (make-d-var-refs (cdr d-vars) (cdr varl)))
        (make-d-var-refs (cdr d-vars) (cdr varl)))
    ()))

(defun make-var-refs (params)
  ;;don't work for rest parameters
  (mapcar (lambda (req)
            (make-instance <var-ref> :var req))
          (?var-list params)))

(defun make-var-refs-rest (params)
  (make-var-refs-rest1 (?var-list params)
                       (list
                        (make-instance <var-ref>
                                       :var (?rest params)))))

(defun make-var-refs-rest1 (req-vars rest)
  (if req-vars
      (cons (make-instance <var-ref> :var (car req-vars))
            (make-var-refs-rest1 (cdr req-vars) rest))
    rest))

;;;-----------------------------------------------------------------------------
;;; ~compute-discriminating-function
;;;-----------------------------------------------------------------------------

(defmethod ~compute-discriminating-function
  ((gf <generic-fun>) domain lookup-fn methods)
  ;; methods may be empty !!!
  (let ((few-methods (few-methods? gf methods))
        (closure (gf-with-closure? gf methods))
        (next-method (gf-with-next-method? gf methods))
        )
    (if methods
        (if few-methods
            (compute-std-discrfun-4-few-methods gf few-methods)
          (compute-std-discrfun gf closure next-method)
          )
      (add-function (?fun (mk-error-method gf))))))

;;
;;(defmethod ~compute-discrimination-depth ((gf <defined-generic-fun>))
;;  (let ((methods (~generic-function-methods gf)))
;;    (if methods
;;      (let ((dom1 (~method-domain (car methods))))
;;        (discr-depth ; (~generic-function-domain gf)
;;         dom1 (cdr methods)
;;         0 (length dom1)))
;;      0)))
;;
;;(defun discr-depth (dom1 rest-methods depth leng)
;;  (if rest-methods
;;    (let* ((dom2 (~method-domain (car rest-methods)))
;;           (new-depth (calc-depth dom1 dom2 depth 0)))
;;      (if (> new-depth depth)
;;        (if (= new-depth leng) leng
;;            (discr-depth dom2 (cdr rest-methods) new-depth leng))
;;        (discr-depth dom2 (cdr rest-methods) depth leng)))
;;    depth))
;;
;;(defun calc-depth (dom1 dom2 d n)
;;  (if dom1
;;      (if (eq (car dom1) (car dom2))
;;         (calc-depth (cdr dom1) (cdr dom2) d (+ n 1))
;;       (calc-depth (cdr dom1) (cdr dom2) (+ n 1) (+ n 1)))
;;   d))
;;;-----------------------------------------------------------------------------
;;; ~compute-discrimination-offset
;;;-----------------------------------------------------------------------------
;;
;;(defmethod ~compute-discrimination-offset ((gf <defined-generic-fun>))
;;  (let ((methods (~generic-function-methods gf)))
;;    (if methods
;;      (if (cdr methods)
;;        (discr-offset (~method-domain (car methods))
;;                      (cdr methods) 999) ; a dummy number
;;        0)
;;      0)))
;;
;;(defun discr-offset (first-dom rest-methods offset)
;;  (if rest-methods
;;    (let* ((second-dom (~method-domain (car rest-methods)))
;;           (new-offset (discr-offset1 first-dom second-dom 0 offset)))
;;      (if (= new-offset 0) 0
;;          (discr-offset second-dom (cdr rest-methods) new-offset)))
;;    offset))
;;
;;(defun discr-offset1 (dom1 dom2 nr offset)
;;  (if (>= nr offset) offset
;;      (if dom1
;;        (if (eq (car dom1) (car dom2))
;;          (discr-offset1 (cdr dom1) (cdr dom2) (+ nr 1) offset)
;;          nr)
;;       nr)))



;;(defun sort-methods (d-args gf-dom gf methods)
;; result: ((dom1 . specific-method) .... (domn . general-method))
;;  (let* ((sortm (sort-methods1 (cdr methods)
;;                               (list (cons
;;                                      (~method-domain (car methods))
;;                                      (car method)))))
;;         (last (last-method sortm)))
;;    (if (more-specific? (car last)
;;                         gf-dom)
;;      (add-error-method gf-dom gf sortm)
;;      sortm)))
;;
;;(defun sort-methods1 (ms lst)
;;  (if (null? ms) lst
;;      (let* ((m (car ms))
;;             (mdom (~method-domain m)))
;;        (if (more-specific? mdom (car (car lst)))
;;          (sort-methods1 (cdr ms)
;;                         (cons (cons mdom m) lst))
;;          (progn
;;            (sort-methods2 mdom m lst)
;;            (sort-methods1 (cdr ms) lst)))))
;;)
;;
;;(defun sort-methods2 (mdom m lst)
;;  (if (cdr lst)
;;    (let ((nxdom (car (car (cdr lst)))))
;;      (if (more-specific? mdom nxdom)
;;          (setf (cdr lst)
;;                (cons (cons mdom m)
;;                      (cdr lst)))
;;          (sort-methods2 mdom m (cdr lst))))
;;    (setf (cdr lst)
;;          (cons (cons mdom m) ()))))
;;
;;(defun last-method (ml)
;;  (if (cdr ml) (last-method (cdr ml))
;;      (car ml)))
;;(defun compute-decision-tree (d-args methods)
;;  (let ((tree (cons () ())))
;;    (if (compute-decision-tree1 d-args methods tree)
;;      (cdr tree)
;;      ()))
;;)

;;(defun calc-d-tree (d-args gf-dom dom method tree)
;;  (if d-args
;;    (if (car d-args)
;;      (calc-d-tree1 (if (eq (car dom) (car gf-dom))
;;                      t (car dom))
;;                    (cdr dom)
;;                    (cdr d-args)
;;                    (cdr gf-dom)
;;                    method tree)
;;      (calc-d-tree (cdr d-args) (cdr gf-dom) (cdr dom) method tree))
;;    (let ((method1 (cdr tree)))
;;      (if (more-specific? (~method-domain method)
;;                           (~method-domain method1))
;;        (setf (cdr tree) method) ())
;;      tree)))

;;(defgeneric calc-d-tree1 (dom doml d-args gf-dom method tree))
;;
;;(defmethod calc-d-tree1 ((dom <standard-class-def>)
;;                         doml d-args gf-dom method tree)
;;  (let ((item (assoc dom (cdr tree)))
;;        (subcl (~class-subclasses dom)))
;;    (if item
;;      (calc-d-tree d-args gf-dom
;;                   doml
;;                   method item)
;;      (nconcat (cdr tree)
;;               (list
;;                (cons dom
;;                      (compute-subtree d-args gf-dom doml method)))))
;;    (if subcl
;;      (calc-d-list subcl doml d-args gf-dom method tree)
;;      tree)))
;;
;;(defmethod calc-d-tree1 ((dom <abstract-class-def>)
;;                         doml d-args gf-dom method tree)
;;  (let ((subcl (~class-subclasses dom)))
;;    (if subcl
;;      (calc-d-list subcl doml d-args gf-dom method tree)
;;      tree)))
;;
;;(defmethod calc-d-tree1 (dom doml d-args gf-dom method tree)
;;  (let ((item (assoc dom (cdr tree))))
;;    (if item
;;      (calc-d-tree d-args gf-dom
;;                   doml
;;                   method item)
;;      (nconcat (cdr tree)
;;               (list
;;                (cons dom
;;                      (compute-subtree d-args gf-dom doml method))))))
;;)
;;
;;(defun calc-d-list (cl-list doml d-args gf-dom method tree)
;;  (if cl-list
;;    (progn
;;      (calc-d-tree1 (car cl-list) doml d-args gf-dom method tree)
;;      (calc-d-list (cdr cl-list) doml d-args gf-dom method tree)
;;      tree)
;;    tree))
;;
;;(defun compute-subtree (d-args gf-dom doml method)
;; result: method | ((class . subtree) ...)
;;  (if d-args
;;    (if (car d-args)
;;      (compute-subtree1
;;       (if (eq (car doml) (car gf-dom))
;;         t (car doml))
;;       (cdr d-args) (cdr gf-dom) (cdr doml) method)
;;      (compute-subtree (cdr d-args) (cdr gf-dom) (cdr doml) method))
;;    method))
;;
;;(defgeneric compute-subtree1 (dom d-args gf-dom doml method))
;;
;;(defmethod compute-subtree1 ((dom <standard-class-def>)
;;                             d-args gf-dom doml method)
;;  (let ((item (cons dom
;;                    (compute-subtree d-args gf-dom doml method)))
;;        (subcl (~class-subclasses dom)))
;;    (if subcl
;;      (cons item
;;            (compute-subtree-list subcl d-args gf-dom doml method))
;;      (cons item ()))))
;;
;;(defmethod compute-subtree1 ((dom <abstract-class-def>)
;;                             d-args gf-dom doml method)
;;  (let ((subcl (~class-subclasses dom)))
;;    (if subcl (compute-subtree-list subcl d-args gf-dom doml method) ())))
;;
;;
;;(defun compute-subtree-list (cl-list d-args gf-dom doml method)
;;  (if cl-list
;;    (nconcat (compute-subtree1 (car cl-list)
;;                               d-args gf-dom doml method)
;;             (compute-subtree-list (cdr cl-list) d-args gf-dom doml method))
;;    ()))

;;;-----------------------------------------------------------------------------
#module-end
;;;-----------------------------------------------------------------------------
