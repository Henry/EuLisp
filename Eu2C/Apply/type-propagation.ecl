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
;;;  Title: 
;;;  Description:
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Horst Friedrich
;;;-----------------------------------------------------------------------------

#module type-propagation
(import
 ((except (format) eulisp1)
  SIMPLE-PROGRAMMING
  LZS
  MZS
  context
  analyse-h
  vector ; make-vector and vector-ref
  (only (assoc format logand ash) common-lisp)
  (only (valid-for-then? valid-for-else?) ti-special)
  type-inference)

 syntax
 (eulisp1)

 export
 (
  make-move-tds ; (tds var move)
  fill-type-vec ; (typevec curpath varvec from to) -> type-vec
  fill-type-vector-with-types ; (n td tvec) from n to 0
  make-formal-type-descr ;
  ;; (tpathes stat result con-type var-descr arg-num old-type-descr)
  ;;     -> type-descr-s
  make-actual-type-descr ; (tpathes stat var-descr arg-num old-type-descr)
  ;;######################     -> type-descr-s
  ;;make-formal-type-descr-with-result-type ;(pathes var-descr arg-num
  ;;#######################################   result-type old-type-descr)
  ;;#######################################    ->  type-descr-s
  ;;make-actual-type-descr-with-result-type ; (tpathes stat var-descr arg-num
  ;;#######################################    result-type old-type-descr)
  ;;    ->  type-descr
  link-var-vec ; (var-vec stat arg-num) - > vare-vec
  link-funcall-variable ; (var funcall) -> ()
  ;; propagate-type-descrs
  get-arg-type ; (var-or-constant type-path) -> type-expr
  select-then-type-descr ; type-descriptors
  select-else-type-descr ; type-descriptors
  set-slot-tds get-slot-tds
  )
 )



(defvar new-td-stats ())

(defun make-formal-type-descr
  (tpathes stat result con-type var-descr arg-num old-type-descr
           recursive)
  (make-formal-type-descr1 tpathes stat result con-type
                           (?var-vec var-descr)
                           arg-num old-type-descr recursive))

(defun make-formal-type-descr1
  (tpathes stat result con-type var-vec arg-num old-type-descr recursive)
  (if (null? tpathes) old-type-descr
    (let* ((curpath (car tpathes))
           ;;             (typevec (make-vector (+ arg-num 1)))
           ;;             (typedescr (make <formal-type-descr>
           ;;                              :stat stat
           ;;                              :t-descr-before curpath
           ;;                              :type-vec typevec
           ;;                              :type-spec 0))
           (typedescr (if recursive (empty-recursive-descr arg-num)
                        (empty-formal-descr arg-num)))
           )
      (setf (?stat typedescr) stat)
      (setf (?t-descr-before typedescr) curpath)
      (setf (?type-vars typedescr) (ti-copy-subs (?type-vars curpath))) ;*ak*
      (set-descr-type typedescr 0
                      (if con-type con-type (get-arg-type result curpath)))
      (fill-type-vec typedescr curpath var-vec 1 arg-num)
      (cons (reduce-descr typedescr)
            (make-formal-type-descr1
             (cdr tpathes) stat result con-type var-vec
             arg-num old-type-descr recursive)))))

(defun make-actual-type-descr (tpathes stat var-descr arg-num old-type-descr)
  (make-actual-type-descr1 tpathes stat (?var-vec var-descr)
                           arg-num old-type-descr))

(defun get-slot-tds (tds var stat)
  (if tds
      (let ((curpath (car tds))
            (typedescr (empty-actual-descr 1)))
        (setf (?stat typedescr) stat)
        (setf (?t-descr-before typedescr) curpath)
        (set-descr-type typedescr 0 (general-type))
        (set-descr-type typedescr 1 (get-arg-type var curpath))
        (cons typedescr (get-slot-tds (cdr tds) var stat)))
    ())
  )

(defun set-slot-tds (tds var init stat)
  (if tds
      (let ((curpath (car tds))
            (typedescr (empty-actual-descr 2)))
        (setf (?stat typedescr) stat)
        (setf (?t-descr-before typedescr) curpath)
        (set-descr-type typedescr 0 (general-type))
        (set-descr-type typedescr 1 (get-arg-type var curpath))
        (set-descr-type typedescr 2 (get-arg-type init curpath))
        (cons typedescr (set-slot-tds (cdr tds) var init  stat)))
    ())
  )

(defun make-actual-type-descr1 (tpathes stat var-vec arg-num old-type-descr)
  (if (null? tpathes) old-type-descr
    (let* ((curpath (car tpathes))
           ;;             (typevec (make-vector (+ arg-num 1)))
           ;;             (typedescr (make <act-type-descr>
           ;;                              :stat stat
           ;;                              :t-descr-before curpath
           ;;                              :type-vec typevec
           ;;                              :type-spec 0
           ;;                              :error-spec 0))
           (typedescr (empty-actual-descr arg-num)))
      (setf (?stat typedescr) stat)
      (setf (?t-descr-before typedescr) curpath)
      (fill-type-vec typedescr curpath var-vec 1 arg-num)
      (set-descr-type typedescr 0 (general-type))
      (cons typedescr
            (make-actual-type-descr1
             (cdr tpathes) stat var-vec arg-num old-type-descr)))))
;; *hf* 27.05
(defun make-move-tds (tds var move)
  (if (null? tds) ()
    (let* ((curp (car tds))
           (td (empty-actual-descr 1))
           (type (get-arg-type var curp)))
      (setf (?stat td) move)
      (setf (?t-descr-before td) curp)
      (set-descr-type td 0 type)
      (set-descr-type td 1 type)
      (get-previous-subs td)
      (cons td (make-move-tds (cdr tds) var move))))
  )


(defun link-var-vec (var-vec stat arg-anz)
  (link-var-vec1 var-vec stat 0 arg-anz))

(defun link-var-vec1 (var-vec stat nr arg-anz)
  (if (> nr arg-anz) var-vec
    (let ((var (vector-ref var-vec nr)))
      (cond ((or (local-static-p var)
                 (tempvar-p var))
             (setf (?link var)
                   (cons (cons stat nr)
                         (?link var)))))
      (link-var-vec1 var-vec stat (+ nr 1) arg-anz))))

(defun link-funcall-variable (var funcall)
  (cond ((or (local-static-p var)
             (tempvar-p var))
         (setf (?link var)
               (cons (cons funcall $funcall)
                     (?link var)))
         ;; set value-type
         (setf (?value-type funcall) (general-type)))

        ))

(defun fill-type-vec (typedescr curpath varvec from to)
  (if (> from to) typedescr
    (progn
      (set-descr-type typedescr from
                      (get-arg-type (vector-ref varvec from) curpath))
      (fill-type-vec typedescr curpath varvec (+ from 1) to))))

(defun fill-type-vector-with-types (n td tvec)
  (let ((typ (vector-ref tvec n)))
    (if typ
        (set-descr-type td n (class-as-type-expr typ))
      (set-descr-type td n (general-type)))
    ;;    (setf (vector-ref vect n)
    ;;          (if typ
    ;;            (class-as-type-expr typ)
    ;;            (general-type)))
    (if (= n 0) ()
      (fill-type-vector-with-types (- n 1) td tvec))))


;;(defun propagate-type-descrs (type-descrs call)
;;  (propagate-type-descr1 type-descrs call
;;                         (?var-vec (?var-descr call))
;;                         (?arg-num call)))
;;
;;(defun propagate-type-descr1 (tdescrs call varvec argnum)
;;  (if tdescrs
;;    (let* ((td (car tdescrs))
;;           (tspec (?type-spec td))
;;           (error ()))
;;      (if (= tspec 0) (propagate-type-descr1
;;                       (cdr tdescrs) call varvec argnum)
;;          ())
;;      (if (and (act-type-descr-p td)
;;               (> (?error-spec td) 0))
;;        (progn (setq tspec (?error-spec td))
;;               (setq error t))
;;        ())
;;      (setf (?type-spec td) 0)
;;      (prop-type tspec 0 argnum varvec (?type-vec td) (?path td) error)
;;      (propagate-type-descr1 (cdr tdescrs)call varvec argnum))
;;   ()))

;;(defun prop-type (tspec idx num varvec typevec path err)
;;  (if (> idx num) ()
;;      (progn
;;        (if (logand tspec 1)
;;          (prop-var-type (vector-ref varvec idx)
;;                         (vector-ref typevec idx)
;;                         path err)
;; !!!! global variables not correct implemented !!!
;;          ())
;;        (prop-type (ash tspec -1) (+ idx 1) num varvec typevec path err))
;;))

;;(defun prop-var-type (var type path err)
;;  (if (null? (or (tempvar-p var) (local-static-p var))) ()
;; !!!! global variables not correct implemented !!!
;;      (let ((new ()))
;;        (dynamic-let ((new-td-stats ()))
;;                   (prop-link-type path path (?link var) type err)
;;                   (setq new (dynamic new-td-stats)))
;;        (next-inference-steps new))))
;;
;;(defun next-inference-steps (td-st-list)
;;  (cond ((null? td-st-list) ())
;;        (t (let ((td (car (car td-st-list)))
;;                 (stat (cdr (car td-st-list))))
;;             (cond ((specialize-fun-app-type
;;                     (if (fun-p stat) stat (?function stat))
;;                     td
;;                     (?var-descr stat) (?arg-num stat))
;;                    (propagate-type-descrs (list td) stat)))
;; list is a hack (lack of propagate-type-descr) !!!
;;             (next-inference-steps (cdr td-st-list))))))

;;(defun prop-link-type (spath path link type err)
;;  (if (null? spath) ()
;;      (progn (find-link-to-set-type (car spath) link path type err)
;;             (prop-link-type (cdr spath) path link type err))))
;;
;;(defun find-link-to-set-type (block link path type err)
;;  (cond  ((null? link) ())
;;         ((eq (car (car link)) block)
;;          (set-type (car link) path type err)
;;          (find-link-to-set-type block (cdr link) path type err))
;;         (t (find-link-to-set-type block (cdr link) path type err))))
;;
;;(defun set-type (link path type err)
;; exist a typedesrc for the path ??
;; (let* ((where (car (cdr link)))
;;        (stat (car (cdr (cdr link))))
;;        (typedescrs (?type-descr-s stat))
;;        (curtd (find-type-descr path typedescrs)))
;;   (cond  ((< where 0) ()) ; !!! funcall not implemented
;;          (curtd
;;           (let ((tvec (?type-vec curtd)))
;;             (cond ((eq (vector-ref tvec where) type) () )
;;                   (t (setf (vector-ref tvec where) type)
;;                      (if (null? (assoc curtd (dynamic new-td-stats)))
;;                        (setf (dynamic new-td-stats)
;;                              (cons (cons curtd stat)
;;                                    (dynamic new-td-stats)))
;;                        ())))))
;;; make a new typedescr
;;          (t (let* ((curtd (if (fun-p stat)
;;                            (make <formal-type-descr>
;;                                  :path path
;;                                  :type-spec 0)
;;                            (make <act-type-descr>
;;                                  :path path
;;                                  :type-spec 0
;;                                  :error-spec (if err
;;                                                (ash 1 where)
;;                                                0))))
;;                    (argnum (?arg-num stat))
;;                    (typevec (make-vector (+ argnum 1)))
;;                    (varvec (?var-vec (?var-descr stat))))
;;               (fill-type-vec typevec path varvec 0 (- where 1))
;;               (setf (vector-ref typevec where) type)
;;               (fill-type-vec typevec path varvec (+ where 1) argnum)
;;               (setf (?type-vec curtd) typevec)
;;               (setf (?type-descr-s stat)
;;                     (cons curtd (?type-descr-s stat)))
;;               (setf (dynamic new-td-stats)
;;                              (cons (cons curtd stat)
;;                                    (dynamic new-td-stats)))
;;)))))




(defgeneric get-arg-type  (var-or-constant path))

;;;-----------------------------------------------------------------------------
;;; constants
;;;-----------------------------------------------------------------------------

(defmethod get-arg-type ((con <cast>) path)
  (class-as-type-expr (?type con))
  )

(defmethod get-arg-type ((con <named-const>) path)
  ;;
  ;; form = <defined-named-constant>, <imported-named-constant>
  ;;
  (constant-type con)
  )

(defmethod get-arg-type ((con <sym>) path)
  ;;
  ;; form = <defined-symbol>, <imported-symbol>
  ;;
  (constant-type con)
  )

(defmethod get-arg-type ((con <symbol>) path)
  ;; symbol is an slot-name
  ;;
  ;; form = <defined-symbol>, <imported-symbol>
  ;;
  (if (eq con ^unknown) (general-type)
    (constant-type con) )
  )


(defmethod get-arg-type ((con <structured-literal>) path)
  ;;
  ;; value = <vector>, <pair>, <string>, LITERAL-INSTANCE
  ;;
  (constant-type con)
  )

(defmethod get-arg-type ((con <fpi>) path)
  ;;
  ;; <fpi> - single precisition integer
  ;;
  (constant-type con)
  )

(defmethod get-arg-type ((con <double-float>) path)
  (constant-type con)
  )

(defmethod get-arg-type ((con <character>) path)
  (constant-type con)
  )

(defmethod get-arg-type ((con <class-def>) path)
  ;;
  ;;  form = <defined-class>, <imported-class>
  ;;
  (constant-type con)
  )

(defmethod get-arg-type ((con <literal-instance>) path)
  (constant-type con)
  )

(defmethod get-arg-type ((con <global-fun>) path)

  (constant-type con)
  )

(defmethod get-arg-type ((con <local-fun>) path)

  (constant-type con)
  )

(defmethod get-arg-type ((con <imported-fun>) path)

  (constant-type con)
  )

(defmethod get-arg-type ((con <special-sys-fun>) path)

  (constant-type con)
  )

(defmethod get-arg-type ((con <global-generic-fun>) path)

  (constant-type con)
  )

(defmethod get-arg-type ((con <local-generic-fun>) path)

  (constant-type con)
  )

(defmethod get-arg-type ((con <imported-generic-fun>) path)

  (constant-type con))

(defmethod get-arg-type ((con <cont>) path)
  ;; (print "get-arg-type <cont> not correct implemented~s")
  ;;(constant-type con)
  (%object-type)
  )

(defmethod get-arg-type ((con <null>) path)
  (constant-type con)
  )

;;;-----------------------------------------------------------------------

(defmethod get-arg-type ((var <local-static>) path)
  (get-var-type var path))

(defmethod get-arg-type ((var <tempvar>) path)
  (get-var-type var path))

(defun get-var-type (var path)
  (get-var-type1 var path (?link var)))

(defun get-var-type1 (var path varlink)
  (if (null? path) (general-type)
    (let* ((stat (?stat path))
           (link (assoc stat varlink)))
      (if (null? link) (get-var-type3 var (?t-descr-before path) varlink)
        (get-descr-type path (cdr link))))))

(defun get-var-type3 (var tds link)
  (if (null? tds) (general-type)
    (if (cons? tds)
        (let ((tp (get-var-type2 var (car tds) link)))
          (if tp tp
            (get-var-type3 var (cdr tds) link)))
      (get-var-type1 var tds link))))

(defun get-var-type2 (var path varlink)
  (if (null? path) ()
    (let* ((stat (?stat path))
           (link (assoc stat varlink)))
      (if (null? link) (get-var-type3 var (?t-descr-before path) varlink)
        (get-descr-type path (cdr link))))))

;;***HGW removed unused function
;; (defun find-type-descr (path typedescrs)
;;   (if (null? typedescrs) ()
;;     (let ((typedescr (car typedescrs)))
;;       (if (eq  (?path typedescr) path) typedescr
;;         (find-type-descr path (cdr typedescrs))))))


(defmethod get-arg-type ((var <global-static>) path)
  (let ((typ (?type var)))
    (if (null? typ) (%object-type)
      (class-as-type-expr typ)))
  )

(defmethod get-arg-type ((var <imported-static>) path)
  (let ((typ (?type var)))
    (if (null? typ) (%object-type)
      (class-as-type-expr typ)))
  )

(defmethod get-arg-type ((var <dynamic>) path)
  (%object-type))

(defun select-then-type-descr (fun td)
  (if td
      (let ((curtd (car td)))
        (if (valid-for-then? fun (car td))
            (cons curtd (select-then-type-descr fun (cdr td)))
          (select-then-type-descr fun (cdr td)))
        )
    ()))

(defun select-else-type-descr (fun td)
  (if td
      (let ((curtd (car td)))
        (if (valid-for-else? fun (car td))
            (cons curtd (select-else-type-descr fun (cdr td)))
          (select-else-type-descr fun (cdr td))
          ))
    ()))

#module-end
