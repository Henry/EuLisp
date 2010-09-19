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
;;; Title: balanced all types and add types to move-statements
;;;  Description:
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Horst Friedrich
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

#module cleartypes
(import ((except (format)
                 level-0)
         dynamic
         LZS
         MZS
         type-inference
         ti-signature
         vector
         predicates
         (only (%void)
               tail-module)
         (only (mapc
                mapcar
                format
                append)
               common-lisp))
 syntax (level-0
         dynamic)
 export (clear-types1
         clear-types2
         clear-types2-global-optimization
         clear-types3))

(defglobal move-vars ())
(defglobal generic-calls ())

;;; Condense generic type schemes (type-descr-s) of statements to one-line
;;; schemes (type-descr).
(defun clear-types1 (fun)
  (if (and (simple-fun? fun)
           (or (= (?pass fun) 3)
               (= (?pass fun) 5)))
      (let ((calls (?calls fun))
            (rcalls (?rec-calls fun)) ; *hf* 17.02
            (tests (?tests fun))
            (moves (?moves fun))
            (get-slot-value (?get-slot-value fun))
            (set-slot-value (?set-slot-value fun)))
        ;;      (format t ".") ; for debug
        (dynamic-let ((move-vars ())
                      (generic-calls ()))
                     (mapc #'balance-and-clear-types-calls calls)
                     (mapc #'balance-and-clear-types rcalls) ; *hf* 17.02
                     (mapc #'balance-and-clear-types tests)
                     (mapc #'balance-and-clear-types get-slot-value)
                     (mapc #'balance-and-clear-types set-slot-value)
                     (mapc #'handle-moves moves)
                     (balance-multiple-assignment (dynamic move-vars))
                     (optimize-generic-calls (dynamic generic-calls))))))

;;; Condense generic type schemes (signature) of functions to one-line
;;; schemes (type-descr). Global optimization is performed.
(defun clear-types2-global-optimization (fun)
  (if (and (simple-fun? fun) (= (?pass fun) 3))
      (let* ((applications (append (?rec-calls fun) (?applications fun)))
             (typedescr (cond ((null? (?applications fun))  ; e.g. init funs
                               (balance (?signature fun)))
                              ((unknown-applications? fun); e.g. exported
                               (balance (?signature fun)))
                              (t
                               (balance-applications fun applications)))))
        (setf (?type-descr fun) typedescr)
        (setf (?stat typedescr) fun))
    (setf (?type-descr-s fun) ())))

;;; Condense generic type schemes (signature) of functions to one-line
;;; schemes (type-descr). No global optimization is performed.
(defun clear-types2 (fun)
  (if (and (simple-fun? fun) (= (?pass fun) 3))
      (progn
        (setf (?type-descr fun) (balance (?signature fun)))
        (setf (?type-descr-s fun) ()))))

;;; Convert type expressions to classes.
(defun clear-types3 (fun)
  (if (null? (signature-needed-for-code-generation? fun))
      (setf (?signature fun) ()))       ; no longer needed
  (if (and (simple-fun? fun)
           (or (= (?pass fun) 5)
               (= (?pass fun) 3)))
      (let ((calls (?calls fun))
            ;;  (rcalls (?rec-calls fun)) : *hf* 17.02
            (tests (?tests fun))
            (moves (?moves fun))
            (get-slot-value (?get-slot-value fun))
            (set-slot-value (?set-slot-value fun)))
        (convert-types fun)
        (check-if-result-type-%void fun)
        (mapc #'convert-types calls)
        ;; (mapc #'convert-types rcalls)
        (mapc #'convert-types tests)
        (mapc #'convert-types moves)
        (mapc #'convert-types get-slot-value)
        (mapc #'convert-types set-slot-value))))

(defun check-if-result-type-%void (fun)
  (if (and (?range-and-domain fun)
           (eq (vector-ref (?range-and-domain fun) 0) %void))
      (setf (vector-ref (?type-descr fun) 0) %void)))

(defun convert-types (stat)
  (setf (?type-descr stat)
        (convert-to-sys-type-vec (?type-descr stat))))

(defun optimize-generic-calls (call-list)
  (if call-list
      call-list
    ;; (format t "~% Generic-calls ~s" call-list)
    ()))

(defun balance-multiple-assignment (vars)
  (if vars
      (progn
        (balance-multiple-assignment-var (car vars))
        (balance-multiple-assignment (cdr vars)))
    ()))

(defun balance-multiple-assignment-var (var)
  (set-joined-result-types (collect-assignment (?link var))))

;; (defun union-result-types (list-of-td)
;;  (format t "~% Union ~s " list-of-td))

(defun collect-assignment (link)
  (if link
      (if (eq (cdr (car link)) 0)
          (cons (?type-descr (car (car link)))
                (collect-assignment (cdr link)))
        (collect-assignment (cdr link)))
    ()))

(defun balance-and-clear-types (stat)
  (cond ((?type-descr-s stat)
         (setf (?type-descr stat) (balance (?type-descr-s stat)))
         (setf (?type-descr-s stat) ()))))

(defun balance-and-clear-types-calls (call)
  (balance-and-clear-types call)
  (if (and (generic-fun? (?function call))
           (null? (member call (dynamic generic-calls))))
      (dynamic-setq generic-calls (cons call (dynamic generic-calls))))
  )

(defun handle-moves (move) ; *hf* 27.05
  (let ((tds (?type-descr-s move))
        (td (?type-descr move))
        (var (vector-ref (?var-vec (?var-descr move)) 0)))
    (if tds
        (setf (?type-descr move) (balance tds))
      (setf (?type-descr move) td))
    (if (and (or (tempvar? var)
                 (local-static? var))
             (null? (member var (dynamic move-vars)))
             (more-than-one-assignment (?link var)))
        (dynamic-setq move-vars (cons var (dynamic move-vars)))
      ()))
  (setf (?type-descr-s move) ())
  )

(defun more-than-one-assignment (link)
  (if link
      (if (eq (cdr (car link)) 0)
          (more-than-one-assignment-aux (cdr link))
        (more-than-one-assignment (cdr link)))
    ()))

(defun more-than-one-assignment-aux (link)
  (if link
      (if (eq (cdr (car link)) 0)
          t
        (more-than-one-assignment-aux (cdr link)))
    ()))

;;;-----------------------------------------------------------------------------
#module-end
;;;-----------------------------------------------------------------------------
