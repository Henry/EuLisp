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
;;;  Title: el-conditions in cl
;;;  Description:
;;    fake of the EL-Condition system
;;;  Problems:
;;    system rised conditions cannot have a continuation
;;    here one can only accept a condition
;;    Error and cerror not exported because of package conflicts in n-1 files
;;;  Authors: E. Ulrich Kriegel
;;;-----------------------------------------------------------------------------

#module el-conditions
(import
 (eulisp-kernel
  el-conditions-0)

 syntax
 (eulisp-kernel el-conditions-0)



 expose ((only (defcondition with-handled-conditions  let/cc) el-conditions-0))
 export ( signal
          condition-message
          <condition>
          <execution-condition>
          <invalid-operator>
          <bad-apply-argument>
          <cannot-update-setter>
          <no-setter>
          <improper-unquote-splice>
          <environment-condition>
          <arithmetic-condition>
          <division-by-zero>
          <conversion-condition>
          <no-converter>
          <stream-condition>
          <syntax-error>
          <thread-condition>
          <telos-condition>
          <no-next-method>
          <no-congruent-lambda-list>
          <incompatible-method-signature>
          <no-applicable-method>))


;;definitions and init-forms



;;;-----------------------------------------------------------------------------
;;; Definition of EL level-0-condition classes
;;;-----------------------------------------------------------------------------

(defcondition <condition>
              ()
              ((continuation :accessor   condition-continuation
                             :initarg :continuation)
               (message :accessor condition-message :initarg message)))

(defcondition <execution-condition>(<condition>)())
(defcondition <invalid-operator>(<execution-condition>)())
(defcondition <bad-apply-argument>(<execution-condition>)())
(defcondition <cannot-update-setter>(<execution-condition>)())
(defcondition <no-setter>(<execution-condition>)())
(defcondition <improper-unquote-splice>(<execution-condition>)())
(defcondition <environment-condition>(<condition>)())
(defcondition <arithmetic-condition>(<condition>)())
(defcondition <division-by-zero> (<arithmetic-condition>)())
(defcondition <conversion-condition>(<condition>)())
(defcondition <no-converter>(<conversion-condition>)())
(defcondition <stream-condition>(<condition>)())
(defcondition <syntax-error>(<condition>)())
(defcondition <thread-condition>(<condition>)())
(defcondition <telos-condition>(<condition>)())
(defcondition <no-next-method>(<telos-condition>)())
(defcondition <no-congruent-lambda-list>(<telos-condition>)())
(defcondition <incompatible-method-signature>(<telos-condition>)())
(defcondition <no-applicable-method>(<telos-condition>)())

(defun signal (condition continuation)
  (if(cl:subtypep (cl:class-of condition) <condition>)
      (setf (condition-continuation condition) continuation)
    ())
  (cl:signal condition))


#module-end



