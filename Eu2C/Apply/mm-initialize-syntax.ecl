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
;;;-----------------------------------------------------------------------------

#module mm-initialize-syntax
(import
 (representation
  tail-module
  (only (?var) accessors)
  (only (get-option check-options) option-lists)
  level-0
  expand-literal
  (only ($mtss $stss $stms max-used-card-descriptor max-used-type-descriptor) apply-funs)
  )
 syntax
 (level-0
  (rename ((incf cl:incf)
           (push cl:push))
          (only (incf push ) common-lisp)))
 export ( create-runtime-cdscr-default)
 )




;;;-----------------------------------------------------------------------------
;;; define a dummy generic function to solve package problems
;;;-----------------------------------------------------------------------------

(defgeneric create-runtime-cdscr-default (class cdscr cardtype size tdscr))

(defmacro canonize-multiple-card-descriptors
  (class representation-object size mm-type card-type key descriptor-list )
  `(let ((cds (get-option ,key ,descriptor-list ())))
     (if cds

         (progn

           (if (eq (?var ,card-type) $mtss)

;;;set card descriptor again to be independent of module load
;;;hierarchy
               (create-runtime-cdscr-default ,class cds
                                              ,card-type ,size ,mm-type)
             ())
           ;;use existing card descriptor
           (setf(?mm-card ,representation-object)
                (literal-instance %signed-word-integer cds))
           )
       ;;add new cds to list of descriptors and create run-time
       ;;initialization form
       (progn
         (cl:incf max-used-card-descriptor)
         (cl:push max-used-card-descriptor
                  ,descriptor-list)
         (cl:push ,key ,descriptor-list)
         (setf (?mm-card ,representation-object)
               (literal-instance
                %signed-word-integer max-used-card-descriptor))
         (create-runtime-cdscr-default ,class max-used-card-descriptor ,card-type ,size ,mm-type)))))

(defmacro literal-instance
  (type . values )
  `(make-literal-instance ,type
                          (list ,@values)))



#module-end
