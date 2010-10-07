;; File   : Eulisp input file; wm.em
;; Date   : 30 Jan 1995
;; Author : Tracy Gardner
;; Description: Working Memory Module for OPS5.
(defmodule wm
  (syntax (macros macros-tag)
   import (level1 basic prod-gf wm-gf cond-el-gf conflict ops-out))

(print "### wm" nl)

;;;-----------------------------------------------------------------------------
;;; <wm-clock>
; Working Memory clock. Use get-timestamp method to get a timestamp and
; advance the clock by one.
;;;-----------------------------------------------------------------------------
(defclass <wm-clock> ()
  ((current-time
    default: 0 ;; to make timestamps match up (change to 1)
    reader:  gettime
    writer:  set-time))

  constructor: (make-wm-clock))

;;;-----------------------------------------------------------------------------
;;; get-timestamp
; Gets a timestamp from a <wm-clock> and advances it by one.
;;;-----------------------------------------------------------------------------
(defgeneric get-timestamp (clock)
  method: (((clock <wm-clock>))
           (let ((current (gettime clock)))
             (set-time clock (+ current 1))
             current)))

;;;-----------------------------------------------------------------------------
;;; <wm-element>
; Working Memory Element, used to model user defined working memory
; elements.
;;;-----------------------------------------------------------------------------
(defclass <wm-element> ()
  ((class-type
    keyword: c-name:
    reader:  c-name)
   (time-stamp
    keyword: ts:
    reader:  timestamp
    writer:  set-timestamp)
   (attrib-vals
    keyword: attrib-vals:
    reader:  attrib-vals
    writer:  set-attrib-vals))
  constructor: (make-wm-element c-name: ts: attrib-vals:))

(defmethod wme-class-name ((wme <wm-element>))
  (c-name wme))

(defmethod wme-attrib-vals ((wme <wm-element>))
  (attrib-vals wme))

(defmethod wme-timestamp ((wme <wm-element>))
  (timestamp wme))

;;;-----------------------------------------------------------------------------
;;; <working-memory>
; Used to store working memory elements, elements are keyed by timestamp.
; for now this will be a simple member-alistiation list
;;;-----------------------------------------------------------------------------
(defclass <working-memory> ()
  ((wm-elements
    default: ()
    reader:  wm-elements
    writer:  set-wm-elements))

  constructor: (make-working-memory))

;;;-----------------------------------------------------------------------------
;;; wm-insert
; Used by wm-manager to insert a new working memory element
;;;-----------------------------------------------------------------------------
(defgeneric wm-insert (wm wm-element)
  method: (((wm <working-memory>) (wm-element <wm-element>))
           ;;(print "Inserting new WME:" nl)
           ;;(format "~a: ~a ~a~%" (c-name wm-element)
           ;;     (attrib-vals wm-element)
           ;;   (timestamp wm-element))
           (set-wm-elements wm
                            (cons (cons
                                   (timestamp wm-element)
                                   wm-element)
                                  (wm-elements wm)))))

;;;-----------------------------------------------------------------------------
;;; wm-remove
; Used by WM-Manager to remove a working memory element
;;;-----------------------------------------------------------------------------
(defgeneric wm-remove (wm timestamp)
  method: (((wm <working-memory>) timestamp)
           ;;(format "Removing timestamp: ~a~%:" timestamp)
           (let ((rem (member-alist timestamp (wm-elements wm))))
             (when (null? rem) (sformat ops-out
                                        "Remove FAILED: ~a~%" timestamp))
             (set-wm-elements wm (list-remove rem (wm-elements wm)))
             (cdr rem))))

;;;-----------------------------------------------------------------------------
;;; <wm-manager>
; Working Memory manager, controls insertions to and deletions from working
; memory. Informs Condition Element manager and Conflict Resolution manager
; of changes when necessary. Uses <wm-clock> to assign a unique timestamp to
; each new <wm-element>.
;;;-----------------------------------------------------------------------------
(defclass <wm-manager> ()
  ((working-mem
    default: (make-working-memory)
    reader:  working-mem)
   (clock
    default: (make-wm-clock)
    reader:  clock))

  constructor: (make-wm-manager))

;;;-----------------------------------------------------------------------------
;;; insert-wme
; Insert a new working memory element into working memory with a new
; timestamp. Also inform condition-element manager of change so that affected
; condition elements can be updated.
;;;-----------------------------------------------------------------------------
(defgeneric insert-wme (wm-manager ce-manager cr-manager class-name bindings)
  method: (((wm-manager <wm-manager>) ce-manager cr-manager
            class-name bindings)
           ;;(print "insert-wme" nl)
           (let ((new-wme
                  (make-wm-element class-name
                                   (get-timestamp
                                    (clock wm-manager))
                                   bindings)))
             (wm-insert (working-mem wm-manager) new-wme)
             ;;(print "Wme inserted" nl)
             (inform-of-insert ce-manager new-wme cr-manager))))

;;;-----------------------------------------------------------------------------
;;; remove-wme
; Remove the working memory element with the given timestamp from working
; memory. Also inform the condition-element manager of the change so that
; affected condition elements can be updated and inform the conflict resolver
; so that production instantiations containing the removed working memory
; element can be removed from the conflict set.
;;;-----------------------------------------------------------------------------
(defgeneric remove-wme (wm-manager ce-manager cr-manager timestamp)
  method: (((wm-manager <wm-manager>) ce-manager cr-manager timestamp)
           (let ((rem-wme (wm-remove (working-mem wm-manager) timestamp)))
             (inform-of-remove ce-manager rem-wme cr-manager)
             (inform-of-remove cr-manager rem-wme cr-manager)
             rem-wme)))

;;;-----------------------------------------------------------------------------
;;; inform-of-insert
; informs a condition element manager of the addition of a new working
; memory element
;;;-----------------------------------------------------------------------------
(defun inform-of-insert (ce-manager new-wme cr-manager)
  (match-insert ce-manager new-wme cr-manager))

;;;-----------------------------------------------------------------------------
;;; inform-of-remove
; informs a condition element manager or conflict resolution manager of the
; removal of a working memory element
;;;-----------------------------------------------------------------------------
(defgeneric inform-of-remove (manager rem-wme cr-manager)
  method: (((ce-manager <ce-manager>) (rem-wme <wm-element>) cr-manager)
           (match-remove ce-manager rem-wme cr-manager))

  method: (((cr-manager <cr-manager>) (rem-wme <wm-element>) cr-manager)
           (remove-by-timestamp cr-manager (timestamp rem-wme))))
(export make-wm-manager insert-wme remove-wme)

;;;-----------------------------------------------------------------------------
)  ;; End of module
;;;-----------------------------------------------------------------------------
