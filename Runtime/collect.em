;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: level1
;;;  Authors: Andreas Kind, Julian Padget
;;; Description: collections
;;;-----------------------------------------------------------------------------
(defmodule collect
  (syntax (_telos0)
   import (telos compare)
   export (<collection> collectionp <sequence> sequencep
           accumulate accumulate1 anyp allp do fill map member
           element concatenate emptyp size reverse sort find select
           delete remove reset name reverse! sort!))

;;;-----------------------------------------------------------------------------
;;; Classes <collection> and <sequence>
;;;-----------------------------------------------------------------------------
  (defclass <collection> (<object>) ()
    abstractp: t)

  (defclass <sequence> (<collection>) ()
    keywords: (size: fill-value:)
    abstractp: t)

;;;-----------------------------------------------------------------------------
;;; Predicates; should return t (not x when generated) in positive case
;;;-----------------------------------------------------------------------------
  (defgeneric collectionp (x)
    method: (((x <object>)) ())
    method: (((x <collection>)) t))

  (defgeneric sequencep (x)
    method: (((x <object>)) ())
    method: (((x <sequence>)) t))

  (defmethod equal ((c1 <collection>) (c2 <collection>))
    (and (eq (class-of c1) (class-of c2))
         (if (listp c1)
             (allp equal c1 c2)
           (and (int-binary= (size c1) (size c2))
                (allp equal c1 c2)))))

;;;-----------------------------------------------------------------------------
;;; Interations
;;;-----------------------------------------------------------------------------
  (defgeneric accumulate (f i c))
  (defgeneric accumulate1 (f c))
  (defgeneric anyp (f c . cs))
  (defgeneric allp (f c . cs))
  (defgeneric do (f c . cs))
  (defgeneric map (f c . cs))
  (defgeneric fill (c x . keys))
  (defgeneric member (v c . f))
  (defgeneric find (v c . f))
  (defgeneric select (f c . cs))
  (defmethod find (x (c <collection>) . preds)
    (apply member x c preds))

;;;-----------------------------------------------------------------------------
;;; Access
;;;-----------------------------------------------------------------------------
  (defgeneric element (c s))
  (defgeneric (setter element) (c s v))
  (defgeneric delete (c s . pred)) ;; destructive
  (defgeneric remove (c s . pred)) ;; non-destructive
  (defgeneric reset (x)) ;; also used for streams

;;;-----------------------------------------------------------------------------
;;; Length
;;;-----------------------------------------------------------------------------
  (defgeneric emptyp (o))
  (defgeneric size (c))

;;;-----------------------------------------------------------------------------
;;; Reverse
;;;-----------------------------------------------------------------------------
  (defgeneric reverse (c)) ;; non-destructive
  (defgeneric reverse! (c)) ;; destructive

;;;-----------------------------------------------------------------------------
;;; Sort
;;;-----------------------------------------------------------------------------
  (defgeneric sort (c . comp)) ;; non-destructive
  (defgeneric sort! (c . comp)) ;; destructive

;;;-----------------------------------------------------------------------------
;;; Concatenate
;;;-----------------------------------------------------------------------------
  (defgeneric concatenate (c . cs))

;;;-----------------------------------------------------------------------------
;;; Name
;;;-----------------------------------------------------------------------------
  (defgeneric name (x)
    method: (((x <symbol>)) (symbol-name x))
    method: (((x <keyword>)) (keyword-name x))
    method: (((x <function>)) (function-name x))
    method: (((x <class>)) (class-name x))
    method: (((x <slot>)) (slot-name x)))

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------