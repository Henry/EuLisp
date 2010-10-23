;;; Copyright 1997 J. Garcia & University of Bath
;;; Copyright 2010 Henry G. Weller
;;;-----------------------------------------------------------------------------
;;  This file is part of
;;; ---                         EuLisp System 'Youtoo'
;;;-----------------------------------------------------------------------------
;;
;;  Youtoo is free software: you can redistribute it and/or modify it under the
;;  terms of the GNU General Public License version 2 as published by the Free
;;  Software Foundation.
;;
;;  Youtoo is distributed in the hope that it will be useful, but WITHOUT ANY
;;  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;  details.
;;
;;  You should have received a copy of the GNU General Public License along with
;;  this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;-----------------------------------------------------------------------------
;;; Title: tk_browser application
;;;  Library: tcltk
;;;  Authors: J. Garcia
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule tk_browser
  (syntax (macros)
   import (level1
           tcltk)
   export (inspect))

;;;-----------------------------------------------------------------------------
;;; Global variables
;;;-----------------------------------------------------------------------------

;; The next global variables will contain global widgets.
(deflocal *lb-slots* 0)
(deflocal *lb-previous* 0)
(deflocal *info-title* 0)
(deflocal *info-text* 0)
(deflocal *info-hierchy* 0)

;; The next ones will contain global information.
;;
;; *objects-inspected*  A list with the objects that have been
;;                        inspected during the navigation.
;;                        The limit of this list is 10 objects.
;;                        The car of the list is the parent of the
;;                        actual-object.
;; *actual-object*  The object that is being inspected
(deflocal *objects-inspected* ())
(deflocal *actual-object* 0)

;;;-----------------------------------------------------------------------------
;;                     General Callbacks functions.
;;;-----------------------------------------------------------------------------

;; The next function is called when the mouse is clicked once
;;  over one of the slots in the list of slots.
;;  Depending on the slot clicked, it will show the requested
;;  information.
(defun show-slot-information ()
  (let* ((name-slot (tk-selection-get))
         auxtext)
    (cond ((eq name-slot 'Self)
           (setq auxtext (fmt "~a" *actual-object*))
           (tk-conf-widget *info-title* text: "Object Itself"))

          ((eq name-slot 'Class)
           (setq auxtext (fmt "~a" (class-of *actual-object*)))
           (tk-conf-widget *info-title* text: "Class"))

          ((and (string? *actual-object*) (eq name-slot 'data))
           (setq auxtext (fmt "~a" *actual-object*))
           (tk-conf-widget *info-title* text: "Slot Information"))

          (t
           (setq auxtext (fmt "~a" (slot-value *actual-object* name-slot)))
           (tk-conf-widget *info-title* text: "Slot Information")))
    (tk-delete *info-text* "1.0" "end")
    (tk-insert *info-text* "end" auxtext)))

;; The next function will add the *actual-object* to the
;; *objects-inspected* list. But it will check the size
;; of the list, to avoid having a list with more than
;; 10 objects. If that is the case, the last one is throw
;; it away.
(defun add-actual-object ()
  (if (= (list-size *objects-inspected*) 10)
      (setq *objects-inspected*
            (cons *actual-object*
                  (remove-element *objects-inspected* 9)))
    (setq *objects-inspected*
          (cons *actual-object* *objects-inspected*))))

;; This function will be called when the mouse is clicked twice
;;  over one of the slots in the list of slots.
;;  This function will arrange everything to go to the class
;;  of the slot clicked.
;;
(defun go-level-down ()
  (let* ((name-slot (tk-selection-get))
         auxtext)
    ()
    (cond
      ((and (string? *actual-object*) (eq name-slot 'data))
       (tk-conf-widget *info-title* text: (fmt "Information Message"))
       (tk-delete *info-text* "1.0" "end")
       (tk-insert *info-text* "end" "C representation"))

      ((eq name-slot 'Self)
       ())

      (t
       (add-actual-object)
       (setq *actual-object*
             (if (eq name-slot 'Class)
                 (class-of *actual-object*)
               (slot-value *actual-object* name-slot)))

       (generate-layout)))))

;;  The next function deletes the element in the position
;; specified of the list
(defun remove-element (list pos)
  (let ((i 0)
        (result ()))
    (do (lambda (el)
          (progn
            (and (null? (eq i pos))
                 (setq result (cons el result)))
            (setq i (+ i 1))))
        list)
    (reverse result)))

;;  The next function goes to a previous object that has been
;;   inspected before. This is a callback function, and it
;;   will be called when a Double click occurs in the listbox
;;   with the objects inspected.
(defun go-previous-object ()
  (cond
    (*objects-inspected*
     (let ((position (convert
                      (car (tk-listbox-curselection *lb-previous*))
                      <fpi>)))
       (setq *actual-object* (element *objects-inspected* position))
       (setq *objects-inspected* (remove-element *objects-inspected* position))
       (generate-layout)))))

;;  The next function will update all the widgets in the
;;   main window, according with the *actual-object*
;;   *actual-slots* and *objects-inspected*.
(defun generate-layout ()
  (let ((i 0)
        auxtext list-slots aux-list-objects)

    (tk-delete *lb-slots* "0" "end")
    (tk-delete *lb-previous* "0" "end")

    (tk-insert *lb-slots* "end" "Self")
    (tk-insert *lb-slots* "end" "Class")


    ;; Firstly, it will fill the list of slots with the slots of the
    ;; *actual-object*. The *lb-slots* will be updated.

    (do (lambda (s)
          (tk-insert *lb-slots* "end" (symbol-name (slot-name s))))
        (class-slots (class-of *actual-object*)))

    ;; Secondly, the text widget and the information widget
    ;; will contain information about the object.


    (tk-conf-widget *info-title* text: "Object Itself")
    (tk-delete *info-text* "1.0" "end")
    (tk-insert *info-text* "end" (fmt "~a" *actual-object*))

    ;; Finally the listbox with the list of the previous
    ;; objects inspected will be updated.

    (do (lambda (o)
          (tk-insert *lb-previous* "end" (fmt "~a" o)))
        *objects-inspected*)
    ))

;;;-----------------------------------------------------------------------------
;;                              Inspect Method
;;;-----------------------------------------------------------------------------

;; The next function creates all the widgets that are necessary.
;;   It generates part of the layout. The one that is going to be
;;   keep the whole application.
(defgeneric inspect (self))

(defmethod inspect (self)
  (tk-wm "title" () "Browser")
  (tk-wm "geometry" () "800x800")
  (setq *actual-object* self)
  (let* ((right (tk-make-frame () relief: "sunken" bd: "2"))
         (decoration (tk-make-frame right relief: "sunken" bd: "2"))
         (scroll-text (tk-make-scrollbar right))

         (left (tk-make-frame () width: "2c" height: "10c"
                              relief: "sunken" bd: "3"
                              bg: "SteelBlue"))
         (left-up (tk-make-frame left width: "2c" height: "6c"
                                 relief: "sunken" bg: "SteelBlue"))
         (left-down (tk-make-frame left width: "2c" height: "4c"
                                   relief: "sunken" bg: "SteelBlue"))
         (scroll-slots (tk-make-scrollbar left-up bg: "blue4"))
         (exit-button (tk-make-button left-down text: "Exit" fg: "red"
                                      command: tk-exit))
         (label1 (tk-make-label left-up text: "List slots"
                                fg: "white" bg: "SteelBlue"))
         (label2 (tk-make-label left-down text: "Previous objects"
                                fg: "white" bg: "SteelBlue"))
         )


    ;; Global widgets in the rhs. They have to be known by the whole module.
    (setq *info-title* (tk-make-label right fg: "SeaGreen4"))
    (setq *info-text* (tk-make-text decoration width: "30" bd: "2"))

    (eul-associate *info-text* scroll-text 'vertical)

    ;; Global widget in the lhs.
    (setq *lb-slots*
          (tk-make-listbox left-up width: "30" bg: "SteelBlue"))

    (eul-associate *lb-slots* scroll-slots 'vertical)

    (setq *lb-previous*
          (tk-make-listbox left-down height: "10" width: "30"))
    (setq *info-hierchy* (tk-make-canvas decoration))

    ()
    (tk-bind *lb-previous* "<Double-ButtonRelease-1>" go-previous-object)
    (tk-bind *lb-slots* "<Double-ButtonRelease-1>" go-level-down)
    (tk-bind *lb-slots* "<ButtonRelease-1>" show-slot-information)

    (tk-pack left side: "left" fill: "both")
    (tk-pack left-up side: "top" expand: "1" fill: "both")
    (tk-pack left-down side: "bottom")
    (tk-pack label1 side: "top" fill: "x")
    (tk-pack *lb-slots* side: "left" fill: "y")
    (tk-pack scroll-slots side: "right" fill: "y")

    (tk-pack label2 side: "top" fill: "x")
    (tk-pack *lb-previous*)
    (tk-pack exit-button side: "bottom" fill: "x" pady: "4m")
    (tk-pack right side: "right" expand: "1" fill: "both")
    (tk-pack *info-title* side: "top" fill: "x")
    (tk-pack scroll-text side: "right" fill: "y")
    (tk-pack decoration side: "left" expand: "1" fill: "both")
    (tk-pack *info-text* side: "top" expand: "1" fill: "both")
    (tk-pack *info-hierchy* side: "bottom")
    (generate-layout))
  (Tk_MainLoop))

(defclass <Person> ()
  ((namePer accessor: namePer keyword: namePer:)
   (phone accessor: phone keyword: phone:)
   (postcode accessor: postcode keyword: postcode:)
   (university accessor: university keyword: university:)))

(defclass <University> ()
  ((nameUni accessor: nameUni keyword: nameUni:)
   (address accessor: address keyword: address:)
   (country accessor: country keyword: country:)))

(defun trying ()
  (let ((person1  (make <Person>))
        (university1 (make <University>)))

    ((setter nameUni) university1 "University of Bath")
    ((setter address) university1 "Claverton Down")
    ((setter country) university1 "United Kingdom")

    ((setter namePer) person1 "Juli")
    ((setter phone) person1 "8107")
    ((setter postcode) person1 "BA2 7JX")
    ((setter university) person1 university1)
    (inspect person1)))

(trying)

;;;-----------------------------------------------------------------------------
)  ;; End of module tk_browser
;;;-----------------------------------------------------------------------------
