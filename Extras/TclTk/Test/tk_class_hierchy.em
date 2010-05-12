(defmodule tk_class_hierchy
    (syntax (macros)
     import (level1 tcltk))

;;;-----------------------------------------------------------------------------
;;; Callback Functions
;;;-----------------------------------------------------------------------------
  (defconstant DEV-HEIGHT 700)
  (defconstant DEV-WIDTH 800)
  (defconstant INFO-HEIGHT 800)
  (deflocal INFO-WIDTH 0)
  (defconstant SPACE-BETWEEN-CLASSES 130)
  (defconstant SPACE-BETWEEN-ROWS 120)
  (deflocal *canvas1* ())
  (deflocal *listbox* ())
  (deflocal *class-name-label* ())
  (deflocal MARGIN-OVAL 20)
  (deflocal *no-value* (gensym "no value "))
  (deflocal *objects* (make <table> fill-value: *no-value* comparator: binary=))

  ;; *open-rows* is a list. The car will always contain the number of
  ;; the last open row.
  (deflocal *open-rows* (list 0))

;;;-----------------------------------------------------------------------------
;;; Test Function
  ; px and py will have the position of the father object that has been
  ; clicked in order to show/unshow their sons
  ; All the calculations are in order to reproduce the reproduce the next formaula:
  ;
  ;   This formaula calculates the x position of everyone of the children.
  ; px is the x position of the father. i iterates from 0 to (size - 1)
  ; SPACE-BETWEEN-CLASSES has the padding spaces between two children.
  ;
  ;    x = px + [2i - (size - 1)]*(SPACE-BETWEEN-CLASESS / 2)
  ;
  ;    y = py + 90
;;;-----------------------------------------------------------------------------
  (defun display-next-level (next-level-list objecte px py father-row)
    (let ((list-tk-elements ())
          (shift 0)
          (y (+ py SPACE-BETWEEN-ROWS))
          (half-space (/ SPACE-BETWEEN-CLASSES 2))
          (length (- (list-size next-level-list) 1))
          (half-oval-height (/ (+ 13 MARGIN-OVAL) 2))
          (i 0)
          text-size i half-oval-width)

      (do (lambda (el)
            (let* ((value1 (- (* 2 i) length))
                   (x (+ px (* value1 half-space) shift))
                   oval text line)
              (and (eq i 0)
                   (< x 0)
                   (setq shift (+ (- 0 x) SPACE-BETWEEN-CLASSES))
                   (setq x (+ x shift)))
              (setq line (tk-add-line-canvas *canvas1* px py x y))
              (tk-lower-item-canvas line)
              (setq text-size (size (symbol-name (class-name el))))

              (setq half-oval-width (/ (+ (* text-size 7) MARGIN-OVAL) 2))
              (setq oval (tk-add-oval-canvas *canvas1*
                                             (- x half-oval-width) (- y half-oval-height)
                                             (+ x half-oval-width) (+ y half-oval-height)
                                             fill: "SeaGreen4"))

              (setq text (tk-add-text-canvas *canvas1* x y
                                             text: (symbol-name (class-name el))
                                             font: "7x13bold"
                                             anchor: "center"))

              (tk-bind-item-canvas oval "<Button-1>" show-unshow-next-level
                                   args: (list el x y (+ 1 father-row)))
              (tk-bind-item-canvas text "<Button-1>" show-unshow-next-level
                                   args: (list el x y (+ 1 father-row)))

              (tk-bind-item-canvas oval "<Button-2>" show-class-info args: (list el))
              (tk-bind-item-canvas text "<Button-2>" show-class-info args: (list el))
              (setq list-tk-elements (concatenate (list oval text line) list-tk-elements))
              (setq i (+ i 1))))
          next-level-list)

      ((setter table-ref) *objects* father-row list-tk-elements)
      (setq *open-rows* (cons (+ 1 father-row) *open-rows*))
      ))

  ;; The next function will delete the sub-level when the clicked
  ;; class has the level open... If the sublevel has another sub-levels, these
  ;; ones are also deleted.
  (defun delete-sublevel (row)
    (let ((list-tk-elements (table-ref *objects* row)))
      (do (lambda (el)
            (tk-delete-item-canvas *canvas1* el))
          list-tk-elements)

      ((setter table-ref) *objects* row ())
      (if (> (- (car *open-rows*) 1) row)
          (delete-sublevel (+ row 1))
        ())
      (setq *open-rows* (cdr *open-rows*))))

  (defun show-unshow-next-level (objecte px py father-row)
    (let* ((next-level (class-direct-subclasses objecte)))

      (and next-level
           (if (eq (car *open-rows*) father-row)
               (display-next-level next-level objecte px py father-row)
             (progn
               (delete-sublevel father-row)
               (display-next-level next-level objecte px py father-row))))))

  (defun show-class-info (current-class)
    (tk-conf-widget *class-name-label* text: (symbol-name (class-name current-class)))
    (tk-delete *listbox* "0" "end")
    (do (lambda (s)
          (tk-insert *listbox* "end" (symbol-name (slot-name s))))
        (class-slots current-class)))

  ;; The next function shows the hierchy of the class system in youtoo.
  ;; The first sublevel determines the width of the canvas.
  (defun class-hierchy ()
    (tk-wm "title" () "CLASS HIERCHY")
    (let* ((frame1 (tk-make-frame ()))
           (frame2 (tk-make-frame frame1))
           (frame3 (tk-make-frame ()))
           (but (tk-make-button frame3 text: "Exit" command: tk-exit))
           (label (tk-make-label frame3 text: "Class Slots" fg: "white" bg: "Bisque3"))
           (label1 (tk-make-label frame2 text: "Class-hierchy" fg: "white" bg: "Bisque3"))
           (scroll (tk-make-scrollbar frame1 orient: "vertical"))
           (scroll1 (tk-make-scrollbar frame2 orient: "horizontal"))
           (scroll2 (tk-make-scrollbar frame3 orient: "vertical"))
           (posy 30)
           posx oval text)

      (setq INFO-WIDTH (* SPACE-BETWEEN-CLASSES
                          (list-size (class-direct-subclasses <object>))))
      (setq INFO-WIDTH (+ INFO-WIDTH SPACE-BETWEEN-CLASSES))
      (setq posx (/ INFO-WIDTH 2))
      (setq *listbox* (tk-make-listbox frame3 width: "30" bg: "SteelBlue"))
      (setq *canvas1* (tk-make-canvas frame2 width: DEV-WIDTH
                                      height: DEV-HEIGHT
                                      scrollregion: (format () "0 0 ~a ~a" INFO-WIDTH INFO-HEIGHT)))
      (setq *class-name-label* (tk-make-label frame3 fg: "black"))
      (eul-associate *canvas1* scroll 'vertical)
      (eul-associate *canvas1* scroll1 'horizontal)
      (eul-associate *listbox* scroll2 'vertical)
      (setq oval (tk-add-oval-canvas *canvas1*
                                     (- posx 31) (- posy 14)
                                     (+ posx 31) (+ posy 14)
                                     fill: "SeaGreen4"))
      (setq text (tk-add-text-canvas *canvas1* posx posy
                                     text: (symbol-name (class-name <object>))
                                     font: "7x13bold"
                                     anchor: "center"))


      (tk-bind-item-canvas oval "<Button-1>" show-unshow-next-level args: (list <object> posx posy 0))
      (tk-bind-item-canvas text "<Button-1>" show-unshow-next-level args: (list <object> posx posy 0))

      (tk-bind-item-canvas oval "<Button-2>" show-class-info args: (list <object>))
      (tk-bind-item-canvas text "<Button-2>" show-class-info args: (list <object>))
      ()
      (tk-pack frame1 frame3 side: "left" fill: "both")
      (tk-pack scroll1 side: "bottom" fill: "x")
      (tk-pack label1 *canvas1* side: "top" expand: "yes")
      (tk-pack scroll side: "right" fill: "y")
      (tk-pack frame2 side: "left")
      (tk-pack label *class-name-label* side: "top")
      (tk-pack but side: "bottom" fill: "x" padx: "3m" pady: "3m")
      (tk-pack *listbox* side: "left" fill: "y")
      (tk-pack scroll2 side: "right" fill: "y")
      )
    (Tk_MainLoop))

  (class-hierchy)

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
