;;;-----------------------------------------------------------------------------
;;;  By Julio Garcia Moreno & University of Bath.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo/tk'
;;;-----------------------------------------------------------------------------
;;;  Library:
;;;  Authors: Julio Garcia Moreno
;;; Description: YouToo/Tk module
;;    that provides additional basic operations for
;;    text and canvas widget classes.
;;;-----------------------------------------------------------------------------
(defmodule tk_class2
    (syntax (macros)
     import (level1 tk_general)
     export (tk-add-arc-canvas tk-add-bitmap-canvas tk-add-image-canvas
             tk-add-line-canvas tk-add-oval-canvas tk-add-polygon-canvas tk-add-rectangle-canvas
             tk-add-text-canvas tk-add-window-canvas tk-bind-item-canvas tk-conf-item-canvas
             tk-find-canvas-above tk-find-canvas-all tk-find-canvas-below tk-find-canvas-enclosed
             tk-find-canvas-overlapping tk-find-canvas-withtag tk-lower-item-canvas
             tk-move-item-canvas tk-coords-item-canvas tk-delete-item-canvas
             tk-text-index tk-text-mark tk-text-tag-add tk-bind-tag-text tk-conf-tag-text))

;;;-----------------------------------------------------------------------------
;;; The next part of code provides commands to CANVAS widget.
;;;-----------------------------------------------------------------------------
  (defextern eul_create_item_canvas (<string> <string> ptr ptr) ptr)

  (defextern eul_tk_bind_element (<string> ptr <string> <string> <string> <string> ptr) ptr)

  ;(defextern eul_tk_find_canvas (<string> ptr <string> ptr) ptr)

  (defextern eul_tk_cmd_item_canvas (<string> ptr <string> ptr <string>) ptr)

  ;; All the following functions allow the creation of elements within the
  ;; canvas widgets. The coordinates always have to be numbers. String types
  ;; are not allowed for the coordinates.
  (defun tk-add-arc-canvas (canvas x1 y1 x2 y2 . options)
    (and (tk-canvas? canvas)
         (tk-add-item-canvas 'arc  canvas (list x1 y1 x2 y2) options)))

  (defun tk-add-bitmap-canvas (canvas x1 y1 . options)
    (and (tk-canvas? canvas)
         (tk-add-item-canvas 'bitmap  canvas (list x1 y1) options)))

  (defun tk-add-image-canvas (canvas x1 y1 . options)
    (and (tk-canvas? canvas)
         (tk-add-item-canvas 'image  canvas (list x1 y1) options)))

  (defun tk-add-line-canvas (canvas x1 y1 x2 y2 . coords-and-options)
    (and (tk-canvas? canvas)
         (tk-add-item-canvas 'line  canvas (list x1 y1 x2 y2) coords-and-options)))

  (defun tk-add-oval-canvas (canvas x1 y1 x2 y2 . coords-and-options)
    (and (tk-canvas? canvas)
         (tk-add-item-canvas 'oval  canvas (list x1 y1 x2 y2) coords-and-options)))

  (defun tk-add-polygon-canvas (canvas x1 y1 x2 y2 x3 y3 . coords-and-options)
    (and (tk-canvas? canvas)
         (tk-add-item-canvas 'polygon  canvas (list x1 y1 x2 y2 x3 y3) coords-and-options)))

  (defun tk-add-rectangle-canvas (canvas x1 y1 x2 y2 . options)
    (and (tk-canvas? canvas)
         (tk-add-item-canvas 'rectangle canvas (list x1 y1 x2 y2) options)))

  (defun tk-add-text-canvas (canvas x y . options)
    (and (tk-canvas? canvas)
         (tk-add-item-canvas 'text canvas (list x y) options)))

  (defun tk-add-window-canvas (canvas x y . options)
    (and (tk-canvas? canvas)
         (tk-add-item-canvas 'window' canvas (list x y) options)))

  ;; The next function receives the type of the new item, the canvas, the coordinates of
  ;; the points and the options. Options can have more coordinates.
  ;; Basically what it does is to prepare the parameters to call the
  ;; foreign function.
  (defun tk-add-item-canvas (type canvas coords coords-and-options)
    (let* ((x (make <tk-item-canvas>))
           (options (as-c-options (concatenate coords coords-and-options) x)))
      ((setter tk-name) x (tk-name canvas))
      ((setter tk-handler) x (tk-handler canvas))
      ((setter tk-item-canvas-id) x
       (eul_create_item_canvas (symbol-name type) (tk-name x) (tk-handler x) options))
      x))

  ;; -----------------------------------------------------------------
  (defgeneric tk-conf-item-canvas ((item-or-canvas <tk-object>) . args))

  (defmethod tk-conf-item-canvas ((canvas <tk-canvas>) . args)
    (and (car args)
         (stringp (car args))
         (eul_tk_cmd_item_canvas (tk-name canvas) (tk-handler canvas)
                                 (car args) (as-c-options (cdr args)) "itemconfigure")))

  (defmethod tk-conf-item-canvas ((item <tk-item-canvas>) . args)
    (eul_tk_cmd_item_canvas (tk-name item) (tk-handler item)
                            (tk-item-canvas-id item) (as-c-options args item) "itemconfigure"))

  ;; -----------------------------------------------------------------
  (defgeneric tk-bind-item-canvas ((item-or-canvas <tk-object>) . args))

  (defmethod tk-bind-item-canvas ((canvas <tk-canvas>) . args)
    (and (>= (list-size args) 3)
         (let* ((tag (car args))
                (event (cadr args))
                (function (caddr args))
                (args2 (as-c-accessors (cdr (cddr args))))
                function-key)
           (and (stringp tag)
                (stringp event)
                (function? function)
                (setq function-key (symbol-name (gensym (symbol-name (function-name function)))))
                (tk_allocate_registers function-key (cons function (cadr args2)))
                (eul_tk_bind_element (tk-name canvas) (tk-handler canvas) "canvas"
                                     tag event function-key (car args2))
                ))))

  (defmethod tk-bind-item-canvas ((item <tk-item-canvas>) . args)
    (and (>= (list-size args) 2)
         (let ((event (car args))
               (function (cadr args))
               (args2 (as-c-accessors (cddr args)))
               function-key)
           (and (stringp event)
                (function? function)
                (setq function-key (symbol-name (gensym (symbol-name (function-name function)))))
                (tk_allocate_registers function-key (cons function (cadr args2)))
                (eul_tk_bind_element (tk-name item) (tk-handler item) "canvas"
                                     (tk-item-canvas-id item) event function-key (car args2))))))

  ;; -----------------------------------------------------------------
  (defgeneric tk-lower-item-canvas ((item-or-canvas <tk-object>) . args))

  (defmethod tk-lower-item-canvas ((canvas <tk-canvas>) . args)
    (let ((size (list-size args))
          tag below)
      (cond
        ((binary= size 1)
         (setq tag (car args))
         (and (stringp tag)
              (eul_tk_cmd_item_canvas (tk-name canvas) (tk-handler canvas) tag () "lower"))
         ((binary= size 2)
          (setq tag (car args))
          (setq below (cadr args))
          (and (stringp tag)
               (or (stringp below) (tk-item-canvas? below))
               (setq below (if (stringp below) below
                             (tk-item-canvas-id below)))
               (eul_tk_cmd_item_canvas (tk-name canvas) (tk-handler canvas) tag
                                       (cons 1 below) "lower")))))))

  (defmethod tk-lower-item-canvas ((item <tk-item-canvas>) . args)
    (let ((size (list-size args))
          tag_id)
      (cond
        ((binary= size 0)
         (eul_tk_cmd_item_canvas (tk-name item) (tk-handler item)
                                 (tk-item-canvas-id item) () "lower"))
        ((binary= size 1)
         (setq tag_id (car args))
         (and (or (stringp tag_id) (tk-item-canvas? tag_id))
              (setq tag_id (if (stringp tag_id) tag_id
                             (tk-item-canvas-id item)))
              (eul_tk_cmd_item_canvas (tk-name item) (tk-handler item) (tk-item-canvas-id item)
                                      (cons 1 tag_id) "lower"))))))

  ;; -----------------------------------------------------------------
  ()
  (defgeneric tk-move-item-canvas ((item-or-canvas <tk-object>) . args))

  (defmethod tk-move-item-canvas ((canvas <tk-canvas>) . args)
    (let ((size (list-size args))
          tag x-incr y-incr)
      (if (binary= size 3)
          (progn
            (setq tag (car args))
            (setq x-incr (cadr args))
            (setq y-incr (caddr args))
            (and (stringp tag)
                 (number? x-incr)
                 (number? y-incr)
                 (eul_tk_cmd_item_canvas (tk-name canvas) (tk-handler canvas)
                                         tag (list 2 (convert x-incr <string>) (convert y-incr <string>))
                                         "move")))
        ())))

  (defmethod tk-move-item-canvas ((item <tk-item-canvas>) . args)
    (let ((size (list-size args))
          x-incr y-incr)
      (if (binary= size 2)
          (progn
            (setq x-incr (car args))
            (setq y-incr (cadr args))
            (and (number? x-incr)
                 (number? y-incr)
                 (eul_tk_cmd_item_canvas (tk-name item) (tk-handler item)
                                         (tk-item-canvas-id item)
                                         (list 2 (convert x-incr <string>)
                                               (convert y-incr <string>))
                                         "move")))
        ())))

  ;; -----------------------------------------------------------------
  (defgeneric tk-coords-item-canvas ((item-or-canvas <tk-object>) . args))

  (defmethod tk-coords-item-canvas ((canvas <tk-canvas>) . args)
    (let ((size (list-size args))
          tag
          (coords ()))
      (and args
           (do (lambda (el) (setq coords (cons (convert el <string>) coords)))
               args))
      (cond
        ((binary= size 1)
         (setq tag (car args))
         (and (stringp tag)
              (eul_tk_cmd_item_canvas (tk-name canvas) (tk-handler canvas)
                                      tag () "coords")))
        ((>= size 3)
         (setq tag (car args))
         (and (stringp tag)
              (eul_tk_cmd_item_canvas (tk-name canvas) (tk-handler canvas)
                                      tag (cons (- size 1) (cdr coords)) "coords"))))))

  (defmethod tk-coords-item-canvas ((item <tk-item-canvas>) . args)
    (let ((size (list-size args))
          (coords ()))
      (and args
           (do (lambda (el) (setq coords (cons (convert el <string>) coords)))
               (reverse args)))

      (cond
        ((binary= size 0)
         (eul_tk_cmd_item_canvas (tk-name item) (tk-handler item)
                                 (tk-item-canvas-id item) () "coords"))
        ((>= size 2)
         (eul_tk_cmd_item_canvas (tk-name item) (tk-handler item)
                                 (tk-item-canvas-id item) (cons size coords) "coords")))))
  ()

  ;; -----------------------------------------------------------------
  (defgeneric tk-delete-item-canvas ((canvas <tk-canvas>) . more-item-or-tags))

  (defmethod tk-delete-item-canvas ((canvas <tk-canvas>) . more-item-or-tags)
    (let ((size (list-size more-item-or-tags))
          (list-item-tags ()))
      (cond
        ((>= size 1)
         (do (lambda (el)
               (cond
                 ((stringp el)
                  (setq list-item-tags (cons el list-item-tags)))
                 ((tk-item-canvas? el)
                  (setq list-item-tags (cons (tk-item-canvas-id el)
                                             list-item-tags)))))
             more-item-or-tags)
         (eul_tk_cmd_item_canvas (tk-name canvas) (tk-handler canvas)
                                 (car list-item-tags)
                                 (cdr list-item-tags)
                                 "delete")
         ))))

  ;; ----------------------------------------------------------------
  ;;                    Functions for find items in canvases
  ;; ----------------------------------------------------------------
  (defgeneric tk-find-canvas-above ((item-or-canvas <tk-object>) . args))

  (defmethod tk-find-canvas-above ((canvas <tk-canvas>) . args)
    (and (stringp (car args))
         (null? (cdr args))
         (eul_tk_cmd_item_canvas (tk-name canvas) (tk-handler canvas) "above"
                                 (cons 1 (car args)) "find")))

  (defmethod tk-find-canvas-above ((item <tk-item-canvas>) . args)
    (and (null? args)
         (eul_tk_cmd_item_canvas (tk-name item) (tk-handler item) "above"
                                 (cons 1 (tk-item-canvas-id item)) "find")))

  ;; -----------------------------------------------------------------
  (defun tk-find-canvas-all (canvas)
    (and (tk-canvas? canvas)
         (eul_tk_cmd_item_canvas (tk-name canvas) (tk-handler canvas) "all" () "find")))

  ;; -----------------------------------------------------------------
  (defgeneric tk-find-canvas-below ((item-or-canvas <tk-object>) . args))
  ()

  (defmethod tk-find-canvas-below ((canvas <tk-canvas>) . args)
    (and (stringp (car args))
         (null? (cdr args))
         (eul_tk_cmd_item_canvas (tk-name canvas) (tk-handler canvas) "below"
                                 (cons 1 (car args)) "find")))
  ()

  (defmethod tk-find-canvas-below ((item <tk-item-canvas>) . args)
    (and (null? args)
         (eul_tk_cmd_item_canvas (tk-name item) (tk-handler item) "below"
                                 (cons 1 (tk-item-canvas-id item)) "find")))

  ;; -----------------------------------------------------------------
  (defun tk-find-canvas-enclosed (canvas x1 y1 x2 y2)
    (and (tk-canvas? canvas)
         (number? x1)
         (number? y1)
         (number? x2)
         (number? y2)
         (let ((args ()))
           (do (lambda (el) (setq args (cons (convert el <string>) args)))
               (list y2 x2 y1 x1))
           (eul_tk_cmd_item_canvas (tk-name canvas) (tk-handler canvas) "enclosed"
                                   (cons 4 args) "find"))))

  ;; -----------------------------------------------------------------
  (defun tk-find-canvas-overlapping (canvas x1 y1 x2 y2)
    (and (tk-canvas? canvas)
         (number? x1)
         (number? y1)
         (number? x2)
         (number? y2)
         (let ((args ()))
           (do (lambda (el) (setq args (cons (convert el <string>) args)))
               (list y2 x2 y1 x1))
           (eul_tk_cmd_item_canvas (tk-name canvas) (tk-handler canvas) "overlapping"
                                   (cons 4 args) "find"))))

  ;; -----------------------------------------------------------------
  (defgeneric tk-find-canvas-withtag ((item-or-canvas <tk-object>) . args))

  (defmethod tk-find-canvas-withtag ((canvas <tk-canvas>) . args)
    (and (stringp (car args))
         (null? (cdr args))
         (eul_tk_cmd_item_canvas (tk-name canvas) (tk-handler canvas) "withtag"
                                 (cons 1 args) "find")))

  (defmethod tk-find-canvas-withtag ((item <tk-item-canvas>) . args)
    (and (null? args)
         (eul_tk_cmd_item_canvas (tk-name item) (tk-handler item) "withtag"
                                 (cons 1 (tk-item-canvas-id item)) "find")))

;;;-----------------------------------------------------------------------------
;;; The next part of code provides commands to TEXT widget.
;;;-----------------------------------------------------------------------------
  ()
  (defextern eul_tk_cmd_text (<string> ptr <string> ptr) ptr)

  (defgeneric tk-text-index ((text <tk-text>) index))

  (defmethod tk-text-index ((text <tk-text>) index)
    (eul_tk_cmd_text (tk-name text) (tk-handler text) "index" (list '1 index)))

  (defgeneric tk-text-mark ((text <tk-text>) (command <symbol>) (mark <string>) . index))

  (defmethod tk-text-mark ((text <tk-text>) (command <symbol>) (mark <string>) . index)
    (cond
      ((and (binary= command 'set)
            (binary= (list-size index) 1))
       (eul_tk_cmd_text (tk-name text) (tk-handler text) "mark"
                        (list '3 "set" mark (car index))))
      ((and (binary= command 'unset)
            (null? index))
       (eul_tk_cmd_text (tk-name text) (tk-handler text) "mark" (list  '2 "unset" mark)))
      (t
        (format t "Incorrect arguments in tk-text-mark function\n"))))
  ()

  (defun tk-text-tag-add (text tagName index1 . index2)
    (and (tk-text? text)
         (stringp tagName)
         (stringp index1)
         (binary= (% (list-size index2) 2) 1)
         (eul_tk_cmd_text (tk-name text) (tk-handler text) "tag"
                          (list (+ 3 (list-size index2)) "add" tagName index1 (car index2)))))

  (defun tk-bind-tag-text (text tagName event function . args)
    (let ((args2 (as-c-accessors args))
          function-key)
      (and (tk-text? text)
           (stringp tagName)
           (stringp event)
           (function? function)
           (setq function-key (symbol-name (gensym (symbol-name (function-name function)))))
           (tk_allocate_registers function-key (cons function (cadr args2)))
           (eul_tk_bind_element (tk-name text) (tk-handler text) "tag" tagName
                                event function-key (car args2)))))

  (defun tk-conf-tag-text (text tagName . args)
    (let ((options (as-c-options args)))
      (and (tk-text? text)
           (stringp tagName)
           (>= (list-size args) 2)
           (eul_tk_cmd_text (tk-name text) (tk-handler text) "tag"
                            (cons (+ (car options) 2)
                                  (concatenate  (list "configure" tagName)
                                                (cdr options)))))))

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
