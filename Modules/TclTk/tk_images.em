;;;-----------------------------------------------------------------------------
;;;  By Julio Garcia Moreno & University of Bath.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo/tk'
;;;-----------------------------------------------------------------------------
;;;  Library: tcltk
;;;  Authors: Julio Garcia Moreno
;;; Description: YouToo/Tk module
;;    that provides the basic operations for the image commands.
;;;-----------------------------------------------------------------------------
(defmodule tk_images
  (syntax (macros)
          import (level1 tk_general)
          export (tk-create-image tk-delete-image tk-height-image tk-names-image
                  tk-type-image tk-types-image tk-width-image tk-photo))

;;;-----------------------------------------------------------------------------
;;; Image enquiry and manipulation functions
;;;-----------------------------------------------------------------------------
  (defextern eul_tk_image_cmd (<string> <string> ptr) ptr)

  (defun tk-create-image (type . options)
    (let ((args (as-c-options options)))
      (and (or (eq type 'bitmap)
               (eq type 'photo))
           (eul_tk_image_cmd "image" "create"
                             (cons (+ (car args) 1)
                                   (cons (symbol-name  type)
                                         (cdr args)))))))

  (defun tk-delete-image (image1 . images)
    (and (stringp image1)
         (eul_tk_image_cmd "image" "delete" (cons (+ (list-size images) 1)
                                                  (cons image1
                                                        images)))))

  (defun tk-height-image (image)
    (and (stringp image)
         (eul_tk_image_cmd "image" "height" (list 1 image))))

  (defun tk-names-image ()
    (eul_tk_image_cmd "image" "names" ()))

  (defun tk-type-image (image)
    (and (stringp image)
         (eul_tk_image_cmd "image" "type" (list 1 image))))

  (defun tk-types-image ()
    (eul_tk_image_cmd "image" "types" ()))

  (defun tk-width-image (image)
    (and (stringp image)
         (eul_tk_image_cmd "image" "width" (list 1 image))))

;;;-----------------------------------------------------------------------------
;;; Photo manipulation
;;;-----------------------------------------------------------------------------
  (defextern eul_tk_image_photo_cmd (<string>) ptr)

  (defun tk-photo (img cmd . options)
    (labels
     ((loop (l str)
            (if l
                (let* ((x (car l))
                       (y (if (keyword? x)
                              (concatenate "-" x)
                            (convert x <string>))))
                  (loop (cdr l) (concatenate str " " y)))
              str)))
     (eul_tk_image_photo_cmd (loop (cons img (cons cmd options)) ""))))

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
