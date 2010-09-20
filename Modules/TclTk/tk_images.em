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
;;; Title: Basic operations for the image commands
;;;  Library: tcltk
;;;  Authors: J Garcia
;;;-----------------------------------------------------------------------------

(defmodule tk_images
  (syntax (macros)
   import (level1 tk_general)
   export (tk-create-image
           tk-delete-image tk-height-image tk-names-image
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
  (and (string? image1)
       (eul_tk_image_cmd "image" "delete" (cons (+ (list-size images) 1)
                                                (cons image1
                                                      images)))))

(defun tk-height-image (image)
  (and (string? image)
       (eul_tk_image_cmd "image" "height" (list 1 image))))

(defun tk-names-image ()
  (eul_tk_image_cmd "image" "names" ()))

(defun tk-type-image (image)
  (and (string? image)
       (eul_tk_image_cmd "image" "type" (list 1 image))))

(defun tk-types-image ()
  (eul_tk_image_cmd "image" "types" ()))

(defun tk-width-image (image)
  (and (string? image)
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
)  ;; End of module
;;;-----------------------------------------------------------------------------
