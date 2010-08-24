(defmodule tk_proves
  (syntax (macros)
   import (level1 tcl-tk)
   )
;;--------------------------------------------------------------------------;;
;;                                                                          ;;
;;                             Callback Functions                           ;;
;;                                                                          ;;
;;--------------------------------------------------------------------------;;
(defun callback (but x)
  (tk-delete-image x)
  (tk-conf-widget but image: ""))

(defun proving ()
  (let* ((imatge (tk-create-image 'photo file: "/home/ma5jg/market.gif"))
         (button (tk-make-button () image: imatge command: callback args: (list self: imatge))))

    (tk-wm "title" () "proving")
    (tk-pack button)

    (format t "height: ~s\n" (tk-height-image imatge))
    (flush)
    (format t "width: ~s\n" (tk-width-image imatge))
    (flush)
    (format t "type: ~s\n" (tk-type-image imatge))
    (flush)
    (format t "names: ~s\n" (tk-names-image))
    (flush)
    (format t "types: ~s\n" (tk-types-image))
    (flush)
    (Tk_MainLoop)))
(proving)
)
