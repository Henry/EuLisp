(defmodule tcltk
  (syntax (macros)
   import (level1 tk_general)
   expose (tk_general tk_class tk_class2 tk_commands tk_utils tk_images))
  (if (null (eul_initialize_tk))
      (progn
        (format t "Error during Tk Initialisation\n")
        (flush)
        (exit))
    ()))
