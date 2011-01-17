main ()
(
 (if (and prompt? (not *readline*))
     (progn
       (%print #\\n)
       (%print (current-module))
       (%print "> "))
   ())
 (setq *last* (read *FILE-INPUT*))
 (if (eq *last* **eof**)
     (exit)
   ())
 (if prompt?
     (printnl (eval/cm *last*))
   (eval/cm *last*))
 (main))
