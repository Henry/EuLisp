;;; Copyright 1997 A. Kind & University of Bath
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
;;; Title: parse arguments
;;;  Library: comp (EuLisp to Bytecode Compiler -- EuLysses)
;;;  Authors: Andreas Kind, Keith Playford
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule i-args
  (syntax (_macros
           _i-aux0)
   import (i-all)
   export (parse-args
           print-version
           print-params
           print-help))

;;;-----------------------------------------------------------------------------
;;; Parse the arguments of the compiler invocation
;;;-----------------------------------------------------------------------------
(defun parse-args (argv)
  (let ((n (vector-size argv)))
    (labels
     ((loop (i)
            (with-ct-handler (fmt "bad parameter ~a" argv) argv
             (if (= i n) t
               (let ((arg (vector-ref argv i)))
                 (cond
                   ((or (string-equal arg "-h") (string-equal arg "--help"))
                    (print-help)
                    (ct-exit))
                   ((or (string-equal arg "-V") (string-equal arg "--version"))
                    (print-version)
                    (ct-exit))
                   ((or (string-equal arg "-params") (string-equal arg "-p")
                        (string-equal arg "--params"))
                    (print-params)
                    (ct-exit))
                   ((or (string-equal arg "-load_path")
                        (string-equal arg "--load-path"))
                    (setq *load-path*
                          (cons (vector-ref argv (+ i 1)) *load-path*))
                    (loop (+ i 2)))
                   ((or (string-equal arg "-o") (string-equal arg "--object"))
                    (setq *dest-file-name*
                          (make-symbol (vector-ref argv (+ i 1))))
                    (loop (+ i 2)))
                   ((or (string-equal arg "-od") (string-equal arg "-O")
                        (string-equal arg "-object-dir"))
                    (setq *object-dir*
                          (make-symbol (vector-ref argv (+ i 1))))
                    (loop (+ i 2)))
                   ((or (string-equal arg "-s") (string-equal arg "--script"))
                    (setq *script-file* (vector-ref argv (+ i 1)))
                    (setq *script* t)
                    (setq *interpreter* t)
                    (setq *verbose* ())
                    (setq *silent* t)
                    (loop (+ i 2)))
                   ((or (string-equal arg "-m") (string-equal arg "--module"))
                    (setq *script-file* (vector-ref argv (+ i 1)))
                    (setq *eval-module* t)
                    (setq *verbose* ())
                    (setq *silent* t)
                    (loop (+ i 2)))
                   ((or (string-equal arg "-q") (string-equal arg "--quiet"))
                    (setq *silent* t)
                    (setq *verbose* ())
                    (setq *C-cc-flags* (string-append *C-cc-flags* " -w"))
                    (loop (+ i 1)))
                   ((or (string-equal arg "-v") (string-equal arg "--verbose"))
                    (setq *silent* ())
                    (setq *verbose* t)
                    (setq *C-cc-flags* (string-append *C-cc-flags* " -v"))
                    (loop (+ i 1)))
                   ((or (string-equal arg "-no_warnings")
                        (string-equal arg "--no-warnings"))
                    (setq *warnings* ())
                    (loop (+ i 1)))
                   ((or (string-equal arg "-no_inline")
                        (string-equal arg "--no-inline"))
                    (setq *inline-level* 0)
                    (loop (+ i 1)))
                   ((or (string-equal arg "-no_peephole")
                        (string-equal arg "--no-peephole"))
                    (setq *peephole* ())
                    (loop (+ i 1)))
                   ((or (string-equal arg "-no_errors")
                        (string-equal arg "--no-errors"))
                    (setq *errors* ())
                    (loop (+ i 1)))
                   ((or (string-equal arg "-i") (string-equal arg "-interpret"))
                    (setq *interpreter* t)
                    (setq *silent* t)
                    (setq *verbose* ())
                    (loop (+ i 1)))
                   ((or (string-equal arg "-cc") (string-equal arg "--cc"))
                    (setq *C-cc* (vector-ref argv (+ i 1)))
                    (loop (+ i 2)))
                   ((or (string-equal arg "-ld") (string-equal arg "--ld"))
                    (setq *C-ld* (vector-ref argv (+ i 1)))
                    (loop (+ i 2)))
                   ((or (string-equal arg "-cflags")
                        (string-equal arg "--cflags"))
                    (setq *C-cc-flags*
                          (string-append
                           (string-append *C-cc-flags* " ")
                           (vector-ref argv (+ i 1))))
                    (if (eq (string-ref (vector-ref argv (+ i 2)) 0) #\-)
                        ()
                      (setq *C-cc-flags*
                            (string-append
                             (string-append *C-cc-flags* " ")
                             (vector-ref argv (+ i 2)))))
                    (loop (+ i 2)))
                   ((or (string-equal arg "-static")
                        (string-equal arg "--static"))
                    (let ((flag
                           (cond
                             ((string-equal *C-cc* "gcc") " --static")
                             ((string-equal *ostype* "SUNOS5") " --dn")
                             ((string-equal *ostype* "IRIX") " --non-shared")
                             (t " --static"))))
                      (setq *C-cc-flags* (string-append *C-cc-flags* flag))
                      (loop (+ i 1))))
                   ((or (string-equal arg "-no_else")
                        (string-equal arg "--no-else"))
                    (setq *no-else* t)
                    (loop (+ i 1)))
                   ((or (string-equal arg "-redefine")
                        (string-equal arg "--redefine"))
                    (setq *redefine-imported-bindings* t)
                    (loop (+ i 1)))
                   ((or (string-equal arg "-g") (string-equal arg "--debug"))
                    (setq *C-cc-flags* (string-append *C-cc-flags* " -g"))
                    (setq *strip-stand-alone* ())
                    (setq *debug* t)
                    (loop (+ i 1)))
                   ((or (string-equal arg "-ar_cmd")
                        (string-equal arg "--ar-cmd"))
                    (setq *C-ar* (vector-ref argv (+ i 1)))
                    (loop (+ i 2)))
                   ((or (string-equal arg "-ranlib_cmd")
                        (string-equal arg "--ranlib-cmd"))
                    (setq *C-ranlib* (vector-ref argv (+ i 1)))
                    (loop (+ i 2)))
                   ((or (string-equal arg "-l") (string-equal arg "--library"))
                    (setq *linked-C-libraries*
                          (cons (make-symbol (vector-ref argv (+ i 1)))
                                *linked-C-libraries*))
                    (loop (+ i 2)))
                   ((or (string-equal arg "-L")
                        (string-equal arg "--library-path"))
                    (setq *C-library-load-path*
                          (cons (vector-ref argv (+ i 1))
                                *C-library-load-path*))
                    (loop (+ i 2)))
                   ((or (string-equal arg "-fff") (string-equal arg "--fff"))
                    (setq *linked-C-ff-files*
                          (cons (make-symbol (vector-ref argv (+ i 1)))
                                *linked-C-ff-files*))
                    (loop (+ i 2)))
                   ((or (string-equal arg "-ffl") (string-equal arg "--ffl"))
                    (setq *linked-C-ff-libraries*
                          (cons (make-symbol (vector-ref argv (+ i 1)))
                                *linked-C-ff-libraries*))
                    (loop (+ i 2)))
                   ((or (string-equal arg "-stop_after")
                        (string-equal arg "--stop-after"))
                    (stop-after-pass (make-symbol (vector-ref argv (+ i 1))))
                    (loop (+ i 2)))
                   ((or (string-equal arg "-c") (string-equal arg "--c-module"))
                    (setq *create-C-module* t)
                    (setq *stand-alone* ())
                    (loop (+ i 1)))
                   ((or (string-equal arg "-stand_alone")
                        (string-equal arg "--stand-alone"))
                    (setq *stand-alone* t)
                    (loop (+ i 1)))
                   ((or (string-equal arg "-ar") (string-equal arg "--archive"))
                    (setq *create-C-library* t)
                    (setq *stand-alone* ())
                    (loop (+ i 1)))
                   ((or (string-equal arg "-recompile")
                        (string-equal arg "--recompile"))
                    (setq *recompile* t)
                    (setq *no-recompile* ())
                    (loop (+ i 1)))
                   ((or (string-equal arg "-no_recompile")
                        (string-equal arg "--no-recompile"))
                    (setq *no-recompile* t)
                    (setq *recompile* ())
                    (loop (+ i 1)))
                   ((or (string-equal arg "-no_gc")
                        (string-equal arg "--no-gc"))
                    (setq *no-gc* t)
                    (loop (+ i 1)))
                   ((or (string-equal arg "-no_ct_handlers")
                        (string-equal arg "--no-ct-handlers"))
                    (setq *no-ct-handlers* t)
                    (loop (+ i 1)))
                   (t
                    (if (eq (string-ref arg 0) #\-)
                        (progn
                          (ct-serious-warning
                           ()
                           "~a: unrecognized option '~a'"
                           (vector-ref argv 0)
                           arg)
                          (print-help)
                          (loop (+ i 1)))
                      (let* ((n (string-size arg))
                             (name
                              (if (and (eql (string-ref arg (- n 3)) #\.)
                                       (eql (string-ref arg (- n 2)) #\e)
                                       (eql (string-ref arg (- n 1)) #\m))
                                  (make <symbol>
                                        name: (substring arg 0 (- n 3)))
                                (make <symbol>
                                      name: arg))))
                        (setq *source-file-names*
                              (cons name *source-file-names*))
                        (loop (+ i 1)))))))))))
     (notify0 "Parse arguments ...")
     (if (= n 1)
         (progn
           (setq *interpreter* t)
           (setq *silent* t)
           (setq *verbose* ()))
       (loop 1)))))

;;;-----------------------------------------------------------------------------
;;; Print main parameters
;;;-----------------------------------------------------------------------------
(defun print-params ()
  (print nl)
  (print "PARAMETER SETTINGS:" nl)
  (print-param "*silent*" *silent*)
  (print-param "*verbose*" *verbose*)
  (print-param "*warnings*" *warnings*)
  (print-param "*errors*" *errors*)
  (print-param "*load-path*" *load-path*)
  (print-param "*C-library-load-path*" *C-library-load-path*)
  (print-param "*eulysses-dir*" *eulysses-dir*)
  (print-param "*create-C-module*" *create-C-module*)
  (print-param "*create-C-library*" *create-C-library*)
  (print-param "*stand-alone*" *stand-alone*)
  (print-param "*C-cc*" *C-cc*)
  (print-param "*C-ld*" *C-ld*)
  (print-param "*C-cc-flags*" *C-cc-flags*)
  (print-param "*C-ar*" *C-ar*)
  (print-param "*C-ranlib*" *C-ranlib*)
  (print-param "*no-ct-handlers*" *no-ct-handlers*)
  (print-param "*inline-level*" *inline-level*)
  (print-param "*recompile*" *recompile*)
  (flush))

(defun print-param (name value)
  (format "  ~a = ~a\n" name value))

;;;-----------------------------------------------------------------------------
;;; Print usage
;;;-----------------------------------------------------------------------------
(defun print-help ()
  (print "Usage: youtoo [OPTION]... [FILE]..." nl)
  (print "  -h --help               Print this usage information." nl)
  (print "  -V --version            Print current release." nl)
  (print "  -p --params             Print current parameter setting." nl)
  (print "     --load-path dir      Add dir to load path." nl)
  (print "  -c --c-module           Create C linkable module file only." nl)
  (print "     --stand-alone        Create a stand-alone application." nl)
  (print "     --archive            Create C linkable library file." nl)
  (print "  -l --library lib        Link with library lib." nl)
  (print "  -L --library-path dir   Add dir to library load path." nl)
  (print "     --fff file           Provide C foreign function file." nl)
  (print "     --ffl lib            Provide C foreign function library lib." nl)
  (print "  -o --object file        Generated object file." nl)
  (print "  -O --object-dir dir     Set destination directory for object files to dir." nl)
  (print "  -s --script file        Read and execute script from file." nl)
  (print "  -m --module module-name Read and execute module from file module-name.em." nl)
  (print "  -q --quiet              Print no messages, prompts or values.." nl)
  (print "  -v --verbose            Set verbose mode." nl)
  (print "     --no-warnings        Set no warning messages." nl)
  (print "     --no-errors          Set no error messages." nl)
  (print "     --no-else            Set no warning for if without else." nl)
  (print "     --redefine           Redefine imported bindings." nl)
  (print "     --no-inline          Ignore inline declarations." nl)
  (print "     --stop-after phase   Stop after specified compilation phase (e.g. read)." nl)
  (print "     --recompile          Recompile imported modules." nl)
  (print "     --no-recompile       No automatic recompilation of imports." nl)
  (print "     --no-gc              Link without garbage collection library." nl)
  (print "     --cc cc              Set C compiler to cc." nl)
  (print "     --ld ld              Set C linker to ld." nl)
  (print "     --ar-cmd ar          Set C ar command to ar." nl)
  (print "     --ranlib-cmd ranlib  Set C ranlib command to ranlib." nl)
  (print "     --cflags             Set ddditional C flags." nl)
  (print "     --static             Link with static libraries." nl)
  (print "  -g --debug              Generate C debug information." nl)
  (print "  -i --interpret          Set interpretation mode." nl)
  (print "     --no-ct-handlers     Set no compile-time error handling." nl)
  (flush))

(defun print-version ()
  (print "EuLisp System 'youtoo'" nl)
  (format "Version ~a updated\n" *version*)
  (print "Copyright 1996, 1997 A. Kind & University of Bath" nl)
  (print "Copyright 2010 Henry G. Weller" nl)
  (print "This is free software; see the source and the file COPYING for copying conditions." nl)
  (print "There is NO warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE." nl)
  (print nl)
  (flush))

;;;-----------------------------------------------------------------------------
)  ;; End of module i-args
;;;-----------------------------------------------------------------------------
