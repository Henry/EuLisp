;;; EuLisp system 'youtoo'
;;;   Interface file for module i-args

(definterface i-args
  (import (i-all)
   syntax (_macros _i-aux0)
   full-import (i-error i-notify i-param i-level1 boot1 boot symbol stream3 random handler read table table1 vector convert1 format list socket stream2 lock stream1 stream float character compare collect fpi number integer copy convert string callback let-cc dynamic thread event condition bit mop-alloc mop-access mop-prim mop-key mop-class mop-init mop-inspect mop-gf mop-meth mop-defcl telos level1 aux-table i-all)
   export (
    ((name . print-help) (pos . 2) (origin i-args . print-help))
    ((name . parse-args) (pos . 4) (origin i-args . parse-args))
    ((name . print-version) (pos . 5) (origin i-args . print-version))
    ((name . print-params) (pos . 3) (origin i-args . print-params))
   )
   local-literals (
    (print-help . 133)
    (print-params . 132)
    (parse-args . 131)
    (print-version . 130)
    (print-param . 129)
    ("  ~a = ~a
" . 127)
    ("There is NO warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE." . 125)
    ("This is free software; see the source and the file COPYING for copying conditions." . 124)
    ("Copyright (c) 1996, 1997 by A Kind & University of Bath" . 123)
    ("Version ~a updated
" . 122)
    ("EuLisp System 'youtoo'" . 121)
    ("Parse arguments ..." . 119)
    (name: . 117)
    ("bad parameter ~a" . 116)
    ("-no_ct_handlers" . 115)
    ("-no_gc" . 114)
    ("-no_recompile" . 113)
    ("-recompile" . 112)
    ("-ar" . 111)
    ("-stand_alone" . 110)
    ("-c" . 109)
    ("-stop_after" . 108)
    ("-ffl" . 107)
    ("-fff" . 106)
    ("-L" . 105)
    ("-l" . 104)
    ("-ranlib_cmd" . 103)
    ("-ar_cmd" . 102)
    (" -g" . 101)
    ("-g" . 100)
    ("-redefine" . 99)
    ("-no_else" . 98)
    (" -static" . 97)
    (" -non_shared" . 96)
    ("IRIX" . 95)
    (" -dn" . 94)
    ("SUNOS5" . 93)
    (" -static" . 92)
    ("gcc" . 91)
    ("-static" . 90)
    (" " . 89)
    (" " . 88)
    ("-cflags" . 87)
    ("-ld" . 86)
    ("-cc" . 85)
    ("-i" . 84)
    ("-no_errors" . 83)
    ("-no_peephole" . 82)
    ("-no_inline" . 81)
    ("-no_warnings" . 80)
    (" -v" . 79)
    ("-verbose" . 78)
    (" -w" . 77)
    ("-silent" . 76)
    ("--script" . 75)
    ("-od" . 74)
    ("-o" . 73)
    ("-load_path" . 72)
    ("-params" . 71)
    ("-version" . 70)
    ("-help" . 69)
    (|(method G003)| . 68)
    (anonymous . 67)
    (ct-error-value: . 65)
    ("bad parameter ~a" . 64)
    ("compile time error condition: " . 63)
    ("*recompile*" . 60)
    ("*inline-level*" . 59)
    ("*no-ct-handlers*" . 58)
    ("*C-ranlib*" . 57)
    ("*C-ar*" . 56)
    ("*C-cc-flags*" . 55)
    ("*C-ld*" . 54)
    ("*C-cc*" . 53)
    ("*stand-alone*" . 52)
    ("*create-C-library*" . 51)
    ("*create-C-module*" . 50)
    ("*eulysses-dir*" . 49)
    ("*C-library-load-path*" . 48)
    ("*load-path*" . 47)
    ("*errors*" . 46)
    ("*warnings*" . 45)
    ("*verbose*" . 44)
    ("*silent*" . 43)
    ("PARAMETER SETTINGS:" . 42)
    ("  -no_ct_handlers       --  no compile-time error handling" . 40)
    ("  -i                    --  force interpretation mode" . 39)
    ("  -g                    --  C debug info" . 38)
    ("  -static               --  no shared libraries used" . 37)
    ("  -cflags               --  additional C flag" . 36)
    ("  -ranlib_cmd           --  used C ranlib command" . 35)
    ("  -ar_cmd               --  used C ar command" . 34)
    ("  -ld                   --  used C linker" . 33)
    ("  -cc                   --  used C compiler" . 32)
    ("  -no_gc                --  garbage collection library not linked" . 31)
    ("  -no_recompile         --  no automatic recompilation of imports" . 30)
    ("  -recompile            --  recompile imported modules" . 29)
    ("  -stop_after <phase>   --  stop after compilation phase (e.g. read)" . 28)
    ("  -no_inline            --  ignore inline declarations" . 27)
    ("  -redefine             --  redefine imported bindings" . 26)
    ("  -no_else              --  omit warning for if without else" . 25)
    ("  -no_errors            --  no error messages" . 24)
    ("  -no_warnings          --  no warning messages" . 23)
    ("  -verbose              --  verbose mode" . 22)
    ("  -silent               --  silent mode" . 21)
    ("  --script <file>       --  script mode" . 20)
    ("  -od <dir>             --  destination directory for object files" . 19)
    ("  -o <file>             --  destination file" . 18)
    ("  -ffl <lib>            --  specify C foreign function library" . 17)
    ("  -fff <file>           --  specify C foreign function file" . 16)
    ("  -L <dir>              --  extent C linkable library load path" . 15)
    ("  -l <lib>              --  specify C linkable library" . 14)
    ("  -ar                   --  create C linkable library file" . 13)
    ("  -c                    --  create C linkable module file only" . 12)
    ("  -load_path <dir>      --  add <dir> to load path" . 11)
    ("  -params               --  show current parameter setting" . 10)
    ("  -version              --  show current release" . 9)
    ("  -help                 --  show usage" . 8)
    ("Usage: youtoo [<options>] <source-file(s)> [<options>]" . 7)
   )
   literals (
   )
))
