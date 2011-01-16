/// Copyright 1988 David Michael Betz
/// Copyright 1994 Russell Bradford
/// Copyright 2010, 2011 Henry G. Weller
///-----------------------------------------------------------------------------
//  This file is part of
/// ---                           EuLisp System 'EuXLisp'
///-----------------------------------------------------------------------------
//
//  EuXLisp is free software: you can redistribute it and/or modify it under the
//  terms of the GNU General Public License version 2 as published by the Free
//  Software Foundation.
//
//  EuXLisp is distributed in the hope that it will be useful, but WITHOUT ANY
//  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
//  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
//  details.
//
//  You should have received a copy of the GNU General Public License along with
//  this program.  If not, see <http://www.gnu.org/licenses/>.
//
///-----------------------------------------------------------------------------
/// Title: initialization functions
///  Maintainer: Henry G. Weller
///-----------------------------------------------------------------------------
#include "euxlisp.h"
#include "euxlBCodes.h"

///-----------------------------------------------------------------------------
/// Macros
///-----------------------------------------------------------------------------
///  Store a byte into a bytecode vector
#define pb(x) (*bcode++ = (x))

///-----------------------------------------------------------------------------
/// Global variables
///-----------------------------------------------------------------------------

///  Characters
euxlValue
euxlp_left_paren,
euxlp_right_paren,
euxlp_dot,
euxlp_quote;

///  Symbols
euxlValue
euxl_true,
euxl_eof_object,
euxl_default_object,
euxls_unassigned,
euxls_quote,
euxls_eval_cm,
euxls_unbound,
euxls_stdin,
euxls_stdout,
euxls_stderr,
euxls_filein,
euxls_fixfmt,
euxls_flofmt,
euxls_direct_slots,
euxls_direct_keywords,
euxls_name,
euxls_default,
euxls_requiredp,
euxls_keyword,
euxls_abstractp,
euxls_predicate,
euxls_constructor,
euxls_keywords,
euxls_superclasses,
euxls_reader,
euxls_writer,
euxls_accessor,
euxls_class,
euxls_defclass,
euxls_find_slot_index,
euxls_getivar,
euxls_setivar,
euxls_list,
euxls_lambda,
euxls_defun,
euxls_object,
euxls_value,
euxls_backtracep,
euxls_eq,
euxls_eqv,
euxls_equal,
euxls_equals,
euxls_import,
euxls_only,
euxls_except,
euxls_rename,
euxls_rename_flag,
euxls_syntax,
euxls_export,
euxls_expose,
euxls_callcc,
euxls_make,
euxls_apply,
euxls_setter,
euxls_signal,
euxls_unwind_protect,
euxls_general_error,
euxls_no_applic_error,
euxls_no_next_md_error,
euxls_bad_type_error,
euxls_telos_error,
euxls_teloeuxls_bad_ref,
euxls_incompatible_md,
euxls_unbound_error,
euxls_arith_error,
euxls_user_intr,
euxls_syntax_error,
euxls_compile_error,
euxls_letname,
euxls_progn,
euxls_compile,
euxls_set_module,
euxls_get_module,
euxls_reintern,
euxls_module_directives,
euxls_binary_plus,
euxls_binary_minus,
euxls_binary_times,
euxls_binary_divide,
euxls_quotient,
euxls_binary_less,
euxls_binary_equal,
euxls_current_thread,
euxlc_thread,
euxls_qualified_symbols,
euxls_set_generic_args,
euxls_syntax_error,
euxls_supplied_env,
euxls_debug,
euxls_xlframe,
euxls_gcmsgs,
euxls_arg_list,
euxls_next_methods;

///  Continuations
euxlValue
euxls_map_list_cont,
euxls_do_list_cont,
euxls_with_file_cont,
euxls_load_cont,
euxls_force_cont,
euxls_init_loop_cont;

///  Optional symbols
#ifndef NO_CHECK_REF
euxlValue euxls_check_ref;
#endif

#ifdef SOCK
euxlValue euxls_socket_error;
#endif

///-----------------------------------------------------------------------------
/// Functions
///-----------------------------------------------------------------------------
///  euxcInitWorkspace - create an initial workspace
void euxcInitWorkspace(unsigned int ssize)
{
    // Allocate memory for the workspace
    euxcMinit(ssize);

    euxls_gcmsgs = euxmNil;

    euxcObArray = euxcNewVector(euxmSymbolTableSize);

    euxcInitRootModule();

    euxls_unbound = euxmNil;    // to make euxcMakeSymbol work

    // Enter the eof object
    euxl_eof_object = euxcCons(euxmEnter("**eof**"), euxmNil);
    euxmSetValue(euxmCar(euxl_eof_object), euxl_eof_object);

    // Enter the default object
    euxl_default_object = euxcCons(euxmEnter("**default**"), euxmNil);

    // Install the built-in functions
    int i;
    euxcXFunDef *xp;
    for (i = 0, xp = xFunTab; xp->fun != NULL; ++i, ++xp)
    {
        euxcXFun(xp->name, euxmXFun, xp->fun, i);
    }

    for (i = 0, xp = xContFunTab; xp->fun != NULL; ++i, ++xp)
    {
        euxcXFun(xp->name, euxmXFunCont, xp->fun, i);
    }

    euxcFunDef *p;
    for (i = 0, p = funTab; p->fun != NULL; ++i, ++p)
    {
        euxcFun(p->name, euxmFun, p->fun, i);
    }

    // Setup some synonyms
    euxmSetValue(euxmEnter("not"), euxmGetValue(euxmEnter("euxmNull?")));

    // Enter all of the symbols used by the runtime system
    euxcSymbols();

    // Set the initial values of the symbols t and euxmNil, etc.
    euxmSetValue(euxl_true, euxl_true);
    euxmSetValue(euxmEnter("t"), euxl_true);
    euxmSetValue(euxmEnter("nil"), euxmNil);
    euxmSetValue(euxmEnter("backtrace?"), euxl_true);
    euxmSetValue(euxmEnter("qualified-symbols?"), euxmNil);
    euxmSetValue(euxmEnter("supplied-env"), euxmNil);
    euxmSetValue(euxmEnter("pathname_prefix"), euxmNil);

    euxcOInit();  // initialize xsobj.c

    // Setup the print formats for numbers
    euxmSetValue(euxls_fixfmt, euxcMakeString(euxmFPIFmt));
    euxmSetValue(euxls_flofmt, euxcMakeString(euxmDoubleFloatFmt));

    // Build the 'eval' function
    euxlValue code = euxcNewCode(4);
    euxmStackCheckPush(code);
    euxmSetElement(code, 0, euxcNewString(0x12));
    euxmSetElement(code, 1, euxmEnter("eval/cm"));
    euxmSetElement(code, 2, euxcCons(euxmEnter("X"), euxmNil));
    euxmSetElement(code, 3, euxmEnter("compile"));
    euxmStackDrop(1);

    // Store the byte codes
    unsigned char *bcode = (unsigned char *)euxmGetString(euxmGetBCode(code));

    pb(OP_FRAME);
    pb(0x02);           // 0000 12 02 FRAME 02
    pb(OP_MVARG);
    pb(0x01);           // 0002 13 01 MVARG 01
    pb(OP_ALAST);       // 0004 1a ALAST
    pb(OP_SAVE);
    pb(0x00);
    pb(0x10);           // 0005 0b 00 10 SAVE 0010
    pb(OP_EREF);
    pb(0x00);
    pb(0x01);           // 0008 09 00 01 EREF 00 01 ; x
    pb(OP_PUSH);        // 000b 10 PUSH
    pb(OP_GREF);
    pb(0x03);           // 000c 05 03 GREF 03 ; compile
    pb(OP_CALL);
    pb(0x01);           // 000e 0c 01 CALL 01
    pb(OP_CALL);
    pb(0x00);           // 0010 0c 00 CALL 00

    euxmSetValue(euxmGetElement(code, 1), euxcMakeClosure(code, euxmNil));

    // setup the initialization code
    code = euxcNewCode(6);
    euxmStackCheckPush(code);
    euxmSetElement(code, 0, euxcNewString(0x11));
    euxmSetElement(code, 1, euxmEnter("*INITIALIZE*"));
    euxmSetElement(code, 3, euxcMakeString("euxlisp.ini"));
    euxmSetElement(code, 4, euxmEnter("load"));
    euxmSetElement(code, 5, euxmEnter("*toplevel*"));
    euxmStackDrop(1);

    // store the byte codes
    bcode = (unsigned char *)euxmGetString(euxmGetBCode(code));

    pb(OP_FRAME);
    pb(0x01);           // 0000 12 01 FRAME 01
    pb(OP_ALAST);       // 0002 1a ALAST
    pb(OP_SAVE);
    pb(0x00);
    pb(0x0d);           // 0003 0b 00 0d SAVE 000d
    pb(OP_LIT);
    pb(0x03);           // 0006 04 03 LIT 03 ; "euxlisp.ini"
    pb(OP_PUSH);        // 0008 10 PUSH
    pb(OP_GREF);
    pb(0x04);           // 0009 05 04 GREF 04 ; load
    pb(OP_CALL);
    pb(0x01);           // 000b 0c 01 CALL 01
    pb(OP_GREF);
    pb(0x05);           // 000d 05 05 GREF 05 ; *toplevel*
    pb(OP_CALL);
    pb(0x00);           // 000f 0c 00 CALL 00

    euxmSetValue(euxmGetElement(code, 1), euxcMakeClosure(code, euxmNil));

    // (define (*toplevel*)
    // (if prompt?
    // (begin
    // (printnl)
    // (print (current-module))
    // (print "> ")))
    // (setq *last* (read *FILE-INPUT*))
    // (if (eq *last* **eof**) (exit))
    // (if prompt?
    // (write (eval *last*))
    // (eval *last*))
    // (*toplevel*))

    // setup the main loop code
    code = euxcNewCode(15);
    euxmStackCheckPush(code);
    euxmSetElement(code, 0, euxcNewString(0x70));
    euxmSetElement(code, 1, euxmEnter("*toplevel*"));
    #ifdef READLINE
    euxmSetElement(code, 2, euxmEnter("nil"));
    #else
    euxmSetElement(code, 2, euxmEnter("prompt?"));
    #endif
    euxmSetElement(code, 3, euxmEnter("prompt?"));
    euxmSetElement(code, 4, euxmEnter("printnl"));
    euxmSetElement(code, 5, euxmEnter("%print"));
    euxmSetElement(code, 6, euxcMakeString("> "));
    euxmSetElement(code, 7, euxmEnter("*FILE-INPUT*"));
    euxmSetElement(code, 8, euxmEnter("read"));
    euxmSetElement(code, 9, euxmEnter("*last*"));
    euxmSetElement(code, 10, euxmEnter("**eof**"));
    euxmSetElement(code, 11, euxmEnter("exit"));
    euxmSetElement(code, 12, euxmEnter("eval/cm"));
    euxmSetElement(code, 13, euxmEnter("printnl"));
    euxmSetElement(code, 14, euxmEnter("*toplevel*"));
    euxmStackDrop(1);

    // store the byte codes
    bcode = (unsigned char *)euxmGetString(euxmGetBCode(code));

    //  0000 12 01    FRAME 01
    //  0002 1a       ALAST
    //  0003 05 03    GREF 15 ; prompt?@root
    //  0005 02 00 22 BRF 0022
    //  0008 0b 00 0f SAVE 000f
    //  000b 05 04    GREF 04 ; printnl@root
    //  000d 0c 00    CALL 00
    //  000f 0b 00 18 SAVE 0018
    //  0012 50       current-module
    //  0013 10       PUSH
    //  0014 05 05    GREF 05 ; print@root
    //  0016 0c 01    CALL 01
    //  0018 0b 00 22 SAVE 0022
    //  001b 04 06    LIT 06 ; "> "
    //  001d 10       PUSH
    //  001e 05 05    GREF 05 ; print@root
    //  0020 0c 01    CALL 01
    //  0022 0b 00 2c SAVE 002c
    //  0025 05 07    GREF 07 ; *FILE-INPUT*@root
    //  0027 10       PUSH
    //  0028 05 08    GREF 08 ; read@root
    //  002a 0c 01    CALL 01
    //  002c 06 09    GSET 09 ; *last*@root
    //  002e 05 0a    GREF 0a ; **eof**@root
    //  0030 10       PUSH
    //  0031 05 09    GREF 09 ; *last*@root
    //  0033 1f       euxcEq
    //  0034 02 00 3e BRF 003e
    //  0037 0b 00 3e SAVE 003e
    //  003a 05 0b    GREF 0b ; exit@root
    //  003c 0c 00    CALL 00
    //  003e 05 03    GREF 03 ; prompt?@root
    //  0040 02 00 58 BRF 0058
    //  0043 0b 00 55 SAVE 0055
    //  0046 0b 00 50 SAVE 0050
    //  0049 05 09    GREF 09 ; *last*@root
    //  004b 10       PUSH
    //  004c 05 0c    GREF 0c ; eval@root
    //  004e 0c 01    CALL 01
    //  0050 10       PUSH
    //  0051 05 0d    GREF 0d ; write@root
    //  0053 0c 01    CALL 01
    //  0055 03 00 62 BR 0062
    //  0058 0b 00 62 SAVE 0062
    //  005b 05 09    GREF 09 ; *last*@root
    //  005d 10       PUSH
    //  005e 05 0c    GREF 0c ; eval@root
    //  0060 0c 01    CALL 01
    //  0062 05 0e    GREF 0e ; *toplevel*@root
    //  0064 0c 00    CALL 00

    pb(OP_FRAME);
    pb(0x01);
    pb(OP_ALAST);
    pb(OP_GREF);
    pb(0x02);
    pb(OP_BRF);
    pb(0x00);
    pb(0x22);
    pb(OP_SAVE);
    pb(0x00);
    pb(0x0f);
    pb(OP_GREF);
    pb(0x04);
    pb(OP_CALL);
    pb(0x00);
    pb(OP_SAVE);
    pb(0x00);
    pb(0x18);
    pb(OP_CURMOD);
    pb(OP_PUSH);
    pb(OP_GREF);
    pb(0x05);
    pb(OP_CALL);
    pb(0x01);
    pb(OP_SAVE);
    pb(0x00);
    pb(0x22);
    pb(OP_LIT);
    pb(0x06);
    pb(OP_PUSH);
    pb(OP_GREF);
    pb(0x05);
    pb(OP_CALL);
    pb(0x01);
    pb(OP_SAVE);
    pb(0x00);
    pb(0x2c);
    pb(OP_GREF);
    pb(0x07);
    pb(OP_PUSH);
    pb(OP_GREF);
    pb(0x08);
    pb(OP_CALL);
    pb(0x01);
    pb(OP_GSET);
    pb(0x09);
    pb(OP_GREF);
    pb(0x0a);
    pb(OP_PUSH);
    pb(OP_GREF);
    pb(0x09);
    pb(OP_EQ);
    pb(OP_BRF);
    pb(0x00);
    pb(0x3e);
    pb(OP_SAVE);
    pb(0x00);
    pb(0x3e);
    pb(OP_GREF);
    pb(0x0b);
    pb(OP_CALL);
    pb(0x00);
    pb(OP_GREF);
    pb(0x03);
    pb(OP_BRF);
    pb(0x00);
    pb(0x58);
    pb(OP_SAVE);
    pb(0x00);
    pb(0x55);
    pb(OP_SAVE);
    pb(0x00);
    pb(0x50);
    pb(OP_GREF);
    pb(0x09);
    pb(OP_PUSH);
    pb(OP_GREF);
    pb(0x0c);
    pb(OP_CALL);
    pb(0x01);
    pb(OP_PUSH);
    pb(OP_GREF);
    pb(0x0d);
    pb(OP_CALL);
    pb(0x01);
    pb(OP_BR);
    pb(0x00);
    pb(0x62);
    pb(OP_SAVE);
    pb(0x00);
    pb(0x62);
    pb(OP_GREF);
    pb(0x09);
    pb(OP_PUSH);
    pb(OP_GREF);
    pb(0x0c);
    pb(OP_CALL);
    pb(0x01);
    pb(OP_GREF);
    pb(0x0e);
    pb(OP_CALL);
    pb(0x00);

    euxmSetValue(euxmGetElement(code, 1), euxcMakeClosure(code, euxmNil));
    euxcInitRootExports();
}

///  euxcSymbols - lookup/enter all symbols used by the run-time system
void euxcSymbols()
{
    // euxmStackTop-level procedure symbol
    euxls_eval_cm = euxmEnter("eval/cm");

    // enter the symbols used by the system
    euxl_true = euxmEnter("t");

    euxls_unbound = euxmEnter("*unbound*");
    euxls_unassigned = euxmEnter("#!UNASSIGNED");

    // enter the i/o symbols
    euxls_stdin = euxmEnter("stdin");
    euxls_stdout = euxmEnter("stdout");
    euxls_stderr = euxmEnter("stderr");
    euxls_filein = euxmEnter("*FILE-INPUT*");

    // enter the symbols used by the printer
    euxls_fixfmt = euxmEnter("*fpi-format*");
    euxls_flofmt = euxmEnter("*float-format*");

    // enter symbols needed by the reader
    euxlp_left_paren = euxmEnter("(");
    euxlp_right_paren = euxmEnter(")");
    euxlp_dot = euxmEnter(".");
    euxlp_quote = euxmEnter("'");
    euxls_quote = euxmEnter("quote");

    // 'else' is a useful synonym for `t' in cond clauses
    euxlValue sym = euxmEnter("else");
    euxmSetValue(sym, euxl_true);

    // do we want a prompt?
    sym = euxmEnter("prompt?");

    if (quiet)
    {
        euxmSetValue(sym, euxmNil);
    }
    else
    {
        euxmSetValue(sym, euxl_true);
    }

    // GC messages
    sym = euxmEnter("*gc-msgs*");
    euxmSetValue(sym, euxmNil);

    // restore OS environment
    euxls_supplied_env = euxmEnter("supplied-env");
    euxlValue env = euxmGetValue(euxls_supplied_env);
    for (; euxmConsp(env); env = euxmCdr(env))
    {
        putenv(euxmGetString(euxmCdr(euxmCar(env))));
    }

    // Set the tickeuxls_per_second constant
    euxcSetTicksPerSecond();

    // setup stdin/stdout/stderr
    euxmSetValue(euxls_stdin, euxcMakeStream(stdin, euxmPortFlagInput));
    euxmSetValue(euxls_stdout, euxcMakeStream(stdout, euxmPortFlagOutput));
    euxmSetValue(euxls_stderr, euxcMakeStream(stderr, euxmPortFlagOutput));
    euxmSetValue(euxls_filein, euxcMakeStream(filein, euxmPortFlagInput));

    // get the built-in continuation funs
    euxls_map_list_cont = euxmGetValue(euxmEnter("%map-list-cont"));
    euxls_do_list_cont = euxmGetValue(euxmEnter("%do-list-cont"));
    euxls_with_file_cont = euxmGetValue(euxmEnter("%with-file-cont"));
    euxls_load_cont = euxmGetValue(euxmEnter("%load-cont"));
    euxls_force_cont = euxmGetValue(euxmEnter("%force-cont"));
    euxls_init_loop_cont = euxmGetValue(euxmEnter("%initloop-cont"));

    euxls_direct_slots = euxcEnterKeyword("direct-slots:");
    euxls_direct_keywords = euxcEnterKeyword("direct-keywords:");
    euxls_name = euxcEnterKeyword("name:");
    euxls_default = euxcEnterKeyword("default:");
    euxls_requiredp = euxcEnterKeyword("required?:");
    euxls_keyword = euxcEnterKeyword("keyword:");
    euxls_abstractp = euxcEnterKeyword("abstract?:");
    euxls_predicate = euxcEnterKeyword("predicate:");
    euxls_constructor = euxcEnterKeyword("constructor:");
    euxls_keywords = euxcEnterKeyword("keywords:");
    euxls_superclasses = euxcEnterKeyword("superclasses:");
    euxls_reader = euxcEnterKeyword("reader:");
    euxls_writer = euxcEnterKeyword("writer:");
    euxls_accessor = euxcEnterKeyword("accessor:");
    euxls_class = euxcEnterKeyword("class:");
    euxls_defclass = euxmEnter("defclass");

    euxls_find_slot_index = euxmEnter("find-slot-index");
    euxls_getivar = euxmEnter("%GETIVAR");
    euxls_setivar = euxmEnter("%SETIVAR");
    euxls_list = euxmEnter("list");
    euxls_lambda = euxmEnter("lambda");
    euxls_defun = euxmEnter("defun");
    euxls_object = euxmEnter("object");
    euxls_value = euxmEnter("value");

    euxls_backtracep = euxmEnter("backtrace?");

    euxls_eq = euxmEnter("eq");
    euxls_eqv = euxmEnter("eql");
    euxls_equal = euxmEnter("%equal");
    euxls_equals = euxmEnter("%=");

    euxls_import = euxmEnter("import");
    euxls_only = euxmEnter("only");
    euxls_except = euxmEnter("except");
    euxls_rename = euxmEnter("rename");
    euxls_rename_flag = euxmEnter("%rename");
    euxls_syntax = euxmEnter("syntax");
    euxls_export = euxmEnter("export");
    euxls_expose = euxmEnter("expose");

    euxls_callcc = euxmEnter("call/cc");

    euxls_make = euxmEnter("make");
    euxls_apply = euxmEnter("apply");
    euxls_setter = euxmEnter("setter");

    euxls_signal = euxmEnter("signal");
    euxls_unwind_protect = euxmEnter("unwind-protect");
    euxls_letname = euxmEnter("let-binding");
    euxls_general_error = euxmEnter("general-error");
    euxls_no_applic_error = euxmEnter("no-applicable-method-error");
    euxls_no_next_md_error = euxmEnter("no-next-method-error");
    euxls_bad_type_error = euxmEnter("bad-type-error");
    euxls_telos_error = euxmEnter("telos-error");
    euxls_teloeuxls_bad_ref = euxmEnter("telos-bad-ref");
    euxls_incompatible_md = euxmEnter("incompatible-method");
    euxls_unbound_error = euxmEnter("unbound-error");
    euxls_arith_error = euxmEnter("arith-error");
    euxls_user_intr = euxmEnter("user-interrupt");
    euxls_syntax_error = euxmEnter("syntax-error");
    euxls_compile_error = euxmEnter("compilation-error");

    euxls_progn = euxmEnter("progn");
    euxls_compile = euxmEnter("compile");
    euxls_set_module = euxmEnter("set-module");
    euxls_get_module = euxmEnter("current-module");
    euxls_reintern = euxmEnter("reintern-module-symbols");
    euxls_module_directives = euxmEnter("module-directives");

    euxls_binary_plus = euxmEnter("binary+");
    euxls_binary_minus = euxmEnter("binary-");
    euxls_binary_times = euxmEnter("binary*");
    euxls_binary_divide = euxmEnter("binary/");
    euxls_quotient = euxmEnter("quotient");
    euxls_binary_less = euxmEnter("binary<");
    euxls_binary_equal = euxmEnter("binary=");

    euxls_current_thread = euxmEnter("current-self");
    euxlc_thread = euxmEnter("<thread>");
    euxls_qualified_symbols = euxmEnter("qualified-symbols?");

    euxls_set_generic_args = euxmEnter("set-generic-args");

    euxls_syntax_error = euxmEnter("syntax-error");
    #ifdef SOCK
    euxls_socket_error = euxmEnter("socket-error");
    #endif

    euxls_debug = euxmEnter("debug");
    euxls_xlframe = euxmEnter("*xlframe*");

    euxls_gcmsgs = euxmEnter("*gc-msgs*");
    euxls_arg_list = euxmEnter("arg-list");
    euxls_next_methods = euxmEnter("next-methods");

    #ifndef NO_CHECK_REF
    euxls_check_ref = euxmEnter("check-ref");
    #endif
}

///-----------------------------------------------------------------------------
