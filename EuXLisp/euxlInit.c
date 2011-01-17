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
euxs_t,
euxs_eof,
euxs_default,
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

#ifdef READLINE
euxlValue euxls_readline;
#endif

///-----------------------------------------------------------------------------
/// Functions
///-----------------------------------------------------------------------------

static void compileString(const char *str)
{
    FILE *filein1 = euxcOSAOpen(str, "r");

    // euxcCurFun used as GC-protected temp var hare
    euxcCurFun = euxcMakeStream(filein1, euxmPortFlagInput);
    euxcCurVal = euxmNil;
    for (;;)
    {
        // name
        if (!euxcRead(euxcCurFun, &euxcCurEnv))
        {
            break;
        }
        euxmStackCheckPush(euxcCurEnv);

        // args
        if (!euxcRead(euxcCurFun, &euxcCurEnv))
        {
            euxmStackDrop(1);
            break;
        }
        euxmStackCheckPush(euxcCurEnv);

        // body
        if (!euxcRead(euxcCurFun, &euxcCurEnv))
        {
            euxmStackDrop(2);
            break;
        }
        euxmStackCheckPush(euxcCurEnv);

        // compile
        euxcCurVal = euxcCompileFunction
        (
            euxcStackPtr[2],
            euxcStackPtr[1],
            euxcStackPtr[0],
            euxmNil
        );
        euxcCurVal = euxcMakeClosure(euxcCurVal, euxmNil);
        euxmSetValue(euxcStackPtr[2], euxcCurVal);
        euxmStackDrop(3);
    }
    euxcCurFun = euxcCurEnv = euxcCurVal = euxmNil;
}


///  euxcInitSymbols - lookup/enter all symbols used by the run-time system
void euxcInitSymbols()
{
    // Enter the eof object
    euxs_eof = euxcCons(euxmInternAndExport("**eof**"), euxmNil);
    euxmSetValue(euxmCar(euxs_eof), euxs_eof);

    // Enter the default object
    euxs_default = euxcCons(euxmInternAndExport("**default**"), euxmNil);

    euxls_eval_cm = euxmInternAndExport("eval/cm");

    // True and false (nil)
    euxs_t = euxmInternAndExport("t");
    euxmSetValue(euxs_t, euxs_t);
    euxmSetValue(euxmInternAndExport("nil"), euxmNil);

    euxls_unbound = euxmInternAndExport("*unbound*");
    euxls_unassigned = euxmInternAndExport("#!UNASSIGNED");

    // enter the i/o symbols
    euxls_stdin = euxmInternAndExport("stdin");
    euxls_stdout = euxmInternAndExport("stdout");
    euxls_stderr = euxmInternAndExport("stderr");
    euxls_filein = euxmInternAndExport("*FILE-INPUT*");

    // Setup the print formats for numbers
    euxls_fixfmt = euxmInternAndExport("*fpi-format*");
    euxmSetValue(euxls_fixfmt, euxcMakeString(euxmFPIFmt));
    euxls_flofmt = euxmInternAndExport("*float-format*");
    euxmSetValue(euxls_flofmt, euxcMakeString(euxmDoubleFloatFmt));

    // enter symbols needed by the reader
    euxlp_left_paren = euxmInternAndExport("(");
    euxlp_right_paren = euxmInternAndExport(")");
    euxlp_dot = euxmInternAndExport(".");
    euxlp_quote = euxmInternAndExport("'");
    euxls_quote = euxmInternAndExport("quote");

    // 'else' is a useful synonym for `t' in cond clauses
    euxlValue sym = euxmInternAndExport("else");
    euxmSetValue(sym, euxs_t);

    // do we want a prompt?
    sym = euxmInternAndExport("prompt?");
    if (quiet)
    {
        euxmSetValue(sym, euxmNil);
    }
    else
    {
        euxmSetValue(sym, euxs_t);
    }

    // GC messages
    euxls_gcmsgs = euxmInternAndExport("*gc-msgs*");
    euxmSetValue(euxls_gcmsgs, euxmNil);

    // restore OS environment
    euxls_supplied_env = euxmInternAndExport("supplied-env");
    euxmSetValue(euxls_supplied_env, euxmNil);
    euxmSetValue(euxmInternAndExport("pathname_prefix"), euxmNil);

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
    euxls_map_list_cont = euxmGetValue(euxmInternAndExport("%map-list-cont"));
    euxls_do_list_cont = euxmGetValue(euxmInternAndExport("%do-list-cont"));
    euxls_with_file_cont = euxmGetValue(euxmInternAndExport("%with-file-cont"));
    euxls_load_cont = euxmGetValue(euxmInternAndExport("%load-cont"));
    euxls_force_cont = euxmGetValue(euxmInternAndExport("%force-cont"));
    euxls_init_loop_cont = euxmGetValue(euxmInternAndExport("%initloop-cont"));

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
    euxls_defclass = euxmInternAndExport("defclass");

    euxls_find_slot_index = euxmInternAndExport("find-slot-index");
    euxls_getivar = euxmInternAndExport("%GETIVAR");
    euxls_setivar = euxmInternAndExport("%SETIVAR");
    euxls_list = euxmInternAndExport("list");
    euxls_lambda = euxmInternAndExport("lambda");
    euxls_defun = euxmInternAndExport("defun");
    euxls_object = euxmInternAndExport("object");
    euxls_value = euxmInternAndExport("value");

    euxls_backtracep = euxmInternAndExport("backtrace?");
    euxmSetValue(euxls_backtracep, euxs_t);

    euxls_eq = euxmInternAndExport("eq");
    euxls_eqv = euxmInternAndExport("eql");
    euxls_equal = euxmInternAndExport("%equal");
    euxls_equals = euxmInternAndExport("%=");

    euxls_import = euxmInternAndExport("import");
    euxls_only = euxmInternAndExport("only");
    euxls_except = euxmInternAndExport("except");
    euxls_rename = euxmInternAndExport("rename");
    euxls_rename_flag = euxmInternAndExport("%rename");
    euxls_syntax = euxmInternAndExport("syntax");
    euxls_export = euxmInternAndExport("export");
    euxls_expose = euxmInternAndExport("expose");

    euxls_callcc = euxmInternAndExport("call/cc");

    euxls_make = euxmInternAndExport("make");
    euxls_apply = euxmInternAndExport("apply");
    euxls_setter = euxmInternAndExport("setter");

    euxls_signal = euxmInternAndExport("signal");
    euxls_unwind_protect = euxmInternAndExport("unwind-protect");
    euxls_letname = euxmInternAndExport("let-binding");
    euxls_general_error = euxmInternAndExport("general-error");
    euxls_no_applic_error = euxmInternAndExport("no-applicable-method-error");
    euxls_no_next_md_error = euxmInternAndExport("no-next-method-error");
    euxls_bad_type_error = euxmInternAndExport("bad-type-error");
    euxls_telos_error = euxmInternAndExport("telos-error");
    euxls_teloeuxls_bad_ref = euxmInternAndExport("telos-bad-ref");
    euxls_incompatible_md = euxmInternAndExport("incompatible-method");
    euxls_unbound_error = euxmInternAndExport("unbound-error");
    euxls_arith_error = euxmInternAndExport("arith-error");
    euxls_user_intr = euxmInternAndExport("user-interrupt");
    euxls_syntax_error = euxmInternAndExport("syntax-error");
    euxls_compile_error = euxmInternAndExport("compilation-error");

    euxls_progn = euxmInternAndExport("progn");
    euxls_compile = euxmInternAndExport("compile");
    euxls_set_module = euxmInternAndExport("set-module");
    euxls_get_module = euxmInternAndExport("current-module");
    euxls_reintern = euxmInternAndExport("reintern-module-symbols");
    euxls_module_directives = euxmInternAndExport("module-directives");

    euxls_binary_plus = euxmInternAndExport("binary+");
    euxls_binary_minus = euxmInternAndExport("binary-");
    euxls_binary_times = euxmInternAndExport("binary*");
    euxls_binary_divide = euxmInternAndExport("binary/");
    euxls_quotient = euxmInternAndExport("quotient");
    euxls_binary_less = euxmInternAndExport("binary<");
    euxls_binary_equal = euxmInternAndExport("binary=");

    euxls_current_thread = euxmInternAndExport("current-self");
    euxlc_thread = euxmInternAndExport("<thread>");

    euxls_qualified_symbols = euxmInternAndExport("qualified-symbols?");
    euxmSetValue(euxls_qualified_symbols, euxmNil);

    euxls_set_generic_args = euxmInternAndExport("set-generic-args");

    euxls_syntax_error = euxmInternAndExport("syntax-error");
    #ifdef SOCK
    euxls_socket_error = euxmInternAndExport("socket-error");
    #endif

    euxls_debug = euxmInternAndExport("debug");
    euxls_xlframe = euxmInternAndExport("*xlframe*");

    euxls_arg_list = euxmInternAndExport("arg-list");
    euxls_next_methods = euxmInternAndExport("next-methods");

    #ifndef NO_CHECK_REF
    euxls_check_ref = euxmInternAndExport("check-ref");
    #endif

    #ifdef READLINE
    euxls_readline = euxmInternAndExport("*readline*");
    euxmSetValue(euxls_readline, euxs_t);
    #endif

    // Setup some synonyms
    euxmSetValue
    (
        euxmInternAndExport("not"),
        euxmGetValue(euxmInternAndExport("null?"))
    );
}

///  euxcInitWorkspace - create an initial workspace
void euxcInitWorkspace(unsigned int ssize)
{
    // Allocate memory for the workspace
    euxcAllocInit(ssize);

    // Set euxls_gcmsgs and euxls_unbound to nil
    // while the symbol handling is being bootstrapped
    euxls_gcmsgs = euxmNil;
    euxls_unbound = euxmNil;

    euxcObArray = euxcNewVector(euxmSymbolTableSize);

    euxcInitRootModule();

    // Install the built-in functions

    int i;
    euxcXFunDef *xp;
    for (i = 0, xp = xFunTab; xp->fun != NULL; ++i, ++xp)
    {
        euxcXFun(xp->name, euxmXFun, xp->fun, i);
    }

    euxcXFunDef *xcp;
    for (i = 0, xcp = xContFunTab; xcp->fun != NULL; ++i, ++xcp)
    {
        euxcXFun(xcp->name, euxmXFunCont, xcp->fun, i);
    }

    euxcFunDef *p;
    for (i = 0, p = funTab; p->fun != NULL; ++i, ++p)
    {
        euxcFun(p->name, euxmFun, p->fun, i);
    }

    // Initialize symbols used by the runtime system
    euxcInitSymbols();

    // Initialize TELOS
    euxcInitTelos();

    // Compile the default REPL
    compileString("repl.em");

    // Export all the system symbols from the root module
    euxcInitRootExports();
}

///-----------------------------------------------------------------------------
