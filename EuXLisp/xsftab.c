/// Copyright 1988 David Michael Betz
/// Copyright 1994 Russell Bradford
/// Copyright 2010 Henry G. Weller
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
/// Title: Built-in function table
///  Maintainer: Henry G. Weller
///-----------------------------------------------------------------------------

#include "xscheme.h"
#include "xssymbols.h"
#include "xsproto.h"

// include machine specific declarations
#include "osdefs.h"

int xsubrcnt = 11;              // number of XSUBR functions
int csubrcnt = 17;              // number of CSUBR functions + xsubrcnt

typedef LVAL(*FP) ();

///-----------------------------------------------------------------------------
/// Built-in functions
///-----------------------------------------------------------------------------
FUNDEF funtab[] =
{
    // functions that call eval or apply (# must match xsubrcnt)
    {"apply", (FP) xapply},
    {"call-with-current-continuation", (FP) xcallcc},
    {"call/cc", (FP) xcallcc},
    {"map-list", (FP) xmap},
    {"for-each", (FP) xforeach},
    {"call-with-input-file", (FP) xcallwi},
    {"call-with-output-file", (FP) xcallwo},
    {"load", (FP) xload},
    {"load-noisily", (FP) xloadnoisily},
    {"force", (FP) xforce},
    {"initialize-object", (FP) xinitialize_object},

    // continuations for xsubrs (# must match csubrcnt)
    {"%map1", (FP) xmap1},
    {"%for-each1", (FP) xforeach1},
    {"%with-file1", (FP) xwithfile1},
    {"%load1", (FP) xload1},
    {"%force1", (FP) xforce1},
    {"%initloop1", (FP) xinitloop1},

    // list functions
    {"cons", xcons},
    {"car", xcar},
    {"cdr", xcdr},
    {"caar", xcaar},
    {"cadr", xcadr},
    {"cdar", xcdar},
    {"cddr", xcddr},
    {"caaar", xcaaar},
    {"caadr", xcaadr},
    {"cadar", xcadar},
    {"caddr", xcaddr},
    {"cdaar", xcdaar},
    {"cdadr", xcdadr},
    {"cddar", xcddar},
    {"cdddr", xcdddr},
    {"caaaar", xcaaaar},
    {"caaadr", xcaaadr},
    {"caadar", xcaadar},
    {"caaddr", xcaaddr},
    {"cadaar", xcadaar},
    {"cadadr", xcadadr},
    {"caddar", xcaddar},
    {"cadddr", xcadddr},
    {"cdaaar", xcdaaar},
    {"cdaadr", xcdaadr},
    {"cdadar", xcdadar},
    {"cdaddr", xcdaddr},
    {"cddaar", xcddaar},
    {"cddadr", xcddadr},
    {"cdddar", xcdddar},
    {"cddddr", xcddddr},
    {"list", xlist},
    {"list*", xliststar},
    {"append", xappend},
    {"reverse-list", xreverse},
    {"last-pair", xlastpair},
    {"list-size", xsize},
    {"member-list", xmember},
    {"memv", xmemv},
    {"memq", xmemq},
    {"%assoc", xassoc},
    {"assv", xassv},
    {"assq", xassq},
    {"list-ref", xlistref},
    {"list-tail", xlisttail},

    // destructive list functions
    {"set-car", xsetcar},
    {"set-cdr", xsetcdr},

    // symbol functions
    {"symbol-exists?", xboundp},
    {"symbol-value", xsymvalue},
    {"set-symbol-value", xsetsymvalue},
    {"symbol-plist", xsymplist},
    {"set-symbol-plist", xsetsymplist},
    {"gensym", xgensym},
    {"get", xget},
    {"put", xput},
    {"get-syntax", xgetsyntax},
    {"put-syntax", xputsyntax},

    // environment functions
    {"the-environment", xtheenvironment},
    {"procedure-environment", xprocenvironment},
    {"environment?", xenvp},
    {"environment-bindings", xenvbindings},
    {"environment-parent", xenvparent},

    // vector functions
    {"vector", xvector},
    {"make-vector", xmakevector},
    {"vector-size", xvsize},
    {"vector-ref", xvref},
    {"vector-set", xvset},

    // conversion functions
    {"symbol->string", xsymstr},
    {"string->symbol", xstrsym},
    {"vector->list", xvectlist},
    {"list->vector", xlistvect},
    {"string->list", xstrlist},
    {"list->string", xliststring},
    {"char->integer", xcharint},
    {"integer->char", xintchar},
    {"string->number", xstringnum},
    {"number->string", xnumstring},

    // predicate functions
    {"null?", xnullp},
    {"atom?", xatomp},
    {"list?", xlistp},
    {"number?", xnumberp},
    {"boolean?", xbooleanp},
    {"cons?", xconsp},
    {"symbol?", xsymbolp},
    {"keyword?", xkeywordp},
    {"complex?", xfloatp},       // (1)
    {"float?", xfloatp},
    {"double-float?", xfloatp},
    {"rational?", xintegerp},   // (1)
    {"integer?", xintegerp},
    {"char?", xcharp},
    {"string?", xstringp},
    {"vector?", xvectorp},
    {"function?", xfunctionp},
    {"stream?", xstreamp},
    {"input-stream?", xinputstreamp},
    {"output-stream?", xoutputstreamp},
    {"object?", xobjectp},
    {"eof-object?", xeofobjectp},
    {"default-object?", xdefaultobjectp},
    {"eq", xeq},
    {"eql", xeqv},
    {"%equal", xequal},

    // arithmetic functions
    {"%zero?", xzerop},
    {"positive?", xpositivep},
    {"negative?", xnegativep},
    {"odd?", xoddp},
    {"even?", xevenp},
    {"exact?", xexactp},
    {"inexact?", xinexactp},
    {"truncate", xtruncate},
    {"floor", xfloor},
    {"ceiling", xceiling},
    {"round", xround},
    {"add1", xadd1},
    {"sub1", xsub1},
    {"%abs", xabs},
    {"%gcd", xgcd},
    {"random", xrandom},
    {"%+", xadd},
    {"%-", xsub},
    {"%*", xmul},
    {"%/", xdiv},
    {"%quotient", xquo},
    {"%remainder", xrem},
    {"%min", xmin},
    {"%max", xmax},
    {"sin", xsin},
    {"cos", xcos},
    {"tan", xtan},
    {"asin", xasin},
    {"acos", xacos},
    {"atan", xatan},
    {"exp", xxexp},
    {"sqrt", xsqrt},
    {"expt", xexpt},
    {"log", xxlog},

    // bitwise logical functions
    {"logand", xlogand},
    {"logior", xlogior},
    {"logxor", xlogxor},
    {"lognot", xlognot},

    // numeric comparison functions
    {"%<", xlss},
    {"%<=", xleq},
    {"%=", xeql},
    {"%>=", xgeq},
    {"%>", xgtr},

    // string functions
    {"string-size", xstrlen},
    {"string-null?", xstrnullp},
    {"string-append", xstrappend},
    {"string-ref", xstrref},
    {"string-set", xstrset},
    {"substring", xsubstring},
    {"string<?", xstrlss},
    {"string<=?", xstrleq},
    {"string=?", xstreql},
    {"string>=?", xstrgeq},
    {"string>?", xstrgtr},
    {"string-ci<?", xstrilss},
    {"string-ci<=?", xstrileq},
    {"string-ci=?", xstrieql},
    {"string-ci>=?", xstrigeq},
    {"string-ci>?", xstrigtr},
    {"make-string", xmakestring},

    // character functions
    {"char<?", xchrlss},
    {"char<=?", xchrleq},
    {"char=?", xchreql},
    {"char>=?", xchrgeq},
    {"char>?", xchrgtr},
    {"char-ci<?", xchrilss},
    {"char-ci<=?", xchrileq},
    {"char-ci=?", xchrieql},
    {"char-ci>=?", xchrigeq},
    {"char-ci>?", xchrigtr},

    // i/o functions
    {"read", xread},
    {"read-char", xrdchar},
    {"read-byte", xrdbyte},
    {"read-short", xrdshort},
    {"read-long", xrdlong},
    {"peek-char", xpeek_char},
    {"char-ready?", xchar_readyp},
    {"%write", xwrite},
    {"write-char", xwrchar},
    {"write-byte", xwrbyte},
    {"write-short", xwrshort},
    {"write-long", xwrlong},
    {"%print", xprint},
    {"printnl", xprintnl},
    {"sflush", xsflush},
    {"flush", xflush},

    // print control functions
    {"print-breadth", xprbreadth},
    {"print-depth", xprdepth},

    // file i/o functions
    {"open-input-file", xopeni},
    {"open-output-file", xopeno},
    {"open-append-file", xopena},
    {"open-update-file", xopenu},
    {"close-stream", xclose},
    {"close-input-stream", xclosei},
    {"close-output-stream", xcloseo},
    {"get-file-position", xgetfposition},
    {"set-file-position", xsetfposition},
    {"unlink", xunlink},

    // utility functions
    {"transcript-on", xtranson},
    {"transcript-off", xtransoff},
    {"getarg", xgetarg},
    {"exit", xexit},
    {"compile", xcompile},
    {"decompile", xdecompile},
    {"gc", xgc},
    {"save", xsave},
    {"restore", xrestore},
    {"reset", xreset},
    {"xserror", xerror},
    {"default-handler", default_handler},

    // debugging functions
    {"trace-on", xtraceon},
    {"trace-off", xtraceoff},

    // internal functions
    {"%car", xicar},
    {"%cdr", xicdr},
    {"%set-car", xisetcar},
    {"%set-cdr", xisetcdr},
    {"%vector-size", xivsize},
    {"%vector-ref", xivref},
    {"%vector-set", xivset},
    {"%keywords", xkeyword_array},

    // module functions
    {"module-symbols", module_symbols},
    {"module-exports", module_exports},
    {"symbol-module", symbol_module},
    {"find-module", xfind_module},
    {"current-module", current_mod},
    {"module-list", mod_list},
    {"unintern", unintern},

    // telos
    {"allocate", xallocate},
    {"find-key", xfind_key},
    {"raise-telos-error", xtelos_error},
    {"initialize-class", xinitialize_class},
    {"class-of", xclassof},
    {"describe", xdescribe},
    {"class-name", xclass_name},
    {"class-superclasses", xclass_superclasses},
    {"class-precedence-list", xclass_cpl},
    {"class-slots", xclass_slots},
    {"class-keywords", xclass_keywords},
    {"set-class-keywords", xset_class_keywords},
    {"class-subclasses", xclass_subclasses},
    {"class-instance-size", xclass_instsize},
    {"class-abstract?", xclass_abstractp},
    {"class?", xclassp},
    {"subclass?", xsubclassp},
    {"generic-name", xgf_name},
    {"generic-args", xgf_args},
    {"set-generic-args", xgf_setargs},
    {"generic-optargs?", xgf_optargs},
    {"generic-methods", xgf_methods},
    {"generic-cache1", xgf_cache1},
    {"generic-cache2", xgf_cache2},
    {"make-generic", xmake_generic},
    {"method-generic", xmethod_gf},
    {"method-function", xmethod_fun},
    {"method-domain", xmethod_domain},
    {"make-and-add-method", xmake_and_add_method},
    {"make-method", xmake_method},
    {"add-method", xadd_method},
    {"slot-name", xslot_name},
    {"slot-keyword", xslot_keyword},
    {"slot-default", xslot_default},
    {"set-slot-default", xset_slot_default},
    {"slot-required?", xslot_requiredp},
    {"set-slot-required?", xset_slot_requiredp},
    {"find-slot-index", xfind_slot_index},

    {"make-table", xmake_table},
    {"table-ref", xtable_ref},
    {"table-set", xtable_set},
    {"table-comparator", xtable_comparator},
    {"table-delete", xtable_delete},
    {"table-size", xtable_size},
    {"table-keys", xtable_keys},
    {"table-values", xtable_values},

    {"backtrace", xbacktrace},

    {"set-module", xset_module},
    {"reintern-module-symbols", xreintern},
    {"reintern-syntax", xreintern_syntax},
    {"module-directives", xmodule_directives},

    {"table-fill", xtable_fill},
    {"set-table-fill", xtable_setfill},
    {"table-clear", xtable_clear},

    {"raise-syntax-error", xsyntax_error},

    {"setivar", xsetivar},
    {"getivar", xgetivar},

    {"tmpfile", xtmpfile},
    {"getenv", xgetenv},
    {"putenv", xputenv},
    {"cpu-time", x_cpu_time},
    {"xsprintf", xsprintf},

    // debugging
    {"frame-up", xframe_up},
    {"frame-down", xframe_down},
    {"frame-env", xframe_env},
    {"frame-fun", xframe_fun},

    #ifndef NO_CHECK_REF
    {"check-ref", xcheck_ref},
    #endif

    #ifdef SOCK
    {"socket-socket", socket_socket},
    {"socket-connect", socket_connect},
    {"socket-bind", socket_bind},
    {"socket-listen", socket_listen},
    {"socket-accept", socket_accept},
    {"socket-block", socket_block},
    {"socket-nonblock", socket_nonblock},
    {"socket-reuse", socket_reuse},
    {"socket-noreuse", socket_noreuse},
    {"socket-close", socket_close},
    {"socket-shutdown", socket_shutdown},
    {"socket-peeraddr", socket_peeraddr},
    {"socket-peerstream", socket_peerstream},
    {"socket-sockaddr", socket_sockaddr},
    {"socket-sockstream", socket_sockstream},
    {"socket-host-to-ip", socket_host_to_ip},
    {"socket-ip-to-host", socket_ip_to_host},
    {"socket-convert-to-stream", socket_convert_to_stream},
    {"stream-fd", stream_fd,},
    {"stream-unbuffered", stream_unbuffered,},
    {"stream-block-buffered", stream_block_buffered,},
    {"stream-line-buffered", stream_line_buffered,},
    {"socket-fd-zero-read", socket_fd_zero_read},
    {"socket-fd-set-read", socket_fd_set_read},
    {"socket-fd-isset-read", socket_fd_isset_read},
    {"socket-select-read", socket_select_read},
    {"socket-fd-zero-write", socket_fd_zero_write},
    {"socket-fd-set-write", socket_fd_set_write},
    {"socket-fd-isset-write", socket_fd_isset_write},
    {"socket-select-write", socket_select_write},
    {"stream-xdr-send-int", stream_xdr_send_int},
    {"stream-xdr-recv-int", stream_xdr_recv_int},
    {"stream-xdr-send-float", stream_xdr_send_float},
    {"stream-xdr-recv-float", stream_xdr_recv_float},
    {"stream-xdr-send-string", stream_xdr_send_string},
    {"stream-xdr-recv-string", stream_xdr_recv_string},
    #endif

    // include machine specific table entries
    #include "osptrs.h"

    {0, 0}      // end of table marker
};

// Notes:
// (1) This version only supports integers and reals.

///-----------------------------------------------------------------------------
/// Functions
///-----------------------------------------------------------------------------
// xstdin - get the stdin stream
LVAL xstdin()
{
    return (getvalue(s_stdin));
}

// xstdout - get the stdout stream
LVAL xstdout()
{
    return (getvalue(s_stdout));
}

// eq - internal 'eq' function
int eq(LVAL arg1, LVAL arg2)
{
    if (symbolp(arg1) && symbolp(arg2))
    {
        return symboleq(arg1, arg2);
    }
    return (arg1 == arg2);
}

// eqv - internal 'eql' function
int eqv(LVAL arg1, LVAL arg2)
{
    // try the eq test first
    if (arg1 == arg2)
    {
        return (TRUE);
    }

    // compare fixnums, flonums and characters
    if (!null(arg1))
    {
        switch (ntype(arg1))
        {
            case SYMBOL:
                return (symbolp(arg2) && symboleq(arg1, arg2));
            case FIXNUM:
                return (fixp(arg2) && getfixnum(arg1) == getfixnum(arg2));
            case FLONUM:
                return (floatp(arg2) && getflonum(arg1) == getflonum(arg2));
            case CHAR:
                return (charp(arg2) && getchcode(arg1) == getchcode(arg2));
        }
    }
    return (FALSE);
}

// equal - internal 'equal' function
int equal(LVAL arg1, LVAL arg2)
{
    // try the eq test first
    if (arg1 == arg2)
    {
        return (TRUE);
    }

    // compare fixnums, flonums, characters, strings, vectors and conses
    if (!null(arg1))
    {
        switch (ntype(arg1))
        {
            case SYMBOL:
                return (symbolp(arg2) && symboleq(arg1, arg2));
            case FIXNUM:
                return (fixp(arg2) && getfixnum(arg1) == getfixnum(arg2));
            case FLONUM:
                return (floatp(arg2) && getflonum(arg1) == getflonum(arg2));
            case CHAR:
                return (charp(arg2) && getchcode(arg1) == getchcode(arg2));
            case STRING:
                return (stringp(arg2)
                && strcmp(getstring(arg1), getstring(arg2)) == 0);
            case VECTOR:
                return (vectorp(arg2) && vectorequal(arg1, arg2));
            case CONS:
                return (consp(arg2)
                && equal(car(arg1), car(arg2))
                && equal(cdr(arg1), cdr(arg2)));
        }
    }
    return (FALSE);
}

// equality of two numbers
int equals(LVAL arg1, LVAL arg2)
{
    if (fixp(arg1))
    {
        if (fixp(arg2))
        {
            return (getfixnum(arg1) == getfixnum(arg2));
        }
        else if (floatp(arg2))
        {
            return (getfixnum(arg1) == getflonum(arg2));
        }
        xlcerror("equals called with non-numeric arg", arg2, NIL);
    }
    else if (floatp(arg1))
    {
        if (fixp(arg2))
        {
            return (getflonum(arg1) == getfixnum(arg2));
        }
        else if (floatp(arg2))
        {
            return (getflonum(arg1) == getflonum(arg2));
        }
        xlcerror("equals called with non-numeric arg", arg2, NIL);
    }
    xlcerror("equals called with non-numeric arg", arg1, NIL);
    return 0;   // not reached
}

// vectorequal - compare two vectors
int vectorequal(LVAL v1, LVAL v2)
{
    // compare the vector sizes
    int len;
    if ((len = getsize(v1)) != getsize(v2))
    {
        return (FALSE);
    }

    // compare the vector elements
    for (int i = 0; i < len; ++i)
    {
        if (!equal(getelement(v1, i), getelement(v2, i)))
        {
            return (FALSE);
        }
    }

    return (TRUE);
}

// xltoofew - too few arguments to this function
LVAL xltoofew(char *cfn_name)
{
    LVAL name = cvstring(cfn_name);
    xlcerror("too few arguments", name, NIL);
    return NIL; // notreached
}

// xltoofew - too few arguments to this function
void xltoofew_int()
{
    extern LVAL xlfun;
    xlinterror("too few arguments", xlfun, NIL);
}

// xltoomany - too many arguments to this function
void xltoomany(char *cfn_name)
{
    LVAL name = cvstring(cfn_name);
    xlcerror("too many arguments", name, NIL);
}

// xltoomany - too many arguments to this function
void xltoomany_int()
{
    extern LVAL xlfun;
    xlinterror("too many arguments", xlfun, NIL);
}

// xlbadtype - incorrect argument type
// cf badargtype in xsint.c
LVAL xlbadtype(LVAL val, char *name, char *fn)
{
    extern LVAL s_bad_type_error, s_unbound;

    char buf[256];
    sprintf(buf, "incorrect type in %s", fn);

    LVAL cond = getvalue(s_bad_type_error);
    if (cond != s_unbound)
    {
        LVAL class = name[0] == '<' ?
        getvalue(xlenter_module(name, root_module)) : cvstring(name);
        setivar(cond, 3, class);        // cf condcl.em
    }

    xlcerror(buf, val, s_bad_type_error);
    return (NIL);       // never reached
}


///-----------------------------------------------------------------------------
