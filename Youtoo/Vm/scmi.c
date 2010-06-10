//  Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.

///-----------------------------------------------------------------------------
/// ---                 EuLisp System 'youtoo'
///-----------------------------------------------------------------------------
///  Library: eulvm (Bytecode Interpreter -- Eutopia)
///  Authors: Andreas Kind
///  Description: fast access to level1 bindings
 **
///                !!! CHANGES ONLY TO leveli.skl.c !!!
 **
///-----------------------------------------------------------------------------

#include "eulisp.h"

static LispRef level1_tab;
static LispRef level1_syntax_tab;
static char *eul_level1_module_names;
static char *eul_level1_lexical_module_name, *eul_level1_syntax_module_name;

void fill_level1_table();
void fill_level1_syntax_table();


LispRef eul_dyn_level1_binding_info(LispRef binding_name)
{
    char *binding_name_str;

    LispRef entry;

    binding_name_str = eul_symbol_as_c_string(binding_name);
    entry = eul_fast_table_ref(level1_tab, binding_name_str);

    return entry;
}


LispRef eul_dyn_level1_syntax_binding_info(LispRef binding_name)
{
    char *binding_name_str;

    LispRef entry;

    binding_name_str = eul_symbol_as_c_string(binding_name);
    entry = eul_fast_table_ref(level1_syntax_tab, binding_name_str);

    return entry;
}


LispRef eul_dyn_level1_binding_ref(LispRef binding_name, LispRef absent)
{
    char *module_name_str;

    LispRef entry;

    int pos;

    entry = eul_dyn_level1_binding_info(binding_name);

    if (eul_null(entry))
        return absent;

    pos = eul_int_as_c_int(eul_car(entry));
    module_name_str = eul_symbol_as_c_string(eul_car(eul_cdr(entry)));

    return eul_dyn_binding_ref(module_name_str, pos);
}


LispRef eul_initialize_level1_tables()
{
    LispRef res;

    /* Allocate and register binding vector for user modules; see cg-dld.em */
    eul_dyn_create_module("user", 1024);
    /* eul_dyn_create_module("macros", 1024); */

    /* Initialize the fast lookup table for level1/user bindings */
    eul_allocate_table(level1_tab, eul_nil);
    eul_allocate_table(level1_syntax_tab, eul_nil);
    fill_level1_table();
    fill_level1_syntax_table();

    eul_allocate_vector(res, 5, eul_nil);
    slot_ref(res, 0) = level1_tab;
    slot_ref(res, 1) = level1_syntax_tab;
    eul_allocate_string(slot_ref(res, 2), eul_level1_module_names);
    eul_allocate_string(slot_ref(res, 3), eul_level1_lexical_module_name);
    eul_allocate_string(slot_ref(res, 4), eul_level1_syntax_module_name);
    return res;
}


///-----------------------------------------------------------------------------
/// Initialize level1_tab with liblevel1.i contents
/// e.g. eul_fast_table_set(level1_tab, <key>, make_entry(<pos>, <module_name>));
///-----------------------------------------------------------------------------

LispRef make_entry(int pos, char *module_str, char *origin_str)
{
    LispRef res, tmp, module_name, origin_name;

    eul_intern_symbol(module_name, module_str);
    eul_intern_symbol(origin_name, origin_str);
    eul_allocate_cons(tmp, module_name, origin_name);
    eul_allocate_cons(res, c_int_as_eul_int(pos), tmp);
    return res;
}


void fill_level1_table()
{
    eul_level1_lexical_module_name = "scheme";
    eul_level1_syntax_module_name = "scheme0";
    eul_level1_module_names =
        "level1 string convert copy integer number fpi collect compare condition event thread dynamic let-cc callback telos boot1 boot mop-defcl mop-meth mop-gf mop-inspect mop-init mop-class mop-key mop-prim mop-access mop-alloc character float stream3 vector stream stream1 lock stream2 socket list format convert1 table1 table handler symbol read scheme scheme0";
    eul_fast_table_set(level1_tab, ">=", make_entry(11, "compare", ">="));
    eul_fast_table_set(level1_tab, "char-alphabetic?", make_entry(79, "scheme",
            "char-alphabetic?"));
    eul_fast_table_set(level1_tab, "call-with-current-continuation",
        make_entry(62, "scheme", "call-with-current-continuation"));
    eul_fast_table_set(level1_tab, "even?", make_entry(60, "scheme", "even?"));
    eul_fast_table_set(level1_tab, "length", make_entry(59, "scheme",
            "length"));
    eul_fast_table_set(level1_tab, "char-upcase", make_entry(61, "scheme",
            "char-upcase"));
    eul_fast_table_set(level1_tab, "tan", make_entry(16, "math", "tan"));
    eul_fast_table_set(level1_tab, "denumerator", make_entry(4, "bigrat",
            "denumerator"));
    eul_fast_table_set(level1_tab, "make-polar", make_entry(78, "scheme",
            "make-polar"));
    eul_fast_table_set(level1_tab, "string>", make_entry(58, "scheme",
            "string>"));
    eul_fast_table_set(level1_tab, "close-output-port", make_entry(57, "scheme",
            "close-output-port"));
    eul_fast_table_set(level1_tab, "char<?", make_entry(77, "scheme",
            "char<?"));
    eul_fast_table_set(level1_tab, "not", make_entry(56, "scheme", "not"));
    eul_fast_table_set(level1_tab, "reverse", make_entry(26, "collect",
            "reverse"));
    eul_fast_table_set(level1_tab, "number->string", make_entry(112, "scheme",
            "number->string"));
    eul_fast_table_set(level1_tab, "vector", make_entry(76, "scheme",
            "vector"));
    eul_fast_table_set(level1_tab, "char-ci<?", make_entry(24, "scheme",
            "char-ci<?"));
    eul_fast_table_set(level1_tab, "number?", make_entry(55, "scheme",
            "number?"));
    eul_fast_table_set(level1_tab, "asin", make_entry(10, "math", "asin"));
    eul_fast_table_set(level1_tab, "remainder", make_entry(23, "scheme",
            "remainder"));
    eul_fast_table_set(level1_tab, "int-binary%", make_entry(34, "boot1", "%"));
    eul_fast_table_set(level1_tab, "current-input-port", make_entry(54,
            "scheme", "current-input-port"));
    eul_fast_table_set(level1_tab, "vector-set!", make_entry(52, "scheme",
            "vector-set!"));
    eul_fast_table_set(level1_tab, "char-ci=?", make_entry(111, "scheme",
            "char-ci=?"));
    eul_fast_table_set(level1_tab, "list-ref", make_entry(20, "boot",
            "list-ref"));
    eul_fast_table_set(level1_tab, "zero?", make_entry(22, "scheme", "zero?"));
    eul_fast_table_set(level1_tab, "char<=?", make_entry(53, "scheme",
            "char<=?"));
    eul_fast_table_set(level1_tab, "char-ci<=?", make_entry(21, "scheme",
            "char-ci<=?"));
    eul_fast_table_set(level1_tab, "string", make_entry(20, "scheme",
            "string"));
    eul_fast_table_set(level1_tab, "*", make_entry(24, "number", "*"));
    eul_fast_table_set(level1_tab, "for-each", make_entry(110, "scheme",
            "for-each"));
    eul_fast_table_set(level1_tab, "memq", make_entry(108, "scheme", "memq"));
    eul_fast_table_set(level1_tab, "boolean?", make_entry(51, "scheme",
            "boolean?"));
    eul_fast_table_set(level1_tab, "list->vector", make_entry(19, "scheme",
            "list->vector"));
    eul_fast_table_set(level1_tab, "memv", make_entry(75, "scheme", "memv"));
    eul_fast_table_set(level1_tab, "make-string", make_entry(18, "scheme",
            "make-string"));
    eul_fast_table_set(level1_tab, "int-zerop", make_entry(33, "boot1",
            "int-zerop"));
    eul_fast_table_set(level1_tab, "inexact->exact", make_entry(109, "scheme",
            "inexact->exact"));
    eul_fast_table_set(level1_tab, "rational?", make_entry(49, "scheme",
            "rational?"));
    eul_fast_table_set(level1_tab, "char?", make_entry(17, "scheme", "char?"));
    eul_fast_table_set(level1_tab, "rationalize", make_entry(107, "scheme",
            "rationalize"));
    eul_fast_table_set(level1_tab, "call-with-input-file", make_entry(74,
            "scheme", "call-with-input-file"));
    eul_fast_table_set(level1_tab, "write-char", make_entry(45, "scheme",
            "write-char"));
    eul_fast_table_set(level1_tab, "real-part", make_entry(106, "scheme",
            "real-part"));
    eul_fast_table_set(level1_tab, "exp", make_entry(7, "math", "exp"));
    eul_fast_table_set(level1_tab, "string-set!", make_entry(48, "scheme",
            "string-set!"));
    eul_fast_table_set(level1_tab, "int-binary+", make_entry(17, "boot1", "+"));
    eul_fast_table_set(level1_tab, "char-ready", make_entry(50, "scheme",
            "char-ready"));
    eul_fast_table_set(level1_tab, "caar", make_entry(27, "list", "caar"));
    eul_fast_table_set(level1_tab, "max", make_entry(5, "compare", "max"));
    eul_fast_table_set(level1_tab, "int-binary=", make_entry(12, "boot1", "="));
    eul_fast_table_set(level1_tab, "int-binary-", make_entry(18, "boot1", "-"));
    eul_fast_table_set(level1_tab, "symbol?", make_entry(47, "scheme",
            "symbol?"));
    eul_fast_table_set(level1_tab, "inc", make_entry(23, "boot1", "inc"));
    eul_fast_table_set(level1_tab, "string->number", make_entry(16, "scheme",
            "string->number"));
    eul_fast_table_set(level1_tab, "modulo", make_entry(104, "scheme",
            "modulo"));
    eul_fast_table_set(level1_tab, "ceiling", make_entry(9, "float",
            "ceiling"));
    eul_fast_table_set(level1_tab, "string<", make_entry(46, "scheme",
            "string<"));
    eul_fast_table_set(level1_tab, "expt", make_entry(105, "scheme", "expt"));
    eul_fast_table_set(level1_tab, "complex?", make_entry(15, "scheme",
            "complex?"));
    eul_fast_table_set(level1_tab, "exact?", make_entry(14, "scheme",
            "exact?"));
    eul_fast_table_set(level1_tab, "set-car!", make_entry(103, "scheme",
            "set-car!"));
    eul_fast_table_set(level1_tab, "substring", make_entry(7, "string",
            "substring"));
    eul_fast_table_set(level1_tab, "real?", make_entry(102, "scheme", "real?"));
    eul_fast_table_set(level1_tab, "cos", make_entry(6, "math", "cos"));
    eul_fast_table_set(level1_tab, "read", make_entry(101, "scheme", "read"));
    eul_fast_table_set(level1_tab, "+", make_entry(10, "number", "+"));
    eul_fast_table_set(level1_tab, ">", make_entry(4, "compare", ">"));
    eul_fast_table_set(level1_tab, "string<=", make_entry(100, "scheme",
            "string<="));
    eul_fast_table_set(level1_tab, "#t", make_entry(44, "scheme", "#t"));
    eul_fast_table_set(level1_tab, "integer?", make_entry(43, "scheme",
            "integer?"));
    eul_fast_table_set(level1_tab, "else", make_entry(99, "scheme", "else"));
    eul_fast_table_set(level1_tab, "min", make_entry(10, "compare", "min"));
    eul_fast_table_set(level1_tab, "string?", make_entry(13, "scheme",
            "string?"));
    eul_fast_table_set(level1_tab, "/", make_entry(73, "scheme", "/"));
    eul_fast_table_set(level1_tab, "exact->inexact", make_entry(72, "scheme",
            "exact->inexact"));
    eul_fast_table_set(level1_tab, "atan", make_entry(5, "math", "atan"));
    eul_fast_table_set(level1_tab, "vector-length", make_entry(98, "scheme",
            "vector-length"));
    eul_fast_table_set(level1_tab, "nyi", make_entry(97, "scheme", "nyi"));
    eul_fast_table_set(level1_tab, "char=?", make_entry(71, "scheme",
            "char=?"));
    eul_fast_table_set(level1_tab, "char-ci>?", make_entry(96, "scheme",
            "char-ci>?"));
    eul_fast_table_set(level1_tab, "int-binary*", make_entry(35, "boot1", "*"));
    eul_fast_table_set(level1_tab, "char-downcase", make_entry(95, "scheme",
            "char-downcase"));
    eul_fast_table_set(level1_tab, "current-output-port", make_entry(70,
            "scheme", "current-output-port"));
    eul_fast_table_set(level1_tab, "assoc", make_entry(42, "scheme", "assoc"));
    eul_fast_table_set(level1_tab, "cons", make_entry(2, "boot1", "cons"));
    eul_fast_table_set(level1_tab, "approximate", make_entry(41, "scheme",
            "approximate"));
    eul_fast_table_set(level1_tab, "char->integer", make_entry(40, "scheme",
            "char->integer"));
    eul_fast_table_set(level1_tab, "string-append", make_entry(94, "scheme",
            "string-append"));
    eul_fast_table_set(level1_tab, "lcm", make_entry(22, "number", "lcm"));
    eul_fast_table_set(level1_tab, "display", make_entry(69, "scheme",
            "display"));
    eul_fast_table_set(level1_tab, "call-with-output-file", make_entry(93,
            "scheme", "call-with-output-file"));
    eul_fast_table_set(level1_tab, "log", make_entry(14, "math", "log"));
    eul_fast_table_set(level1_tab, "round", make_entry(5, "float", "round"));
    eul_fast_table_set(level1_tab, "write", make_entry(10, "stream", "write"));
    eul_fast_table_set(level1_tab, "char>=?", make_entry(39, "scheme",
            "char>=?"));
    eul_fast_table_set(level1_tab, "dec", make_entry(45, "boot1", "dec"));
    eul_fast_table_set(level1_tab, "int-binary<", make_entry(42, "boot1", "<"));
    eul_fast_table_set(level1_tab, "<", make_entry(3, "compare", "<"));
    eul_fast_table_set(level1_tab, "vector?", make_entry(12, "scheme",
            "vector?"));
    eul_fast_table_set(level1_tab, "procedure?", make_entry(38, "scheme",
            "procedure?"));
    eul_fast_table_set(level1_tab, "gcd", make_entry(21, "number", "gcd"));
    eul_fast_table_set(level1_tab, "make-vector", make_entry(39, "boot1",
            "make-vector"));
    eul_fast_table_set(level1_tab, "char-ci>=?", make_entry(11, "scheme",
            "char-ci>=?"));
    eul_fast_table_set(level1_tab, "acos", make_entry(12, "math", "acos"));
    eul_fast_table_set(level1_tab, "string=", make_entry(68, "scheme",
            "string="));
    eul_fast_table_set(level1_tab, "imag-part", make_entry(92, "scheme",
            "imag-part"));
    eul_fast_table_set(level1_tab, "read-char", make_entry(4, "read",
            "read-char"));
    eul_fast_table_set(level1_tab, "cdr", make_entry(37, "boot1", "cdr"));
    eul_fast_table_set(level1_tab, "atom?", make_entry(37, "scheme", "atom?"));
    eul_fast_table_set(level1_tab, "cdddar", make_entry(10, "scheme",
            "cdddar"));
    eul_fast_table_set(level1_tab, "integer->char", make_entry(9, "scheme",
            "integer->char"));
    eul_fast_table_set(level1_tab, "numerator", make_entry(5, "bigrat",
            "numerator"));
    eul_fast_table_set(level1_tab, "set-cdr!", make_entry(36, "scheme",
            "set-cdr!"));
    eul_fast_table_set(level1_tab, "char-ready?", make_entry(67, "scheme",
            "char-ready?"));
    eul_fast_table_set(level1_tab, "newline", make_entry(7, "stream",
            "newline"));
    eul_fast_table_set(level1_tab, "pair?", make_entry(35, "scheme", "pair?"));
    eul_fast_table_set(level1_tab, "sqrt", make_entry(11, "math", "sqrt"));
    eul_fast_table_set(level1_tab, "close-input-port", make_entry(8, "scheme",
            "close-input-port"));
    eul_fast_table_set(level1_tab, "caddr", make_entry(22, "list", "caddr"));
    eul_fast_table_set(level1_tab, "string>=", make_entry(84, "scheme",
            "string>="));
    eul_fast_table_set(level1_tab, "list->string", make_entry(34, "scheme",
            "list->string"));
    eul_fast_table_set(level1_tab, "char>?", make_entry(89, "scheme",
            "char>?"));
    eul_fast_table_set(level1_tab, "car", make_entry(31, "boot1", "car"));
    eul_fast_table_set(level1_tab, "truncate", make_entry(3, "float",
            "truncate"));
    eul_fast_table_set(level1_tab, "call/cc", make_entry(91, "scheme",
            "call/cc"));
    eul_fast_table_set(level1_tab, "eq?", make_entry(88, "scheme", "eq?"));
    eul_fast_table_set(level1_tab, "magnitude", make_entry(87, "scheme",
            "magnitude"));
    eul_fast_table_set(level1_tab, "string-length", make_entry(85, "scheme",
            "string-length"));
    eul_fast_table_set(level1_tab, "list", make_entry(26, "boot1", "list"));
    eul_fast_table_set(level1_tab, "append", make_entry(11, "boot", "append"));
    eul_fast_table_set(level1_tab, "char-lower-case?", make_entry(90, "scheme",
            "char-lower-case?"));
    eul_fast_table_set(level1_tab, "cadr", make_entry(20, "list", "cadr"));
    eul_fast_table_set(level1_tab, "peek-char", make_entry(86, "scheme",
            "peek-char"));
    eul_fast_table_set(level1_tab, "apply", make_entry(9, "boot", "apply"));
    eul_fast_table_set(level1_tab, "assq", make_entry(33, "scheme", "assq"));
    eul_fast_table_set(level1_tab, "inexact?", make_entry(66, "scheme",
            "inexact?"));
    eul_fast_table_set(level1_tab, "char-whitespace?", make_entry(32, "scheme",
            "char-whitespace?"));
    eul_fast_table_set(level1_tab, "open-output-file", make_entry(7, "scheme",
            "open-output-file"));
    eul_fast_table_set(level1_tab, "positive?", make_entry(5, "scheme",
            "positive?"));
    eul_fast_table_set(level1_tab, "-", make_entry(2, "number", "-"));
    eul_fast_table_set(level1_tab, "abs", make_entry(19, "number", "abs"));
    eul_fast_table_set(level1_tab, "<=", make_entry(8, "compare", "<="));
    eul_fast_table_set(level1_tab, "floor", make_entry(7, "float", "floor"));
    eul_fast_table_set(level1_tab, "member", make_entry(31, "scheme",
            "member"));
    eul_fast_table_set(level1_tab, "output-port?", make_entry(30, "scheme",
            "output-port?"));
    eul_fast_table_set(level1_tab, "last-pair", make_entry(6, "scheme",
            "last-pair"));
    eul_fast_table_set(level1_tab, "symbol->string", make_entry(83, "scheme",
            "symbol->string"));
    eul_fast_table_set(level1_tab, "negative?", make_entry(65, "scheme",
            "negative?"));
    eul_fast_table_set(level1_tab, "=", make_entry(2, "compare", "="));
    eul_fast_table_set(level1_tab, "null?", make_entry(4, "scheme", "null?"));
    eul_fast_table_set(level1_tab, "string-ref", make_entry(10, "boot1",
            "string-ref"));
    eul_fast_table_set(level1_tab, "char-upper-case?", make_entry(82, "scheme",
            "char-upper-case?"));
    eul_fast_table_set(level1_tab, "sin", make_entry(3, "math", "sin"));
    eul_fast_table_set(level1_tab, "angle", make_entry(29, "scheme", "angle"));
    eul_fast_table_set(level1_tab, "char-numeric?", make_entry(28, "scheme",
            "char-numeric?"));
    eul_fast_table_set(level1_tab, "cddddr", make_entry(3, "scheme", "cddddr"));
    eul_fast_table_set(level1_tab, "string->symbol", make_entry(26, "scheme",
            "string->symbol"));
    eul_fast_table_set(level1_tab, "int-binary/", make_entry(14, "boot1", "/"));
    eul_fast_table_set(level1_tab, "odd?", make_entry(81, "scheme", "odd?"));
    eul_fast_table_set(level1_tab, "open-input-file", make_entry(27, "scheme",
            "open-input-file"));
    eul_fast_table_set(level1_tab, "eqv?", make_entry(64, "scheme", "eqv?"));
    eul_fast_table_set(level1_tab, "input-port?", make_entry(63, "scheme",
            "input-port?"));
    eul_fast_table_set(level1_tab, "list?", make_entry(25, "scheme", "list?"));
    eul_fast_table_set(level1_tab, "equal?", make_entry(2, "scheme", "equal?"));
    eul_fast_table_set(level1_tab, "assv", make_entry(80, "scheme", "assv"));
    eul_fast_table_set(level1_tab, "map", make_entry(2, "collect", "map"));
    eul_fast_table_set(level1_tab, "dynamic-variable-ref", make_entry(3,
            "dynamic", "dynamic-variable-ref"));
    eul_fast_table_set(level1_tab, "push-dynamic-variable", make_entry(2,
            "dynamic", "push-dynamic-variable"));
    eul_fast_table_set(level1_tab, "*redefine-imported-bindings*",
        make_entry(32, "i-param", "*redefine-imported-bindings*"));
    eul_fast_table_set(level1_tab, "pop-dynamic-variables", make_entry(8,
            "dynamic", "pop-dynamic-variables"));
}

void fill_level1_syntax_table()
{
    eul_fast_table_set(level1_syntax_tab, "delay", make_entry(6, "scheme0",
            "delay"));
    eul_fast_table_set(level1_syntax_tab, "cond", make_entry(3, "scheme0",
            "cond"));
    eul_fast_table_set(level1_syntax_tab, "define", make_entry(5, "scheme0",
            "define"));
    eul_fast_table_set(level1_syntax_tab, "and", make_entry(4, "scheme0",
            "and"));
    eul_fast_table_set(level1_syntax_tab, "do", make_entry(13, "scheme0",
            "do"));
    eul_fast_table_set(level1_syntax_tab, "force", make_entry(11, "scheme0",
            "force"));
    eul_fast_table_set(level1_syntax_tab, "or", make_entry(14, "scheme0",
            "or"));
    eul_fast_table_set(level1_syntax_tab, "case", make_entry(2, "scheme0",
            "case"));
    eul_fast_table_set(level1_syntax_tab, "define-syntax", make_entry(12,
            "scheme0", "define-syntax"));
    eul_fast_table_set(level1_syntax_tab, "letrec", make_entry(7, "scheme0",
            "letrec"));
    eul_fast_table_set(level1_syntax_tab, "set!", make_entry(10, "scheme0",
            "set!"));
    eul_fast_table_set(level1_syntax_tab, "begin", make_entry(9, "scheme0",
            "begin"));
    eul_fast_table_set(level1_syntax_tab, "untrace", make_entry(3, "scm0",
            "untrace"));
    eul_fast_table_set(level1_syntax_tab, "trace", make_entry(2, "scm0",
            "trace"));
}
