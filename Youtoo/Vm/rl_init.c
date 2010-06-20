///-----------------------------------------------------------------------------
/// ---               EuLisp Systems 'Youtoo' and 'EuXLisp'
///-----------------------------------------------------------------------------
///  Description: initialization of readline
///-----------------------------------------------------------------------------

#ifdef READLINE
#include <stdlib.h>
#include <stdio.h>
#include <readline/readline.h>
#include <readline/history.h>

// Name of the readline command history file
char* rl_histfile = NULL;

// Functions supplying possible keyword completions
static char **keyword_completion(const char*, int ,int);
char *keyword_generator(const char*,int);

// Add support for the punctuation character "<" to part of a keyword
const char *rl_special_prefixes = "<";

// The list of the possible keyword completions
// Note: this should be pruned to contain only those in the EuLisp definition.
char *keywords[] =
{
    // EuXLisp commands
    "transcript-on",
    "transcript-off",
    "getarg",
    "prompt?",
    "load-module?",
    "exit",
    "compile",
    "decompile",
    "gc",
    "save",
    "restore",
    "load",
    "load-noisily",
    "force",
    "delay",
    "enter-module",
    "reenter-module",
    "class-hierarchy",

    // specials
    "defmodule",
    "defconstant",
    "deflocal",
    "defun",
    "defclass",
    "defgeneric",
    "defmethod",
    "quote",
    "lambda",
    "let",
    "let*",
    "setq",
    "if",
    "cond",
    "progn",
    "and",
    "or",
    "while",
    "call-next-method",
    "next-method?",
    "apply",
    "map-list",
    "block",
    "return-from",
    "labels",
    "when",
    "unless",
    "while",
    "generic-lambda",
    "method-lambda",
    "syntax",

    // list functions
    "cons",
    "car",
    "cdr",
    "caaar",
    "caadr",
    "caar",
    "cadar",
    "cadddr",
    "caddr",
    "cadr",
    "cdaar",
    "cdadr",
    "cdar",
    "cddar",
    "cdddr",
    "cddr",
    "list",
    "list*",
    "append",
    "last-pair",
    "length",
    "list-ref",
    "list-tail",

    // symbol functions
    "bound?",
    "symbol-value",
    "symbol-plist",
    "gensym",
    "get",
    "put",

    // vector functions
    "vector",
    "make-vector",
    "vector-length",
    "vector-ref",

    // array functions
    "make-array",
    "array-ref",

    // predicates
    "eq",
    "eql",
    "null?",
    "atom",
    "list?",
    "number?",
    "boolean?",
    "cons?",
    "symbol?",
    "keyword?",
    "complex?",
    "float?",
    "double-float?",
    "rational?",
    "integer?",
    "char?",
    "string?",
    "vector?",
    "function?",
    "stream?",
    "input-stream?",
    "output-stream?",
    "object?",
    "eof-object?",
    "default-object?",
    "zero?",
    "positive?",
    "negative?",
    "odd?",
    "even?",
    "exact?",
    "inexact?",

    // arithmetic functions
    "truncate",
    "floor",
    "ceiling",
    "round",
    "abs",
    "gcd",
    "lcm",
    "random",
    "quotient",
    "remainder",
    "min",
    "max",
    "sin",
    "cos",
    "tan",
    "asin",
    "acos",
    "atan",
    "exp",
    "sqrt",
    "pow",
    "log",
    "binary+",
    "binary-",
    "unary-",
    "binary*",
    "binary/",
    "unary/",
    "binary%",
    "binary-gcd",

    // bitwise logical functions
    "logand",
    "logior",
    "logxor",
    "lognot",

    // string functions
    "make-string",
    "string-length",
    "string-null?",
    "string-append",
    "string-ref",
    "substring",

    // i/o functions
    "read",
    "read-char",
    "read-byte",
    "read-short",
    "read-long",

    "swrite",
    "write",
    "write-char",
    "write-byte",
    "write-short",
    "write-long",

    "sprin",
    "sprint",
    "prin",
    "print",

    "newline",
    "char-ready?",
    "peek-char",

    // print control functions
    "print-breadth",
    "print-depth",

    // file i/o functions
    "open-input-file",
    "open-output-file",
    "open-append-file",
    "open-update-file",
    "close-stream",
    "close-input-stream",
    "close-output-stream",
    "get-file-position",
    "unlink",

    // Standard streams
    "stdin",
    "stdout",
    "stderr",

    // debugging functions
    "trace-on",
    "trace-off",

    // module functions
    "import",
    "export",
    "expose",
    "module-symbols",
    "module-exports",
    "symbol-module",
    "current-module",
    "module-list",
    "unintern",

    // telos
    "allocate",
    "describe",
    "class?",
    "subclass?",

    // tables
    "make-table",
    "table-ref",
    "table-comparator",
    "table-delete",
    "table-length",
    "table-keys",
    "table-values",
    "table-fill",
    "table-clear",

    // plus some others
    "binary",
    "text",
    "not",
    "prin1",
    "princ",
    "eval",        // no guarantees this one will work
    "system",
    "getenv",
    "putenv",
    "tmpfile",
    "current-time",
    "ticks-per-second",
    "backtrace",
    "backtrace?",

    // thread
    "<thread>",
    "<simple-thread>",
    "make-thread",
    "thread?",
    "thread-reschedule",
    "current-thread",
    "thread-kill",
    "thread-queue",
    "current-thread",
    "thread-start",
    "thread-value",
    "thread-state",
    "<thread-condition>",
    "<thread-error>",
    "<thread-already-started>",

    "<lock>",
    "<simple-lock>",
    "make-lock",
    "lock?",
    "lock",
    "unlock",
    "<lock-condition>",
    "<lock-error>",

    "wait",
    "<wait-condition>",
    "<wait-error>",

    "let/cc",
    "with-handler",
    "unwind-protect",
    "<wrong-condition-class>",
    "signal",
    "error",
    "cerror",

    // telos
    "<object>",
    "<class>",
    "<simple-class>",
    "<list>",
    "<cons>",
    "<null>",
    "<number>",
    "<integer>",
    "<fpi>",
    "<float>",
    "<double-float>",
    "<symbol>",
    "<keyword>",
    "<string>",
    "<simple-string>",
    "<stream>",
    "<input-stream>",
    "<output-stream>",
    "<i/o-stream>",
    "<vector>",
    "<simple-vector>",
    "<char>",
    "<simple-char>",
    "<promise>",
    "<table>",
    "<hash-table>",
    "<function>",
    "<simple-function>",
    "<subr>",
    "<continuation>",
    "<generic>",
    "<simple-generic>",
    "<method>",
    "<simple-method>",
    "<slot>",
    "<local-slot>",
    "<structure>",

    "generic-prin",
    "generic-write",
    "wait",

    "make",
    "initialize",

    // setter
    "setter",

    // converter
    "converter",

    "convert",
    "<conversion-condition>",
    "<no-converter>",

    // condcl
    "defcondition",
    "condition?",
    "condition-message",
    "condition-value",
    "<condition>",
    "<telos-condition>",
    "<telos-error>",
    "<telos-general-error>",
    "<telos-bad-ref>",
    "<no-applicable-method>",
    "<no-next-method>",
    "<incompatible-method-domain>",
    "<arithmetic-condition>",
    "<arithmetic-error>",
    "<error>",
    "<general-error>",
    "<bad-type>",
    "<unbound-error>",
    "<compilation-error>",
    "<macro-error>",
    "<syntax-error>",
    "<user-interrupt>",

    // compare
    "binary<",
    "binary=",
    "max",
    "min",
    "assoc",

    // macros
    "defmacro",
    "quasiquote",
    "unquote",
    "unquote-splicing",
    "symbol-macro",
    "macroexpand",
    "macroexpand1",
    "syntax",
    "dprint",

    // collect
    "<collection-condition>",
    "<collection-error>",
    "collection?",
    "sequence?",
    "accumulate",
    "accumulate1",
    "all?",
    "any?",
    "concatenate",
    "delete",
    "do",
    "element",
    "empty?",
    "fill",
    "map",
    "member",
    "remove",
    "reverse",
    "size",
    "slice",

    // copy
    "deep-copy",
    "shallow-copy",

    // format
    "format",

    // from telosint, export them all while developing
    "class-of",
    "class-name",
    "class-superclasses",
    "class-precedence-list",
    "class-slots",
    "class-keywords",
    "class-subclasses",
    "class-instance-size",
    "class-abstract?",
    "generic-name",
    "generic-args",
    "generic-optargs?",
    "generic-methods",
    "generic-cache1",
    "generic-cache2",
    "method-generic",
    "method-function",
    "method-domain",
    "add-method",
    "slot-name",
    "slot-keyword",
    "slot-default",
    "slot-required?",
};


int eul_rl_initialize()
{
    rl_attempted_completion_function = keyword_completion;
    rl_bind_key('\t', rl_complete);

    const char* eulisp_history = "/.eulisp_history";
    char* home = getenv("HOME");

    if (home == NULL)
    {
        fprintf
        (
            stderr,
            "Cannot find environment variable HOME for reading ~%s\n",
            eulisp_history
        );

        return 0;
    }
    else
    {
        rl_histfile = malloc(strlen(home) + strlen(eulisp_history) + 1);
        strcpy(rl_histfile, home);
        strcat(rl_histfile, eulisp_history);

        if (!read_history(rl_histfile))
        {
            printf("Reading readline history from %s\n", rl_histfile);
            fflush(stdout);
        }

        return 1;
    }
}

static char **keyword_completion(const char *text , int start,  int end)
{
    // Switch-off default filename completion
    rl_attempted_completion_over = 1;
    return rl_completion_matches(text, &keyword_generator);
}

char *keyword_generator(const char *text, int state)
{
    static int list_index, len;
    char *name;

    if (!state)
    {
        list_index = 0;
        len = strlen (text);
    }

    while ((name = keywords[list_index]))
    {
        list_index++;

        if (strncmp(name, text, len) == 0)
        {
            return strdup(name);
        }
    }

    // If no names matched, then return NULL.
    return ((char *)NULL);
}

#else
int eul_rl_initialize()
{
    return 1;
}
#endif


///-----------------------------------------------------------------------------
