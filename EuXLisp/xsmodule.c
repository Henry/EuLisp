// Euscheme code Copyright (c) 1994 Russell Bradford
// xsmodule.c -- module definitions

#include "xscheme.h"

LVAL module_list = NIL;
LVAL current_module = NIL;
LVAL root_module = NIL;
LVAL reintern_module = NIL;
LVAL keyword_array;
extern LVAL true, s_syntax_error, s_general_error;

typedef struct
{
    char *ft_name;
    void (*ft_fcn) ();
} FTDEF;
extern FTDEF ftab[];

void init_root_module()
{
    keyword_array = newvector(HSIZE);   // all the keywords

    root_module = cvmodule("root");
    module_list = cons(root_module, module_list);
    reintern_module = cvmodule("reintern");
    module_list = cons(reintern_module, module_list);

    current_module = root_module;
}

// export everthing from root module
void init_root_exports()
{
    int i;
    LVAL exports, array;
    FTDEF *fptr;
    extern LVAL append();

    // ensure all specials are entered
    for (fptr = ftab; fptr->ft_name; fptr++)
        xlenter(fptr->ft_name);

    array = getmsymbols(root_module);
    exports = getmexports(root_module);

    for (i = 0; i < HSIZE; i++)
    {
        exports = append(getelement(array, i), exports);
    }

    setmexports(current_module, exports);
}

static LVAL get_a_module()
{
    static char *cfn_name = "get_a_module";
    LVAL name, mod;

    if (moreargs())
    {
        name = xlgetarg();
        if (!symbolp(name) && !stringp(name))
            xlcerror("symbol or string wanted as module name", name,
            s_syntax_error);

        if (symbolp(name))
            name = getpname(name);
        mod = find_module(name);

        if (mod == NIL)
            xlcerror("no such module", name, s_general_error);
    }
    else
        mod = current_module;

    xllastarg();

    return mod;
}

// (module-symbols [mod])
LVAL module_symbols()
{
    return getmsymbols(get_a_module());
}

// (module-exports [mod])
LVAL module_exports()
{
    return getmexports(get_a_module());
}

// (symbol-module sym)
LVAL symbol_module()
{
    static char *cfn_name = "symbol-module";
    LVAL sym, mod;
    sym = xlgasymbol();
    xllastarg();
    mod = getmodule(sym);
    return mod == NIL ? NIL : getmname(mod);
}

// (current-module)
LVAL current_mod()
{
    static char *cfn_name = "current-module";
    xllastarg();
    return getmname(current_module);
}

// (module-list)
LVAL mod_list()
{
    static char *cfn_name = "module-list";
    xllastarg();
    return module_list;
}

// (unintern sym ...)
LVAL unintern()
{
    static char *cfn_name = "unintern";
    LVAL array, sym, syms1, syms2;
    int i;
    char *name;

    while (moreargs())
    {
        sym = xlgasymbol();

        name = getstring(getpname(sym));
        array = getmsymbols(current_module);
        i = hash(name, HSIZE);

        syms1 = getelement(array, i);
        if (syms1 == NIL)
            continue;

        if (sym == car(syms1))
        {
            setelement(array, i, cdr(syms1));
            continue;
        }

        for (syms2 = cdr(syms1); syms2; syms1 = cdr(syms1), syms2 = cdr(syms2))
            if (sym == car(syms2))
            {
                rplacd(syms1, cdr(syms2));
                break;
            }
    }

    return true;
}

// (keyword-array)
LVAL xkeyword_array()
{
    static char *cfn_name = "keyword-array";
    xllastarg();
    return keyword_array;
}

// (set-module mod)
LVAL xset_module()
{
    static char *cfn_name = "set-module";
    LVAL mod;

    mod = xlgamodule();
    xllastarg();

    current_module = mod;
    #ifdef TRACE_SETMODULE
    xlputstr(xstdout(), "<1curmod=");
    xlprin1(current_module, xstdout());
    xlputstr(xstdout(), ">");
    #endif
    return mod;
}

// sym a symbol or a string that might name a module
LVAL find_module(LVAL sym)
{
    LVAL mods;
    char *name;

    if (symbolp(sym))
        name = getstring(getpname(sym));
    else
        name = getstring(sym);

    for (mods = module_list; mods; mods = cdr(mods))
        if (strcmp(name, getstring(getmname(car(mods)))) == 0)
            return car(mods);

    return NIL;
}

LVAL xfind_module()
{
    static char *cfn_name = "find-module";
    LVAL mod;

    mod = xlgetarg();
    xllastarg();

    if (stringp(mod) || symbolp(mod))
        return find_module(mod);

    xlbadtype(mod, "string or symbol", cfn_name);

    return NIL; // not reached
}

LVAL get_module(char *name)
{
    return find_module(xlenter_module(name, root_module));
}
