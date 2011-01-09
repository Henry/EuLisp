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
/// Title: Module definitions
///  Maintainer: Henry G. Weller
///-----------------------------------------------------------------------------

#include "euxlisp.h"

euxlValue module_list = NIL;
euxlValue current_module = NIL;
euxlValue root_module = NIL;
euxlValue reintern_module = NIL;
euxlValue keyword_array;
extern euxlValue true, s_syntax_error, s_general_error;

typedef struct
{
    char *ft_name;
    void (*ft_fcn) ();
} FTDEF;
extern FTDEF ftab[];

///-----------------------------------------------------------------------------
/// Functions
///-----------------------------------------------------------------------------
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
    extern euxlValue append();

    // ensure all specials are entered
    for (FTDEF *fptr = ftab; fptr->ft_name; fptr++)
    {
        xlenter(fptr->ft_name);
    }

    euxlValue array = getmsymbols(root_module);
    euxlValue exports = getmexports(root_module);

    for (int i = 0; i < HSIZE; i++)
    {
        exports = append(getelement(array, i), exports);
    }

    setmexports(current_module, exports);
}

static euxlValue get_a_module()
{
    static char *cfn_name = "get_a_module";

    euxlValue mod;

    if (moreargs())
    {
        euxlValue name = xlgetarg();
        if (!symbolp(name) && !stringp(name))
        {
            xlcerror
            (
                "symbol or string wanted as module name",
                name,
                s_syntax_error
            );
        }

        if (symbolp(name))
        {
            name = getpname(name);
        }
        mod = find_module(name);

        if (mod == NIL)
        {
            xlcerror("no such module", name, s_general_error);
        }
    }
    else
    {
        mod = current_module;
    }

    xllastarg();

    return mod;
}

// (module-symbols [mod])
euxlValue module_symbols()
{
    return getmsymbols(get_a_module());
}

// (module-exports [mod])
euxlValue module_exports()
{
    return getmexports(get_a_module());
}

// (symbol-module sym)
euxlValue symbol_module()
{
    static char *cfn_name = "symbol-module";

    euxlValue sym = xlgasymbol();
    xllastarg();
    euxlValue mod = getmodule(sym);
    return mod == NIL ? NIL : getmname(mod);
}

// (current-module)
euxlValue current_mod()
{
    static char *cfn_name = "current-module";
    xllastarg();
    return getmname(current_module);
}

// (module-list)
euxlValue mod_list()
{
    static char *cfn_name = "module-list";
    xllastarg();
    return module_list;
}

// (unintern sym ...)
euxlValue unintern()
{
    static char *cfn_name = "unintern";

    while (moreargs())
    {
        euxlValue sym = xlgasymbol();

        char *name = getstring(getpname(sym));
        euxlValue array = getmsymbols(current_module);
        int i = hash(name, HSIZE);

        euxlValue syms1 = getelement(array, i);
        if (syms1 == NIL)
        {
            continue;
        }

        if (sym == car(syms1))
        {
            setelement(array, i, cdr(syms1));
            continue;
        }

        for
        (
            euxlValue syms2 = cdr(syms1);
            syms2;
            syms1 = cdr(syms1), syms2 = cdr(syms2)
        )
        {
            if (sym == car(syms2))
            {
                rplacd(syms1, cdr(syms2));
                break;
            }
        }
    }

    return true;
}

// (keyword-array)
euxlValue xkeyword_array()
{
    static char *cfn_name = "keyword-array";
    xllastarg();
    return keyword_array;
}

// (set-module mod)
euxlValue xset_module()
{
    static char *cfn_name = "set-module";

    euxlValue mod = xlgamodule();
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
euxlValue find_module(euxlValue sym)
{
    char *name;

    if (symbolp(sym))
    {
        name = getstring(getpname(sym));
    }
    else
    {
        name = getstring(sym);
    }

    for (euxlValue mods = module_list; mods; mods = cdr(mods))
    {
        if (strcmp(name, getstring(getmname(car(mods)))) == 0)
        {
            return car(mods);
        }
    }

    return NIL;
}

euxlValue xfind_module()
{
    static char *cfn_name = "find-module";

    euxlValue mod = xlgetarg();
    xllastarg();

    if (stringp(mod) || symbolp(mod))
    {
        return find_module(mod);
    }

    xlbadtype(mod, "string or symbol", cfn_name);

    return NIL; // not reached
}

euxlValue get_module(char *name)
{
    return find_module(xlenter_module(name, root_module));
}


///-----------------------------------------------------------------------------
