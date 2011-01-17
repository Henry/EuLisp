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

///-----------------------------------------------------------------------------
/// Global variables
///-----------------------------------------------------------------------------
euxlValue euxcModuleList = euxmNil;
euxlValue euxcCurrentModule = euxmNil;
euxlValue euxcRootModule = euxmNil;
euxlValue euxcReinternModule = euxmNil;
euxlValue euxcKeywordArray;

///-----------------------------------------------------------------------------
/// Functions
///-----------------------------------------------------------------------------
///  euxcInitRootModule
void euxcInitRootModule()
{
    // all the keywords
    euxcKeywordArray = euxcNewVector(euxmSymbolTableSize);

    euxcRootModule = euxcMakeModule("root");
    euxcModuleList = euxcCons(euxcRootModule, euxcModuleList);
    euxcReinternModule = euxcMakeModule("reintern");
    euxcModuleList = euxcCons(euxcReinternModule, euxcModuleList);

    euxcCurrentModule = euxcRootModule;
}

///  euxcInitRootExports - export everthing from root module
void euxcInitRootExports()
{
    // Ensure all specials are entered
    for (specialFormDef *fptr = specialFormTab; fptr->name; fptr++)
    {
        euxmInternAndExport(fptr->name);
    }

    euxlValue array = euxmGetModuleSymbols(euxcRootModule);
    euxlValue exports = euxmGetModuleExports(euxcRootModule);

    for (int i = 0; i < euxmSymbolTableSize; i++)
    {
        exports = euxcAppend(euxmGetElement(array, i), exports);
    }

    euxmSetModuleExports(euxcCurrentModule, exports);
}

///  getModule
static euxlValue getModule()
{
    static char *functionName = "getModule";

    euxlValue mod;

    if (euxmMoreArgs())
    {
        euxlValue name = euxmGetArg();
        if (!euxmSymbolp(name) && !euxmStringp(name))
        {
            euxcCerror
            (
                "symbol or string wanted as module name",
                name,
                euxls_syntax_error
            );
        }

        if (euxmSymbolp(name))
        {
            name = euxmGetPName(name);
        }
        mod = euxcFindModule(name);

        if (mod == euxmNil)
        {
            euxcCerror("no such module", name, euxls_general_error);
        }
    }
    else
    {
        mod = euxcCurrentModule;
    }

    euxmLastArg();

    return mod;
}

///  euxcModuleSymbols - (module-symbols [mod])
euxlValue euxcModuleSymbols()
{
    return euxmGetModuleSymbols(getModule());
}

///  euxcModuleExports - (module-exports [mod])
euxlValue euxcModuleExports()
{
    return euxmGetModuleExports(getModule());
}

///  euxcSymbolModule - (symbol-module sym)
euxlValue euxcSymbolModule()
{
    static char *functionName = "symbol-module";

    euxlValue sym = euxmGetArgSymbol();
    euxmLastArg();
    euxlValue mod = euxmGetModule(sym);
    return mod == euxmNil ? euxmNil : euxmGetModuleName(mod);
}

///  euxcCurrentMod - (current-module)
euxlValue euxcCurrentMod()
{
    static char *functionName = "current-module";
    euxmLastArg();
    return euxmGetModuleName(euxcCurrentModule);
}

///  euxcModList - (module-list)
euxlValue euxcModList()
{
    static char *functionName = "module-list";
    euxmLastArg();
    return euxcModuleList;
}

///  euxcUnintern - (unintern sym ...)
euxlValue euxcUnintern()
{
    static char *functionName = "unintern";

    while (euxmMoreArgs())
    {
        euxlValue sym = euxmGetArgSymbol();

        char *name = euxmGetString(euxmGetPName(sym));
        euxlValue array = euxmGetModuleSymbols(euxcCurrentModule);
        int i = euxcHash(name, euxmSymbolTableSize);

        euxlValue syms1 = euxmGetElement(array, i);
        if (syms1 == euxmNil)
        {
            continue;
        }

        if (sym == euxmCar(syms1))
        {
            euxmSetElement(array, i, euxmCdr(syms1));
            continue;
        }

        for
        (
            euxlValue syms2 = euxmCdr(syms1);
            syms2;
            syms1 = euxmCdr(syms1), syms2 = euxmCdr(syms2)
        )
        {
            if (sym == euxmCar(syms2))
            {
                euxmSetCdr(syms1, euxmCdr(syms2));
                break;
            }
        }
    }

    return euxs_t;
}

///  euxlKeywordArray - (keyword-array)
euxlValue euxlKeywordArray()
{
    static char *functionName = "keyword-array";
    euxmLastArg();
    return euxcKeywordArray;
}

///  euxlSetModule - (set-module mod)
euxlValue euxlSetModule()
{
    static char *functionName = "set-module";

    euxlValue mod = euxmGetArgModule();
    euxmLastArg();

    euxcCurrentModule = mod;
    #ifdef TRACE_SETeuxmModule
    euxcPutString(euxlStdout(), "<1curmod=");
    euxcPrin1(euxcCurrentModule, euxlStdout());
    euxcPutString(euxlStdout(), ">");
    #endif
    return mod;
}

///  euxcFindModule - sym a symbol or a string that might name a module
euxlValue euxcFindModule(euxlValue sym)
{
    char *name;

    if (euxmSymbolp(sym))
    {
        name = euxmGetString(euxmGetPName(sym));
    }
    else
    {
        name = euxmGetString(sym);
    }

    for (euxlValue mods = euxcModuleList; mods; mods = euxmCdr(mods))
    {
        if (strcmp(name, euxmGetString(euxmGetModuleName(euxmCar(mods)))) == 0)
        {
            return euxmCar(mods);
        }
    }

    return euxmNil;
}

///  euxlFindModule
euxlValue euxlFindModule()
{
    static char *functionName = "find-module";

    euxlValue mod = euxmGetArg();
    euxmLastArg();

    if (euxmStringp(mod) || euxmSymbolp(mod))
    {
        return euxcFindModule(mod);
    }

    euxcBadType(mod, "string or symbol", functionName);

    return euxmNil; // not reached
}

///  euxcGetModule
euxlValue euxcGetModule(const char *name)
{
    return euxcFindModule(euxcEnterModule(name, euxcRootModule));
}


///-----------------------------------------------------------------------------
