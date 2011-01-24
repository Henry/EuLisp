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
/// Title: Print functions
///  Maintainer: Henry G. Weller
///-----------------------------------------------------------------------------
#include "euxlisp.h"

///-----------------------------------------------------------------------------
/// Global variables
///-----------------------------------------------------------------------------
int printBreadth = 200;
int printDepth = 30;

///-----------------------------------------------------------------------------
/// Local variables
///-----------------------------------------------------------------------------
static char buf[200];

///-----------------------------------------------------------------------------
/// Forward declarations
///-----------------------------------------------------------------------------
static void print(euxlValue fptr, euxlValue vptr, int escflag, int depth);
static void putAtom(euxlValue fptr, const char *tag, euxlValue val);
static void putString(euxlValue fptr, const char *str, int len);
static void putObject(euxlValue fptr, euxlValue val, int escflag, int depth);
static void putSymbol
(
    euxlValue fptr,
    const char *str,
    int escflag,
    int keyword
);
static void putFunction(euxlValue fptr, const char *tag, euxlValue val);
static void putClosure(euxlValue fptr, const char *tag, euxlValue val);
static void putCode(euxlValue fptr, const char *tag, euxlValue val);
static void putNumber(euxlValue fptr, euxmFPIType n);
static void putDoubleFloat(euxlValue fptr, euxmDoubleFloatType n);
static void putCharacter(euxlValue fptr, int ch);
static void putGeneric(euxlValue fptr, euxlValue vptr);
static void putMethod(euxlValue fptr, euxlValue vptr);
static void putSlot(euxlValue fptr, euxlValue vptr);
static void putTable(euxlValue fptr, euxlValue vptr);
static void putClassName(euxlValue fptr, euxlValue vptr);

///-----------------------------------------------------------------------------
/// Functions
///-----------------------------------------------------------------------------
///  euxcPrin1 - print an expression with quoting
void euxcPrin1(euxlValue expr, euxlValue file)
{
    print(file, expr, euxmTrue, 0);
}

///  euxcPrint - print an expression without quoting
void euxcPrint(euxlValue expr, euxlValue file)
{
    print(file, expr, euxmFalse, 0);
}

///  euxcTerpri - terminate the current print line
void euxcTerpri(euxlValue fptr)
{
    euxcPutc(fptr, '\n');
}

///  euxcPutString - output a string
void euxcPutString(euxlValue fptr, const char *str)
{
    while (*str)
    {
        euxcPutc(fptr, *str++);
    }
}

///  print - internal print function
static void print(euxlValue fptr, euxlValue vptr, int escflag, int depth)
{
    // print nil
    if (vptr == euxmNil)
    {
        euxcPutString(fptr, "()");
        return;
    }

    // Check value type
    switch (euxmNodeType(vptr))
    {
        case euxmFun:
        case euxmXFun:
            putFunction(fptr, "Fun", vptr);
            break;
        case euxmXFunCont:
            putFunction(fptr, "XFunCont", vptr);
            break;
        case euxmCons:
            {
                if (printDepth >= 0 && depth >= printDepth)
                {
                    euxcPutString(fptr, "(...)");
                    break;
                }
                euxcPutc(fptr, '(');
                int breadth = 0;
                for (euxlValue nptr = vptr; nptr != euxmNil;)
                {
                    if (printBreadth >= 0 && breadth++ >= printBreadth)
                    {
                        euxcPutString(fptr, "...");
                        break;
                    }
                    print(fptr, euxmCar(nptr), escflag, depth + 1);
                    euxlValue next;
                    if ((next = euxmCdr(nptr)) != euxmNil)
                    {
                        if (euxmConsp(next))
                        {
                            euxcPutc(fptr, ' ');
                        }
                        else
                        {
                            euxcPutString(fptr, " . ");
                            print(fptr, next, escflag, depth + 1);
                            break;
                        }
                    }
                    nptr = next;
                }
                euxcPutc(fptr, ')');
            }
            break;
        case euxmVector:
            if (printDepth >= 0 && depth >= printDepth)
            {
                euxcPutString(fptr, "#(...)");
                break;
            }
            euxcPutString(fptr, "#(");
            for (int i = 0, size = euxmGetSize(vptr); i < size; ++i)
            {
                if (i != 0)
                {
                    euxcPutc(fptr, ' ');
                }
                print(fptr, euxmGetElement(vptr, i), escflag, depth + 1);
            }
            euxcPutc(fptr, ')');
            break;
        case euxmObject:
            putObject(fptr, vptr, escflag, depth);
            break;
        case euxmSymbol:
            putSymbol
            (
                fptr,
                euxmGetString(euxmGetPName(vptr)),
                escflag,
                euxmKeywordp(vptr)
            );
            if (euxmGetValue(euxls_qualified_symbols) != euxmNil)
            {
                euxcPutc(fptr, '@');
                if (euxmGetModule(vptr) == euxmNil)
                {
                    euxcPutString(fptr, "keyword");
                }
                else
                {
                    euxcPutString
                    (
                        fptr,
                        euxmGetString(euxmGetModuleName(euxmGetModule(vptr)))
                    );
                }
            }
            break;
        case euxmPromise:
            if (euxmGetPProc(vptr) != euxmNil)
            {
                putAtom(fptr, "Promise", vptr);
            }
            else
            {
                putAtom(fptr, "Forced-promise", vptr);
            }
            break;
        case euxmClosure:
            putClosure(fptr, "simple-function", vptr);
            break;
        case euxmFPI:
            putNumber(fptr, euxmGetFPI(vptr));
            break;
        case euxmDoubleFloat:
            putDoubleFloat(fptr, euxmGetDoubleFloat(vptr));
            break;
        case euxmChar:
            if (escflag)
            {
                putCharacter(fptr, euxmGetCharCode(vptr));
            }
            else
            {
                euxcPutc(fptr, euxmGetCharCode(vptr));
            }
            break;
        case euxmString:
            if (escflag)
            {
                putString
                (
                    fptr,
                    euxmGetString(vptr),
                    euxmGetStringLength(vptr) - 1
                );
            }
            else
            {
                euxcPutString(fptr, euxmGetString(vptr));
            }
            break;
        case euxmStream:
            putAtom(fptr, "file-stream:", vptr);
            break;
        case euxmCode:
            putCode(fptr, "Code", vptr);
            break;
        case euxmContinuation:
            putAtom(fptr, "Escape-procedure", vptr);
            break;
        case euxmEnv:
            putAtom(fptr, "Environment", vptr);
            break;
        case euxmTable:
            putTable(fptr, vptr);
            break;
        case euxmFree:
            putAtom(fptr, "Free", vptr);
            break;
        case euxmModule:
            sprintf
            (
                buf,
                "#<Module %s>",
                euxmGetString(euxmGetModuleName(vptr))
            );
            euxcPutString(fptr, buf);
            break;
        case euxmMethod:
            putMethod(fptr, vptr);
            break;
        case euxmGeneric:
            putGeneric(fptr, vptr);
            break;
        case euxmSlot:
            putSlot(fptr, vptr);
            break;
        default:
            putAtom(fptr, "Foo", vptr);
            break;
    }
}

///  putAtom - output an atom
static void putAtom(euxlValue fptr, const char *tag, euxlValue val)
{
    sprintf(buf, "#<%s #", tag);
    euxcPutString(fptr, buf);
    sprintf(buf, euxmAddrFmt, (euxmOffType)val);
    euxcPutString(fptr, buf);
    euxcPutc(fptr, '>');
}

///  putString - output a string
static void putString(euxlValue fptr, const char *str, int len)
{
    // output the initial quote
    euxcPutc(fptr, '"');

    // output each character in the string
    for (int i = 0; i < len; i++)
    {
        int ch = *str++;
        switch (ch)
        {
            case '\\':
                euxcPutString(fptr, "\\\\");
                break;
            case '"':
                euxcPutString(fptr, "\\\"");
                break;
            case '\007':
                euxcPutString(fptr, "\\a");
                break;
            case '\b':
                euxcPutString(fptr, "\\b");
                break;
            case '\177':       // delete
                euxcPutString(fptr, "\\d");
                break;
            case '\f':
                euxcPutString(fptr, "\\f");
                break;
            case '\n':
                euxcPutString(fptr, "\\n");
                break;
            case '\r':
                euxcPutString(fptr, "\\r");
                break;
            case '\t':
                euxcPutString(fptr, "\\t");
                break;
            case '\v':
                euxcPutString(fptr, "\\v");
                break;
            case EOF:
                euxcPutString(fptr, "\\e");
                break;
            default:
                if (isascii(ch) && isprint(ch))
                {
                    euxcPutc(fptr, ch);
                }
                else
                {
                    sprintf(buf, "\\x%04x", ch & 0xff);
                    euxcPutString(fptr, buf);
                }
                break;
        }
    }

    // output the terminating quote
    euxcPutc(fptr, '"');
}

///  escapedId - do we need to escape this id when printing?
//    yes if (1) it contains a dodgy character
//           (2) it is the id of zero size
//           (3) it starts with the syntax of a number
//           (4) it ends with a : but is not a keyword
static int escapedId(const char *id, int keyword)
{
    for (int i = 0; id[i]; i++)
    {
        if (!isgraph(id[i]) || id[i] == '|' || id[i] == '\\')
        {
            return 1;
        }
    }

    if
    (
        strpbrk(id, "|\\#()\"',;` ")
     || id[0] == 0                                              // zero size id
     || (!keyword && id[strlen(id) - 1] == ':')
     || isdigit(id[0])                                               // 123
     || (id[0] == '.' && !id[1])                                     // |.|
     || (id[0] == '.' && id[1] && isdigit(id[1]))                    // .123
     || ((id[0] == '+' || id[0] == '-') && id[1] && (isdigit(id[1])  // +123
     || (id[1] == '.' && id[2] && isdigit(id[2]))))                  // +.123
    )
    {
        return 1;
    }
    else
    {
        return 0;
    }
}

///  putSymbol - output a symbol
static void putSymbol(euxlValue fptr, const char *str, int escflag, int keyword)
{
    // Check for printing without escapes
    if (!escflag)
    {
        euxcPutString(fptr, str);
        return;
    }

    // output each character
    if (escapedId(str, keyword))
    {
        euxcPutc(fptr, '|');
        int ch;
        while ((ch = *str++) != '\0')
        {
            if (ch == '|' || ch == '\\')
            {
                euxcPutc(fptr, '\\');
            }
            if (keyword && ch == ':' && *str == 0)
            {
                euxcPutc(fptr, '|');
                euxcPutc(fptr, ':');
                return;
            }
            euxcPutc(fptr, ch);
        }
        euxcPutc(fptr, '|');
    }
    else
    {
        int ch;
        while ((ch = *str++) != '\0')
        {
            euxcPutc(fptr, ch);
        }
    }
}

///  stripAngles - Strip the angle-brackets from the string
static void stripAngles(char *name, euxlValue fptr)
{
    int len = strlen(name);
    if (name[0] == '<' && name[len - 1] == '>')
    {
        strncpy(buf, name + 1, len - 2);
        buf[len - 2] = 0;
        putSymbol(fptr, buf, euxmTrue, euxmFalse);
    }
    else
    {
        putSymbol(fptr, name, euxmTrue, euxmFalse);
    }
}

///  take euxmCare on thread <-> lock references
static void putthread(euxlValue fptr, euxlValue val, euxlValue cls)
{
    euxcPutString(fptr, "#<");
    putClassName(fptr, cls);

    euxlValue slots;
    int i;
    for
    (
        slots = euxmGetIVar(cls, euxmSlotsId), i = 1;
        slots;
        slots = euxmCdr(slots), i++
    )
    {
        euxcPutString(fptr, " ");
        euxcPrin1(euxmGetSlotName(euxmCar(slots)), fptr);
        euxcPutString(fptr, " ");
        if
        (
            !strcmp
            (
                euxmGetString(euxmGetPName(euxmGetSlotName(euxmCar(slots)))),
                "locks"
            )
        )
        {
            int len = euxcListSize(euxmGetIVar(val, i));
            euxcPrin1(euxmMakeSmallFPI(len), fptr);
        }
        else
        {
            euxcPrin1(euxmGetIVar(val, i), fptr);
        }
    }

    sprintf(buf, ">");
    euxcPutString(fptr, buf);
}

///  putObject - output a telos object
static void putObject(euxlValue fptr, euxlValue val, int escflag, int depth)
{
    euxlValue cls = euxmGetClass(val);
    if (cls == euxmGetValue(euxlc_thread))
    {
        putthread(fptr, val, cls);
        return;
    }

    euxcPutString(fptr, "#<");
    if (euxmClassp(val))
    {
        #if 0
        putClassName(fptr, cls);
        euxcPutString(fptr, " ");
        #else
        euxcPutString(fptr, "class ");
        #endif
        char *name;
        if (euxmSymbolp(euxmGetIVar(val, euxmClassNameId)))
        {
            name = euxmGetString
            (
                euxmGetPName(euxmGetIVar(val, euxmClassNameId))
            );
        }
        else
        {
            name = "(anon)";
        }
        stripAngles(name, fptr);
    }
    else
    {
        char *name;
        if (euxmSymbolp(euxmGetIVar(cls, euxmClassNameId)))
        {
            name = euxmGetString
            (
                euxmGetPName(euxmGetIVar(cls, euxmClassNameId))
            );
        }
        else
        {
            name = "(anon)";
        }
        stripAngles(name, fptr);
        if (printDepth >= 0 && depth >= printDepth)
        {
            euxcPutString(fptr, " ...");
        }
        else
        {
            int i;
            euxlValue slots;
            for
            (
                slots = euxmGetIVar(cls, euxmSlotsId), i = 1;
                slots;
                slots = euxmCdr(slots), i++
            )
            {
                euxcPutString(fptr, " ");
                euxcPrin1(euxmGetSlotName(euxmCar(slots)), fptr);
                euxcPutString(fptr, " ");
                print(fptr, euxmGetIVar(val, i), escflag, depth + 1);
            }
        }
    }
    euxcPutString(fptr, ">");

}

///  putFunction - output a fun/ffun
static void putFunction(euxlValue fptr, const char *tag, euxlValue val)
{
    sprintf(buf, "#<%s %s>", tag, euxmGetFunName(val));
    euxcPutString(fptr, buf);
}

///  putClosure - output a closure
static void putClosure(euxlValue fptr, const char *tag, euxlValue val)
{
    putCode(fptr, tag, euxmGetCode(val));
}

///  putCode - output a code object
static void putCode(euxlValue fptr, const char *tag, euxlValue val)
{
    euxlValue name;
    if ((name = euxmGetElement(val, 1)) == euxmNil)
    {
        putAtom(fptr, tag, val);
    }
    else
    {
        sprintf(buf, "#<%s %s>", tag, euxmGetString(euxmGetPName(name)));
        euxcPutString(fptr, buf);
    }
}

///  putNumber - output a number
static void putNumber(euxlValue fptr, euxmFPIType n)
{
    euxlValue fmt = euxmGetValue(euxls_fixfmt);
    sprintf(buf, (euxmStringp(fmt) ? euxmGetString(fmt) : euxmFPIFmt), n);
    euxcPutString(fptr, buf);
}

///  putDoubleFloat - output a float
static void putDoubleFloat(euxlValue fptr, euxmDoubleFloatType n)
{
    euxlValue fmt = euxmGetValue(euxls_flofmt);
    sprintf
    (
        buf,
        (euxmStringp(fmt) ? euxmGetString(fmt) : euxmDoubleFloatFmt),
        n
    );
    euxcPutString(fptr, buf);
}

///  putCharacter - output a character value
static void putCharacter(euxlValue fptr, int ch)
{
    switch (ch)
    {
        case '\007':   // '\a'
            euxcPutString(fptr, "#\\\\a");
            break;
        case '\b':
            euxcPutString(fptr, "#\\\\b");
            break;
        case '\177':
            euxcPutString(fptr, "#\\\\d");
            break;
        case '\f':
            euxcPutString(fptr, "#\\\\f");
            break;
        case '\n':
            euxcPutString(fptr, "#\\\\n");
            break;
        case '\r':
            euxcPutString(fptr, "#\\\\r");
            break;
        case ' ':
            euxcPutString(fptr, " ");
            break;
        case '\t':
            euxcPutString(fptr, "#\\\\t");
            break;
        case '\v':
            euxcPutString(fptr, "#\\\\v");
            break;
        case EOF:
            euxcPutString(fptr, "#\\eof");
            break;
        default:
            if (ch == 0)
            {
                sprintf(buf, "#\\^@");
            }
            else if (iscntrl(ch))
            {
                sprintf(buf, "#\\^%c",
                (isupper(ch + '@') ? ch + '`' : ch + '@'));
            }
            else if (ch > '\177')
            {
                sprintf(buf, "#\\x%02X", ch);
            }
            else if (ch < 0)
            {
                sprintf(buf, "#\\x%04X", (unsigned int)ch & 0xFFFF);
            }
            else
            {
                sprintf(buf, "#\\%c", ch);
            }
            euxcPutString(fptr, buf);
            break;
    }
}

///  putGeneric
static void putGeneric(euxlValue fptr, euxlValue vptr)
{
    euxcPutString(fptr, "#<");
    putClassName(fptr, euxcClassOf(vptr));

    sprintf(buf, " %s>",
    euxmSymbolp(euxmGetGenericName(vptr)) ?
    euxmGetString(euxmGetPName(euxmGetGenericName(vptr))) : "(anon)");
    euxcPutString(fptr, buf);
}

///  putMethod
static void putMethod(euxlValue fptr, euxlValue vptr)
{
    euxcPutString(fptr, "#<");
    putClassName(fptr, euxcClassOf(vptr));

    if (euxmGenericp(euxmGetMethodGenericFun(vptr)))
    {
        euxlValue gf = euxmGetMethodGenericFun(vptr);
        sprintf(buf, " %s",
        euxmSymbolp(euxmGetGenericName(gf)) ?
        euxmGetString(euxmGetPName(euxmGetGenericName(gf))) : "(anon gf)");
        euxcPutString(fptr, buf);
    }
    else
    {
        euxcPutString(fptr, " (unattached)");
    }

    euxlValue domain = euxmGetMethodDomain(vptr);
    for (; domain; domain = euxmCdr(domain))
    {
        euxcPutString(fptr, " ");
        char *name = euxmGetString
        (
            euxmGetPName(euxmGetIVar(euxmCar(domain), euxmClassNameId))
        );
        stripAngles(name, fptr);
    }

    if (euxmGetMethodOpt(vptr))
    {
        euxcPutString(fptr, " . rest");
    }

    euxcPutString(fptr, ">");
}

///  putSlot
static void putSlot(euxlValue fptr, euxlValue vptr)
{
    euxcPutString(fptr, "#<");
    putClassName(fptr, euxcClassOf(vptr));

    sprintf(buf, " %s>", euxmGetString(euxmGetPName(euxmGetSlotName(vptr))));
    euxcPutString(fptr, buf);
}

///  putTable
static void putTable(euxlValue fptr, euxlValue vptr)
{
    euxlValue eqp = euxmGetValue(euxls_eq);
    euxlValue eqvp = euxmGetValue(euxls_eqv);
    euxlValue equalp = euxmGetValue(euxls_equal);
    euxlValue equals = euxmGetValue(euxls_equals);

    euxcPutString(fptr, "#<");
    putClassName(fptr, euxcClassOf(vptr));

    euxlValue comp = euxmGetTableComp(vptr);
    if (comp == eqp)
    {
        euxcPutString(fptr, " eq>");
    }
    else if (comp == eqvp)
    {
        euxcPutString(fptr, " eql>");
    }
    else if (comp == equalp)
    {
        euxcPutString(fptr, " equal>");
    }
    else if (comp == equals)
    {
        euxcPutString(fptr, " =>");
    }
    else
    {
        euxcPutString(fptr, " \?\?\?>");
    }
}

///  putClassName
static void putClassName(euxlValue fptr, euxlValue cls)
{
    char *name;
    if (euxmSymbolp(euxmGetIVar(cls, euxmClassNameId)))
    {
        name = euxmGetString(euxmGetPName(euxmGetIVar(cls, euxmClassNameId)));
    }
    else
    {
        name = "(anon)";
    }
    stripAngles(name, fptr);
}


///-----------------------------------------------------------------------------
