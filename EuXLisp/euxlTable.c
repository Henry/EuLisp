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
/// Title: Tables
///  Maintainer: Henry G. Weller
///-----------------------------------------------------------------------------
#include "euxlisp.h"

///-----------------------------------------------------------------------------
/// Forward declarations
///-----------------------------------------------------------------------------
static int tableHashEq(euxlValue key);
static int tableHashEqv(euxlValue key);
static int tableHashEqual(euxlValue key);
static int tableHashEquals(euxlValue key);

///-----------------------------------------------------------------------------
/// Functions
///-----------------------------------------------------------------------------
///  euxlMakeTable - make a table
euxlValue euxlMakeTable()
{
    static char *functionName = "make-table";

    euxlValue eqp = euxmGetValue(euxls_eq);
    euxlValue eqvp = euxmGetValue(euxls_eqv);
    euxlValue equalp = euxmGetValue(euxls_equal);
    euxlValue equals = euxmGetValue(euxls_equals);

    euxlValue comp = eqvp;
    euxlValue fill = euxmNil;

    if (euxmMoreArgs())
    {
        comp = euxmGetArg();
        if (euxmMoreArgs())
        {
            fill = euxmGetArg();
            euxmLastArg();
        }
    }

    if (comp != eqp && comp != eqvp && comp != equalp && comp != equals)
    {
        euxcCerror
        (
            "only eq, eqv, = and euxcEqual allowed as table comparators",
            comp,
            euxmNil
        );
    }

    return euxcMakeTable(comp, fill);
}

///  euxlTableRef - get an elt from a table
euxlValue euxlTableRef()
{
    static char *functionName = "table-ref";

    int (*equality) ();

    euxlValue table = euxmGetArgTable();
    euxlValue key = euxmGetArg();
    euxmLastArg();

    euxlValue comp = euxmGetTableComp(table);
    euxlValue vec = euxmGetTableTable(table);

    euxmStackCheck(2);

    int index;
    if (comp == euxmGetValue(euxls_eq))
    {
        index = tableHashEq(key);
        equality = euxcEq;
    }
    else if (comp == euxmGetValue(euxls_eqv))
    {
        index = tableHashEqv(key);
        equality = euxcEqv;
    }
    else if (comp == euxmGetValue(euxls_equals))
    {
        index = tableHashEquals(key);
        equality = euxcEquals;
    }
    else
    {
        index = tableHashEqual(key);
        equality = euxcEqual;
    }

    euxmStackPush(euxmGetElement(vec, index));
    euxmStackPush(key);
    euxcArgC = 2;
    euxlValue val = euxcAssoc(equality);

    if (val == euxmNil)
    {
        return euxmGetTableFill(table);
    }
    else
    {
        return euxmCdr(val);
    }

}

///  euxlTableSet - put an elt into a table
euxlValue euxlTableSet()
{
    static char *functionName = "table-set";

    int (*equality) ();

    euxlValue table = euxmGetArgTable();
    euxlValue key = euxmGetArg();
    euxlValue value = euxmGetArg();
    euxmLastArg();

    euxlValue comp = euxmGetTableComp(table);
    euxlValue vec = euxmGetTableTable(table);

    euxmStackCheck(3);

    int index;
    if (comp == euxmGetValue(euxls_eq))
    {
        index = tableHashEq(key);
        equality = euxcEq;
    }
    else if (comp == euxmGetValue(euxls_eqv))
    {
        index = tableHashEqv(key);
        equality = euxcEqv;
    }
    else if (comp == euxmGetValue(euxls_equals))
    {
        index = tableHashEquals(key);
        equality = euxcEquals;
    }
    else
    {
        index = tableHashEqual(key);
        equality = euxcEqual;
    }

    euxmStackPush(euxmGetElement(vec, index));
    euxmStackPush(key);
    euxcArgC = 2;
    euxlValue val = euxcAssoc(equality);

    if (val == euxmNil)  // new key
    {
        euxmStackPush(table);
        euxmStackPush(key);
        euxmStackPush(value);
        val = euxcCons(key, value);
        val = euxcCons(val, euxmGetElement(vec, index));
        euxmSetElement(vec, index, val);
        euxmStackDrop(3);
    }
    else        // replace old
    {
        euxmSetCdr(val, value);
    }

    return value;
}

///  euxlTableComparator - return the table comparator
euxlValue euxlTableComparator()
{
    static char *functionName = "table-comparator";

    euxlValue table = euxmGetArgTable();
    euxmLastArg();

    return euxmGetTableComp(table);
}

///  nasty destructive delete
static euxlValue delq(euxlValue list, euxlValue obj)
{
    if (list == euxmNil)
    {
        return euxmNil;
    }

    if (euxmCar(list) == obj)
    {
        return euxmCdr(list);
    }

    euxlValue before, after;
    for
    (
        before = list, after = euxmCdr(list);
        after;
        before = euxmCdr(before), after = euxmCdr(after)
    )
    {
        if (euxmCar(after) == obj)
        {
            euxmSetCdr(before, euxmCdr(after));
            return list;
        }
    }

    return list;
}

///  euxlTableDelete - remove an object from a table
euxlValue euxlTableDelete()
{
    static char *functionName = "table-delete";

    int (*equality) ();

    euxlValue table = euxmGetArgTable();
    euxlValue key = euxmGetArg();
    euxmLastArg();

    euxlValue comp = euxmGetTableComp(table);
    euxlValue vec = euxmGetTableTable(table);

    euxmStackCheck(2);

    int index;
    if (comp == euxmGetValue(euxls_eq))
    {
        index = tableHashEq(key);
        equality = euxcEq;
    }

    if (comp == euxmGetValue(euxls_eqv))
    {
        index = tableHashEqv(key);
        equality = euxcEqv;
    }

    if (comp == euxmGetValue(euxls_equals))
    {
        index = tableHashEquals(key);
        equality = euxcEquals;
    }
    else
    {
        index = tableHashEqual(key);
        equality = euxcEqual;
    }

    euxmStackPush(euxmGetElement(vec, index));
    euxmStackPush(key);
    euxcArgC = 2;
    euxlValue val = euxcAssoc(equality);

    if (val == euxmNil)
    {
        return euxmNil;
    }

    euxmSetElement(vec, index, delq(euxmGetElement(vec, index), val));

    return euxmCdr(val);
}

///  euxlTableSize - the number of objects in the table
euxlValue euxlTableSize()
{
    static char *functionName = "table-size";

    euxlValue table = euxmGetArgTable();
    euxmLastArg();

    euxlValue vec = euxmGetTableTable(table);

    int count = 0;
    for (int i = 0; i < euxmHashTableSize; i++)
    {
        euxlValue entry = euxmGetElement(vec, i);
        for (; entry; entry = euxmCdr(entry))
        {
            count++;
        }
    }

    return euxcMakeFPI(count);
}

///  euxlTableKeys - a list of the keys for objects in the table
euxlValue euxlTableKeys()
{
    static char *functionName = "table-keys";

    euxlValue table = euxmGetArgTable();
    euxmLastArg();

    euxlValue vec = euxmGetTableTable(table);

    euxmStackCheckPush(table);

    euxlValue keylist = euxmNil;
    for (int i = 0; i < euxmHashTableSize; i++)
    {
        euxlValue entry = euxmGetElement(vec, i);
        for (; entry; entry = euxmCdr(entry))
        {
            keylist = euxcCons(euxmCar(euxmCar(entry)), keylist);
        }
    }
    euxmStackDrop(1);

    return keylist;
}

///  euxlTableValues - a list of the objects in the table
euxlValue euxlTableValues()
{
    static char *functionName = "table-values";

    euxlValue table = euxmGetArgTable();
    euxmLastArg();

    euxlValue vec = euxmGetTableTable(table);

    euxmStackCheckPush(table);

    euxlValue vallist = euxmNil;
    for (int i = 0; i < euxmHashTableSize; i++)
    {
        euxlValue entry = euxmGetElement(vec, i);
        for (; entry; entry = euxmCdr(entry))
        {
            vallist = euxcCons(euxmCdr(euxmCar(entry)), vallist);
        }
    }
    euxmStackDrop(1);

    return vallist;
}

///  euxlTableFill - the table-fill value
euxlValue euxlTableFill()
{
    static char *functionName = "table-fill";

    euxlValue table = euxmGetArgTable();
    euxmLastArg();

    return euxmGetTableFill(table);
}

///  euxlTableSetFill - set the table-fill value
euxlValue euxlTableSetFill()
{
    static char *functionName = "set-table-fill";

    euxlValue table = euxmGetArgTable();
    euxlValue val = euxmGetArg();
    euxmLastArg();

    euxmSetTableFill(table, val);
    return val;
}

///  euxlTableClear - remove all entries
euxlValue euxlTableClear()
{
    static char *functionName = "table-clear";

    euxlValue table = euxmGetArgTable();
    euxmLastArg();

    euxlValue vec = euxmGetTableTable(table);

    for (int i = 0; i < euxmHashTableSize; i++)
    {
        euxmSetElement(vec, i, euxmNil);
    }

    return table;
}

///  tableHashEq - things may be relocated on image save/restore, thus use
//    euxcMakePtr rather than direct address: slow perhaps, as hashing time is
//    proportional to number of live segments
static int tableHashEq(euxlValue key)
{
    int hash = euxcMakePtr(euxmSymbolp(key) ? euxmGetPName(key) : key)
    % euxmHashTableSize;

    #if 0
    euxcPrin1(key, euxlStdout());
    euxcPutString(euxlStdout(), "->");
    euxcPrin1(euxcMakeFPI(hash), euxlStdout());
    euxcTerpri(euxlStdout());
    #endif

    return hash;
}

///  tableHashEqv - c.f. code for euxcEqv in euxlFunTab.c
static int tableHashEqv(euxlValue key)
{
    if (key == euxmNil)
    {
        return 0;
    }

    int val = 0;
    switch (euxmNodeType(key))
    {
        case euxmFPI:
            val = euxmGetFPI(key) % euxmHashTableSize;
            break;

        case euxmDoubleFloat:
            val = (int)(euxmGetDoubleFloat(key)) % euxmHashTableSize;
            break;

        case euxmChar:
            val = euxmGetCharCode(key) % euxmHashTableSize;
            break;

        default:
            val = tableHashEq(key);
            break;
    }

    return val < 0 ? -val : val;
}

static int tableHashEquals(euxlValue key)
{
    int val = 0;
    switch (euxmNodeType(key))
    {
        case euxmFPI:
            val = euxmGetFPI(key) % euxmHashTableSize;
            break;

        case euxmDoubleFloat:
            val = (int)(euxmGetDoubleFloat(key)) % euxmHashTableSize;
            break;

        case euxmChar:
            val = euxmGetCharCode(key) % euxmHashTableSize;
            break;

        case euxmString:
            // from euxmEnter
            val = euxcHash(euxmGetString(key), euxmHashTableSize);
            break;

        default:
            euxcCerror
            (
                "table ref on key incompatible with comparator =",
                key, euxmNil
            );
    }

    return val < 0 ? -val : val;
}

///  tableHashEqual - c.f. code for euxcEqual in euxlFunTab.c
//    this has problems on user-defined classes, where the user has
//    created new methods on equal.  We could search the entire table,
//    I suppose. :-(
//    (defmethod euxcEqual ((a <integer>) (b <foo>)) t)
static int tableHashEqual(euxlValue key)
{
    if (key == euxmNil)
    {
        return 0;
    }

    int val = 0, len = 0;
    switch (euxmNodeType(key))
    {
        case euxmFPI:
            val = euxmGetFPI(key) % euxmHashTableSize;
            break;

        case euxmDoubleFloat:
            val = (int)(euxmGetDoubleFloat(key)) % euxmHashTableSize;
            break;

        case euxmChar:
            val = euxmGetCharCode(key) % euxmHashTableSize;
            break;

        case euxmString:
            // from euxmEnter
            val = euxcHash(euxmGetString(key), euxmHashTableSize);
            break;

        case euxmVector:
            len = euxmGetSize(key);
            val = 0;
            for (int i = 0; i < len; i++)
                val =
                    (val + tableHashEqual(euxmGetElement(key, i)))
                  % euxmHashTableSize;
            break;

        case euxmCons:
            val =
                (tableHashEqual(euxmCar(key)) + tableHashEqual(euxmCdr(key)))
              % euxmHashTableSize;
            break;

        default:
            val = tableHashEq(key);
            break;
    }

    return val < 0 ? -val : val;
}


///-----------------------------------------------------------------------------
