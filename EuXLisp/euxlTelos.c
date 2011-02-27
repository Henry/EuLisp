/// Copyright 1994 Russell Bradford0
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
/// Title: Telos for EuLisp Level-0
///  Maintainer: Henry G. Weller
///-----------------------------------------------------------------------------
#include "euxlisp.h"

///-----------------------------------------------------------------------------
/// Global variables
///-----------------------------------------------------------------------------
euxlValue euxlc_object, euxlc_class, euxlc_simple_class, euxlc_vector;

///-----------------------------------------------------------------------------
/// Local variables
///-----------------------------------------------------------------------------
static char *expect_class = "<simple-class>";

///-----------------------------------------------------------------------------
/// Forward declarations
///-----------------------------------------------------------------------------
static void mergeSlots(euxlValue obj, euxlValue super, euxlValue inits);
static void mergeKeywords(euxlValue obj, euxlValue super, euxlValue inits);
static void initializationLoop(int index, euxlValue slots, euxlValue inits);

static euxlValue redefineSlot
(
    euxlValue slotname,
    euxlValue defn,
    euxlValue slotkey,
    euxlValue reqd,
    euxlValue slots,
    euxlValue slotdesc
);

static int slotEqual();

///-----------------------------------------------------------------------------
/// Functions
///-----------------------------------------------------------------------------
///  euxcClassOf
euxlValue euxcClassOf(euxlValue obj)
{
    // small integers
    if (!euxmIsPointer(obj))
    {
        return euxmGetElement(euxlc_vector, euxmFPI);
    }

    if (obj == NULL)
    {
        return euxmGetElement(euxlc_vector, euxmNullType);
    }

    if (euxmObjectp(obj))
    {
        return euxmGetClass(obj);
    }

    if (euxmKeywordp(obj))
    {
        return euxmGetElement(euxlc_vector, euxmKeyword);
    }

    if (euxmStreamp(obj))
    {
        if ((euxmGetPFlags(obj) & euxmPortFlagInput) != 0)
        {
            if ((euxmGetPFlags(obj) & euxmPortFlagOutput) != 0)
            {
                // input/output stream
                return euxmGetElement(euxlc_vector, IeuxmOStream);
            }
            else
            {
                // input stream
                return euxmGetElement(euxlc_vector, euxmIStream);
            }
        }
        else
        {
            // output stream
            return euxmGetElement(euxlc_vector, euxmOStream);
        }
    }

    if (obj->type < euxmNTypes)
    {
        return euxmGetElement(euxlc_vector, obj->type);
    }

    euxcCerror("class-of bad object", obj, euxmNil);
    return euxmNil;
}

///  euxlClassOf
euxlValue euxlClassOf()
{
    static char *functionName = "class-of";

    euxlValue obj = euxmGetArg();
    euxmLastArg();
    return euxcClassOf(obj);
}

///  euxcDescribe
void euxcDescribe(euxlValue obj)
{
    euxlValue fptr = euxlStdout();
    euxlValue cls = euxcClassOf(obj);

    #if 0
    euxcPutString(fptr, "object: ");
    euxcPrin1(obj, fptr);
    euxcTerpri(fptr);
    euxcPutString(fptr, " class: ");
    euxcPrin1(cls, fptr);
    euxcTerpri(fptr);
    #else
    euxcPutString(fptr, "object has class: ");
    euxcPrin1(cls, fptr);
    euxcTerpri(fptr);
    #endif

    // change to switch?
    if (euxmGenericp(obj))
    {
        euxcPutString(fptr, "required argument classes ");
        euxcPrin1(euxmGetGenericArgs(obj), fptr);
        euxcTerpri(fptr);

        if (euxmGetGenericOpt(obj) != euxmNil)
        {
            euxcPutString(fptr, "with rest argument\n");
        }

        euxlValue mds = euxmGetGenericMethods(obj);
        if (mds == euxmNil)
        {
            euxcPutString(fptr, "there are no attached methods\n");
        }
        else
        {
            euxcPutString(fptr, "attached methods:\n");
            for (; mds; mds = euxmCdr(mds))
            {
                euxcPrin1(euxmCar(mds), fptr);
                euxcPutString(fptr, " ");
            }
            euxcTerpri(fptr);
        }

        return;
    }

    if (euxmMethodp(obj))
    {
        euxcPutString(fptr, "with domain ");
        euxcPrin1(euxmGetMethodDomain(obj), fptr);
        euxcTerpri(fptr);

        if (euxmGetMethodOpt(obj) != euxmNil)
        {
            euxcPutString(fptr, "with rest argument\n");
        }

        return;
    }

    if (euxmObjectp(obj))
    {
        euxlValue sds = euxmGetIVar(cls, euxmSlotsId);

        if (sds == euxmNil)
        {
            euxcPutString(fptr, "with no slots\n");
        }
        else
        {
            euxcPutString(fptr, "with slots\n");
            for (int i = 1; sds; i++, sds = euxmCdr(sds))
            {
                int len =
                    15
                  - strlen
                    (
                        euxmGetString
                        (
                            euxmGetPName(euxmGetSlotName(euxmCar(sds)))
                        )
                    );
                euxcPrin1(euxmGetSlotName(euxmCar(sds)), fptr);
                euxcPutc(fptr, ':');
                while (len-- > 0)
                {
                    euxcPutc(fptr, ' ');
                }
                euxcPrin1(euxmGetIVar(obj, i), fptr);
                euxcTerpri(fptr);
            }
        }

        return;
    }

    if (euxmSlotp(obj))
    {
        euxcPutString(fptr, "name:    ");
        euxcPrin1(euxmGetSlotName(obj), fptr);
        euxcTerpri(fptr);

        if (euxmGetSlotKey(obj) == euxls_unbound)
        {
            euxcPutString(fptr, "no keyword");
        }
        else
        {
            euxcPutString(fptr, "keyword: ");
            euxcPrin1(euxmGetSlotKey(obj), fptr);
        }

        euxcTerpri(fptr);
        euxcPutString(fptr, "has ");

        if (euxmClosurep(euxmGetSlotDefault(obj)))
        {
            euxcPutString(fptr, "a");
        }
        else
        {
            euxcPutString(fptr, "no");
        }

        euxcPutString(fptr, " default function, and is ");

        if (euxmGetSlotRequiredp(obj) == euxmNil)
        {
            euxcPutString(fptr, "not ");
        }

        euxcPutString(fptr, "required\n");

        return;
    }

    if (euxmTablep(obj))
    {
        euxcPutString(fptr, "comparator: ");
        euxcPrin1(euxmGetTableComp(obj), fptr);
        euxcPutString(fptr, "\nfill-value: ");
        euxcPrin1(euxmGetTableFill(obj), fptr);
        euxcTerpri(fptr);

        return;
    }

    if (euxmClosurep(obj))
    {
        euxcPutString(fptr, "args:      ");
        euxcPrin1(euxmGetVNames(euxmGetCode(obj)), fptr);
        euxcTerpri(fptr);

        return;
    }
}

///  euxlDescribe
euxlValue euxlDescribe()
{
    static char *functionName = "describe";

    euxlValue obj = euxmGetArg();
    euxmLastArg();
    euxcDescribe(obj);

    return euxs_t;
}

///  euxlAllocate
euxlValue euxlAllocate()
{
    static char *functionName = "allocate";

    euxlValue cls = euxmGetArg();
    euxlValue inits = euxmGetArg();
    euxmLastArg();

    if (!euxmClassp(cls))
    {
        euxcBadType(cls, expect_class, functionName);
    }

    if (!euxmListp(inits))
    {
        euxcCerror
        (
            "malformed init list in allocate",
            inits,
            euxls_telos_error
        );
    }

    if (euxmGetIVar(cls, euxmAbstractpId) != euxmNil)
    {
        euxcCerror
        (
            "attempt to allocate an abstract class",
            cls,
            euxls_telos_error
        );
    }

    return euxcNewObject(cls, euxmGetFPI(euxmGetIVar(cls, euxmInstanceSizeId)));
}

///  euxcFindKey - 'default' is a C reserved word
euxlValue euxcFindKey(euxlValue key, euxlValue inits, euxlValue deefault)
{
    euxlValue intl = inits;

    for (; inits; inits = euxmCdr(euxmCdr(inits)))
    {
        if (euxmCdr(inits) == euxmNil)
        {
            euxcCerror("odd-size init list", intl, euxls_telos_error);
        }

        if (euxmCar(inits) == key)
        {
            return euxmCar(euxmCdr(inits));
        }
    }

    return deefault;

}

///  euxlFindKey
euxlValue euxlFindKey()
{
    static char *functionName = "find-key";

    euxlValue key = euxmGetArgSymbol();

    if (!euxmKeywordp(key))
    {
        euxcCerror("not a keyword in init list", key, euxls_telos_error);
    }

    euxlValue inits = euxmGetArgList();
    euxlValue deefault = euxmGetArg();
    euxmLastArg();

    return euxcFindKey(key, inits, deefault);
}

///  euxlTelosError
euxlValue euxlTelosError()
{
    static char *functionName = "raise-telos-error";

    euxlValue msg = euxmGetArgString();
    euxlValue value = euxmGetArg();
    euxmLastArg();
    euxcCerror(euxmGetString(msg), value, euxls_telos_error);

    return euxmNil; // not reached
}

///  checkLegalKeywords
static void checkLegalKeywords(euxlValue inits, euxlValue keys)
{
    for (; inits; inits = euxmCdr(euxmCdr(inits)))
    {
        if (euxcMember(euxmCar(inits), keys, euxcEq) == euxmNil)
        {
            euxcCerror
            (
                "illegal keyword in initialize",
                euxmCar(inits),
                euxls_telos_error
            );
        }
    }
}

///  checkRequiredKeywords
static void checkRequiredKeywords(euxlValue inits, euxlValue slots)
{
    for (; slots; slots = euxmCdr(slots))
    {
        if (euxmGetSlotRequiredp(euxmCar(slots)) == euxmNil)
        {
            continue;
        }

        euxlValue key = euxmGetSlotKey(euxmCar(slots));
        euxlValue ints;

        for (ints = inits; ints; ints = euxmCdr(euxmCdr(ints)))
        {
            if (key == euxmCar(ints))
            {
                break;
            }
        }

        if (ints == NULL)
        {
            euxcCerror
            (
                "missing required keyword in initialize",
                key,
                euxls_telos_error
            );
        }
    }
}

///  euxlInitLoopCont - Continuation for initializationLoop
void euxlInitLoopCont()
{
    euxlValue val = euxcCurVal;
    euxlValue inits = euxmStackPop();
    euxlValue slots = euxmStackPop();
    euxlValue ind = euxmStackPop();
    int index = (int)euxmGetFPI(ind);
    euxcCurVal = euxmStackPop();
    euxmSetIVar(euxcCurVal, index, val);
    initializationLoop(index + 1, euxmCdr(slots), inits);
}

///  initializationLoop
static void initializationLoop(int index, euxlValue slots, euxlValue inits)
{
    if (slots == euxmNil)
    {
        euxcReturn();
        return;
    }

    euxlValue slot = euxmCar(slots);
    euxlValue slot_key = euxmGetSlotKey(slot);
    euxlValue defaultfn = euxmGetSlotDefault(slot);

    euxlValue init = euxcMember(slot_key, inits, euxcEq);
    if (init == euxmNil)
    {
        if (euxmClosurep(defaultfn))
        {
            // no keyword, a defaultfn
            euxmStackCheck(6);
            euxmStackPush(euxcCurVal);        // the object
            euxmStackPush(euxcMakeFPI((euxmFPIType) index));
            euxmStackPush(slots);
            euxmStackPush(inits);
            euxmStackPush(euxls_init_loop_cont);
            euxmStackPush(euxcCurEnv);
            euxcCurVal = defaultfn;
            euxcArgC = 0;
            euxcApply();
        }
        else
        {
            // no keyword, no default function
            euxmSetIVar(euxcCurVal, index, euxls_unbound);
            initializationLoop(index + 1, euxmCdr(slots), inits);
        }
    }
    else
    {
        // a keyword
        euxmSetIVar(euxcCurVal, index, euxmCar(euxmCdr(init)));
        initializationLoop(index + 1, euxmCdr(slots), inits);
    }
}

///  euxlInitializeObject
void euxlInitializeObject()
{
    static char *functionName = "initialize <object>";

    euxcCurVal = euxmGetArg();
    euxlValue inits = euxmGetArgList();
    euxmLastArg();

    if (!euxmObjectp(euxcCurVal))
    {
        euxcReturn();
    }
    else
    {
        if ((euxcListSize(inits) & 1) == 1)
        {
            euxcCerror
            (
                "odd-size init list in initialize",
                inits,
                euxls_telos_error
            );
        }

        euxlValue cls = euxcClassOf(euxcCurVal);

        checkLegalKeywords(inits, euxmGetIVar(cls, euxmKeywordS));
        checkRequiredKeywords(inits, euxmGetIVar(cls, euxmSlotsId));

        initializationLoop(1, euxmGetIVar(cls, euxmSlotsId), inits);
    }
}

///  euxlInitializeClass - when we get here, initialize-object
//    has already been done
euxlValue euxlInitializeClass()
{
    static char *functionName = "initialize <class>";

    euxlValue obj = euxmGetArg();
    euxlValue inits = euxmGetArgList();
    euxmLastArg();

    if (!euxmClassp(obj))
    {
        return obj;
    }

    euxmStackCheck(2);
    euxmStackPush(obj);
    euxmStackPush(inits);

    euxlValue super = euxmGetIVar(obj, euxmSuperClassId);
    if (!euxmConsp(super) || euxmCdr(super) != euxmNil)
    {
        euxcCerror
        (
            "superclass not a single-element list in initialize <class>",
            super,
            euxls_telos_error
        );
    }

    super = euxmCar(super);
    if (!euxmClassp(super))
    {
        euxcCerror
        (
            "trying to subclass a non-class in initialize <class>",
            super,
            euxls_telos_error
        );
    }

    #if 0
    if (euxmGetIVar(super, euxmAbstractpId) == euxmNil)
    {
        euxcCerror
        (
            "only subclassing of abstract classes",
            super,
            euxls_telos_error
        );
    }
    #endif

    euxlValue val = euxcCons
    (
        obj,
        euxmGetIVar(super, euxmClassPrecedenceListId)
    );
    euxmSetIVar(obj, euxmClassPrecedenceListId, val);

    mergeSlots(obj, super, inits);

    mergeKeywords(obj, super, inits);

    int size = euxcListSize(euxmGetIVar(obj, euxmSlotsId));
    euxmSetIVar(obj, euxmInstanceSizeId, euxcMakeFPI((euxmFPIType) size));

    // link into hierarchy
    val = euxcCons(obj, euxmGetIVar(super, euxmSubClassesId));
    euxmSetIVar(super, euxmSubClassesId, val);

    euxmStackDrop(2);
    return obj;
}

///  mergeSlots
static void mergeSlots(euxlValue obj, euxlValue super, euxlValue inits)
{
    euxlValue slots = euxmGetIVar(super, euxmSlotsId);
    euxlValue dirslots = euxcFindKey(euxls_direct_slots, inits, euxmNil);

    if (dirslots == euxmNil)
    {
        euxmSetIVar(obj, euxmSlotsId, slots);
        return;
    }

    slots = euxcReverseList(slots);   // easier to work on this way

    euxmStackCheck(2);
    euxmStackPush(dirslots);

    for (; dirslots; dirslots = euxmCdr(dirslots))
    {
        euxlValue slotdesc = euxmCar(dirslots);
        if (!euxmConsp(slotdesc))
        {
            euxcCerror
            (
                "bad slot description in initialize of class",
                slotdesc,
                euxls_telos_error
            );
        }

        euxlValue slotname = euxcFindKey(euxls_name, slotdesc, euxls_unbound);

        if (slotname == euxls_unbound)
        {
            euxcCerror
            (
                "missing slot name in initialize of class",
                slotdesc,
                euxls_telos_error
            );
        }

        euxlValue defn = euxcFindKey(euxls_default, slotdesc, euxls_unbound);
        euxlValue slotkey = euxcFindKey(euxls_keyword, slotdesc, euxls_unbound);
        euxlValue reqd = euxcFindKey(euxls_requiredp, slotdesc, euxls_unbound);

        euxmStackPush(slots);

        if (euxcMember(slotname, slots, slotEqual) == euxmNil)
        {
            if
            (
                reqd != euxls_unbound
             && reqd != euxmNil
             && slotkey == euxls_unbound
            )
            {
                euxcCerror
                (
                    "keyword required with no keyword defined",
                    slotdesc,
                    euxls_telos_error
                );
            }

            euxlValue new = euxcNewSlot(slotname);
            euxmSetSlotKey(new, slotkey);
            euxmSetSlotDefault(new, defn);
            euxmSetSlotRequiredp(new, reqd == euxls_unbound ? euxmNil : reqd);
            slots = euxcCons(new, slots);
        }
        else
        {
            slots =
            redefineSlot(slotname, defn, slotkey, reqd, slots, slotdesc);
        }
        euxmStackDrop(1);
    }

    slots = euxcReverseList(slots);
    euxmSetIVar(obj, euxmSlotsId, slots);

    euxmStackDrop(1);

    return;
}

///  slotEqual
static int slotEqual(euxlValue name, euxlValue slot)
{
    if (euxmSymbolp(name) && euxmSymbolp(euxmGetSlotName(slot)))
    {
        return euxmSymbolEq(name, euxmGetSlotName(slot));
    }

    return name == euxmGetSlotName(slot);
}

///  redefineSlot
//   inheritance rules:
//   inherit keywords and defaults
//   illegal to shadow a keyword from a super that has one defined
//   OK to shadow a default from a super
//   if super is required, then new is required
static euxlValue redefineSlot
(
    euxlValue slotname,
    euxlValue defn,
    euxlValue slotkey,
    euxlValue reqd,
    euxlValue slots,
    euxlValue slotdesc
)
{
    if (slots == euxmNil)
    {
        return euxmNil;
    }

    euxlValue old = euxmCar(slots);
    euxlValue new;
    if (slotEqual(slotname, old))
    {
        if
        (
            (euxmGetSlotKey(old) != euxls_unbound)
         && (slotkey != euxls_unbound)
         && (slotkey != euxmGetSlotKey(old))
        )
        {
            euxcCerror
            (
                "attempt to shadow inherited slot keyword",
                slotdesc,
                euxls_telos_error
            );
        }

        if ((euxmGetSlotRequiredp(old) != euxmNil) && (reqd == euxmNil))
        {
            euxcCerror
            (
                "attempt to make required slot keyword not required",
                slotdesc,
                euxls_telos_error
            );
        }

        new = euxcNewSlot(slotname);
        euxmSetSlotKey
        (
            new,
            slotkey == euxls_unbound ? euxmGetSlotKey(old) : slotkey
        );
        euxmSetSlotDefault
        (
            new,
            defn == euxls_unbound ? euxmGetSlotDefault(old) : defn
        );
        euxmSetSlotRequiredp
        (
            new,
            reqd == euxls_unbound ? euxmGetSlotRequiredp(old) : reqd
        );

        if
        (
            euxmGetSlotKey(new) == euxls_unbound && euxmGetSlotRequiredp(new)
         != euxmNil
        )
        {
            euxcCerror
            (
                "keyword required with no keyword defined or inherited",
                slotdesc,
                euxls_telos_error
            );
        }
    }
    else
    {
        new = euxmCar(slots);
    }

    return euxcCons
    (
        new,
        redefineSlot
        (
            slotname,
            defn,
            slotkey,
            reqd,
            euxmCdr(slots),
            slotdesc
        )
    );
}

///  mergeKeywords
static void mergeKeywords(euxlValue obj, euxlValue super, euxlValue inits)
{
    euxlValue dirkeys = euxcFindKey(euxls_direct_keywords, inits, euxmNil);
    euxlValue keys = euxmGetIVar(super, euxmKeywordS);

    for (; dirkeys; dirkeys = euxmCdr(dirkeys))
    {
        if (!euxcMember(euxmCar(dirkeys), keys, euxcEq))
        {
            keys = euxcCons(euxmCar(dirkeys), keys);
        }
    }

    euxmSetIVar(obj, euxmKeywordS, keys);
}

///  euxlClassName
euxlValue euxlClassName()
{
    static char *functionName = "class-name";

    euxlValue val = euxmGetArg();
    euxmLastArg();

    if (!euxmClassp(val))
    {
        euxcBadType(val, expect_class, functionName);
    }

    return euxmGetIVar(val, euxmClassNameId);
}

///  euxlClassSuperclasses
euxlValue euxlClassSuperclasses()
{
    static char *functionName = "class-superclasses";

    euxlValue val = euxmGetArg();
    euxmLastArg();

    if (!euxmClassp(val))
    {
        euxcBadType(val, expect_class, functionName);
    }

    return euxmGetIVar(val, euxmSuperClassId);
}

///  euxlClassSubclasses
euxlValue euxlClassSubclasses()
{
    static char *functionName = "class-subclasses";

    euxlValue val = euxmGetArg();
    euxmLastArg();

    if (!euxmClassp(val))
    {
        euxcBadType(val, expect_class, functionName);
    }

    return euxmGetIVar(val, euxmSubClassesId);
}

///  euxlClassSlots
euxlValue euxlClassSlots()
{
    static char *functionName = "class-slots";

    euxlValue val = euxmGetArg();
    euxmLastArg();

    if (!euxmClassp(val))
    {
        euxcBadType(val, expect_class, functionName);
    }

    return euxmGetIVar(val, euxmSlotsId);
}

///  euxlClassKeywords
euxlValue euxlClassKeywords()
{
    static char *functionName = "class-keywords";

    euxlValue val = euxmGetArg();
    euxmLastArg();

    if (!euxmClassp(val))
    {
        euxcBadType(val, expect_class, functionName);
    }

    return euxmGetIVar(val, euxmKeywordS);
}

///  euxlSetClassKeywords
euxlValue euxlSetClassKeywords()
{
    static char *functionName = "set-class-keywords";

    euxlValue cl = euxmGetArg();
    euxlValue val = euxmGetArgList();
    euxmLastArg();

    if (!euxmClassp(cl))
    {
        euxcBadType(cl, expect_class, functionName);
    }

    euxmSetIVar(cl, euxmKeywordS, val);

    return val;
}

///  euxlClassInstsize
euxlValue euxlClassInstsize()
{
    static char *functionName = "class-instance-size";

    euxlValue val = euxmGetArg();
    euxmLastArg();

    if (!euxmClassp(val))
    {
        euxcBadType(val, expect_class, functionName);
    }

    return euxmGetIVar(val, euxmInstanceSizeId);
}

///  euxlClassAbstractp
euxlValue euxlClassAbstractp()
{
    static char *functionName = "class-abstract?";

    euxlValue val = euxmGetArg();
    euxmLastArg();

    if (!euxmClassp(val))
    {
        euxcBadType(val, expect_class, functionName);
    }

    return euxmGetIVar(val, euxmAbstractpId);
}

///  euxlClassCpl
euxlValue euxlClassCpl()
{
    static char *functionName = "class-precedence-list";

    euxlValue val = euxmGetArg();
    euxmLastArg();

    if (!euxmClassp(val))
    {
        euxcBadType(val, expect_class, functionName);
    }

    return euxmGetIVar(val, euxmClassPrecedenceListId);
}

///  euxlClassp
euxlValue euxlClassp()
{
    static char *functionName = "class?";

    euxlValue obj = euxmGetArg();
    euxmLastArg();

    return euxmClassp(obj) ? euxs_t : euxmNil;
}

///  euxcSubClassp - cl1 and cl2 are classes
int euxcSubClassp(euxlValue cl1, euxlValue cl2)
{
    euxlValue cpl = euxmGetIVar(cl1, euxmClassPrecedenceListId);

    for (; cpl; cpl = euxmCdr(cpl))
    {
        if (cl2 == euxmCar(cpl))
        {
            return euxmTrue;
        }
    }

    return euxmFalse;
}

///  euxlSubClassp
euxlValue euxlSubClassp()
{
    static char *functionName = "subclass?";

    euxlValue cl1 = euxmGetArg();
    euxlValue cl2 = euxmGetArg();

    if (!euxmClassp(cl1))
    {
        euxcBadType(cl1, expect_class, functionName);
    }

    if (!euxmClassp(cl2))
    {
        euxcBadType(cl2, expect_class, functionName);
    }

    return euxcSubClassp(cl1, cl2) ? euxs_t : euxmNil;
}

///  euxlSetIVar
euxlValue euxlSetIVar()
{
    static char *functionName = "euxmSetIVar";

    euxlValue obj = euxmGetArgObject();
    euxlValue index = euxmGetArgFPI();
    euxlValue val = euxmGetArg();
    euxmLastArg();

    int len = euxmGetFPI(euxmGetIVar(euxcClassOf(obj), euxmInstanceSizeId));
    int i = euxmGetFPI(index);

    if (i < 1 || i > len)
    {
        euxcCerror
        (
            "index out of range in euxmSetIVar",
            index,
            euxls_telos_error
        );
    }

    euxmSetIVar(obj, i, val);

    return val;
}

///  euxlGetIVar
euxlValue euxlGetIVar()
{
    static char *functionName = "euxmGetIVar";

    euxlValue obj = euxmGetArgObject();
    euxlValue index = euxmGetArgFPI();
    euxmLastArg();

    int len = euxmGetFPI(euxmGetIVar(euxcClassOf(obj), euxmInstanceSizeId));
    int i = euxmGetFPI(index);

    if (i < 1 || i > len)
    {
        euxcCerror
        (
            "index out of range in euxmGetIVar",
            index,
            euxls_telos_error
        );
    }

    return euxmGetIVar(obj, i);
}

///  euxlGfName
euxlValue euxlGfName()
{
    static char *functionName = "generic-name";

    euxlValue gf = euxmGetArgGeneric();
    euxmLastArg();

    return euxmGetGenericName(gf);
}

///  euxlGfArgs
euxlValue euxlGfArgs()
{
    static char *functionName = "generic-args";

    euxlValue gf = euxmGetArgGeneric();
    euxmLastArg();

    return euxmGetGenericArgs(gf);
}

///  euxlGfSetargs
euxlValue euxlGfSetargs()
{
    static char *functionName = "set-generic-args";

    euxlValue gf = euxmGetArgGeneric();
    euxlValue val = euxmGetArgList();
    euxmLastArg();

    for (euxlValue cls = val; cls; cls = euxmCdr(cls))
    {
        if (!euxmClassp(euxmCar(cls)))
        {
            euxcBadType(euxmCar(cls), expect_class, "defgeneric");
        }
    }

    euxmSetGenericArgs(gf, val);

    return val;
}

///  euxlGfOptargs
euxlValue euxlGfOptargs()
{
    static char *functionName = "generic-optargs?";

    euxlValue gf = euxmGetArgGeneric();
    euxmLastArg();

    return euxmGetGenericOpt(gf);
}

///  euxlGfMethods
euxlValue euxlGfMethods()
{
    static char *functionName = "generic-methods";

    euxlValue gf = euxmGetArgGeneric();
    euxmLastArg();

    return euxmGetGenericMethods(gf);
}

///  euxlGfCache1
euxlValue euxlGfCache1()
{
    static char *functionName = "generic-cache1";

    euxlValue gf = euxmGetArgGeneric();
    euxmLastArg();

    return euxmGetGenericCache1(gf);
}

///  euxlGfCache2
euxlValue euxlGfCache2()
{
    static char *functionName = "generic-cache2";

    euxlValue gf = euxmGetArgGeneric();
    euxmLastArg();

    return euxmGetGenericCache2(gf);
}

///  euxlMethodGf
euxlValue euxlMethodGf()
{
    static char *functionName = "method-generic";

    euxlValue md = euxmGetArgMethod();
    euxmLastArg();

    return euxmGetMethodGenericFun(md);
}

///  euxlMethodFun
euxlValue euxlMethodFun()
{
    static char *functionName = "method-function";

    euxlValue md = euxmGetArgMethod();
    euxmLastArg();

    return euxmGetMethodFun(md);
}

///  euxlMethodDomain
euxlValue euxlMethodDomain()
{
    static char *functionName = "method-domain";

    euxlValue md = euxmGetArgMethod();
    euxmLastArg();

    return euxmGetMethodDomain(md);
}

///  euxlSlotName
euxlValue euxlSlotName()
{
    static char *functionName = "slot-name";

    euxlValue slot = euxmGetArgSlot();
    euxmLastArg();

    return euxmGetSlotName(slot);
}

///  euxlSlotKeyword
euxlValue euxlSlotKeyword()
{
    static char *functionName = "slot-keyword";

    euxlValue slot = euxmGetArgSlot();
    euxmLastArg();

    return euxmGetSlotKey(slot);
}

///  euxlSlotDefault
euxlValue euxlSlotDefault()
{
    static char *functionName = "slot-default";

    euxlValue slot = euxmGetArgSlot();
    euxmLastArg();
    return euxmGetSlotDefault(slot);
}

///  euxlSetSlotDefault
euxlValue euxlSetSlotDefault()
{
    static char *functionName = "set-slot-default";

    euxlValue slot = euxmGetArgSlot();
    euxlValue val = euxmGetArg();
    euxmLastArg();

    euxmSetSlotDefault(slot, val);
    return slot;
}

///  euxlSlotRequiredp
euxlValue euxlSlotRequiredp()
{
    static char *functionName = "slot-required-p";

    euxlValue slot = euxmGetArgSlot();
    euxmLastArg();
    return euxmGetSlotRequiredp(slot);
}

///  euxlSetSlotRequiredp
euxlValue euxlSetSlotRequiredp()
{
    static char *functionName = "set-slot-required-p";

    euxlValue slot = euxmGetArgSlot();
    euxlValue val = euxmGetArg();
    euxmLastArg();

    euxmSetSlotRequiredp(slot, val);
    return slot;
}

///  euxlFindSlotIndex
euxlValue euxlFindSlotIndex()
{
    static char *functionName = "find-slot-index";

    euxlValue name = euxmGetArgSymbol();
    euxlValue cls = euxmGetArgObject();
    euxmLastArg();

    if (!euxmClassp(cls))
    {
        euxcCerror("not a class in find-slot", cls, euxls_telos_error);
    }

    euxlValue slots = euxmGetIVar(cls, euxmSlotsId);

    for (int index = 1; slots; slots = euxmCdr(slots), index++)
    {
        if (slotEqual(name, euxmCar(slots)))
        {
            return euxcMakeFPI(index);
        }
    }

    return euxmNil;
}

///  initClass
static euxlValue initClass
(
    int type,
    const char *name,
    euxlValue super,
    euxlValue absp
)
{
    euxlValue cl = euxcNewObject(euxlc_simple_class, euxmClassSize);
    euxlValue sym = euxmInternAndExport(name);
    euxmSetValue(sym, cl);
    euxmSetIVar(cl, euxmClassNameId, sym);
    euxmSetIVar(cl, euxmSuperClassId, euxcCons(super, euxmNil));
    euxmSetIVar
    (
        cl,
        euxmClassPrecedenceListId,
        euxcCons(cl, euxmGetIVar(super, euxmClassPrecedenceListId))
    );
    euxmSetIVar(cl, euxmSlotsId, euxmNil);
    euxmSetIVar(cl, euxmKeywordS, euxmNil);
    euxmSetIVar(cl, euxmSubClassesId, euxmNil);
    euxmSetIVar(cl, euxmInstanceSizeId, euxcMakeFPI((euxmFPIType) 0));
    euxmSetIVar(cl, euxmAbstractpId, absp);

    euxmSetIVar
    (
        super,
        euxmSubClassesId,
        euxcCons(cl, euxmGetIVar(super, euxmSubClassesId))
    );

    if (type >= 0)
    {
        euxmSetElement(euxlc_vector, type, cl);
    }

    return cl;
}

///  initBuiltinClasses
static void initBuiltinClasses()
{
    // incl. euxmNullType, euxmKeyword, euxmIStream, euxmOStream, IeuxmOStream
    euxlc_vector = euxcNewVector(euxmNTypes + euxmExtraTypes);

    euxlValue char_cl =
        initClass(-1, "<char>", euxlc_object, euxs_t);
    initClass(euxmChar, "<simple-char>", char_cl, euxmNil);

    // condition is created in Level-0/condition.em

    euxlValue function_cl =
        initClass(-1, "<function>", euxlc_object, euxs_t);
    initClass(euxmContinuation, "<continuation>", function_cl, euxmNil);
    euxlValue simplefun_cl =
        initClass(-1, "<simple-function>", function_cl, euxmNil);
    euxlValue generic_cl =
        initClass(-1, "<generic>", function_cl, euxs_t);
    initClass(euxmGeneric, "<simple-generic>", generic_cl, euxmNil);

    euxlValue collection_cl =
        initClass(-1, "<collection>", euxlc_object, euxs_t);
    euxlValue sequence_cl =
        initClass(-1, "<sequence>", collection_cl, euxs_t);
    euxlValue list_cl =
        initClass(-1, "<list>", sequence_cl, euxs_t);
    initClass(euxmCons, "<cons>", list_cl, euxmNil);
    initClass(euxmNTypes, "<null>", list_cl, euxmNil);
    euxlValue character_sequence_cl =
    initClass(-1, "<character-sequence>", sequence_cl, euxs_t);
    initClass(euxmString, "<string>", character_sequence_cl, euxmNil);
    initClass(euxmVector, "<vector>", sequence_cl, euxmNil);
    euxlValue table_cl =
    initClass(-1, "<table>", collection_cl, euxs_t);
    initClass(euxmTable, "<hash-table>", table_cl, euxmNil);

    initClass(-1, "<lock>", euxlc_object, euxs_t);

    euxlValue number_cl =
        initClass(-1, "<number>", euxlc_object, euxs_t);
    euxlValue integer_cl =
        initClass(-1, "<integer>", number_cl, euxs_t);
    initClass(euxmFPI, "<fpi>", integer_cl, euxmNil);

    euxlValue float_cl =
        initClass(-1, "<float>", number_cl, euxs_t);
    initClass(euxmDoubleFloat, "<double-float>", float_cl, euxmNil);

    euxlValue stream_cl =
        initClass(euxmStream, "<stream>", euxlc_object, euxs_t);
    initClass(euxmIStream, "<input-stream>", stream_cl, euxmNil);
    initClass(euxmOStream, "<output-stream>", stream_cl, euxmNil);
    initClass(IeuxmOStream, "<i/o-stream>", stream_cl, euxmNil);

    euxlValue name_cl =
        initClass(euxmSymbol, "<name>", euxlc_object, euxs_t);
    initClass(euxmSymbol, "<symbol>", name_cl, euxmNil);
    initClass(euxmKeyword, "<keyword>", name_cl, euxmNil);

    // EuXLisp specific built-in classes

    initClass(euxmPromise, "<promise>", euxlc_object, euxmNil);
    initClass(euxmEnv, "<env>", euxlc_object, euxmNil);
    initClass(euxmCode, "<code>", euxlc_object, euxmNil);
    initClass(euxmModule, "<module>", euxlc_object, euxmNil);

    initClass(euxmClosure, "<closure>", simplefun_cl, euxmNil);
    initClass(euxmFun, "<fun>", simplefun_cl, euxmNil);
    initClass(euxmXFun, "<xfun>", euxlc_object, euxmNil);
    initClass(euxmXFunCont, "<xfuncont>", euxlc_object, euxmNil);

    euxlValue method_cl =
        initClass(-1, "<method>", euxlc_object, euxs_t);
    initClass(euxmMethod, "<simple-method>", method_cl, euxmNil);

    euxlValue slot_cl =
        initClass(-1, "<slot>", euxlc_object, euxs_t);
    initClass(euxmSlot, "<local-slot>", slot_cl, euxmNil);
}

///  euxcInitTelos
void euxcInitTelos()
{
    euxlValue scls = euxmInternAndExport("<simple-class>");
    euxlc_simple_class = euxcNewObject(euxmNil, euxmClassSize);
    euxmSetClass(euxlc_simple_class, euxlc_simple_class);
    euxmSetValue(scls, euxlc_simple_class);

    euxlValue cls = euxmInternAndExport("<class>");
    euxlc_class = euxcNewObject(euxlc_simple_class, euxmClassSize);
    euxmSetValue(cls, euxlc_class);

    euxlValue obj = euxmInternAndExport("<object>");
    euxlc_object = euxcNewObject(euxlc_simple_class, euxmClassSize);
    euxmSetValue(obj, euxlc_object);

    euxmSetIVar
    (
        euxlc_object,
        euxmClassNameId, obj
    );
    euxmSetIVar
    (
        euxlc_object,
        euxmSuperClassId,
        euxcCons(euxlc_object, euxmNil)
    );
    euxmSetIVar
    (
        euxlc_object,
        euxmClassPrecedenceListId,
        euxcCons(euxlc_object, euxmNil)
    );
    euxmSetIVar
    (
        euxlc_object,
        euxmSlotsId,
        euxmNil
    );
    euxmSetIVar
    (
        euxlc_object,
        euxmKeywordS,
        euxmNil
    );
    euxmSetIVar
    (
        euxlc_object,
        euxmSubClassesId, euxcCons(euxlc_class, euxmNil)
    );
    euxmSetIVar
    (
        euxlc_object,
        euxmInstanceSizeId, euxcMakeFPI((euxmFPIType) 0)
    );
    euxmSetIVar
    (
        euxlc_object,
        euxmAbstractpId,
        euxs_t
    );

    euxmSetIVar
    (
        euxlc_class,
        euxmClassNameId,
        cls
    );
    euxmSetIVar
    (
        euxlc_class,
        euxmSuperClassId, euxcCons(euxlc_object, euxmNil)
    );
    euxmSetIVar
    (
        euxlc_class,
        euxmClassPrecedenceListId,
        euxcCons
        (
            euxlc_class,
            euxmGetIVar(euxlc_object, euxmClassPrecedenceListId)
        )
    );
    euxmSetIVar
    (
        euxlc_class,
        euxmSubClassesId,
        euxcCons(euxlc_simple_class, euxmNil)
    );
    euxmSetIVar
    (
        euxlc_class,
        euxmInstanceSizeId,
        euxcMakeFPI((euxmFPIType) euxmClassSize)
    );
    euxmSetIVar
    (
        euxlc_class,
        euxmAbstractpId,
        euxs_t
    );

    euxmSetIVar
    (
        euxlc_simple_class,
        euxmClassNameId, scls
    );
    euxmSetIVar
    (
        euxlc_simple_class,
        euxmSuperClassId,
        euxcCons(euxlc_class, euxmNil)
    );
    euxmSetIVar
    (
        euxlc_simple_class,
        euxmClassPrecedenceListId,
        euxcCons
        (
            euxlc_simple_class,
            euxmGetIVar(euxlc_class, euxmClassPrecedenceListId)
        )
    );
    euxmSetIVar
    (
        euxlc_simple_class,
        euxmSubClassesId,
        euxmNil
    );
    euxmSetIVar
    (
        euxlc_simple_class,
        euxmInstanceSizeId,
        euxcMakeFPI((euxmFPIType) euxmClassSize)
    );
    euxmSetIVar
    (
        euxlc_simple_class,
        euxmAbstractpId,
        euxmNil
    );

    euxlValue sds = euxcCons
    (
        euxcNewSlot(euxmInternAndExport("abstract?")),
        euxmNil
    );
    euxmSetSlotKey(euxmCar(sds), euxcEnterKeyword("abstract?:"));
    sds = euxcCons(euxcNewSlot(euxmInternAndExport("instance-size")), sds);
    sds = euxcCons(euxcNewSlot(euxmInternAndExport("subclasses")), sds);
    sds = euxcCons(euxcNewSlot(euxmInternAndExport("keywords")), sds);
    sds = euxcCons(euxcNewSlot(euxmInternAndExport("slots")), sds);
    sds = euxcCons
    (
        euxcNewSlot(euxmInternAndExport("class-precedence-list")),
        sds
    );
    sds = euxcCons(euxcNewSlot(euxmInternAndExport("superclasses")), sds);
    euxmSetSlotKey(euxmCar(sds), euxcEnterKeyword("superclasses:"));
    sds = euxcCons(euxcNewSlot(euxmInternAndExport("name")), sds);
    euxmSetSlotKey(euxmCar(sds), euxcEnterKeyword("name:"));
    euxmSetIVar(euxlc_class, euxmSlotsId, sds);
    euxmSetIVar(euxlc_simple_class, euxmSlotsId, sds);

    sds = euxcCons(euxcEnterKeyword("abstract?:"), euxmNil);
    sds = euxcCons(euxcEnterKeyword("direct-keywords:"), sds);
    sds = euxcCons(euxcEnterKeyword("direct-slots:"), sds);
    sds = euxcCons(euxcEnterKeyword("superclasses:"), sds);
    sds = euxcCons(euxcEnterKeyword("name:"), sds);
    euxmSetIVar(euxlc_class, euxmKeywordS, sds);
    euxmSetIVar(euxlc_simple_class, euxmKeywordS, sds);

    initBuiltinClasses();
}

///  applicablep - is this method applicable to this domain?
static int applicablep(euxlValue md, euxlValue domain)
{
    euxlValue md_domain = euxmGetMethodDomain(md);

    for (; md_domain; md_domain = euxmCdr(md_domain), domain = euxmCdr(domain))
    {
        if (!euxcSubClassp(euxmCar(domain), euxmCar(md_domain)))
        {
            return euxmFalse;
        }
    }

    return euxmTrue;
}

///  specific - order by domains as more or less specific
static int specific(euxlValue * md1, euxlValue * md2)
{
    euxlValue dom1 = euxmGetMethodDomain(*md1);
    euxlValue dom2 = euxmGetMethodDomain(*md2);

    for (; dom1; dom1 = euxmCdr(dom1), dom2 = euxmCdr(dom2))
    {
        if (euxmCar(dom1) == euxmCar(dom2))
        {
            continue;
        }
        else if (euxcSubClassp(euxmCar(dom1), euxmCar(dom2)))
        {
            return -1;
        }
        else if (euxcSubClassp(euxmCar(dom2), euxmCar(dom1)))
        {
            return 1;
        }
    }

    return 0;
}

///  applicableMethods - find and sort applicable methods
static euxlValue applicableMethods(euxlValue gf, euxlValue classes)
{
    euxlValue methods = euxmGetGenericMethods(gf);
    if (methods == euxmNil)
    {
        return euxmNil;
    }

    int app = 0;
    euxlValue buf[128];

    for (app = 0; methods; methods = euxmCdr(methods))
    {
        if (applicablep(euxmCar(methods), classes))
        {
            buf[app] = euxmCar(methods);
            app++;
        }
    }

    if (app == 0)
    {
        return euxmNil;
    }
    if (app == 1)
    {
        return euxcCons(buf[0], euxmNil);
    }

    #if 0
    {
        euxcPutString(euxlStdout(), "<applic");
        for (int i = 0; i < app; i++)
        {
            euxcPutString(euxlStdout(), " ");
            euxcPrin1(buf[i], euxlStdout());
        }
        euxcPutString(euxlStdout(), ">\n");
    }
    qsort((char *)buf, app, sizeof(euxlValue), specific);
    {
        euxcPutString(euxlStdout(), "<sorted");
        for (int i = 0; i < app; i++)
        {
            euxcPutString(euxlStdout(), " ");
            euxcPrin1(buf[i], euxlStdout());
        }
        euxcPutString(euxlStdout(), ">\n");
    }
    #else
    qsort
    (
        buf,
        app,
        sizeof(euxlValue),
        (int (*)(const void *, const void *))specific
    );
    #endif

    euxlValue applicable = euxmNil;
    for (int i = app - 1; i >= 0; i--)
    {
        applicable = euxcCons(buf[i], applicable);
        }

    return applicable;
}

///  classList - get classes of required args
static euxlValue classList(euxlValue l, euxlValue gf)
{
    euxlValue reqd = euxmGetGenericArgs(gf);

    if (l == euxmNil)
    {
        if (reqd != euxmNil)
        {
            euxcCerror("insufficient args for generic function", gf, euxmNil);
        }

        return euxmNil;
    }

    euxlValue start = euxcCons(euxcClassOf(euxmCar(l)), euxmNil);
    reqd = euxmCdr(reqd);

    euxmStackCheckPush(start);
    euxlValue end;
    for
    (
        end = start, l = euxmCdr(l);
        l && reqd;
        l = euxmCdr(l), end = euxmCdr(end)
    )
    {
        euxmSetCdr(end, euxcCons(euxcClassOf(euxmCar(l)), euxmNil));
        reqd = euxmCdr(reqd);
    }

    euxmStackDrop(1);

    if (reqd != euxmNil)
    {
        euxcCerror("insufficient args for generic function", gf, euxmNil);
    }

    if (l != euxmNil && euxmGetGenericOpt(gf) == euxmNil)
    {
        euxcCerror("too many args for generic function", gf, euxmNil);
    }

    return start;

}

///  euxcFindAndCacheMethods - find, sort and cache applicable methods
euxlValue euxcFindAndCacheMethods(euxlValue gf, euxlValue arglist)
{
    euxlValue classes = classList(arglist, gf);

    euxlValue cache1 = euxmGetGenericCache1(gf);
    if (cache1 != euxmNil && euxcEqual(classes, euxmCar(cache1)))
    {
        // fprintf(stderr, "<cache1 hit>");
        return euxmCdr(cache1);
    }

    euxlValue cache2 = euxmGetGenericCache2(gf);
    if (cache2 != euxmNil)
    {
        for (; cache2; cache2 = euxmCdr(cache2))
        {
            if (euxcEqual(classes, euxmCar(euxmCar(cache2))))
            {
                // fprintf(stderr, "<cache2 hit>");
                euxmSetGenericCache1(gf, euxmCar(cache2));
                return euxmCdr(euxmCar(cache2));
            }
        }
    }

    euxmStackCheck(3);
    euxmStackPush(classes);
    euxmStackPush(gf);

    euxlValue applicable = applicableMethods(gf, classes);
    if (applicable == euxmNil)
    {
        euxmStackDrop(2);
        return euxmNil;
    }
    euxmStackPush(applicable);

    euxlValue methodfns;
    for (methodfns = applicable; applicable; applicable = euxmCdr(applicable))
    {
        euxmSetCar(applicable, euxmGetMethodFun(euxmCar(applicable)));
    }

    cache1 = euxcCons(classes, methodfns);
    euxmSetGenericCache1(gf, cache1);
    cache2 = euxcCons(cache1, euxmGetGenericCache2(gf));
    euxmSetGenericCache2(gf, cache2);

    euxmStackDrop(3);
    return methodfns;
}

///  addOrReplaceMethod - replace old method of same domain, if there
static euxlValue addOrReplaceMethod(euxlValue md, euxlValue mdlist)
{
    euxlValue domain = euxmGetMethodDomain(md);
    euxlValue list;
    for (list = mdlist; list; list = euxmCdr(list))
    {
        if (euxcEqual(domain, euxmGetMethodDomain(euxmCar(list))))
        {
            euxmSetCar(list, md);
            break;
        }
    }

    if (list == euxmNil)    // it wasn't there, so add
    {
        mdlist = euxcCons(md, mdlist);
    }

    return mdlist;
}

///  checkDomainCompatibility
static void checkDomainCompatibility
(
    euxlValue domain,
    euxlValue gf,
    euxlValue optargs
)
{
    euxlValue gfdomain = euxmGetGenericArgs(gf);

    for
    (;
     domain && gfdomain;
     domain = euxmCdr(domain), gfdomain = euxmCdr(gfdomain)
    )
    {
        if (!euxmClassp(euxmCar(domain)))
        {
            euxcBadType(euxmCar(domain), expect_class, "defmethod");
        }

        if (!euxcSubClassp(euxmCar(domain), euxmCar(gfdomain)))
        {
            euxcCerror
            (
                "trying to extend generic domain in defmethod",
                gf,
                euxls_incompatible_md
            );
        }
    }

    if (domain || gfdomain)
    {
        euxcCerror
        (
            "wrong number of required args in defmethod",
            gf,
            euxls_incompatible_md
        );
    }

    if (optargs != euxmGetGenericOpt(gf))
    {
        euxcCerror
        (
            "method and generic don't agree on rest args",
            gf,
            euxls_incompatible_md
        );
    }
}

///  euxlMakeAndAddMethod - (make-and-add-method gf closure domain optargs?)
euxlValue euxlMakeAndAddMethod()
{
    static char *functionName = "make-and-add-method";

    euxlValue gf = euxmGetArgGeneric();
    euxlValue mdfn = euxmGetArgClosure();
    euxlValue domain = euxmGetArgList();
    euxlValue optargs = euxmGetArg();
    euxmLastArg();

    euxmStackCheck(4);
    euxmStackPush(gf);
    euxmStackPush(mdfn);
    euxmStackPush(domain);
    euxmStackPush(optargs);

    checkDomainCompatibility(domain, gf, optargs);

    euxlValue md = euxcNewMethod();
    euxmSetMethodGenericFun(md, gf);
    euxmSetMethodFun(md, mdfn);
    euxmSetMethodDomain(md, domain);
    euxmSetMethodOpt(md, optargs);
    euxlValue meths = addOrReplaceMethod(md, euxmGetGenericMethods(gf));
    euxmSetGenericMethods(gf, meths);
    euxmSetGenericCache1(gf, euxmNil);
    euxmSetGenericCache2(gf, euxmNil);

    euxmStackDrop(4);
    return gf;

}

///  euxlMakeMethod - (make-method closure domain optargs?)
euxlValue euxlMakeMethod()
{
    static char *functionName = "make-method";

    euxlValue mdfn = euxmGetArgClosure();
    euxlValue domain = euxmGetArgList();
    euxlValue optargs = euxmGetArg();
    euxmLastArg();

    euxmStackCheck(3);
    euxmStackPush(mdfn);
    euxmStackPush(domain);
    euxmStackPush(optargs);

    euxlValue md = euxcNewMethod();

    euxmSetMethodGenericFun(md, euxmNil);
    euxmSetMethodFun(md, mdfn);
    euxmSetMethodDomain(md, domain);
    euxmSetMethodOpt(md, optargs);

    euxmStackDrop(3);

    return md;
}

///  euxlAddMethod - (add-method gf md)
euxlValue euxlAddMethod()
{
    static char *functionName = "add-method";

    euxlValue gf = euxmGetArgGeneric();
    euxlValue md = euxmGetArgMethod();
    euxmLastArg();

    euxmStackCheck(2);
    euxmStackPush(gf);
    euxmStackPush(md);

    checkDomainCompatibility(euxmGetMethodDomain(md), gf, euxmGetMethodOpt(md));
    euxlValue meths = addOrReplaceMethod(md, euxmGetGenericMethods(gf));
    euxmSetGenericMethods(gf, meths);
    euxmSetGenericCache1(gf, euxmNil);
    euxmSetGenericCache2(gf, euxmNil);
    euxmSetMethodGenericFun(md, gf);

    euxmStackDrop(2);

    return gf;
}

///  euxlMakeGeneric - (make-generic name domain optargs?)
euxlValue euxlMakeGeneric()
{
    static char *functionName = "make-generic";

    euxlValue name = euxmGetArgSymbol();
    euxlValue domain = euxmGetArgList();
    euxlValue optargs = euxmGetArg();
    euxmLastArg();

    euxmStackCheck(3);
    euxmStackPush(name);
    euxmStackPush(domain);
    euxmStackPush(optargs);

    euxlValue gf = euxcNewGeneric();
    euxmSetGenericName(gf, name);
    euxmSetGenericArgs(gf, domain);
    euxmSetGenericOpt(gf, optargs);
    euxmSetGenericMethods(gf, euxmNil);
    euxmSetGenericCache1(gf, euxmNil);
    euxmSetGenericCache2(gf, euxmNil);

    euxmStackDrop(3);

    return gf;
}

#ifndef NO_CHECK_REF
///  euxcTelosBadRefError
void euxcTelosBadRefError(euxlValue object, euxlValue wanted, int interp)
{
    euxlValue cond = euxmGetValue(euxls_teloeuxls_bad_ref);
    if (cond != euxls_unbound)
    {
        euxmSetIVar(cond, 3, wanted);
    }

    if (interp)
    {
        euxcIntError
        (
            "wrong type for slot accessor",
            euxlc_object,
            euxls_teloeuxls_bad_ref
        );
    }
    else
    {
        euxcCerror
        (
            "wrong type for slot accessor",
            euxlc_object,
            euxls_teloeuxls_bad_ref
        );
    }
}

///  euxlCheckRef - (check-ref class obj)
euxlValue euxlCheckRef()
{
    static char *functionName = "check-ref";

    euxlValue class = euxmGetArgObject();
    euxlValue object = euxmGetArg();
    euxmLastArg();

    if (!euxmClassp(euxlc_class))
    {
        euxcCerror("not a class in check-ref", class, euxls_telos_error);
    }

    if (euxcSubClassp(euxcClassOf(object), class))
    {
        return euxs_t;
    }

    euxcTelosBadRefError(object, euxlc_class, euxmFalse);

    return euxmNil; // not reached
}

#endif


///-----------------------------------------------------------------------------
