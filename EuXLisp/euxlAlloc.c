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
/// Title: Dynamic memory allocation and garbage collection
///  Maintainer: Henry G. Weller
///-----------------------------------------------------------------------------
#include "euxlisp.h"

///-----------------------------------------------------------------------------
/// Global variables
///-----------------------------------------------------------------------------

///  Virtual machine registers
euxlValue xlfun = euxmNil;               // current function
euxlValue xlenv = euxmNil;               // current environment
euxlValue xlval = euxmNil;               // value of most recent instruction
euxlValue *euxcStackPtr = NULL;              // value stack pointer

///  Stack limits
euxlValue *euxcStackBase = NULL;         // base of value stack
euxlValue *euxcStackTop = NULL;          // top of value stack (actually, one beyond)

///  Variables shared with euxlImage.c
euxmFPIType total = 0;              // total number of bytes of memory in use
euxmFPIType gccalls = 0;            // number of calls to the garbage collector

///  Node space
euxcNodeSegment *nsegments = NULL;     // list of node segments
euxcNodeSegment *nslast = NULL;        // last node segment
int nscount = 0;                // number of node segments
euxmFPIType nnodes = 0;             // total number of nodes
euxmFPIType nfree = 0;              // number of nodes in free list
euxlValue fnodes = euxmNil;              // list of free nodes

///  Vector (and string) space
euxcVectorSegment *vsegments = NULL;     // list of vector segments
euxcVectorSegment *vscurrent = NULL;     // current vector segment
int vscount = 0;                // number of vector segments
euxlValue *vfree = NULL;             // next free location in vector space
euxlValue *veuxmStackTop = NULL;              // euxmStackTop of vector space

///-----------------------------------------------------------------------------
/// Forward declarations
///-----------------------------------------------------------------------------
static euxlValue allocNode(int type);
static void findMemory();
static euxlValue allocVector(int type, int size);
static int findVMemory(int size);
static void mark(euxlValue ptr);
static void markVector(euxlValue vect);
static void compact();
static void compactVector(euxcVectorSegment * vseg);
static void sweep();
static void sweepSegment(euxcNodeSegment * nseg);
static void badObjectType(int type);

///-----------------------------------------------------------------------------
/// Functions
///-----------------------------------------------------------------------------
///  euxcCons - construct a new euxcCons node
euxlValue euxcCons(euxlValue x, euxlValue y)
{
    euxlValue nnode;

    // get a free node
    if ((nnode = fnodes) == euxmNil)
    {
        euxmStackCheck(2);
        euxmStackPush(x);
        euxmStackPush(y);
        findMemory();
        if ((nnode = fnodes) == euxmNil)
            euxcAbort("insufficient node space");
        euxmStackDrop(2);
    }

    // unlink the node from the free list
    fnodes = euxmCdr(nnode);
    --nfree;

    // initialize the new node
    nnode->type = euxmCons;
    euxmSetCar(nnode, x);
    euxmSetCdr(nnode, y);

    // return the new node
    return (nnode);
}

///  euxcNewFrame - create a new environment frame
euxlValue euxcNewFrame(euxlValue parent, int size)
{
    euxlValue frame = euxcCons(euxcNewVector(size), parent);
    frame->type = euxmEnv;
    return (frame);
}

///  euxcMakeString - convert a string to a string node
euxlValue euxcMakeString(const char *str)
{
    euxlValue val = euxcNewString(strlen(str) + 1);
    strcpy(euxmGetString(val), str);
    return (val);
}

///  euxcMakeString2 - convert a string (possibly containing NULLs) to a string node
euxlValue euxcMakeString2(const char *str, int len)
{
    euxlValue val = euxcNewString(len + 1);

    for (int i = 0; i < len; i++)
    {
        euxmGetString(val)[i] = str[i];
    }

    euxmGetString(val)[len] = 0;

    return (val);
}

///  ensure unique names for symbols
static void set_symbol_name(euxlValue new, const char *pname)
{
    int i = euxcHash(pname, euxmSymbolTableSize);

    euxlValue sym, name;
    for (sym = euxmGetElement(euxcObArray, i); sym; sym = euxmCdr(sym))
    {
        if (strcmp(pname, euxmGetString(euxmGetPName(euxmCar(sym)))) == 0)
        {
            euxmSetPName(new, euxmGetPName(euxmCar(sym)));
            euxmSetThePList(new, euxmGetThePList(euxmCar(sym)));
            return;
        }
    }

    // new name
    name = euxcMakeString(pname);
    euxmSetPName(new, name);
    euxmSetThePList(new, euxcCons(euxmNil, euxmNil));

    sym = euxcCons(new, euxmGetElement(euxcObArray, i));
    euxmSetElement(euxcObArray, i, sym);
}

///  euxcMakeSymbol - convert a string to a symbol
euxlValue euxcMakeSymbol(const char *pname)
{
    euxlValue val = allocVector(euxmSymbol, euxmSymbolSize);
    euxmStackCheckPush(val);
    euxmSetValue(val, euxls_unbound);
    set_symbol_name(val, pname);
    euxmPutSyntax(val, euxmNil);
    euxmSetModule(val, euxcCurrentModule);
    return (euxmStackPop());
}

///  euxcMakeModule - convert a string to a module
euxlValue euxcMakeModule(const char *name)
{
    // delete old module of same name if there
    if (euxcModuleList != euxmNil)
    {
        if (strcmp(name, euxmGetString(euxmGetModuleName(euxmCar(euxcModuleList)))) == 0)
        {
            euxcModuleList = euxmCdr(euxcModuleList);
        }
        else
        {
            euxlValue mods1, mods2;
            for
            (
                mods1 = euxcModuleList, mods2 = euxmCdr(euxcModuleList);
                mods2;
                mods1 = euxmCdr(mods1), mods2 = euxmCdr(mods2)
            )
            {
                if (strcmp(name, euxmGetString(euxmGetModuleName(euxmCar(mods2)))) == 0)
                {
                    euxmSetCdr(mods1, euxmCdr(mods2));
                    break;
                }
            }
        }
    }

    // make new module
    euxlValue val = allocVector(euxmModule, euxmModuleSize);
    euxmStackCheckPush(val);
    euxmSetModuleName(val, euxcMakeString(name));      // module name
    euxlValue euxcObArray = euxcNewVector(euxmSymbolTableSize);
    euxmSetModuleSymbols(val, euxcObArray);  // module euxcObArray
    // next line to ensure that oblists of different modules differ, and are
    // not compiled into euxcEqual literals
    euxmSetElement(euxcObArray, 0, euxcCons(euxcEnterKeyword(name), euxmNil));
    euxmSetModuleExports(val, euxmNil);      // module exports
    return (euxmStackPop());
}

///  euxcMakeFPI - convert an integer to a FPI node
euxlValue euxcMakeFPI(euxmFPIType n)
{
    if (n >= euxmFPIMin && n <= euxmFPIMax)
    {
        return (euxmMakeSmallFPI(n));
    }
    euxlValue val = allocNode(euxmFPI);
    val->value.fpi = n;
    return (val);
}

///  euxcMakeDoubleFloat - convert a floating point number to a float node
euxlValue euxcMakeDoubleFloat(euxmDoubleFloatType n)
{
    euxlValue val = allocNode(euxmDoubleFloat);
    val->value.euxcDoubleFloat = n;
    return (val);
}

///  euxcMakeChar - convert an integer to a character node
euxlValue euxcMakeChar(int ch)
{
    euxlValue val = allocNode(euxmChar);
    val->value.charCode = ch;
    return (val);
}

///  euxcMakeClosure - convert code and an environment to a closure
euxlValue euxcMakeClosure(euxlValue code, euxlValue env)
{
    euxlValue val = euxcCons(code, env);
    val->type = euxmClosure;
    return (val);
}

///  euxcMakePromise - convert a procedure to a promise
euxlValue euxcMakePromise(euxlValue code, euxlValue env)
{
    euxlValue val = euxcCons(euxcMakeClosure(code, env), euxmNil);
    val->type = euxmPromise;
    return (val);
}

///  euxcMakeXFun - convert a function to an xfun
euxlValue euxcMakeXFun(int type, euxcXFunType fcn, int offset)
{
    euxlValue val = allocNode(type);
    val->value.fun.xfun = fcn;
    val->value.fun.offset = offset;
    return (val);
}

///  euxcMakeFun - convert a function to a fun
euxlValue euxcMakeFun(int type, euxcFunType fcn, int offset)
{
    euxlValue val = allocNode(type);
    val->value.fun.fun = fcn;
    val->value.fun.offset = offset;
    return (val);
}

///  euxcMakeStream - convert a file pointer to an stream
euxlValue euxcMakeStream(FILE *fp, int flags)
{
    euxlValue val = allocNode(euxmStream);
    euxmSetFile(val, fp);
    euxmSetSaveChar(val, '\0');
    euxmSetPFlags(val, flags);
    return (val);
}

///  euxcMakeTable - convert a comparator function to a table
euxlValue euxcMakeTable(euxlValue comp, euxlValue fill)
{
    euxmStackCheck(3);
    euxmStackPush(comp);
    euxmStackPush(fill);
    euxlValue val = allocVector(euxmTable, euxmTableSIZE);
    euxmStackPush(val);
    euxmSetTablecomp(val, comp);
    euxmSetTableTable(val, euxcNewVector(euxmHashTableSize));
    euxmSetTableFill(val, fill);
    euxmStackDrop(3);
    return val;
}

///  euxcNewVector - allocate and initialize a new vector
euxlValue euxcNewVector(int size)
{
    return (allocVector(euxmVector, size));
}

///  euxcNewString - allocate and initialize a new string
euxlValue euxcNewString(int size)
{
    euxlValue val = allocVector(euxmString, euxmByteToWordSize(size));
    val->value.vector.size = size;
    return (val);
}

///  euxcNewCode - create a new code object
euxlValue euxcNewCode(int nlits)
{
    return (allocVector(euxmCode, nlits));
}

///  euxcNewContinuation - create a new continuation object
euxlValue euxcNewContinuation(int size)
{
    return (allocVector(euxmContinuation, size));
}

///  euxcNewObject - allocate and initialize a new object
euxlValue euxcNewObject(euxlValue cls, int size)
{
    euxlValue val = allocVector(euxmObject, size + 1);        // class, ivars
    euxmSetClass(val, cls);
    for (int i = 1; i <= size; i++)
    {
        euxmSetIVar(val, i, euxls_unbound);
    }

    return (val);
}

euxlValue euxcNewGeneric()
{
    return allocVector(euxmGeneric, euxmGenericSize);
}

euxlValue euxcNewMethod()
{
    return allocVector(euxmMethod, euxmMethodSize);
}

euxlValue euxcNewSlot(euxlValue name)
{
    euxlValue val = allocVector(euxmSlot, euxmSlotSIZE);
    euxmSetSlotName(val, name);
    euxmSetSlotKey(val, euxls_unbound);
    euxmSetSlotDefault(val, euxls_unbound);
    euxmSetSlotRequiredp(val, euxmNil);
    return val;
}

///  allocNode - allocate a new node
static euxlValue allocNode(int type)
{
    euxlValue nnode;

    // get a free node
    if ((nnode = fnodes) == euxmNil)
    {
        findMemory();
        if ((nnode = fnodes) == euxmNil)
            euxcAbort("insufficient node space");
    }

    // unlink the node from the free list
    fnodes = euxmCdr(nnode);
    --nfree;

    // initialize the new node
    nnode->type = type;
    euxmSetCdr(nnode, euxmNil);

    // return the new node
    return (nnode);
}

///  findMemory - garbage collect, then add more node space if necessary
static void findMemory()
{
    // first try garbage collecting
    gc(euxmGcNode);

    // expand memory only if less than one segment is free
    if (nfree < (long)euxmNsSize)
    {
        euxcNExpand(euxmNsSize);
        //nexpand(euxmNsSize);        // rjb
    }
}

///  euxcNExpand - expand node space
int euxcNExpand(int size)
{
    euxcNodeSegment *euxcNewnsegment(), *newseg;

    // allocate the new segment
    if ((newseg = euxcNewnsegment(size)) != NULL)
    {
        // add each new node to the free list
        euxlValue p = &newseg->data[0];
        for (int i = euxmNsSize; --i >= 0; ++p)
        {
            p->type = euxmFree;
            p->flags = 0;
            euxmSetCdr(p, fnodes);
            fnodes = p;
        }
    }
    return (newseg != NULL);
}

///  allocVector - allocate and initialize a new vector node
static euxlValue allocVector(int type, int size)
{
    register euxlValue val;

    // get a free node
    if ((val = fnodes) == euxmNil)
    {
        findMemory();
        if ((val = fnodes) == euxmNil)
        {
            euxcAbort("insufficient node space");
        }
    }

    // unlink the node from the free list
    fnodes = euxmCdr(fnodes);
    --nfree;

    // initialize the vector node
    val->type = type;
    val->value.vector.size = size;
    val->value.vector.data = NULL;
    euxmStackCheckPush(val);

    // add space for the backpointer
    ++size;

    // make sure there's enough space
    if
    (
        !euxmVcompare(vfree, size, veuxmStackTop)
     && !euxcCheckVmemory(size)
     && !findVMemory(size)
    )
    {
        euxcAbort("insufficient vector space");
    }

    // allocate the next available block
    register euxlValue *p = vfree;
    vfree += size;

    // store the backpointer
    *p++ = euxmStackTop();
    val->value.vector.data = p;

    // set all the elements to euxmNil
    for (register int i = size; i > 1; --i)
    {
        *p++ = euxmNil;
    }

    // return the new vector
    return (euxmStackPop());
}

///  findVMemory - find vector memory
static int findVMemory(int size)
{
    // try garbage collecting
    gc(euxmGcVector);

    // Check to see if we found enough memory
    if (euxmVcompare(vfree, size, veuxmStackTop) || euxcCheckVmemory(size))
    {
        return (euxmTrue);
    }

    // expand vector space
    return (euxcMakeVmemory(size));
}

///  euxcCheckVmemory - Check for vector memory (used by 'euxlImage.c')
int euxcCheckVmemory(int size)
{
    for (euxcVectorSegment *vseg = vsegments; vseg != NULL; vseg = vseg->next)
    {
        if (vseg != vscurrent && euxmVcompare(vseg->free, size, vseg->top))
        {
            if (vscurrent != NULL)
            {
                vscurrent->free = vfree;
            }
            vfree = vseg->free;
            veuxmStackTop = vseg->top;
            vscurrent = vseg;
            return (euxmTrue);
        }
    }

    return (euxmFalse);
}

///  euxcMakeVmemory - make vector memory (used by 'euxlImage.c')
int euxcMakeVmemory(int size)
{
    return (euxcVexpand(size < euxmVsSize ? euxmVsSize : size));
}

///  euxcVexpand - expand vector space
int euxcVexpand(int size)
{
    euxcVectorSegment *euxcNewvsegment(), *vseg;

    // allocate the new segment
    if ((vseg = euxcNewvsegment(size)) != NULL)
    {

        // initialize the new segment and make it current
        if (vscurrent != NULL)
        {
            vscurrent->free = vfree;
        }
        vfree = vseg->free;
        veuxmStackTop = vseg->top;
        vscurrent = vseg;
    }
    return (vseg != NULL);
}

///  euxcNewnsegment - create a new node segment
euxcNodeSegment *euxcNewnsegment(unsigned int n)
{
    euxcNodeSegment *newseg;

    // allocate the new segment
    if ((newseg = (euxcNodeSegment *) calloc(1, euxmNSegSize(n))) == NULL)
    {
        return (NULL);
    }

    // initialize the new segment
    newseg->size = n;
    newseg->next = NULL;
    if (nsegments)
    {
        nslast->next = newseg;
    }
    else
    {
        nsegments = newseg;
    }
    nslast = newseg;

    // update the statistics
    total += (long)euxmNSegSize(n);
    nnodes += (long)n;
    nfree += (long)n;
    ++nscount;

    // return the new segment
    return (newseg);
}

///  euxcNewvsegment - create a new vector segment
euxcVectorSegment *euxcNewvsegment(unsigned int n)
{
    euxcVectorSegment *newseg;

    // allocate the new segment
    if ((newseg = (euxcVectorSegment *) calloc(1, euxmVSegSize(n))) == NULL)
    {
        return (NULL);
    }

    // initialize the new segment
    newseg->free = &newseg->data[0];
    newseg->top = newseg->free + n;
    newseg->next = vsegments;
    vsegments = newseg;

    // update the statistics
    total += (long)euxmVSegSize(n);
    ++vscount;

    // return the new segment
    return (newseg);
}

void euxcPstack()
{
    if (euxls_stdout && euxmGetValue(euxls_stdout))
    {
        euxcTerpri(euxlStdout());
        for (euxlValue *p = euxcStackPtr; p < euxcStackTop; ++p)
        {
            euxlValue tmp = *p;
            euxcPrin1(tmp, euxlStdout());
            euxcTerpri(euxlStdout());
        }
    }
}

///  gc - garbage collect
void gc(int reason)
{
    if (!quiet && euxls_gcmsgs && euxmGetValue(euxls_gcmsgs))
    {
        fprintf
        (
            stderr, "<%cG", reason == euxmGcNode ? 'c' :
            reason == euxmGcVector ? 'v' :
            reason == euxmGcUser ? 'u' :
            reason == euxmGcSave ? 's' : '?'
        );
    }

    // mark the current environment
    if (xlfun && euxmIsPointer(xlfun))
    {
        mark(xlfun);
    }
    if (xlenv && euxmIsPointer(xlenv))
    {
        mark(xlenv);
    }
    if (xlval && euxmIsPointer(xlval))
    {
        mark(xlval);
    }
    if (euxl_default_object && euxmIsPointer(euxl_default_object))
    {
        mark(euxl_default_object);
    }
    if (euxl_eof_object && euxmIsPointer(euxl_eof_object))
    {
        mark(euxl_eof_object);
    }
    if (euxl_true && euxmIsPointer(euxl_true))
    {
        mark(euxl_true);
    }
    if (euxcModuleList && euxmIsPointer(euxcModuleList))
    {
        mark(euxcModuleList);
    }
    if (euxlc_vector && euxmIsPointer(euxlc_vector))
    {
        mark(euxlc_vector);
    }
    if (euxcKeywordArray && euxmIsPointer(euxcKeywordArray))
    {
        mark(euxcKeywordArray);
    }
    if (euxcObArray && euxmIsPointer(euxcObArray))
    {
        mark(euxcObArray);
    }

    // mark the stack
    for (register euxlValue *p = euxcStackPtr; p < euxcStackTop; ++p)
    {
        register euxlValue tmp;
        if ((tmp = *p) != euxmNil && euxmIsPointer(tmp))
        {
            mark(tmp);
        }
    }

    // compact vector space
    euxcGcProtect(compact);

    // sweep memory collecting all unmarked nodes
    sweep();

    // count the gc call
    ++gccalls;

    if (!quiet && euxls_gcmsgs && euxmGetValue(euxls_gcmsgs))
        fprintf(stderr, "C>");
    #if 0
    euxcPstack();
    #endif

}

///  mark - mark all accessible nodes
static void mark(euxlValue ptr)
{
    if (!euxmIsPointer(ptr))
    {
        return;
    }

    // initialize
    register euxlValue prev = euxmNil;
    register euxlValue this = ptr;

    // mark this node
    for (;;)
    {
        // descend as far as we can
        while (!(this->flags & euxmMark))
        {
            // mark this node and trace its children
            switch (this->type)
            {
                case euxmCons:     // mark cons-like nodes
                case euxmClosure:
                case euxmPromise:
                case euxmEnv:
                    {
                        this->flags |= euxmMark;
                        register euxlValue tmp;
                        if ((tmp = euxmCar(this)) != euxmNil && euxmIsPointer(tmp))
                        {
                            this->flags |= euxmLeft;
                            euxmSetCar(this, prev);
                            prev = this;
                            this = tmp;
                        }
                        else if ((tmp = euxmCdr(this)) != euxmNil && euxmIsPointer(tmp))
                        {
                            euxmSetCdr(this, prev);
                            prev = this;
                            this = tmp;
                        }
                    }
                    break;
                case euxmSymbol:   // mark vector-like nodes
                case euxmObject:
                case euxmVector:
                case euxmCode:
                case euxmContinuation:
                case euxmModule:
                case euxmGeneric:
                case euxmMethod:
                case euxmSlot:
                case euxmTable:
                    this->flags |= euxmMark;
                    markVector(this);
                    break;
                case euxmFPI:   // mark objects that don't contain pointers
                case euxmDoubleFloat:
                case euxmString:
                case euxmStream:
                case euxmFun:
                case euxmXFun:
                case euxmXFunCont:
                case euxmChar:
                    this->flags |= euxmMark;
                    break;
                default:       // bad object type
                    badObjectType(this->type);
                    break;
            }
        }

        // backup to a point where we can continue descending
        for (;;)
        {
            // make sure there is a previous node
            if (prev)
            {
                register euxlValue tmp;
                if (prev->flags & euxmLeft)
                {
                    // came from left side
                    prev->flags &= ~euxmLeft;
                    tmp = euxmCar(prev);
                    euxmSetCar(prev, this);
                    if ((this = euxmCdr(prev)) != euxmNil && euxmIsPointer(this))
                    {
                        euxmSetCdr(prev, tmp);
                        break;
                    }
                }
                else
                {
                    // came from right side
                    tmp = euxmCdr(prev);
                    euxmSetCdr(prev, this);
                }
                this = prev;    // step back up the branch
                prev = tmp;
            }
            // no previous node, must be done
            else
            {
                return;
            }
        }
    }
}

///  markVector - mark a vector-like node
static void markVector(euxlValue vect)
{
    register euxlValue *p;
    if ((p = vect->value.vector.data) != NULL)
    {
        register int n = euxmGetSize(vect);
        while (--n >= 0)
        {
            register euxlValue tmp;
            if ((tmp = *p++) != euxmNil && euxmIsPointer(tmp))
            {
                mark(tmp);
            }
        }
    }
}

///  compact - compact vector space
static void compact()
{
    euxcVectorSegment *vseg;

    // store the current segment information
    if (vscurrent)
    {
        vscurrent->free = vfree;
    }

    // compact each vector segment
    for (vseg = vsegments; vseg != NULL; vseg = vseg->next)
    {
        compactVector(vseg);
    }

    // make the first vector segment current
    if ((vscurrent = vsegments) != NULL)
    {
        vfree = vscurrent->free;
        veuxmStackTop = vscurrent->top;
    }
}

///  compactVector - compact a vector segment
static void compactVector(euxcVectorSegment * vseg)
{
    register euxlValue *vdata = &vseg->data[0];
    register euxlValue *vnext = vdata;
    register euxlValue *vfree = vseg->free;

    while (vdata < vfree)
    {
        register euxlValue vector = *vdata;
        register int vsize =
        (
            vector->type == euxmString
            ? euxmByteToWordSize(vector->value.vector.size)
            : vector->value.vector.size
        ) + 1;

        if (vector->flags & euxmMark)
        {
            if (vdata == vnext)
            {
                vdata += vsize;
                vnext += vsize;
            }
            else
            {
                vector->value.vector.data = vnext + 1;
                while (--vsize >= 0)
                {
                    *vnext++ = *vdata++;
                }
            }
        }
        else
        {
            vdata += vsize;
        }
    }
    vseg->free = vnext;
}

///  sweep - sweep all unmarked nodes and add them to the free list
static void sweep()
{
    // empty the free list
    fnodes = euxmNil;
    nfree = 0L;

    // sweep each node segment
    for (euxcNodeSegment *nseg = nsegments; nseg != NULL; nseg = nseg->next)
    {
        sweepSegment(nseg);
    }
}

///  sweepSegment - sweep a node segment
static void sweepSegment(euxcNodeSegment * nseg)
{
    register euxmFPIType n;
    register euxlValue p;

    // add all unmarked nodes
    for (p = &nseg->data[0], n = nseg->size; --n >= 0L; ++p)
    {
        if (!(p->flags & euxmMark))
        {
            switch (p->type)
            {
                case euxmStream:
                    if (euxmGetFile(p))
                        euxcOSClose(euxmGetFile(p));
                    break;
            }
            p->type = euxmFree;
            euxmSetCdr(p, fnodes);
            fnodes = p;
            ++nfree;
        }
        else
        {
            p->flags &= ~euxmMark;
        }
    }
}

///  euxcMinit - initialize the dynamic memory module
void euxcMinit(unsigned int ssize)
{
    // initialize our internal variables
    gccalls = 0;
    total = 0L;

    // initialize node space
    nsegments = nslast = NULL;
    nscount = 0;
    nnodes = nfree = 0L;
    fnodes = euxmNil;

    // initialize vector space
    vsegments = vscurrent = NULL;
    vscount = 0;
    vfree = veuxmStackTop = NULL;

    // allocate the value stack
    unsigned int n = ssize * sizeof(euxlValue);
    if ((euxcStackBase = (euxlValue *) calloc(1, n)) == NULL)
    {
        euxcFatal("insufficient memory");
    }
    total += (long)n;

    // initialize objects that are marked by the collector
    euxl_default_object = euxl_eof_object = euxl_true = euxmNil;
    xlfun = xlenv = xlval = euxmNil;

    // initialize the stack
    euxcStackPtr = euxcStackTop = euxcStackBase + ssize;
}

///  badObjectType - report a bad object type error
static void badObjectType(int type)
{
    char buf[100];
    sprintf(buf, "bad object type - %d", type);
    euxcFatal(buf);
}


///-----------------------------------------------------------------------------
