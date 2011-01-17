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
/// Title: memory image save/restore functions
///  Maintainer: Henry G. Weller
///-----------------------------------------------------------------------------
#include "euxlisp.h"

///-----------------------------------------------------------------------------
/// Macros
///-----------------------------------------------------------------------------
#define _path_open_path_sep ":"
#define _path_open_string_format "%s/%s"

///-----------------------------------------------------------------------------
/// Local variables
///-----------------------------------------------------------------------------
static euxmOffType off, foff;
static FILE *fp;
static const char *image_search_path[] = { IMAGE_SEARCH_PATH, 0 };

///-----------------------------------------------------------------------------
/// Forward declarations
///-----------------------------------------------------------------------------
static void freeImage();
static void setOffset();
static void writeNode(euxlValue node);
static void writePtr(euxmOffType off);
static void readNode(int type, euxlValue node);
static euxmOffType readPtr();
static euxlValue convertOffsetToPtr(euxmOffType o);
static euxlValue *getVectorSpace(euxlValue node, unsigned int size);

///-----------------------------------------------------------------------------
/// Functions
///-----------------------------------------------------------------------------
///  euxlSaveImage - save the memory image
int euxlSaveImage(const char *fname)
{
    // open the output file
    if ((fp = euxcOSBOpen(fname, "w")) == NULL)
    {
        return (euxmFalse);
    }

    // first call the garbage collector to clean up memory
    gc(euxmGcSave);

    // write out version identification
    int size = strlen(euxlBanner);
    for (unsigned char *cp = (unsigned char *)euxlBanner; size-- >= 0;)
    {
        euxcOSBPutc(*cp++, fp);
    }

    // write out the stack size
    writePtr((euxmOffType) (euxcStackTop - euxcStackBase));

    // write out various constants
    writePtr(euxcMakePtr(euxs_eof));
    writePtr(euxcMakePtr(euxs_default));
    writePtr(euxcMakePtr(euxcRootModule));
    writePtr(euxcMakePtr(euxcReinternModule));
    writePtr(euxcMakePtr(euxcCurrentModule));
    writePtr(euxcMakePtr(euxcModuleList));
    writePtr(euxcMakePtr(euxlc_simple_class));
    writePtr(euxcMakePtr(euxlc_class));
    writePtr(euxcMakePtr(euxlc_object));
    writePtr(euxcMakePtr(euxlc_vector));
    writePtr(euxcMakePtr(euxcKeywordArray));
    writePtr(euxcMakePtr(euxcObArray));

    // setup the initial file offsets
    off = foff = (euxmOffType) 2;

    // write out all nodes that are still in use
    for (euxcNodeSegment *nseg = nsegments; nseg != NULL; nseg = nseg->next)
    {
        euxlValue p = &nseg->data[0];
        int n = nseg->size;
        for (; --n >= 0; ++p, off += sizeof(euxcNode))
        {
            switch (euxmNodeType(p))
            {
                case euxmFree:
                    break;
                case euxmCons:
                case euxmClosure:
                case euxmPromise:
                case euxmEnv:
                    setOffset();
                    euxcOSBPutc(p->type, fp);
                    writePtr(euxcMakePtr(euxmCar(p)));
                    writePtr(euxcMakePtr(euxmCdr(p)));
                    foff += sizeof(euxcNode);
                    break;
                case euxmSymbol:
                case euxmObject:
                case euxmVector:
                case euxmCode:
                case euxmContinuation:
                case euxmModule:
                case euxmGeneric:
                case euxmMethod:
                case euxmSlot:
                case euxmTable:
                    setOffset();
                    euxcOSBPutc(p->type, fp);
                    size = euxmGetSize(p);
                    writePtr((euxmOffType) size);
                    for (euxlValue *vp = p->value.vector.data; --size >= 0;)
                        writePtr(euxcMakePtr(*vp++));
                    foff += sizeof(euxcNode);
                    break;
                case euxmString:
                    setOffset();
                    euxcOSBPutc(p->type, fp);
                    size = euxmGetStringlength(p);
                    writePtr((euxmOffType) size);
                    for
                    (
                        unsigned char *cp = (unsigned char *)euxmGetString(p);
                        --size >= 0;
                    )
                    {
                        euxcOSBPutc(*cp++, fp);
                    }
                    foff += sizeof(euxcNode);
                    break;
                default:
                    setOffset();
                    writeNode(p);
                    foff += sizeof(euxcNode);
                    break;
            }
        }
    }

    // write the terminator
    euxcOSBPutc(euxmFree, fp);
    writePtr((euxmOffType) 0);

    // close the output file
    euxcOSClose(fp);

    // return successfully
    return (euxmTrue);
}

///  search for a file: first looking at an env variable, then a built-in path
//    NULL if not found
FILE *euxcPathOpen
(
    const char *fname,
    const char *env_var_name,
    const char **builtin_path,
    char *found
)
{
    const char *path = getenv(env_var_name);
    char env_search_path[1024];
    if (path)
    {
        strcpy(env_search_path, path);
    }
    else
    {
        *env_search_path = 0;
    }

    #ifdef PATH_OPEN
    fprintf(stderr, "path_open %s\n", fname);
    fprintf(stderr, "env is '%s'\n", env_search_path);
    #endif

    fp = NULL;

    // env variable path
    for
    (
        path = strtok(env_search_path, _path_open_path_sep);
        path;
        path = strtok(NULL, _path_open_path_sep)
    )
    {
        #ifdef PATH_OPEN
        fprintf(stderr, "searching env %s/%s\n", path, fname);
        #endif
        char buf[256];
        sprintf(buf, _path_open_string_format, path, fname);
        fp = euxcOSBOpen(buf, "r");
        if (fp != NULL)
        {
            break;
        }
    }

    #ifdef PATH_OPEN
    if (fp == NULL)
    {
        fprintf(stderr, "Searching built-in\n");
    }
    else
    {
        fprintf(stderr, "found it\n");
    }
    #endif

    // built in path
    for (const char **ppath = builtin_path; *ppath && fp == NULL; ppath++)
    {
        path = *ppath;
        #ifdef PATH_OPEN
        fprintf(stderr, "searching builtin %s/%s\n", path, fname);
        #endif
        char buf[256];
        sprintf(buf, "%s/%s", path, fname);
        fp = euxcOSBOpen(buf, "r");
    }

    if (fp != NULL && found != NULL)
    {
        strcpy(found, path);
    }

    return fp;
}

///  euxlRestoreImage - restore a saved memory image
int euxlRestoreImage(const char *fname)
{
    fp = euxcPathOpen(fname, "EU_IMAGE_PATH", image_search_path, NULL);

    if (fp == NULL)
    {
        return (euxmFalse);
    }

    // read the version identification
    char bannerbuf[128];
    unsigned char *cp = (unsigned char *)bannerbuf;
    int size = 0;
    do
    {
        *cp = euxcOSBGetc(fp);
        size++;
    } while (*cp++ && size < 80);

    *cp = 0;
    if (strcmp(euxlBanner, bannerbuf))
    {
        char err[128];
        sprintf(err,
        "bad image or image version mismatch:\nfound header '%s'",
        bannerbuf);
        euxcOSError(err);
        euxcOSClose(fp);
        return (euxmFalse);
    }

    // free the old memory image
    freeImage();

    // read the stack size
    unsigned int ssize = (unsigned int)readPtr();

    // allocate memory for the workspace
    euxcAllocInit(ssize);

    // read various constants
    euxs_eof = convertOffsetToPtr(readPtr());
    euxs_default = convertOffsetToPtr(readPtr());
    euxcRootModule = convertOffsetToPtr(readPtr());
    euxcReinternModule = convertOffsetToPtr(readPtr());
    euxcCurrentModule = convertOffsetToPtr(readPtr());
    euxcModuleList = convertOffsetToPtr(readPtr());
    euxlc_simple_class = convertOffsetToPtr(readPtr());
    euxlc_class = convertOffsetToPtr(readPtr());
    euxlc_object = convertOffsetToPtr(readPtr());
    euxlc_vector = convertOffsetToPtr(readPtr());
    euxcKeywordArray = convertOffsetToPtr(readPtr());
    euxcObArray = convertOffsetToPtr(readPtr());

    // read each node
    int type;
    euxlValue p;
    for (off = (euxmOffType) 2; (type = euxcOSBGetc(fp)) >= 0;)
    {
        switch (type)
        {
            case euxmFree:
                if ((off = readPtr()) == (euxmOffType) 0)
                {
                    goto done;
                }
                break;
            case euxmCons:
            case euxmClosure:
            case euxmPromise:
            case euxmEnv:
                p = convertOffsetToPtr(off);
                p->type = type;
                euxmSetCar(p, convertOffsetToPtr(readPtr()));
                euxmSetCdr(p, convertOffsetToPtr(readPtr()));
                off += sizeof(euxcNode);
                break;
            case euxmSymbol:
            case euxmObject:
            case euxmVector:
            case euxmCode:
            case euxmContinuation:
            case euxmModule:
            case euxmGeneric:
            case euxmMethod:
            case euxmSlot:
            case euxmTable:
                p = convertOffsetToPtr(off);
                p->type = type;
                p->value.vector.size = size = (int)readPtr();
                p->value.vector.data = getVectorSpace(p, size);
                for (euxlValue *vp = p->value.vector.data; --size >= 0;)
                {
                    *vp++ = convertOffsetToPtr(readPtr());
                }
                off += sizeof(euxcNode);
                break;
            case euxmString:
                p = convertOffsetToPtr(off);
                p->type = type;
                p->value.vector.size = size = (int)readPtr();
                p->value.vector.data =
                    getVectorSpace(p, euxmByteToWordSize(size));
                for (cp = (unsigned char *)euxmGetString(p); --size >= 0;)
                {
                    *cp++ = euxcOSBGetc(fp);
                }
                off += sizeof(euxcNode);
                break;
            case euxmStream:
                p = convertOffsetToPtr(off);
                readNode(type, p);
                euxmSetFile(p, NULL);
                off += sizeof(euxcNode);
                break;
            case euxmXFun:
                p = convertOffsetToPtr(off);
                readNode(type, p);
                p->value.fun.xfun = xFunTab[euxmGetFunOffset(p)].fun;
                off += sizeof(euxcNode);
                break;
            case euxmFun:
                p = convertOffsetToPtr(off);
                readNode(type, p);
                p->value.fun.fun = funTab[euxmGetFunOffset(p)].fun;
                off += sizeof(euxcNode);
                break;
            default:
                readNode(type, convertOffsetToPtr(off));
                off += sizeof(euxcNode);
                break;
        }
    }

done:

    // close the input file
    euxcOSClose(fp);

    // collect to initialize the free space
    gc(euxmGcSave);

    // plenty of room
    euxcVexpand(euxmVsSize);

    // lookup all of the symbols the interpreter uses
    {
        euxlValue curmod = euxcCurrentModule;
        euxcCurrentModule = euxcRootModule;
        euxcInitSymbols();
        euxcCurrentModule = curmod;
    }

    // return successfully
    return (euxmTrue);
}

///  freeImage - free the current memory image
static void freeImage()
{
    // close all open streams and free each node segment
    while (nsegments != NULL)
    {
        euxcNodeSegment *nextnseg = nsegments->next;
        euxlValue p = &nsegments->data[0];
        int n = nsegments->size;
        for (; --n >= 0; ++p)
        {
            switch (euxmNodeType(p))
            {
                case euxmStream:
                    {
                        FILE *fp;
                        if
                        (
                            (fp = euxmGetFile(p)) != NULL
                            && (fp != stdin && fp != stdout && fp != stderr)
                        )
                        {
                            euxcOSClose(euxmGetFile(p));
                        }
                    }
                    break;
            }
        }
        free((char *)nsegments);
        nsegments = nextnseg;
    }

    // free each vector segment
    while (vsegments != NULL)
    {
        euxcVectorSegment *nextvseg = vsegments->next;
        free((char *)vsegments);
        vsegments = nextvseg;
    }

    // free the stack
    if (euxcStackBase)
    {
        free((char *)euxcStackBase);
    }
}

///  setOffset - output a positioning command if nodes have been skipped
static void setOffset()
{
    if (off != foff)
    {
        euxcOSBPutc(euxmFree, fp);
        writePtr(off);
        foff = off;
    }
}

///  writeNode - write a node to a file
static void writeNode(euxlValue node)
{
    char *p = (char *)&node->value;
    int n = sizeof(union euxcNodeValue);
    euxcOSBPutc(node->type, fp);
    while (--n >= 0)
    {
        euxcOSBPutc(*p++, fp);
    }
}

///  writePtr - write a pointer to a file
static void writePtr(euxmOffType off)
{
    char *p = (char *)&off;
    int n = sizeof(euxmOffType);
    while (--n >= 0)
    {
        euxcOSBPutc(*p++, fp);
    }
}

///  readNode - read a node
static void readNode(int type, euxlValue node)
{
    char *p = (char *)&node->value;
    int n = sizeof(union euxcNodeValue);
    node->type = type;
    while (--n >= 0)
    {
        *p++ = euxcOSBGetc(fp);
    }
}

///  readPtr - read a pointer
static euxmOffType readPtr()
{
    euxmOffType off;
    char *p = (char *)&off;
    int n = sizeof(euxmOffType);
    while (--n >= 0)
    {
        *p++ = euxcOSBGetc(fp);
    }
    return (off);
}

///  convertOffsetToPtr - convert a pointer on input
static euxlValue convertOffsetToPtr(euxmOffType o)
{
    euxcNodeSegment *euxcNewnsegment(), *nseg;
    euxmOffType off = (euxmOffType) 2;
    euxmOffType nextoff;

    // Check for nil and small FPIs
    if (o == (euxmOffType) 0 || (o & 1) == 1)
    {
        return ((euxlValue) o);
    }

    // compute a pointer for this offset
    for (nseg = nsegments; nseg != NULL; nseg = nseg->next)
    {
        nextoff = off + (euxmOffType) (nseg->size * sizeof(euxcNode));
        if (o >= off && o < nextoff)
        {
            return ((euxlValue) ((euxmOffType) & nseg->data[0] + o - off));
        }
        off = nextoff;
    }

    // create new segments if necessary
    for (;;)
    {
        // create the next segment
        if ((nseg = euxcNewnsegment(euxmNsSize)) == NULL)
        {
            euxcFatal("insufficient memory - segment");
        }

        // Check to see if the offset is in this segment
        nextoff = off + (euxmOffType) (nseg->size * sizeof(euxcNode));
        if (o >= off && o < nextoff)
        {
            break;
        }
        off = nextoff;
    }

    return ((euxlValue) ((euxmOffType) & nseg->data[0] + o - off));
}

///  euxcMakePtr - convert a pointer on output
euxmOffType euxcMakePtr(euxlValue p)
{
    euxmOffType off = (euxmOffType) 2;
    euxcNodeSegment *nseg;

    // Check for nil and small FPIs
    if (p == euxmNil || !euxmIsPointer(p))
    {
        return ((euxmOffType) p);
    }

    // compute an offset for this pointer
    for (nseg = nsegments; nseg != NULL; nseg = nseg->next)
    {
        if (euxmInSegment(p, nseg))
        {
            return (off + ((euxmOffType) p - (euxmOffType) & nseg->data[0]));
        }
        off += (euxmOffType) (nseg->size * sizeof(euxcNode));
    }

    // pointer not within any segment
    euxcError("bad pointer found during image save", p);

    return ((euxmOffType) 0);       // never reached
}

///  getVectorSpace - allocate vector space
static euxlValue *getVectorSpace(euxlValue node, unsigned int size)
{
    ++size;     // space for the back pointer
    if
    (
        !euxmVcompare(vfree, size, euxmVecStackTop)
     && !euxcCheckVmemory(size)
     && !euxcMakeVmemory(size)
    )
    {
        euxcFatal("insufficient vector space");
    }

    euxlValue *p = vfree;
    vfree += size;
    *p++ = node;

    return (p);
}


///-----------------------------------------------------------------------------
