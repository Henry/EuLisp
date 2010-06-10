// xsimage.c - xscheme memory image save/restore functions
/*      Copyright (c) 1988, by David Michael Betz
        All Rights Reserved */
// Euscheme code Copyright (c) 1994 Russell Bradford

#ifdef RISCOS
#define _path_open_path_sep ","
#define _path_open_string_format "%s%s"
#else
#define _path_open_path_sep ":"
#define _path_open_string_format "%s/%s"
#endif

#include "xscheme.h"
#include "xsobj.h"
#include "xsbanner.h"

// virtual machine registers
extern LVAL xlfun;              // current function
extern LVAL xlenv;              // current environment
extern LVAL xlval;              // value of most recent instruction

// stack limits
extern LVAL *xlstkbase;         // base of value stack
extern LVAL *xlstktop;          // top of value stack

// node space
extern NSEGMENT *nsegments;     // list of node segments

// vector (and string) space
extern VSEGMENT *vsegments;     // list of vector segments
extern LVAL *vfree;             // next free location in vector space
extern LVAL *vtop;              // top of vector space

// global variables
extern LVAL eof_object, default_object;
extern JMP_BUF top_level;
extern FUNDEF funtab[];

// local variables
static OFFTYPE off, foff;
static FILE *fp;

// external routines
extern FILE *osbopen();

static char *image_search_path[] = { IMAGE_SEARCH_PATH, 0 };

// forward declarations
static void freeimage();
static void setoffset();
static void writenode(LVAL node);
static void writeptr(OFFTYPE off);
static void readnode(int type, LVAL node);
static OFFTYPE readptr();
static LVAL cviptr(OFFTYPE o);
OFFTYPE cvoptr(LVAL p);
static LVAL *getvspace(LVAL node, unsigned int size);

// xlisave - save the memory image
int xlisave(char *fname)
{
    unsigned char *cp;
    NSEGMENT *nseg;
    int size, n;
    LVAL p, *vp;

    // open the output file
    if ((fp = osbopen(fname, "w")) == NULL)
        return (FALSE);

    // first call the garbage collector to clean up memory
    gc(GC_SAVE);

    // write out version identification
    size = strlen(BANNER);
    for (cp = (unsigned char *)BANNER; size-- >= 0;)
        osbputc(*cp++, fp);

    // write out the stack size
    writeptr((OFFTYPE) (xlstktop - xlstkbase));

    // write out various constants
    writeptr(cvoptr(eof_object));
    writeptr(cvoptr(default_object));
    writeptr(cvoptr(root_module));
    writeptr(cvoptr(reintern_module));
    writeptr(cvoptr(current_module));
    writeptr(cvoptr(module_list));
    writeptr(cvoptr(simple_class));
    writeptr(cvoptr(class));
    writeptr(cvoptr(object));
    writeptr(cvoptr(class_vector));
    writeptr(cvoptr(keyword_array));
    #ifndef OLDSYM
    writeptr(cvoptr(obarray));
    #endif

    // setup the initial file offsets
    off = foff = (OFFTYPE) 2;

    // write out all nodes that are still in use
    for (nseg = nsegments; nseg != NULL; nseg = nseg->ns_next)
    {
        p = &nseg->ns_data[0];
        n = nseg->ns_size;
        for (; --n >= 0; ++p, off += sizeof(NODE))
            switch (ntype(p))
            {
                case FREE:
                    break;
                case CONS:
                case CLOSURE:
                case PROMISE:
                case ENV:
                    setoffset();
                    osbputc(p->n_type, fp);
                    writeptr(cvoptr(car(p)));
                    writeptr(cvoptr(cdr(p)));
                    foff += sizeof(NODE);
                    break;
                case SYMBOL:
                case OBJECT:
                case VECTOR:
                case CODE:
                case CONTINUATION:
                case MODULE:
                case GENERIC:
                case METHOD:
                case SLOT:
                case TABLE:
                    setoffset();
                    osbputc(p->n_type, fp);
                    size = getsize(p);
                    writeptr((OFFTYPE) size);
                    for (vp = p->n_vdata; --size >= 0;)
                        writeptr(cvoptr(*vp++));
                    foff += sizeof(NODE);
                    break;
                case STRING:
                    setoffset();
                    osbputc(p->n_type, fp);
                    size = getslength(p);
                    writeptr((OFFTYPE) size);
                    for (cp = (unsigned char *)getstring(p); --size >= 0;)
                        osbputc(*cp++, fp);
                    foff += sizeof(NODE);
                    break;
                default:
                    setoffset();
                    writenode(p);
                    foff += sizeof(NODE);
                    break;
            }
    }

    // write the terminator
    osbputc(FREE, fp);
    writeptr((OFFTYPE) 0);

    // close the output file
    osclose(fp);

    // return successfully
    return (TRUE);
}

/* search for a file: first looking at an env variable,
 * then a built-in path
 * NULL if not found */
FILE *path_open(fname, env_var_name, builtin_path, found)
    char *fname;
    char *env_var_name;
    char **builtin_path;
    char *found;
{
    char **ppath, buf[256];
    extern FILE *osbopen();
    char env_search_path[1024], *path;
    extern char *getenv(), *strtok();

    path = getenv(env_var_name);
    if (path)
        strcpy(env_search_path, path);
    else
        *env_search_path = 0;

    #ifdef PATH_OPEN
    fprintf(stderr, "path_open %s\n", fname);
    fprintf(stderr, "env is '%s'\n", env_search_path);
    #endif

    fp = NULL;

    // env variable path
    for (path = strtok(env_search_path, _path_open_path_sep);
         path; path = strtok(NULL, _path_open_path_sep))
    {
        #ifdef PATH_OPEN
        #ifdef RISCOS
        fprintf(stderr, "searching env " _path_open_string_format "\n", path,
        fname);
        #else
        fprintf(stderr, "searching env %s/%s\n", path, fname);
        #endif
        #endif
        sprintf(buf, _path_open_string_format, path, fname);
        fp = osbopen(buf, "r");
        if (fp != NULL)
            break;
    }

    #ifdef PATH_OPEN
    if (fp == NULL)
        fprintf(stderr, "Searching built-in\n");
    else
        fprintf(stderr, "found it\n");
    #endif

    // built in path
    for (ppath = builtin_path; *ppath && fp == NULL; ppath++)
    {
        path = *ppath;
        #ifdef PATH_OPEN
        #ifdef RISCOS
        fprintf(stderr, "searching builtin " _path_open_string_format "\n",
        path, fname);
        #else
        fprintf(stderr, "searching builtin %s/%s\n", path, fname);
        #endif
        #endif
        sprintf(buf, "%s/%s", path, fname);
        fp = osbopen(buf, "r");
    }

    if (fp != NULL && found != NULL)
        strcpy(found, path);
    return fp;
}


// xlirestore - restore a saved memory image
int xlirestore(char *fname)
{
    unsigned int ssize;
    unsigned char *cp;
    int size, type;
    LVAL p, *vp;
    char bannerbuf[128];

    fp = path_open(fname, "EU_IMAGE_PATH", image_search_path, NULL);

    if (fp == NULL)
        return (FALSE);

    // read the version identification
    cp = (unsigned char *)bannerbuf;
    size = 0;
    do
    {
        *cp = osbgetc(fp);
        size++;
    } while (*cp++ && size < 80);
    *cp = 0;
    if (strcmp(BANNER, bannerbuf))
    {
        char err[128];
        sprintf(err,
        "bad image or image version mismatch:\nfound header '%s'",
        bannerbuf);
        oserror(err);
        osclose(fp);
        return (FALSE);
    }

    // free the old memory image
    freeimage();

    // read the stack size
    ssize = (unsigned int)readptr();

    // allocate memory for the workspace
    xlminit(ssize);

    // read various constants
    eof_object = cviptr(readptr());
    default_object = cviptr(readptr());
    root_module = cviptr(readptr());
    reintern_module = cviptr(readptr());
    current_module = cviptr(readptr());
    module_list = cviptr(readptr());
    simple_class = cviptr(readptr());
    class = cviptr(readptr());
    object = cviptr(readptr());
    class_vector = cviptr(readptr());
    keyword_array = cviptr(readptr());
    #ifndef OLDSYM
    obarray = cviptr(readptr());
    #endif

    // read each node
    for (off = (OFFTYPE) 2; (type = osbgetc(fp)) >= 0;)
        switch (type)
        {
            case FREE:
                if ((off = readptr()) == (OFFTYPE) 0)
                    goto done;
                break;
            case CONS:
            case CLOSURE:
            case PROMISE:
            case ENV:
                p = cviptr(off);
                p->n_type = type;
                rplaca(p, cviptr(readptr()));
                rplacd(p, cviptr(readptr()));
                off += sizeof(NODE);
                break;
            case SYMBOL:
            case OBJECT:
            case VECTOR:
            case CODE:
            case CONTINUATION:
            case MODULE:
            case GENERIC:
            case METHOD:
            case SLOT:
            case TABLE:
                p = cviptr(off);
                p->n_type = type;
                p->n_vsize = size = (int)readptr();
                p->n_vdata = getvspace(p, size);
                for (vp = p->n_vdata; --size >= 0;)
                    *vp++ = cviptr(readptr());
                off += sizeof(NODE);
                break;
            case STRING:
                p = cviptr(off);
                p->n_type = type;
                p->n_vsize = size = (int)readptr();
                p->n_vdata = getvspace(p, btow_size(size));
                for (cp = (unsigned char *)getstring(p); --size >= 0;)
                    *cp++ = osbgetc(fp);
                off += sizeof(NODE);
                break;
            case PORT:
                p = cviptr(off);
                readnode(type, p);
                setfile(p, NULL);
                off += sizeof(NODE);
                break;
            case SUBR:
            case XSUBR:
                p = cviptr(off);
                readnode(type, p);
                p->n_subr = funtab[getoffset(p)].fd_subr;
                off += sizeof(NODE);
                break;
            default:
                readnode(type, cviptr(off));
                off += sizeof(NODE);
                break;
        }
done:

    // close the input file
    osclose(fp);

    // collect to initialize the free space
    gc(GC_SAVE);
    // plenty of room
    vexpand(VSSIZE);

    // lookup all of the symbols the interpreter uses
    {
        LVAL curmod = current_module;
        current_module = root_module;
        xlsymbols();
        current_module = curmod;
    }

    // return successfully
    return (TRUE);
}

// freeimage - free the current memory image
static void freeimage()
{
    NSEGMENT *nextnseg;
    VSEGMENT *nextvseg;
    FILE *fp;
    LVAL p;
    int n;

    // close all open ports and free each node segment
    while (nsegments != NULL)
    {
        nextnseg = nsegments->ns_next;
        p = &nsegments->ns_data[0];
        n = nsegments->ns_size;
        for (; --n >= 0; ++p)
            switch (ntype(p))
            {
                case PORT:
                    if ((fp = getfile(p)) != NULL
                    && (fp != stdin && fp != stdout && fp != stderr))
                        osclose(getfile(p));
                    break;
            }
        free((char *)nsegments);
        nsegments = nextnseg;
    }

    // free each vector segment
    while (vsegments != NULL)
    {
        nextvseg = vsegments->vs_next;
        free((char *)vsegments);
        vsegments = nextvseg;
    }

    // free the stack
    if (xlstkbase)
        free((char *)xlstkbase);
}

// setoffset - output a positioning command if nodes have been skipped
static void setoffset()
{
    if (off != foff)
    {
        osbputc(FREE, fp);
        writeptr(off);
        foff = off;
    }
}

// writenode - write a node to a file
static void writenode(LVAL node)
{
    char *p = (char *)&node->n_info;
    int n = sizeof(union ninfo);
    osbputc(node->n_type, fp);
    while (--n >= 0)
        osbputc(*p++, fp);
}

// writeptr - write a pointer to a file
static void writeptr(OFFTYPE off)
{
    char *p = (char *)&off;
    int n = sizeof(OFFTYPE);
    while (--n >= 0)
        osbputc(*p++, fp);
}

// readnode - read a node
static void readnode(int type, LVAL node)
{
    char *p = (char *)&node->n_info;
    int n = sizeof(union ninfo);
    node->n_type = type;
    while (--n >= 0)
        *p++ = osbgetc(fp);
}

// readptr - read a pointer
static OFFTYPE readptr()
{
    OFFTYPE off;
    char *p = (char *)&off;
    int n = sizeof(OFFTYPE);
    while (--n >= 0)
        *p++ = osbgetc(fp);
    return (off);
}

// cviptr - convert a pointer on input
static LVAL cviptr(OFFTYPE o)
{
    NSEGMENT *newnsegment(), *nseg;
    OFFTYPE off = (OFFTYPE) 2;
    OFFTYPE nextoff;

    // check for nil and small fixnums
    if (o == (OFFTYPE) 0 || (o & 1) == 1)
        return ((LVAL) o);

    // compute a pointer for this offset
    for (nseg = nsegments; nseg != NULL; nseg = nseg->ns_next)
    {
        nextoff = off + (OFFTYPE) (nseg->ns_size * sizeof(NODE));
        if (o >= off && o < nextoff)
            return ((LVAL) ((OFFTYPE) & nseg->ns_data[0] + o - off));
        off = nextoff;
    }

    // create new segments if necessary
    for (;;)
    {

        // create the next segment
        if ((nseg = newnsegment(NSSIZE)) == NULL)
            xlfatal("insufficient memory - segment");

        // check to see if the offset is in this segment
        nextoff = off + (OFFTYPE) (nseg->ns_size * sizeof(NODE));
        if (o >= off && o < nextoff)
            break;
        off = nextoff;
    }

    return ((LVAL) ((OFFTYPE) & nseg->ns_data[0] + o - off));
}

// cvoptr - convert a pointer on output
OFFTYPE cvoptr(LVAL p)
{
    OFFTYPE off = (OFFTYPE) 2;
    NSEGMENT *nseg;

    // check for nil and small fixnums
    if (p == NIL || !ispointer(p))
        return ((OFFTYPE) p);

    // compute an offset for this pointer
    for (nseg = nsegments; nseg != NULL; nseg = nseg->ns_next)
    {
        if (INSEGMENT(p, nseg))
            return (off + ((OFFTYPE) p - (OFFTYPE) & nseg->ns_data[0]));
        off += (OFFTYPE) (nseg->ns_size * sizeof(NODE));
    }

    // pointer not within any segment
    xlerror("bad pointer found during image save", p);
    return ((OFFTYPE) 0);       // never reached
}

// getvspace - allocate vector space
static LVAL *getvspace(LVAL node, unsigned int size)
{
    LVAL *p;
    ++size;     // space for the back pointer
    if (!VCOMPARE(vfree, size, vtop)
    && !checkvmemory(size) && !makevmemory(size))
        xlfatal("insufficient vector space");
    p = vfree;
    vfree += size;
    *p++ = node;
    return (p);
}
