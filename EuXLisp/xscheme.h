/// Copyright 1988 David Michael Betz
/// Copyright 1994 Russell Bradford
/// Copyright 2010 Henry G. Weller
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
/// Title: xscheme definitions
///-----------------------------------------------------------------------------
#ifndef XSCHEME_H
#define XSCHEME_H

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <setjmp.h>
#ifdef STRINGH
#include <string.h>
#else
#include <strings.h>
#endif

// save signal masks
#ifdef _POSIX_SOURCE
#define JMP_BUF sigjmp_buf
#define LONGJMP siglongjmp
#define SETJMP(env) sigsetjmp(env, 1)
#else
#define JMP_BUF jmp_buf
#define LONGJMP longjmp
#define SETJMP(env) setjmp(env)
#endif

// for segmented addresses on Intel processors
#ifdef SEGADDR
#define INSEGMENT(n,s)  ((unsigned long)(n) >> 16 \
                      == (unsigned long)(s) >> 16)
#endif

// size of each type of memory segment
#ifndef NSSIZE
#define NSSIZE  4000    // number of nodes per node segment
#endif
#ifndef VSSIZE
#define VSSIZE  10000   // number of LVAL's per vector segment
#endif

// reasons for GC
#define GC_NODE   0
#define GC_VECTOR 1
#define GC_USER   2
#define GC_SAVE   3

// AFMT         printf format for addresses ("%x" or "%lx")
// OFFTYPE      number the size of an address (int or long)
// FIXTYPE      data type for fixed point numbers (long)
// ITYPE        fixed point input conversion function type (long atol())
// ICNV         fixed point input conversion function (atol)
// IFMT         printf format for fixed point numbers ("%ld")
// FLOTYPE      data type for floating point numbers (float)
// FFMT         printf format for floating point numbers (%.15g)

// default important word-size dependent definitions
#if WORD_LENGTH==32 // 32bit OS
#   define AFMT             "%x"
#   define OFFTYPE         int
#elif WORD_LENGTH==64 // 64bit OS
#   define AFMT            "%lx"
#   define OFFTYPE         long
#else
#   error WORD_LENGTH set incorrectly
#endif

// default important definitions
#define FIXTYPE         long
#define ITYPE           long atol()
#define ICNV(n)         atol(n)
#define IFMT            "%ld"
#define FLOTYPE         double
#define FFMT            "%#.15g"
#define SFIXMIN         -1048576
#define SFIXMAX         1048575

#ifndef INSEGMENT
#define INSEGMENT(n,s)  ((n) >= &(s)->ns_data[0] \
                      && (n) <  &(s)->ns_data[0] + (s)->ns_size)
#endif
#ifndef VCOMPARE
#define VCOMPARE(f,s,t) ((f) + (s) <= (t))
#endif

// useful definitions
#ifndef TRUE
#define TRUE    1
#endif
#ifndef FALSE
#define FALSE   0
#endif
#ifndef NIL
#define NIL     (LVAL)0
#endif

extern FILE *filein;
#define NOCHAR -2

// program limits
#define STRMAX          100     // maximum length of a string constant
#define HSIZE           199     // symbol hash table size
#define SAMPLE          100     // control character sample rate

// stack manipulation macros
#define check(n)        { if (xlsp - (n) < xlstkbase) xlstkover(); }
#define cpush(v)        { if (xlsp > xlstkbase) push(v); else xlstkover(); }
#define push(v)         (*--xlsp = (v))
#define pop()           (*xlsp++)
#define top()           (*xlsp)
#define settop(v)       (*xlsp = (v))
#define drop(n)         (xlsp += (n))

// argument list parsing macros
#define xlgetarg()      (testarg(nextarg()))
#define xllastarg()     {if (xlargc != 0) xltoomany(cfn_name);}
#define xlpoprest()     {xlsp += xlargc;}
#define testarg(e)      (moreargs() ? (e) : xltoofew(cfn_name))
#define typearg(tp,n)   (tp(*xlsp) ? nextarg() : xlbadtype(*xlsp,n,cfn_name))
#define nextarg()       (--xlargc, *xlsp++)
#define moreargs()      (xlargc > 0)

// macros to get arguments of a particular type
#define xlgacons()      (testarg(typearg(consp,"<cons>")))
#define xlgalist()      (testarg(typearg(listp,"<list>")))
#define xlgasymbol()    (testarg(typearg(symbolp,"<symbol>")))
#define xlgastring()    (testarg(typearg(stringp,"<string>")))
#define xlgaobject()    (testarg(typearg(objectp,"<object>")))
#define xlgafixnum()    (testarg(typearg(fixp,"<integer>")))
#define xlgafloat()     (testarg(typearg(floatp,"<float>")))
#define xlganumber()    (testarg(typearg(numberp,"<number>")))
#define xlgachar()      (testarg(typearg(charp,"<char>")))
#define xlgavector()    (testarg(typearg(vectorp,"<vector>")))
#define xlgastream()    (testarg(typearg(streamp,"<stream>")))
#define xlgaistream()   (testarg(typearg(istreamp,"<input-stream>")))
#define xlgaostream()   (testarg(typearg(ostreamp,"<output-stream>")))
#define xlgaclosure()   (testarg(typearg(closurep,"<closure>")))
#define xlgaenv()       (testarg(typearg(envp,"<env>")))
#define xlgamodule()    (testarg(typearg(modulep,"<module>")))
#define xlgageneric()   (testarg(typearg(genericp,"<generic>")))
#define xlgamethod()    (testarg(typearg(methodp,"<method>")))
#define xlgaslot()      (testarg(typearg(slotp,"<slot>")))
#define xlgatable()     (testarg(typearg(tablep,"<table>")))

// node types
#define FREE            0
#define CONS            1
#define SYMBOL          2
#define FIXNUM          3
#define FLONUM          4
#define STRING          5
#define OBJECT          6
#define STREAM          7
#define VECTOR          8
#define CLOSURE         9
#define CODE            11
#define SUBR            12
#define XSUBR           13
#define CSUBR           14
#define CONTINUATION    15
#define CHAR            16
#define PROMISE         17
#define ENV             18
#define MODULE          19
#define GENERIC         20
#define METHOD          21
#define SLOT            22
#define TABLE           23

// number of node types
#define NTYPES          24
#define NULLTYPE        NTYPES
#define KEYWORD         (NTYPES+1)
#define ISTREAM         (NTYPES+2)
#define OSTREAM         (NTYPES+3)
#define IOSTREAM        (NTYPES+4)
#define EXTRA_TYPES     5

// node flags
#define MARK            1
#define LEFT            2

// stream flags
#define PF_INPUT        1
#define PF_OUTPUT       2
#define PF_BINARY       4

// new node access macros
#define ntype(x)        ((OFFTYPE)(x) & 1 ? FIXNUM : (x)->n_type)

// macro to determine if a non-nil value is a pointer
#define ispointer(x)    (((OFFTYPE)(x) & 1) == 0)

// type predicates
#define atom(x)         ((x) == NIL || ntype(x) != CONS)
#define null(x)         ((x) == NIL)
#define listp(x)        ((x) == NIL || ntype(x) == CONS)
#define numberp(x)      ((x) && (ntype(x) == FIXNUM || ntype(x) == FLONUM))
#define boundp(x)       (getvalue(x) != s_unbound)
#define istreamp(x)     (streamp(x) && (getpflags(x) & PF_INPUT) != 0)
#define ostreamp(x)     (streamp(x) && (getpflags(x) & PF_OUTPUT) != 0)

// basic type predicates
#define consp(x)        ((x) && ntype(x) == CONS)
#define stringp(x)      ((x) && ntype(x) == STRING)
#define symbolp(x)      ((x) && ntype(x) == SYMBOL)
#define streamp(x)      ((x) && ntype(x) == STREAM)
#define objectp(x)      ((x) && ntype(x) == OBJECT)
#define fixp(x)         ((x) && ntype(x) == FIXNUM)
#define floatp(x)       ((x) && ntype(x) == FLONUM)
#define vectorp(x)      ((x) && ntype(x) == VECTOR)
#define closurep(x)     ((x) && ntype(x) == CLOSURE)
#define continuationp(x) ((x) && ntype(x) == CONTINUATION)
#define codep(x)        ((x) && ntype(x) == CODE)
#define subrp(x)        ((x) && ntype(x) == SUBR)
#define csubrp(x)       ((x) && ntype(x) == CSUBR)
#define xsubrp(x)       ((x) && ntype(x) == XSUBR)
#define charp(x)        ((x) && ntype(x) == CHAR)
#define promisep(x)     ((x) && ntype(x) == PROMISE)
#define envp(x)         ((x) && ntype(x) == ENV)
#define booleanp(x)     ((x) == NIL || ntype(x) == BOOLEAN)
#define modulep(x)      ((x) && ntype(x) == MODULE)
#define classp(x)       (objectp(x) && ((getclass(x) == simple_class) || \
                                        (xlsubclassp(getclass(x), class))))
#define genericp(x)     ((x) && ntype(x) == GENERIC)
#define methodp(x)      ((x) && ntype(x) == METHOD)
#define keywordp(x)     (symbolp(x) && (getmodule(x) == NIL))
#define slotp(x)        ((x) && ntype(x) == SLOT)
#define tablep(x)        ((x) && ntype(x) == TABLE)

/* vector update macro
   This is necessary because the memory pointed to by the n_vdata field
   of a vector object can move during a garbage collection.  This macro
   guarantees that evaluation happens in the right order.
*/
#define vupdate(x,i,v)  { LVAL vutmp=(v); (x)->n_vdata[i] = vutmp; }

// cons access macros
#define car(x)          ((x)->n_car)
#define cdr(x)          ((x)->n_cdr)
#define rplaca(x,y)     ((x)->n_car = (y))
#define rplacd(x,y)     ((x)->n_cdr = (y))

// symbol access macros
#define getvalue(x)      ((x)->n_vdata[0])
#define setvalue(x,v)    vupdate(x,0,v)
#define getpname(x)      ((x)->n_vdata[1])
#define setpname(x,v)    vupdate(x,1,v)
#define gettheplist(x)   ((x)->n_vdata[2])
#define settheplist(x,v) vupdate(x,2,v)
#define getplist(x)      car(gettheplist(x))
#define setplist(x,v)    rplaca(gettheplist(x),v)
#define symboleq(a,b)    (getpname(a) == getpname(b))
#define getmodule(x)     ((x)->n_vdata[3])
#define setmodule(x,v)   vupdate(x,3,v)
#define constantp(x)     ((x)->n_vdata[4])
#define setconstant(x,v) vupdate(x,4,v)
#define getsyntax(x)     ((x)->n_vdata[5])
#define putsyntax(x,v)   vupdate(x,5,v)
#define SYMSIZE         6

#define SYNTAX_OPEN     '{'
#define SYNTAX_CLOSE    '}'

// vector access macros
#define getsize(x)      ((x)->n_vsize)
#define getelement(x,i) ((x)->n_vdata[(int)(i)])
#define setelement(x,i,v) vupdate(x,i,v)

// object access macros
#define getclass(x)     ((x)->n_vdata[0])
#define setclass(x,v)   vupdate(x,0,v)
#define getivar(x,i)    ((x)->n_vdata[(int)(i)])
#define setivar(x,i,v)  vupdate(x,i,v)

// promise access macros
#define getpproc(x)     ((x)->n_car)
#define setpproc(x,v)   ((x)->n_car = (v))
#define getpvalue(x)    ((x)->n_cdr)
#define setpvalue(x,v)  ((x)->n_cdr = (v))

// closure access macros
#define getcode(x)      ((x)->n_car)
#define getcenv(x)      ((x)->n_cdr)

// code access macros
#define getbcode(x)             ((x)->n_vdata[0])
#define setbcode(x,v)           vupdate(x,0,v)
#define getcname(x)             ((x)->n_vdata[1])
#define setcname(x,v)           vupdate(x,1,v)
#define getvnames(x)            ((x)->n_vdata[2])
#define setvnames(x,v)          vupdate(x,2,v)
#define FIRSTLIT                3

// fixnum/flonum/character access macros
#define getfixnum(x)    ((OFFTYPE)(x) & 1 ? getsfixnum(x) : (x)->n_int)
#define getflonum(x)    ((x)->n_flonum)
#define getchcode(x)    ((x)->n_chcode)

// small fixnum access macros
#define cvsfixnum(x)    ((LVAL)(((OFFTYPE)x << 1) | 1))
#define getsfixnum(x)   ((FIXTYPE)((OFFTYPE)(x) >> 1))

// string access macros
#define getstring(x)    ((char *)(x)->n_vdata)
#define getslength(x)   ((x)->n_vsize)

// istream/ostream access macros
#define getfile(x)      ((x)->n_fp)
#define setfile(x,v)    ((x)->n_fp = (v))
#define getsavech(x)    ((x)->n_savech)
#define setsavech(x,v)  ((x)->n_savech = (v))
#define getpflags(x)    ((x)->n_pflags)
#define setpflags(x,v)  ((x)->n_pflags = (v))

// subr access macros
#define getsubr(x)      ((x)->n_subr)
#define getoffset(x)    ((x)->n_offset)

// module access macros
#define getmname(x)     ((x)->n_vdata[0])
#define setmname(x,v)   vupdate(x,0,v)
#define getmsymbols(x)  ((x)->n_vdata[1])
#define setmsymbols(x,v) vupdate(x,1,v)
#define getmexports(x)  ((x)->n_vdata[2])
#define setmexports(x,v) vupdate(x,2,v)
#define MODSIZE         3

// generic access macros
#define getgname(x)     ((x)->n_vdata[0])
#define setgname(x,v)   vupdate(x,0,v)
#define getgargs(x)     ((x)->n_vdata[1])
#define setgargs(x,v)   vupdate(x,1,v)
#define getgopt(x)      ((x)->n_vdata[2])
#define setgopt(x,v)    vupdate(x,2,v)
#define getgmethods(x)  ((x)->n_vdata[3])
#define setgmethods(x,v) vupdate(x,3,v)
#define getgcache1(x)    ((x)->n_vdata[4])
#define setgcache1(x,v)  vupdate(x,4,v)
#define getgcache2(x)    ((x)->n_vdata[5])
#define setgcache2(x,v)  vupdate(x,5,v)
#define GENSIZE         6

// method access macros
#define getmdgf(x)      ((x)->n_vdata[0])
#define setmdgf(x,v)    vupdate(x,0,v)
#define getmdfun(x)     ((x)->n_vdata[1])
#define setmdfun(x,v)   vupdate(x,1,v)
#define getmddomain(x)  ((x)->n_vdata[2])
#define setmddomain(x,v) vupdate(x,2,v)
#define getmdopt(x)     ((x)->n_vdata[3])
#define setmdopt(x,v)   vupdate(x,3,v)
#define MDSIZE 4

// slot access macros
#define getslotname(x)      ((x)->n_vdata[0])
#define setslotname(x,v)    vupdate(x,0,v)
#define getslotkey(x)       ((x)->n_vdata[1])
#define setslotkey(x,v)     vupdate(x,1,v)
#define getslotdefault(x)   ((x)->n_vdata[2])
#define setslotdefault(x,v) vupdate(x,2,v)
#define getslotrequiredp(x) ((x)->n_vdata[3])
#define setslotrequiredp(x,v) vupdate(x,3,v)
#define SLOTSIZE 4

// table access macros
#define gettablecomp(x)     ((x)->n_vdata[0])
#define settablecomp(x,v)   vupdate(x,0,v)
#define gettabletable(x)    ((x)->n_vdata[1])
#define settabletable(x,v)  vupdate(x,1,v)
#define gettablefill(x)     ((x)->n_vdata[2])
#define settablefill(x,v)   vupdate(x,2,v)
#define TABLESIZE 3

// size of a table
#define HTABLESIZE 31

// list node
#define n_car           n_info.n_xlist.xl_car
#define n_cdr           n_info.n_xlist.xl_cdr

// integer node
#define n_int           n_info.n_xint.xi_int

// flonum node
#define n_flonum        n_info.n_xflonum.xf_flonum

// character node
#define n_chcode        n_info.n_xchar.xc_chcode

// file pointer node
#define n_fp            n_info.n_xfptr.xf_fp
#define n_savech        n_info.n_xfptr.xf_savech
#define n_pflags        n_info.n_xfptr.xf_pflags

// vector/object node
#define n_vsize         n_info.n_xvect.xv_size
#define n_vdata         n_info.n_xvect.xv_data

// subr node
#define n_subr          n_info.n_xsubr.xs_subr
#define n_offset        n_info.n_xsubr.xs_offset

// node structure
typedef struct node
{
    char n_type;                // type of node
    char n_flags;               // flag bits
    union ninfo
    {                           // value
        struct xlist
        {                       // list node (cons)
            struct node *xl_car;        // the car pointer
            struct node *xl_cdr;        // the cdr pointer
        } n_xlist;
        struct xint
        {                       // integer node
            FIXTYPE xi_int;     // integer value
        } n_xint;
        struct xflonum
        {                       // flonum node
            FLOTYPE xf_flonum;  // flonum value
        } n_xflonum;
        struct xchar
        {                       // character node
            int xc_chcode;      // character code
        } n_xchar;
        struct xfptr
        {                       // file pointer node
            FILE *xf_fp;        // the file pointer
            short xf_savech;    // lookahead character for input files
            short xf_pflags;    // stream flags
        } n_xfptr;
        struct xvect
        {                       // vector node
            int xv_size;        // vector size
            struct node **xv_data;      // vector data
        } n_xvect;
        struct xsubr
        {                       // subr/fsubr node
            struct node *(*xs_subr) (); // function pointer
            int xs_offset;      // offset into funtab
        } n_xsubr;
    } n_info;
} NODE, *LVAL;

// memory allocator definitions

// macros to compute the size of a segment
#define nsegsize(n) (sizeof(NSEGMENT)+((n)-1)*sizeof(struct node))
#define vsegsize(n) (sizeof(VSEGMENT)+((n)-1)*sizeof(LVAL))

// macro to convert a byte size to a word size
#define btow_size(n)    (((n) + sizeof(LVAL) - 1) / sizeof(LVAL))

// node segment structure
typedef struct nsegment
{
    struct nsegment *ns_next;   // next node segment
    unsigned int ns_size;       // number of nodes in this segment
    struct node ns_data[1];     // segment data
} NSEGMENT;

// vector segment structure
typedef struct vsegment
{
    struct vsegment *vs_next;   // next vector segment
    LVAL *vs_free;              // next free location in this segment
    LVAL *vs_top;               // top of segment (plus one)
    LVAL vs_data[1];            // segment data
} VSEGMENT;

// function definition structure
typedef struct
{
    char *fd_name;              // function name
        LVAL(*fd_subr) ();      // function entry point
} FUNDEF;

// external variables
extern LVAL *xlstkbase;         // base of value stack
extern LVAL *xlstktop;          // top of value stack
extern LVAL *xlsp;              // value stack pointer
extern int xlargc;              // argument count for current call
extern LVAL current_module;     // current module
extern LVAL root_module;        // the root module
extern LVAL reintern_module;    // module for reinterning symbols
extern LVAL module_list;        // all the modules
extern LVAL keyword_array;      // all the keywords
extern LVAL obarray;            // prototype symbols

#define xlenter(name)           xlenter_module(name,current_module)

// external function declarations
#include "xsproto.h"

///-----------------------------------------------------------------------------
#endif // XSCHEME_H
///-----------------------------------------------------------------------------
