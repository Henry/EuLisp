// xsprint.c - xscheme print routine
/*      Copyright (c) 1988, by David Michael Betz
        All Rights Reserved */
// Euscheme code Copyright (c) 1994 Russell Bradford

#include "xscheme.h"
#include "xsobj.h"
#include "xssymbols.h"

// global variables
int prbreadth = 200;
int prdepth = 30;

// local variables
static char buf[200];

static void print(LVAL fptr, LVAL vptr, int escflag, int depth);
static void putatm(LVAL fptr, char *tag, LVAL val);
static void putstring(LVAL fptr, char *str, int len);
static void putobject(LVAL fptr, LVAL val, int escflag, int depth);
static void putsym(LVAL fptr, char *str, int escflag, int keyword);
static void putsubr(LVAL fptr, char *tag, LVAL val);
static void putclosure(LVAL fptr, char *tag, LVAL val);
static void putcode(LVAL fptr, char *tag, LVAL val);
static void putnumber(LVAL fptr, FIXTYPE n);
//static void putoct(LVAL fptr,int n);
static void putflonum(LVAL fptr, FLOTYPE n);
static void putcharacter(LVAL fptr, int ch);
static void putgeneric(LVAL fptr, LVAL vptr);
static void putmethod(LVAL fptr, LVAL vptr);
static void putslot(LVAL fptr, LVAL vptr);
static void puttable(LVAL fptr, LVAL vptr);
static void putclassname(LVAL fptr, LVAL vptr);

// xlprin1 - print an expression with quoting
void xlprin1(LVAL expr, LVAL file)
{
    print(file, expr, TRUE, 0);
}

// xlprinc - print an expression without quoting
void xlprinc(LVAL expr, LVAL file)
{
    print(file, expr, FALSE, 0);
}

// xlterpri - terminate the current print line
void xlterpri(LVAL fptr)
{
    xlputc(fptr, '\n');
}

// xlputstr - output a string
void xlputstr(LVAL fptr, char *str)
{
    while (*str)
        xlputc(fptr, *str++);
}

// print - internal print routine
static void print(LVAL fptr, LVAL vptr, int escflag, int depth)
{
    int breadth, size, i;
    LVAL nptr, next;

    // print nil
    if (vptr == NIL)
    {
        xlputstr(fptr, "()");
        return;
    }

    // check value type
    switch (ntype(vptr))
    {
        case SUBR:
        case XSUBR:
            putsubr(fptr, "Subr", vptr);
            break;
        case CSUBR:
            putsubr(fptr, "CSubr", vptr);
            break;
        case CONS:
            if (prdepth >= 0 && depth >= prdepth)
            {
                xlputstr(fptr, "(...)");
                break;
            }
            xlputc(fptr, '(');
            breadth = 0;
            for (nptr = vptr; nptr != NIL;)
            {
                if (prbreadth >= 0 && breadth++ >= prbreadth)
                {
                    xlputstr(fptr, "...");
                    break;
                }
                print(fptr, car(nptr), escflag, depth + 1);
                if ((next = cdr(nptr)) != NIL)
                {
                    if (consp(next))
                    {
                        xlputc(fptr, ' ');
                    }
                    else
                    {
                        xlputstr(fptr, " . ");
                        print(fptr, next, escflag, depth + 1);
                        break;
                    }
                }
                nptr = next;
            }
            xlputc(fptr, ')');
            break;
        case VECTOR:
            if (prdepth >= 0 && depth >= prdepth)
            {
                xlputstr(fptr, "#(...)");
                break;
            }
            xlputstr(fptr, "#(");
            for (i = 0, size = getsize(vptr); i < size; ++i)
            {
                if (i != 0)
                    xlputc(fptr, ' ');
                print(fptr, getelement(vptr, i), escflag, depth + 1);
            }
            xlputc(fptr, ')');
            break;
        case OBJECT:
            putobject(fptr, vptr, escflag, depth);
            break;
        case SYMBOL:
            putsym(fptr, getstring(getpname(vptr)), escflag, keywordp(vptr));
            if (getvalue(s_qualified_symbols) != NIL)
            {
                xlputc(fptr, '@');
                if (getmodule(vptr) == NIL)
                    xlputstr(fptr, "keyword");
                else
                    xlputstr(fptr, getstring(getmname(getmodule(vptr))));
            }
            break;
        case PROMISE:
            if (getpproc(vptr) != NIL)
                putatm(fptr, "Promise", vptr);
            else
                putatm(fptr, "Forced-promise", vptr);
            break;
        case CLOSURE:
            putclosure(fptr, "simple-function", vptr);
            break;
        case FIXNUM:
            putnumber(fptr, getfixnum(vptr));
            break;
        case FLONUM:
            putflonum(fptr, getflonum(vptr));
            break;
        case CHAR:
            if (escflag)
                putcharacter(fptr, getchcode(vptr));
            else
                xlputc(fptr, getchcode(vptr));
            break;
        case STRING:
            if (escflag)
                putstring(fptr, getstring(vptr), getslength(vptr) - 1);
            else
                xlputstr(fptr, getstring(vptr));
            break;
        case STREAM:
            putatm(fptr, "file-stream:", vptr);
            break;
        case CODE:
            putcode(fptr, "Code", vptr);
            break;
        case CONTINUATION:
            putatm(fptr, "Escape-procedure", vptr);
            break;
        case ENV:
            putatm(fptr, "Environment", vptr);
            break;
        case TABLE:
            puttable(fptr, vptr);
            break;
        case FREE:
            putatm(fptr, "Free", vptr);
            break;
        case MODULE:
            sprintf(buf, "#<Module %s>", getstring(getmname(vptr)));
            xlputstr(fptr, buf);
            break;
        case METHOD:
            putmethod(fptr, vptr);
            break;
        case GENERIC:
            putgeneric(fptr, vptr);
            break;
        case SLOT:
            putslot(fptr, vptr);
            break;
        default:
            putatm(fptr, "Foo", vptr);
            break;
    }
}

// putatm - output an atom
static void putatm(LVAL fptr, char *tag, LVAL val)
{
    sprintf(buf, "#<%s #", tag);
    xlputstr(fptr, buf);
    sprintf(buf, AFMT, (OFFTYPE)val);
    xlputstr(fptr, buf);
    xlputc(fptr, '>');
}

// putstring - output a string
static void putstring(LVAL fptr, char *str, int len)
{
    int ch, i;

    // output the initial quote
    xlputc(fptr, '"');

    // output each character in the string
    for (i = 0; i < len; i++)
    {
        ch = *str++;
        switch (ch)
        {
            case '\\':
                xlputstr(fptr, "\\\\");
                break;
            case '"':
                xlputstr(fptr, "\\\"");
                break;
            case '\007':
                xlputstr(fptr, "\\a");
                break;
            case '\b':
                xlputstr(fptr, "\\b");
                break;
            case '\177':       // delete
                xlputstr(fptr, "\\d");
                break;
            case '\f':
                xlputstr(fptr, "\\f");
                break;
            case '\n':
                xlputstr(fptr, "\\n");
                break;
            case '\r':
                xlputstr(fptr, "\\r");
                break;
            case '\t':
                xlputstr(fptr, "\\t");
                break;
            case '\v':
                xlputstr(fptr, "\\v");
                break;
            case EOF:
                xlputstr(fptr, "\\e");
                break;
            default:
                if (isascii(ch) && isprint(ch))
                    xlputc(fptr, ch);
                else
                {
                    sprintf(buf, "\\x%04x", ch & 0xff);
                    xlputstr(fptr, buf);
                }
                break;
        }
    }

    // output the terminating quote
    xlputc(fptr, '"');
}

/* do we need to escape this id when printing?
 * yes if (1) it contains a dodgy character
 *        (2) it is the id of zero length
 *        (3) it starts with the syntax of a number
 *        (4) it ends with a : but is not a keyword
 *
 */
static int escaped_id(char *id, int keyword)
{
    int i;

    for (i = 0; id[i]; i++)
        if (!isgraph(id[i]) || id[i] == '|' || id[i] == '\\')
            return 1;

    //***HGW #t and #f are special cases
    //***HGW if (id[0] == '#' && (id[1] == 't' || id[1] == 'f') && id[2] == 0)
    //***HGW    return 0;

    if (strpbrk(id, "|\\#()\"',;` ") || id[0] == 0 ||   // zero length id
    (!keyword && id[strlen(id) - 1] == ':') || isdigit(id[0]) ||    // 123
    (id[0] == '.' && !id[1]) ||     // |.|
    (id[0] == '.' && id[1] && isdigit(id[1])) ||    // .123
    ((id[0] == '+' || id[0] == '-') && id[1] && (isdigit(id[1]) ||  /* +123
                                                                     */
    (id[1] == '.' && id[2] && isdigit(id[2])))))    // +.123
        return 1;
    else
        return 0;
}

// putsym - output a symbol
static void putsym(LVAL fptr, char *str, int escflag, int keyword)
{
    int ch;

    // check for printing without escapes
    if (!escflag)
    {
        xlputstr(fptr, str);
        return;
    }

    // output each character
    if (escaped_id(str, keyword))
    {
        xlputc(fptr, '|');
        while ((ch = *str++) != '\0')
        {
            if (ch == '|' || ch == '\\')
                xlputc(fptr, '\\');
            if (keyword && ch == ':' && *str == 0)
            {
                xlputc(fptr, '|');
                xlputc(fptr, ':');
                return;
            }
            xlputc(fptr, ch);
        }
        xlputc(fptr, '|');
    }
    else
        while ((ch = *str++) != '\0')
            xlputc(fptr, ch);

}

static void strip_angles(char *name, LVAL fptr)
{
    int len;

    len = strlen(name);
    if (name[0] == '<' && name[len - 1] == '>')
    {
        strncpy(buf, name + 1, len - 2);
        buf[len - 2] = 0;
        putsym(fptr, buf, TRUE, FALSE);
    }
    else
        putsym(fptr, name, TRUE, FALSE);
}

// take care on thread <-> lock references
static void putthread(LVAL fptr, LVAL val, LVAL cls)
{
    LVAL slots;
    int i, len;

    xlputstr(fptr, "#<");
    putclassname(fptr, cls);

    for (slots = getivar(cls, SLOTS), i = 1; slots; slots = cdr(slots), i++)
    {
        xlputstr(fptr, " ");
        xlprin1(getslotname(car(slots)), fptr);
        xlputstr(fptr, " ");
        if (!strcmp(getstring(getpname(getslotname(car(slots)))), "locks"))
        {
            len = length(getivar(val, i));
            xlprin1(cvsfixnum(len), fptr);
        }
        else
            xlprin1(getivar(val, i), fptr);
    }

    sprintf(buf, ">");
    xlputstr(fptr, buf);
}

// putobject - output a telos object
static void putobject(LVAL fptr, LVAL val, int escflag, int depth)
{
    char *name;
    LVAL cls, slots;
    int i;
    extern LVAL s_thread_class;

    cls = getclass(val);
    if (cls == getvalue(s_thread_class))
    {
        putthread(fptr, val, cls);
        return;
    }

    xlputstr(fptr, "#<");
    if (classp(val))
    {
        #if 0
        putclassname(fptr, cls);
        xlputstr(fptr, " ");
        #else
        xlputstr(fptr, "class ");
        #endif
        if (symbolp(getivar(val, CNAME)))
            name = getstring(getpname(getivar(val, CNAME)));
        else
            name = "(anon)";
        strip_angles(name, fptr);
    }
    else
    {
        if (symbolp(getivar(cls, CNAME)))
            name = getstring(getpname(getivar(cls, CNAME)));
        else
            name = "(anon)";
        strip_angles(name, fptr);
        if (prdepth >= 0 && depth >= prdepth)
            xlputstr(fptr, " ...");
        else
        {
            for (slots = getivar(cls, SLOTS), i = 1; slots;
                 slots = cdr(slots), i++)
            {
                xlputstr(fptr, " ");
                xlprin1(getslotname(car(slots)), fptr);
                xlputstr(fptr, " ");
                print(fptr, getivar(val, i), escflag, depth + 1);
            }
        }
    }
    xlputstr(fptr, ">");

}

// putsubr - output a subr/fsubr
static void putsubr(LVAL fptr, char *tag, LVAL val)
{
    extern FUNDEF funtab[];
    sprintf(buf, "#<%s %s>", tag, funtab[getoffset(val)].fd_name);
    xlputstr(fptr, buf);
}

// putclosure - output a closure
static void putclosure(LVAL fptr, char *tag, LVAL val)
{
    putcode(fptr, tag, getcode(val));
}

// putcode - output a code object
static void putcode(LVAL fptr, char *tag, LVAL val)
{
    LVAL name;
    if ((name = getelement(val, 1)) == NIL)
        putatm(fptr, tag, val);
    else
    {
        sprintf(buf, "#<%s %s>", tag, getstring(getpname(name)));
        xlputstr(fptr, buf);
    }
}

// putnumber - output a number
static void putnumber(LVAL fptr, FIXTYPE n)
{
    LVAL fmt = getvalue(s_fixfmt);
    sprintf(buf, (stringp(fmt) ? (char *)getstring(fmt) : IFMT), n);
    xlputstr(fptr, buf);
}

// putflonum - output a flonum
static void putflonum(LVAL fptr, FLOTYPE n)
{
    LVAL fmt = getvalue(s_flofmt);
    sprintf(buf, (stringp(fmt) ? (char *)getstring(fmt) : FFMT), n);
    xlputstr(fptr, buf);
}

// putcharacter - output a character value
static void putcharacter(LVAL fptr, int ch)
{
    switch (ch)
    {
        case '\007':   // '\a'
            xlputstr(fptr, "#\\a");
            break;
        case '\b':
            xlputstr(fptr, "#\\b");
            break;
        case '\177':
            xlputstr(fptr, "#\\d");
            break;
        case '\f':
            xlputstr(fptr, "#\\f");
            break;
        case '\n':
            xlputstr(fptr, "#\\n");
            break;
        case '\r':
            xlputstr(fptr, "#\\r");
            break;
        case ' ':
            xlputstr(fptr, " ");
            break;
        case '\t':
            xlputstr(fptr, "#\\t");
            break;
        case '\v':
            xlputstr(fptr, "#\\v");
            break;
        case EOF:
            xlputstr(fptr, "#\\eof");
            break;
        default:
            if (ch == 0)
                sprintf(buf, "#\\^@");
            else if (iscntrl(ch))
                sprintf(buf, "#\\^%c",
                (isupper(ch + '@') ? ch + '`' : ch + '@'));
            else if (ch > '\177')
                sprintf(buf, "#\\x%02X", ch);
            else if (ch < 0)
                sprintf(buf, "#\\x%04X", (unsigned int)ch & 0xFFFF);
            else
                sprintf(buf, "#\\%c", ch);
            xlputstr(fptr, buf);
            break;
    }
}

static void putgeneric(LVAL fptr, LVAL vptr)
{
    xlputstr(fptr, "#<");
    putclassname(fptr, class_of(vptr));

    sprintf(buf, " %s>",
    symbolp(getgname(vptr)) ?
    getstring(getpname(getgname(vptr))) : "(anon)");
    xlputstr(fptr, buf);
}

static void putmethod(LVAL fptr, LVAL vptr)
{
    LVAL gf, domain;
    char *name;

    xlputstr(fptr, "#<");
    putclassname(fptr, class_of(vptr));

    if (genericp(getmdgf(vptr)))
    {
        gf = getmdgf(vptr);
        sprintf(buf, " %s",
        symbolp(getgname(gf)) ?
        getstring(getpname(getgname(gf))) : "(anon gf)");
        xlputstr(fptr, buf);
    }
    else
    {
        xlputstr(fptr, " (unattached)");
    }

    domain = getmddomain(vptr);
    for (; domain; domain = cdr(domain))
    {
        xlputstr(fptr, " ");
        name = getstring(getpname(getivar(car(domain), CNAME)));
        strip_angles(name, fptr);
    }

    if (getmdopt(vptr))
        xlputstr(fptr, " . rest");

    xlputstr(fptr, ">");
}

static void putslot(LVAL fptr, LVAL vptr)
{
    xlputstr(fptr, "#<");
    putclassname(fptr, class_of(vptr));

    sprintf(buf, " %s>", getstring(getpname(getslotname(vptr))));
    xlputstr(fptr, buf);
}

static void puttable(LVAL fptr, LVAL vptr)
{
    LVAL comp;
    extern LVAL s_eq, s_eqv, s_equal, s_equals;
    LVAL eqp, eqvp, equalp, equals;

    eqp = getvalue(s_eq);
    eqvp = getvalue(s_eqv);
    equalp = getvalue(s_equal);
    equals = getvalue(s_equals);

    xlputstr(fptr, "#<");
    putclassname(fptr, class_of(vptr));

    comp = gettablecomp(vptr);
    if (comp == eqp)
        xlputstr(fptr, " eq>");
    else if (comp == eqvp)
        xlputstr(fptr, " eql>");
    else if (comp == equalp)
        xlputstr(fptr, " equal>");
    else if (comp == equals)
        xlputstr(fptr, " =>");
    else
        xlputstr(fptr, " \?\?\?>");
}

static void putclassname(LVAL fptr, LVAL cls)
{
    char *name;

    if (symbolp(getivar(cls, CNAME)))
        name = getstring(getpname(getivar(cls, CNAME)));
    else
        name = "(anon)";
    strip_angles(name, fptr);
}
