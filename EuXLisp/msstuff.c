// msstuff.c - OS specific routines
/*      Copyright (c) 1988, by David Michael Betz
        All Rights Reserved */
// Euscheme code Copyright (c) 1994 Russell Bradford

#ifdef NOTDEF
#include <dos.h>
#endif
#include <sys/types.h>
#include <time.h>
#include <sys/time.h>
#include "xscheme.h"

#define LBSIZE 200

unsigned _stklen = 16384;

// external variables
#include "xssymbols.h"
extern FILE *tfp;

// old errnos were everywhere
#if 0
#if defined(CODEBLDR)
#include <errno.h>
#else
extern int errno;
#endif
#endif

// new place for errno
#include <errno.h>

#ifdef UNIX
int reading;
#endif

// local variables
static char lbuf[LBSIZE];
#ifndef UNIX
static int lpos[LBSIZE];
#endif
static int lindex;
static int lcount;
static int lposition;
#if DOBBS
static long rseed = 1L;
#endif

#ifdef UNIX
#define xputc putchar
#define MAXFDS                  32
#ifndef FD_SET
#define FD_SET(fd,fdset)        (fdset)->fds_bits[0] |= (1<<(fd))
#define FD_CLR(fd,fdset)        (fdset)->fds_bits[0] &= ~(1<<(fd))
#define FD_ZERO(fdset)          (fdset)->fds_bits[0] = 0
#define FD_ISSET(fd,fdset)      (((fdset)->fds_bits[0]) & (1<<(fd)))
typedef struct fd_set
{
    long fds_bits[32];
} fd_set;
#endif

#endif

#ifdef NOTDEF
LVAL tmpfilelist = NIL;
#endif

static void xflush();
#ifndef UNIX
static void xinfo();
static int xgetc();
static void xputc(int ch);
static int xcheck();
#endif

void ostputc(), ostputs(), oscheck(), osflush();
static void xflush();
void check_if_disabled();

// main - the main routine
int main(int argc, char *argv[])
{
    xlmain(argc, argv);
    return 0;
}

// osinit - initialize
void osinit(char *banner)
{
    extern int quiet;

    if (!quiet)
    {
        ostputs(banner);
        #ifdef SOCK
        ostputs("s");
        #endif
        ostputc('\n');
    }
    lposition = 0;
    lindex = 0;
    lcount = 0;
}

// osfinish - clean up before returning to the operating system
void osfinish()
{
    #ifdef NOTDEF
    for (; tmpfilelist; tmpfilelist = cdr(tmpfilelist))
    {
        fclose(getfile(car(car(tmpfilelist))));
        osunlink(getstring(cdr(car(tmpfilelist))));
    }
    #endif
}

// oserror - print an error message
void oserror(char *msg)
{
    ostputs("error: ");
    ostputs(msg);
    ostputc('\n');
}

// osrand - return a random number between 0 and n-1
#ifdef DOBBS
int osrand(int n)
{
    long k1;

    // make sure we don't get stuck at zero
    if (rseed == 0L)
        rseed = 1L;

    // algorithm taken from Dr. Dobbs Journal, November 1985, page 91
    k1 = rseed / 127773L;
    if ((rseed = 16807L * (rseed - k1 * 127773L) - k1 * 2836L) < 0L)
        rseed += 2147483647L;

    // return a random number between 0 and n-1
    return ((int)(rseed % (long)n));
}
#else
// Wichmann & Hill
int osrand(int n)
{
    static int x = 50, y = 100, z = 150;
    double val;

    x = (171 * x) % 30269;
    y = (172 * y) % 30307;
    z = (170 * z) % 30323;

    val = (x / 30269.0) + (y / 30307.0) + (z / 30323.0);
    val -= (int)val;

    return (int)(n * val);
}
#endif

// osaopen - open an ascii file
FILE *osaopen(char *name, char *mode)
{
    return (fopen(name, mode));
}

// osbopen - open a binary file
FILE *osbopen(char *name, char *mode)
{
    #ifdef UNIX
    return (fopen(name, mode));
    #else
    char bmode[10];

    strcpy(bmode, mode);
    strcat(bmode, "b");
    return (fopen(name, bmode));
    #endif
}

// osclose - close a file
int osclose(FILE *fp)
{
    #ifdef NOTDEF
    int ret;
    LVAL list1, list2;

    if (tmpfilelist == NIL)
        return fclose(fp);

    if (getfile(car(car(tmpfilelist))) == fp)
    {
        ret = fclose(fp);
        osunlink(getstring(cdr(car(tmpfilelist))));
        tmpfilelist = cdr(tmpfilelist);
        return ret;
    }
    for (list1 = tmpfilelist, list2 = cdr(list1); list2;
         list1 = cdr(list1), list2 = cdr(list2))
        if (getfile(car(car(list2))) == fp)
        {
            ret = fclose(fp);
            osunlink(getstring(cdr(car(list2))));
            cdr(list1) = cdr(list2);
            return ret;
        }
    return fclose(fp);
    #else
    return (fclose(fp));
    #endif
}

// ostell - get the current file position
long ostell(FILE *fp)
{
    return (ftell(fp));
}

// osunlink - remove a file
int osunlink(char *path)
{
    return remove(path);
}

// osseek - set the current file position
int osseek(FILE *fp, long offset, int whence)
{
    return (fseek(fp, offset, whence));
}

// osagetc - get a character from an ascii file
int osagetc(FILE *fp)
{
    int ch;

    #ifdef UNIX
    reading = 1;
    ch = getc(fp);
    reading = 0;
    #else
    ch = getc(fp);
    #endif
    return ch;
}

// osaputc - put a character to an ascii file
int osaputc(int ch, FILE *fp)
{
    return (putc(ch, fp));
}

// osbgetc - get a character from a binary file
int osbgetc(FILE *fp)
{
    int ch;

    #ifdef UNIX
    reading = 1;
    ch = getc(fp);
    reading = 0;
    #else
    ch = getc(fp);
    #endif
    return ch;
}

// osbputc - put a character to a binary file
int osbputc(int ch, FILE *fp)
{
    return (putc(ch, fp));
}

// ostgetc - get a character from the terminal
int ostgetc()
{
    #ifdef UNIX
    extern int ctrl_c;
    #else
    int ch;
    #endif
    extern int quiet;

    // check for a buffered character
    if (lcount--)
        return (lbuf[lindex++]);

    #ifdef UNIX
    reading = 1;
    fgets(lbuf, LBSIZE, stdin);
    reading = 0;

    if (ctrl_c)
    {
        lcount = 0;
        ctrl_c = 0;
        osflush();
        ostputc('\n');
        xltoplevel();
    }
    lcount = strlen(lbuf);

    if (feof(stdin))
    {
        if (!quiet)
            ostputc('\n');
        clearerr(stdin);
        lcount = 0;
        return EOF;
    }

    // write it to the transcript file
    if (tfp)
        for (lindex = 0; lindex < lcount; ++lindex)
            osaputc(lbuf[lindex], tfp);

    lindex = 0;
    lcount--;
    return (lbuf[lindex++]);
    #else
    // get an input line
    for (lcount = 0;;)
        switch (ch = xgetc())
        {
            case '\r':
                lbuf[lcount++] = '\n';
                xputc('\r');
                xputc('\n');
                lposition = 0;
                if (tfp)
                    for (lindex = 0; lindex < lcount; ++lindex)
                        osaputc(lbuf[lindex], tfp);
                lindex = 0;
                lcount--;
                return (lbuf[lindex++]);
            case '\010':
            case '\177':
            if (lcount)
            {
                lcount--;
                while (lposition > lpos[lcount])
                {
                    xputc('\010');
                    xputc(' ');
                    xputc('\010');
                    lposition--;
                }
            }
            break;
            case '\032':
                xflush();
                return (EOF);
            default:
                if (ch == '\t' || (ch >= 0x20 && ch < 0x7F))
                {
                    lbuf[lcount] = ch;
                    lpos[lcount] = lposition;
                    if (ch == '\t')
                        do
                        {
                            xputc(' ');
                        } while (++lposition & 7);
                    else
                    {
                        xputc(ch);
                        lposition++;
                    }
                    lcount++;
                }
                else
                {
                    xflush();
                    switch (ch)
                    {
                        case '\003':
                            xltoplevel();       // control-c
                        case '\007':
                            xlcleanup();        // control-g
                        case '\020':
                            xlcontinue();       // control-p
                        case '\032':
                            return (EOF);       // control-z
                        case '\034':
                            xlwrapup(0);        /* control-\ */
                        default:
                            return (ch);
                    }
                }
        }
    #endif
}

int osselect(FILE *fp)
{
    #if defined(UNIX)
    int ch, fd;
    fd_set readfds;
    struct timeval poll;

    fd = fileno(fp);
    FD_ZERO(&readfds);
    FD_SET(fd, &readfds);
    poll.tv_sec = 0;
    poll.tv_usec = 0;

    if (select(MAXFDS, &readfds, NULL, NULL, &poll) < 0)
        return NOCHAR;

    if (FD_ISSET(fd, &readfds))
    {
        ch = getc(fp);
        ungetc(ch, fp);
    }
    else
        ch = NOCHAR;

    return ch;
    #else
    return NOCHAR;
    #endif
}


// ospeekchar
int ospeekchar(FILE *fp)
{
    #ifdef UNIX
    // grub around in the internals of stdio.h
    #ifdef __linux
    #ifdef OLD_LINUX
    // earlier versions of Linux require this
    if (fp->_gptr < fp->_egptr)
        return (int)(*(unsigned char *)fp->_gptr);
    #else
    if (fp->_IO_read_ptr < fp->_IO_save_end)
        return (int)(*(unsigned char *)fp->_IO_read_ptr);
    #endif
    #else
    #ifdef __BSD_NET2
    // for 386BSD and the like (Torek's stdio)
    if (fp->_r > 0)
        return (int)*fp->_p;
    #else
    // otherwise it generally looks like this
    if (fp->_cnt > 0)
        return (int)*fp->_ptr;
    #endif // linux
    #endif

    return osselect(fp);
    #else
    int ch;

    if (fp == stdin)
    {
        ch = xcheck();
        return ch == (int)NULL ? NOCHAR : ch;
    }
    return osselect(fp);
    #endif
}

// ostputc - put a character to the terminal
void ostputc(int ch)
{
    // check for control characters
    oscheck();

    // output the character
    if (ch == '\n')
    {
        #ifndef UNIX
        xputc('\r');
        #endif
        xputc('\n');
        lposition = 0;
    }
    else
    {
        xputc(ch);
        lposition++;
    }

    // output the character to the transcript file
    if (tfp)
        osaputc(ch, tfp);
}

// ostputs - output a string to the terminal
void ostputs(char *str)
{
    while (*str != '\0')
        ostputc(*str++);
}

// osflush - flush the terminal input buffer
void osflush()
{
    lindex = lcount = lposition = 0;
}

// oscheck - check for control characters during execution
void oscheck()
{
    #ifdef UNIX
    extern int ctrl_c;

    if (ctrl_c)
    {
        ctrl_c = 0;
        xflush();
        xltoplevel();
    }
    #else
    switch (xcheck())
    {
        case '\002':   // control-b
        case '\003':   // control-c
        xflush();
        xltoplevel();
        break;
        case '\024':   // control-t
            xinfo();
            break;
        case '\023':   // control-s
            while (xcheck() != '\021')
                ;
            break;
        case '\034':   /* control-\ */
            xlwrapup(0);
            break;
    }
    #endif
}

// oscheck_int - check for control characters during interpreting
void oscheck_int()
{
    #ifdef UNIX
    extern int ctrl_c;

    if (ctrl_c)
    {
        ctrl_c = 0;
        xflush();
        xltoplevel_int();
    }
    #else
    switch (xcheck())
    {
        case '\002':   // control-b
        case '\003':   // control-c
        xflush();
        xltoplevel();
        break;
        case '\024':   // control-t
            xinfo();
            break;
        case '\023':   // control-s
            while (xcheck() != '\021')
                ;
            break;
        case '\034':   /* control-\ */
            xlwrapup(0);
            break;
    }
    #endif
}

#ifndef UNIX
// xinfo - show information on control-t
static void xinfo()
{
    /*
      extern int nfree,gccalls;
      extern long total;
      char buf[80];
      sprintf(buf,"\n[ Free: %d, GC calls: %d, Total: %ld ]",
      nfree,gccalls,total);
      errputstr(buf);
    */
}
#endif

// xflush - flush the input line buffer and start a new line
static void xflush()
{
    osflush();
    ostputc('\n');
}

#ifndef UNIX
// xgetc - get a character from the terminal without echo
static int xgetc()
{
    return (bdos(7, 0, 0) & 0xFF);
}

// xputc - put a character to the terminal
static void xputc(int ch)
{
    bdos(6, ch, 0);
}

// xcheck - check for a character
static int xcheck()
{
    return (bdos(6, 0xFF, 0) & 0xFF);
}
#endif

#ifdef NOTDEF
// xinbyte - read a byte from an input port
LVAL xinbyte()
{
    int portno;
    LVAL val;
    static char *cfn_name = "inbyte";
    val = xlgafixnum();
    portno = (int)getfixnum(val);
    xllastarg();
    return (cvfixnum((FIXTYPE) inportb(portno)));
}

// xoutbyte - write a byte to an output port
LVAL xoutbyte()
{
    int portno, byte;
    LVAL val;
    static char *cfn_name = "outbyte";
    val = xlgafixnum();
    portno = (int)getfixnum(val);
    val = xlgafixnum();
    byte = (int)getfixnum(val);
    xllastarg();
    outportb(portno, byte);
    return (NIL);
}

// xint86 - invoke a system interrupt
LVAL xint86()
{
    union REGS inregs, outregs;
    struct SREGS sregs;
    LVAL inv, outv, val;
    int intno;
    static char *cfn_name = "int86";

    // get the interrupt number and the list of register values
    val = xlgafixnum();
    intno = (int)getfixnum(val);
    inv = xlgavector();
    outv = xlgavector();
    xllastarg();

    // check the vector lengths
    if (getsize(inv) != 9)
        xlerror("incorrect vector length", inv);
    if (getsize(outv) != 9)
        xlerror("incorrect vector length", outv);

    // load each register from the input vector
    val = getelement(inv, 0);
    inregs.x.ax = (fixp(val) ? (int)getfixnum(val) : 0);
    val = getelement(inv, 1);
    inregs.x.bx = (fixp(val) ? (int)getfixnum(val) : 0);
    val = getelement(inv, 2);
    inregs.x.cx = (fixp(val) ? (int)getfixnum(val) : 0);
    val = getelement(inv, 3);
    inregs.x.dx = (fixp(val) ? (int)getfixnum(val) : 0);
    val = getelement(inv, 4);
    inregs.x.si = (fixp(val) ? (int)getfixnum(val) : 0);
    val = getelement(inv, 5);
    inregs.x.di = (fixp(val) ? (int)getfixnum(val) : 0);
    val = getelement(inv, 6);
    sregs.es = (fixp(val) ? (int)getfixnum(val) : 0);
    val = getelement(inv, 7);
    sregs.ds = (fixp(val) ? (int)getfixnum(val) : 0);
    val = getelement(inv, 8);
    inregs.x.cflag = (fixp(val) ? (int)getfixnum(val) : 0);

    // do the system interrupt
    int86x(intno, &inregs, &outregs, &sregs);

    // store the results in the output vector
    setelement(outv, 0, cvfixnum((FIXTYPE) outregs.x.ax));
    setelement(outv, 1, cvfixnum((FIXTYPE) outregs.x.bx));
    setelement(outv, 2, cvfixnum((FIXTYPE) outregs.x.cx));
    setelement(outv, 3, cvfixnum((FIXTYPE) outregs.x.dx));
    setelement(outv, 4, cvfixnum((FIXTYPE) outregs.x.si));
    setelement(outv, 5, cvfixnum((FIXTYPE) outregs.x.di));
    setelement(outv, 6, cvfixnum((FIXTYPE) sregs.es));
    setelement(outv, 7, cvfixnum((FIXTYPE) sregs.ds));
    setelement(outv, 8, cvfixnum((FIXTYPE) outregs.x.cflag));

    // return the result list
    return (outv);
}

// getnext - get the next fixnum from a list
static int getnext(LVAL *plist)
{
    LVAL val;
    if (consp(*plist))
    {
        val = car(*plist);
        *plist = cdr(*plist);
        if (!fixp(val))
            xlerror("expecting an integer", val);
        return ((int)getfixnum(val));
    }
    return (0);
}
#endif

// time initialisation
#ifndef NO_GTOD

#define GETTIMEOFDAY(x) gettimeofday(x, NULL)

static long time_offset;

// gettimeofday returns microseconds, but milliseconds is more realistic
#define NOISE 1000

void set_ticks()
{
    struct timeval tp;

    GETTIMEOFDAY(&tp);
    time_offset = tp.tv_sec;
    setvalue(xlenter("ticks-per-second"), cvflonum(1e6 / NOISE));
}

// xtime - get the current time
LVAL xtime()
{
    static char *cfn_name = "current-time";
    struct timeval tp;

    xllastarg();

    GETTIMEOFDAY(&tp);
    return cvfixnum(((tp.tv_sec - time_offset) * 1000000 + tp.tv_usec) / NOISE);
}

#else

void set_ticks()
{
    setvalue(xlenter("ticks-per-second"), cvflonum(1.0));
}

// xtime - get the current time
LVAL xtime()
{
    static char *cfn_name = "current-time";
    xllastarg();
    return (cvfixnum((FIXTYPE) time((time_t) 0)));
}
#endif // GTOD

#ifdef MSDOS
// xdifftime - get the difference between two time values
LVAL xdifftime()
{
    static char *cfn_name = "difftime";
    time_t t1, t2;
    LVAL val;
    val = xlgafixnum();
    t1 = (time_t) getfixnum(val);
    val = xlgafixnum();
    t2 = (time_t) getfixnum(val);
    xllastarg();
    return (cvflonum((FLOTYPE) difftime(t1, t2)));
}
#endif

void check_if_disabled(char *name)
{
    extern int no_system;

    if (no_system)
        xlcerror("function disabled", cvstring(name), NIL);
}

// xsystem - execute a system command
LVAL xsystem()
{
    static char *cfn_name = "system";
    char *cmd;

    check_if_disabled(cfn_name);

    cmd = (char *)getstring(xlgastring());
    xllastarg();

    return (system(cmd) == 0 ? true : cvfixnum((FIXTYPE) errno));
}

#if 0
// tmpfile - open a temporary file
LVAL xtmpfile()
{
    static char *cfn_name = "tmpfile";
    FILE *fp;
    #ifdef NOTDEF
    char *getenv(), *str;
    #endif

    xllastarg();

    #ifdef NOTDEF
    // not everyone has a /tmp
    str = getenv("TMPDIR");
    if (str == NULL)
    {
        putenv("TMPDIR=.");
    }
    #endif

    fp = tmpfile();
    if (fp == NULL)
        xlcerror("failed to create temporary file", cvstring(cfn_name), NIL);

    return cvport(fp, PF_INPUT | PF_OUTPUT);
}

#else

LVAL xtmpfile()
{
    static char *cfn_name = "tmpfile";
    FILE *fp;
    LVAL port;
    #ifdef NOTDEF
    char *name, *tempnam();
    #endif

    xllastarg();

    // tmpfile doesn't seem to work on dos gcc
    #ifdef NOTDEF
    name = tempnam(".", "eutmp");
    fp = fopen(name, "w+b");
    #else
    fp = tmpfile();
    #endif
    if (fp == NULL)
        xlcerror("failed to create temporary file", cvstring(cfn_name), NIL);

    port = cvport(fp, PF_INPUT | PF_OUTPUT);

    #ifdef NOTDEF
    cpush(port);
    tmpfilelist = cons(cons(port, cvstring(name)), tmpfilelist);
    drop(1);
    #endif

    return port;
}
#endif

// xgetenv - getenv
LVAL xgetenv()
{
    static char *cfn_name = "getenv";
    extern char *getenv();
    LVAL arg;
    char *str;

    arg = xlgastring();
    xllastarg();

    str = getenv(getstring(arg));
    if (str == NULL)
        return NIL;
    return cvstring(str);
}

// xputenv - (putenv name val)
// some contortions to keep a chunk of string memory
LVAL xputenv()
{
    static char *cfn_name = "putenv";
    extern int putenv();
    LVAL name, val, env, new, old;
    extern LVAL s_supplied_env, xassoc();
    char buf[1024];
    int retval;

    name = xlgastring();
    val = xlgastring();
    xllastarg();

    sprintf(buf, "%s=%s", getstring(name), getstring(val));
    new = cvstring(buf);

    retval = putenv(getstring(new));
    if (retval > 0)
        return NIL;

    check(3);
    push(new);

    env = getvalue(s_supplied_env);
    push(env);
    push(name);
    xlargc = 2;
    old = xassoc();

    if (old)
        rplacd(old, new);
    else
    {
        env = cons(cons(name, new), env);
        setvalue(s_supplied_env, env);
    }

    drop(1);
    return new;
}

#ifdef MSDOS
// xgetkey - get a key from the keyboard
LVAL xgetkey()
{
    static char *cfn_name = "getkey";
    xllastarg();
    return (cvfixnum((FIXTYPE) xgetc()));
}
#endif

// ossymbols - enter os specific symbols
void ossymbols()
{
}
