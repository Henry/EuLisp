// msstuff.c - OS specific routines
/*      Copyright (c) 1988, by David Michael Betz
        All Rights Reserved */
// Euscheme code Copyright (c) 1994 Russell Bradford

#include <sys/types.h>
#include <time.h>
#include <sys/time.h>
#include "xscheme.h"

#ifdef READLINE
#include <readline/readline.h>
#include <readline/history.h>
#endif

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

int reading;

// local variables
#ifdef READLINE
static char* lbuf;
static char rl_histfile[255];
#else
#define LBSIZE 200
static char lbuf[LBSIZE];
#endif

static int lindex;
static int lcount;
static int lposition;

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

static void osflushn();
void ostputc(), ostputs(), oscheck(), osflush();
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

    #ifdef READLINE
    const char* eulisp_history = "/.eulisp_history";
    char* home = getenv("HOME");
    if (home == NULL)
    {
        ostputs("Cannot find environment variable HOME for reading ~/");
        oserror(eulisp_history);
    }
    else
    {
        strcpy(rl_histfile, home);
        strcat(rl_histfile, eulisp_history);

        if (!read_history(rl_histfile))
        {
            ostputs("Reading readline history from ");
            ostputs(rl_histfile);
            ostputc('\n');
        }
    }
    #endif
}

// osfinish - clean up before returning to the operating system
void osfinish()
{}

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
    // make sure we don't get stuck at zero
    if (rseed == 0L)
    {
        rseed = 1L;
    }

    // algorithm taken from Dr. Dobbs Journal, November 1985, page 91
    long k1 = rseed / 127773L;
    if ((rseed = 16807L * (rseed - k1 * 127773L) - k1 * 2836L) < 0L)
    {
        rseed += 2147483647L;
    }

    // return a random number between 0 and n-1
    return ((int)(rseed % (long)n));
}
#else
// Wichmann & Hill
int osrand(int n)
{
    static int x = 50, y = 100, z = 150;

    x = (171 * x) % 30269;
    y = (172 * y) % 30307;
    z = (170 * z) % 30323;

    double val = (x / 30269.0) + (y / 30307.0) + (z / 30323.0);
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
    return (fopen(name, mode));
}

// osclose - close a file
int osclose(FILE *fp)
{
    return (fclose(fp));
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
    reading = 1;
    int ch = getc(fp);
    reading = 0;

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
    reading = 1;
    int ch = getc(fp);
    reading = 0;
    return ch;
}

// osbputc - put a character to a binary file
int osbputc(int ch, FILE *fp)
{
    return (putc(ch, fp));
}

// Read a string, and return a pointer to it.
// Returns NULL on EOF.
#ifdef READLINE
void rlgets()
{
    // If the buffer has already been allocated,
    // return the memory to the free pool.
    if (lbuf)
    {
        free(lbuf);
        lbuf = (char *)NULL;
    }

    // Get a line from the user.
    lbuf = readline("");

    // If the line has any text in it,
    // save it on the history.
    if (lbuf && *lbuf)
    {
        add_history(lbuf);
        write_history(rl_histfile);
    }
}
#endif

// ostgetc - get a character from the terminal
int ostgetc()
{
    extern int ctrl_c;
    extern int quiet;

    // Check for a buffered character
    if (lcount--)
    {
        #ifdef READLINE
        // If it is the last character return \n
        if (lcount == 0)
        {
            return '\n';
        }
        else
        #endif
        {
            return (lbuf[lindex++]);
        }
    }

    reading = 1;

    #ifdef READLINE
    rlgets();
    #else
    fgets(lbuf, LBSIZE, stdin);
    #endif

    reading = 0;

    if (ctrl_c)
    {
        lcount = 0;
        ctrl_c = 0;
        osflush();
        ostputc('\n');
        xltoplevel();
    }

    // If the line buffer is not allocated or the stdin is at eof
    // return EOF
    #ifdef READLINE
    if (!lbuf || feof(stdin))
    #else
    if (feof(stdin))
    #endif
    {
        if (!quiet)
        {
            ostputc('\n');
        }
        clearerr(stdin);
        lcount = 0;
        return EOF;
    }

    lcount = strlen(lbuf);

    // For readline pretend the buffer is one longer than it really is
    // for the \n which readline strips
    #ifdef READLINE
    lcount++;
    #endif

    // write it to the transcript file
    if (tfp)
    {
        for (lindex = 0; lindex < lcount; ++lindex)
        {
            osaputc(lbuf[lindex], tfp);
        }
    }

    lindex = 0;
    lcount--;

    #ifdef READLINE
    // If it is the last character return \n
    if (lcount == 0)
    {
        return '\n';
    }
    else
    #endif
    {
        return (lbuf[lindex++]);
    }
}

int osselect(FILE *fp)
{
    int ch, fd;
    fd_set readfds;
    struct timeval poll;

    fd = fileno(fp);
    FD_ZERO(&readfds);
    FD_SET(fd, &readfds);
    poll.tv_sec = 0;
    poll.tv_usec = 0;

    if (select(MAXFDS, &readfds, NULL, NULL, &poll) < 0)
    {
        return NOCHAR;
    }

    if (FD_ISSET(fd, &readfds))
    {
        ch = getc(fp);
        ungetc(ch, fp);
    }
    else
    {
        ch = NOCHAR;
    }

    return ch;
}


// ospeekchar
int ospeekchar(FILE *fp)
{
    // grub around in the internals of stdio.h
    #ifdef __linux
    if (fp->_IO_read_ptr < fp->_IO_save_end)
    {
        return (int)(*(unsigned char *)fp->_IO_read_ptr);
    }
    #else
    #ifdef __BSD_NET2
    // for 386BSD and the like (Torek's stdio)
    if (fp->_r > 0)
    {
        return (int)*fp->_p;
    }
    #else
    // otherwise it generally looks like this
    if (fp->_cnt > 0)
    {
        return (int)*fp->_ptr;
    }
    #endif // linux
    #endif

    return osselect(fp);
}

// ostputc - put a character to the terminal
void ostputc(int ch)
{
    // check for control characters
    oscheck();

    // output the character
    if (ch == '\n')
    {
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
    {
        osaputc(ch, tfp);
    }
}

// ostputs - output a string to the terminal
void ostputs(char *str)
{
    while (*str != '\0')
    {
        ostputc(*str++);
    }
}

// osflush - flush the terminal input buffer
void osflush()
{
    lindex = lcount = lposition = 0;
}

// oscheck - check for control characters during execution
void oscheck()
{
    extern int ctrl_c;

    if (ctrl_c)
    {
        ctrl_c = 0;
        osflushn();
        xltoplevel();
    }
}

// oscheck_int - check for control characters during interpreting
void oscheck_int()
{
    extern int ctrl_c;

    if (ctrl_c)
    {
        ctrl_c = 0;
        osflushn();
        xltoplevel_int();
    }
}

// osflushn - flush the input line buffer and start a new line
static void osflushn()
{
    osflush();
    ostputc('\n');
}

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

void check_if_disabled(char *name)
{
    extern int no_system;

    if (no_system)
    {
        xlcerror("function disabled", cvstring(name), NIL);
    }
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

    xllastarg();

    FILE *fp = tmpfile();
    if (fp == NULL)
    {
        xlcerror("failed to create temporary file", cvstring(cfn_name), NIL);
    }

    return cvstream(fp, PF_INPUT | PF_OUTPUT);
}

#else

LVAL xtmpfile()
{
    static char *cfn_name = "tmpfile";

    xllastarg();

    // tmpfile doesn't seem to work on dos gcc
    FILE *fp = tmpfile();
    if (fp == NULL)
    {
        xlcerror("failed to create temporary file", cvstring(cfn_name), NIL);
    }

    LVAL stream = cvstream(fp, PF_INPUT | PF_OUTPUT);

    return stream;
}
#endif

// xgetenv - getenv
LVAL xgetenv()
{
    static char *cfn_name = "getenv";
    extern char *getenv();

    LVAL arg = xlgastring();
    xllastarg();

    char *str = getenv(getstring(arg));
    if (str == NULL)
    {
        return NIL;
    }

    return cvstring(str);
}

// xputenv - (putenv name val)
// some contortions to keep a chunk of string memory
LVAL xputenv()
{
    static char *cfn_name = "putenv";
    extern int putenv();
    extern LVAL s_supplied_env, xassoc();

    LVAL name = xlgastring();
    LVAL val = xlgastring();
    xllastarg();

    char buf[1024];
    sprintf(buf, "%s=%s", getstring(name), getstring(val));
    LVAL new = cvstring(buf);

    int retval = putenv(getstring(new));
    if (retval > 0)
    {
        return NIL;
    }

    check(3);
    push(new);

    LVAL env = getvalue(s_supplied_env);
    push(env);
    push(name);
    xlargc = 2;
    LVAL old = xassoc();

    if (old)
    {
        rplacd(old, new);
    }
    else
    {
        env = cons(cons(name, new), env);
        setvalue(s_supplied_env, env);
    }

    drop(1);
    return new;
}

// ossymbols - enter os specific symbols
void ossymbols()
{
}
