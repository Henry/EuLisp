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
/// Title: OS specific functions
///  Maintainer: Henry G. Weller
///-----------------------------------------------------------------------------
#include "euxlisp.h"

#include <errno.h>
#include <unistd.h>
#include <sys/times.h>
#include <sys/stat.h>
#include <stdarg.h>

#include <dlfcn.h>

#ifdef READLINE
#include <readline/readline.h>
#include <readline/history.h>

#include "../RLCompletion/eulisp_keywords.h"
#include "../RLCompletion/euxlisp_keywords.h"
#include "../RLCompletion/keyword_completion.h"
#endif

///-----------------------------------------------------------------------------
/// Global variables
///-----------------------------------------------------------------------------
int reading;

///-----------------------------------------------------------------------------
/// Macros
///-----------------------------------------------------------------------------
///  Line buffer size when not using readline
#define LBSIZE 200

///  Maximum number of file descriptors
#define MAXFDS                  32

///-----------------------------------------------------------------------------
/// Local variables
///-----------------------------------------------------------------------------
#ifdef READLINE
static char* lbuf;
static char rl_histfile[255];
#endif

static int lindex;
static int lcount;
static int lposition;

///-----------------------------------------------------------------------------
/// Forward declarations
///-----------------------------------------------------------------------------
static void osFlushNl();

///-----------------------------------------------------------------------------
/// String and file utility functions
///-----------------------------------------------------------------------------
///  euxcStringConcat - Concatenate strings, return is dynamically allocated
//    and must be freed
char *euxcStringConcat(const char *str, ...)
{
    // Sum sizes of all the argument strings
    // in order to allocate the correct result string
    size_t rlen = 1;

    va_list ap;
    va_start(ap, str);

    for (const char *s = str; s != NULL; s = va_arg(ap, const char *))
    {
        rlen += strlen(s);
    }

    va_end(ap);


    // Allocate the result string and concatenate arguments
    char *result = (char *)malloc(rlen);
    char *rp = result;

    va_start(ap, str);

    for (const char *s = str; s != NULL; s = va_arg(ap, const char *))
    {
        size_t len = strlen(s);
        rp = memcpy(rp, s, len) + len;
    }

    va_end(ap);

    // Terminate the result string
    *rp++ = '\0';

    // NOTE! this result string in dynamically allocated and must be freed
    return result;
}

///-----------------------------------------------------------------------------
/// Functions
///-----------------------------------------------------------------------------
///  euxcOSInit - initialize
void euxcOSInit(const char *banner)
{
    if (!quiet)
    {
        euxcOSTPuts(banner);
        #ifdef SOCK
        euxcOSTPuts("s");
        #endif
        euxcOSTPutc('\n');
    }
    lposition = 0;
    lindex = 0;
    lcount = 0;

    #ifdef READLINE
    if (!quiet)
    {
        rl_attempted_completion_function = keyword_completion;
        rl_bind_key('\t', rl_complete);

        char* eulisp_history = "/.eulisp_history";
        char* home = getenv("HOME");
        if (home == NULL)
        {
            euxcOSTPuts("Cannot find environment variable HOME for reading ~/");
            euxcOSError(eulisp_history);
        }
        else
        {
            strcpy(rl_histfile, home);
            strcat(rl_histfile, eulisp_history);

            if (!read_history(rl_histfile))
            {
                euxcOSTPuts("Reading readline history from ");
                euxcOSTPuts(rl_histfile);
                euxcOSTPutc('\n');
            }
        }
    }
    else
    #endif
    {
        lbuf = malloc(LBSIZE);
    }
}

///  euxcOSFinish - clean up before returning to the operating system
void euxcOSFinish()
{
    #ifdef READLINE
    if (quiet)
    #endif
    {
        free(lbuf);
    }
}

///  euxcOSError - print an error message
void euxcOSError(const char *msg)
{
    euxcOSTPuts("error: ");
    euxcOSTPuts(msg);
    euxcOSTPutc('\n');
}

///  euxcOSFileExists - returns true if file exists
int euxcOSFileExists(const char* filename)
{
    struct stat fileStat;
    return (stat(filename, &fileStat) == 0);
}

///  euxcOSFileNewer - returns true if f1 is newer than f2
int euxcOSFileNewer(const char* f1, const char* f2)
{
    struct stat f1Stat;
    int f1Exists = (stat(f1, &f1Stat) == 0);

    struct stat f2Stat;
    int f2Exists = (stat(f2, &f2Stat) == 0);

    return
    (
        (f1Exists && !f2Exists)
        || (f1Exists && f2Exists && f1Stat.st_mtime > f2Stat.st_mtime)
    );
}

///  euxcOSRand - return a random number between 0 and n-1
#ifdef DOBBS
int euxcOSRand(int n)
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
///  Wichmann & Hill
int euxcOSRand(int n)
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

///  euxcOSAOpen - open an ascii file
FILE *euxcOSAOpen(const char *name, const char *mode)
{
    return (fopen(name, mode));
}

///  euxcOSBOpen - open a binary file
FILE *euxcOSBOpen(const char *name, const char *mode)
{
    return (fopen(name, mode));
}

///  euxcOSClose - close a file
int euxcOSClose(FILE *fp)
{
    return (fclose(fp));
}

///  euxcOSTell - get the current file position
long euxcOSTell(FILE *fp)
{
    return (ftell(fp));
}

///  euxcOSUnlink - remove a file
int euxcOSUnlink(const char *path)
{
    return remove(path);
}

///  euxcOSSeek - set the current file position
int euxcOSSeek(FILE *fp, long offset, int whence)
{
    return (fseek(fp, offset, whence));
}

///  euxcOSAGetc - get a character from an ascii file
int euxcOSAGetc(FILE *fp)
{
    reading = 1;
    int ch = getc(fp);
    reading = 0;

    return ch;
}

///  euxcOSAPutc - put a character to an ascii file
int euxcOSAPutc(int ch, FILE *fp)
{
    return (putc(ch, fp));
}

///  euxcOSBGetc - get a character from a binary file
int euxcOSBGetc(FILE *fp)
{
    reading = 1;
    int ch = getc(fp);
    reading = 0;
    return ch;
}

///  euxcOSBPutc - put a character to a binary file
int euxcOSBPutc(int ch, FILE *fp)
{
    return (putc(ch, fp));
}

///  Read a string, and return a pointer to it.
///  Returns NULL on EOF.
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

    // Make the prompt from the current module name
    static char prompt[255];
    int debug_depth = 0;

    euxlValue thread_module = euxcGetModule("thread");

    if (thread_module)
    {
        // Tell the debugger that readline is handling the prompt
        euxmSetValue(euxcEnterModule("*debug-rl*", thread_module), euxs_t);

        // Get the debug-depth; 0 = no error
        debug_depth = euxmGetFPI
        (
            euxmGetValue(euxcEnterModule("*debug-depth*", thread_module))
        );
    }

    if (debug_depth)
    {
        sprintf
        (
            prompt,
            "[error%d] %s> ",
            debug_depth,
            euxmGetString(euxmGetModuleName(euxcCurrentModule))
        );
    }
    else
    {
        sprintf
        (
            prompt,
            "%s> ",
            euxmGetString(euxmGetModuleName(euxcCurrentModule))
        );
    }

    // Get a line from the user.
    lbuf = readline(prompt);

    // If the line has any text in it,
    // save it on the history.
    if (lbuf && *lbuf)
    {
        add_history(lbuf);
        write_history(rl_histfile);
    }
}
#endif

///  euxcOSTGetc - get a character from the terminal
int euxcOSTGetc()
{
    // Check for a buffered character
    if (lcount--)
    {
        #ifdef READLINE
        // If it is the last character return \n
        if (!quiet && lcount == 0)
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
    if (!quiet)
    {
        rlgets();
    }
    else
    #endif
    {
        fgets(lbuf, LBSIZE, stdin);
    }

    reading = 0;

    if (ctrl_c)
    {
        lcount = 0;
        ctrl_c = 0;
        euxcOSFlush();
        euxcOSTPutc('\n');
        euxcTopLevel();
    }

    // If the line buffer is not allocated or the stdin is at eof
    // return EOF
    #ifdef READLINE
    if ((!quiet && !lbuf) || feof(stdin))
    #else
    if (feof(stdin))
    #endif
    {
        if (!quiet)
        {
            euxcOSTPutc('\n');
        }
        clearerr(stdin);
        lcount = 0;
        return EOF;
    }

    lcount = strlen(lbuf);

    // For readline pretend the buffer is one longer than it really is
    // for the \n which readline strips
    #ifdef READLINE
    if (!quiet)
    {
        lcount++;
    }
    #endif

    lindex = 0;
    lcount--;

    #ifdef READLINE
    // If it is the last character return \n
    if (!quiet && lcount == 0)
    {
        return '\n';
    }
    else
    #endif
    {
        return (lbuf[lindex++]);
    }
}

int euxcOSSelect(FILE *fp)
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
        return euxmNoChar;
    }

    if (FD_ISSET(fd, &readfds))
    {
        ch = getc(fp);
        ungetc(ch, fp);
    }
    else
    {
        ch = euxmNoChar;
    }

    return ch;
}


///  euxcOSPeekChar
int euxcOSPeekChar(FILE *fp)
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

    return euxcOSSelect(fp);
}

///  euxcOSTPutc - put a character to the terminal
void euxcOSTPutc(int ch)
{
    // Check for control characters
    euxcOSCheck();

    // output the character
    if (ch == '\n')
    {
        putchar('\n');
        lposition = 0;
    }
    else
    {
        putchar(ch);
        lposition++;
    }
}

///  euxcOSTPuts - output a string to the terminal
void euxcOSTPuts(const char *str)
{
    while (*str != '\0')
    {
        euxcOSTPutc(*str++);
    }
}

///  euxcOSFlush - flush the terminal input buffer
void euxcOSFlush()
{
    lindex = lcount = lposition = 0;
}

///  euxcOSCheck - Check for control characters during execution
void euxcOSCheck()
{
    if (ctrl_c)
    {
        ctrl_c = 0;
        osFlushNl();
        euxcTopLevel();
    }
}

///  euxcOSCheckInt - Check for control characters during interruption
void euxcOSCheckInt()
{
    if (ctrl_c)
    {
        ctrl_c = 0;
        osFlushNl();
        euxcTopLevelInt();
    }
}

///  osFlushNl - flush the input line buffer and start a new line
static void osFlushNl()
{
    euxcOSFlush();
    euxcOSTPutc('\n');
}

void euxcSetTicksPerSecond()
{
    euxmSetValue
    (
        euxmInternAndExport("ticks-per-second"), euxcMakeFPI(sysconf(_SC_CLK_TCK))
    );
}

#define TIMES_SIZE 3
#define TIMES_REAL(x) x->value.vector.data[0]
#define TIMES_USER(x) x->value.vector.data[1]
#define TIMES_SYS(x) x->value.vector.data[2]

euxlValue euxlCpuTime()
{
    static struct tms buffer;

    euxlValue res = euxcNewVector(TIMES_SIZE);

    euxmFPIType r = (euxmFPIType)times(&buffer);
    if (r == -1)
    {
        return euxmNil;
    }

    euxmFPIType u = (euxmFPIType)buffer.tms_utime;
    euxmFPIType s = (euxmFPIType)buffer.tms_stime;

    TIMES_REAL(res) = euxcMakeFPI(r);
    TIMES_USER(res) = euxcMakeFPI(u);
    TIMES_SYS(res) = euxcMakeFPI(s);

    return res;
}

///  euxlSystem - execute a system command
euxlValue euxlSystem()
{
    static char *functionName = "system";
    char *cmd;

    cmd = euxmGetString(euxmGetArgString());
    euxmLastArg();

    return (system(cmd) == 0 ? euxs_t : euxcMakeFPI((euxmFPIType) errno));
}

///  tmpfile - open a temporary file
euxlValue euxlTmpFile()
{
    static char *functionName = "tmpfile";

    euxmLastArg();

    // tmpfile doesn't seem to work on dos gcc
    FILE *fp = tmpfile();
    if (fp == NULL)
    {
        euxcCerror
        (
            "failed to create temporary file",
            euxcMakeString(functionName),
            euxmNil
        );
    }

    return euxcMakeStream(fp, euxmPortFlagInput | euxmPortFlagOutput);
}

///  euxlGetenv - getenv
euxlValue euxlGetenv()
{
    static char *functionName = "getenv";

    euxlValue arg = euxmGetArgString();
    euxmLastArg();

    char *str = getenv(euxmGetString(arg));
    if (str == NULL)
    {
        return euxmNil;
    }

    return euxcMakeString(str);
}

///  euxlPutenv - (putenv name val)
//    some contortions to keep a chunk of string memory
euxlValue euxlPutenv()
{
    static char *functionName = "putenv";

    euxlValue name = euxmGetArgString();
    euxlValue val = euxmGetArgString();
    euxmLastArg();

    char buf[1024];
    sprintf(buf, "%s=%s", euxmGetString(name), euxmGetString(val));
    euxlValue new = euxcMakeString(buf);

    int retval = putenv(euxmGetString(new));
    if (retval > 0)
    {
        return euxmNil;
    }

    euxmStackCheck(3);
    euxmStackPush(new);

    euxlValue env = euxmGetValue(euxls_supplied_env);
    euxmStackPush(env);
    euxmStackPush(name);
    euxcArgC = 2;
    euxlValue old = euxlAssoc();

    if (old)
    {
        euxmSetCdr(old, new);
    }
    else
    {
        env = euxcCons(euxcCons(name, new), env);
        euxmSetValue(euxls_supplied_env, env);
    }

    euxmStackDrop(1);
    return new;
}

///-----------------------------------------------------------------------------
/// Dynamic library support
///-----------------------------------------------------------------------------

static void **dlList = NULL;
static int dlListUsed = 0;
static int dlListSize = 0;

///  eulcLoadDl
void eulcLoadDl(const char* soname)
{
    void* libPtr = dlopen(soname, RTLD_NOW);

    if (!libPtr)
    {
        fputs(dlerror(), stderr);
        fputs("\n", stderr);
    }

    for (int libi = 0; libi < dlListUsed; libi++)
    {
        if (dlList[libi] == libPtr)
        {
            dlclose(libPtr);
            return;
        }
    }

    if (dlListUsed >= dlListSize)
    {
        int newSize = dlListUsed + 64;
        void **newDlList = realloc(dlList, newSize);
        if (!newDlList) return;
        dlListSize = newSize;
        dlList = newDlList;
    }

    // Store dl handle for euxcCloseDls
    dlList[dlListUsed++] = libPtr;
}

///  euxcCloseDls
void euxcCloseDls()
{
    for (int libi = dlListUsed - 1; libi >= 0; libi--)
    {
        void *libPtr = dlList[libi];
        if (!libPtr) continue;
        dlclose(libPtr);
    }

    free(dlList);
    dlList = NULL;
    dlListUsed = dlListSize = 0;
}

///-----------------------------------------------------------------------------
