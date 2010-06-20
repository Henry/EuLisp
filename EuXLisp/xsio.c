//  Copyright (c) 1988, by David Michael Betz.
//  Copyright (c) 1994, by Russell Bradford.
//  All rights reserved.
///-----------------------------------------------------------------------------
/// ---                 EuLisp System 'EuXLisp'
///-----------------------------------------------------------------------------
///  File: xsio.c
///  Description: i/o routines
///-----------------------------------------------------------------------------

#include "xscheme.h"

// global variables
FIXTYPE xlfsize;

// external variables
extern LVAL s_stdin, s_stdout, s_stderr, s_unbound;

// xlgetc - get a character from a file or stream
int xlgetc(LVAL fptr)
{
    FILE *fp;
    int ch;

    // check for input from nil
    if (fptr == NIL)
    {
        ch = EOF;
    }   // otherwise, check for a buffered character
    else if ((ch = getsavech(fptr)) != '\0')
    {
        setsavech(fptr, '\0');
    }   // otherwise, check for terminal input or file input
    else
    {
        fp = getfile(fptr);
        if (fp == NULL)
        {
            xlcerror("attempt to read from a closed stream", fptr, NIL);
        }

        if (fp == stdin || fp == stderr)
        {
            ch = ostgetc();
        }
        else if ((getpflags(fptr) & PF_BINARY) != 0)
        {
            ch = osbgetc(fp);
        }
        else
        {
            ch = osagetc(fp);
        }
    }

    // return the character
    return (ch);
}

// xlungetc - unget a character
void xlungetc(LVAL fptr, int ch)
{
    // check for ungetc from nil
    if (fptr == NIL)
    {}  // otherwise, it must be a file
    else
    {
        setsavech(fptr, ch);
    }
}

// xlpeekchar
int xlpeekchar(LVAL fptr)
{
    FILE *fp;
    int ch;

    if (fptr == NIL)
    {
        ch = EOF;
    }
    else if ((ch = getsavech(fptr)) == '\0')
    {
        fp = getfile(fptr);
        if (fp == NULL)
        {
            xlcerror("attempt to read from a closed stream", fptr, NIL);
        }
        ch = ospeekchar(fp);
    }

    return ch;
}

// xlputc - put a character to a file or stream
void xlputc(LVAL fptr, int ch)
{
    FILE *fp;

    // count the character
    ++xlfsize;

    // check for output to nil
    if (fptr == NIL)
    {}  // otherwise, check for terminal output or file output
    else
    {
        fp = getfile(fptr);
        if (fp == NULL)
        {
            xlcerror("attempt to write to closed stream", fptr, NIL);
        }
        if (fp == stdout || fp == stderr)
        {
            ostputc(ch);
        }
        else if ((getpflags(fptr) & PF_BINARY) != 0)
        {
            osbputc(ch, fp);
        }
        else
        {
            osaputc(ch, fp);
        }
    }
}

// xlflush - flush the input buffer
void xlflush()
{
    osflush();
}

// stdputstr - print a string to *standard-output*
void stdputstr(char *str)
{
    xlputstr(getvalue(s_stdout), str);
}

// errprint - print to *error-output*
void errprint(LVAL expr)
{
    xlprin1(expr, getvalue(s_stderr));
    xlterpri(getvalue(s_stderr));
}

// errprin - prin to *error-output*
void errprin(LVAL expr)
{
    xlprin1(expr, getvalue(s_stderr));
}

// errputstr - print a string to *error-output*
void errputstr(char *str)
{
    xlputstr(getvalue(s_stderr), str);
}


///-----------------------------------------------------------------------------
