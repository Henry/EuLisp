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
/// Title: I/O functions
///  Maintainer: Henry G. Weller
///-----------------------------------------------------------------------------
#include "euxlisp.h"

///-----------------------------------------------------------------------------
/// Global variables
///-----------------------------------------------------------------------------
FIXTYPE xlfsize;

///-----------------------------------------------------------------------------
/// External variables
///-----------------------------------------------------------------------------
extern LVAL s_stdin, s_stdout, s_stderr, s_unbound;

///-----------------------------------------------------------------------------
/// Functions
///-----------------------------------------------------------------------------
// xlgetc - get a character from a file or stream
int xlgetc(LVAL fptr)
{
    int ch;

    // check for input from nil
    if (fptr == NIL)
    {
        ch = EOF;
    }
    // otherwise, check for a buffered character
    else if ((ch = getsavech(fptr)) != '\0')
    {
        setsavech(fptr, '\0');
    }
    // otherwise, check for terminal input or file input
    else
    {
        FILE *fp = getfile(fptr);

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
    {}
    // otherwise, it must be a file
    else
    {
        setsavech(fptr, ch);
    }
}

// xlpeekchar
int xlpeekchar(LVAL fptr)
{
    int ch;
    if (fptr == NIL)
    {
        ch = EOF;
    }
    else if ((ch = getsavech(fptr)) == '\0')
    {
        FILE *fp = getfile(fptr);
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
    // count the character
    ++xlfsize;

    // check for output to nil
    if (fptr == NIL)
    {}
    // otherwise, check for terminal output or file output
    else
    {
        FILE *fp = getfile(fptr);

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
