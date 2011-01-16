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
/// Title: I/O functions
///  Maintainer: Henry G. Weller
///-----------------------------------------------------------------------------
#include "euxlisp.h"

///-----------------------------------------------------------------------------
/// Functions
///-----------------------------------------------------------------------------
///  euxcGetc - get a character from a file or stream
int euxcGetc(euxlValue fptr)
{
    int ch;

    // Check for input from nil
    if (fptr == euxmNil)
    {
        ch = EOF;
    }
    // otherwise, Check for a buffered character
    else if ((ch = euxmGetSaveChar(fptr)) != '\0')
    {
        euxmSetSaveChar(fptr, '\0');
    }
    // otherwise, Check for terminal input or file input
    else
    {
        FILE *fp = euxmGetFile(fptr);

        if (fp == NULL)
        {
            euxcCerror("attempt to read from a closed stream", fptr, euxmNil);
        }

        if (fp == stdin || fp == stderr)
        {
            ch = euxcOSTGetc();
        }
        else if ((euxmGetPFlags(fptr) & euxmPortFlagBinary) != 0)
        {
            ch = euxcOSBGetc(fp);
        }
        else
        {
            ch = euxcOSAGetc(fp);
        }
    }

    // return the character
    return (ch);
}

///  euxcUngetc - unget a character
void euxcUngetc(euxlValue fptr, int ch)
{
    // Check for ungetc from nil
    if (fptr == euxmNil)
    {}
    // otherwise, it must be a file
    else
    {
        euxmSetSaveChar(fptr, ch);
    }
}

///  euxcPeekChar
int euxcPeekChar(euxlValue fptr)
{
    int ch;
    if (fptr == euxmNil)
    {
        ch = EOF;
    }
    else if ((ch = euxmGetSaveChar(fptr)) == '\0')
    {
        FILE *fp = euxmGetFile(fptr);
        if (fp == NULL)
        {
            euxcCerror("attempt to read from a closed stream", fptr, euxmNil);
        }
        ch = euxcOSPeekChar(fp);
    }

    return ch;
}

///  euxcPutc - put a character to a file or stream
void euxcPutc(euxlValue fptr, int ch)
{
    // Check for output to nil
    if (fptr == euxmNil)
    {}
    // otherwise, Check for terminal output or file output
    else
    {
        FILE *fp = euxmGetFile(fptr);

        if (fp == NULL)
        {
            euxcCerror("attempt to write to closed stream", fptr, euxmNil);
        }
        if (fp == stdout || fp == stderr)
        {
            euxcOSTPutc(ch);
        }
        else if ((euxmGetPFlags(fptr) & euxmPortFlagBinary) != 0)
        {
            euxcOSBPutc(ch, fp);
        }
        else
        {
            euxcOSAPutc(ch, fp);
        }
    }
}

///  euxcFlush - flush the input buffer
void euxcFlush()
{
    euxcOSFlush();
}

///  euxcStdPutString - print a string to *standard-output*
void euxcStdPutString(const char *str)
{
    euxcPutString(euxmGetValue(euxls_stdout), str);
}

///  euxcErrorPrint - print to *error-output*
void euxcErrorPrint(euxlValue expr)
{
    euxcPrin1(expr, euxmGetValue(euxls_stderr));
    euxcTerpri(euxmGetValue(euxls_stderr));
}

///  euxcErrorPrin - prin to *error-output*
void euxcErrorPrin(euxlValue expr)
{
    euxcPrin1(expr, euxmGetValue(euxls_stderr));
}

///  euxcErrorPutString - print a string to *error-output*
void euxcErrorPutString(const char *str)
{
    euxcPutString(euxmGetValue(euxls_stderr), str);
}


///-----------------------------------------------------------------------------
