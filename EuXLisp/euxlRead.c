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
/// Title: Input functions
///  Maintainer: Henry G. Weller
///-----------------------------------------------------------------------------
#include "euxlisp.h"

///-----------------------------------------------------------------------------
/// Forward declarations
///-----------------------------------------------------------------------------
static void readCdr(euxlValue fptr, euxlValue last);
static void readComment(euxlValue fptr);
static euxlValue readList(euxlValue fptr);
static euxlValue readVector(euxlValue fptr);
static euxlValue readComma(euxlValue fptr);
static euxlValue readQuote(euxlValue fptr, const char *sym);
static euxlValue readString(euxlValue fptr);
static euxlValue readSpecial(euxlValue fptr);
static euxlValue readRadix(euxlValue fptr, int radix);
static euxlValue readWithRadix(euxlValue fptr, euxmFPIType radix);
static int isRadixDigit(int ch, int radix);
static int getDigit(int ch);
static int scan(euxlValue fptr);
static int stackCheckEOF(euxlValue fptr);
static int isSymbol(int ch);
static int getSymbol(euxlValue fptr, char *buf);
static euxlValue readSymbolOrNumber(euxlValue fptr);
static euxlValue readSymbol(euxlValue fptr, int ch);
static void readUnescaped
(
    euxlValue fptr,
    int ch,
    char *buf,
    int index,
    int *keyword
);
static void readEscaped(euxlValue fptr, char *buf, int index, int *keyword);
static euxlValue readNumber(euxlValue fptr, int ch, char *buf, int index);
static euxlValue readPeculiar(euxlValue fptr, int ch);

///-----------------------------------------------------------------------------
/// Functions
///-----------------------------------------------------------------------------
///  euxcRead - read an expression
int euxcRead(euxlValue fptr, euxlValue * pval)
{
    // Check the next non-blank character
    int ch;
    while ((ch = scan(fptr)) != EOF)
    {
        switch (ch)
        {
            case '(':
                *pval = readList(fptr);
                return (euxmTrue);
            case ')':
                euxcFail("misplaced right paren", euxls_syntax_error);
            case '\'':
                *pval = readQuote(fptr, "quote");
                return (euxmTrue);
            case '`':
                *pval = readQuote(fptr, "quasiquote");
                return (euxmTrue);
            case ',':
                *pval = readComma(fptr);
                return (euxmTrue);
            case '"':
                *pval = readString(fptr);
                return (euxmTrue);
            case '#':
                ch = stackCheckEOF(fptr);
                euxcUngetc(fptr, ch);
                *pval = readSpecial(fptr);
                return (euxmTrue);
            case ';':
                readComment(fptr);
                break;
            default:
                euxcUngetc(fptr, ch);
                *pval = readSymbolOrNumber(fptr);
                return (euxmTrue);
        }
    }
    return (euxmFalse);
}

///  readList - read a list
static euxlValue readList(euxlValue fptr)
{
    euxmStackCheckPush(euxmNil);
    euxlValue last = euxmNil;
    int ch;
    while ((ch = scan(fptr)) != EOF)
    {
        switch (ch)
        {
            case ';':
                readComment(fptr);
                break;
            case '#':
                {
                    ch = stackCheckEOF(fptr);
                    euxcUngetc(fptr, ch);
                    euxlValue val = readSpecial(fptr);
                    val = euxcCons(val, euxmNil);
                    if (last)
                    {
                        euxmSetCdr(last, val);
                    }
                    else
                    {
                        euxmSetStackTop(val);
                    }
                    last = val;
                }
                break;
            case ')':
                return (euxmStackPop());
            default:
                {
                    euxcUngetc(fptr, ch);
                    euxlValue val;
                    if (!euxcRead(fptr, &val))
                    {
                        euxcFail("unexpected EOF", euxls_syntax_error);
                    }
                    if (val == euxmEnter("."))
                    {
                        if (last == euxmNil)
                        {
                            euxcFail("misplaced dot", euxls_syntax_error);
                        }
                        readCdr(fptr, last);
                        return (euxmStackPop());
                    }
                    else
                    {
                        val = euxcCons(val, euxmNil);
                        if (last)
                        {
                            euxmSetCdr(last, val);
                        }
                        else
                        {
                            euxmSetStackTop(val);
                        }
                        last = val;
                    }
                }
                break;
        }
    }

    euxcFail("unexpected EOF", euxls_syntax_error);
    return (euxmNil);       // never reached
}

///  readCdr - read the euxmCdr of a dotted pair
static void readCdr(euxlValue fptr, euxlValue last)
{
    // read the euxmCdr expression
    euxlValue val;
    if (!euxcRead(fptr, &val))
    {
        euxcFail("unexpected EOF", euxls_syntax_error);
    }
    euxmSetCdr(last, val);

    // Check for the close paren
    int ch;
    while (1)
    {
        ch = scan(fptr);
        if (ch == ';')
        {
            readComment(fptr);
        }
        else if (ch == '#')
        {
            ch = stackCheckEOF(fptr);
            euxcUngetc(fptr, ch);
            ch = '#';
            break;
        }
        else
        {
            break;
        }
    }

    if (ch != ')')
    {
        euxcFail("missing right paren", euxls_syntax_error);
    }
}

///  readComment - read a comment (to end of line)
static void readComment(euxlValue fptr)
{
    int ch;
    while ((ch = euxcGetc(fptr)) != EOF && ch != '\n');
    if (ch != EOF)
    {
        euxcUngetc(fptr, ch);
    }
}

///  readVector - read a vector
static euxlValue readVector(euxlValue fptr)
{
    int len = 0, i;

    euxmStackCheckPush(euxmNil);
    euxlValue last = euxmNil;
    int ch;
    while ((ch = scan(fptr)) != EOF)
    {
        switch (ch)
        {
            case ';':
                readComment(fptr);
                break;
            case '#':
                {
                    ch = stackCheckEOF(fptr);
                    euxcUngetc(fptr, ch);
                    euxlValue val = readSpecial(fptr);
                    val = euxcCons(val, euxmNil);
                    if (last)
                    {
                        euxmSetCdr(last, val);
                    }
                    else
                    {
                        euxmSetStackTop(val);
                    }
                    last = val;
                    ++len;
                }
                break;
            case ')':
                {
                    euxlValue val = euxcNewVector(len);
                    for
                    (
                        last = euxmStackPop(), i = 0;
                        i < len;
                        ++i, last = euxmCdr(last)
                    )
                    {
                        euxmSetElement(val, i, euxmCar(last));
                    }
                    return (val);
                }
            default:
                {
                    euxcUngetc(fptr, ch);
                    euxlValue val;
                    if (!euxcRead(fptr, &val))
                    {
                        euxcFail("unexpected EOF", euxls_syntax_error);
                    }
                    val = euxcCons(val, euxmNil);
                    if (last)
                    {
                        euxmSetCdr(last, val);
                    }
                    else
                    {
                        euxmSetStackTop(val);
                    }
                    last = val;
                    ++len;
                }
                break;
        }
    }

    euxcFail("unexpected EOF", euxls_syntax_error);

    return (euxmNil);       // never reached
}

///  readComma - read a unquote or unquote-splicing expression
static euxlValue readComma(euxlValue fptr)
{
    int ch;
    if ((ch = euxcGetc(fptr)) == '@')
    {
        return (readQuote(fptr, "unquote-splicing"));
    }
    else
    {
        euxcUngetc(fptr, ch);
        return (readQuote(fptr, "unquote"));
    }
}

///  readQuote - parse the tail of a quoted expression
static euxlValue readQuote(euxlValue fptr, const char *sym)
{
    euxlValue val;
    if (!euxcRead(fptr, &val))
    {
        euxcFail("unexpected EOF", euxls_syntax_error);
    }
    euxmStackCheckPush(euxcCons(val, euxmNil));
    euxmSetStackTop(euxcCons(euxmEnter(sym), euxmStackTop()));
    return (euxmStackPop());
}

///  readString - parse a string
static euxlValue readString(euxlValue fptr)
{
    char buf[euxmVsSize + 1];
    int ch, i;

    // get symbol name
    for (i = 0; (ch = stackCheckEOF(fptr)) != '"';)
    {
        if (ch == '\\')
        {
            ch = stackCheckEOF(fptr);
            switch (ch)
            {
                case '\\':
                case '\"':
                break;
                case 'a':
                    ch = '\007';
                    break;
                case 'b':
                    ch = '\b';
                    break;
                case 'd':
                    ch = '\177';        // delete
                    break;
                case 'f':
                    ch = '\f';
                    break;
                case 'l':
                case 'n':
                ch = '\n';
                break;
                case 'r':
                    ch = '\r';
                    break;
                case 't':
                    ch = '\t';
                    break;
                case 'v':
                    ch = '\v';
                    break;
                case 'x':
                case 'X':
                {
                    int x = ch;
                    ch = stackCheckEOF(fptr);
                    int count = 0;
                    int value = 0;
                    while
                    (
                        ch != EOF
                     && isascii(ch)
                     && isxdigit(ch)
                     && count++ < 4
                    )
                    {
                        value = value * 16 +
                        (
                            ('0' <= ch && ch <= '9') ? ch - '0' :
                            ('a' <= ch && ch <= 'f') ? ch - 'a' + 10 :
                            ch - 'A' + 10
                        );
                        ch = euxcGetc(fptr);
                    }
                    euxcUngetc(fptr, ch);
                    if (count == 0)
                    {
                        ch = x; // just 'x' or 'X'
                    }
                    else
                    {
                        ch = value;
                    }
                }
                break;
                default:
                    break;
            }
        }

        if (i < euxmVsSize)
        {
            buf[i++] = ch;
        }
    }

    buf[i] = 0;

    // return a string
    return (euxcMakeString2(buf, i));
}

///  readHexChar - #\x1234
static euxlValue readHexChar(euxlValue fptr, int x)
{
    int count = 0;
    int value = 0;
    int ch = euxcGetc(fptr);

    while (ch != EOF && isascii(ch) && isxdigit(ch) && count++ < 4)
    {
        value = value * 16 +
        (
            ('0' <= ch && ch <= '9') ? ch - '0' :
            ('a' <= ch && ch <= 'f') ? ch - 'a' + 10 :
            ch - 'A' + 10
        );
        ch = euxcGetc(fptr);
    }

    euxcUngetc(fptr, ch);

    if (count == 0)
    {
        return euxcMakeChar(x);       // just #\x or #\X
    }

    return euxcMakeChar(value);
}

///  readControlChar - #\^r
static euxlValue readControlChar(euxlValue fptr)
{
    int ch = stackCheckEOF(fptr);
    if (isascii(ch))
    {
        if
        (
            isupper(ch)
         || ch == '@'
         || ch == '['
         || ch == '\\'
         || ch == ']'
         || ch == '^'
         || ch == '_'
        )
        {
            return euxcMakeChar(ch - '@');
        }
        else if
        (
            islower(ch)
         || ch == '`'
         || ch == '{'
         || ch == '|'
         || ch == '}'
         || ch == '~'
        )
        {
            return euxcMakeChar(ch - '`');
        }
    }

    euxcUngetc(fptr, ch); // just #\^ (euxmCaret)
    return euxcMakeChar('^');
}

///  readSpecial - parse an euxmAtom starting with '#'
static euxlValue readSpecial(euxlValue fptr)
{
    char buf[euxmStringMax + 1];
    int ch;
    switch (ch = stackCheckEOF(fptr))
    {
        case '\\':
            ch = stackCheckEOF(fptr);        // get the next character
            if (ch == 'x' || ch == 'X')
            {
                return readHexChar(fptr, ch);
            }
            if (ch == '^')
            {
                return readControlChar(fptr);
            }
            euxcUngetc(fptr, ch); // but allow getSymbol to get it also
            if (getSymbol(fptr, buf))
            {
                for (char *p = buf; *p; p++)
                {
                    if (isupper(*p))
                    {
                        *p = tolower(*p);
                    }
                }
                if (strcmp(buf, "\\a") == 0) // alert
                {
                    ch = '\007';        // '\a'
                }
                else if (strcmp(buf, "\\b") == 0) // backspace
                {
                    ch = '\b';
                }
                else if (strcmp(buf, "\\d") == 0) // delete
                {
                    ch = '\177';
                }
                else if (strcmp(buf, "\\f") == 0) // formfeed
                {
                    ch = '\f';
                }
                else if (strcmp(buf, "\\l") == 0) // linefeed
                {
                    ch = '\n';
                }
                else if ((strcmp(buf, "\\n") == 0)) // newline
                {
                    ch = '\n';
                }
                else if (strcmp(buf, "\\r") == 0) // return
                {
                    ch = '\r';
                }
                else if (strcmp(buf, "space") == 0)
                {
                    ch = ' ';
                }
                else if (strcmp(buf, "\\t") == 0) // tab
                {
                    ch = '\t';
                }
                else if (strcmp(buf, "\\v") == 0) // vertical-tab
                {
                    ch = '\v';
                }
                else if (strlen(buf) > 1)
                {
                    euxcCerror
                    (
                        "unexpected symbol after '#\\'",
                        euxcMakeString(buf),
                        euxls_syntax_error
                    );
                }
            }
            else        // wasn't a symbol, get the character
            {
                ch = stackCheckEOF(fptr);
            }
            return (euxcMakeChar(ch));
        case '(':
            return (readVector(fptr));
        case 'b':
        case 'B':
        return (readRadix(fptr, 2));
        case 'o':
        case 'O':
        return (readRadix(fptr, 8));
        case 'd':
        case 'D':
        return (readRadix(fptr, 10));
        case 'x':
        case 'X':
        return (readRadix(fptr, 16));
        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9':
        return (readWithRadix(fptr, ch - '0'));
        default:
            euxcUngetc(fptr, ch);
            if (getSymbol(fptr, buf))
            {
                for (char *p = buf; *p; p++)
                {
                    if (isupper(*p))
                    {
                        *p = tolower(*p);
                    }
                }
                if (strcmp(buf, "t") == 0)
                {
                    return (euxl_true);
                }
                else if (strcmp(buf, "f") == 0)
                {
                    return (euxmNil);
                }
                else
                {
                    euxcCerror
                    (
                        "unexpected symbol after '#'",
                        euxcMakeString(buf),
                        euxls_syntax_error
                    );
                }
            }
            else
            {
                euxcCerror
                (
                    "unexpected character after '#'",
                    euxcMakeChar(euxcGetc(fptr)),
                    euxls_syntax_error
                );
            }
            break;
    }

    return (euxmNil);       // never reached
}

///  readRadix - read a number in a specified radix
static euxlValue readRadix(euxlValue fptr, int radix)
{
    if (radix < 2 || radix > 36)
    {
        euxcCerror("invalid base in radix integer", euxcMakeFPI(radix),
        euxls_syntax_error);
    }

    // get symbol name
    euxmFPIType val;
    int ch;
    for (val = (euxmFPIType)0; (ch = euxcGetc(fptr)) != EOF && isSymbol(ch);)
    {
        if (islower(ch))
        {
            ch = toupper(ch);
        }

        if (!isRadixDigit(ch, radix))
        {
            char buf[64];
            sprintf(buf, "invalid digit in radix %d integer", radix);
            euxcCerror(buf, euxcMakeChar(ch), euxls_syntax_error);
        }

        val = val * radix + getDigit(ch);
    }

    // save the break character
    euxcUngetc(fptr, ch);

    // return the number
    return (euxcMakeFPI(val));
}

///  readWithRadix - integers of the form #23r42
static euxlValue readWithRadix(euxlValue fptr, euxmFPIType radix)
{
    int ch;

    for (; (ch = euxcGetc(fptr)) != EOF && ('0' <= ch && ch <= '9');)
    {
        radix = radix * 10 + getDigit(ch);
    }

    if (ch != 'r')
    {
        euxcCerror
        (
            "malformed radix integer",
            euxcMakeFPI(radix),
            euxls_syntax_error
        );
    }

    return readRadix(fptr, radix);
}

///  isRadixDigit - Check to see if a character is a digit in a radix
static int isRadixDigit(int ch, int radix)
{
    if (radix <= 10)
    {
        return (ch >= '0' && ch <= '0' + radix - 1);
    }

    return ((ch >= '0' && ch <= '9') || (ch >= 'A' && ch <= 'A' + radix - 11));
}

///  getDigit - convert an ascii code to a digit
static int getDigit(int ch)
{
    return ('0' <= ch && ch <= '9') ? ch - '0' : ch - 'A' + 10;
}

///  scan - scan for the first non-blank character
static int scan(euxlValue fptr)
{
    // look for a non-blank character
    int ch;
    while ((ch = euxcGetc(fptr)) != EOF && isspace(ch));
    return (ch);
}

///  stackCheckEOF - get a character and Check for end of file
static int stackCheckEOF(euxlValue fptr)
{
    int ch;
    if ((ch = euxcGetc(fptr)) == EOF)
    {
        euxcFail("unexpected EOF", euxls_syntax_error);
    }
    return (ch);
}

///  isSymbol - is this a symbol character?
static int isSymbol(int ch)
{
    if (!isspace(ch))
    {
        for (char *p = "()';"; *p != 0;)
        {
            if (*p++ == ch)
            {
                return (euxmFalse);
            }
        }
        return (euxmTrue);
    }

    return (euxmFalse);
}

///  getSymbol - get a symbol name
static int getSymbol(euxlValue fptr, char *buf)
{
    // get symbol name
    int ch, i;
    for (i = 0; (ch = euxcGetc(fptr)) != EOF && isSymbol(ch);)
    {
        if (i < euxmStringMax)
        {
            buf[i++] = ch;
        }
    }

    buf[i] = 0;

    // save the break character
    euxcUngetc(fptr, ch);

    return (buf[0] != 0);
}

///  readSymbolOrNumber -
static euxlValue readSymbolOrNumber(euxlValue fptr)
{
    char buf[euxmStringMax + 1];

    int ch = euxcGetc(fptr);  // can't be EOF, as just did ungetc

    if (isascii(ch))
    {
        if (isdigit(ch))
        {
            return readNumber(fptr, ch, buf, 0);
        }

        if (ch == '.' || ch == '+' || ch == '-')
        {
            return readPeculiar(fptr, ch);
        }

        return readSymbol(fptr, ch);

    }
    else
    {
        euxcCerror
        (
            "bad character in input",
            euxcMakeChar(ch),
            euxls_syntax_error
        );
    }

    return euxmNil; // not reached
}

///  readSymbol -
static euxlValue readSymbol(euxlValue fptr, int ch)
{
    if (!isprint(ch))
    {
        euxcCerror
        (
            "bad character in input",
            euxcMakeChar(ch),
            euxls_syntax_error
        );
    }

    char buf[euxmStringMax + 1];
    buf[0] = 0;

    int keyword;

    if (ch == '|')
    {
        readEscaped(fptr, buf, 0, &keyword);
    }
    else
    {
        readUnescaped(fptr, ch, buf, 0, &keyword);
    }

    if (keyword)
    {
        return euxcEnterKeyword(buf);
    }

    return euxmEnter(buf);

}

#define isconstituent(ch)                                                      \
    (                                                                          \
        isgraph(ch)                                                            \
        && ch != ' '                                                           \
        && ch != '#'                                                           \
        && ch != '('                                                           \
        && ch != ')'                                                           \
        && ch != '"'                                                           \
        && ch != '\''                                                          \
        && ch != ','                                                           \
        && ch != ';'                                                           \
        && ch != '`'                                                           \
    )

///  readUnescaped -
static void readUnescaped
(
    euxlValue fptr,
    int ch,
    char *buf,
    int index,
    int *keyword
)
{
    while (isconstituent(ch))
    {
        if (ch == '|')
        {
            readEscaped(fptr, buf, index, keyword);
            return;
        }
        if (ch == '\\')
        {
            ch = stackCheckEOF(fptr);
            buf[index++] = ch;
            *keyword = euxmFalse;
        }
        else
        {
            buf[index++] = ch;
            if (ch == ':')
            {
                *keyword = euxmTrue;
            }
            else
            {
                *keyword = euxmFalse;
            }
        }
        ch = euxcGetc(fptr);
    }

    euxcUngetc(fptr, ch); // restore the break character
    buf[index] = 0;
}

static void readEscaped(euxlValue fptr, char *buf, int index, int *keyword)
{
    *keyword = euxmFalse;

    int ch = stackCheckEOF(fptr);
    while (ch != '|')
    {
        if (ch == '\\')
        {
            ch = stackCheckEOF(fptr);
        }
        buf[index++] = ch;
        ch = stackCheckEOF(fptr);
    }

    ch = euxcGetc(fptr);
    readUnescaped(fptr, ch, buf, index, keyword);
}

///  readInteger -
static int readInteger(euxlValue fptr, int ch, char *buf, int index)
{
    while (ch != EOF && isascii(ch) && isdigit(ch))
    {
        buf[index++] = ch;
        ch = euxcGetc(fptr);
    }

    euxcUngetc(fptr, ch);
    return index;
}

///  readExponent -
static euxlValue readExponent(euxlValue fptr, char *buf, int index)
{
    buf[index++] = 'E';
    int ch = stackCheckEOF(fptr);

    if (ch == '+' || ch == '-')
    {
        buf[index++] = ch;
        ch = stackCheckEOF(fptr);
    }
    if (!isascii(ch) || !isdigit(ch))
    {
        buf[index] = 0;
        euxcUngetc(fptr, ch);
        euxcCerror("malformed floating point number", euxcMakeString(buf),
        euxls_syntax_error);
    }
    index = readInteger(fptr, ch, buf, index);

    buf[index] = 0;
    return euxcMakeDoubleFloat(atof(buf));

}

///  readPointFloat - point and at least one digit has been read
static euxlValue readPointFloat(euxlValue fptr, int ch, char *buf, int index)
{
    index = readInteger(fptr, ch, buf, index);

    ch = euxcGetc(fptr);
    if (ch == EOF)
    {
        buf[index] = 0;
        return euxcMakeDoubleFloat(atof(buf));
    }

    if (ch == 'e' || ch == 'E' || ch == 'd' || ch == 'D')
    {
        return readExponent(fptr, buf, index);
    }

    euxcUngetc(fptr, ch);
    buf[index] = 0;
    return euxcMakeDoubleFloat(atof(buf));

}

///  readNumber -
static euxlValue readNumber(euxlValue fptr, int ch, char *buf, int index)
{
    index = readInteger(fptr, ch, buf, index);

    buf[index] = 0;
    ch = euxcGetc(fptr);
    if (ch == EOF)
    {
        return euxcMakeFPI(euxmCstringToFPI(buf));
    }

    if (ch != '.' && ch != 'e' && ch != 'E' && ch != 'd' && ch != 'D')
    {
        euxcUngetc(fptr, ch);
        return euxcMakeFPI(euxmCstringToFPI(buf));
    }

    if (ch == '.')
    {
        buf[index++] = ch;
        ch = euxcGetc(fptr);
        return readPointFloat(fptr, ch, buf, index);
    }

    if (ch == 'e' || ch == 'E' || ch == 'd' || ch == 'D')
    {
        return readExponent(fptr, buf, index);
    }

    euxcUngetc(fptr, ch);
    return euxcMakeDoubleFloat(atof(buf));

}

///  readPeculiar - symbols that nearly look like numbers
///  ch is . or + or -
static euxlValue readPeculiar(euxlValue fptr, int ch)
{
    char buf[euxmStringMax + 1];
    buf[0] = ch;
    buf[1] = 0;

    int ch1 = euxcGetc(fptr);
    if (ch1 == EOF)     // . or + or -
    {
        return euxmEnter(buf);
    }

    buf[1] = ch1;
    buf[2] = 0;

    if (ch1 == '#')   // +#x1 or -#x1
    {

        if (ch == '.')       // .#
        {
            euxcUngetc(fptr, ch1);
            buf[1] = 0;
            return euxmEnter(buf);
        }

        int ch2 = euxcGetc(fptr);

        if (ch2 == EOF || !isascii(ch2))
        {
            euxcCerror
            (
                "malformed number",
                euxcMakeString(buf),
                euxls_syntax_error
            );
        }

        if
        (
            ch2 == 'x'
         || ch2 == 'X'
         || ch2 == 'o'
         || ch2 == 'O'
         || ch2 == 'b'
         || ch2 == 'B'
         || isdigit(ch2)
        )
        {
            euxcUngetc(fptr, ch2);
            euxlValue val = readSpecial(fptr);

            if (ch == '+')
            {
                return val;
            }

            return euxcMakeFPI(-euxmGetFPI(val));
        }

        char *p = &buf[2];

        while (isconstituent(ch2))
        {
            *p++ = ch2;
            ch2 = euxcGetc(fptr);
        }

        euxcUngetc(fptr, ch2);
        *p = 0;
        euxcCerror
        (
            "malformed symbol or number",
            euxcMakeString(buf),
            euxls_syntax_error
        );
    }

    if (!isconstituent(ch1))   // . or + or -
    {
        euxcUngetc(fptr, ch1);
        buf[1] = 0;
        return euxmEnter(buf);
    }

    if (isdigit(ch1))   // .1 or +1 or -1
    {
        if (ch == '.')
        {
            return readPointFloat(fptr, ch1, buf, 1);
        }
        else
        {
            return readNumber(fptr, ch1, buf, 1);
        }
    }

    if (ch == '.')   // .x
    {
        int keyword;
        readUnescaped(fptr, ch1, buf, 1, &keyword);
        if (keyword)
        {
            return euxcEnterKeyword(buf);
        }

        return euxmEnter(buf);
    }

    int ch2 = euxcGetc(fptr);

    if (ch2 == EOF)     // +x or -x
    {
        return euxmEnter(buf);
    }

    if (!isconstituent(ch2))   // +x or -x
    {
        euxcUngetc(fptr, ch2);
        return euxmEnter(buf);
    }

    if (ch1 == '.' && isdigit(ch2))     // +.1 or -.1
    {
        return readPointFloat(fptr, ch2, buf, 2);
    }

    // +xy or -xy
    int keyword;
    readUnescaped(fptr, ch2, buf, 2, &keyword);
    if (keyword)
    {
        return euxcEnterKeyword(buf);
    }

    return euxmEnter(buf);
}


///-----------------------------------------------------------------------------
