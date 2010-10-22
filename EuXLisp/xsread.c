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
/// Title: Input functions
///  Maintainer: Henry G. Weller
///-----------------------------------------------------------------------------

#include "xscheme.h"

///-----------------------------------------------------------------------------
/// External variables
///-----------------------------------------------------------------------------
extern LVAL true;

// external functions
extern double atof();
extern ITYPE;
extern LVAL cvstring2(), s_syntax_error, s_unbound;
extern LVAL xlenter_keyword();

///-----------------------------------------------------------------------------
/// Forward declarations
///-----------------------------------------------------------------------------
static void read_cdr(LVAL fptr, LVAL last);
static void read_comment(LVAL fptr);
static LVAL read_list(LVAL fptr);
static LVAL read_vector(LVAL fptr);
static LVAL read_comma(LVAL fptr);
static LVAL read_quote(LVAL fptr, char *sym);
static LVAL read_string(LVAL fptr);
static LVAL read_special(LVAL fptr);
static LVAL read_radix(LVAL fptr, int radix);
static LVAL read_with_radix(LVAL fptr, FIXTYPE radix);
static int isradixdigit(int ch, int radix);
static int getdigit(int ch);
static int scan(LVAL fptr);
static int checkeof(LVAL fptr);
static int issym(int ch);
static int getsymbol(LVAL fptr, char *buf);
static LVAL read_symbol_or_number(LVAL fptr), read_symbol(LVAL fptr, int ch);
static void read_unescaped(LVAL fptr, int ch, char *buf, int index,
int *keyword);
static void read_escaped(LVAL fptr, char *buf, int index, int *keyword);
static LVAL read_number(LVAL fptr, int ch, char *buf, int index);
static LVAL read_peculiar(LVAL fptr, int ch);

///-----------------------------------------------------------------------------
/// Functions
///-----------------------------------------------------------------------------
// xlread - read an expression
int xlread(LVAL fptr, LVAL * pval)
{
    // check the next non-blank character
    int ch;
    while ((ch = scan(fptr)) != EOF)
    {
        switch (ch)
        {
            case '(':
                *pval = read_list(fptr);
                return (TRUE);
            case ')':
                xlfail("misplaced right paren", s_syntax_error);
            case '\'':
                *pval = read_quote(fptr, "quote");
                return (TRUE);
            case '`':
                *pval = read_quote(fptr, "quasiquote");
                return (TRUE);
            case ',':
                *pval = read_comma(fptr);
                return (TRUE);
            case '"':
                *pval = read_string(fptr);
                return (TRUE);
            case '#':
                ch = checkeof(fptr);
                xlungetc(fptr, ch);
                *pval = read_special(fptr);
                return (TRUE);
            case ';':
                read_comment(fptr);
                break;
            default:
                xlungetc(fptr, ch);
                *pval = read_symbol_or_number(fptr);
                return (TRUE);
        }
    }
    return (FALSE);
}

// read_list - read a list
static LVAL read_list(LVAL fptr)
{
    cpush(NIL);
    LVAL last = NIL;
    int ch;
    while ((ch = scan(fptr)) != EOF)
    {
        switch (ch)
        {
            case ';':
                read_comment(fptr);
                break;
            case '#':
                {
                    ch = checkeof(fptr);
                    xlungetc(fptr, ch);
                    LVAL val = read_special(fptr);
                    val = cons(val, NIL);
                    if (last)
                    {
                        rplacd(last, val);
                    }
                    else
                    {
                        settop(val);
                    }
                    last = val;
                }
                break;
            case ')':
                return (pop());
            default:
                {
                    xlungetc(fptr, ch);
                    LVAL val;
                    if (!xlread(fptr, &val))
                    {
                        xlfail("unexpected EOF", s_syntax_error);
                    }
                    if (val == xlenter("."))
                    {
                        if (last == NIL)
                        {
                            xlfail("misplaced dot", s_syntax_error);
                        }
                        read_cdr(fptr, last);
                        return (pop());
                    }
                    else
                    {
                        val = cons(val, NIL);
                        if (last)
                        {
                            rplacd(last, val);
                        }
                        else
                        {
                            settop(val);
                        }
                        last = val;
                    }
                }
                break;
        }
    }

    xlfail("unexpected EOF", s_syntax_error);
    return (NIL);       // never reached
}

// read_cdr - read the cdr of a dotted pair
static void read_cdr(LVAL fptr, LVAL last)
{
    // read the cdr expression
    LVAL val;
    if (!xlread(fptr, &val))
    {
        xlfail("unexpected EOF", s_syntax_error);
    }
    rplacd(last, val);

    // check for the close paren
    int ch;
    while (1)
    {
        ch = scan(fptr);
        if (ch == ';')
        {
            read_comment(fptr);
        }
        else if (ch == '#')
        {
            ch = checkeof(fptr);
            xlungetc(fptr, ch);
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
        xlfail("missing right paren", s_syntax_error);
    }
}

// read_comment - read a comment (to end of line)
static void read_comment(LVAL fptr)
{
    int ch;
    while ((ch = xlgetc(fptr)) != EOF && ch != '\n');
    if (ch != EOF)
    {
        xlungetc(fptr, ch);
    }
}

// read_vector - read a vector
static LVAL read_vector(LVAL fptr)
{
    int len = 0, i;

    cpush(NIL);
    LVAL last = NIL;
    int ch;
    while ((ch = scan(fptr)) != EOF)
    {
        switch (ch)
        {
            case ';':
                read_comment(fptr);
                break;
            case '#':
                {
                    ch = checkeof(fptr);
                    xlungetc(fptr, ch);
                    LVAL val = read_special(fptr);
                    val = cons(val, NIL);
                    if (last)
                    {
                        rplacd(last, val);
                    }
                    else
                    {
                        settop(val);
                    }
                    last = val;
                    ++len;
                }
                break;
            case ')':
                {
                    LVAL val = newvector(len);
                    for (last = pop(), i = 0; i < len; ++i, last = cdr(last))
                    {
                        setelement(val, i, car(last));
                    }
                    return (val);
                }
            default:
                {
                    xlungetc(fptr, ch);
                    LVAL val;
                    if (!xlread(fptr, &val))
                    {
                        xlfail("unexpected EOF", s_syntax_error);
                    }
                    val = cons(val, NIL);
                    if (last)
                    {
                        rplacd(last, val);
                    }
                    else
                    {
                        settop(val);
                    }
                    last = val;
                    ++len;
                }
                break;
        }
    }

    xlfail("unexpected EOF", s_syntax_error);

    return (NIL);       // never reached
}

// read_comma - read a unquote or unquote-splicing expression
static LVAL read_comma(LVAL fptr)
{
    int ch;
    if ((ch = xlgetc(fptr)) == '@')
    {
        return (read_quote(fptr, "unquote-splicing"));
    }
    else
    {
        xlungetc(fptr, ch);
        return (read_quote(fptr, "unquote"));
    }
}

// read_quote - parse the tail of a quoted expression
static LVAL read_quote(LVAL fptr, char *sym)
{
    LVAL val;
    if (!xlread(fptr, &val))
    {
        xlfail("unexpected EOF", s_syntax_error);
    }
    cpush(cons(val, NIL));
    settop(cons(xlenter(sym), top()));
    return (pop());
}

// read_string - parse a string
static LVAL read_string(LVAL fptr)
{
    char buf[VSSIZE + 1];
    int ch, i;

    // get symbol name
    for (i = 0; (ch = checkeof(fptr)) != '"';)
    {
        if (ch == '\\')
        {
            ch = checkeof(fptr);
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
                    ch = checkeof(fptr);
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
                        ch = xlgetc(fptr);
                    }
                    xlungetc(fptr, ch);
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

        if (i < VSSIZE)
        {
            buf[i++] = ch;
        }
    }

    buf[i] = 0;

    // return a string
    return (cvstring2(buf, i));
}

// read_hex_char: #\x1234
static LVAL read_hex_char(LVAL fptr, int x)
{
    int count = 0;
    int value = 0;
    int ch = xlgetc(fptr);

    while (ch != EOF && isascii(ch) && isxdigit(ch) && count++ < 4)
    {
        value = value * 16 +
        (
            ('0' <= ch && ch <= '9') ? ch - '0' :
            ('a' <= ch && ch <= 'f') ? ch - 'a' + 10 :
            ch - 'A' + 10
        );
        ch = xlgetc(fptr);
    }

    xlungetc(fptr, ch);

    if (count == 0)
    {
        return cvchar(x);       // just #\x or #\X
    }

    return cvchar(value);
}

// read_control_char: #\^r
static LVAL read_control_char(LVAL fptr)
{
    int ch = checkeof(fptr);
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
            return cvchar(ch - '@');
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
            return cvchar(ch - '`');
        }
    }

    xlungetc(fptr, ch); // just #\^ (caret)
    return cvchar('^');
}

// read_special - parse an atom starting with '#'
static LVAL read_special(LVAL fptr)
{
    char buf[STRMAX + 1];
    int ch;
    switch (ch = checkeof(fptr))
    {
        case '\\':
            ch = checkeof(fptr);        // get the next character
            if (ch == 'x' || ch == 'X')
            {
                return read_hex_char(fptr, ch);
            }
            if (ch == '^')
            {
                return read_control_char(fptr);
            }
            xlungetc(fptr, ch); // but allow getsymbol to get it also
            if (getsymbol(fptr, buf))
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
                    xlcerror("unexpected symbol after '#\\'", cvstring(buf),
                    s_syntax_error);
                }
            }
            else        // wasn't a symbol, get the character
            {
                ch = checkeof(fptr);
            }
            return (cvchar(ch));
        case '(':
            return (read_vector(fptr));
        case 'b':
        case 'B':
        return (read_radix(fptr, 2));
        case 'o':
        case 'O':
        return (read_radix(fptr, 8));
        case 'd':
        case 'D':
        return (read_radix(fptr, 10));
        case 'x':
        case 'X':
        return (read_radix(fptr, 16));
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
        return (read_with_radix(fptr, ch - '0'));
        default:
            xlungetc(fptr, ch);
            if (getsymbol(fptr, buf))
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
                    return (true);
                }
                else if (strcmp(buf, "f") == 0)
                {
                    return (NIL);
                }
                else
                {
                    xlcerror("unexpected symbol after '#'", cvstring(buf),
                    s_syntax_error);
                }
            }
            else
            {
                xlcerror("unexpected character after '#'", cvchar(xlgetc(fptr)),
                s_syntax_error);
            }
            break;
    }

    return (NIL);       // never reached
}

// read_radix - read a number in a specified radix
static LVAL read_radix(LVAL fptr, int radix)
{
    if (radix < 2 || radix > 36)
    {
        xlcerror("invalid base in radix integer", cvfixnum(radix),
        s_syntax_error);
    }

    // get symbol name
    FIXTYPE val;
    int ch;
    for (val = (FIXTYPE)0; (ch = xlgetc(fptr)) != EOF && issym(ch);)
    {
        if (islower(ch))
        {
            ch = toupper(ch);
        }

        if (!isradixdigit(ch, radix))
        {
            char buf[64];
            sprintf(buf, "invalid digit in radix %d integer", radix);
            xlcerror(buf, cvchar(ch), s_syntax_error);
        }

        val = val * radix + getdigit(ch);
    }

    // save the break character
    xlungetc(fptr, ch);

    // return the number
    return (cvfixnum(val));
}

// integers of the form #23r42
static LVAL read_with_radix(LVAL fptr, FIXTYPE radix)
{
    int ch;

    for (; (ch = xlgetc(fptr)) != EOF && ('0' <= ch && ch <= '9');)
    {
        radix = radix * 10 + getdigit(ch);
    }

    if (ch != 'r')
    {
        xlcerror("malformed radix integer", cvfixnum(radix), s_syntax_error);
    }

    return read_radix(fptr, radix);
}

// isradixdigit - check to see if a character is a digit in a radix
static int isradixdigit(int ch, int radix)
{
    if (radix <= 10)
    {
        return (ch >= '0' && ch <= '0' + radix - 1);
    }

    return ((ch >= '0' && ch <= '9') || (ch >= 'A' && ch <= 'A' + radix - 11));
}

// getdigit - convert an ascii code to a digit
static int getdigit(int ch)
{
    return ('0' <= ch && ch <= '9') ? ch - '0' : ch - 'A' + 10;
}

// scan - scan for the first non-blank character
static int scan(LVAL fptr)
{
    // look for a non-blank character
    int ch;
    while ((ch = xlgetc(fptr)) != EOF && isspace(ch));
    return (ch);
}

// checkeof - get a character and check for end of file
static int checkeof(LVAL fptr)
{
    int ch;
    if ((ch = xlgetc(fptr)) == EOF)
    {
        xlfail("unexpected EOF", s_syntax_error);
    }
    return (ch);
}

// issym - is this a symbol character?
static int issym(int ch)
{
    if (!isspace(ch))
    {
        for (char *p = "()';"; *p != 0;)
        {
            if (*p++ == ch)
            {
                return (FALSE);
            }
        }
        return (TRUE);
    }

    return (FALSE);
}

// getsymbol - get a symbol name
static int getsymbol(LVAL fptr, char *buf)
{
    // get symbol name
    int ch, i;
    for (i = 0; (ch = xlgetc(fptr)) != EOF && issym(ch);)
    {
        if (i < STRMAX)
        {
            buf[i++] = ch;
        }
    }

    buf[i] = 0;

    // save the break character
    xlungetc(fptr, ch);

    return (buf[0] != 0);
}

static LVAL read_symbol_or_number(LVAL fptr)
{
    char buf[STRMAX + 1];

    int ch = xlgetc(fptr);  // can't be EOF, as just did ungetc

    if (isascii(ch))
    {
        if (isdigit(ch))
        {
            return read_number(fptr, ch, buf, 0);
        }

        if (ch == '.' || ch == '+' || ch == '-')
        {
            return read_peculiar(fptr, ch);
        }

        return read_symbol(fptr, ch);

    }
    else
    {
        xlcerror("bad character in input", cvchar(ch), s_syntax_error);
    }

    return NIL; // not reached
}

static LVAL read_symbol(LVAL fptr, int ch)
{
    if (!isprint(ch))
    {
        xlcerror("bad character in input", cvchar(ch), s_syntax_error);
    }

    char buf[STRMAX + 1];
    buf[0] = 0;

    int keyword;

    if (ch == '|')
    {
        read_escaped(fptr, buf, 0, &keyword);
    }
    else
    {
        read_unescaped(fptr, ch, buf, 0, &keyword);
    }

    if (keyword)
    {
        return xlenter_keyword(buf);
    }

    return xlenter(buf);

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

static void
read_unescaped(LVAL fptr, int ch, char *buf, int index, int *keyword)
{
    while (isconstituent(ch))
    {
        if (ch == '|')
        {
            read_escaped(fptr, buf, index, keyword);
            return;
        }
        if (ch == '\\')
        {
            ch = checkeof(fptr);
            buf[index++] = ch;
            *keyword = FALSE;
        }
        else
        {
            buf[index++] = ch;
            if (ch == ':')
            {
                *keyword = TRUE;
            }
            else
            {
                *keyword = FALSE;
            }
        }
        ch = xlgetc(fptr);
    }

    xlungetc(fptr, ch); // restore the break character
    buf[index] = 0;
}

static void read_escaped(LVAL fptr, char *buf, int index, int *keyword)
{
    *keyword = FALSE;

    int ch = checkeof(fptr);
    while (ch != '|')
    {
        if (ch == '\\')
        {
            ch = checkeof(fptr);
        }
        buf[index++] = ch;
        ch = checkeof(fptr);
    }

    ch = xlgetc(fptr);
    read_unescaped(fptr, ch, buf, index, keyword);
}

static int read_integer(LVAL fptr, int ch, char *buf, int index)
{
    while (ch != EOF && isascii(ch) && isdigit(ch))
    {
        buf[index++] = ch;
        ch = xlgetc(fptr);
    }

    xlungetc(fptr, ch);
    return index;
}

static LVAL read_exponent(LVAL fptr, char *buf, int index)
{
    buf[index++] = 'E';
    int ch = checkeof(fptr);

    if (ch == '+' || ch == '-')
    {
        buf[index++] = ch;
        ch = checkeof(fptr);
    }
    if (!isascii(ch) || !isdigit(ch))
    {
        buf[index] = 0;
        xlungetc(fptr, ch);
        xlcerror("malformed floating point number", cvstring(buf),
        s_syntax_error);
    }
    index = read_integer(fptr, ch, buf, index);

    buf[index] = 0;
    return cvflonum(atof(buf));

}

// point and at least one digit has been read
static LVAL read_point_float(LVAL fptr, int ch, char *buf, int index)
{
    index = read_integer(fptr, ch, buf, index);

    ch = xlgetc(fptr);
    if (ch == EOF)
    {
        buf[index] = 0;
        return cvflonum(atof(buf));
    }

    if (ch == 'e' || ch == 'E' || ch == 'd' || ch == 'D')
    {
        return read_exponent(fptr, buf, index);
    }

    xlungetc(fptr, ch);
    buf[index] = 0;
    return cvflonum(atof(buf));

}

static LVAL read_number(LVAL fptr, int ch, char *buf, int index)
{
    index = read_integer(fptr, ch, buf, index);

    buf[index] = 0;
    ch = xlgetc(fptr);
    if (ch == EOF)
    {
        return cvfixnum(ICNV(buf));
    }

    if (ch != '.' && ch != 'e' && ch != 'E' && ch != 'd' && ch != 'D')
    {
        xlungetc(fptr, ch);
        return cvfixnum(ICNV(buf));
    }

    if (ch == '.')
    {
        buf[index++] = ch;
        ch = xlgetc(fptr);
        return read_point_float(fptr, ch, buf, index);
    }

    if (ch == 'e' || ch == 'E' || ch == 'd' || ch == 'D')
    {
        return read_exponent(fptr, buf, index);
    }

    xlungetc(fptr, ch);
    return cvflonum(atof(buf));

}

// symbols that nearly look like numbers
// ch is . or + or -
static LVAL read_peculiar(LVAL fptr, int ch)
{
    char buf[STRMAX + 1];
    buf[0] = ch;
    buf[1] = 0;

    int ch1 = xlgetc(fptr);
    if (ch1 == EOF)     // . or + or -
    {
        return xlenter(buf);
    }

    buf[1] = ch1;
    buf[2] = 0;

    if (ch1 == '#')   // +#x1 or -#x1
    {

        if (ch == '.')       // .#
        {
            xlungetc(fptr, ch1);
            buf[1] = 0;
            return xlenter(buf);
        }

        int ch2 = xlgetc(fptr);

        if (ch2 == EOF || !isascii(ch2))
        {
            xlcerror("malformed number", cvstring(buf), s_syntax_error);
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
            xlungetc(fptr, ch2);
            LVAL val = read_special(fptr);

            if (ch == '+')
            {
                return val;
            }

            return cvfixnum(-getfixnum(val));
        }

        char *p = &buf[2];

        while (isconstituent(ch2))
        {
            *p++ = ch2;
            ch2 = xlgetc(fptr);
        }

        xlungetc(fptr, ch2);
        *p = 0;
        xlcerror("malformed symbol or number", cvstring(buf), s_syntax_error);
    }

    if (!isconstituent(ch1))   // . or + or -
    {
        xlungetc(fptr, ch1);
        buf[1] = 0;
        return xlenter(buf);
    }

    if (isdigit(ch1))   // .1 or +1 or -1
    {
        if (ch == '.')
        {
            return read_point_float(fptr, ch1, buf, 1);
        }
        else
        {
            return read_number(fptr, ch1, buf, 1);
        }
    }

    if (ch == '.')   // .x
    {
        int keyword;
        read_unescaped(fptr, ch1, buf, 1, &keyword);
        if (keyword)
        {
            return xlenter_keyword(buf);
        }

        return xlenter(buf);
    }

    int ch2 = xlgetc(fptr);

    if (ch2 == EOF)     // +x or -x
    {
        return xlenter(buf);
    }

    if (!isconstituent(ch2))   // +x or -x
    {
        xlungetc(fptr, ch2);
        return xlenter(buf);
    }

    if (ch1 == '.' && isdigit(ch2))     // +.1 or -.1
    {
        return read_point_float(fptr, ch2, buf, 2);
    }

    // +xy or -xy
    int keyword;
    read_unescaped(fptr, ch2, buf, 2, &keyword);
    if (keyword)
    {
        return xlenter_keyword(buf);
    }

    return xlenter(buf);
}


///-----------------------------------------------------------------------------
