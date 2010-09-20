/// Copyright 1997 A. Kind & University of Bath
/// Copyright 2010 Henry G. Weller
///-----------------------------------------------------------------------------
//  This file is part of
/// ---                         EuLisp System 'Youtoo'
///-----------------------------------------------------------------------------
//
//  Youtoo is free software: you can redistribute it and/or modify it under the
//  terms of the GNU General Public License version 2 as published by the Free
//  Software Foundation.
//
//  Youtoo is distributed in the hope that it will be useful, but WITHOUT ANY
//  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
//  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
//  details.
//
//  You should have received a copy of the GNU General Public License along with
//  this program.  If not, see <http://www.gnu.org/licenses/>.
//
///-----------------------------------------------------------------------------
///  Library: level1, eval, youtoo
///  Authors: Julian Padget, Andreas Kind
///  Description: tokenizer
///-----------------------------------------------------------------------------

#include "eulisp.h"
#include <limits.h>
#include "eul-ext.h"
#include "../Runtime/stream2.h"

#ifdef WITH_PCR_THREADS
    // See alse Vm/stdc.h
#   define read PCR_read
#endif

#define BUFFER_SIZE 1024

// Character constants
#define STRING_START '"'
#define STRING_STOP '"'
#define STRING_ESCAPE '\\'
#define HEX_INSERTION 'x'
#define N_HEX_DIGITS 4
#define EXTENSION_MARK '#'
#define SHEBANG '!'
#define LIST_START '('
#define LIST_STOP ')'
#define CHARACTER_START '\\'
#define SINGLE_ESCAPE '\\'
#define MULTIPLE_ESCAPE_START '|'
#define MULTIPLE_ESCAPE_STOP '|'
#define QUOTE '\''
#define QUASIQUOTE '`'
#define COMMA ','
#define DECIMAL_POINT '.'
#define DOT '.'
#define MINUS_SIGN '-'
#define PLUS_SIGN '+'
#define COMMENT_START ';'
#define AT_SIGN '@'

// Integer formats
#define BINARY_DIGIT_P(c) check_base_digit_p(c, 2)
#define OCTAL_DIGIT_P(c) check_base_digit_p(c, 8)
#define BINARY_START_P(c) ((*c=='b')||(*c=='B'))
#define OCTAL_START_P(c) ((*c=='o')||(*c=='O'))
#define HEX_START_P(c) ((*c=='x')||(*c=='X'))
#define EXPONENT_MARK_P(c) ((*c=='e')||(*c=='E'))
#define RADIX_START_P(c) ((*c=='r')||(*c=='R'))
#define SPECIAL_P(c) (memchr(";'\"()`", *c, 6))
#define HEX_DIGIT_P(c) (isxdigit(c))
#define DECIMAL_DIGIT_P(c) (isdigit(c))
#define BREAK_P(c) (isspace(*c)||SPECIAL_P(c))
#define NORMAL_INITIAL_P(c) \
    (isalpha(*c) || *c == SINGLE_ESCAPE || *c == MULTIPLE_ESCAPE_START)

// Input buffer interface

// Declare fill-buffer so C can call Lisp
EUL_DEFINTERN(fill_buffer, "fill-buffer", 1, stream2)
#define EOB (c>=maxc)
#define EOB_LOOKAHEAD(n) ((c+n)>=maxc)
#define FILL_BUFFER() (eul_int_as_c_int(fill_buffer(stream)))
#define BUFFER_RESET() (c = buffer, c[n] = (char)0, maxc = c+n)

/*
  #define SHOW_STATE(x) \
  fprintf(stdout, "%s: c=%x, n=%d end=%x, siding=%s, buffer=%s\n", \
  x, c, n, n+buffer, (tokstart<buffer?tokstart:""), buffer)
*/
#define SHOW_STATE(x) {}

#define UPDATE_FCB_POS(y)                                                      \
    {                                                                          \
        CONTROL_BLOCK_BUFFER_POS(fcb) = c_int_as_eul_int(c-buffer);            \
        return y;                                                              \
    }

#define RETURN_CHARACTER(x) UPDATE_FCB_POS(c_char_as_eul_char(x))
#define RETURN_FPI(x) UPDATE_FCB_POS(c_int_as_eul_int(x))
#define RETURN_DOUBLE(x) UPDATE_FCB_POS(c_double_as_eul_double(x))
#define RETURN_STRING(x, s) UPDATE_FCB_POS(c_strn_as_eul_str(x, s))
#define RETURN_NIL() UPDATE_FCB_POS(eul_nil)
#define RETURN_SYMBOL(x, s) \
    UPDATE_FCB_POS(c_strn_as_eul_symbol_or_keyword(x, s))
#define RETURN_SPECIAL(x) UPDATE_FCB_POS(slot_ref(special_tokens, SPECIAL_##x))
#define RETURN_REF(x) UPDATE_FCB_POS(x)
#define SPECIAL_LIST_START    0
#define SPECIAL_LIST_STOP     1
#define SPECIAL_VECTOR_START  2
#define SPECIAL_VECTOR_STOP   3
#define SPECIAL_QUOTE         4
#define SPECIAL_QUASIQUOTE    5
#define SPECIAL_COMMA         6
#define SPECIAL_COMMA_AT_SIGN 7
#define SPECIAL_DOT           8
#define SPECIAL_OBJECT_COMMENT_START  9
#define SPECIAL_EOF           10

// Stuff for parsing integers in various bases
#define BINARY 2
#define OCTAL 8
#define DECIMAL 0
#define HEX 16
#define fpibase_DIGIT_P(c) check_base_digit_p(c, fpibase)

#define GET_INTEGER(baze, result)                                              \
    tokstart = c;                                                              \
    while (baze##_DIGIT_P(*c))                                                 \
    {                                                                          \
        c++;                                                                   \
        CHK_OVERFLOW("base", break);                                           \
    }                                                                          \
    result = (tokstart-c==0)?0:strtol(tokstart, &c, baze);

#define CHK_OVERFLOW(kind_of, eof_action)                                      \
    if (EOB)                                                                   \
    {                                                                          \
        n = FILL_BUFFER();                                                     \
        if (n==0) eof_action;                                                  \
        BUFFER_RESET();                                                        \
        SHOW_STATE("after " kind_of " panic");                                 \
    }

#define CHK_OVERFLOW_AND_COPY(kind_of, eof_action)                             \
    if (EOB)                                                                   \
    {                                                                          \
        if (tokstart<buffer)                                                   \
        {                                                                      \
            syntax_error(tokstart, maxc,                                       \
            "token exceeds processor limits: breaking here");                  \
            break;                                                             \
        }                                                                      \
        strcpy(buffer-(maxc-tokstart), tokstart);                              \
        tokstart = buffer-(maxc-tokstart);                                     \
        n = FILL_BUFFER();                                                     \
        if (n==0) eof_action;                                                  \
        BUFFER_RESET();                                                        \
        SHOW_STATE("after " kind_of " panic");                                 \
    }

#define HEX_CHAR_TO_DIGIT(c)                                                   \
    (isdigit(*c)?*c-'0':(isupper(*c)?*c-'A'+10:*c-'a'+10))


int check_base_digit_p(char c, int base)
{
    if (base < DECIMAL)
    {
        return (('0' <= c) && (c <= ('0' + base)));
    }

    if (isdigit(c))
    {
        return (1);
    }

    return
    (
        (('A' <= c) && (c <= ('A' + base)))
     || (('a' <= c) && (c <= ('a' + base)))
    );
}


void syntax_error(char *buffer, char *c, char *msg)
{
    fprintf(stdout, "\n*** WARNING [read]: %s: ", msg);
    fprintf(stdout, buffer);

    if ((*c - 1) != '\n')
    {
        fprintf(stdout, "\n");
    }

    for (char *p = buffer; p < c; p++)
    {
        fprintf(stdout, "-");
    }

    fprintf(stdout, "^");
}


// Stream and control block layout
#define CONTROL_BLOCK_BUFFER(x) (slot_ref(x, 0))
#define CONTROL_BLOCK_BUFFER_SIZE(x) (slot_ref(x, 1))
#define CONTROL_BLOCK_BUFFER_POS(x) (slot_ref(x, 2))
#define CONTROL_BLOCK_BUFFER_CNT(x) (slot_ref(x, 3))

#define STREAM_SINK(x) (slot_ref(x, 0))
#define STREAM_SOURCE(x) (slot_ref(x, 1))


// Macro for reading symbols with support for single and multiple escaping
// This is used 3 times in ntok
#define READ_SYMBOL                                                            \
                                                                               \
    symsize = 0;                                                               \
                                                                               \
    /* pass 1 to calculate the size */                                         \
    while (1)                                                                  \
    {                                                                          \
        symsize++;                                                             \
        if (isalnum(*c))                                                       \
        {                                                                      \
            c++;                                                               \
        }                                                                      \
        else if (isspace(*c))                                                  \
        {                                                                      \
            symsize--;                                                         \
            break;                                                             \
        }                                                                      \
        else if (*c == SINGLE_ESCAPE)                                          \
        {                                                                      \
            c++;                                                               \
            CHK_OVERFLOW_AND_COPY("symbol escape", break);                     \
            c++;                                                               \
        }                                                                      \
        else if (*c == MULTIPLE_ESCAPE_START)                                  \
        {                                                                      \
            symsize--;                                                         \
            c++;                                                               \
            while (*c != MULTIPLE_ESCAPE_STOP)                                 \
            {                                                                  \
                symsize++;                                                     \
                if (*c == SINGLE_ESCAPE)                                       \
                {                                                              \
                    c++;                                                       \
                    CHK_OVERFLOW_AND_COPY("symbol escape", break);             \
                    c++;                                                       \
                }                                                              \
                else                                                           \
                {                                                              \
                    c++;                                                       \
                }                                                              \
                CHK_OVERFLOW_AND_COPY("symbol multiple escape", break);        \
            }                                                                  \
            c++;                                                               \
        }                                                                      \
        else if (SPECIAL_P(c))                                                 \
        {                                                                      \
            symsize--;                                                         \
            break;                                                             \
        }                                                                      \
        else                                                                   \
        {                                                                      \
            c++;                                                               \
        }                                                                      \
        CHK_OVERFLOW_AND_COPY("symbol", break);                                \
    }                                                                          \
    symtok = (char *)gc_malloc(symsize + 1);                                   \
                                                                               \
    /* pass 2 to copy */                                                       \
    p = symtok;                                                                \
    i = symsize;                                                               \
    while (i > 0)                                                              \
    {                                                                          \
        if (*tokstart == SINGLE_ESCAPE)                                        \
        {                                                                      \
            *p++ = *++tokstart;                                                \
            tokstart++;                                                        \
            i--;                                                               \
        }                                                                      \
        else if                                                                \
        (                                                                      \
            *tokstart == MULTIPLE_ESCAPE_START                                 \
            || *tokstart == MULTIPLE_ESCAPE_STOP                               \
        )                                                                      \
        {                                                                      \
            tokstart++;                                                        \
        }                                                                      \
        else                                                                   \
        {                                                                      \
            *p++ = *tokstart++;                                                \
            i--;                                                               \
        }                                                                      \
    }                                                                          \
    *p = '\0';                                                                 \
                                                                               \
    RETURN_SYMBOL(symtok, symsize);                                            \
                                                                               \
    SHOW_STATE("after symbol");


LispRef ntok(LispRef stream, LispRef special_tokens)
{
    LispRef fcb = STREAM_SOURCE(stream);
    char *siding = eul_string_as_c_string(CONTROL_BLOCK_BUFFER(fcb));
    char *buffer =
    siding + (eul_int_as_c_int(CONTROL_BLOCK_BUFFER_SIZE(fcb)) >> 1);

    // ntok state variables
    char *c = buffer + eul_int_as_c_int(CONTROL_BLOCK_BUFFER_POS(fcb));
    char *maxc = buffer + eul_int_as_c_int(CONTROL_BLOCK_BUFFER_CNT(fcb));
    int negative = 0;
    int token_overflow = 0;

    // ntok temporary variables
    char *p;
    int n = 0;
    int i;
    char *tokstart = NULL;

    // variables to hold the token value
    int fpibase;
    int fpitok;
    double dbltok;
    char chartok;
    char *strtok;
    int strsize;
    char *symtok;
    int symsize;

    // fprintf(stdout, "ntok siding = %x, buffer = %x, c = %x, maxc = %x\n",
    // siding, buffer, c, maxc);

    // make sure we've got some characters to scan
    // note: no check for zero chars read at thus point...

    CHK_OVERFLOW("at start", RETURN_SPECIAL(EOF));
    SHOW_STATE("at start");

    while (1)
    {
        // ------------------------------------------------------SYMBOL
        if (NORMAL_INITIAL_P(c))
        {
            tokstart = c;
            READ_SYMBOL
        }

        // --------------------------------------------------LIST-START
        else if (*c == LIST_START)
        {
            c++;
            RETURN_SPECIAL(LIST_START);
        }

        // ----------------------------------------------------LIST-END
        else if (*c == LIST_STOP)
        {
            c++;
            RETURN_SPECIAL(LIST_STOP);
        }

        // ----------------------------------------------FPI-AND-DOUBLE
        else if (isdigit(*c))
        {
            tokstart = c;
            while (isdigit(*c))
            {
                c++;
                CHK_OVERFLOW_AND_COPY("fpi", {});
            }
            if (*c == DECIMAL_POINT)
            {
                c++;
                while (1)
                {
                    // This loop has the feature that a double followed by two
                    // bufferfuls of e's will generate a syntax error
                    if (isdigit(*c))
                    {
                        c++;
                    }
                    else if (EXPONENT_MARK_P(c))
                    {
                        c++;
                    }
                    else
                    {
                        break;
                    }

                    // eof_action is suspect
                    CHK_OVERFLOW_AND_COPY("double", break);
                }

                dbltok = strtod(tokstart, &c);
                RETURN_DOUBLE(negative ? -dbltok : dbltok);
                SHOW_STATE("after double");
            }
            else
            {
                fpitok = strtol(tokstart, &c, 0);
                if ((fpitok == LONG_MAX) || (fpitok > MAX_FPI))
                {
                    LispRef cb, eul_strtok, res;

                    cb = CALLBACK_OPERATOR(CB_READ_OVERFLOW);

                    INITIALISE_REGISTER_SET(tame_regs);
                    tame_regs->reg_value_stack->sp =
                    tame_regs->reg_value_stack->base;
                    tame_regs->reg_context_stack->sp =
                    tame_regs->reg_context_stack->base;
                    tame_regs->reg_current_cv = (Instruction *) LAMBDA_CODE(cb);
                    tame_regs->reg_pc = tame_regs->reg_current_cv;
                    tame_regs->reg_env = LAMBDA_ENV(cb);
                    tame_regs->reg_next_methods = eul_nil;

                    eul_allocate_string(eul_strtok, tokstart);
                    EXTERNAL_PUSHVAL1(eul_strtok);
                    EXTERNAL_PUSHVAL1(c_bool_as_eul_bool(negative));
                    tame_regs->reg_arg_count = 2;

                    interpret(tame_regs);

                    // We're not interested in the result
                    EXTERNAL_POPVAL1(res);
                    RETURN_REF(res);
                }
                else
                {
                    RETURN_FPI(negative ? -fpitok : fpitok);
                }
                SHOW_STATE("after fpi");
            }
        }

        // -------------------------------------------------------QUOTE
        else if (*c == QUOTE)
        {
            c++;
            RETURN_SPECIAL(QUOTE);
        }

        // ------------------------------------------------------STRING
        else if (*c == STRING_START)
        {
            strsize = 0;
            c++;

            CHK_OVERFLOW("string start", RETURN_SPECIAL(EOF));
            tokstart = c;

            // pass 1 to calculate the size
            while (*c != STRING_STOP)
            {
                strsize++;
                if (*c++ == STRING_ESCAPE)
                {
                    if (*c++ == HEX_INSERTION)
                    {
                        c = c + N_HEX_DIGITS;
                    }
                }

                if (EOB)
                {
                    if (tokstart < buffer)
                    {
                        token_overflow = 1;     // used for patch-up: see below

                        syntax_error
                        (
                            tokstart,
                            maxc,
                            "token exceeds processor limits"
                        );
                        break;
                    }
                    strcpy(buffer - (maxc - tokstart), tokstart);
                    tokstart = buffer - (maxc - tokstart);
                    n = FILL_BUFFER();
                    i = c - maxc;       // calculate overrun due to
                                        // HEX_INSERTION
                    BUFFER_RESET();
                    c = c + i;  // and adjust accordingly
                    SHOW_STATE("after string panic");
                }
            }
            c++;
            strtok = (char *)gc_malloc(strsize + 1);

            // pass 2 to interpret any insertions
            p = strtok;
            i = strsize;
            for (; i > 0; i--)
            {
                if (*tokstart != STRING_ESCAPE)
                {
                    *p++ = *tokstart++;
                }
                else
                {
                    switch (*++tokstart)
                    {
                        case 'a':
                            *p++ = '\a';
                            break;
                        case 'b':
                            *p++ = '\b';
                            break;
                        case 'd':
                            *p++ = '\x7f';
                            break;
                        case 'f':
                            *p++ = '\f';
                            break;
                        case 'l':
                            *p++ = '\x0c';
                            break;
                        case 'n':
                            *p++ = '\n';
                            break;
                        case 'r':
                            *p++ = '\r';
                            break;
                        case 't':
                            *p++ = '\t';
                            break;
                        case 'v':
                            *p++ = '\v';
                            break;
                        case 'x':
                            chartok = 0;
                            for (int i=0; i<N_HEX_DIGITS; i++)
                            {
                            tokstart++;
                            if (isxdigit(*tokstart))
                            {
                                chartok =
                                (chartok << 4) |
                                (HEX_CHAR_TO_DIGIT(tokstart));
                            }
                            else
                            {
                                syntax_error(buffer, tokstart,
                                "invalid hex insertion digit");
                            }
                            }
                            *p++ = chartok;
                            break;
                        case STRING_START:
                            *p++ = STRING_START;
                            break;
                        case STRING_ESCAPE:
                            *p++ = STRING_ESCAPE;
                            break;
                        default:
                            *p++ = *tokstart;
                    }
                    tokstart++;
                }
            }
            *p = '\0';
            if (token_overflow)
            {
                // Come here if string is more than 2*buffer
                n = FILL_BUFFER();
                BUFFER_RESET();
                c = c - 1;
                *c = '"';       // insert double quote before rest of string
                token_overflow = 0;
                SHOW_STATE("after second string panic");
            }
            RETURN_STRING(strtok, strsize);
            SHOW_STATE("after string");
        }

        // ---------------------------------------------------EXTENSION
        else if (*c == EXTENSION_MARK)
        {
            c++;
            CHK_OVERFLOW("extension mark", {});
            if (*c == LIST_START)
            {
                c++;
                RETURN_SPECIAL(VECTOR_START);
            }
            else if (*c == COMMENT_START)
            {
                c++;
                RETURN_SPECIAL(OBJECT_COMMENT_START);
            }
            else if (*c == SHEBANG)
            {
                c++;
                CHK_OVERFLOW("shebang", RETURN_SPECIAL(EOF));
                while (*c != '\n')
                {
                    c++;
                    if (EOB)
                    {
                        n = FILL_BUFFER();
                        BUFFER_RESET();
                    }
                }
            }
            else if (*c == CHARACTER_START)
            {
                c++;
                CHK_OVERFLOW("character", RETURN_SPECIAL(EOF)); // Questionable

                if (*c == STRING_ESCAPE)
                {
                    c++;
                    CHK_OVERFLOW("second character", RETURN_SPECIAL(EOF)); // Ditto

                    switch (*c++)
                    {
                        case 'a':
                            chartok = '\a';
                            break;
                        case 'b':
                            chartok = '\b';
                            break;
                        case 'd':
                            chartok = '\x7f';
                            break;
                        case 'f':
                            chartok = '\f';
                            break;
                        case 'l':
                            chartok = '\x0c';
                            break;
                        case 'n':
                            chartok = '\n';
                            break;
                        case 'r':
                            chartok = '\r';
                            break;
                        case 't':
                            chartok = '\t';
                            break;
                        case 'v':
                            chartok = '\v';
                            break;
                        case 'x':
                            chartok = 0;
                            if (EOB_LOOKAHEAD(N_HEX_DIGITS))
                            {
                                i = maxc - c;
                                strcpy(buffer - (maxc - c), c);
                                n = FILL_BUFFER();
                                BUFFER_RESET();
                                c = c - i;
                                SHOW_STATE("after first hex char panic");
                            }
                            for (int i=0; i<N_HEX_DIGITS; i++)
                            {
                                if (isxdigit(*c))
                                {
                                    chartok =
                                    (chartok << 4) | (HEX_CHAR_TO_DIGIT(c));
                                }
                                else
                                {
                                    syntax_error
                                    (
                                        buffer,
                                        tokstart,
                                        "invalid hex insertion digit"
                                    );
                                }
                                c++;
                            }
                            break;
                        case STRING_START:
                            chartok = STRING_START;
                            break;
                        case STRING_ESCAPE:
                            chartok = STRING_ESCAPE;
                            break;
                        default:
                            chartok = ' ';
                            syntax_error(buffer, c, "invalid character name");
                    }
                    RETURN_CHARACTER(chartok);
                }
                else
                {
                    chartok = *c++;
                    RETURN_CHARACTER(chartok);
                }
            }
            else if (BINARY_START_P(c))
            {
                c++;
                GET_INTEGER(BINARY, fpitok);
                RETURN_FPI(fpitok);
            }
            else if (OCTAL_START_P(c))
            {
                c++;
                GET_INTEGER(OCTAL, fpitok);
                RETURN_FPI(fpitok);
            }
            else if (HEX_START_P(c))
            {
                c++;
                GET_INTEGER(HEX, fpitok);
                RETURN_FPI(fpitok);
            }
            else if (isdigit(*c))
            {
                GET_INTEGER(DECIMAL, fpibase);
                if (RADIX_START_P(c))
                {
                    c++;
                    GET_INTEGER(fpibase, fpitok);
                    RETURN_FPI(fpitok);
                }
                else
                {
                    syntax_error(buffer, c, "unknown extension code");
                }
            }
            else
            {   // It's a symbol
                c--;
                *c = EXTENSION_MARK;
                tokstart = c;
                READ_SYMBOL
            }
        }

        // ---------------------------------------------------QUASIQUOTE
        else if (*c == QUASIQUOTE)
        {
            c++;
            RETURN_SPECIAL(QUASIQUOTE);
        }

        // -------------------------------------------------------COMMA
        else if (*c == COMMA)
        {
            c++;
            CHK_OVERFLOW("comma", RETURN_SPECIAL(COMMA));
            if (*c == AT_SIGN)
            {
                c++;
                RETURN_SPECIAL(COMMA_AT_SIGN);
            }
            RETURN_SPECIAL(COMMA);
        }

        // -----------------------------------------------------COMMENT
        else if (*c == COMMENT_START)
        {
            c++;
            CHK_OVERFLOW("comment sign", RETURN_SPECIAL(EOF));
            while (*c != '\n')
            {
                c++;
                if (EOB)
                {
                    n = FILL_BUFFER();
                    BUFFER_RESET();
                }
            }
        }

        // --------------------------------------------------!SPECIAL_P
        else if ((ispunct(*c)) && (!SPECIAL_P(c)))
        {
            // This case is a real mess because we have to do two character
            // look-ahead to cope with signs and decimal points preceding
            // floating point numbers, finally we get to identifiers
            // starting with "other" characters
            tokstart = c;
            c++;
            CHK_OVERFLOW_AND_COPY("!special_p", break);

            // NOTE: the third and fourth test below are broken in the
            // circumstance that the end of buffer falls after -. or +.
            // this can probably be fixed by _very_ careful reorganisation
            // of this code.  Postponed until we feel braver
            if ((*tokstart == MINUS_SIGN) && isdigit(*c))
            {
                negative = 1;   // integer will be read on next iteration
            }
            else if ((*tokstart == PLUS_SIGN) && (isdigit(*c)))
            {
                negative = 0;   // integer will be read on next iteration
            }
            else if
            (
                (*tokstart == MINUS_SIGN)
             && (*c == DOT)
             && (isdigit(*(c + 1)))
            )
            {
                negative = 1;   // double will be read on next iteration
            }
            else if
            (
                (*tokstart == PLUS_SIGN)
             && (*c == DOT)
             && (isdigit(*(c + 1)))
            )
            {
                negative = 0;   // double will be read on next iteration
            }
            else if ((*tokstart == DOT) && (isdigit(*c)))
            {
                while (1)
                {
                    // This loop has the feature that a double followed by two
                    // bufferfuls of e's will generate a syntax error
                    if (isdigit(*c))
                    {
                        c++;
                    }
                    else if (EXPONENT_MARK_P(c))
                    {
                        c++;
                    }
                    else
                    {
                        break;
                    }
                    CHK_OVERFLOW_AND_COPY("double", break);     // eof_action
                                                                // is suspect
                }
                dbltok = strtod(tokstart, &c);
                RETURN_DOUBLE(negative ? -dbltok : dbltok);
                SHOW_STATE("after double");
            }
            else if ((*tokstart == DOT) && (BREAK_P(c)))
            {
                RETURN_SPECIAL(DOT);
            }
            else
            {
                c = tokstart;
                READ_SYMBOL
            }
        }

        // ------------------------------------------------------------
        else
        {
            // Skip unrecognised input
            //fprintf(stdout, "skipping character code %d\n", *c);
            c++;

            SHOW_STATE("after skip");
        }

        CHK_OVERFLOW("buffer", RETURN_SPECIAL(EOF));
    }

    return NULL; // ***HGW Should not get here
}


///-----------------------------------------------------------------------------
