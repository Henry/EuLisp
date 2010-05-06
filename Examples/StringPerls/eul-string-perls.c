///-----------------------------------------------------------------------------
/// ---                 EuLisp System 'youtoo'
///-----------------------------------------------------------------------------
/// Library: string-perls
///-----------------------------------------------------------------------------

#include "eul-string-perls.h"
#include "pcre.h"

///-----------------------------------------------------------------------------

///-----------------------------------------------------------------------------
/// Split
///-----------------------------------------------------------------------------
LispRef eul_split_string(const char *str, const char *delimiters)
{
    #ifdef DEBUG
    printf
    (
        "Splitting string \"%s\" into tokens using delimiters \"%s\":\n",
        str,
        delimiters
    );
    #endif

    // Copy the argument string because strtok is destructive
    char *strbuf = (char *)gc_malloc(strlen(str) + 1);
    strcpy(strbuf, str);
    char *substr = strtok(strbuf, delimiters);

    // Initialise the return list to be empty in case there is no match
    LispRef res = eul_nil;

    while (substr != NULL)
    {
        #ifdef DEBUG
        printf("sub-string %s\n", substr);
        #endif

        LispRef eul_substr;
        eul_allocate_string(eul_substr, substr);
        eul_allocate_cons(res, eul_substr, res);

        substr = strtok(NULL, delimiters);
    }

    return res;
}

///-----------------------------------------------------------------------------
/// PCRE Regular Expression handling
///-----------------------------------------------------------------------------

// Compile and return the regular expression
pcre *eul_compile_regex(const char *regex)
{
    const char *error;
    int erroffset;

    pcre *re = pcre_compile
    (
        regex,      // the pattern
        0,          // default options
        &error,     // for error message
        &erroffset, // for error offset
        NULL        // use default character table
    );

    if (!re)
    {
        #ifdef DEBUG
        fprintf
        (
            stderr,
            "PCRE compilation failed at expression offset %d: %s\n",
            erroffset,
            error
        );
        #endif
    }

    return re;
}

#define OVECCOUNT 30 // should be a multiple of 3

// Match the next occurrence of the regular expression
// and reset the start of the string to the end of the occurrence
LispRef eul_match_next_string(const char **strPtr, const pcre* re)
{
    const char *str = *strPtr;
    int ovector[OVECCOUNT];

    int rc = pcre_exec
    (
        re,           // the compiled pattern
        NULL,         // no extra data - we didn't study the pattern
        str,          // the subject string
        strlen(str),  // the length of the subject
        0,            // start at offset 0 in the subject
        0,            // default options
        ovector,      // output vector for substring information
        OVECCOUNT     // number of elements in the output vector
    );

    if (rc < 0)
    {
        switch(rc)
        {
            case PCRE_ERROR_NOMATCH:
                #ifdef DEBUG
                printf("No match found in text\n");
                #endif
                break;

                // More cases defined...

            default:
                #ifdef DEBUG
                printf("Match error %d\n", rc);
                #endif
                break;
        }
    }

    LispRef res = eul_nil;

    if (rc > 0)
    {
        // Loop over the matches in reverse-order
        // to cons into the list in the correct order
        for (int i=rc-1; i>=0; i--)
        {
            int j = 2*i;

            LispRef eul_substr;
            const char *start = str + ovector[j];
            int length = ovector[j+1] - ovector[j];
            eul_allocate_nstring(eul_substr, start, length);
            eul_allocate_cons(res, eul_substr, res);
        }

        // Reset the start of the search string to the end of the last match
        *strPtr = str + ovector[2*(rc - 1) + 1];
    }

    return res;
}

LispRef eul_match_string(const char *str, const char *regex)
{
    pcre* re = eul_compile_regex(regex);

    if (re)
    {
        const char *strStart = str;
        return eul_match_next_string(&strStart, re);
    }
    else
    {
        return eul_nil;
    }
}

LispRef eul_match_all_string(const char *str, const char *regex)
{
    LispRef match, res = eul_nil;
    pcre* re = eul_compile_regex(regex);

    if (re)
    {
        while ((match = eul_match_next_string(&str, re)) != eul_nil)
        {
            eul_allocate_cons(res, match, res);
        }
    }

    return res;
}

///-----------------------------------------------------------------------------
