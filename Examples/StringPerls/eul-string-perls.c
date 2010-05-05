///-----------------------------------------------------------------------------
/// ---                 EuLisp System 'youtoo'
///-----------------------------------------------------------------------------
/// Library: string-perls
///-----------------------------------------------------------------------------

#include "eul-string-perls.h"

///-----------------------------------------------------------------------------

///-----------------------------------------------------------------------------
/// Split
///-----------------------------------------------------------------------------
LispRef eul_split_string(char *str, char *delimiters)
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
