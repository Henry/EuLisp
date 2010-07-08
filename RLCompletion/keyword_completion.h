///-----------------------------------------------------------------------------
/// ---              EuLisp Systems Youtoo and EuXLisp
///-----------------------------------------------------------------------------
///  File: keyword_completion.h
///  Description: Support for keyword completion using readline
///  See also: eulisp_keywords.h, youtoo_keywords.h, euxlisp_keywords.h
///-----------------------------------------------------------------------------
#ifndef KEYWORD_COMPLETION_H
#define KEYWORD_COMPLETION_H

// Functions supplying possible keyword completions
static char **keyword_completion(const char*, int ,int);
char *keyword_generator(const char*,int);

// Add support for the punctuation character "<" to part of a keyword
const char *rl_special_prefixes = "<";

static char **keyword_completion(const char *text , int start,  int end)
{
    // Switch-off default filename completion
    rl_attempted_completion_over = 1;
    return rl_completion_matches(text, &keyword_generator);
}

char *keyword_generator(const char *text, int state)
{
    static int list_index, len;
    char *name;

    if (!state)
    {
        list_index = 0;
        len = strlen(text);
    }

    while ((name = eulisp_keywords[list_index]))
    {
        list_index++;

        if (strncmp(name, text, len) == 0)
        {
            return strdup(name);
        }
    }

    // If no names matched, then return NULL.
    return ((char *)NULL);
}

///-----------------------------------------------------------------------------
#endif // KEYWORD_COMPLETION_H
///-----------------------------------------------------------------------------
