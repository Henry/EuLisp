///-----------------------------------------------------------------------------
/// ---                 EuLisp System 'youtoo'
///-----------------------------------------------------------------------------
///  Library: string-perls
///  Description: foreign function interface to POSIX and PCRE libraries
///-----------------------------------------------------------------------------

#ifndef EUL_STRING_PERLS_H
#define EUL_STRING_PERLS_H

#include "eulisp.h"

///-----------------------------------------------------------------------------

// Split the string str at the delimiters and return a list of the sub-strings
// Note: the list returned is in reverse order
extern LispRef eul_split_string(const char *str, const char *delimiters);

extern LispRef eul_match_string(const char *str, const char *regex);
extern LispRef eul_match_all_string(const char *str, const char *regex);

///-----------------------------------------------------------------------------
#endif // EUL_STRING_PERLS_H
///-----------------------------------------------------------------------------
