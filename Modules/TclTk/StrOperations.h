#ifndef STR_OPERATIONS_H
#define STR_OPERATIONS_H

#include "eulisp.h"

struct infoargs
{
    int argc;
    const char **argv;
};

extern int ParseArguments2
(
    struct infoargs *,
    const char *,
    const char *,
    LispRef
);

extern int ParseArguments3
(
    struct infoargs *,
    const char *,
    const char *,
    const char *,
    LispRef
);

extern int ParseArguments4
(
    struct infoargs *,
    const char *,
    const char *,
    const char *,
    const char *
);

#endif
