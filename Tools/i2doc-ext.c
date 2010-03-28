#include <eulisp.h>
#include <time.h>

char *eul_strftime (char *str)
{
    char buffer[1024];
    size_t n;
    time_t t;
    struct tm *lt = NULL;
    char *res;

    t = time((time_t *)0);
    lt = localtime (&t);

    n = strftime (buffer, 1024, (char *)str, lt);
    buffer[n] = '\0';
    res = (char *) gc_malloc (n + 1);
    strcpy (res, buffer);
    // Should I be doing something using c_strn_as_eul_str() here???
    return res;
}
