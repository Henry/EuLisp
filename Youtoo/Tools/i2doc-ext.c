#include <eulisp.h>
#include <time.h>

char *eul_strftime (char *str)
{
    char buffer[1024];

    time_t t = time((time_t *)0);
    struct tm *lt = localtime (&t);
    size_t n = strftime (buffer, 1024, (char *)str, lt);
    buffer[n] = '\0';
    char *res = (char *) gc_malloc (n + 1);
    strcpy (res, buffer);

    // Should I be doing something using c_strn_as_eul_str() here???
    return res;
}
