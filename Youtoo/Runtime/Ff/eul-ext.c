/// Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
///-----------------------------------------------------------------------------
/// ---                 EuLisp System 'youtoo'
///-----------------------------------------------------------------------------
///  Library: boot, telos, level1, eval, youtoo
///  Authos: Andreas Kind
///  Description: built-in foreign functions
///-----------------------------------------------------------------------------

#include "eulisp.h"

///-----------------------------------------------------------------------------
/// String buffer
///-----------------------------------------------------------------------------
#define BUFFER_SIZE 512
static char buffer[BUFFER_SIZE];


///-----------------------------------------------------------------------------
/// Make string
///-----------------------------------------------------------------------------
LispRef eul_init_string(LispRef str, int n, char c)
{
    char *data = (char *)gc_malloc(n + 1);
    memset(data, c, n);
    *(data + n) = '\0';
    STRING_DATA(str) = (LispRef) data;
    eul_string_size(str) = c_int_as_eul_int(n);

    return str;
}

LispRef c_strn_as_eul_str(char *str, int n)
{
    char *tmp_str = str;
    LispRef loc;
    ALLOCATE_WARM_OBJECT(loc, PGLOBAL(glob_string_class), STRING_SIZE);
    eul_allocate_int(eul_string_size(loc), n);
    STRING_DATA(loc) = (LispRef) tmp_str;

    return loc;
}

LispRef eul_int_as_hex_str(int x)
{
    // printf("eul_int_as_hex_str %d\n", x); fflush(stdout);

    if (x < 0)
    {
        if (x < -127)
        {
            return eul_nil;
        }
        else
        {
            sprintf(buffer, "%02x", x + 256);
        }
    }
    else if (x > 255)
    {
        return eul_nil;
    }
    else
    {
        sprintf(buffer, "%02x", x);
    }
    char *res = (char *)gc_malloc(3);
    strcpy(res, buffer);

    return c_strn_as_eul_str(res, 2);
}


///-----------------------------------------------------------------------------
/// Make vector
///-----------------------------------------------------------------------------
LispRef eul_make_vector(int n, LispRef list)
{
    LispRef loc;
    eul_allocate_vector(loc, n, list);

    return loc;
}


///-----------------------------------------------------------------------------
/// List of strings into one string
///-----------------------------------------------------------------------------
LispRef eul_list_as_eul_string(LispRef x)
{
    // Get string size
    LispRef tmp, l = x;
    int n = 0;
    while (eul_is_cons(l))
    {
        tmp = eul_car(l);
        if (eul_is_string(tmp))
        {
            n += eul_int_as_c_int(eul_string_size(tmp));
        }
        else if (eul_is_symbol(tmp))
        {
            n += eul_int_as_c_int(eul_string_size(eul_symbol_name(tmp)));
        }
        else if (eul_is_char(tmp))
        {
            n++;
        }
        l = eul_cdr(l);
    }

    // Allocate and fill the result string
    char *str = (char *)gc_malloc(n + 1);
    l = x;
    int i = 0;
    while (eul_is_cons(l))
    {
        tmp = eul_car(l);
        if (eul_is_string(tmp))
        {
            n = eul_int_as_c_int(eul_string_size(tmp));
            strcpy(str + i, eul_string_as_c_string(tmp));
            i += n;
        }
        else if (eul_is_symbol(tmp))
        {
            n = eul_int_as_c_int(eul_string_size(eul_symbol_name(tmp)));
            strcpy(str + i, eul_symbol_as_c_string(tmp));
            i += n;
        }
        else if (eul_is_char(tmp))
        {
            *(str + i) = eul_char_as_c_char(tmp);
            i++;
        }
        l = eul_cdr(l);
    }
    *(str + i) = '\0';

    LispRef res;
    eul_allocate_nstring(res, str, i);

    return res;
}


///-----------------------------------------------------------------------------
/// Make symbol and keyword
///-----------------------------------------------------------------------------
LispRef eul_make_symbol(char *str)
{
    LispRef loc;
    eul_intern_symbol(loc, str);

    return loc;
}

LispRef eul_c_str_as_eul_symbol(LispRef str)
{
    LispRef loc;
    eul_intern_symbol(loc, (char *)str);

    return loc;
}

LispRef eul_init_symbol(LispRef sym)
{
    char *str = eul_symbol_as_c_string(sym);
    LispRef res = eul_fast_table_ref(PGLOBAL(glob_symbols), str);
    if (eul_null(res))
    {
        eul_fast_table_set(PGLOBAL(glob_symbols), str, sym);
        return sym;
    }

    return res;
}

LispRef eul_make_keyword(char *str)
{
    LispRef loc;
    eul_intern_keyword(loc, str);

    return loc;
}

LispRef eul_init_keyword(LispRef key)
{
    char *str = eul_keyword_as_c_string(key);
    LispRef res = eul_fast_table_ref(PGLOBAL(glob_keywords), str);
    if (eul_null(res))
    {
        eul_fast_table_set(PGLOBAL(glob_keywords), str, key);
        return key;
    }

    return res;
}

LispRef c_strn_as_eul_symbol_or_keyword(char *str, int n)
{
    LispRef loc;
    if (*(str + n - 1) == ':')
    {
        *(str + n - 1) = '\0';
        eul_intern_keyword(loc, str);
    }
    else
    {
        eul_intern_symbol(loc, str);
    }

    return loc;
}


///-----------------------------------------------------------------------------
/// Conversion int -> string
///-----------------------------------------------------------------------------
char *eul_int_as_str(int x)
{
    sprintf(buffer, "%d", x);
    char *res = (char *)gc_malloc(strlen(buffer) + 1);
    strcpy(res, buffer);

    return res;
}


///-----------------------------------------------------------------------------
/// Lower/upper string
///-----------------------------------------------------------------------------
char *eul_str_tolower(char *str)
{
    int n = strlen(str);
    char *res = (char *)gc_malloc(n + 1);

    for (int i = 0; (i < n) && (i < BUFFER_SIZE); i++)
    {
        buffer[i] = tolower(str[i]);
    }
    strcpy(res, buffer);

    return res;
}

char *eul_str_toupper(char *str)
{
    int n = strlen(str);

    char *res = (char *)gc_malloc(n + 1);
    for (int i = 0; (i < n) && (i < BUFFER_SIZE); i++)
    {
        buffer[i] = toupper(str[i]);
    }
    strcpy(res, buffer);

    return res;
}


///-----------------------------------------------------------------------------
/// String functions
///-----------------------------------------------------------------------------
int eul_find_char(char c, char *str)
{
    char *ptr = (char *)strchr(str, c);
    if (ptr == NULL)
    {
        return -1;
    }
    else
    {
        return ((int)(ptr - str));
    }
}

char *eul_substr(char *str, int i, int j)
{
    int n = j - i;

    if (BUFFER_SIZE < n + 1)
    {
        n = BUFFER_SIZE;
    }

    char *res = (char *)gc_malloc(n + 1);
    strncpy(buffer, (char *)(str + i), n);
    buffer[n] = '\0';
    strcpy(res, buffer);

    return res;
}

char *eul_tailstr(char *str, int i)
{
    int n = strlen(str) - i;
    char *res = (char *)gc_malloc(n + 1);
    strcpy(res, (char *)(str + i));

    return res;
}

LispRef eul_str_member1(char c, char *str)
{
    ptrInt i = (ptrInt) strchr(str, c);
    if (i == (ptrInt) NULL)
    {
        return eul_nil;
    }
    else
    {
        LispRef res;
        eul_allocate_int(res, (i - ((ptrInt) str)));
        return res;
    }
}

char *eul_reverse_str(char *str)
{
    int n = strlen(str);
    char *res = (char *)gc_malloc(n + 1);

    for (int i = 0; i < n; i++)
    {
        *(res + (n - 1) - i) = *(str + i);
    }
    *(res + n) = '\0';

    return res;
}

char *eul_reverse_des_str(char* str)
{
    int n = strlen(str);

    for (int i=0; i<n/2; i++)
    {
        char c = *(str + (n - 1) - i);
        *(str + (n - 1) - i) = *(str + i);
        *(str + i) = c;
    }
    *(str + n) = '\0';

    return str;
}

char *eul_str_append(char *str1, char *str2)
{
    int n1 = strlen(str1);
    int n2 = strlen(str2);
    char *res = (char *)gc_malloc(n1 + n2 + 1);
    strcpy(res, str1);
    strcpy(res + n1, str2);

    return res;
}


///-----------------------------------------------------------------------------
/// Return address string used with default prin/write
///-----------------------------------------------------------------------------
char *eul_addr_str(void *obj)
{
    sprintf(buffer, "0x%08"ptrIntPM"X", (ptrInt) obj);
    char *res = (char *)gc_malloc(strlen(buffer) + 1);
    strcpy(res, buffer);

    return res;
}


///-----------------------------------------------------------------------------
/// String copy
///-----------------------------------------------------------------------------
char *eul_str_copy(char *str)
{
    char *res = (char *)gc_malloc(strlen(str) + 1);
    strcpy(res, str);

    return res;
}


///-----------------------------------------------------------------------------
/// Hash tables
///-----------------------------------------------------------------------------
LispRef eul_table_ref(LispRef tab, char *key)
{
    return eul_fast_table_ref(tab, key);
}

LispRef eul_table_set(LispRef tab, char *key, LispRef value)
{
    return eul_fast_table_set(tab, key, value);
}


///-----------------------------------------------------------------------------
/// Dynamic access to module bindings
///-----------------------------------------------------------------------------
LispRef eul_dyn_binding_ref(char *module_name, int i)
{
    LispRef bindings = eul_fast_table_ref(eul_modules, module_name);

    if (eul_null(bindings))
    {
        return eul_nil;
    }
    else
    {
        return (*(((LispRef *) bindings) + i));
    }
}

LispRef eul_dyn_binding_set(char *module_name, int i, LispRef x)
{
    LispRef bindings = eul_fast_table_ref(eul_modules, module_name);

    if (eul_null(bindings))
    {
        return eul_nil;
    }
    else
    {
        return ((*(((LispRef *) bindings) + i)) = x);
    }
}


///-----------------------------------------------------------------------------
/// Get errno
///-----------------------------------------------------------------------------
int get_errno()
{
    return errno;
}


///-----------------------------------------------------------------------------
/// sprintf_*
///-----------------------------------------------------------------------------
int eul_sprintf(char *buf, int buf_offset, char *fmt_str, LispRef x)
{
    if (eul_is_int(x))
    {
        return sprintf(buf + buf_offset, fmt_str, eul_int_as_c_int(x));
    }
    else if (eul_is_double(x))
    {
        return sprintf
        (
            buf + buf_offset, fmt_str,
            eul_double_as_c_double(x)
        );
    }
    else if (eul_is_char(x))
    {
        return sprintf(buf + buf_offset, fmt_str, eul_char_as_c_char(x));
    }
    else
    {
        return sprintf(buf + buf_offset, fmt_str, (ptrInt) x);
    }
}

int eul_sprintf_string
(
    char *buf,
    int buf_offset,
    int n,
    int x_offset,
    char *str,
    char *x
)
{
    // Format string ignored
    memcpy(buf + buf_offset, x + x_offset, n);

    return 1;
}


///-----------------------------------------------------------------------------
/// Double conversion
///-----------------------------------------------------------------------------
LispRef c_double_as_eul_double(double x)
{
    LispRef res;
    eul_allocate_double(res, x);

    return res;
}


///-----------------------------------------------------------------------------
/// Get error message
///-----------------------------------------------------------------------------
char *eul_strerror()
{
    if (errno)
    {
        return (char *)strerror(errno);
    }
    else
    {
        return "";
    }
}


///-----------------------------------------------------------------------------
/// Format string information
///-----------------------------------------------------------------------------
LispRef eul_format_info(char *str)
{
    char c;
    int i = 0, j = 0;
    LispRef res = eul_nil;

    while (1)
    {
        if (((c = *(str + i)) == '\0') || (c == '~'))
        {
            if (c == '\0')
            {
                eul_allocate_cons(res, c_int_as_eul_int(j), res);
                eul_allocate_cons(res, c_int_as_eul_int(i - j), res);
                eul_allocate_cons(res, c_char_as_eul_char('\0'), res);
                return res;
            }
            else
            {
                eul_allocate_cons(res, c_int_as_eul_int(j), res);
                eul_allocate_cons(res, c_int_as_eul_int(i - j), res);
                eul_allocate_cons(res, c_char_as_eul_char(*(str + (++i))), res);
                j = ++i;
            }
        }
        else
        {
            i++;
        }
    }
}


///-----------------------------------------------------------------------------
/// Posix codes for file access
///-----------------------------------------------------------------------------
LispRef eul_posix_codes()
{
    LispRef res;
    eul_allocate_object(res, PGLOBAL(glob_vector_class), 10, eul_nil);
    eul_allocate_int(eul_slot_ref(res, 0), O_RDONLY);
    eul_allocate_int(eul_slot_ref(res, 1), O_WRONLY);
    eul_allocate_int(eul_slot_ref(res, 2), O_RDWR);
    eul_allocate_int(eul_slot_ref(res, 3), O_APPEND);
    eul_allocate_int(eul_slot_ref(res, 4), O_NONBLOCK);
    eul_allocate_int(eul_slot_ref(res, 5), O_CREAT);
    eul_allocate_int(eul_slot_ref(res, 6), O_TRUNC);
    eul_allocate_int(eul_slot_ref(res, 7), O_EXCL);
    eul_allocate_int(eul_slot_ref(res, 8), F_GETFL);
    eul_allocate_int(eul_slot_ref(res, 9), F_SETFL);

    return res;
}


///-----------------------------------------------------------------------------
/// Check for readable files
///-----------------------------------------------------------------------------
LispRef eul_file_lookup(char *name, LispRef dirs)
{
    static char delim_char = '/', *delim_str = "/";

    char *dir, *file_name;
    int n;
    LispRef eul_dir;

    while (dirs != eul_nil)
    {
        eul_dir = eul_car(dirs);
        dir = eul_string_as_c_string(eul_dir);
        n = strlen(dir);
        if (*(dir + n - 1) == delim_char)
        {
            file_name = dir;
        }
        else
        {
            file_name = eul_str_append(dir, delim_str);
        }
        file_name = eul_str_append(file_name, name);

        if (access(file_name, R_OK) == 0)
        {
            LispRef res, eul_file_name;

            eul_allocate_string(eul_file_name, file_name);
            eul_allocate_cons(res, eul_file_name, eul_dir);
            return res;
        }
        dirs = eul_cdr(dirs);
    }

    return eul_nil;
}


///-----------------------------------------------------------------------------
/// Compare file modification times
///-----------------------------------------------------------------------------
LispRef eul_file_newer_p(char *file_name1, char *file_name2)
{
    static struct stat buf;

    int t1, t2;
    LispRef res;

    if (stat(file_name1, &buf) != 0)
    {
        eul_allocate_string(res, file_name1);
        return res;
    }

    t1 = buf.st_mtime;

    if (stat(file_name2, &buf) != 0)
    {
        eul_allocate_string(res, file_name2);
        return res;
    }

    t2 = buf.st_mtime;

    if (t1 > t2)
    {
        return eul_true;
    }
    else
    {
        return eul_nil;
    }
}


///-----------------------------------------------------------------------------
/// Pseudo-Random numbers
///-----------------------------------------------------------------------------
int eul_rand_max()
{
    return RAND_MAX;
}

int eul_srand(int n)
{
    srand(n);
    return n;
}


///-----------------------------------------------------------------------------
/// Convert Lisp values to Lisp references
///-----------------------------------------------------------------------------
LispRef eul_int_as_eul_int_ref(LispRef x)
{
    int *ptr = (int *)gc_malloc(sizeof(int *));
    (*ptr) = eul_int_as_c_int(x);

    LispRef res;
    eul_allocate_int_ref(res, ptr);

    return res;
}

LispRef eul_double_as_eul_double_ref(LispRef x)
{
    double *ptr = (double *)gc_malloc(sizeof(double *));
    (*ptr) = eul_double_as_c_double(x);

    LispRef res;
    eul_allocate_double_ref(res, ptr);

    return res;
}

LispRef eul_string_as_eul_string_ref(LispRef x)
{
    char **ptr = (char **)gc_malloc(sizeof(char **));
    (*ptr) = eul_string_as_c_string(x);

    LispRef res;
    eul_allocate_string_ref(res, ptr);

    return res;
}


///-----------------------------------------------------------------------------
/// CPU time
///-----------------------------------------------------------------------------
int eul_ticks_per_second()
{
    return (int)sysconf(_SC_CLK_TCK);
}

#define TIMES_SIZE 3
#define TIMES_REAL(x) slot_ref(x, 0)
#define TIMES_USER(x) slot_ref(x, 1)
#define TIMES_SYS(x) slot_ref(x, 2)

LispRef eul_cpu_time()
{
    static struct tms buffer;

    LispRef res;
    eul_allocate_object(res, PGLOBAL(glob_vector_class), TIMES_SIZE, eul_nil);

    int r = (int)times(&buffer);
    if (r == -1)
    {
        return eul_nil;
    }

    int u = (int)buffer.tms_utime;
    int s = (int)buffer.tms_stime;

    TIMES_REAL(res) = c_int_as_eul_int(r);
    TIMES_USER(res) = c_int_as_eul_int(u);
    TIMES_SYS(res) = c_int_as_eul_int(s);

    return res;
}


///-----------------------------------------------------------------------------
/// Access to simple function code
///-----------------------------------------------------------------------------
LispRef eul_get_lambda_code(LispRef lambda)
{
    return LAMBDA_CODE(lambda);
}

char *eul_set_lambda_code(LispRef lambda, char *code)
{
    return (char *)(LAMBDA_CODE(lambda) = (LispRef) code);
}


///-----------------------------------------------------------------------------
/// Bitwise operators
///-----------------------------------------------------------------------------
int eul_bit_and(int x, int y)
{
    return x & y;
}

int eul_bit_ior(int x, int y)
{
    return x | y;
}

int eul_bit_xor(int x, int y)
{
    return x ^ y;
}

int eul_bit_not(int x)
{
    return ~x;
}

int eul_bit_shift(int x, int n)
{
    if (n < 0)
    {
        return x << n;
    }
    else
    {
        return x >> (0 - n);
    }
}


///-----------------------------------------------------------------------------
/// Benchmarking function
///-----------------------------------------------------------------------------
LispRef eul_ffoo(LispRef x, int y, char *z)
{
    return eul_nil;
}


///-----------------------------------------------------------------------------
