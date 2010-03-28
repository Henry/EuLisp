/** Copyright (c) 1997 by A Kind & University of Bath. All rights reserved. **/

/** ----------------------------------------------------------------------- **
 **                     EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Library: eul-serial
 **  Authors: Andreas Kind
 **  Description: marshaling EuLisp objects in a serialized format
 ** ----------------------------------------------------------------------- **/

#include "eulisp.h"
#include "eul-serial.h"
#include "eul-serial2.h"
#include "serial.h"


/** ----------------------------------------------------------------- **
 ** Primitive serialization (should use xdr soon!)
 ** ----------------------------------------------------------------- **/

LispRef eul_serial_short_data(int x)
{
    LispRef res;
    eul_allocate_blank_string(res, sizeof(short));
    *(short*)eul_string_as_c_string(res) = (short)x;
    return res;
}


LispRef eul_serial_int_data(int x)
{
    LispRef res;
    eul_allocate_blank_string(res, sizeof(int));
    *(int*)eul_string_as_c_string(res) = x;
    return res;
}


LispRef eul_serial_double_data(LispRef ref)
{
    double x = eul_double_as_c_double(ref);
    LispRef res;
    eul_allocate_blank_string(res, sizeof(double));
    *(double*)eul_string_as_c_string(res) = x;
    return res;
}


LispRef eul_serial_lambda_data(LispRef x)
{
    Instruction *code = (Instruction *) LAMBDA_CODE(x);
    int n = eul_int_as_c_int(eul_size_of(x));
    LispRef res;
    eul_allocate_blank_string(res, n);
    Instruction *data = (Instruction *)eul_string_as_c_string(res);
    memcpy(data, code, n);
    return res;
}


LispRef eul_serial_bytevector_data(LispRef x)
{
    char *data = (char *)BYTEVECTOR_DATA(x);
    int n = eul_int_as_c_int(eul_size_of(x));
    LispRef res;
    eul_allocate_nstring(res, data, n);
    return res;
}


int eul_serial_relative_pc(LispRef fun, LispRef eul_pc)
{
    Instruction *pc = (Instruction *) eul_pc;
    Instruction *bv = (Instruction *) LAMBDA_CODE(fun);
    return (int)(pc - bv);
}


EUL_DEFINTERN(eul_serial_read_bytes, "eul-serial-read-bytes", 4, serial)
EUL_DEFINTERN(eul_serial_error, "eul-serial-error", 3, serial)
EUL_DEFINTERN(eul_serial_make_class, "eul-serial-make-class", 3, serial)
EUL_DEFINTERN(eul_serial_make_state, "eul-serial-make-state", 4, serial)

EUL_DEFINTERN
(
    eul_serial_make_file_stream,
    "eul-serial-make-file-stream",
    2,
    serial
)

EUL_DEFINTERN
(
    eul_serial_make_instance,
    "eul-serial-make-instance",
    2,
    serial
)

EUL_DEFINTERN
(
    eul_serial_allocate_instance,
    "eul-serial-allocate-instance",
    1,
    serial
)

EUL_DEFINTERN
(
    eul_serial_initialize_instance,
    "eul-serial-initialize-instance",
    2,
    serial
)


LispRef eul_serial_initialize()
{
    LispRef res;
    eul_allocate_vector(res, 21, eul_nil);

    slot_ref(res, 0) = c_int_as_eul_int(STREAM_MAGIC);
    slot_ref(res, 1) = c_int_as_eul_int(STREAM_VERSION);
    slot_ref(res, 2) = c_char_as_eul_char(TC_NULL);
    slot_ref(res, 3) = c_char_as_eul_char(TC_REFERENCE);
    slot_ref(res, 4) = c_char_as_eul_char(TC_CLASS);
    slot_ref(res, 5) = c_char_as_eul_char(TC_OBJECT);
    slot_ref(res, 6) = c_char_as_eul_char(TC_STRING);
    slot_ref(res, 7) = c_char_as_eul_char(TC_VECTOR);
    slot_ref(res, 8) = c_char_as_eul_char(TC_KEYWORD);
    slot_ref(res, 9) = c_char_as_eul_char(TC_STREAM);
    slot_ref(res, 10) = c_char_as_eul_char(TC_STATE);
    slot_ref(res, 11) = c_char_as_eul_char(TC_RESET);
    slot_ref(res, 12) = c_char_as_eul_char(TC_SELF);
    slot_ref(res, 14) = c_char_as_eul_char(TC_FUNCTION);
    slot_ref(res, 15) = c_char_as_eul_char(TC_BYTEVECTOR);
    slot_ref(res, 16) = c_char_as_eul_char(TC_INT);
    slot_ref(res, 17) = c_char_as_eul_char(TC_DOUBLE);
    slot_ref(res, 18) = c_char_as_eul_char(TC_SYMBOL);
    slot_ref(res, 19) = c_char_as_eul_char(TC_CHAR);
    slot_ref(res, 20) = c_char_as_eul_char(TC_CONS);

    return res;
}


/** ----------------------------------------------------------------- **
 ** Auxiliary functions
 ** ----------------------------------------------------------------- **/

LispRef eul_symbol_as_eul_keyword(LispRef sym)
{
    char *str1 = eul_symbol_as_c_string(sym);
    int n = eul_int_as_c_int(eul_string_size(eul_symbol_name(sym)));
    char *str2 = (char *)gc_malloc(n + 1);
    strcpy(str2, str1);
    LispRef key;
    eul_intern_keyword(key, str2);
    return key;
}


/** ----------------------------------------------------------------- **
 ** Interface to the Lisp world
 ** ----------------------------------------------------------------- **/

LispRef eul_serial_read_header
(
    LispRef stream,
    LispRef eos_error_p,
    LispRef eos_value
)
{
    return header(stream, eos_error_p, eos_value);
}


LispRef eul_serial_read_object
(
    LispRef stream,
    LispRef eos_error_p,
    LispRef eos_value
)
{
    return content(stream, eos_error_p, eos_value);
}


/** ----------------------------------------------------------------- **
 ** header:
 **   magic version
 ** ----------------------------------------------------------------- **/

LispRef eul_keyword_key;
LispRef eul_name_key;

LispRef header(LispRef stream, LispRef eos_error_p, LispRef eos_value)
{
    WITH_DEBUG(fprintf(stderr, "header\n"));

    LispRef magic;
    eul_read_short(magic);

    LispRef version;
    eul_read_short(version);

    LispRef res;
    eul_allocate_cons(res, magic, version);

    eul_intern_keyword(eul_keyword_key, "keyword");
    eul_intern_keyword(eul_name_key, "name");

    return res;
}


/** ----------------------------------------------------------------- **
 ** content:
 **   object
 ** ----------------------------------------------------------------- **/

LispRef content(LispRef stream, LispRef eos_error_p, LispRef eos_value)
{
    WITH_DEBUG(fprintf(stderr, "content\n"));

    char tag;
    read_byte(tag);

    WITH_DEBUG(fprintf(stderr, "  tag: %x\n", tag));

    switch (tag)
    {
        case TC_NULL:
            return nullReference(stream, eos_error_p, eos_value);
        case TC_REFERENCE:
            return prevObject(stream, eos_error_p, eos_value);
        case TC_CLASS:
            return newClass(stream, eos_error_p, eos_value);
        case TC_OBJECT:
            return newObject(stream, eos_error_p, eos_value);
        case TC_STRING:
            return newString(stream, eos_error_p, eos_value);
        case TC_STATE:
            return newState(stream, eos_error_p, eos_value);
        case TC_VECTOR:
            return newVector(stream, eos_error_p, eos_value);
        case TC_STREAM:
            return newStream(stream, eos_error_p, eos_value);
        case TC_RESET:
            return reset(stream, eos_error_p, eos_value);
        case TC_SELF:
            return stream;
        case TC_FUNCTION:
            return newFunction(stream, eos_error_p, eos_value);
        case TC_BYTEVECTOR:
            return newBytevector(stream, eos_error_p, eos_value);
        case TC_INT:
            return newInt(stream, eos_error_p, eos_value);
        case TC_DOUBLE:
            return newDouble(stream, eos_error_p, eos_value);
        case TC_SYMBOL:
            return newSymbol(stream, eos_error_p, eos_value);
        case TC_KEYWORD:
            return newKeyword(stream, eos_error_p, eos_value);
        case TC_CHAR:
            return newChar(stream, eos_error_p, eos_value);
        case TC_CONS:
            return newCons(stream, eos_error_p, eos_value);
        default:
            {
                LispRef str, args;

                eul_allocate_string(str, "unknown tag in ~a");
                eul_allocate_cons(args, stream, eul_nil);
                eul_serial_error(stream, str, args);
                return eul_nil;
            }
    }
}


/** ----------------------------------------------------------------- **
 ** newClass:
 **   TC_CLASS content content content newHandle
 ** ----------------------------------------------------------------- **/

LispRef newClass(LispRef stream, LispRef eos_error_p, LispRef eos_value)
{
    WITH_DEBUG(fprintf(stderr, "newClass\n"));

    LispRef name = content(stream, eos_error_p, eos_value);
    LispRef supers = content(stream, eos_error_p, eos_value);
    LispRef slot_descs = content(stream, eos_error_p, eos_value);
    LispRef res = eul_serial_make_class(name, supers, slot_descs);
    newHandle(stream, res);

    return res;
}


/** ----------------------------------------------------------------- **
 ** newObject:
 **   TC_OBJECT content
 ** ----------------------------------------------------------------- **/

LispRef newObject(LispRef stream, LispRef eos_error_p, LispRef eos_value)
{
    WITH_DEBUG(fprintf(stderr, "newObject\n"));

    LispRef cl = content(stream, eos_error_p, eos_value);
    LispRef res = eul_serial_allocate_instance(cl);
    newHandle(stream, res);
    LispRef inits = content(stream, eos_error_p, eos_value);
    eul_serial_initialize_instance(res, inits);

    return res;
}


/** ----------------------------------------------------------------- **
 ** newCons:
 **   TC_CONS content content
 ** ----------------------------------------------------------------- **/

LispRef newCons(LispRef stream, LispRef eos_error_p, LispRef eos_value)
{
    WITH_DEBUG(fprintf(stderr, "newCons\n"));

    LispRef res;
    eul_allocate_empty_cons(res);
    newHandle(stream, res);
    LispRef the_car = content(stream, eos_error_p, eos_value);
    LispRef the_cdr = content(stream, eos_error_p, eos_value);
    eul_car(res) = the_car;
    eul_cdr(res) = the_cdr;

    return res;
}


/** ----------------------------------------------------------------- **
 ** newFunction:
 **   TC_FUNCTION content content content content int [byte] newHandle content
 ** ----------------------------------------------------------------- **/

LispRef newFunction(LispRef stream, LispRef eos_error_p, LispRef eos_value)
{
    WITH_DEBUG(fprintf(stderr, "newFunction\n"));

    LispRef name = content(stream, eos_error_p, eos_value);
    LispRef domain = content(stream, eos_error_p, eos_value);
    LispRef setter = content(stream, eos_error_p, eos_value);
    LispRef env = content(stream, eos_error_p, eos_value);

    int n;
    read_int(n);

    WITH_DEBUG(fprintf(stderr, "size: %d\n", n));

    char *data = read_bytes(n);
    char *new_data = (char *)gc_malloc(n);
    memcpy(new_data, data, n);

    LispRef code;
    eul_allocate_nstring(code, new_data, n);

    LispRef res;
    eul_allocate_lambda1(res, name, domain, code);
    LAMBDA_SETTER(res) = setter;
    LAMBDA_ENV(res) = env;
    newHandle(stream, res);
    LispRef refs = content(stream, eos_error_p, eos_value);

    WITH_DEBUG(fprintf(stderr, "!!!Refs: "));
    WITH_DEBUG(fprint_ref(stderr, refs));
    WITH_DEBUG(fprintf(stderr, "\n"));

    if (refs != eul_nil)
    {
        if (eul_link_lambda_refs(res, refs) == eul_nil)
        {
            LispRef str;
            eul_allocate_string(str, "cannot serialize foreign function");
            eul_serial_error(stream, str, eul_nil);
            return eul_nil;
        }
    }

    return res;
}


/** ----------------------------------------------------------------- **
 ** newBytevector:
 **   TC_BYTEVECTOR int [byte] content
 ** ----------------------------------------------------------------- **/

LispRef newBytevector(LispRef stream, LispRef eos_error_p, LispRef eos_value)
{
    WITH_DEBUG(fprintf(stderr, "newBytevector\n"));

    int n;
    read_int(n);

    WITH_DEBUG(fprintf(stderr, "size: %d\n", n));

    char *data = read_bytes(n);
    char *new_data = (char *)gc_malloc(n);
    memcpy(new_data, data, n);

    LispRef code;
    eul_allocate_nstring(code, new_data, n);

    LispRef res;
    eul_allocate_bytevector1(res, code, n);

    LispRef refs = content(stream, eos_error_p, eos_value);

    if (refs != eul_nil)
    {
        if (eul_link_lambda_refs(res, refs) == eul_nil)
        {
            LispRef str;

            eul_allocate_string(str, "cannot serialize foreign function");
            eul_serial_error(stream, str, eul_nil);
            return eul_nil;
        }
    }

    return res;
}


/** ----------------------------------------------------------------- **
 ** newVector int [content]
 ** ----------------------------------------------------------------- **/

LispRef newVector(LispRef stream, LispRef eos_error_p, LispRef eos_value)
{
    WITH_DEBUG(fprintf(stderr, "newVector\n"));

    int n;
    read_int(n);

    LispRef vec;
    eul_allocate_vector(vec, n, eul_nil);
    newHandle(stream, vec);

    for (int i = 0; i < n; i++)
    {
        slot_ref(vec, i) = content(stream, eos_error_p, eos_value);
    }

    return vec;
}


/** ----------------------------------------------------------------- **
 ** newState content int content int
 ** ----------------------------------------------------------------- **/

LispRef newState(LispRef stream, LispRef eos_error_p, LispRef eos_value)
{
    LispRef value_stack = content(stream, eos_error_p, eos_value);

    LispRef nvalue_stack;
    eul_read_int(nvalue_stack);

    LispRef context_stack = content(stream, eos_error_p, eos_value);

    LispRef ncontext_stack;
    eul_read_int(ncontext_stack);

    // Change relative program counters into absolute values
    LispRef vec = eul_nil + 1;
    while (vec != eul_nil)
    {
        int n = eul_int_as_c_int(eul_size_of(vec));
        for (int i = 1; i < n; i++)
        {
            LispRef fun = slot_ref(vec, i + 3);
            if (fun != eul_nil)
            {
                ptrInt rel_pc = (ptrInt)slot_ref(vec, i + 1);
                Instruction bv = (Instruction *) LAMBDA_CODE(fun);
                Instruction *pc = bv + rel_pc;
                slot_ref(vec, i + 1) = (LispRef) pc;
            }
        }
        vec = slot_ref(vec, 0);
    }

    return eul_serial_make_state
    (
        value_stack,
        nvalue_stack,
        context_stack,
        ncontext_stack
    );
}


/** ----------------------------------------------------------------- **
 ** newString:
 **   TC_STRING utf newHandle
 ** ----------------------------------------------------------------- **/

LispRef newString(LispRef stream, LispRef eos_error_p, LispRef eos_value)
{
    WITH_DEBUG(fprintf(stderr, "newString\n"));
    LispRef str;
    eul_read_utf(str);
    newHandle(stream, str);
    return str;
}


/** ----------------------------------------------------------------- **
 ** newSymbol:
 **   TC_Symbol utf
 ** ----------------------------------------------------------------- **/

LispRef newSymbol(LispRef stream, LispRef eos_error_p, LispRef eos_value)
{
    WITH_DEBUG(fprintf(stderr, "newSymbol\n"));
    char *str;
    read_utf(str);
    LispRef res;
    eul_intern_symbol(res, str);
    return res;
}


/** ----------------------------------------------------------------- **
 ** newKeyword:
 **   TC_KEYWORD utf
 ** ----------------------------------------------------------------- **/

LispRef newKeyword(LispRef stream, LispRef eos_error_p, LispRef eos_value)
{
    WITH_DEBUG(fprintf(stderr, "newKeyword\n"));
    char *str;
    read_utf(str);
    LispRef res;
    eul_intern_keyword(res, str);
    return res;
}


/** ----------------------------------------------------------------- **
 ** newInt:
 **   TC_INT word
 ** ----------------------------------------------------------------- **/

LispRef newInt(LispRef stream, LispRef eos_error_p, LispRef eos_value)
{

    WITH_DEBUG(fprintf(stderr, "newInt\n"));
    LispRef res;
    eul_read_int(res);
    return res;
}


/** ----------------------------------------------------------------- **
 ** newDouble:
 **   TC_DOUBLE word
 ** ----------------------------------------------------------------- **/

LispRef newDouble(LispRef stream, LispRef eos_error_p, LispRef eos_value)
{
    WITH_DEBUG(fprintf(stderr, "newDouble\n"));
    LispRef res;
    eul_read_double(res);
    return res;
}


/** ----------------------------------------------------------------- **
 ** newChar:
 **   TC_CHAR byte
 ** ----------------------------------------------------------------- **/

LispRef newChar(LispRef stream, LispRef eos_error_p, LispRef eos_value)
{
    WITH_DEBUG(fprintf(stderr, "newChar\n"));
    LispRef res;
    eul_read_byte(res);
    return res;
}


/** ----------------------------------------------------------------- **
 ** newStream:
 **   TC_STREAM byte
 ** ----------------------------------------------------------------- **/

LispRef newStream(LispRef stream, LispRef eos_error_p, LispRef eos_value)
{
    WITH_DEBUG(fprintf(stderr, "newStream\n"));
    LispRef mode = content(stream, eos_error_p, eos_value);
    LispRef file_name = content(stream, eos_error_p, eos_value);
    return eul_serial_make_file_stream(mode, file_name);
}


/** ----------------------------------------------------------------- **
 ** prevObject:
 **   TC_REFERENCE (int)handle
 ** ----------------------------------------------------------------- **/

LispRef prevObject(LispRef stream, LispRef eos_error_p, LispRef eos_value)
{
    WITH_DEBUG(fprintf(stderr, "prevObject\n"));

    short handle1;
    read_short(handle1);

    short handle2;
    read_short(handle2);

    WITH_DEBUG(fprintf(stderr, "  handle: %x %x\n", handle1, handle2));

    int handle = handle2;

    LispRef prev_objects = EUL_OBJECT_STREAM_CACHE(stream);

    int n = eul_int_as_c_int(object_size(prev_objects));

    if (handle > n)
    {
        LispRef str;
        eul_allocate_string(str, "bad handle");
        eul_serial_error(stream, str, eul_nil);

        return eul_nil;
    }

    LispRef obj = slot_ref(prev_objects, handle);

    fprintf(stderr, "*** CACHE GET (read) %d FOR ", handle);
    fprint_ref(stdout, obj);
    fflush(stdout);
    fprintf(stderr, "\n");

    return obj;
}


/** ----------------------------------------------------------------- **
 ** nullReference:
 **   TC_NULL
 ** ----------------------------------------------------------------- **/

LispRef nullReference(LispRef stream, LispRef eos_error_p, LispRef eos_value)
{
    WITH_DEBUG(fprintf(stderr, "nullReference\n"));
    return eul_nil;
}


/** ----------------------------------------------------------------- **
 ** reset:
 **   TC_RESET
 ** ----------------------------------------------------------------- **/

LispRef reset(LispRef stream, LispRef eos_error_p, LispRef eos_value)
{
    WITH_DEBUG(fprintf(stderr, "reset\n"));
    EUL_OBJECT_STREAM_CACHE_INDEX(stream) = c_int_as_eul_int(0);
    return eul_nil;
}


/** ----------------------------------------------------------------- **
 ** newHandle:
 ** ----------------------------------------------------------------- **/

void newHandle(LispRef stream, LispRef obj)
{
    WITH_DEBUG(fprintf(stderr, ">>> newHandle for "));
    WITH_DEBUG(fprint_ref(stderr, obj));
    WITH_DEBUG(fprintf(stderr, "\n"));

    int handle = eul_int_as_c_int(EUL_OBJECT_STREAM_CACHE_INDEX(stream));
    EUL_OBJECT_STREAM_CACHE_INDEX(stream) = c_int_as_eul_int(handle + 1);

    WITH_DEBUG(fprintf(stderr, "  handle: %x\n", handle));

    LispRef prev_objects = EUL_OBJECT_STREAM_CACHE(stream);
    int n = eul_int_as_c_int(eul_size_of(prev_objects));

    WITH_DEBUG(fprintf(stderr, "  cache size: %d\n", n));

    // Check if cache is large enough
    if (n <= handle)
    {
        unsigned int m = 2 * (n + 1);

        LispRef new_prev_objects;

        eul_allocate_vector(new_prev_objects, m, eul_nil);

        WITH_DEBUG(fprintf(stderr, "  Extending cache ...\n"));

        for (int i = 0; i < n; i++)
        {
            slot_ref(new_prev_objects, i) = slot_ref(prev_objects, i);
        }

        EUL_OBJECT_STREAM_CACHE(stream) = prev_objects = new_prev_objects;
    }

    fprintf(stderr, "*** CACHE PUT (read) %d FOR ", handle);
    fprint_ref(stdout, obj);
    fflush(stdout);
    fprintf(stderr, "\n");

    // Store object in cache
    slot_ref(prev_objects, handle) = obj;
}


/** ----------------------------------------------------------------------- **/
