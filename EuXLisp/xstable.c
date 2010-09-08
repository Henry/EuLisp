/// Copyright (c) 1994, by Russell Bradford. All rights reserved.
///-----------------------------------------------------------------------------
/// ---                 EuLisp System 'EuXLisp'
///-----------------------------------------------------------------------------
///  File: xstable.c
///  Description: tables
///-----------------------------------------------------------------------------

#include "xscheme.h"
#include "xssymbols.h"
#include "xsproto.h"

// forward declarations
static int thash_eq(LVAL key);
static int thash_eqv(LVAL key);
static int thash_equal(LVAL key);
static int thash_equals(LVAL key);

// xmake_table - make a table
LVAL xmake_table()
{
    static char *cfn_name = "make-table";

    LVAL eqp = getvalue(s_eq);
    LVAL eqvp = getvalue(s_eqv);
    LVAL equalp = getvalue(s_equal);
    LVAL equals = getvalue(s_equals);

    LVAL comp = eqvp;
    LVAL fill = NIL;

    if (moreargs())
    {
        comp = xlgetarg();
        if (moreargs())
        {
            fill = xlgetarg();
            xllastarg();
        }
    }

    if (comp != eqp && comp != eqvp && comp != equalp && comp != equals)
    {
        xlcerror
        (
            "only eq, eqv, = and equal allowed as table comparators",
            comp,
            NIL
        );
    }

    return cvtable(comp, fill);
}

// xtable_ref - get an elt from a table
LVAL xtable_ref()
{
    static char *cfn_name = "table-ref";

    int (*equality) ();

    LVAL table = xlgatable();
    LVAL key = xlgetarg();
    xllastarg();

    LVAL comp = gettablecomp(table);
    LVAL vec = gettabletable(table);

    check(2);

    int index;
    if (comp == getvalue(s_eq))
    {
        index = thash_eq(key);
        equality = eq;
    }
    else if (comp == getvalue(s_eqv))
    {
        index = thash_eqv(key);
        equality = eqv;
    }
    else if (comp == getvalue(s_equals))
    {
        index = thash_equals(key);
        equality = equals;
    }
    else
    {
        index = thash_equal(key);
        equality = equal;
    }

    push(getelement(vec, index));
    push(key);
    xlargc = 2;
    LVAL val = assoc(equality);

    if (val == NIL)
    {
        return gettablefill(table);
    }
    else
    {
        return cdr(val);
    }

}

// xtable_set - put an elt into a table
LVAL xtable_set()
{
    static char *cfn_name = "table-set!";

    int (*equality) ();

    LVAL table = xlgatable();
    LVAL key = xlgetarg();
    LVAL value = xlgetarg();
    xllastarg();

    LVAL comp = gettablecomp(table);
    LVAL vec = gettabletable(table);

    check(3);

    int index;
    if (comp == getvalue(s_eq))
    {
        index = thash_eq(key);
        equality = eq;
    }
    else if (comp == getvalue(s_eqv))
    {
        index = thash_eqv(key);
        equality = eqv;
    }
    else if (comp == getvalue(s_equals))
    {
        index = thash_equals(key);
        equality = equals;
    }
    else
    {
        index = thash_equal(key);
        equality = equal;
    }

    push(getelement(vec, index));
    push(key);
    xlargc = 2;
    LVAL val = assoc(equality);

    if (val == NIL)  // new key
    {
        push(table);
        push(key);
        push(value);
        val = cons(key, value);
        val = cons(val, getelement(vec, index));
        setelement(vec, index, val);
        drop(3);
    }
    else        // replace old
    {
        rplacd(val, value);
    }

    return value;
}

// xtable_comparator - return the table comparator
LVAL xtable_comparator()
{
    static char *cfn_name = "table-comparator";

    LVAL table = xlgatable();
    xllastarg();

    return gettablecomp(table);
}

// nasty destructive delete
static LVAL delq(LVAL list, LVAL obj)
{
    if (list == NIL)
    {
        return NIL;
    }

    if (car(list) == obj)
    {
        return cdr(list);
    }

    LVAL before, after;
    for
    (
        before = list, after = cdr(list);
        after;
        before = cdr(before), after = cdr(after)
    )
    {
        if (car(after) == obj)
        {
            rplacd(before, cdr(after));
            return list;
        }
    }

    return list;
}

// xtable_delete - remove an object from a table
LVAL xtable_delete()
{
    static char *cfn_name = "table-delete";

    int (*equality) ();

    LVAL table = xlgatable();
    LVAL key = xlgetarg();
    xllastarg();

    LVAL comp = gettablecomp(table);
    LVAL vec = gettabletable(table);

    check(2);

    int index;
    if (comp == getvalue(s_eq))
    {
        index = thash_eq(key);
        equality = eq;
    }

    if (comp == getvalue(s_eqv))
    {
        index = thash_eqv(key);
        equality = eqv;
    }

    if (comp == getvalue(s_equals))
    {
        index = thash_equals(key);
        equality = equals;
    }
    else
    {
        index = thash_equal(key);
        equality = equal;
    }

    push(getelement(vec, index));
    push(key);
    xlargc = 2;
    LVAL val = assoc(equality);

    if (val == NIL)
    {
        return NIL;
    }

    setelement(vec, index, delq(getelement(vec, index), val));

    return cdr(val);
}

// xtable_size - the number of objects in the table
LVAL xtable_size()
{
    static char *cfn_name = "table-size";

    LVAL table = xlgatable();
    xllastarg();

    LVAL vec = gettabletable(table);

    int count = 0;
    for (int i = 0; i < HTABLESIZE; i++)
    {
        LVAL entry = getelement(vec, i);
        for (; entry; entry = cdr(entry))
        {
            count++;
        }
    }

    return cvfixnum(count);
}

// xtable_keys - a list of the keys for objects in the table
LVAL xtable_keys()
{
    static char *cfn_name = "table-keys";

    LVAL table = xlgatable();
    xllastarg();

    LVAL vec = gettabletable(table);

    cpush(table);

    LVAL keylist = NIL;
    for (int i = 0; i < HTABLESIZE; i++)
    {
        LVAL entry = getelement(vec, i);
        for (; entry; entry = cdr(entry))
        {
            keylist = cons(car(car(entry)), keylist);
        }
    }
    drop(1);

    return keylist;
}

// xtable_values - a list of the objects in the table
LVAL xtable_values()
{
    static char *cfn_name = "table-values";

    LVAL table = xlgatable();
    xllastarg();

    LVAL vec = gettabletable(table);

    cpush(table);

    LVAL vallist = NIL;
    for (int i = 0; i < HTABLESIZE; i++)
    {
        LVAL entry = getelement(vec, i);
        for (; entry; entry = cdr(entry))
        {
            vallist = cons(cdr(car(entry)), vallist);
        }
    }
    drop(1);

    return vallist;
}

// xtable_fill - the table-fill value
LVAL xtable_fill()
{
    static char *cfn_name = "table-fill";

    LVAL table = xlgatable();
    xllastarg();

    return gettablefill(table);
}

// xtable_setfill - set the table-fill value
LVAL xtable_setfill()
{
    static char *cfn_name = "set-table-fill!";

    LVAL table = xlgatable();
    LVAL val = xlgetarg();
    xllastarg();

    settablefill(table, val);
    return val;
}

// xtable_clear - remove all entries
LVAL xtable_clear()
{
    static char *cfn_name = "table-clear";

    LVAL table = xlgatable();
    xllastarg();

    LVAL vec = gettabletable(table);

    for (int i = 0; i < HTABLESIZE; i++)
    {
        setelement(vec, i, NIL);
    }

    return table;
}

// things may be relocated on image save/restore, thus use
// cvoptr rather than direct address: slow perhaps, as hashing time is
// proportional to number of live segments
static int thash_eq(LVAL key)
{
    extern OFFTYPE cvoptr();

    int hash = cvoptr(symbolp(key) ? getpname(key) : key)
    % HTABLESIZE;

    #if 0
    xlprin1(key, xstdout());
    xlputstr(xstdout(), "->");
    xlprin1(cvfixnum(hash), xstdout());
    xlterpri(xstdout());
    #endif

    return hash;
}

// c.f. code for eqv in xsftab.c
static int thash_eqv(LVAL key)
{
    if (key == NIL)
    {
        return 0;
    }

    int val = 0;
    switch (ntype(key))
    {
        case FIXNUM:
            val = getfixnum(key) % HTABLESIZE;
            break;

        case FLONUM:
            val = (int)(getflonum(key)) % HTABLESIZE;
            break;

        case CHAR:
            val = getchcode(key) % HTABLESIZE;
            break;

        default:
            val = thash_eq(key);
            break;
    }

    return val < 0 ? -val : val;
}

static int thash_equals(LVAL key)
{
    int val = 0;
    switch (ntype(key))
    {
        case FIXNUM:
            val = getfixnum(key) % HTABLESIZE;
            break;

        case FLONUM:
            val = (int)(getflonum(key)) % HTABLESIZE;
            break;

        case CHAR:
            val = getchcode(key) % HTABLESIZE;
            break;

        case STRING:
            val = hash(getstring(key), HTABLESIZE);     // from xlenter
            break;

        default:
            xlcerror
            (
                "table ref on key incompatible with comparator =",
                key, NIL
            );
    }

    return val < 0 ? -val : val;
}

// c.f. code for equal in xsftab.c
/* this has problems on user-defined classes, where the user has
 * created new methods on equal.  We could search the entire table,
 * I suppose. :-(
 * (defmethod equal ((a <integer>) (b <foo>)) t)
 */
static int thash_equal(LVAL key)
{
    if (key == NIL)
    {
        return 0;
    }

    int val = 0, len = 0;
    switch (ntype(key))
    {
        case FIXNUM:
            val = getfixnum(key) % HTABLESIZE;
            break;

        case FLONUM:
            val = (int)(getflonum(key)) % HTABLESIZE;
            break;

        case CHAR:
            val = getchcode(key) % HTABLESIZE;
            break;

        case STRING:
            val = hash(getstring(key), HTABLESIZE);     // from xlenter
            break;

        case VECTOR:
            len = getsize(key);
            val = 0;
            for (int i = 0; i < len; i++)
                val = (val + thash_equal(getelement(key, i))) % HTABLESIZE;
            break;

        case CONS:
            val = (thash_equal(car(key)) + thash_equal(cdr(key))) % HTABLESIZE;
            break;

        default:
            val = thash_eq(key);
            break;
    }

    return val < 0 ? -val : val;
}


///-----------------------------------------------------------------------------
