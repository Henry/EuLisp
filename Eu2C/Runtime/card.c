/// Copyright 1994-2010 Fraunhofer ISST
/// Copyright 2010 Henry G. Weller
///-----------------------------------------------------------------------------
//  This file is part of
/// ---                           EuLisp System 'Eu2C'
///-----------------------------------------------------------------------------
//
//  Eu2C is free software: you can redistribute it and/or modify it under the
//  terms of the GNU General Public License version 2 as published by the Free
//  Software Foundation.
//
//  Eu2C is distributed in the hope that it will be useful, but WITHOUT ANY
//  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
//  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
//  details.
//
//  You should have received a copy of the GNU General Public License along with
//  this program.  If not, see <http://www.gnu.org/licenses/>.
//
///-----------------------------------------------------------------------------
/// Title: xalloc card and type description
///  Library: Runtime
///  Authors: Jens Bimberg
///  Description:
///    contains the user interface to the internal card and type arrays
///-----------------------------------------------------------------------------
#include <stdio.h>
#include <stdlib.h>
#include "xalloc.h"

static int initialized = 0;

static int lowest_type_descriptor = SMALLEST_TYPEDSCR;
static int lowest_card_descriptor = SMALLEST_CARDDSCR;

void initialize()
{
    int cd;
    initialize_heap();
    initialized = 1;
    last_used_type = lowest_type_descriptor;
    last_used_card = lowest_card_descriptor;
    used_cards_end = usedcards;
    for (cd = SMALLEST_CARDDSCR; cd < HIGHEST_CARDDSCR; cd++)
    {
        store_free_object(cd, NULL);
        store_card_d(cd, NULL);
    }
}

int set_lowest_type_descriptor(value)
    TypeDscr value;
{
    if (!initialized)
    {
        lowest_type_descriptor = value;
        return 1;
    }
    else
    {
        return 0;
    }
}

TypeDscr set_type_descriptor(td, t, fcn)
    register TypeDscr td;
    register unsigned long t;
    register void *(*fcn) ();   /* void* instead of void to be compatible to
                                 * Apply */
{
    if (!initialized)
        initialize();
#ifdef SECURITY_FIRST
    if (td >= lowest_type_descriptor)
    {
        printf("\ntds %ld\n", td);
        error("set_type_descriptor: td is out of range");
    }
#endif
    DISABLE_SCHEDULE();
    store_user_type(td, t);
    store_mark_fcn(td, (void (*)())fcn);
    ENABLE_SCHEDULE();
    return td;
}

TypeDscr describe_type(t, fcn)
    register unsigned long t;
    register void *(*fcn) ();   /* void* instead of void to be compatible to
                                 * Apply */
{
    register TypeDscr i;
    if (!initialized)
        initialize();
    DISABLE_SCHEDULE();
    /* check whether type already in use */
    for (i = 1; i < last_used_type; i++)
    {
        if (t == get_user_type(i))
        {
#ifdef SECURITY_FIRST
            if ((void (*)())fcn != get_mark_fcn(i))
                error("describe_type: Type defined twice");
#endif
            return (i);
        }
    }
    /* i is now last_used_type, record new type */

#ifdef  SECURITY_FIRST
    if (i > HIGHEST_TYPEDSCR)
        error("describe_type: To many different types");
#endif

    store_user_type(i, t);
    store_mark_fcn(i, (void (*)())fcn);
    last_used_type++;
    ENABLE_SCHEDULE();
    return (i);
}

int set_lowest_card_descriptor(value)
    CardDscr value;
{
    if (!initialized)
    {
        lowest_card_descriptor = value;
        return 1;
    }
    else
    {
        return 0;
    }
}

CardDscr set_card_descriptor(cd, c_type, o_size, t_dscr)
    register CardDscr cd;
    register char c_type;
    register long o_size;
    register TypeDscr t_dscr;
{  /* lot of duplicated code from next function */
    if (!initialized)
        initialize();
#ifdef  SECURITY_FIRST
    if (cd >= lowest_card_descriptor)
        error("set_card_descriptor: cd is out of range");
    if ((c_type != MTSS) && (!is_tdscr(t_dscr)))
        error("set_card_descriptor: unknown t_dscr");
    if ((c_type != STMS) && (o_size <= 0))
        error("set_card_descriptor: negative or zero size");
#ifndef USE_STSS
    if (c_type == STSS)
        error("set_card_descriptor: USE_STSS undefined");
#endif
#ifndef USE_MTSS
    if (c_type == MTSS)
        error("set_card_descriptor: USE_MTSS undefined");
#endif
#ifndef USE_STMS
    if (c_type == STMS)
        error("set_card_descriptor: USE_STMS undefined");
#endif
#endif /* SECURITY_FIRST */

    o_size = byte2word(o_size);
    switch (c_type)
    {
#ifdef  USE_MTSS
        case MTSS:
            t_dscr = (TypeDscr) NULL;
            o_size = MT_ALIGN(o_size);
#ifdef  OPTIMIZED_POINTER_TESTS
            /* increase small object sizes up to powers of 2 */
            if (o_size == 3)
                o_size = 4;
            else if ((o_size > 4) && (o_size < 8))
                o_size = 8;
#endif
            break;
#endif
#ifdef  USE_STSS
        case STSS:
#ifdef  OPTIMIZED_POINTER_TESTS
            if (o_size == 3)
                o_size = 4;
            else if ((o_size > 4) && (o_size < 8))
                o_size = 8;
#endif
            break;
#endif
#ifdef  USE_STMS
        case STMS:
            o_size = 0;
            break;
#endif
        default:
            error("set_card_descriptor: Undefined Card-Type");
    }

#ifdef  SECURITY_FIRST
#ifndef USE_LARGE
    if (o_size > USERSPACE)
        error("set_card_descriptor: size to large");
#endif
#endif
    DISABLE_SCHEDULE();
    store_type_d(cd, t_dscr);
    store_size_d(cd, o_size);
    /* store_card_d(cd, (card *)NULL); First card of this carddscr */
    /* store_free_object(cd,NULL); First free object of this carddscr */
    ENABLE_SCHEDULE();
    return cd;
}


CardDscr describe_card(c_type, o_size, t_dscr)
    register char c_type;
    register long o_size;
    register TypeDscr t_dscr;
{
    register CardDscr i;
    if (!initialized)
        initialize();
#ifdef  SECURITY_FIRST
    if ((c_type != MTSS) && (!is_tdscr(t_dscr)))
        error("describe_card: unknown t_dscr");
    if ((c_type != STMS) && (o_size <= 0))
        error("describe_card: negative or zero size");
#ifndef USE_STSS
    if (c_type == STSS)
        error("describe_card: USE_STSS undefined");
#endif
#ifndef USE_MTSS
    if (c_type == MTSS)
        error("describe_card: USE_MTSS undefined");
#endif
#ifndef USE_STMS
    if (c_type == STMS)
        error("describe_card: USE_STMS undefined");
#endif
#endif /* SECURITY_FIRST */

    o_size = byte2word(o_size);
    switch (c_type)
    {
#ifdef  USE_MTSS
        case MTSS:
            t_dscr = (TypeDscr) NULL;
            o_size = MT_ALIGN(o_size);
#ifdef  OPTIMIZED_POINTER_TESTS
            /* increase small object sizes up to powers of 2 */
            if (o_size == 3)
                o_size = 4;
            else if ((o_size > 4) && (o_size < 8))
                o_size = 8;
#endif
            break;
#endif
#ifdef  USE_STSS
        case STSS:
#ifdef  OPTIMIZED_POINTER_TESTS
            if (o_size == 3)
                o_size = 4;
            else if ((o_size > 4) && (o_size < 8))
                o_size = 8;
#endif
            break;
#endif
#ifdef  USE_STMS
        case STMS:
            o_size = 0;
            break;
#endif
        default:
            error("describe_card: Undefined Card-Type");
    }

#ifdef  SECURITY_FIRST
#ifndef USE_LARGE
    if (o_size > USERSPACE)
        error("describe_card: size to large");
#endif
#endif
    DISABLE_SCHEDULE();
    /* Test whether this card_description already exists */
    for (i = 0; i < last_used_card; i++)
        if ((get_type_d(i) == t_dscr) && (get_size_d(i) == o_size))
            return (i);
    /* i is now last_used_card, record card information */

#ifdef  SECURITY_FIRST
    if (i > HIGHEST_CARDDSCR)
        error("describe_card: To many different card-descriptors");
#endif

    store_type_d(i, t_dscr);
    store_size_d(i, o_size);
    /* store_card_d(i, (card *)NULL); First card of this carddscr */
    /* store_free_object(i,NULL); First free object of this carddscr */
    last_used_card++;
    ENABLE_SCHEDULE();
    return (i);
}

unsigned long safe_get_type(p)
    register long *p;
{
    register card *c;
    if (p_in_heap(p))
    {
        c = CARD_ADDR(p);
        if (living_card(c))
        {
            if (p >= c->userspace)
            {
                #ifdef  USE_STMS
                if (ms_card(c))
                {
                    if (consistent_ms_pointer(c, p))
                    {
                        return (get_user_type(get_tdscr(c)));
                    }
                    else
                    {
                        return (NO_CONSISTENT_POINTER);
                    }
                }
                #endif
                #ifdef  USE_MTSS
                if (mt_card(c))
                {
                    long *fp = p - 1;
                    if (consistent_pointer(c, fp, get_size(c), get_mask(c)))
                    {
                        if (is_tdscr(*fp))
                        {
                            return (get_user_type(*fp));
                        }
                        else
                        {
                            return (NO_CONSISTENT_POINTER);
                        }
                    }
                }
                #endif
                #ifdef  USE_STSS
                if (consistent_pointer(c, p, get_size(c), get_mask(c)))
                {
                    return (get_user_type(get_tdscr(c)));
                }
                else
                {
                    return (NO_CONSISTENT_POINTER);
                }
                #endif
            }
        }
    }
    return (NO_CONSISTENT_POINTER);
}

unsigned long get_type(register long *p)
{
    register card *c = CARD_ADDR(p);
    #ifdef  USE_MTSS
    if (mt_card(c))
        return (get_user_type((TypeDscr) (*(--p))));
    #endif
    return (get_user_type(get_tdscr(c)));
}

char get_card_type(cd)
    register CardDscr cd;
{
#ifdef  USE_MTSS
    if (mt_card_d(cd))
        return (MTSS);
#endif
#ifdef  USE_STMS
    if (ms_card_d(cd))
        return (STMS);
#endif
    return (STSS);
}

TypeDscr get_type_descriptor(ptr)
    register long *ptr;
{
#ifdef USE_MTSS
    if (mt_card(CARD_ADDR(ptr)))
        return ((TypeDscr) (*(--ptr)));
#endif
    return ((TypeDscr) get_tdscr(CARD_ADDR(ptr)));
}

unsigned long type_from_descriptor(td)
    TypeDscr td;
{
    return (get_user_type(td));
}

long get_object_size(p)
    void *p;
{
    card *c = CARD_ADDR(p);
    if (ms_card(c))
        return get_ms_real_size(p);
    else
        return get_real_size(c);
}

void add_static_card(c, fuw, cd)
    card *c;                    /* card address */
    long *fuw;                  /* first used word */
    CardDscr cd;
{
    long *en;
#ifndef ROOT_SET_IN_USE
    error("static cards migth not be used whithout root set");
#endif
#ifdef SECURITY_FIRST
    /* check whether c is a multiple of CARDSIZE */
    if ((long)c & CARDMASK)
        error("add_static_card: wrong cardaddress");
    /* check whether fuw is pointer to that card */
    if ((fuw < c->userspace) || (fuw >= (long *)(c + 1)) || ((long)fuw & 3))
        error("add_static_card: wrong first used word");
    /* check whether cd is an appropriate card_descriptor */
    if (!is_cdscr(cd))
        error("add_static_card: wrong card descriptor");
    /* check whether we have room for this card */
    if (curnumcard >= MAX_NUM_OF_CARDS)
        error("add_static_card: card list overflow");
    /* make sure, that the hole between c->userspace and fuw is not 1 word on
     * stms-cards */
    if (ms_card_d(cd) && (fuw == (c->userspace + 1)))
        error("add_static_card: hole of one word on stms-card");
#endif
    DISABLE_SCHEDULE();
    /* decrease HEAPBEGIN, static cards are not assumed to start on higher
     * addresses than our real heap */
    HEAPBEGIN = min(HEAPBEGIN, (long *)c);
    /* increase curnumcard by one, don't worry that the card may be large */
    curnumcard++;
    if (!ms_card_d(cd))
    {   /* single size card */
#ifdef USE_LARGE
        if (get_size_d(cd) > USERSPACE)
        {       /* large object? */
            register_card(c);
            put_tdscr(c, get_type_d(cd));
            put_size(c, get_size_d(cd));
            put_real_size(c, (get_size_d(cd) - (mt_card_d(cd) ? 1 : 0))
                * sizeof(long));
            put_cardnum(c, basiccards(get_size_d(cd)));
            into_card_list(cd, c);
            set_default_mask(c);
            large_unmark(c);
            return;
        }
#endif /* normal length single-size-card */
        register_card(c);
        put_tdscr(c, get_type_d(cd));
        put_size(c, get_size_d(cd));
        put_real_size(c, (get_size_d(cd) - (mt_card_d(cd) ? 1 : 0))
            * sizeof(long));
        put_cardnum(c, 1);
        into_card_list(cd, c);
        if (fuw > c->userspace) /* add free part to freelist */
            store_free_object(cd, build_free_list(c, fuw, cd));
        set_mask(c);
        clear_marks(c);
        ENABLE_SCHEDULE();
        return;
    }
    /* multi size card */
    register_card(c);
    put_tdscr(c, get_type_d(cd));
    put_size(c, 0);
    put_real_size(c, 0);
    put_cardnum(c, 1);  /* may be changed afterwards */
    into_card_list(cd, c);
    if (fuw > c->userspace)
    {   /* add free part to freelist */
        ((vobject *) (c->userspace))->vlength = fuw - c->userspace;
        ((vobject *) (c->userspace))->vnext = (vobject *) get_free_object(cd);
        store_free_object(cd, c->userspace);
    }
    set_default_mask(c);
    clear_marks(c);
    /* the remaining part is to mark all objects. we assume, that we find them
     * all by following fuw and the numbers in the first words of the objetcs.
     * we either reach or exceed next card. in the latter case we must update
     * cardnum afterwards */
    en = (long *)(c + 1);
    while (fuw < en)
    {
        mark(c, fuw);   /* begin of an object */
        fuw += *fuw;    /* next object */
    }
    if (fuw > en)       /* exceeded: update cardnum */
        put_cardnum(c, CARD_ADDR(fuw) - c + 1);
    ENABLE_SCHEDULE();
    return;
}
