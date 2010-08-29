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
///  Title: xalloc allocate functions
///  Library: Runtime
///  Authors: Jens Bimberg
///-----------------------------------------------------------------------------

#include <stdio.h>
#include <stdlib.h>
#include "xalloc.h"

extern void garbage_collection();
extern long ss_sweep(register card *c, register CardDscr cd);
extern long ms_sweep(register card *c, register CardDscr cd);

#ifdef SKIP_SWEEP
#ifdef MULTI_FSL
#define as_active_card(cd, c){                                                 \
        card * c1;                                                             \
        if((c1 = get_active(cd)))                                              \
            into_full_list(cd, c1);                                            \
        store_active(cd, c);}
#define flush_active(cd)        as_active_card(cd, NULL)
#else
#define as_active_card(cd, c)   into_full_list(cd, c)
#define flush_active(cd)
#endif
#else
#define as_active_card(cd, c)   into_card_list(cd, c)
#define flush_active(cd)
#endif

#ifdef MULTI_FSL
#define ALLOCATION_BEGIN()
#define ALLOCATION_END()
#define DEEP_ALLOC_BEGIN()      DISABLE_SCHEDULE()
#define DEEP_ALLOC_END()        ENABLE_SCHEDULE()
#else
#define ALLOCATION_BEGIN()      DISABLE_SCHEDULE()
#define ALLOCATION_END()        ENABLE_SCHEDULE()
#define DEEP_ALLOC_BEGIN()
#define DEEP_ALLOC_END()
#endif


#ifdef SKIP_SWEEP
extern int allocated_cards;
#define inc_allocated_cards(n)  allocated_cards += (n)
#else
#define inc_allocated_cards(n)
#endif

long *build_free_list(c, pt, cd)        /* a function to be used in the next
                                         * macro: builds an and returns pointer
                                         * to the free list on card c */
    register card *c;
    register long *pt;
    register CardDscr cd;
{
    register long stp = get_size_d(cd);
    register long *en = c->userspace;
    register long *fsl = get_free_object(cd);
    pt -= stp;
    while (pt >= en)
    {
        *pt = (long)fsl;
        fsl = pt;
        pt -= stp;
    }
    return (pt + stp);
}

void prepare_ss_card(CardDscr cd, card * c, int o)
{
    inc_allocated_cards(1);
    register_card(c);
    as_active_card(cd, c);
    put_tdscr(c, get_type_d(cd));
    put_size(c, get_size_d(cd));
    put_real_size(c, (get_size_d(cd) - o) * sizeof(long));
    put_cardnum(c, 1);
    store_free_object(cd, build_free_list(c, c + 1, cd));
    set_mask(c);
    clear_marks(c);
}

#ifdef  USE_STMS
void prepare_ms_card(CardDscr cd, card * c, int n)
{
    inc_allocated_cards(n);
    register_card(c);
    as_active_card(cd, c);
    put_tdscr(c, get_type_d(cd));
    put_size(c, 0);
    put_real_size(c, 0);
    put_cardnum(c, n);
    ((vobject *) (c->userspace))->vlength = USERSPACE;
    ((vobject *) (c->userspace))->vnext = (vobject *) get_free_object(cd);
    store_free_object(cd, c->userspace);
    set_default_mask(c);
    clear_marks(c);
}
#endif /* defined USE_STMS */

#ifdef USE_LARGE
void prepare_large_ss_card(CardDscr cd, card * c, int o)
{
    inc_allocated_cards(basiccards(get_size_d(cd)));
    register_card(c);
    as_active_card(cd, c);
    put_tdscr(c, get_type_d(cd));
    put_size(c, get_size_d(cd));
    put_real_size(c, (get_size_d(cd) - o) * sizeof(long));
    put_cardnum(c, basiccards(get_size_d(cd)));
    set_default_mask(c);
    large_unmark(c);
}
#endif

#ifdef SKIP_SWEEP
#define sweep_and_search(cd, c){                                               \
        while((c = get_card_d(cd))){                                           \
            off_card_list(cd, c);                                              \
            as_active_card(cd, c);                                             \
            ss_sweep(c, cd);                                                   \
            if((rv = get_free_object(cd))){                                    \
                DEEP_ALLOC_END();                                              \
                goto skip;                                                     \
            }                                                                  \
        }                                                                      \
        flush_active(cd);}

#define search_large_object(cd, c){                                            \
        while((c = get_card_d(cd))){                                           \
            off_card_list(cd, c);                                              \
            into_full_list(cd, c);                                             \
            if(! ismarked(c,c->userspace)){                                    \
                DEEP_ALLOC_END();                                              \
                goto skip;                                                     \
            }                                                                  \
        }}

#define search_before_gc(cd, c) sweep_and_search(cd, c)
#define search_after_gc(cd, c)  sweep_and_search(cd, c)
#define search_large(cd, c)     search_large_object(cd, c)
#else
#define search_before_gc(cd, c)
#define search_after_gc(cd, c)  if((rv = get_free_object(cd))) goto skip
#define search_large(cd, c)
#endif


#define inc_ss_card_space(cd,o){                                               \
        register card *c;                                                      \
        search_before_gc(cd, c);                                               \
        if((c = (card *)new_card()) == (card *)NULL){                          \
            garbage_collection();                                              \
            search_after_gc(cd, c);                                            \
            if((c = (card *)new_card()) == (card *)NULL){                      \
                if(!inc_heap())                                                \
                    error("xalloc: Out of storage");                           \
                c = (card *)new_card();                                        \
            }                                                                  \
        }                                                                      \
        prepare_ss_card(cd, c, o);}


#ifdef  USE_LARGE
#define get_large_card(c,n){                                                   \
        if((c = (card *)new_large_card(n)) == (card *)NULL){                   \
            inc_heap_size(max(n,hincr));                                       \
            if((c = (card *)new_large_card(n)) == (card *)NULL)                \
                error("get_large_card: Out of storage");                       \
        }}

#define install_large_ss_card(cd,c,o){                                         \
        search_large(cd, c)                                                    \
        get_large_card(c,basiccards(get_size_d(cd)));                          \
        prepare_large_ss_card(cd, c, o);}
#endif


#ifdef  USE_STSS

void *xalloc_stss(CardDscr cd)           /* returns pointer to free object of carddscr
                                          * cd */
{  /* This fctn. is very fast, because in most cases only those instructions
    * marked with an asterisk at the and of the line will be processed (, if
    * you've decided not to use our security tests). */

    register long *rv;

#ifdef  SECURITY_FIRST
    if (!is_cdscr(cd))
        error1("xalloc_stss: unknown carddescriptor %ld\nBye", cd);
    if (ms_card_d(cd))
        error1("xalloc_stss: stms-carddescriptor %ld\nBye", cd);
    if (mt_card_d(cd))
        error1("xalloc_stss: mtss-carddescriptor %ld\nBye", cd);
#endif
    ALLOCATION_BEGIN();
    if (!(rv = get_free_object(cd)))
    {   /* empty free list *//* * */
        DEEP_ALLOC_BEGIN();
#ifdef  USE_LARGE
        if (get_size_d(cd) > USERSPACE)
        {       /* large object? */
            card *c;
            install_large_ss_card(cd, c, 0);
            DEEP_ALLOC_END();
            ALLOCATION_END();
            return ((void *)(c->userspace));
        }
        else
#endif
        {
            inc_ss_card_space(cd, 0);
            rv = get_free_object(cd);
        }
        DEEP_ALLOC_END();
    }
    skip:store_free_object(cd, (long *)*rv);
                                        /* next into free list *//* * */
    ALLOCATION_END();
    return ((void *)rv);        /* * */
}

#endif

#ifdef  USE_MTSS

void *xalloc_mtss(cd, td)       /* returns pointer to free object of carddscr
                                 * cd, prepared with typedscr td */
     CardDscr cd;
    TypeDscr td;
{
    /* nearly the same as the above fctn., but puts the typedescriptor into the
     * word ahead the return value */

    register long *rv;


#ifdef  SECURITY_FIRST
    if (!is_cdscr(cd))
        error1("xalloc_mtss: unknown carddescriptor %ld\nBye", cd);
    if (!is_tdscr(td))
        error1("xalloc_mtss: unknown typedescriptor %ld\nBye", cd);
    if (ms_card_d(cd))
        error1("xalloc_mtss: stms-carddescriptor %ld\nBye", cd);
    if (!mt_card_d(cd))
        error1("xalloc_mtss: stss-carddescriptor %ld\nBye", cd);
#endif
    ALLOCATION_BEGIN();
    if (!(rv = get_free_object(cd)))
    {   /* empty free list */
        DEEP_ALLOC_BEGIN();
#ifdef  USE_LARGE
        if (get_size_d(cd) > USERSPACE)
        {       /* large object? */
            card *c;
            install_large_ss_card(cd, c, MT_ALIGN_WORDS);
            c->userspace[MT_ALIGN_WORDS - 1] = (long)td;
            DEEP_ALLOC_END();
            ALLOCATION_END();
            return ((void *)(c->userspace + MT_ALIGN_WORDS));
        }
        else
#endif
        {
            inc_ss_card_space(cd, MT_ALIGN_WORDS);
            rv = get_free_object(cd);   /* successful */
        }
        DEEP_ALLOC_END();
    }
    skip:store_free_object(cd, (long *)*rv);
                                        /* next into free list */
    ALLOCATION_END();
    rv[MT_ALIGN_WORDS - 1] = (long)td;  /* store type before return */
    return ((void *)(rv + MT_ALIGN_WORDS));     /* return second aligned word */
}

#endif

#ifdef  USE_STMS

void *xalloc_stms(cd, i)        /* returns pointer to free object of carddscr
                                 * cd, with length i bytes */
     register CardDscr cd;
    register long i;
{
    /* this is really something else, because any found object may be to short.
     * This causes the loop in this function and makes it in general much
     * slower than the both above. if def. fast_stms we prefer gc to long
     * search times */

    register vobject *space, *last;
    register long diff;
    register long sz = byte2word(i) + 1;
    register card *c;
    char gc_just_done = 0;


#ifdef  SECURITY_FIRST
    if (!is_cdscr(cd))
        error1("xalloc_stms: unknown carddescriptor %ld\nBye", cd);
    if (mt_card_d(cd))
        error1("xalloc_stms: mtss-carddescriptor %ld\nBye", cd);
    if (!ms_card_d(cd))
        error1("xalloc_stms: stss-carddescriptor %ld\nBye", cd);
#ifndef USE_LARGE
    if (sz > USERSPACE)
        error("xalloc_stms: size to large");
#endif
#endif /* security first */
    ALLOCATION_BEGIN();
#ifdef  USE_LARGE
    if (sz <= USERSPACE)
    {
#endif
        while (1)
        {       /* start of trying to get a suitable space */
            last = (vobject *) get_freept_addr(cd);
            while ((space = last->vnext) && (space->vlength < sz))
            {
#ifdef FAST_STMS
                last->vnext = space->vnext;     /* remv. space from free list */
#else
                last = space;
#endif
            }
            if (space)
                break;  /* suitable space found */

            DEEP_ALLOC_BEGIN();
#ifdef SKIP_SWEEP
            /* take the next card - if any - and proceed */
            if ((c = get_card_d(cd)))
            {
                off_card_list(cd, c);
                as_active_card(cd, c);
                ms_sweep(c, cd);
                DEEP_ALLOC_END();
                continue;
            }
            flush_active(cd);
#endif
            if ((c = (card *) new_card()) == (card *) NULL)
            {
                if (!gc_just_done)
                {       /* don't collect twice */
                    garbage_collection();       /* call to trace.c */
                    DEEP_ALLOC_END();
                    gc_just_done = 1;   /* prevent from further collection */
                    continue;   /* don't inc_ms_card_space */
                }
                else
                {
                    if (!inc_heap())    /* call to heap.c */
                        error("xalloc_stms: Out of storage");
                    c = (card *) new_card();    /* call to heap.c */
                }
            }
            prepare_ms_card(cd, c, 1);  /* look above for def of this */
            DEEP_ALLOC_END();
        }
        /* found a suitable space */
        if ((diff = space->vlength - sz) < 2)
        {
            /* no or only one basic element left: connect parts of free list */
            last->vnext = space->vnext;
            /* set mark bit to mark begin of object */
            mark(CARD_ADDR(space), (long *)space);
            ALLOCATION_END();
            /* put object size into first word */
            ((long *)space)[0] = space->vlength;
            /* it may be essential to set last word to zero, because we might
             * have increased the size */
            ((long *)space)[sz - 1] = 0;
            /* return second word */
            return ((void *)&(((long *)space)[1]));
        }
        else
        {
            space->vlength = diff;      /* leave some bytes there */
            /* set mark bit to mark begin of object */
            mark(CARD_ADDR(space), (long *)space + diff);
            ALLOCATION_END();
            /* put object size into first word */
            ((long *)space)[diff] = sz;
            /* return second word */
            return ((void *)&(((long *)space)[diff + 1]));
        }

#ifdef  USE_LARGE
    }   /* end if no large object */
    else
    {   /* handling large objects another way than on ss_cards: object will be
         * put on the end of one card, the part before this will be used by
         * normal objects */
        long noc = basiccards(sz), sz1;
        DEEP_ALLOC_BEGIN();
        get_large_card(c, noc);
        /* the following macro will do something wrong this time: freelist will
         * contain only the userspace of the first card, but we're lucky to
         * know this ... */
        prepare_ms_card(cd, c, noc);
        DEEP_ALLOC_END();
        /* sz1 is the part of the objectsize to stay at the first card */
        sz1 = sz - (noc - 1) * byte2word(CARDSIZE);
        /* furthermore we must assure, that the object itself starts on the
         * first basic card; that means it has at min. 2 words on it: the
         * size/mark-word and one which is the begin of object to be pointed to
         * from roots or other objects. */
        sz1 = max(sz1, 2);
        /* the next process is nearly the same as above, sz1 taking the role of
         * sz. (fortunely we decided to fill pages from the end) */
        last = (vobject *) get_freept_addr(cd);
        space = last->vnext;
        if ((diff = space->vlength - sz1) < 2)
        {       /* no or only one elem left */
            last->vnext = space->vnext; /* connect free list */
            mark(CARD_ADDR(space), (long *)space);      /* set mark bit */
            ALLOCATION_END();
            *(long *)space = sz;        /* length into first word */
            return ((void *)((long *)space + 1));       /* return second word */
        }
        else
        {
            space->vlength = diff;      /* leave some bytes there */
            mark(CARD_ADDR(space), (long *)space + diff);       /* set mark bit
                                                                 */
            ALLOCATION_END();
            *((long *)space + diff) = sz;       /* length into first word */
            return ((void *)((long *)space + diff + 1));        /* ret 2nd.
                                                                 * word */
        }
    }
#endif /* use large */
}
#endif /* use stms */

void *xalloc(cd, par)
    register CardDscr cd;
    register long par;          /* holds size or type-descriptor */
{
#ifdef  USE_MTSS
    if (mt_card_d(cd))
        return (xalloc_mtss(cd, (TypeDscr) par));
#endif
#ifdef  USE_STMS
    if (ms_card_d(cd))
        return (xalloc_stms(cd, par));
#endif
#ifdef  USE_STSS
    return (xalloc_stss(cd));
#endif
}
