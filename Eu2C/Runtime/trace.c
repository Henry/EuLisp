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
///-----------------------------------------------------------------------------
/// Title: xalloc garbage collection
///  Library: Xalloc
///  Authors: Jens Bimberg
///  Maintainer: Henry G. Weller
///-----------------------------------------------------------------------------
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include "xalloc.h"

///-----------------------------------------------------------------------------
/// Definitions only used by the collector
///-----------------------------------------------------------------------------

// It is not clear if this should be changed for 64bit
#define MS_MARK 0x80000000      // is set in length word of MS-obj. to mark

#define ms_ismarked(p)  (*(p) & MS_MARK)
#define ms_mark(p)      (*(p) |= MS_MARK)
#define ms_unmark(p)    (*(p) &= ~MS_MARK)

#define stack_not_empty (stkpointer != stktop)

static long *stkpointer = NULL;
static long *endofstack = NULL;

#ifdef STACK_ON_HEAP
#define STKSIZE         CARDSIZE

static long *stktop = NULL;

#define init_gc_stack()                                                        \
    {                                                                          \
        stkpointer = stktop = (long *) sbrk(STKSIZE);                          \
        endofstack = (long *) sbrk(0);                                         \
    }

#define remove_gc_stack() brk(stktop)

// we must not be interrupted during mark !!
#define inc_gc_stack(e)  {sbrk(STKSIZE); e = (long *) sbrk(0);}
#define gcpop(p)         (long *)*(--p)
#define gcpush(v,p,e)                                                          \
    {                                                                          \
        if (p == e)                                                            \
        {                                                                      \
            inc_gc_stack(e);                                                   \
        }                                                                      \
        *p++ = (long)(v);                                                      \
    }

#else // stack on machine stack
static long *stktop  = NULL;

#define init_gc_stack()                                                        \
    {                                                                          \
        long i; stktop = (long *)((long)&i - STACKGAP);                        \
        stkpointer = stktop;                                                   \
    }

#define remove_gc_stack()
#define gcpop(p)                (long *)*(++p)
#define gcpush(v,p,e)           {*p-- = (long)(v);}

#endif // STACK_ON_HEAP

#define itrace(v,p,e)           if (p_in_heap(v)) gcpush(v,p,e)

#define mark_card(c)            (c)->root = NULL
#define card_marked(c)          ((c)->root == NULL)


///-----------------------------------------------------------------------------
/// Rootset
///-----------------------------------------------------------------------------
#ifdef ROOT_SET_IN_USE

long ROOTSET[MAX_ROOT_SET_ENTRIES];
long *ROOTSETEND = ROOTSET;


void initialize_root_set()
{
    ROOTSETEND = ROOTSET;
}


void add_to_root_set(void *p)
{
    #ifdef SECURITY_FIRST
    if (ROOTSETEND >= &ROOTSET[MAX_ROOT_SET_ENTRIES])
    {
        error("Unable to insert root address");
    }
    else
    #endif
    {
        *ROOTSETEND++ = (long)p;
    }
}


void delete_from_root_set(void *p)
{
    long *l = ROOTSET;
    for (; ((l < ROOTSETEND) && (*l != (long)p)); l++);
    if (l < ROOTSETEND)
    {
        *l = *(--ROOTSETEND);
    }
}
#else

extern end, etext;              // all locations of static data

#endif


///-----------------------------------------------------------------------------
/// Trace
///-----------------------------------------------------------------------------
// to be called from user defined mark functions
void *trace_pointer(register void *ptr)
{
    itrace((long *)ptr, stkpointer, endofstack);
    return ptr;
}


///-----------------------------------------------------------------------------
/// Mark
///-----------------------------------------------------------------------------
void *trace_all(register void *ptr, register long length)
{
    while (length > 0)
    {
        length -= PTR_ALIGN;
        itrace((long *)*((long *)((long)ptr + length)), stkpointer, endofstack);
    }
    return ptr;
}

void *trace_nothing(register void *ptr, register long length)
{
    // nothing to do
    return ptr;
}

void *trace_first(register void *ptr, register long length)
{
    itrace(((long **)ptr)[0], stkpointer, endofstack);
    return ptr;
}

void *trace_second(register void *ptr, register long length)
{
    itrace(((long **)ptr)[1], stkpointer, endofstack);
    return ptr;
}

void *trace_pair(register void *ptr, register long length)
{
    itrace(((long **)ptr)[0], stkpointer, endofstack);
    itrace(((long **)ptr)[1], stkpointer, endofstack);
    return ptr;
}

void trace_from_gc_stack()
{
    // trace from the just built gc_stack. unfortunately, we can't use
    // registers to hold our markstackpointer, because it will be changed by
    // called trace_functions, too
    while (stack_not_empty)
    {
        register long *p = gcpop(stkpointer);
        register card *c = CARD_ADDR(p);

        if (living_card(c) && (p >= c->userspace))
        {
            #ifdef USE_MTSS
            if (mt_card(c))
            {
                long *ap = p - MT_ALIGN_WORDS;
                if (consistent_pointer(c, ap, get_size(c), get_mask(c)))
                {
                    if (is_tdscr(*(p - 1)))
                    {
                        if (!ismarked(c, ap))
                        {
                            mark(c, ap);
                            (get_mark_fcn(*(p - 1)) (p, get_real_size(c)));
                        }
                    }
                }
                continue;
            }
            #endif // USE_MTSS

            #ifdef USE_STMS
            if (ms_card(c))
            {
                if (consistent_ms_pointer(c, p))
                {
                    if (!ms_ismarked(p - 1))
                    {
                        ms_mark(p - 1);
                        (get_mark_fcn(get_tdscr(c))) (p, get_ms_real_size(p));
                    }
                }
                continue;
            }
            #endif // USE_STMS

            #ifdef USE_STSS
            #ifdef PTR_IN_OBJ     // calculate the begin instead of test for it
            p = obj_begin(c, p, get_size(c), get_mask(c));
            #else
            if (consistent_pointer(c, p, get_size(c), get_mask(c)))
                #endif
            {
                if (!ismarked(c, p))
                {
                    mark(c, p);
                    (get_mark_fcn(get_tdscr(c))) (p, get_real_size(c));
                }
            }
            #endif // USE_STSS
        }
    }
}


static long *stacktop = NULL;

void set_stacktop(void **p)
{
    if (stacktop == NULL)
    {
        stacktop = (long *)p;
    }
}


void mark_all_roots(register long *from, register long *to)
{
    register long *rsp = stkpointer;
    register long *reos = endofstack;

    // We are trying to be a little faster when using two registers for the
    // stackpointer and the (possible) end of stack area.
    for (register long *p = from; p < to; p++)
    {
        itrace((long *)*p, rsp, reos);
    }

    stkpointer = rsp;
    endofstack = reos;
    trace_from_gc_stack();
}


// First phase of our mark/sweep - collector
void marking()
{
    // Flush the registers
    flush_registers(0,0,0,0,0,0);

    #ifdef NOTHREADS        // single-thread-version
    long i = 0;
    #ifdef STACK_GROWS_DOWN
    mark_all_roots(&i, stacktop);
    #else
    #ifdef STACK_GROWS_UP
    mark_all_roots(stacktop, &i);
    #else
    #error "where else does the stack grow ??"
    #endif
    #endif
    #else // multi-thread-version
    m_thread_mark(mark_all_roots, stacktop);
    #endif // NOTHREADS

    // trace from root set or from all globals resp. data pages
    #ifdef ROOT_SET_IN_USE
    {
        register long *rsp = stkpointer;
        register long *reos = endofstack;

        for (register long *p = ROOTSET; p < ROOTSETEND; p++)
        {
            register long *g = (long *)*p;
            #ifdef SECURITY_FIRST
            if (!is_pointer(g))
            {
                // did the user add something wrong to the root set ?
                error("mark: wrong root in root set");
            }
            #endif
            itrace((long *)*g, rsp, reos);
        }

        stkpointer = rsp;
        endofstack = reos;
    }

    trace_from_gc_stack();

    #else // ! ROOT_SET_IN_USE

    #ifdef DATASTART
    mark_all_roots(DATASTART, gcarraybegin);
    mark_all_roots(gcarrayend, (long *)&end);
    // reg-safe is scanned through, too
    #else
    #error "use ROOTSET or tell us where your data starts"
    #endif

    #endif // ROOT_SET_IN_USE

    // Now all locations reachable via registers, the stack or global variables
    // are marked. We can return.
}


///-----------------------------------------------------------------------------
/// Sweep
///-----------------------------------------------------------------------------

int bytes_in_use = 0;
#ifdef SKIP_SWEEP
int allocated_cards = 0;
int sweeped_cards = 0;
#endif


// remove mark bits for objects in free lists of card-descriptor cd
void clear_fsl_marks(register CardDscr cd)
{
    register long *p = (long *)get_freept_addr(cd);

    while ((p = (long *)*p))
    {
        register card *c = CARD_ADDR(p);
        if (ismarked(c, p))
        {
            unmark(c, p);
        }
    }
}


long ss_sweep(register card *c, register CardDscr cd)
{
    register long *pt = c->mkarea;
    register long *en = pt + NB_OF_MARK_WORDS;

    #ifdef SKIP_SWEEP
    sweeped_cards++;
    #endif

    // At first test, whether this card is empty. This works very fast by
    // checking a word at a time
    for (; (pt < en) && (*pt == 0); pt++);

    if (pt == en)
    {
        #ifdef SKIP_SWEEP
        // we are already in xalloc when sweeping, and therefore hold the card
        // no matter if it is empty or not. therefore we must build a free list
        // on it (without this checking below and without concatenate with the
        // old free list since this should be empty)
        store_free_object(cd, build_free_list(c, c + 1, cd));
        #endif

        return (0);
    }

    // There are some marked objects on this card, build a list of the other
    // ones
    register long step = get_size_d(cd);
    register long *fsl = get_free_object(cd);
    register long used = 0;
    pt = (long *)c + byte2word(CARDSIZE) - step;
    en = c->userspace;

    while (pt >= en)
    {
        // skip all marked objects
        if (ismarked(c, pt))
        {
            pt -= step;
            used += step;
        }
        else // connect the others
        {
            *pt = (long)fsl;
            fsl = pt;

            #ifdef OPTIMIZED_POINTER_TESTS
            // we migth have increased the size of the objects, so the
            // (possibly unused but traced) end of these objects must be set to
            // zero
            if (step == 4)
            {
                pt[3] = 0;
            }
            else if (step == 8)
            {
                pt[5] = 0;
                pt[6] = 0;
                pt[7] = 0;
            }
            #endif

            pt -= step;
        }
    }

    // write internal fsl to free list, if and only if there are some living
    // objects remaining on that card. otherwise we would return 0 and remove
    // that card while it's storage remains in the free list
    #ifndef SKIP_SWEEP
    if (used == 0)
    {
        return 0;
    }
    #endif

    store_free_object(cd, fsl);

    #ifndef MULTI_FSL
    // when using multiple fsl's clearing marks doesn't matter, since we can
    // never be sure that they are clear and therefore perform the clear_marks
    // on all cards before gc
    clear_marks(c);
    #endif

    bytes_in_use += word2byte(used);

    return word2byte(used);
}


int check_marks(card * c)       // return wether there are mark bits set
{
    register long *pt = c->mkarea;
    register long *en = pt + NB_OF_MARK_WORDS;

    for (; (pt < en) && (*pt == 0); pt++);

    if (pt == en)
    {
        return 0;
    }
    else
    {
        return 1;
    }
}


#ifdef USE_STMS
// assumes c to be pointer to ms_card, sweeps card and builds a free list on it,
// storing its space to the free list of its card_dscr.  if an unmarked large
// object is found on the card, and some small objects stay there, the
// 'continued' cards will be reclaimed explicitely, because clean_card_list is
// unable to do this job. function returns number of bytes still in use.
long ms_sweep(register card *c, register CardDscr cd)
{
    register int new = 1;
    register vobject *ifsl = (vobject *) get_free_object(cd);
    register long *ptr = c->userspace;
    register long *e = (long *)(c + 1);
    register long used = 0;

    #ifdef SKIP_SWEEP
    sweeped_cards += get_cardnum(c);
    #endif

    while (ptr < e)
    {
        // begin of object
        if (ismarked(c, ptr))
        {
            // survivor
            if (ms_ismarked(ptr))
            {
                ms_unmark(ptr); // reset mark bit
                new = 1;        // free space will start after this
                used += *ptr;   // still in use
                ptr += *ptr;    // behind this object
            }
            else
            {   // no pointer to this obj, reclaim it
                unmark(c, ptr); // no longer begin of obj

                #ifdef USE_LARGE
                // large object found
                if (*ptr > USERSPACE)
                {
                    // as this is the last object on that card, we can test,
                    // whether we found any surviver before this and break
                    // otherwise
                    #ifndef SKIP_SWEEP
                    // if we have skipped the sweep inside gc we are already in
                    // xalloc, and therefore need this card
                    if (used == 0)
                    {
                        break;
                    }
                    #endif

                    // the card stays used, we must reclaim the tail of now
                    // unused 'continued' cards and decrement c->cardnum
                    reclaim_card(e, get_cardnum(c) - 1);
                    put_cardnum(c, 1);

                    // the part of this object which is on the first card will
                    // become a part of the free list as below
                    if (new)
                    {
                        ((vobject *) ptr)->vlength = e - ptr;   // length
                        ((vobject *) ptr)->vnext = ifsl;
                        ifsl = (vobject *) ptr; // into free list
                        ptr = e;        // behind card
                    }
                    else
                    {
                        // concat. with before space
                        ifsl->vlength += (e - ptr);     // increase before
                        ptr = e;        // behind card
                    }
                }
                else
                    #endif
                {
                    if (new)
                    {
                        // start of space
                        ((vobject *) ptr)->vlength = *ptr;      // length
                        ((vobject *) ptr)->vnext = ifsl;
                        ifsl = (vobject *) ptr; // into free list
                        ptr += ((vobject *) ptr)->vlength;      // behind this
                        new = 0;
                    }
                    else
                    {
                        // connect with before space
                        ifsl->vlength += *ptr;  // increase before space
                        ptr += *ptr;    // behind this object
                    }
                }
            }
        }
        else
        {
            // found part of former free list
            if (new)
            {
                // start of space
                ((vobject *) ptr)->vnext = ifsl;
                ifsl = (vobject *) ptr;
                ptr += ((vobject *) ptr)->vlength;      // behind this object
                new = 0;
            }
            else
            {
                // concat. with before space
                ifsl->vlength += ((vobject *) ptr)->vlength;    // inc before
                ptr += ((vobject *) ptr)->vlength;      // behind this object
            }
        }
    }

    #ifndef SKIP_SWEEP
    if (used == 0)
    {
        return 0;
    }
    #endif

    // some bytes on that card are still in use
    store_free_object(cd, (long *)ifsl);
    bytes_in_use += word2byte(used);

    return (word2byte(used));
}


// assumes c to be pointer to ms_card. clear all markbits on this card
void clear_ms_marks(card * c)
{
    register long *ptr = c->userspace;
    register long *e = (long *)(c + 1);

    while (ptr < e)
    {
        // begin of object
        if (ismarked(c, ptr))
        {
            ms_unmark(ptr);
            ptr += *ptr;
        }
        else
        {
            // part of free list
            ptr += ((vobject *) ptr)->vlength;
        }
    }
}


// assumes c to be ptr to ms-card. return wether there are mark bits set
int check_ms_marks(card * c)
{
    register long *ptr = c->userspace;
    register long *e = (long *)(c + 1);

    while (ptr < e)
    {
        if (ismarked(c, ptr))
        {
            // begin of object
            if (ms_ismarked(ptr))
            {
                // surviving object
                return 1;
            }

            ptr += *ptr;
        }
        else
        {
            // part of free list
            ptr += ((vobject *) ptr)->vlength;
        }
    }

    return 0;   // no living object found
}

#endif


///-----------------------------------------------------------------------------
/// Remove marked cards, close holes in cardlist
///-----------------------------------------------------------------------------
int clean_card_list()
{
    register card **ci = first_card_index;
    #ifdef GCREPORTS
    int reclaimed_cards = 0;
    #endif
    while (ci < last_card_index)
    {
        if (card_marked(*ci))
        {
            #ifdef GCREPORTS
            reclaimed_cards += get_cardnum(*ci);
            #endif

            #ifdef USE_LARGE
            reclaim_card((long*)*ci, get_cardnum(*ci));        // call to heap.c
            #else
            reclaim_card((long*)*ci);
            #endif

            prev_card_index(last_card_index);
            while ((ci < last_card_index) && (card_marked(*last_card_index)))
            {
                #ifdef GCREPORTS
                reclaimed_cards += get_cardnum(*ci);
                #endif

                #ifdef USE_LARGE
                reclaim_card
                (
                    (long*)*last_card_index,
                    get_cardnum(*last_card_index)
                );
                #else
                reclaim_card((long*)*last_card_index);
                #endif

                prev_card_index(last_card_index);
            }

            if (ci < last_card_index)
            {
                *ci = *last_card_index;
                (*ci)->root = ci;
            }
        }
        next_card_index(ci);
    }
    #ifdef GCREPORTS
    return reclaimed_cards;
    #else
    return 0;
    #endif
}


///-----------------------------------------------------------------------------
/// Garbage collection
///-----------------------------------------------------------------------------
// for better legibility and reuse we subdivide the gc into three parts:
// gc_before_actions, the marking phase and gc_after_actions.  the normal
// mark/sweep algorithm says that nothing is to do before the mark and
// afterwards the sweep takes place. we try out another scheme that does nothing
// after mark (sweeping will be done by the mutator) and therefore performs some
// cleanup before the mark

#ifdef SKIP_SWEEP
#ifndef MULTI_FSL

// return whether or not to perform gc
int gc_before_actions()
{
    // let's see which fraction of our heap has been reused since last gc. if
    // it is to small, increase heap instead of gc. this may be done only when
    // the heap may be increased
    if
    (
        (
            (2*(allocated_cards + sweeped_cards) < curnumcard)
         || (
                (GCDIFF * bytes_in_use)
              > (GCMULT * (allocated_cards + sweeped_cards) * CARDSIZE)
            )
        )
     && (curnumcard < MAX_NUM_OF_CARDS)
    )
    {
        inc_heap();
        return 0;
    }

    // if we don't sweep after marking there must be some code to reset all
    // markbits before start of marking phase. furthermore, we should check, if
    // there are empty cards staying since the last gc. such cards should be
    // removed, since it seems, that we do not further need them
    for (register CardDscr cd = 0; cd < last_used_card; cd++)
    {
        register card **ci = &Cards[cd];
        register card *c = NULL;
        #ifdef USE_STMS
        if (ms_card_d(cd))
        {
            while ((c = *ci))
            {
                // c becomes *last
                if (check_ms_marks(c))
                {
                    // bits set ?
                    clear_ms_marks(c);
                    ci = &(c->next);
                }
                else
                {
                    mark_card(c);
                    *ci = c->next;
                }
            }
        }
        else
            #endif
        {
            while ((c = *ci))
            {
                if (check_marks(c))
                {
                    // bits set ?
                    clear_marks(c);
                    ci = &(c->next);
                }
                else
                {
                    mark_card(c);
                    *ci = c->next;
                }
            }
        }

        // append the full card's list. these cards should be swept and
        // therefore free of mark bit
        *ci = get_full_d(cd);
        store_full_d(cd, 0); // reset full card's list
    }

    // since there migth be marked cards we must reorganize the card list
    int i;
    i = clean_card_list();

    #ifdef GCREPORTS
    fprintf(stderr, " - %d of %ld cards reclaimed", i, curnumcard);
    fflush(stderr);
    #endif

    return 1;   // do perform gc
}

#else // defined MULTI_FSL

// return whether or not to perform gc
int gc_before_actions()
{
    // let's see which fraction of our heap has been reused since last gc. if
    // it is to small, increase heap instead of gc. this may be done only when
    // the heap may be increased
    if
    (
        (
            (2 * (allocated_cards + sweeped_cards) < curnumcard)
         || (
             (GCDIFF * bytes_in_use) >
             (GCMULT * (allocated_cards + sweeped_cards) * CARDSIZE)
            )
        )
     && (curnumcard < MAX_NUM_OF_CARDS)
    )
    {
        inc_heap();
        return 0;
    }

    // the above scheme does not work when multiple fsl's are used, since
    // markbits on all active cards of all threads must be reset and we have no
    // better access to them than via the used cards list (actually the
    // b/w-array)
    // first do the same as above: test whether there are unswept cards
    // remaining; these cards may be removed; then append the full cards list
    // and the active card of the current thread -- but all without resetting
    // markbits, since this will be done globally afterwards

    for (register CardDscr cd = 0; cd < last_used_card; cd++)
    {
        register card **ci = &Cards[cd];
        register card *c = NULL;

        #ifdef USE_STMS
        if (ms_card_d(cd))
        {
            while (c = *ci)
            {
                // c becomes *last
                if (check_ms_marks(c))
                {
                    // bits set ?
                    ci = &(c->next);
                }
                else
                {
                    mark_card(c);
                    *ci = c->next;
                }
            }
        }
        else
            #endif
        {
            while (c = *ci)
            {
                if (check_marks(c))
                {
                    // bits set ?
                    ci = &(c->next);
                }
                else
                {
                    mark_card(c);
                    *ci = c->next;
                }
            }
        }

        *ci = get_full_d(cd);   // append the full card's list
        store_full_d(cd, NULL); // reset full card's list

        // append the active card of the current thread
        if (c = get_active(cd))
        {
            into_card_list(cd, c);
            store_active(cd, 0);
        }
    }

    // since there migth be marked cards we must reorganize the card list
    int i = clean_card_list();

    // now reset all markbits on all cards
    for (ci = first_card_index; ci < last_card_index; next_card_index(ci))
    {
        c = *ci;
        #ifdef USE_STMS
        if (ms_card(c))
        {
            clear_ms_marks(c);
        }
        else
            #endif
        {
            clear_marks(c);
        }
    }

    #ifdef GCREPORTS
    fprintf(stderr, " - %i of %i cards reclaimed", i, curnumcard);
    fflush(stderr);
    #endif

    return 1;   // do perform gc
}


#endif // MULTI_FSL

void gc_after_actions()
{
    // clear fsl's and for ss_cards reset markbits mistakenly set on these
    // lists
    for (register CardDscr cd = 0; cd < last_used_card; cd++)
    {
        if (!ms_card_d(cd))
        {
            clear_fsl_marks(cd);
        }

        store_free_object(cd, NULL);    // clear free list of cd
    }

    // reset the global counters
    bytes_in_use = 0;
    sweeped_cards = 0;
    allocated_cards = 0;
}

#else // ! SKIP_SWEEP

// nothing to do before gc; return that gc should happen
#define gc_before_actions()     1

void gc_after_actions()
{
    #ifdef GCREPORTS
    fprintf(stderr, " - sweep");
    fflush(stderr);
    #endif
    bytes_in_use = 0;   // reset global counter

    // inspect all cdscr's to sweep all cards, mark empty cards for later being
    // removed
    for (register CardDscr cd = 0; cd < last_used_card; cd++)
    {
        register card **ci = &Cards[cd];
        register card *c = NULL;

        #ifdef USE_STMS
        if (ms_card_d(cd))
        {
            store_free_object(cd, NULL);        // clear free list of cd
            while ((c = *ci))
            {
                // c becomes *last
                if (ms_sweep(c, cd))
                {
                    // not empty
                    ci = &(c->next);
                }
                else
                {
                    // empty
                    *ci = c->next;      // off_crd_lst
                    mark_card(c);
                }
            }
        }
        else
            #endif
        {
            // ss_card
            #ifdef USE_LARGE
            if (basiccards(get_size_d(cd)) == 1)
            {
                #endif
                // clear mark bits for objects in free list because if any
                // object in the free list has been marked by fault, all
                // objects behind this in free list migth have been marked, too
                clear_fsl_marks(cd);
                store_free_object(cd, NULL);
                while ((c = *ci))
                {
                    if (ss_sweep(c, cd))
                    {
                        // not empty
                        ci = &(c->next);
                    }
                    else
                    {
                        // empty
                        *ci = c->next;  // off_crd_lst
                        mark_card(c);
                    }
                }
                #ifdef USE_LARGE
            }
            else
            {
                // large cards in use on this carddscr.
                while ((c = *ci))
                {
                    if (ismarked(c, c->userspace))
                    {
                        // surviver
                        unmark(c, c->userspace);
                        bytes_in_use += get_real_size(c);
                        ci = &(c->next);
                    }
                    else
                    {
                        *ci = c->next;  // off_card_lst
                        mark_card(c);
                    }
                }       // end while
            }  // end if basiccards(...)==1
            #endif
        }  // end if ms_card ... else ss_card
    }  // end for each carddescriptor

    int i = clean_card_list();

    #ifdef GCREPORTS
    // fprintf(stderr, " - %i of %i bytes (%i of %i cards) reclaimed",
    // CARDSIZE*curnumcard-bytes_in_use, CARDSIZE*curnumcard, i, curnumcard);
    fprintf
    (
        stderr,
        " - %ld %% (%i of %ld cards) reclaimed",
        100*(CARDSIZE*curnumcard - bytes_in_use)/(CARDSIZE*curnumcard),
        i,
        curnumcard
    );
    fflush(stderr);
    #endif

    if ((GCDIFF * bytes_in_use) > (GCMULT * CARDSIZE * curnumcard))
    {
        // to few of our storage has been reclaimed, try to get more
        inc_heap();
    }
}

#endif


static int gc_count = 0;

#ifdef GCREPORTS
#include <sys/times.h>
#endif

void garbage_collection()
{
    #ifdef GCREPORTS
    struct tms tb1, tb2;
    #endif
    ++gc_count;

    #ifdef GCREPORTS
    fprintf(stderr, "GC %d:", gc_count);
    fflush(stderr);
    times(&tb1);
    #endif

    if (gc_before_actions() == 0)
    {
        // skip gc, no need for
        #ifdef GCREPORTS
        fprintf(stderr, " - skipped.\n");
        #endif
        --gc_count;
        return;
    }

    #ifdef GCREPORTS
    fprintf(stderr, " - mark");
    fflush(stderr);
    #endif

    init_gc_stack();    // * * * * * MAKE SURE * * * * * *
    marking();          // THAT NO THREAD-SWITCH MAY OCCURE
    remove_gc_stack();  // * * * DURING THESE 3 CALLS * * *

    gc_after_actions();

    #ifdef GCREPORTS
    times(&tb2);
    int clock_freq = sysconf(_SC_CLK_TCK);
    fprintf
    (
        stderr, " - %.2f s used.\n",
        (double)(tb2.tms_utime - tb1.tms_utime)/clock_freq
    );
    #endif

    return;
}


void force_garbage_collection()
{
    DISABLE_SCHEDULE();
    garbage_collection();
    ENABLE_SCHEDULE();
}


void xalloc_info()
{
    // print configuration and runtime info
    fprintf(stderr, "\nNumber of Garbage Collections: %d", gc_count);
    fprintf(stderr, "\nInitial number of Cards %d", start_nb_of_cards);
    fprintf(stderr, "\nNumber of used Cards %ld", curnumcard);
    // fprintf(stderr,"\nLast heap increment %d",hincr);

    fprintf
    (
        stderr,
        "\nGCMULT %d GCDIFF %d HMULT %d HDIFF %d",
        GCMULT,
        GCDIFF,
        HMULT,
        HDIFF
    );

    #ifdef ROOT_SET_IN_USE
    fprintf(stderr, "\nUserdefined root set was used");
    #else
    fprintf(stderr, "\nAnonymous root set was used");
    #endif

    #ifdef STACK_ON_HEAP
    fprintf(stderr, "\nMarking stack was allocated on heap");
    #else
    fprintf(stderr, "\nMarking stack was allocated on stack");
    #endif

    #ifdef SKIP_SWEEP
    #ifdef MULTI_FSL
    fprintf(stderr, "\nMultiple free lists used");
    #else
    fprintf(stderr, "\nSweeping phase has been skipped");
    #endif
    #else
    fprintf(stderr, "\nSweeping phase has happened");
    #endif

    fprintf(stderr, "\n");
    fflush(stderr);
}


///-----------------------------------------------------------------------------
