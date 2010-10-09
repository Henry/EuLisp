/// Copyright 1997 A. Kind & University of Bath
/// Copyright 2010 Henry G. Weller
///-----------------------------------------------------------------------------
//  This file is part of
/// ---                         EuLisp System 'Youtoo'
///-----------------------------------------------------------------------------
//
//  Youtoo is free software: you can redistribute it and/or modify it under the
//  terms of the GNU General Public License version 2 as published by the Free
//  Software Foundation.
//
//  Youtoo is distributed in the hope that it will be useful, but WITHOUT ANY
//  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
//  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
//  details.
//
//  You should have received a copy of the GNU General Public License along with
//  this program.  If not, see <http://www.gnu.org/licenses/>.
//
///-----------------------------------------------------------------------------
///  Title: stacks
///  Library: eulvm (Bytecode Interpreter -- Eutopia)
///  Authors: Keith Playford, Andreas Kind
///  Maintainer: Henry G. Weller
///-----------------------------------------------------------------------------
#ifndef STACK_H
#define STACK_H

#include "object.h"

///-----------------------------------------------------------------------------
/// Stack parameter
///-----------------------------------------------------------------------------

#define STACK_BUFFER_SIZE      (512)    // light-weight stacks
#define STACK_BUFFER_HANGOVER  (128)
#define MAX_STACK_SEGMENT_SIZE (128)

#define STACK_BUFFER_CONTENTS_SIZE(stk) (stk->sp-stk->base)

///-----------------------------------------------------------------------------
/// Segment structure
///-----------------------------------------------------------------------------

#define STACK_SEGMENT_HEADER_LENGTH (1)
#define STACK_SEGMENT_LAST_SEGMENT(seg) (slot_ref(seg, 0))

///-----------------------------------------------------------------------------
/// Stack structure
///-----------------------------------------------------------------------------

typedef struct stack_structure
{
    LispRef base[STACK_BUFFER_SIZE];    // The body of the buffer
    LispRef *sp;                        // Current stack pointer
    LispRef segment;                    // Flushed stack segments
    int size;                           // Size of flushed stack segments
} Stack;

typedef Stack *StackRef;

extern StackRef eul_allocate_stack();
extern LispRef copy_stack_segment (LispRef);


///-----------------------------------------------------------------------------
/// Stack declaration
///-----------------------------------------------------------------------------

#define DECLARE_STACK_REGISTERS()                                              \
    register LispRef *sreg_value_sp;                                           \
    register LispRef *sreg_value_sb;                                           \
    register LispRef *sreg_context_sp;                                         \
    register LispRef *sreg_context_sb

#define CACHE_STACK_REGISTERS()                                                \
    sreg_value_sp = reg_value_stack->sp;                                       \
    sreg_value_sb = reg_value_stack->base;                                     \
    sreg_context_sp = reg_context_stack->sp;                                   \
    sreg_context_sb = reg_context_stack->base

#define FLUSH_STACK_REGISTERS()                                                \
    reg_value_stack->sp = sreg_value_sp;                                       \
    reg_context_stack->sp = sreg_context_sp


///-----------------------------------------------------------------------------
/// Stack manipulation (low-level)
///-----------------------------------------------------------------------------

#define POP1(sp, v1)        (v1)=(*(--(sp)))
#define POP2(sp, v1, v2)     POP1(sp, v1); POP1(sp, v2)
#define POP3(sp, v1, v2, v3)  POP1(sp, v1); POP2(sp, v2, v3)
#define POPN(sp, n)         ((sp)-=(n))
#define REF(sp, n, v1)       ((v1)=(*((sp)-1-(n))))
#define SET_REF(sp, n, v1)   (*((sp)-1-(n)) = (v1))
#define PUSH1(sp, v1)       (*(sp++)=(v1))
#define PUSH2(sp, v1, v2)    PUSH1(sp, v1); PUSH1(sp, v2)
#define PUSH3(sp, v1, v2, v3) PUSH1(sp, v1); PUSH2(sp, v2, v3)
#define LVPEEK(sp)         (*((sp)-1))
#define LVPEEKVAL()        (LVPEEK(sreg_value_sp))


///-----------------------------------------------------------------------------
/// Stack over-/underflow
///-----------------------------------------------------------------------------

#define FLUSH_STACK(stk, sptr, hangover)                                       \
    NOTIFY1("Flushing %s stack buffer: ",                                      \
    (stk == reg_value_stack ? "value" : "context"))                            \
    stk->sp = sptr;                                                            \
    flush_stack_buffer(stk, hangover);                                         \
    sptr = stk->sp

#define UNFLUSH_STACK(stk, sptr, hangover)                                     \
    NOTIFY1("Unflushing %s stack buffer: ",                                    \
    (stk == reg_value_stack ? "value" : "context"))                            \
    stk->sp = sptr;                                                            \
    unflush_stack_buffer(stk, hangover);                                       \
    sptr = stk->sp

extern void flush_stack_buffer (StackRef, int);

extern void unflush_stack_buffer (StackRef, int);

#define ENSURE_PUSHABILITY(stk, sptr, top, n)                                  \
    if (((sptr)+(n))>(top))                                                    \
    {                                                                          \
        FLUSH_STACK(stk, sptr, STACK_BUFFER_HANGOVER);                         \
    }

#define ENSURE_POPABILITY(stk, sptr, base, n)                                  \
    if (!((base)+(n)<(sptr)))                                                  \
    {                                                                          \
        UNFLUSH_STACK(stk, sptr, STACK_BUFFER_HANGOVER);                       \
    }

#define eul_allocate_segment(loc, n)                                           \
    ALLOCATE_RAW_OBJECT(loc, slots_to_bytes(n+STACK_SEGMENT_HEADER_LENGTH));   \
    STACK_SEGMENT_LAST_SEGMENT(loc) = eul_nil;                                 \
    INITIALIZE_OBJECT(loc, PGLOBAL(glob_vector_class), n+STACK_SEGMENT_HEADER_LENGTH)


///-----------------------------------------------------------------------------
/// Value stack manipulation (high-level)
///-----------------------------------------------------------------------------

#define POPVAL1(v1)                                                            \
    ENSURE_POPABILITY(reg_value_stack, sreg_value_sp, sreg_value_sb, 1);       \
    POP1(sreg_value_sp, v1);

#define POPVAL2(v1, v2)                                                        \
    ENSURE_POPABILITY(reg_value_stack, sreg_value_sp, sreg_value_sb, 2);       \
    POP2(sreg_value_sp, v1, v2)

#define POPVAL3(v1, v2, v3)                                                    \
    ENSURE_POPABILITY(reg_value_stack, sreg_value_sp, sreg_value_sb, 3);       \
    POP3(sreg_value_sp, v1, v2, v3)

#define POPVALN(n)                                                             \
    ENSURE_POPABILITY(reg_value_stack, sreg_value_sp, sreg_value_sb, n);       \
    POPN(sreg_value_sp, n)

// Ref if a critical case: if n>buffer fill we're stuffed

// (it's 1+n because REF(0) is stacktop "=" a pop of one element

#define REFVAL(n, v1)                                                          \
    ENSURE_POPABILITY(reg_value_stack, sreg_value_sp, sreg_value_sb, 1+n);     \
    REF(sreg_value_sp, n, v1)

#define value_stack_size                                                       \
    ((uPtrInt) (sreg_value_sp-sreg_value_sb))

#define context_stack_size                                                     \
    ((uPtrInt) (sreg_context_sp-sreg_context_sb))

#define REFPARAM(prev_size, n, v1)                                             \
    {                                                                          \
        int m;                                                                 \
        m = (value_stack_size-prev_size)-n-1;                                  \
        /*    printf(" prev_size: %d", prev_size);  */                         \
        /*    printf(" size: %d", value_stack_size);  */                       \
        /*    printf(" m: %d ", m); fflush(stdout);  */                        \
        ENSURE_POPABILITY(reg_value_stack, sreg_value_sp, sreg_value_sb, 1+m); \
        REF(sreg_value_sp, m, v1);                                             \
    }

#define SET_REFVAL(n, v1)                                                      \
    ENSURE_POPABILITY(reg_value_stack, sreg_value_sp, sreg_value_sb, 1+n);     \
    SET_REF(sreg_value_sp, n, v1)

#define PUSHVAL1(v1)                                                           \
    ENSURE_PUSHABILITY(reg_value_stack, sreg_value_sp,                         \
    sreg_value_sb+STACK_BUFFER_SIZE-1, 1);                                     \
    PUSH1(sreg_value_sp, v1)

///-----------------------------------------------------------------------------
/// Context stack manipulation (high-level)
///-----------------------------------------------------------------------------

#define REFCONTEXT(n, v1)                                                      \
    if (!((sreg_context_sb+n+1) < sreg_context_sp))                            \
    {                                                                          \
        v1 = eul_nil;                                                          \
    }                                                                          \
    else                                                                       \
    {                                                                          \
        REF(sreg_context_sp, n-1, v1);                                         \
    }

#define CONTEXT_FRAME_SIZE (4)

#define PUSHCONTEXT()                                                          \
    {                                                                          \
        NOTIFY1("PUSHINGCONTEXT[%" ptrIntPM "x]:", (ptrInt)sreg_context_sp);   \
        ENSURE_PUSHABILITY(reg_context_stack, sreg_context_sp,                 \
        sreg_context_sb+STACK_BUFFER_SIZE-1,                                   \
        CONTEXT_FRAME_SIZE);                                                   \
        PUSH1(sreg_context_sp, reg_next_methods);                              \
        PRINT_REG_PC("Pushing");                                               \
        PUSH1(sreg_context_sp, (LispRef) reg_pc);                              \
        PUSH1(sreg_context_sp, reg_env);                                       \
        PUSH1(sreg_context_sp, reg_arg_operator);                              \
        NOTIFY1(":DONE[%" ptrIntPM "x] \n", (ptrInt)sreg_context_sp);          \
    }

#define PUSHEMPTYCONTEXT()                                                     \
    PUSH3(sreg_context_sp, eul_nil, eul_nil, eul_nil);                         \
    PUSH1(sreg_context_sp, eul_nil)

#define POPCONTEXT()                                                           \
    {                                                                          \
        LispRef tmp_reg_pc;                                                    \
        NOTIFY1("POPPINGCONTEXT[%" ptrIntPM "x]:", (ptrInt)sreg_context_sp);   \
        if (reg_context_stack->size == 0                                       \
        && (sreg_context_sp - sreg_context_sb) == CONTEXT_FRAME_SIZE)          \
            goto exit;                                                         \
        ENSURE_POPABILITY(reg_context_stack, sreg_context_sp,                  \
        sreg_context_sb, CONTEXT_FRAME_SIZE);                                  \
        POP1(sreg_context_sp, reg_arg_operator);                               \
        POP1(sreg_context_sp, reg_env);                                        \
        reg_pc = (Instruction *) (POP1(sreg_context_sp, tmp_reg_pc));          \
        POP1(sreg_context_sp, reg_next_methods);                               \
        reg_current_cv = (Instruction *) LAMBDA_CODE(reg_arg_operator);        \
        PRINT_REG_PC("Popping");                                               \
        RESUME_TRACE_LAMBDA();                                                 \
        NOTIFY1(":DONE[%" ptrIntPM "x] \n", (ptrInt)sreg_context_sp);          \
        ++reg_pc;                                                              \
    }

///-----------------------------------------------------------------------------
#endif // STACK_H
///-----------------------------------------------------------------------------
