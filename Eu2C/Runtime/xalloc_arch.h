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
///  Title: architecture dependent xalloc definitions
///  Library: Runtime
///  Authors: Jens Bimberg
///-----------------------------------------------------------------------------
#ifndef XALLOC_ARCH_H
#define XALLOC_ARCH_H

///-----------------------------------------------------------------------------
/// i386 Linux
//------------------------------------------------------------------------------
#ifdef  __i386

#define WORD_SIZE 32
#define BYTES_PER_WORD 4
#define LOG_WORD_SIZE 5

// Alignment forcing if high alignment is necessary, normally 4-byte-alignment
// should be enough, but it may be set to sizeof(double)
#define MT_ALIGNMENT    4

#define STACK_GROWS_DOWN
#define PTR_ALIGN       4
#define NB_REGS         6
#define SAVE_REGS()                      \
    asm("mov reg_safe,      %eax");      \
    asm("mov reg_safe + 4,  %ebx");      \
    asm("mov reg_safe + 8,  %ecx");      \
    asm("mov reg_safe + 12, %edx");      \
    asm("mov reg_safe + 16, %esi");      \
    asm("mov reg_safe + 20, %edi")
#define ARCH_KNOWN
#endif // __i386

///-----------------------------------------------------------------------------
/// x86_64 Linux
///-----------------------------------------------------------------------------
#ifdef  __x86_64

#define WORD_SIZE 64
#define BYTES_PER_WORD 8
#define LOG_WORD_SIZE 6

// Alignment forcing if high alignment is necessary, normally 8-byte-alignment
// should be enough
#define MT_ALIGNMENT    8

#define STACK_GROWS_DOWN
#define PTR_ALIGN       8
#define NB_REGS         6
#define SAVE_REGS()                                                            \
    asm("mov reg_safe,      %eax");                                            \
    asm("mov reg_safe + 8,  %ebx");                                            \
    asm("mov reg_safe + 16, %ecx");                                            \
    asm("mov reg_safe + 24, %edx");                                            \
    asm("mov reg_safe + 32, %esi");                                            \
    asm("mov reg_safe + 40, %edi")
#define ARCH_KNOWN
#endif // __x86_64

///-----------------------------------------------------------------------------
/// SPARC ELC, SPARC 2, SPARC 10 Solaris
///-----------------------------------------------------------------------------
#ifdef  __sparc__

// Alignment forcing if high alignment is necessary, normally 4-byte-alignment
// should be enough, but it may be set to sizeof(double)
#define MT_ALIGNMENT    4

#define STACK_GROWS_DOWN
#define PTR_ALIGN       4
#define DATASTART       (long *)&etext
#define SAVE_REGS()     asm("t  3")     /* ST_FLUSH_WINDOWS */
#define ARCH_KNOWN
#endif  /* __sparc__ */

///-----------------------------------------------------------------------------
/// Unknown
///-----------------------------------------------------------------------------
#ifndef ARCH_KNOWN
#error "unsupported machine, sorry"
#endif

///-----------------------------------------------------------------------------
#endif // XALLOC_ARCH_H
///-----------------------------------------------------------------------------
