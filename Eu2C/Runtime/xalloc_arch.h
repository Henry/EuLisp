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
/// Title: architecture dependent xalloc definitions
///  Library: Xalloc
///  Authors: Jens Bimberg
///  Maintainer: Henry G. Weller
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

// Alignment forcing if high alignment is necessary, normally word-alignment
// should be enough, but it may be set to sizeof(double)
#define MT_ALIGN_WORDS  1

#define STACK_GROWS_DOWN
#define PTR_ALIGN       4

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
#define MT_ALIGN_WORDS  1

#define STACK_GROWS_DOWN
#define PTR_ALIGN       8

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
#define ARCH_KNOWN
#endif  // __sparc__s

///-----------------------------------------------------------------------------
/// Unknown
///-----------------------------------------------------------------------------
#ifndef ARCH_KNOWN
#error "unsupported machine, sorry"
#endif

///-----------------------------------------------------------------------------
#endif // XALLOC_ARCH_H
///-----------------------------------------------------------------------------
