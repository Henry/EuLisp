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
///  Description: Stack switch for sparc
///    function to switch to a new stack and start a function there
///    copied from feel, uni bath
///  Notes: unfortunately, the original bath code is not fully thougt out:
///    1. the sp-fp-chain should end up with a NULL-reference
///    2. since the sparc side effects the new stack when changing sp,
///       there should be a 64 byte gap (to write the %i's and %l's into)
///    3. since the sparc does look ahead 1 instruction (load the following
///       instruction into a pipe inside the processor), the 'restore' would
///       be done after the jmp. therefor include a nop there
///  Authors: Jens Bimberg (based on FEEL)
///-----------------------------------------------------------------------------
gcc_compiled.:
.text
	.align 4
.global _stack_switch_and_go
	.proc 1
_stack_switch_and_go:
	ta 0x3

/* this is the original code
	mov %o0, %sp
	mov %sp, %fp
	jmp %o1
*/
/* and this is what I did of it */
	mov %g0, %fp
	add %o0, -64, %sp
	jmp %o1
	nop

	restore
///-----------------------------------------------------------------------------
