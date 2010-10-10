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
/// Title: Foreign functions for marshaling closures
///  Library: serial
///  Authors: Andreas Kind
///  Maintainer: Henry G. Weller
///-----------------------------------------------------------------------------
#ifndef EUL_SERIAL2_H
#define EUL_SERIAL2_H

extern LispRef eul_lambda_refs(LispRef, LispRef);
extern LispRef eul_bytevector_refs(LispRef, LispRef);
extern LispRef eul_link_lambda_refs(LispRef, LispRef);
extern LispRef eul_link_bytevector_refs(LispRef, LispRef);

#define WITH_DEBUG(x) x

///-----------------------------------------------------------------------------
#endif // EUL_SERIAL2_H
///-----------------------------------------------------------------------------
