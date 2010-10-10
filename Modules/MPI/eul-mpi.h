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
/// Title: Wrapper for the Message Passing Interface (MPI)
///  Library: mpis
///  Authors: Andreas Kind
///  Maintainer: Henry G. Weller
///-----------------------------------------------------------------------------
#ifndef EUL_MPI_H
#define EUL_MPI_H

extern LispRef eul_mpi_initialize();

extern int eul_mpi_status_count(LispRef);
extern int eul_mpi_status_SOURCE(LispRef);
extern int eul_mpi_status_TAG(LispRef);
extern int eul_mpi_status_ERROR(LispRef);

///-----------------------------------------------------------------------------
#endif // EUL_MPI_H
///-----------------------------------------------------------------------------
