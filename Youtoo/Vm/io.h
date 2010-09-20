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
///  Library: eulvm (Bytecode Interpreter -- Eutopia)
///  Authors: Keith Playford, Andreas Kind
///  Description: input/output
///-----------------------------------------------------------------------------
#ifndef IO_H
#define IO_H

///-----------------------------------------------------------------------------
/// File information
///-----------------------------------------------------------------------------

#define EUL_FILE_CONTROL_BLOCK_HANDLE(x) slot_ref(x, 6)
#define EUL_STREAM_SINK(x) slot_ref(x, 4)
#define eul_file_control_block_handle_as_fd(x) ((FILE *) fpi_value(x))

#define eul_stream_as_c_fd(x)                                                  \
    eul_file_control_block_handle_as_fd(                                       \
        EUL_FILE_CONTROL_BLOCK_HANDLE(EUL_STREAM_SINK(x)))

///-----------------------------------------------------------------------------
#endif // IO_H
///-----------------------------------------------------------------------------
