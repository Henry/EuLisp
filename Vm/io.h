/// Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
///-----------------------------------------------------------------------------
/// ---                 EuLisp System 'youtoo'
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
