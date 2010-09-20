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
///  Description: notification, errors, warnings
///-----------------------------------------------------------------------------
#ifndef NOTIFY_H
#define NOTIFY_H

///-----------------------------------------------------------------------------
/// Basic notification
///-----------------------------------------------------------------------------

#define BASIC_NOTIFY0(stream, str)                                             \
    fprintf(stream, str);                                                      \
    fprintf(stream, "\n");                                                     \
    fflush(stream);

#define BASIC_NOTIFY1(stream, str, arg1)                                       \
    fprintf(stream, str, (arg1));                                              \
    fprintf(stream, "\n");                                                     \
    fflush(stream);

#define BASIC_NOTIFY2(stream, str, arg1, arg2)                                 \
    fprintf(stream, str, (arg1), (arg2));                                      \
    fprintf(stream, "\n");                                                     \
    fflush(stream);

#define BASIC_NOTIFY3(stream, str, arg1, arg2, arg3)                           \
    fprintf(stream, str, (arg1), (arg2), (arg3));                              \
    fprintf(stream, "\n");                                                     \
    fflush(stream);

#define BASIC_NOTIFY4(stream, str, arg1, arg2, arg3, arg4)                     \
    fprintf(stream, str, (arg1), (arg2), (arg3), (arg4));                      \
    fprintf(stream, "\n");                                                     \
    fflush(stream);

#define BASIC_NOTIFY5(stream, str, arg1, arg2, arg3, arg4, arg5)               \
    fprintf(stream, str, (arg1), (arg2), (arg3), (arg4), (arg5));              \
    fprintf(stream, "\n");                                                     \
    fflush(stream);

///-----------------------------------------------------------------------------
/// Notification and warnings
///-----------------------------------------------------------------------------

#ifdef SILENT

#define NOTIFY0(str)
#define NOTIFY1(str, arg1)
#define NOTIFY2(str, arg1, arg2)
#define NOTIFY3(str, arg1, arg2, arg3)
#define NOTIFY4(str, arg1, arg2, arg3, arg4)
#define NOTIFY5(str, arg1, arg2, arg3, arg4, arg5)

#define WARNING0(str)
#define WARNING1(str, arg1)
#define WARNING2(str, arg1, arg2)
#define WARNING3(str, arg1, arg2, arg3)
#define WARNING4(str, arg1, arg2, arg3, arg4)
#define WARNING5(str, arg1, arg2, arg3, arg4, arg5)

#else

#define NOTIFY0(str)                                                           \
    BASIC_NOTIFY0(stdout, str)

#define NOTIFY1(str, arg1)                                                     \
    BASIC_NOTIFY1(stdout, str, arg1)

#define NOTIFY2(str, arg1, arg2)                                               \
    BASIC_NOTIFY2(stdout, str, arg1, arg2)

#define NOTIFY3(str, arg1, arg2, arg3)                                         \
    BASIC_NOTIFY3(stdout, str, arg1, arg2, arg3)

#define NOTIFY4(str, arg1, arg2, arg3, arg4)                                   \
    BASIC_NOTIFY4(stdout, str, arg1, arg2, arg3, arg4)

#define NOTIFY5(str, arg1, arg2, arg3, arg4, arg5)                             \
    BASIC_NOTIFY5(stdout, str, arg1, arg2, arg3, arg4, arg5)

#define WARNING_PRE_STR                                                        \
    fprintf(stdout, "\n*** WARNING [system]: ")

#define WARNING0(str)                                                          \
    WARNING_PRE_STR;                                                           \
    BASIC_NOTIFY0(stdout, str);

#define WARNING1(str, arg1)                                                    \
    WARNING_PRE_STR;                                                           \
    BASIC_NOTIFY1(stdout, str, arg1);

#define WARNING2(str, arg1, arg2)                                              \
    WARNING_PRE_STR;                                                           \
    BASIC_NOTIFY2(stdout, str, arg1, arg2);

#define WARNING3(str, arg1, arg2, arg3)                                        \
    WARNING_PRE_STR;                                                           \
    BASIC_NOTIFY3(stdout, str, arg1, arg2, arg3);

#define WARNING4(str, arg1, arg2, arg3, arg4)                                  \
    WARNING_PRE_STR;                                                           \
    BASIC_NOTIFY4(stdout, str, arg1, arg2, arg3, arg4);

#define WARNING5(str, arg1, arg2, arg3, arg4, arg5)                            \
    WARNING_PRE_STR;                                                           \
    BASIC_NOTIFY5(stdout, str, arg1, arg2, arg3, arg4, arg5);

#endif /* SILENT */

///-----------------------------------------------------------------------------
/// Serious warning (non-continuable)
///-----------------------------------------------------------------------------

#define SERIOUS_WARNING_PRE_STR                                                \
    fprintf(stdout, "\n*** ERROR [system]: ")

#define SERIOUS_WARNING0(str)                                                  \
    SERIOUS_WARNING_PRE_STR;                                                   \
    BASIC_NOTIFY0(stdout, str)

#define SERIOUS_WARNING1(str, arg1)                                            \
    SERIOUS_WARNING_PRE_STR;                                                   \
    BASIC_NOTIFY1(stdout, str, arg1)

#define SERIOUS_WARNING2(str, arg1, arg2)                                      \
    SERIOUS_WARNING_PRE_STR;                                                   \
    BASIC_NOTIFY2(stdout, str, arg1, arg2)

#define SERIOUS_WARNING3(str, arg1, arg2, arg3)                                \
    SERIOUS_WARNING_PRE_STR;                                                   \
    BASIC_NOTIFY3(stdout, str, arg1, arg2, arg3)

#define SERIOUS_WARNING4(str, arg1, arg2, arg3, arg4)                          \
    SERIOUS_WARNING_PRE_STR;                                                   \
    BASIC_NOTIFY4(stdout, str, arg1, arg2, arg3, arg4)

#define SERIOUS_WARNING5(str, arg1, arg2, arg3, arg4, arg5)                    \
    SERIOUS_WARNING_PRE_STR;                                                   \
    BASIC_NOTIFY5(stdout, str, arg1, arg2, arg3, arg4, arg5)

///-----------------------------------------------------------------------------
#endif // NOTIFY_H
///-----------------------------------------------------------------------------
