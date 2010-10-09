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
///  Title: foreign socket connection
///  Library: fsocket
///  Authors: Andreas Kind
///  Maintainer: Henry G. Weller
///-----------------------------------------------------------------------------
#ifndef EUL_SOCK_H
#define EUL_SOCK_H
///-----------------------------------------------------------------------------

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <errno.h>
#include <sys/utsname.h>

///-----------------------------------------------------------------------------
/// Externals
///-----------------------------------------------------------------------------
extern int eul_make_socket(char *, char *, int);
extern int eul_socket_accept(int);
extern int eul_make_connection(char *, char *, char *);
extern char *eul_socket_strerror(int);
extern char *eul_hostname();


///-----------------------------------------------------------------------------
/// Error codes
///-----------------------------------------------------------------------------
#define EUL_SOCK_ERROR -1
#define EUL_SOCK_NO_SERVICE -2
#define EUL_SOCK_NO_HOST -3
#define EUL_SOCK_NO_PROTOCOL -4


///-----------------------------------------------------------------------------
/// Constants
///-----------------------------------------------------------------------------
#ifndef INADDR_NONE
#define INADDR_NONE 0xffffffff
#endif


///-----------------------------------------------------------------------------
#endif // EUL_SOCK_H
///-----------------------------------------------------------------------------
