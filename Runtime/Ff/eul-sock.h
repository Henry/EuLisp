/** Copyright (c) 1997 by A Kind & University of Bath. All rights reserved. **/

/** ----------------------------------------------------------------------- **
 **                     EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Library: fsocket
 **  Authos: Andreas Kind
 **  Description: foreign socket connection
 ** ----------------------------------------------------------------------- **/

#ifndef EUL_SOCK_H
#define EUL_SOCK_H

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <errno.h>
#include <sys/utsname.h>

/** ----------------------------------------------------------------- **
 ** Externals
 ** ----------------------------------------------------------------- **/

extern int eul_make_socket(char *, char *, int);
extern int eul_socket_accept(int);
extern int eul_make_connection(char *, char *, char *);
extern char *eul_socket_strerror(int);
extern char *eul_hostname();


/** ----------------------------------------------------------------- **
 ** Error codes
 ** ----------------------------------------------------------------- **/

#define EUL_SOCK_ERROR -1
#define EUL_SOCK_NO_SERVICE -2
#define EUL_SOCK_NO_HOST -3
#define EUL_SOCK_NO_PROTOCOL -4


/** ----------------------------------------------------------------- **
 ** Constants
 ** ----------------------------------------------------------------- **/

#ifndef INADDR_NONE
#define INADDR_NONE 0xffffffff
#endif


#endif // EUL_SOCK_H
