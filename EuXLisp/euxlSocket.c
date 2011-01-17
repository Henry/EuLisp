/// Copyright 1994 Russell Bradford
/// Copyright 2010, 2011 Henry G. Weller
///-----------------------------------------------------------------------------
//  This file is part of
/// ---                           EuLisp System 'EuXLisp'
///-----------------------------------------------------------------------------
//
//  EuXLisp is free software: you can redistribute it and/or modify it under the
//  terms of the GNU General Public License version 2 as published by the Free
//  Software Foundation.
//
//  EuXLisp is distributed in the hope that it will be useful, but WITHOUT ANY
//  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
//  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
//  details.
//
//  You should have received a copy of the GNU General Public License along with
//  this program.  If not, see <http://www.gnu.org/licenses/>.
//
///-----------------------------------------------------------------------------
/// Title: Socket manipulation
///  Notes:
//    This code possibly written by David Halls.
///  WARNING:
//    this code is under development, and is possibly buggy
///  Maintainer: Henry G. Weller
///-----------------------------------------------------------------------------
#ifdef SOCK

#include "euxlisp.h"

#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <fcntl.h>
#include <sys/time.h>
#include <string.h>
#include <rpc/types.h>
#include <rpc/xdr.h>
#include <errno.h>

#ifndef SEEK_CUR
#define SEEK_CUR 1
#endif

///-----------------------------------------------------------------------------
/// Macros
///-----------------------------------------------------------------------------
#if 1
#define HANDLE(expr)                                                           \
    reading = 1;                                                               \
    expr;                                                                      \
    reading = 0;                                                               \
    if (ctrl_c)                                                                \
    {                                                                          \
        ctrl_c = 0;                                                            \
        euxcOSFlush();                                                         \
        euxcTopLevel();                                                        \
    }
#else
#define HANDLE(expr) expr
#endif

///-----------------------------------------------------------------------------
/// Functions
///-----------------------------------------------------------------------------
///  euxlSocketError
void euxlSocketError(char *msg, euxlValue val)
{
    char buf[128];

    #ifdef HAVE_STRERROR
    sprintf(buf, "%s: %s", msg, strerror(errno));
    #else
    #ifdef HAVE_ERRLIST
    sprintf(buf, "%s: %s", msg, syeuxls_errlist[errno]);
    #else
    sprintf(buf, "%s: errno = %d", msg, errno);
    #endif
    #endif

    errno = 0;
    euxcCerror(buf, val, euxls_socket_error);
}

///  euxcSocketSocket - (socket-socket)
euxlValue euxcSocketSocket()
{
    static char *functionName = "socket-socket";
    int s;

    euxmLastArg();

    if ((s = socket(AF_INET, SOCK_euxmStream, 0)) < 0)
    {
        euxlSocketError(functionName, euxcMakeString("inet-stream"));
    }

    return (euxcMakeFPI((euxmFPIType) s));
}

///  euxcSocketConnect - (socket-connect fd name stream)
euxlValue euxcSocketConnect()
{
    static char *functionName = "socket-connect";

    euxlValue arg_s = euxmGetArgFPI();
    int s = (int)euxmGetFPI(arg_s);

    euxlValue arg_name = euxmGetArgString();
    char *name = euxmGetString(arg_name);

    euxlValue arg_stream = euxmGetArgFPI();
    int stream = (int)euxmGetFPI(arg_stream);

    euxmLastArg();

    #if 0
    if (stream > 0 && stream < IPPORT_RESERVED)
    {
        euxcCerror
        (
            "socket-connect: stream out of range",
            arg_stream,
            euxls_socket_error
        );
    }
    #endif

    struct sockaddr_in sin;
    struct hostent *hp;
    if ((hp = gethostbyname(name)) != NULL)
    {
        memcpy(&sin.sin_addr.euxls_addr, hp->h_addr, hp->h_length);
    }
    else
    {
        euxcCerror("socket-connect: unknown host", arg_name, euxls_socket_error);
    }

    sin.sin_family = AF_INET;
    sin.sin_port = (stream <= 0) ? 0 : htons(stream);

    if (connect(s, (struct sockaddr *)&sin, sizeof sin) < 0)
    {
        euxlSocketError("socket-connect", euxcCons(arg_name, arg_stream));
    }

    return euxs_t;
}

///  euxcSocketBind - (socket-bind fd stream)
euxlValue euxcSocketBind()
{
    static char *functionName = "socket-bind";

    euxlValue arg_s = euxmGetArgFPI();
    int s = (int)euxmGetFPI(arg_s);

    euxlValue arg_stream = euxmGetArgFPI();
    int stream = (int)euxmGetFPI(arg_stream);

    euxmLastArg();

    if (stream > 0 && stream < IPPORT_RESERVED)
    {
        euxcCerror
        (
            "socket-bind: stream out of range",
            arg_stream,
            euxls_socket_error
        );
    }

    struct sockaddr_in sin;
    sin.sin_family = AF_INET;
    sin.sin_addr.euxls_addr = htonl(INADDR_ANY);
    sin.sin_port = (stream <= 0) ? 0 : htons(stream);

    if (bind(s, (struct sockaddr *)&sin, sizeof sin) < 0)
    {
        euxlSocketError(functionName, arg_stream);
    }

    return euxs_t;
}

///  euxcSocketListen - (socket-listen fd backlog)
euxlValue euxcSocketListen()
{
    static char *functionName = "socket-listen";

    euxlValue arg_s = euxmGetArgFPI();
    int s = (int)euxmGetFPI(arg_s);

    euxlValue arg_backlog = euxmGetArgFPI();
    int backlog = (int)euxmGetFPI(arg_backlog);

    euxmLastArg();

    if (listen(s, backlog) < 0)
    {
        euxlSocketError(functionName, arg_s);
    }

    return euxs_t;
}

///  euxcSocketAccept - (socket-accept fd)
euxlValue euxcSocketAccept()
{
    static char *functionName = "socket-accept";

    struct sockaddr_in sin;
    socklen_t len = sizeof sin;

    euxlValue arg_s = euxmGetArgFPI();
    int s = (int)euxmGetFPI(arg_s);

    euxmLastArg();

    int new_socket;
    if ((new_socket = accept(s, (struct sockaddr *)&sin, &len)) < 0)
    {
        euxlSocketError(functionName, arg_s);
    }

    return (euxcMakeFPI((euxmFPIType) new_socket));
}

///  euxcSocketBlock - (socket-block fd)
euxlValue euxcSocketBlock()
{
    static char *functionName = "socket-block";

    euxlValue arg_s = euxmGetArgFPI();
    int s = (int)euxmGetFPI(arg_s);

    euxmLastArg();

    int fileflags;
    if ((fileflags = fcntl(s, F_GETFL)) < 0)
    {
        euxlSocketError(functionName, arg_s);
    }

    if (fcntl(s, F_SETFL, fileflags & ~O_NDELAY) < 0)
    {
        euxlSocketError(functionName, arg_s);
    }

    return euxs_t;
}

///  euxcSocketNonBlock - (socket-nonblock fd)
euxlValue euxcSocketNonBlock()
{
    static char *functionName = "socket-nonblock";

    euxlValue arg_s = euxmGetArgFPI();
    int s = (int)euxmGetFPI(arg_s);

    euxmLastArg();

    int fileflags;
    if ((fileflags = fcntl(s, F_GETFL)) < 0)
    {
        euxlSocketError(functionName, arg_s);
    }

    if (fcntl(s, F_SETFL, fileflags | O_NDELAY) < 0)
    {
        euxlSocketError(functionName, arg_s);
    }

    return euxs_t;
}

///  euxcSocketReuse - (socket-reuse fd)
euxlValue euxcSocketReuse()
{
    static char *functionName = "socket-reuse";

    euxlValue arg_s = euxmGetArgFPI();
    int s = (int)euxmGetFPI(arg_s);

    euxmLastArg();

    int val = 1;
    setsockopt(s, SOL_SOCKET, SO_REUSEADDR, (char *)&val, sizeof val);

    return euxs_t;
}

///  euxcSocketNoReuse - (socket-noreuse fd)
euxlValue euxcSocketNoReuse()
{
    static char *functionName = "socket-noreuse";

    euxlValue arg_s = euxmGetArgFPI();
    int s = (int)euxmGetFPI(arg_s);

    euxmLastArg();

    int val = 0;
    setsockopt(s, SOL_SOCKET, SO_REUSEADDR, (char *)&val, sizeof val);

    return euxs_t;
}

///  euxcSocketClose - (socket-close fd)
euxlValue euxcSocketClose()
{
    static char *functionName = "socket-close";

    euxlValue arg_s = euxmGetArgFPI();
    int s = (int)euxmGetFPI(arg_s);

    euxmLastArg();

    euxcSocketClose(s);

    return euxs_t;
}

///  euxcSocketShutdown - (socket-shutdown fd how)
euxlValue euxcSocketShutdown()
{
    static char *functionName = "socket-shutdown";

    euxlValue arg_s = euxmGetArgFPI();
    int s = (int)euxmGetFPI(arg_s);

    euxlValue arg_how = euxmGetArgFPI();
    int how = (int)euxmGetFPI(arg_how);

    euxmLastArg();

    if (shutdown(s, how) < 0)
    {
        euxlSocketError(functionName, arg_s);
    }

    return euxs_t;
}

///  euxcSocketPeerAddr - (socket-peeraddr fd)
euxlValue euxcSocketPeerAddr()
{
    static char *functionName = "socket-peeraddr";

    struct sockaddr_in peer_name;
    socklen_t peer_namelen = sizeof peer_name;
    struct hostent *hp;

    euxlValue arg_s = euxmGetArgFPI();
    int s = (int)euxmGetFPI(arg_s);

    euxmLastArg();

    if (getpeername(s, (struct sockaddr *)&peer_name, &peer_namelen) < 0)
    {
        euxlSocketError(functionName, arg_s);
    }

    if
    (
        (hp =
        gethostbyaddr
        (
            (char *)&peer_name.sin_addr.euxls_addr,
            sizeof peer_name.sin_addr.euxls_addr,
            AF_INET
        )
        ) == NULL
    )
    {
        euxcCerror
        (
            "socket-peeraddr: unknown address",
            euxcMakeFPI((euxmFPIType) peer_name.sin_addr.euxls_addr),
            euxls_socket_error
        );
    }

    return (euxcMakeString(hp->h_name));
}

///  euxcSocketPeerStream - (socket-peerstream fd)
euxlValue euxcSocketPeerStream()
{
    static char *functionName = "socket-peerstream";

    euxlValue arg_s = euxmGetArgFPI();
    int s = (int)euxmGetFPI(arg_s);

    euxmLastArg();

    struct sockaddr_in peer_name;
    socklen_t peer_namelen = sizeof peer_name;
    if (getpeername(s, (struct sockaddr *)&peer_name, &peer_namelen) < 0)
    {
        euxlSocketError(functionName, arg_s);
    }

    return (euxcMakeFPI((euxmFPIType) ntohs(peer_name.sin_port)));
}

///  euxcSocketSockAddr - (socket-sockaddr fd)
euxlValue euxcSocketSockAddr()
{
    static char *functionName = "socket-sockaddr";

    euxlValue arg_s = euxmGetArgFPI();
    int s = (int)euxmGetFPI(arg_s);

    euxmLastArg();

    struct sockaddr_in sock_name;
    socklen_t sock_namelen = sizeof sock_name;
    if (getsockname(s, (struct sockaddr *)&sock_name, &sock_namelen) < 0)
    {
        euxlSocketError(functionName, arg_s);
    }

    struct hostent *hp;
    if
    (
        (hp = gethostbyaddr
        (
            (char *)&sock_name.sin_addr.euxls_addr,
            sizeof sock_name.sin_addr.euxls_addr,
            AF_INET
        )
        ) == NULL
    )
    {
        euxcCerror
        (
            "socket-sockaddr: unknown address",
            euxcMakeFPI((euxmFPIType) sock_name.sin_addr.euxls_addr),
            euxls_socket_error
        );
    }

    return (euxcMakeString(hp->h_name));
}

///  euxcSocketSockStream - (socket-sockstream fd)
euxlValue euxcSocketSockStream()
{
    static char *functionName = "socket-sockstream";

    euxlValue arg_s = euxmGetArgFPI();
    int s = (int)euxmGetFPI(arg_s);

    euxmLastArg();

    struct sockaddr_in sock_name;
    socklen_t sock_namelen = sizeof sock_name;
    if (getsockname(s, (struct sockaddr *)&sock_name, &sock_namelen) < 0)
    {
        euxlSocketError(functionName, arg_s);
    }

    return (euxcMakeFPI((euxmFPIType) ntohs(sock_name.sin_port)));
}

///  euxcSocketHostToIP - (socket-host-to-ip name)
euxlValue euxcSocketHostToIP()
{
    static char *functionName = "socket-host-to-ip";

    euxlValue arg_name = euxmGetArgString();
    char *name = euxmGetString(arg_name);

    euxmLastArg();

    struct hostent *hp;
    if ((hp = gethostbyname(name)) == NULL)
    {
        euxcCerror("host->ip: unknown host", arg_name, euxls_socket_error);
    }

    struct in_addr in;
    memcpy(&in.euxls_addr, hp->h_addr, hp->h_length);

    return (euxcMakeString(inet_ntoa(in)));
}

///  euxcSocketIPToHost - (socket-ip-to-host ip)
euxlValue euxcSocketIPToHost()
{
    static char *functionName = "socket-ip-to-host";

    euxlValue arg_ip = euxmGetArgString();
    char *ip = euxmGetString(arg_ip);

    euxmLastArg();

    unsigned long inet;
    if ((inet = inet_addr(ip)) < 0)
    {
        euxcCerror
        (
            "ip->host: cannot convert IP address",
            arg_ip,
            euxls_socket_error
        );
    }

    struct hostent *hp;
    if ((hp = gethostbyaddr((char *)&inet, sizeof inet, AF_INET)) == NULL)
    {
        euxcCerror("ip->host: unknown ip address", arg_ip, euxls_socket_error);
    }

    return (euxcMakeString(hp->h_name));
}

///  euxcSocketToStream - (socket-convert-to-stream fd)
//     convert to stream to get I/O functions for free
euxlValue euxcSocketToStream()
{
    static char *functionName = "socket-convert-to-stream";

    euxlValue arg_s = euxmGetArgFPI();
    int s = (int)euxmGetFPI(arg_s);

    euxmLastArg();

    FILE *handle;
    if ((handle = fdopen(s, "a+")) == NULL)
    {
        euxlSocketError(functionName, arg_s);
    }

    // make socket unbuffered so we don't lose so much data
    setvbuf(handle, NULL, _IONBF, 0);

    return euxcMakeStream(handle, euxmPortFlagInput | euxmPortFlagOutput);
}

///  euxcStreamFd - (stream-fd stream)
euxlValue euxcStreamFd()
{
    static char *functionName = "stream-fd";

    euxlValue arg_handle = euxmGetArgStream();
    FILE *handle = euxmGetFile(arg_handle);

    euxmLastArg();

    return (euxcMakeFPI((euxmFPIType) fileno(handle)));
}

// buffering stuff loses data on most OSes
#ifdef __linux
///  euxcStreamUnbuffered - (stream-unbuffered stream)
euxlValue euxcStreamUnbuffered()
{
    static char *functionName = "stream-unbuffered";

    euxlValue arg_handle = euxmGetArgStream();
    FILE *handle = euxmGetFile(arg_handle);

    euxmLastArg();

    fflush(handle);
    if (setvbuf(handle, NULL, _IONBF, 0) < 0)
    {
        euxlSocketError(functionName, arg_handle);
    }

    return euxs_t;
}

///  euxcStreamBlockBuffered - (stream-block-buffered stream . bufsize)
euxlValue euxcStreamBlockBuffered()
{
    static char *functionName = "stream-block-buffered";

    euxlValue arg_handle = euxmGetArgStream();
    FILE *handle = euxmGetFile(arg_handle);

    int size;
    if (euxmMoreArgs())
    {
        euxlValue arg_size = euxmGetArgFPI();
        size = euxmGetFPI(arg_size);
        euxmLastArg();
    }
    else
    {
        size = BUFSIZ;
    }

    fflush(handle);
    if (setvbuf(handle, NULL, _IOFBF, size) < 0)
    {
        euxlSocketError(functionName, arg_handle);
    }

    return euxs_t;
}

///  euxcStreamLineBuffered - (stream-line-buffered stream)
euxlValue euxcStreamLineBuffered()
{
    static char *functionName = "stream-line-buffered";

    euxlValue arg_handle = euxmGetArgStream();
    FILE *handle = euxmGetFile(arg_handle);

    euxmLastArg();

    fflush(handle);
    if (setvbuf(handle, NULL, _IOLBF, 0) < 0)
    {
        euxlSocketError(functionName, arg_handle);
    }

    return euxs_t;
}
#else
///  euxcStreamUnbuffered - (stream-unbuffered stream)
euxlValue euxcStreamUnbuffered()
{
    static char *functionName = "stream-unbuffered";

    euxlValue arg_handle = euxmGetArgStream();
    FILE *handle = euxmGetFile(arg_handle);

    euxmLastArg();

    // do nothing

    return euxs_t;
}

///  euxcStreamBlockBuffered - (stream-block-buffered stream . bufsize)
euxlValue euxcStreamBlockBuffered()
{
    static char *functionName = "stream-block-buffered";

    euxlValue arg_handle = euxmGetArgStream();
    FILE *handle = euxmGetFile(arg_handle);

    int size;
    if (euxmMoreArgs())
    {
        euxlValue arg_size = euxmGetArgFPI();
        size = euxmGetFPI(arg_size);
        euxmLastArg();
    }
    else
    {
        size = BUFSIZ;
    }

    // do nothing

    return euxs_t;
}

///  euxcStreamLineBuffered - (stream-line-buffered stream)
euxlValue euxcStreamLineBuffered()
{
    static char *functionName = "stream-line-buffered";

    euxlValue arg_handle = euxmGetArgStream();
    FILE *handle = euxmGetFile(arg_handle);

    euxmLastArg();

    // do nothing

    return euxs_t;
}
#endif // no more buffering

///-----------------------------------------------------------------------------
/// Select functions
///-----------------------------------------------------------------------------
static fd_set readfds, writefds;

///  euxcSocketFdZeroRead - (socket-fd-zero-read)
euxlValue euxcSocketFdZeroRead()
{
    static char *functionName = "socket-fd-zero-read";

    euxmLastArg();
    FD_ZERO(&readfds);
    return euxs_t;
}

///  euxcSocketFdSetRead - (socket-fd-set-read fd)
euxlValue euxcSocketFdSetRead()
{
    static char *functionName = "socket-fd-set-read";

    euxlValue arg_fd = euxmGetArgFPI();
    int fd = (int)euxmGetFPI(arg_fd);

    euxmLastArg();

    FD_SET(fd, &readfds);
    return euxs_t;

}

///  euxcSocketFdIssetRead - (socket-fd-isset-read fd)
euxlValue euxcSocketFdIssetRead()
{
    static char *functionName = "socket-fd-isset-read";

    euxlValue arg_fd = euxmGetArgFPI();
    int fd = (int)euxmGetFPI(arg_fd);

    euxmLastArg();

    return FD_ISSET(fd, &readfds) ? euxs_t : euxmNil;
}

///  euxcSocketSelectRead - (socket-select-read timeout)
euxlValue euxcSocketSelectRead()
{
    static char *functionName = "socket-select-read";

    euxlValue arg_time = euxmGetArgFPI();
    int time = (int)euxmGetFPI(arg_time);

    euxmLastArg();

    struct timeval timeout;
    if (time >= 0)
    {
        timeout.tv_sec = time / 1000;
        timeout.tv_usec = (time - (timeout.tv_sec * 1000)) * 1000;
    }

    switch
    (
        select
        (
            FD_SETSIZE,
            &readfds,
            NULL,
            NULL,
            (time < 0) ? NULL : &timeout
        )
    )
    {
        case -1:
            euxlSocketError(functionName, arg_time);
            break;
        case 0:
            return euxmNil;
        default:
            return euxs_t;
    }

    return euxmNil; // not reached
}

///  euxcSocketFdZeroWrite - (socket-fd-zero-write)
euxlValue euxcSocketFdZeroWrite()
{
    static char *functionName = "socket-fd-zero-write";

    euxmLastArg();
    FD_ZERO(&writefds);
    return euxs_t;
}

///  euxcSocketFdSetWrite - (socket-fd-set-write fd)
euxlValue euxcSocketFdSetWrite()
{
    static char *functionName = "socket-fd-set-write";

    euxlValue arg_fd = euxmGetArgFPI();
    int fd = (int)euxmGetFPI(arg_fd);

    euxmLastArg();

    FD_SET(fd, &writefds);
    return euxs_t;
}

///  euxcSocketFdIssetWrite - (socket-fd-isset-write fd)
euxlValue euxcSocketFdIssetWrite()
{
    static char *functionName = "socket-fd-isset-write";

    euxlValue arg_fd = euxmGetArgFPI();
    int fd = (int)euxmGetFPI(arg_fd);

    euxmLastArg();

    return FD_ISSET(fd, &writefds) ? euxs_t : euxmNil;
}

///  euxcSocketSelectWrite - (socket-select-write timeout)
euxlValue euxcSocketSelectWrite()
{
    static char *functionName = "socket-select-write";

    euxlValue arg_time = euxmGetArgFPI();
    int time = (int)euxmGetFPI(arg_time);

    euxmLastArg();

    struct timeval timeout;
    if (time >= 0)
    {
        timeout.tv_sec = time / 1000;
        timeout.tv_usec = (time - (timeout.tv_sec * 1000)) * 1000;
    }

    switch
    (
        select
        (
            FD_SETSIZE,
            NULL,
            &writefds,
            NULL,
            (time < 0) ? NULL : &timeout
        )
    )
    {
        case -1:
            euxlSocketError(functionName, arg_time);
            break;
        case 0:
            return euxmNil;
        default:
            return euxs_t;
    }

    return euxmNil; // not reached
}

///-----------------------------------------------------------------------------
/// Utility wrappers for XDR I/O
///-----------------------------------------------------------------------------

///  euxcXdrSendInt
bool_t euxcXdrSendInt(FILE *handle, int i)
{
    // following may return 'bad fseek' but seems to fix interleaving reads and
    // writes
    fseek(handle, 0L, SEEK_CUR);
    errno = 0;
    XDR xdrs;
    xdrstdio_create(&xdrs, handle, XDR_ENeuxmCode);
    bool_t retval = xdr_int(&xdrs, &i);
    fflush(handle);
    xdr_destroy(&xdrs);
    return retval;
}

///  euxcXdrRecvInt
bool_t euxcXdrRecvInt(FILE *handle, int *ip)
{
    fseek(handle, 0L, SEEK_CUR);
    errno = 0;
    XDR xdrs;
    xdrstdio_create(&xdrs, handle, XDR_DEeuxmCode);
    bool_t retval;
    HANDLE(retval = xdr_int(&xdrs, ip));
    fflush(handle);
    xdr_destroy(&xdrs);
    return retval;
}

///  euxcXdrSendDouble
bool_t euxcXdrSendDouble(FILE *handle, double i)
{
    fseek(handle, 0L, SEEK_CUR);
    errno = 0;
    XDR xdrs;
    xdrstdio_create(&xdrs, handle, XDR_ENeuxmCode);
    bool_t retval = xdr_double(&xdrs, &i);
    fflush(handle);
    xdr_destroy(&xdrs);
    return retval;
}

///  euxcXdrRecvDouble
bool_t euxcXdrRecvDouble(FILE *handle, double *ip)
{
    fseek(handle, 0L, SEEK_CUR);
    errno = 0;
    XDR xdrs;
    xdrstdio_create(&xdrs, handle, XDR_DEeuxmCode);
    bool_t retval;
    HANDLE(retval = xdr_double(&xdrs, ip));
    fflush(handle);
    xdr_destroy(&xdrs);
    return retval;
}

///  euxcXdrSendString
bool_t euxcXdrSendString(FILE *handle, char *i, int len)
{
    fseek(handle, 0L, SEEK_CUR);
    errno = 0;
    XDR xdrs;
    xdrstdio_create(&xdrs, handle, XDR_ENeuxmCode);
    bool_t retval = xdr_string(&xdrs, &i, len);
    fflush(handle);
    xdr_destroy(&xdrs);
    return retval;
}

///  euxcXdrRecvString
bool_t euxcXdrRecvString(FILE *handle, char **ip, int len)
{
    fseek(handle, 0L, SEEK_CUR);
    errno = 0;
    XDR xdrs;
    xdrstdio_create(&xdrs, handle, XDR_DEeuxmCode);
    bool_t retval;
    HANDLE(retval = xdr_string(&xdrs, ip, len));
    fflush(handle);
    xdr_destroy(&xdrs);
    return retval;
}

///-----------------------------------------------------------------------------
/// Entry functions for XDR I/O to and from streams
///-----------------------------------------------------------------------------

///  euxcStreamXdrSendInt - (stream-xdr-send-int stream val)
euxlValue euxcStreamXdrSendInt()
{
    static char *functionName = "stream-xdr-send-int";

    euxlValue arg_handle = euxmGetArgOstream();
    FILE *handle = euxmGetFile(arg_handle);

    euxlValue arg_i = euxmGetArgFPI();
    int i = (int)euxmGetFPI(arg_i);

    euxmLastArg();

    if (euxcXdrSendInt(handle, i) == euxmFalse)
    {
        euxlSocketError(functionName, euxcCons(arg_handle, arg_i));
    }

    return euxs_t;
}

///  euxcStreamXdrRecvInt - (stream-xdr-recv-int stream)
euxlValue euxcStreamXdrRecvInt()
{
    static char *functionName = "stream-xdr-recv-int";

    euxlValue arg_handle = euxmGetArgIstream();
    FILE *handle = euxmGetFile(arg_handle);

    euxmLastArg();

    int i;
    if (euxcXdrRecvInt(handle, &i) == euxmFalse)
    {
        euxlSocketError(functionName, arg_handle);
    }

    return (euxcMakeFPI((euxmFPIType) i));
}

///  euxcStreamXdrSendDoubleFloat - (stream-xdr-send-float stream val)
euxlValue euxcStreamXdrSendDoubleFloat()
{
    static char *functionName = "stream-xdr-send-float";

    euxlValue arg_handle = euxmGetArgOstream();
    FILE *handle = euxmGetFile(arg_handle);

    euxlValue arg_i = euxmGetArgNumber();

    double i;
    if (euxmFPIp(arg_i))
    {
        i = (double)euxmGetFPI(arg_i);
    }
    else
    {
        i = (double)euxmGetDoubleFloat(arg_i);
    }

    euxmLastArg();

    if (euxcXdrSendDouble(handle, i) == euxmFalse)
    {
        euxlSocketError(functionName, euxcCons(arg_handle, arg_i));
    }

    return euxs_t;
}

///  euxcStreamXdrRecvDoubleFloat - (stream-xdr-recv-float stream)
euxlValue euxcStreamXdrRecvDoubleFloat()
{
    static char *functionName = "stream-xdr-recv-float";

    euxlValue arg_handle = euxmGetArgIstream();
    FILE *handle = euxmGetFile(arg_handle);

    euxmLastArg();

    double i;
    if (euxcXdrRecvDouble(handle, &i) == euxmFalse)
    {
        euxlSocketError(functionName, arg_handle);
    }

    return (euxcMakeDoubleFloat((euxmDoubleFloatType) i));
}

///  euxcStreamXdrSendString - (stream-xdr-send-string stream val)
euxlValue euxcStreamXdrSendString()
{
    static char *functionName = "stream-xdr-send-string";

    euxlValue arg_handle = euxmGetArgOstream();
    FILE *handle = euxmGetFile(arg_handle);

    euxlValue arg_string = euxmGetArgString();
    char *string = euxmGetString(arg_string);

    euxmLastArg();

    int i = euxmGetStringlength(arg_string);

    if (euxcXdrSendInt(handle, i) == euxmFalse)
    {
        euxlSocketError(functionName, euxcCons(arg_handle, arg_string));
    }

    if (euxcXdrSendString(handle, string, i) == euxmFalse)
    {
        euxlSocketError(functionName, euxcCons(arg_handle, arg_string));
    }

    return euxs_t;
}

///  euxcStreamXdrRecvString - (stream-xdr-recv-string stream)
euxlValue euxcStreamXdrRecvString()
{
    static char *functionName = "stream-xdr-recv-string";

    euxlValue arg_handle = euxmGetArgIstream();
    FILE *handle = euxmGetFile(arg_handle);

    euxmLastArg();

    int i;
    if (euxcXdrRecvInt(handle, &i) == euxmFalse)
    {
        euxlSocketError(functionName, arg_handle);
    }

    char *buffer;
    if ((buffer = (char *)malloc(i*sizeof(char))) == NULL)
    {
        euxcCerror
        (
            "stream-xdr-recv-string: failed to allocate memory",
            arg_handle,
            euxls_socket_error
        );
    }

    if (euxcXdrRecvString(handle, &buffer, i) == euxmFalse)
    {
        euxlSocketError(functionName, arg_handle);
    }

    buffer[i - 1] = '\0';

    euxlValue temp = euxcMakeString2(buffer, i - 1);
    free(buffer);

    return temp;
}

#else
// some linkers complain of empty files
void euxcNoSockets()
{
}
#endif


///-----------------------------------------------------------------------------
