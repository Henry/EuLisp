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
#ifdef _ALL_SOURCE
#include <sys/select.h>
#endif

#include "euxlisp.h"

#ifndef SEEK_CUR
#define SEEK_CUR 1
#endif

#include "euxlsymbols.h"
#include "euxlproto.h"

///-----------------------------------------------------------------------------
/// Functions
///-----------------------------------------------------------------------------
#if 1
extern int reading, ctrl_c;
#define HANDLE(expr)                                                           \
    reading = 1;                                                               \
    expr;                                                                      \
    reading = 0;                                                               \
    if (ctrl_c)                                                                \
    {                                                                          \
        ctrl_c = 0;                                                            \
        osflush();                                                             \
        xltoplevel();                                                          \
    }
#else
#define HANDLE(expr) expr
#endif

#define FALSEVALUE NIL
#define TRUEVALUE  true

void xlsockerror(char *msg, euxlValue val)
{
    char buf[128];

    #ifdef HAVE_STRERROR
    sprintf(buf, "%s: %s", msg, strerror(errno));
    #else
    #ifdef HAVE_ERRLIST
    sprintf(buf, "%s: %s", msg, sys_errlist[errno]);
    #else
    sprintf(buf, "%s: errno = %d", msg, errno);
    #endif
    #endif

    errno = 0;
    xlcerror(buf, val, s_socket_error);
}

// (socket-socket)
euxlValue socket_socket()
{
    static char *cfn_name = "socket-socket";
    int s;

    xllastarg();

    if ((s = socket(AF_INET, SOCK_STREAM, 0)) < 0)
    {
        xlsockerror(cfn_name, cvstring("inet-stream"));
    }

    return (cvfixnum((FIXTYPE) s));
}

// (socket-connect fd name stream)
euxlValue socket_connect()
{
    static char *cfn_name = "socket-connect";

    euxlValue arg_s = xlgafixnum();
    int s = (int)getfixnum(arg_s);

    euxlValue arg_name = xlgastring();
    char *name = (char *)getstring(arg_name);

    euxlValue arg_stream = xlgafixnum();
    int stream = (int)getfixnum(arg_stream);

    xllastarg();

    #if 0
    if (stream > 0 && stream < IPPORT_RESERVED)
    {
        xlcerror
        (
            "socket-connect: stream out of range",
            arg_stream,
            s_socket_error
        );
    }
    #endif

    struct sockaddr_in sin;
    struct hostent *hp;
    if ((hp = gethostbyname(name)) != NULL)
    {
        memcpy(&sin.sin_addr.s_addr, hp->h_addr, hp->h_length);
    }
    else
    {
        xlcerror("socket-connect: unknown host", arg_name, s_socket_error);
    }

    sin.sin_family = AF_INET;
    sin.sin_port = (stream <= 0) ? 0 : htons(stream);

    if (connect(s, (struct sockaddr *)&sin, sizeof sin) < 0)
    {
        xlsockerror("socket-connect", cons(arg_name, arg_stream));
    }

    return TRUEVALUE;
}

// (socket-bind fd stream)
euxlValue socket_bind()
{
    static char *cfn_name = "socket-bind";

    euxlValue arg_s = xlgafixnum();
    int s = (int)getfixnum(arg_s);

    euxlValue arg_stream = xlgafixnum();
    int stream = (int)getfixnum(arg_stream);

    xllastarg();

    if (stream > 0 && stream < IPPORT_RESERVED)
    {
        xlcerror
        (
            "socket-bind: stream out of range",
            arg_stream,
            s_socket_error
        );
    }

    struct sockaddr_in sin;
    sin.sin_family = AF_INET;
    sin.sin_addr.s_addr = htonl(INADDR_ANY);
    sin.sin_port = (stream <= 0) ? 0 : htons(stream);

    if (bind(s, (struct sockaddr *)&sin, sizeof sin) < 0)
    {
        xlsockerror(cfn_name, arg_stream);
    }

    return TRUEVALUE;
}

// (socket-listen fd backlog)
euxlValue socket_listen()
{
    static char *cfn_name = "socket-listen";

    euxlValue arg_s = xlgafixnum();
    int s = (int)getfixnum(arg_s);

    euxlValue arg_backlog = xlgafixnum();
    int backlog = (int)getfixnum(arg_backlog);

    xllastarg();

    if (listen(s, backlog) < 0)
    {
        xlsockerror(cfn_name, arg_s);
    }

    return TRUEVALUE;
}

// (socket-accept fd)
euxlValue socket_accept()
{
    static char *cfn_name = "socket-accept";

    struct sockaddr_in sin;
    socklen_t len = sizeof sin;

    euxlValue arg_s = xlgafixnum();
    int s = (int)getfixnum(arg_s);

    xllastarg();

    int new_socket;
    if ((new_socket = accept(s, (struct sockaddr *)&sin, &len)) < 0)
    {
        xlsockerror(cfn_name, arg_s);
    }

    return (cvfixnum((FIXTYPE) new_socket));
}

// (socket-block fd)
euxlValue socket_block()
{
    static char *cfn_name = "socket-block";

    euxlValue arg_s = xlgafixnum();
    int s = (int)getfixnum(arg_s);

    xllastarg();

    int fileflags;
    if ((fileflags = fcntl(s, F_GETFL)) < 0)
    {
        xlsockerror(cfn_name, arg_s);
    }

    if (fcntl(s, F_SETFL, fileflags & ~O_NDELAY) < 0)
    {
        xlsockerror(cfn_name, arg_s);
    }

    return TRUEVALUE;
}

// (socket-nonblock fd)
euxlValue socket_nonblock()
{
    static char *cfn_name = "socket-nonblock";

    euxlValue arg_s = xlgafixnum();
    int s = (int)getfixnum(arg_s);

    xllastarg();

    int fileflags;
    if ((fileflags = fcntl(s, F_GETFL)) < 0)
    {
        xlsockerror(cfn_name, arg_s);
    }

    if (fcntl(s, F_SETFL, fileflags | O_NDELAY) < 0)
    {
        xlsockerror(cfn_name, arg_s);
    }

    return TRUEVALUE;
}

// (socket-reuse fd)
euxlValue socket_reuse()
{
    static char *cfn_name = "socket-reuse";

    euxlValue arg_s = xlgafixnum();
    int s = (int)getfixnum(arg_s);

    xllastarg();

    int val = 1;
    setsockopt(s, SOL_SOCKET, SO_REUSEADDR, (char *)&val, sizeof val);

    return TRUEVALUE;
}

// (socket-noreuse fd)
euxlValue socket_noreuse()
{
    static char *cfn_name = "socket-noreuse";

    euxlValue arg_s = xlgafixnum();
    int s = (int)getfixnum(arg_s);

    xllastarg();

    int val = 0;
    setsockopt(s, SOL_SOCKET, SO_REUSEADDR, (char *)&val, sizeof val);

    return TRUEVALUE;
}

// (socket-close fd)
euxlValue socket_close()
{
    static char *cfn_name = "socket-close";

    euxlValue arg_s = xlgafixnum();
    int s = (int)getfixnum(arg_s);

    xllastarg();

    socket_close(s);

    return TRUEVALUE;
}

// (socket-shutdown fd how)
euxlValue socket_shutdown()
{
    static char *cfn_name = "socket-shutdown";

    euxlValue arg_s = xlgafixnum();
    int s = (int)getfixnum(arg_s);

    euxlValue arg_how = xlgafixnum();
    int how = (int)getfixnum(arg_how);

    xllastarg();

    if (shutdown(s, how) < 0)
    {
        xlsockerror(cfn_name, arg_s);
    }

    return TRUEVALUE;
}

// (socket-peeraddr fd)
euxlValue socket_peeraddr()
{
    static char *cfn_name = "socket-peeraddr";

    struct sockaddr_in peer_name;
    socklen_t peer_namelen = sizeof peer_name;
    struct hostent *hp;

    euxlValue arg_s = xlgafixnum();
    int s = (int)getfixnum(arg_s);

    xllastarg();

    if (getpeername(s, (struct sockaddr *)&peer_name, &peer_namelen) < 0)
    {
        xlsockerror(cfn_name, arg_s);
    }

    if
    (
        (hp =
        gethostbyaddr
        (
            (char *)&peer_name.sin_addr.s_addr,
            sizeof peer_name.sin_addr.s_addr,
            AF_INET
        )
        ) == NULL
    )
    {
        xlcerror
        (
            "socket-peeraddr: unknown address",
            cvfixnum((FIXTYPE) peer_name.sin_addr.s_addr),
            s_socket_error
        );
    }

    return (cvstring(hp->h_name));
}

// (socket-peerstream fd)
euxlValue socket_peerstream()
{
    static char *cfn_name = "socket-peerstream";

    euxlValue arg_s = xlgafixnum();
    int s = (int)getfixnum(arg_s);

    xllastarg();

    struct sockaddr_in peer_name;
    socklen_t peer_namelen = sizeof peer_name;
    if (getpeername(s, (struct sockaddr *)&peer_name, &peer_namelen) < 0)
    {
        xlsockerror(cfn_name, arg_s);
    }

    return (cvfixnum((FIXTYPE) ntohs(peer_name.sin_port)));
}

// (socket-sockaddr fd)
euxlValue socket_sockaddr()
{
    static char *cfn_name = "socket-sockaddr";

    euxlValue arg_s = xlgafixnum();
    int s = (int)getfixnum(arg_s);

    xllastarg();

    struct sockaddr_in sock_name;
    socklen_t sock_namelen = sizeof sock_name;
    if (getsockname(s, (struct sockaddr *)&sock_name, &sock_namelen) < 0)
    {
        xlsockerror(cfn_name, arg_s);
    }

    struct hostent *hp;
    if
    (
        (hp = gethostbyaddr
        (
            (char *)&sock_name.sin_addr.s_addr,
            sizeof sock_name.sin_addr.s_addr,
            AF_INET
        )
        ) == NULL
    )
    {
        xlcerror
        (
            "socket-sockaddr: unknown address",
            cvfixnum((FIXTYPE) sock_name.sin_addr.s_addr),
            s_socket_error
        );
    }

    return (cvstring(hp->h_name));
}

// (socket-sockstream fd)
euxlValue socket_sockstream()
{
    static char *cfn_name = "socket-sockstream";

    euxlValue arg_s = xlgafixnum();
    int s = (int)getfixnum(arg_s);

    xllastarg();

    struct sockaddr_in sock_name;
    socklen_t sock_namelen = sizeof sock_name;
    if (getsockname(s, (struct sockaddr *)&sock_name, &sock_namelen) < 0)
    {
        xlsockerror(cfn_name, arg_s);
    }

    return (cvfixnum((FIXTYPE) ntohs(sock_name.sin_port)));
}

// (socket-host-to-ip name)
euxlValue socket_host_to_ip()
{
    static char *cfn_name = "socket-host-to-ip";

    euxlValue arg_name = xlgastring();
    char *name = (char *)getstring(arg_name);

    xllastarg();

    struct hostent *hp;
    if ((hp = gethostbyname(name)) == NULL)
    {
        xlcerror("host->ip: unknown host", arg_name, s_socket_error);
    }

    struct in_addr in;
    memcpy(&in.s_addr, hp->h_addr, hp->h_length);

    return (cvstring(inet_ntoa(in)));
}

// (socket-ip-to-host ip)
euxlValue socket_ip_to_host()
{
    static char *cfn_name = "socket-ip-to-host";

    euxlValue arg_ip = xlgastring();
    char *ip = (char *)getstring(arg_ip);

    xllastarg();

    unsigned long inet;
    if ((inet = inet_addr(ip)) < 0)
    {
        xlcerror("ip->host: cannot convert IP address", arg_ip, s_socket_error);
    }

    struct hostent *hp;
    if ((hp = gethostbyaddr((char *)&inet, sizeof inet, AF_INET)) == NULL)
    {
        xlcerror("ip->host: unknown ip address", arg_ip, s_socket_error);
    }

    return (cvstring(hp->h_name));
}

// convert to stream to get I/O functions for free

// (socket-convert-to-stream fd)
euxlValue socket_convert_to_stream()
{
    static char *cfn_name = "socket-convert-to-stream";

    euxlValue arg_s = xlgafixnum();
    int s = (int)getfixnum(arg_s);

    xllastarg();

    FILE *handle;
    if ((handle = fdopen(s, "a+")) == NULL)
    {
        xlsockerror(cfn_name, arg_s);
    }

    // make socket unbuffered so we don't lose so much data
    setvbuf(handle, NULL, _IONBF, 0);

    return cvstream(handle, PF_INPUT | PF_OUTPUT);
}

// (stream-fd stream)
euxlValue stream_fd()
{
    static char *cfn_name = "stream-fd";

    euxlValue arg_handle = xlgastream();
    FILE *handle = getfile(arg_handle);

    xllastarg();

    return (cvfixnum((FIXTYPE) fileno(handle)));
}

// buffering stuff loses data on most OSes
#ifdef __linux
// (stream-unbuffered stream)
euxlValue stream_unbuffered()
{
    static char *cfn_name = "stream-unbuffered";

    euxlValue arg_handle = xlgastream();
    FILE *handle = getfile(arg_handle);

    xllastarg();

    fflush(handle);
    if (setvbuf(handle, NULL, _IONBF, 0) < 0)
    {
        xlsockerror(cfn_name, arg_handle);
    }

    return TRUEVALUE;
}

// (stream-block-buffered stream . bufsize)
euxlValue stream_block_buffered()
{
    static char *cfn_name = "stream-block-buffered";

    euxlValue arg_handle = xlgastream();
    FILE *handle = getfile(arg_handle);

    int size;
    if (moreargs())
    {
        euxlValue arg_size = xlgafixnum();
        size = getfixnum(arg_size);
        xllastarg();
    }
    else
    {
        size = BUFSIZ;
    }

    fflush(handle);
    if (setvbuf(handle, NULL, _IOFBF, size) < 0)
    {
        xlsockerror(cfn_name, arg_handle);
    }

    return TRUEVALUE;
}

// (stream-line-buffered stream)
euxlValue stream_line_buffered()
{
    static char *cfn_name = "stream-line-buffered";

    euxlValue arg_handle = xlgastream();
    FILE *handle = getfile(arg_handle);

    xllastarg();

    fflush(handle);
    if (setvbuf(handle, NULL, _IOLBF, 0) < 0)
    {
        xlsockerror(cfn_name, arg_handle);
    }

    return TRUEVALUE;
}
#else
// (stream-unbuffered stream)
euxlValue stream_unbuffered()
{
    static char *cfn_name = "stream-unbuffered";

    euxlValue arg_handle = xlgastream();
    FILE *handle = getfile(arg_handle);

    xllastarg();

    // do nothing

    return TRUEVALUE;
}

// (stream-block-buffered stream . bufsize)
euxlValue stream_block_buffered()
{
    static char *cfn_name = "stream-block-buffered";

    euxlValue arg_handle = xlgastream();
    FILE *handle = getfile(arg_handle);

    int size;
    if (moreargs())
    {
        euxlValue arg_size = xlgafixnum();
        size = getfixnum(arg_size);
        xllastarg();
    }
    else
    {
        size = BUFSIZ;
    }

    // do nothing

    return TRUEVALUE;
}

// (stream-line-buffered stream)
euxlValue stream_line_buffered()
{
    static char *cfn_name = "stream-line-buffered";

    euxlValue arg_handle = xlgastream();
    FILE *handle = getfile(arg_handle);

    xllastarg();

    // do nothing

    return TRUEVALUE;
}
#endif // no more buffering

// select functions

static fd_set readfds, writefds;

// (socket-fd-zero-read)
euxlValue socket_fd_zero_read()
{
    static char *cfn_name = "socket-fd-zero-read";

    xllastarg();
    FD_ZERO(&readfds);
    return TRUEVALUE;
}

// (socket-fd-set-read fd)
euxlValue socket_fd_set_read()
{
    static char *cfn_name = "socket-fd-set-read";

    euxlValue arg_fd = xlgafixnum();
    int fd = (int)getfixnum(arg_fd);

    xllastarg();

    FD_SET(fd, &readfds);
    return TRUEVALUE;

}

// (socket-fd-isset-read fd)
euxlValue socket_fd_isset_read()
{
    static char *cfn_name = "socket-fd-isset-read";

    euxlValue arg_fd = xlgafixnum();
    int fd = (int)getfixnum(arg_fd);

    xllastarg();

    return FD_ISSET(fd, &readfds) ? TRUEVALUE : FALSEVALUE;
}

// (socket-select-read timeout)
euxlValue socket_select_read()
{
    static char *cfn_name = "socket-select-read";

    euxlValue arg_time = xlgafixnum();
    int time = (int)getfixnum(arg_time);

    xllastarg();

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
            xlsockerror(cfn_name, arg_time);
            break;
        case 0:
            return FALSEVALUE;
        default:
            return TRUEVALUE;
    }

    return NIL; // not reached
}

// (socket-fd-zero-write)
euxlValue socket_fd_zero_write()
{
    static char *cfn_name = "socket-fd-zero-write";

    xllastarg();
    FD_ZERO(&writefds);
    return TRUEVALUE;
}

// (socket-fd-set-write fd)
euxlValue socket_fd_set_write()
{
    static char *cfn_name = "socket-fd-set-write";

    euxlValue arg_fd = xlgafixnum();
    int fd = (int)getfixnum(arg_fd);

    xllastarg();

    FD_SET(fd, &writefds);
    return TRUEVALUE;
}

// (socket-fd-isset-write fd)
euxlValue socket_fd_isset_write()
{
    static char *cfn_name = "socket-fd-isset-write";

    euxlValue arg_fd = xlgafixnum();
    int fd = (int)getfixnum(arg_fd);

    xllastarg();

    return FD_ISSET(fd, &writefds) ? TRUEVALUE : FALSEVALUE;
}

// (socket-select-write timeout)
euxlValue socket_select_write()
{
    static char *cfn_name = "socket-select-write";

    euxlValue arg_time = xlgafixnum();
    int time = (int)getfixnum(arg_time);

    xllastarg();

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
            xlsockerror(cfn_name, arg_time);
            break;
        case 0:
            return FALSEVALUE;
        default:
            return TRUEVALUE;
    }

    return NIL; // not reached
}

// utility wrappers for XDR I/O

bool_t xdr_send_int(FILE *handle, int i)
{
    // following may return 'bad fseek' but seems to fix interleaving reads and
    // writes
    fseek(handle, 0L, SEEK_CUR);
    errno = 0;
    XDR xdrs;
    xdrstdio_create(&xdrs, handle, XDR_ENCODE);
    bool_t retval = xdr_int(&xdrs, &i);
    fflush(handle);
    xdr_destroy(&xdrs);
    return retval;
}

bool_t xdr_recv_int(FILE *handle, int *ip)
{
    fseek(handle, 0L, SEEK_CUR);
    errno = 0;
    XDR xdrs;
    xdrstdio_create(&xdrs, handle, XDR_DECODE);
    bool_t retval;
    HANDLE(retval = xdr_int(&xdrs, ip));
    fflush(handle);
    xdr_destroy(&xdrs);
    return retval;
}

bool_t xdr_send_double(FILE *handle, double i)
{
    fseek(handle, 0L, SEEK_CUR);
    errno = 0;
    XDR xdrs;
    xdrstdio_create(&xdrs, handle, XDR_ENCODE);
    bool_t retval = xdr_double(&xdrs, &i);
    fflush(handle);
    xdr_destroy(&xdrs);
    return retval;
}

bool_t xdr_recv_double(FILE *handle, double *ip)
{
    fseek(handle, 0L, SEEK_CUR);
    errno = 0;
    XDR xdrs;
    xdrstdio_create(&xdrs, handle, XDR_DECODE);
    bool_t retval;
    HANDLE(retval = xdr_double(&xdrs, ip));
    fflush(handle);
    xdr_destroy(&xdrs);
    return retval;
}

bool_t xdr_send_string(FILE *handle, char *i, int len)
{
    fseek(handle, 0L, SEEK_CUR);
    errno = 0;
    XDR xdrs;
    xdrstdio_create(&xdrs, handle, XDR_ENCODE);
    bool_t retval = xdr_string(&xdrs, &i, len);
    fflush(handle);
    xdr_destroy(&xdrs);
    return retval;
}

bool_t xdr_recv_string(FILE *handle, char **ip, int len)
{
    fseek(handle, 0L, SEEK_CUR);
    errno = 0;
    XDR xdrs;
    xdrstdio_create(&xdrs, handle, XDR_DECODE);
    bool_t retval;
    HANDLE(retval = xdr_string(&xdrs, ip, len));
    fflush(handle);
    xdr_destroy(&xdrs);
    return retval;
}

// entry functions for XDR I/O to and from streams

// (stream-xdr-send-int stream val)
euxlValue stream_xdr_send_int()
{
    static char *cfn_name = "stream-xdr-send-int";

    euxlValue arg_handle = xlgaostream();
    FILE *handle = getfile(arg_handle);

    euxlValue arg_i = xlgafixnum();
    int i = (int)getfixnum(arg_i);

    xllastarg();

    if (xdr_send_int(handle, i) == FALSE)
    {
        xlsockerror(cfn_name, cons(arg_handle, arg_i));
    }

    return TRUEVALUE;
}

// (stream-xdr-recv-int stream)
euxlValue stream_xdr_recv_int()
{
    static char *cfn_name = "stream-xdr-recv-int";

    euxlValue arg_handle = xlgaistream();
    FILE *handle = getfile(arg_handle);

    xllastarg();

    int i;
    if (xdr_recv_int(handle, &i) == FALSE)
    {
        xlsockerror(cfn_name, arg_handle);
    }

    return (cvfixnum((FIXTYPE) i));
}

// (stream-xdr-send-float stream val)
euxlValue stream_xdr_send_float()
{
    static char *cfn_name = "stream-xdr-send-float";

    euxlValue arg_handle = xlgaostream();
    FILE *handle = getfile(arg_handle);

    euxlValue arg_i = xlganumber();

    double i;
    if (fixp(arg_i))
    {
        i = (double)getfixnum(arg_i);
    }
    else
    {
        i = (double)getflonum(arg_i);
    }

    xllastarg();

    if (xdr_send_double(handle, i) == FALSE)
    {
        xlsockerror(cfn_name, cons(arg_handle, arg_i));
    }

    return TRUEVALUE;
}

// (stream-xdr-recv-float stream)
euxlValue stream_xdr_recv_float()
{
    static char *cfn_name = "stream-xdr-recv-float";

    euxlValue arg_handle = xlgaistream();
    FILE *handle = getfile(arg_handle);

    xllastarg();

    double i;
    if (xdr_recv_double(handle, &i) == FALSE)
    {
        xlsockerror(cfn_name, arg_handle);
    }

    return (cvflonum((FLOTYPE) i));
}

// (stream-xdr-send-string stream val)
euxlValue stream_xdr_send_string()
{
    static char *cfn_name = "stream-xdr-send-string";

    euxlValue arg_handle = xlgaostream();
    FILE *handle = getfile(arg_handle);

    euxlValue arg_string = xlgastring();
    char *string = (char *)getstring(arg_string);

    xllastarg();

    int i = getslength(arg_string);

    if (xdr_send_int(handle, i) == FALSE)
    {
        xlsockerror(cfn_name, cons(arg_handle, arg_string));
    }

    if (xdr_send_string(handle, string, i) == FALSE)
    {
        xlsockerror(cfn_name, cons(arg_handle, arg_string));
    }

    return TRUEVALUE;
}

// (stream-xdr-recv-string stream)
euxlValue stream_xdr_recv_string()
{
    static char *cfn_name = "stream-xdr-recv-string";

    euxlValue arg_handle = xlgaistream();
    FILE *handle = getfile(arg_handle);

    xllastarg();

    int i;
    if (xdr_recv_int(handle, &i) == FALSE)
    {
        xlsockerror(cfn_name, arg_handle);
    }

    char *buffer;
    if ((buffer = (char *)malloc(i*sizeof(char))) == NULL)
    {
        xlcerror
        (
            "stream-xdr-recv-string: failed to allocate memory",
            arg_handle,
            s_socket_error
        );
    }

    if (xdr_recv_string(handle, &buffer, i) == FALSE)
    {
        xlsockerror(cfn_name, arg_handle);
    }

    buffer[i - 1] = '\0';

    euxlValue temp = cvstring2(buffer, i - 1);
    free(buffer);

    return temp;
}

#else
// some linkers complain of empty files
void no_sockets()
{
}
#endif


///-----------------------------------------------------------------------------
