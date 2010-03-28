/** Copyright (c) 1997 by A Kind & University of Bath. All rights reserved. **/

/** ----------------------------------------------------------------------- **
 **                     EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Library: fsocket
 **  Authos: Andreas Kind
 **  Description: foreign socket connection
 ** ----------------------------------------------------------------------- **/

#include "eul-sock.h"
#include "eulisp.h"
#include <arpa/inet.h>

/** ----------------------------------------------------------------- **
 ** Allocate and bind a socket using TCP or UDP
 ** ----------------------------------------------------------------- **/

int eul_make_socket(char *service, char *protocol, int qlen)
{
    struct servent *pse;

    struct protoent *ppe;

    struct sockaddr_in sin;

    int s, type;

    memset((char *)&sin, 0, sizeof(sin));
    sin.sin_family = AF_INET;
    sin.sin_addr.s_addr = INADDR_ANY;

    /* Map service name to port number */
    if ((pse = getservbyname(service, protocol)))
    {
        sin.sin_port = pse->s_port;
    }
    else if ((sin.sin_port = htons(atoi(service))) == 0)
    {
        return EUL_SOCK_NO_SERVICE;
    }

    /* Map protocol name to protocol number */
    if ((ppe = getprotobyname(protocol)) == 0)
        return EUL_SOCK_NO_PROTOCOL;

    /* Use protocol to choose a socket type */
    if (strcmp(protocol, "udp") == 0)
        type = SOCK_DGRAM;
    else
        type = SOCK_STREAM;

    /* Allocate a socket */
    s = socket(PF_INET, type, ppe->p_proto);
    if (s < 0)
        return EUL_SOCK_ERROR;

    /* Bind the socket */
    if (bind(s, (struct sockaddr *)&sin, sizeof(sin)) < 0)
        return EUL_SOCK_ERROR;
    if (type == SOCK_STREAM && listen(s, qlen) < 0)
        return EUL_SOCK_ERROR;

    return s;
}

/** ----------------------------------------------------------------- **
 ** Accept a socket connection
 ** ----------------------------------------------------------------- **/

int eul_socket_accept(int msock)
{
    unsigned int alen = sizeof(struct sockaddr_in);
    struct sockaddr_in *fsin_ptr = (struct sockaddr_in *)gc_malloc(alen);
    int ssock = accept(msock, (struct sockaddr *)fsin_ptr, &alen);

    if (ssock < 0)
    {
        return EUL_SOCK_ERROR;
    }

    return ssock;
}

/** ----------------------------------------------------------------- **
 ** Allocate and connect a socket connection using TCP or UDP
 ** ----------------------------------------------------------------- **/

int eul_make_connection(char *host, char *service, char *protocol)
{
    struct hostent *phe;

    struct servent *pse;

    struct protoent *ppe;

    struct sockaddr_in sin;

    int s, type;

    memset((char *)&sin, 0, sizeof(sin));
    sin.sin_family = AF_INET;

    /* Map service name to port number */
    if ((pse = getservbyname(service, protocol)))
    {
        sin.sin_port = pse->s_port;
    }
    else if ((sin.sin_port = htons(atoi(service))) == 0)
    {
        return EUL_SOCK_NO_SERVICE;
    }

    /* Map host name to IP address, allowing for dotted decimal */
    if ((phe = gethostbyname(host)))
    {
        memcpy((char *)&sin.sin_addr, phe->h_addr, phe->h_length);
    }
    else if ((sin.sin_addr.s_addr = inet_addr(host)) == INADDR_NONE)
    {
        return EUL_SOCK_NO_HOST;
    }

    /* Map protocol name to protocol number */
    if ((ppe = getprotobyname(protocol)) == 0)
    {
        return EUL_SOCK_NO_PROTOCOL;
    }

    /* Use protocol to choose a socket type */
    if (strcmp(protocol, "udp") == 0)
        type = SOCK_DGRAM;
    else
        type = SOCK_STREAM;

    /* Allocate a socket */
    s = socket(PF_INET, type, ppe->p_proto);
    if (s < 0)
        return EUL_SOCK_ERROR;

    /* Connect the socket */
    if (connect(s, (struct sockaddr *)&sin, sizeof(sin)) < 0)
        return EUL_SOCK_ERROR;

    return s;
}

/** ----------------------------------------------------------------- **
 ** Socket related error messages
 ** ----------------------------------------------------------------- **/

char *eul_socket_strerror(int error_code)
{
    switch (error_code)
    {
        case EUL_SOCK_NO_SERVICE:
            return "port/service unavailable";
        case EUL_SOCK_NO_HOST:
            return "host unavailable";
        case EUL_SOCK_NO_PROTOCOL:
            return "protocol unavailable";
        default:
            return "no error message available";
    }
}

/** ----------------------------------------------------------------- **
 ** Host name
 ** ----------------------------------------------------------------- **/

char *eul_hostname()
{
    struct utsname name;

    int n;

    char *str;

    uname(&name);

    n = strlen(name.nodename);
    str = (char *)gc_malloc(n + 1);
    strcpy(str, name.nodename);
    return str;
}
