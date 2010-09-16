/// Copyright 1994-2010 Fraunhofer ISST
/// Copyright 2010 Henry G. Weller
///-----------------------------------------------------------------------------
//  This file is part of
/// ---                           EuLisp System 'Eu2C'
///-----------------------------------------------------------------------------
//
//  Eu2C is free software: you can redistribute it and/or modify it under the
//  terms of the GNU General Public License version 2 as published by the Free
//  Software Foundation.
//
//  Eu2C is distributed in the hope that it will be useful, but WITHOUT ANY
//  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
//  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
//  details.
//
//  You should have received a copy of the GNU General Public License along with
//  this program.  If not, see <http://www.gnu.org/licenses/>.
//
///-----------------------------------------------------------------------------
/// Title: Simple timer support
///  Library: Runtime
///  Authors: Jens Bimberg
///  Description:
//    start_timer locally saves the current time
//    timer prints the used time since start_timer using a format string
//    of the form "used time: %.2f user + %.2f system = %.2f full\n"
//    since there are no different timers on ms-dos, the elapsed time
//    is given as user time and the system time is given as 0.0
//    furthermore timer calls xalloc_info to print garbage collector
///-----------------------------------------------------------------------------
#include <unistd.h>
#include <sys/times.h>
#include <stdio.h>
#include "xalloc_user.h"

static struct tms tb;
static clock_t startutime, startstime;

long start_timer()
{
    times(&tb);
    startutime = tb.tms_utime;
    startstime = tb.tms_stime;
    return 0;
}

long timer(char *string)
{
    times(&tb);
    int clock_freq = sysconf(_SC_CLK_TCK);
    printf
    (
        string,
        (double)(tb.tms_utime - startutime)/clock_freq,
        (double)(tb.tms_stime - startstime)/clock_freq,
        (double)(tb.tms_utime - startutime + tb.tms_stime - startstime)/clock_freq
    );
    xalloc_info();
    return 0;
}


///-----------------------------------------------------------------------------
