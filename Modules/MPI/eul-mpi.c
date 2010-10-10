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
#include <mpi.h>
#include <eulisp.h>

///-----------------------------------------------------------------------------
/// MPI_CHAR is misused to denote string objects
/// MPI_LONG_LONG_INT is misused to denote serialized objects
///-----------------------------------------------------------------------------

#define MPI_EUL_STRING MPI_CHAR
#define MPI_EUL_OBJECT MPI_LONG_LONG_INT

///-----------------------------------------------------------------------------
/// MPI initialization
///-----------------------------------------------------------------------------

LispRef eul_mpi_initialize()
{
    int i, argc, process_id, nprocs, namelen;

    static char **argv;

    LispRef info, eul_processor_name;

    char processor_name[MPI_MAX_PROCESSOR_NAME];

    char *name;

    /* Get argc and argv */
    argc = eul_int_as_c_int(eul_argc);
    argv = (char **)gc_malloc(argc * sizeof(LispRef));

    for (i = 0; i < argc; i++)
        argv[i] = eul_string_as_c_string(slot_ref(eul_argv, i));

    MPI_Init(&argc, &argv);

    /* Get process id and number of processes */
    MPI_Comm_size(MPI_COMM_WORLD, &nprocs);
    MPI_Comm_rank(MPI_COMM_WORLD, &process_id);
    MPI_Get_processor_name(processor_name, &namelen);
    name = (char *)gc_malloc(namelen + 1);
    strncpy(name, processor_name, namelen + 1);
    eul_intern_symbol(eul_processor_name, name);

    /* Return id, nprocs and host name */
    eul_allocate_vector(info, 3, eul_nil);
    slot_ref(info, 0) = c_int_as_eul_int(process_id);
    slot_ref(info, 1) = c_int_as_eul_int(nprocs);
    slot_ref(info, 2) = eul_processor_name;

/*   printf("Initialize process %d with %d processes ...\n", */

/*       process_id, nprocs); */

/*   fflush(stdout); */

    return info;
}

///-----------------------------------------------------------------------------
/// MPI Send
///-----------------------------------------------------------------------------

LispRef eul_mpi_send(LispRef x, int dest, LispRef eul_tag)
{
    int n, tag, flag;

    void *data;

    MPI_Datatype datatype;

    if (eul_is_string(x))
    {
        char *val;

        datatype = MPI_BYTE;
        n = eul_int_as_c_int(eul_string_size(x));
        val = eul_string_as_c_string(x);
        data = (void *)val;
        tag = (int)(eul_tag == eul_nil ? MPI_EUL_STRING : MPI_EUL_OBJECT);
    }
    else if (eul_is_int(x))
    {
        int val;

        datatype = MPI_INT;
        n = 1;
        val = eul_int_as_c_int(x);
        data = (void *)&val;
        tag = (int)MPI_INT;
    }
    else if (eul_is_double(x))
    {
        double val;

        datatype = MPI_DOUBLE;
        n = 1;
        val = eul_double_as_c_double(x);
        data = (void *)&val;
        tag = (int)MPI_DOUBLE;
    }
    else if (eul_is_char(x))
    {
        char val;

        datatype = MPI_BYTE;
        n = 1;
        val = eul_char_as_c_char(x);
        data = (void *)&val;
        tag = (int)MPI_BYTE;
    }
    else
        /* ERROR */
        return eul_nil;

    flag = MPI_Send(data, n, datatype, dest, tag, MPI_COMM_WORLD);
    if (flag == 0)
        return eul_true;
    else
        /* ERROR */
        return eul_nil;
}

///-----------------------------------------------------------------------------
/// MPI Probe
/// Returns nil or a vector #(is_obj datatype_tag n)
///-----------------------------------------------------------------------------

LispRef eul_mpi_probe(LispRef eul_source, LispRef eul_tag, LispRef eul_timeout)
{
    MPI_Status status;

    MPI_Datatype datatype;

    int source, tag;
    int flag = 1, n;

    LispRef res = eul_nil;

    if (eul_source == eul_nil)
        source = MPI_ANY_SOURCE;
    else
        source = eul_int_as_c_int(eul_source);

    if (eul_tag == eul_nil)
        tag = MPI_ANY_TAG;
    else
        tag = eul_int_as_c_int(eul_tag);

    if (eul_timeout == eul_nil)
        MPI_Probe(source, tag, MPI_COMM_WORLD, &status);
    else
        MPI_Iprobe(source, tag, MPI_COMM_WORLD, &flag, &status);

    if ((status.MPI_ERROR) == 0 && flag)
    {
        LispRef is_obj = eul_nil;

        tag = status.MPI_TAG;
        switch (tag)
        {
            case ((int)MPI_EUL_OBJECT):
                is_obj = eul_true;
            case ((tnt)MPI_EUL_STRING):
                datatype = MPI_BYTE;
                break;
            default:
                datatype = (MPI_Datatype) tag;
        }
        flag = MPI_Get_count(&status, datatype, &n);
        if (flag == 0)
        {
            eul_allocate_vector(res, 3, eul_nil);
            slot_ref(res, 0, is_obj);
            slot_ref(res, 1, c_int_as_eul_int(tag));
            slot_ref(res, 2, c_int_as_eul_int(n));
        }
    }
    return res;
}

///-----------------------------------------------------------------------------
/// MPI Recieve
///-----------------------------------------------------------------------------

#define BUFLEN 512

LispRef eul_mpi_receive(LispRef eul_source, LispRef eul_tag, int n)
{
    MPI_Status status;

    int source, tag;

    LispRef res = eul_nil;

    if (eul_source == eul_nil)
        source = MPI_ANY_SOURCE;
    else
        source = eul_int_as_c_int(eul_source);

    if (eul_tag == eul_nil)
        tag = (int)MPI_ANY_TAG;
    else if (eul_tag == eul_true)
        tag = (int)MPI_EUL_OBJECT;
    else
        tag = eul_int_as_c_int(eul_tag);

    switch (tag)
    {
        case ((int)MPI_BYTE):
            {
                char val;

                fprintf(stderr, "Before 1\n");
                MPI_Recv(&val, n, MPI_BYTE, source, tag, MPI_COMM_WORLD,
                    &status);
                if ((status.MPI_ERROR) == 0)
                    res = c_char_as_eul_char(val);
                break;
            }
        case ((int)MPI_INT):
            {
                int val;

                fprintf(stderr, "Before 2 n: %i\n", n);
                MPI_Recv(&val, n, MPI_INT, source, tag, MPI_COMM_WORLD,
                    &status);
                if ((status.MPI_ERROR) == 0)
                    res = c_int_as_eul_int(val);
                break;
            }
        case ((int)MPI_DOUBLE):
            {
                double val;

                fprintf(stderr, "Before 3\n");
                MPI_Recv(&val, n, MPI_DOUBLE, source, tag, MPI_COMM_WORLD,
                    &status);
                if ((status.MPI_ERROR) == 0)
                    eul_allocate_double(res, val);
                break;
            }
        case ((int)MPI_EUL_STRING):
        case ((int)MPI_EUL_OBJECT):
            {
                char *val;

                fprintf(stderr, "Before 4\n");
                val = (char *)gc_malloc(n + 1);
                MPI_Recv(val, n, MPI_BYTE, source, tag, MPI_COMM_WORLD,
                    &status);
                if ((status.MPI_ERROR) == 0)
                {
                    *(val + n) = '\0';
                    eul_allocate_nstring(res, val, n);
                }
                break;
            }
    }

    return res;
}

///-----------------------------------------------------------------------------
