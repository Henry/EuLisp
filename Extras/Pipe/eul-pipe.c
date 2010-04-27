/// Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
///-----------------------------------------------------------------------------
/// ---                 EuLisp System 'youtoo'
///-----------------------------------------------------------------------------
///  Library: pipe
///  Authors: Rob Simmons, Andreas Kind
///  Description: fork child process and control i/o with a stream object
///-----------------------------------------------------------------------------

#include "eulisp.h"
#include <signal.h>

///-----------------------------------------------------------------------------
/// Error codes
///-----------------------------------------------------------------------------

#define EUL_PIPE_ERROR (-1)
#define EUL_PIPE_STRDUP_FAILED (-2)
#define EUL_PIPE_BAD_PROCESS_NAME (-3)
#define EUL_PIPE_BAD_STDIN_STDOUT (-4)
#define EUL_PIPE_FORK_FAILED (-5)
#define EUL_PIPE_DUP2_FAILED (-6)
#define EUL_PIPE_BAD_CHILD (-7)

///-----------------------------------------------------------------------------
/// Pipe related error messages
///-----------------------------------------------------------------------------

char *eul_pipe_strerror(int error_code)
{
    switch (error_code)
    {
        case EUL_PIPE_STRDUP_FAILED:
            return "strdup failed";
        case EUL_PIPE_BAD_PROCESS_NAME:
            return "bad process name";
        case EUL_PIPE_BAD_STDIN_STDOUT:
            return "cannot open stdin/stdout";
        case EUL_PIPE_FORK_FAILED:
            return "fork failed";
        case EUL_PIPE_DUP2_FAILED:
            return "dup2 failed";
        case EUL_PIPE_BAD_CHILD:
            return "cannot start child process";
        default:
            return "no error message available";
    }
}


///-----------------------------------------------------------------------------
/// Start the child process
///-----------------------------------------------------------------------------

int FT_fork_child(LispRef res, char *childname, char **childs_argv, char *path)
{
    int tmp_childs_stdin[2];
    int tmp_childs_stdout[2];

    /* Set up communication pipes */
    if (!((pipe(tmp_childs_stdin) == 0) && (pipe(tmp_childs_stdout) == 0)))
    {
        close(tmp_childs_stdin[0]);
        close(tmp_childs_stdin[1]);
        return (EUL_PIPE_BAD_STDIN_STDOUT);
    }

    int child_id = fork();

    if (child_id < 0)
    {
        return (EUL_PIPE_FORK_FAILED);
    }

    if (child_id == 0)
    {
        // The child process.
        close(tmp_childs_stdin[1]);
        close(tmp_childs_stdout[0]);

        // Connect our pipes to this processes standard streams
        int dup_test = dup2(tmp_childs_stdin[0], 0);        // Standard input
        if (dup_test == -1)
        {
            fprintf
            (
                stderr,
                "*** WARNING [pipe]: %s\n",
                eul_pipe_strerror(EUL_PIPE_DUP2_FAILED)
            );

            exit(EUL_PIPE_DUP2_FAILED);
        }
        dup_test = dup2(tmp_childs_stdout[1], 1);       // Standard output
        if (dup_test == -1)
        {
            fprintf
            (
                stderr,
                "*** WARNING [pipe]: %s\n",
                eul_pipe_strerror(EUL_PIPE_DUP2_FAILED)
            );
            exit(EUL_PIPE_DUP2_FAILED);
        }

        // Next check if any arguments have been provided for the child and set
        // the exec arguments up accordingly

        char **local_argv;

        if (childs_argv == NULL)
        {
            char *null_argv[2];
            null_argv[0] = childname;
            null_argv[1] = NULL;
            local_argv = null_argv;
        }
        else
        {
            local_argv = childs_argv;
        }

        char exec_string[130];

        if (path != NULL)
        {
            int n = strlen(path);
            if (*(path + n - 1) == '/')
            {
                sprintf(exec_string, "%s%s", path, childname);
            }
            else
            {
                sprintf(exec_string, "%s/%s", path, childname);
            }
        }
        else
        {
            // No path provided so lets hope exec can find the binary file
            sprintf(exec_string, childname);
        }

        if (execvp(exec_string, local_argv) < 1)
        {
            return (EUL_PIPE_BAD_CHILD);
        }
    }
    else
    {
        // The parent process
        close(tmp_childs_stdin[0]);
        close(tmp_childs_stdout[1]);

        // Avoid zombie child process in case parent doesn't ask for return
        // code
        signal(SIGCHLD, SIG_IGN);

        // Put childs pid and the two file descriptors in the result vector
        slot_ref(res, 0) = c_int_as_eul_int(((int)child_id));
        slot_ref(res, 1) = c_int_as_eul_int(tmp_childs_stdout[0]);
        slot_ref(res, 2) = c_int_as_eul_int(tmp_childs_stdin[1]);
    }

    return (0);
}


LispRef eul_fork_child(char *arg_string)
{
    // Compute local_argv, argn
    char *string = (char *)strdup(arg_string);
    char *ptr = string;

    if (ptr == NULL)
    {
        return (c_int_as_eul_int(EUL_PIPE_STRDUP_FAILED));
    }

    int local_argc = 0;
    char *local_argv[30];
    while (*ptr != '\0')
    {
        local_argv[local_argc] = ptr;
        while ((*ptr != ' ') && (*ptr != '\0'))
        {
            ++ptr;
        }

        if ((ptr - local_argv[local_argc]) > 0)
        {
            ++local_argc;
        }

        *ptr = '\0';
        ++ptr;
    }

    local_argv[local_argc] = NULL;

    if (local_argc == 0)
    {
        return (c_int_as_eul_int(EUL_PIPE_BAD_PROCESS_NAME));
    }

    LispRef res;
    eul_allocate_vector(res, 3, eul_nil);

    // Fork the child process
    int flag = FT_fork_child(res, local_argv[0], local_argv, NULL);
    if (flag != 0)
    {
        // Error
        return (c_int_as_eul_int(flag));
    }

    free(string);

    return res;
}


///-----------------------------------------------------------------------------
