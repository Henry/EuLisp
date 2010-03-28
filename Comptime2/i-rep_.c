
/** Copyright (c) 1997 by A Kind & University of Bath. All rights reserved. **/

/** ----------------------------------------------------------------------- **
 **                     EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Description: C hook file of EuLisp module i-rep
 ** ----------------------------------------------------------------------- **/

#include <eulisp.h>


/* Initialize module i-rep ... */
extern void initialize_module_i_rep();
extern LispRef i_rep_bindings[];

/* Run application i-rep ... */
void run_application()
{
  initialize_module_i_rep();
  execute_lambda(i_rep_bindings[0]);
}


/* eof */
