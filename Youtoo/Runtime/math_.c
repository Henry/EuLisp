/** ----------------------------------------------------------------------- **
 **                 Generated by EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Title: C hook file of EuLisp module math
 **  Copyright: See file math.em
 ** ----------------------------------------------------------------------- **/

#include "eulisp.h"


/* Initialize module math ... */
extern void initialize_module_math();
extern LispRef math_bindings[];

/* Run application math ... */
void run_application()
{
  initialize_module_math();
  execute_lambda(math_bindings[0]);
}


/* eof */