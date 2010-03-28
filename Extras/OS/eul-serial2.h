/** Copyright (c) 1997 by A Kind & University of Bath. All rights reserved. **/

/** ----------------------------------------------------------------------- **
 **                     EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Library: serial
 **  Authos: Andreas Kind
 **  Description: Foreign functions for marshaling closures
 ** ----------------------------------------------------------------------- **/

#ifndef EUL_SERIAL2_H
#define EUL_SERIAL2_H

extern LispRef eul_lambda_refs(LispRef, LispRef);
extern LispRef eul_bytevector_refs(LispRef, LispRef);
extern LispRef eul_link_lambda_refs(LispRef, LispRef);
extern LispRef eul_link_bytevector_refs(LispRef, LispRef);

#define WITH_DEBUG(x) x


/** ----------------------------------------------------------------------- **/

#endif // EUL_SERIAL2_H

/** ----------------------------------------------------------------------- **/
