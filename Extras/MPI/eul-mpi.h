/** EuLysses header **/

#ifndef EUL_MPI_H
#define EUL_MPI_H

extern LispRef eul_mpi_initialize();

extern int eul_mpi_status_count(LispRef)s;
extern int eul_mpi_status_SOURCE(LispRef);
extern int eul_mpi_status_TAG(LispRef);
extern int eul_mpi_status_ERROR(LispRef);


#endif /* eof */
