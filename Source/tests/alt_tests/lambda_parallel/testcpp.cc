/*********************************************************************
 *
 * Copyright -- see file named COPYRIGHTNOTICE
 *
 *********************************************************************/
#include "stdio.h"
#include "mpi.h"

main(int argc, char **argv) {

  int ierror, mype;

  MPI_Init ( &argc, &argv );
  MPI_Comm_rank ( MPI_COMM_WORLD, &mype );
  printf (" Hello World, I am a C++ code on processor %i\n", mype);
  MPI_Finalize ();
  /* Ends the program's use of the message passing functions */
}
