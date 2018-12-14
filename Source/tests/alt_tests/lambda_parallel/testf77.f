c*********************************************************************
c*
c* Copyright -- see file named COPYRIGHTNOTICE
c*
c*********************************************************************
      program test_mpi
      IMPLICIT NONE
      INCLUDE "mpif.h"
      INTEGER ierror, my_pe
      CALL mpi_init ( ierror )
      CALL mpi_comm_rank ( MPI_COMM_WORLD, my_pe, ierror )
      print *, ' Hello World, I am a Fortran 77 code on processor ',
     &         my_pe
      CALL mpi_finalize ( ierror )
                                ! Ends the program's use of the message passing functions
      END
