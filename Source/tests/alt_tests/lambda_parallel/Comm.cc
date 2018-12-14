// ***************************************************************************
// ***************************************************************************
// A simple processor to processor communications library.
// ***************************************************************************
// ***************************************************************************
/*********************************************************************
 *
 * Copyright -- see file named COPYRIGHTNOTICE
 *
 *********************************************************************/

#include <iostream>
#include <string>
#include <vector>

#include "mpi.h"
#include "Comm.hh"

namespace Comm_ns
{


// ===========================================================================
// Comm class constructor.
// ===========================================================================
Comm::Comm(int &argc, char** &argv)
{
    // Initialize mpi.
    MPI_Init(&argc, &argv);
    //MPI_Errhandler_set(MPI_COMM_WORLD, MPI_ERRORS_RETURN);

    // Set the processor information.
    MPI_Comm_size(MPI_COMM_WORLD, &npes );
    MPI_Comm_rank(MPI_COMM_WORLD, &mype );
    iope = 0;
}


// ===========================================================================
// Barrier
// ===========================================================================
void Comm::barrier()
{
    if (npes == 1) return;

    MPI_Barrier(MPI_COMM_WORLD);
}



// ===========================================================================
// Broadcast an integer from iope to all other processors.
// ===========================================================================
void Comm::broadcast(int &data)
{
    if (npes == 1) return;

    MPI_Bcast(&data, 1, MPI_INT, iope, MPI_COMM_WORLD);
}


// ===========================================================================
// Broadcast a vector of char's from iope to all other processors.
// ===========================================================================
void Comm::broadcast(std::vector<char> &data)
{
    if (npes == 1) return;

    MPI_Bcast(&data[0], (int)data.size(), MPI_CHAR, iope, MPI_COMM_WORLD);
}


// ===========================================================================
// Broadcast an array of char's from iope to all other processors.
// ===========================================================================
void Comm::broadcast(char *data, int ndata)
{
    if (npes == 1) return;

    MPI_Bcast(data, ndata, MPI_CHAR, iope, MPI_COMM_WORLD);
}



// ===========================================================================
// Sum an integer over all processors.
// ===========================================================================
void Comm::sum(int &data)
{
    if (npes == 1) return;

    int data_out;
    MPI_Allreduce(&data, &data_out, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD);
    data = data_out;
}




// ===========================================================================
// Exit the calculation gracefully.
// ===========================================================================
void Comm::finalize()
{
    MPI_Finalize();
}


} // end namespace Comm_ns
