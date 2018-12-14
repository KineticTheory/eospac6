/*********************************************************************
 *
 * Copyright -- see file named COPYRIGHTNOTICE
 *
 *********************************************************************/
#include <iostream>
#include <iomanip>
#include <string>
#include <vector>
#include "Comm.hh"
#include "Ptest.hh"

// ===========================================================================
// This is the driver for running the eospac 6 test code.
// ===========================================================================
main(int argc, char **argv)
{
    // Initialize the processor to processor communication component.
    // This calls MPI_Init(), sets number of processors, etc.
    Comm_ns::Comm comm_cmpt(argc, argv);

    // Do the test.
    Ptest_ns::Ptest ptest(comm_cmpt);
    ptest.do_test();

    // Exit mpi gracefully.
    comm_cmpt.finalize();

    return 0;
}

