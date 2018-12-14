#ifndef PTEST_HH_INCLUDE
#define PTEST_HH_INCLUDE

// ***************************************************************************
// ***************************************************************************
// This class does a parallel test of eospac 6.
// ***************************************************************************
// ***************************************************************************
/*********************************************************************
 *
 * Copyright -- see file named COPYRIGHTNOTICE
 *
 *********************************************************************/

namespace Ptest_ns
{

class Ptest
{
public:
    // Constructors.
    Ptest() {};
    Ptest(Comm_ns::Comm &comm_cmpt);

    // Actually do the test.
    void do_test();

private:

    // Pointer to the processor to processor communication component.
    Comm_ns::Comm *comm;

    // Processor data.
    // The Comm object is used to set these.
    int npes;  // Number of processors.
    int mype;  // The processor index.
    int iope;  // The processor index that does the io.

    // Which problem are we going to test.
    int which_problem;

    // A debug flag.
    bool debug;

    // The number of tables that are going to be loaded.
    int ntables;

    // Array of the table handles. This comes from eos_CreateTables().
    std::vector<int> table_handles;

    // Arrays for doing the interpolation.
    std::vector<double> X, Y, F, dFx, dFy, Fexp;

    // Array for storing the packed tables. This is used for sending
    // the tables from the io processor (iope) to the other processors.
    std::vector<char> packed_tables;

    // Debug print.
    void debug_print(std::string label);
    void debug_print(std::string label, int value);

    // Do the interpolation to get the desired results.
    void interpolate_tables();

    // Pack the tables.
    int pack_tables();

    // Print error.
    void print_error(int ecode, std::string label);

    // Print the interpolation results to the screen.
    void print_interpolation_results();

    // Print table errors.
    bool check_table_errors(int error_code, std::string label);

    // Setup the tables.
    int setup_tables();

    // Unpack the tables.
    int unpack_tables();
};

} // End of the namespace for this component.

#endif
