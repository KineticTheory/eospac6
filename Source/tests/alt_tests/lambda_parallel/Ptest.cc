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

#include <iostream>
#include <iomanip>
#include <string>
#include <vector>

#include "eos_Interface.h"
#include "Comm.hh"
#include "Ptest.hh"

namespace Ptest_ns
{


// ===========================================================================
// Ptest class constructor
// ===========================================================================
Ptest::Ptest(Comm_ns::Comm &comm_cmpt)
{
    // Set the class pointer to the comm object.
    comm = &comm_cmpt;

    // Set the processor information.
    mype = comm->getProcRank();
    npes = comm->getNumProcs();
    iope = comm->getIOProcRank();
}


// ===========================================================================
// Driver for actually doing the test.
// ===========================================================================
void Ptest::do_test()
{
    // Set the debug flag. This will generate messages to the screen
    // giving various debug information.
    debug = true;

    // Select which problem to test.
    //which_problem = 1;  // matid=9003, pec, extrapolation test
    which_problem = 2;  // matid=2140, pec

    // Setup the tables. This defines the tables, material id's, calls
    // eos_CreateTables() and eos_LoadTables().
    // Note that only the io processor sets up the tables. It will later
    // broadcast the tables to the other processors.
    int ires = 0;
    if (mype == iope) ires = setup_tables();
    comm->sum(ires);
    if (ires > 0) return;
    
    // Pack the tables so they can be sent to the other processors.
    // Table packing is only done on the io processor.
    debug_print("Before pack_tables");
    ires = 0;
    if (mype == iope) ires = pack_tables();
    comm->sum(ires);
    if (ires > 0) return;
    debug_print("After pack_tables");

    // The other processors need to know sizes so they can do memory
    // allocation.
    comm->broadcast( ntables );
    int packed_tsize = 0;
    if (mype == iope) packed_tsize = (int)packed_tables.size();
    comm->broadcast( packed_tsize );
    debug_print("Before broadcast packed_tables, ntables = ", ntables);
    debug_print("Before broadcast packed_tables, packed_tsize = ",
                packed_tsize);

    // All processors, except the io processor, need to allocate memory for
    // the arrays that eospac needs (the io processor has already allocated
    // the necessary memory).
    if (mype != iope) {
        packed_tables.resize(packed_tsize, 'x');
        table_handles.resize(ntables, 0);
    }

    // The io processor broadcasts the packed tables to the other processors.
    comm->broadcast(packed_tables);
    
    // Have the io processor print some debugging information.
    if (debug) {
        if (mype == iope) {
            for (int i=0; i<20; i++) {
                std::cout << "packed_tables[" << i << "] = " <<
                    (int)packed_tables[i] << std::endl;
            }
            for (int i=packed_tsize-21; i<packed_tsize; i++) {
                std::cout << "packed_tables[" << i << "] = " <<
                    (int)packed_tables[i] << std::endl;
            }
            for (int i=0; i<ntables; i++) {
                std::cout << "&&&&& table_handles[" << i << "] = " <<
                    table_handles[i] << std::endl;
            }
        }
    }

    // At this point, all the other processors know what they need, we
    // stuff the tables into eospac. After this operation, all the
    // processors are ready to do interpolation.
    debug_print("Before unpack_tables");
    ires = 0;
    if (mype != iope) ires = unpack_tables();
    comm->sum(ires);
    if (ires > 0) return;
    debug_print("After unpack_tables");

    // Do the interpolation to get the desired results.
    debug_print("Before interpolate_tables");
    interpolate_tables();
    debug_print("After interpolate_tables");

    // Print results.
    print_interpolation_results();
}


// ===========================================================================
// Setup the tables.
//
// This method does the following:
// 1. Sets what tables and materials are desired.
// 2. Creates the tables with eos_CreateTables.
// 3. Loads the tables with eos_LoadTables.
//
// The return value is 0 for success and 1 for failure.
//
// This method is only called from the io processor.
//
// ===========================================================================
int Ptest::setup_tables()
{
    // Array of table types to be loaded. An example of a table type is
    // EOS_Pt_DT (Pressure and internal energy as a function of density
    // and temperature).
    std::vector<int> table_type;
    table_type.push_back(EOS_Info);     // Information (abar, zbar, ...)
    table_type.push_back(EOS_Pt_DT);    // P(RHO, T)
    table_type.push_back(EOS_Pt_DUt);   // P(RHO, I)
    table_type.push_back(EOS_Ut_DT);    // I(RHO, T)
    table_type.push_back(EOS_T_DUt);    // T(RHO, I)
    table_type.push_back(EOS_Uic_DT);   // Ion Energy(RHO, T)
    table_type.push_back(EOS_Ue_DT);    // Electron Energy(RHO, T)
    ntables = (int)table_type.size();

    // Setup the material id for each of the tables.
    // RHO = 0.8 0.9 1.0 1.1 1.2
    // T = 4800 4900 5000 5100 5200
    // Mat 9001: P=3 everywhere, I=4 everywhere
    // Mat 9002: P = 1. + 10.*(RHO - .8), I=P
    // Mat 9003: P = 1. + .01*(T - 4800), I=P
    // Mat 9004: P = .92*RHO**2 + 1.e-11*T**3, I=P
    // Mat 9005: P = ideal gas, gamma=1.4, A=1.00797, I=P
    // Mat 9006: P = ideal gas, gamma=5/3, A=131.3,   I=P
    // Sesame Mat 2140: iron
    std::vector<int> matid;
    int matid1 = 0;
    if (which_problem == 1) matid1 = 9003;
    if (which_problem == 2) matid1 = 2140;
    matid.resize(ntables, matid1);

    // Setup and default the table handles. These are set by
    // eos_CreateTables().
    table_handles.resize(ntables, 0);

    // Create the tables.
    std::cout << std::endl;
    std::cout << "eos_CreateTables - Create the tables." << std::endl;
    int error_code = EOS_OK;
    eos_CreateTables ( &ntables, &table_type[0], &matid[0],
                       &table_handles[0], &error_code);

    // Check and print errors from the create tables step.
    if (!check_table_errors(error_code, "eos_CreateTables")) return 1;

    // Set table options.
    error_code = EOS_OK;
    for (int i=0; i < ntables; i++) {
        eos_SetOption ( &table_handles[i], &EOS_RATIONAL,
                        EOS_NullPtr, &error_code );
    }

    // Load data into table data objects
    std::cout << "eos_LoadTables - Load the tables." << std::endl;
    eos_LoadTables ( &ntables, &table_handles[0], &error_code);

    // Check and print errors from the load tables step.
    if (!check_table_errors(error_code, "eos_LoadTables")) return 1;

    // Normal end.
    return 0;
}


// ===========================================================================
// Pack the tables.
//
// Remember that the tables are only read on the io processor.
//
// The io processor packs the tables into the packed_tables char
// array. The size of the packed_tables char array is packed_tsize.
// The packed_tables array is a class variable (an stl vector).
// After this method is done, the io processor will broadcast the packed
// tables and the size to all the other processors.
//
// The return value is 0 for success and 1 for failure.
//
// This method is only called by the io processor.
//
// ===========================================================================
int Ptest::pack_tables()
{
    if (debug)
        std::cout << "&&&&&     pack_tables, ntables = " << ntables << std::endl;

    // First get the size of the packed tables so we can allocate memory
    // to hold the packed tables.
    int error_code = EOS_OK;
    int packed_tsize = 0;
    eos_GetPackedTablesSize(&ntables, &table_handles[0], &packed_tsize,
                            &error_code);


    // Check for table size errors.
    if (!check_table_errors(error_code, "eos_GetPackedTablesSize"))
        return 1;

    // Allocate memory for the packed tables.
    packed_tables.resize(packed_tsize, 'x');

    // Pack the tables into the packed_tables array.
    error_code = EOS_OK;
    eos_GetPackedTables(&ntables, &table_handles[0], &packed_tables[0],
                        &error_code);

    // Check for packing errors.
    if (!check_table_errors(error_code, "eos_GetPackedTables"))
        return 1;

    // Successful completion.
    return 0;
}


// ===========================================================================
// Unpack the tables.
//
// The io processor has broadcast the packed tables to all the other
// processors. This method sets the tables into eospac. 
//
// The return value is 0 for success and 1 for failure.
//
// This method is called by every processor except the io processor.
//
// ===========================================================================
int Ptest::unpack_tables()
{
    int error_code = EOS_OK;
    int packed_tsize = (int)packed_tables.size();

	double *rval;
	double **rval2;
	//	rval = (double*) malloc (sizeof(double) * 1);
	//rval2 = (double**) malloc (sizeof(double*) * 1);

    if (debug) {
        std::cout << "&&&&& Before eos_SetPackedTables" << std::endl;
        std::cout << "&&&&&     ntables = " << ntables << std::endl;
        std::cout << "&&&&&     packed_tables.size() = " << packed_tsize <<
            std::endl;
        std::cout << "&&&&&     table_handles.size() = " <<
            table_handles.size() << std::endl;

        for (int i=0; i<20; i++) {
            std::cout << "packed_tables[" << i << "] = " <<
                (int)packed_tables[i] << std::endl;
        }
        for (int i=packed_tsize-21; i<packed_tsize; i++) {
            std::cout << "packed_tables[" << i << "] = " <<
                (int)packed_tables[i] << std::endl;
        }
    }

    eos_SetPackedTables(&ntables, &packed_tsize, &packed_tables[0],
                        &table_handles[0], &error_code);

    if (debug) {
        std::cout << "&&&&& After eos_SetPackedTables" << std::endl;
        std::cout << "&&&&&     ntables = " << ntables << std::endl;
        std::cout << "&&&&&     packed_tsize = " << packed_tsize << std::endl;
        for (int i=0; i<ntables; i++) {
            std::cout << "&&&&& table_handles[" << i << "] = " <<
                table_handles[i] << std::endl;
        }
    }

    // Check for errors.
    if (!check_table_errors(error_code, "eos_SetPackedTables"))
        return 1;

    // Successful completion.
    return 0;
}



// ===========================================================================
// Do the interpolation to get the desired results.
//
// All processors call this method.
//
// ===========================================================================
void Ptest::interpolate_tables()
{
    // Extrapolation test for material 9003
    if (which_problem == 1) {
        X.push_back(0.93);   Y.push_back(2000.);    Fexp.push_back(-27.);
        X.push_back(2.);     Y.push_back(2000.);    Fexp.push_back(-27.);
        X.push_back(2.);     Y.push_back(5000.);    Fexp.push_back(3.);
        X.push_back(2.);     Y.push_back(6000.);    Fexp.push_back(13.);
        X.push_back(1.15);   Y.push_back(6000.);    Fexp.push_back(13.);
        X.push_back(0.2);    Y.push_back(6000.);    Fexp.push_back(13.);
        X.push_back(0.2);    Y.push_back(4975.);    Fexp.push_back(2.75);
        X.push_back(0.2);    Y.push_back(1050.);    Fexp.push_back(-36.5);
    }

    if (which_problem == 2) {
        X.push_back(8.);  Y.push_back(1.e8);  Fexp.push_back(3.1949e+06);
    }

    // Set the number of pairs.
    int npairs = (int)X.size();

    // Resize the output vectors.
    F.resize(npairs, 0.);
    dFx.resize(npairs, 0.);
    dFy.resize(npairs, 0.);

    // Do the interpolation.
    int error_code;
    eos_Interpolate ( &table_handles[1], &npairs, &X[0], &Y[0], &F[0],
                      &dFx[0], &dFy[0], &error_code);

    // Check and print errors from the interpolation.
    // Note that we allow extrapolation.
    if (error_code != EOS_OK && error_code != EOS_INTERP_EXTRAPOLATED) {
        print_error(error_code, "eos_Interpolate");
        return;
    }
}




// ===========================================================================
// Print a single error.
// ===========================================================================
void Ptest::print_error(int ecode, std::string label)
{
    using std::cout;
    using std::endl;

    // Translate the error code into an error message.
    char emessage[EOS_MaxErrMsgLen];
    eos_GetErrorMessage ( &ecode, emessage );

    // Print the error message.
    cout << label << " ERROR " << ecode << ": " <<
        emessage << endl;
    cout << endl;
}


// ===========================================================================
// Check and print error messages for all the tables.
// ===========================================================================
bool Ptest::check_table_errors(int error_code, std::string label)
{
    using std::cout;
    using std::endl;

    if (error_code == EOS_OK) return true;

    for (int i=0; i<ntables; i++) {

        // Get the error code from the table.
        int ecode = EOS_OK;
        eos_GetErrorCode ( &table_handles[i], &ecode );

        // Process the error if it is an error.
        if (ecode != EOS_OK) {

            // Translate the error code into an error message.
            char emessage[EOS_MaxErrMsgLen];
            eos_GetErrorMessage ( &ecode, emessage );

            // Print the error message.
            cout << "Error in table " << i << endl;
            cout << "    " << label << " ERROR " << ecode << ": " <<
                emessage << endl;
            cout << endl;
        }
    }

    cout << endl;

    return false;
}


// ===========================================================================
// Print the interpolation results to the screen.
// ===========================================================================
void Ptest::print_interpolation_results()
{
    using std::cout;
    using std::endl;
    using std::setprecision;
    using std::setw;

    //    cout << endl;
    if (mype == iope)
      cout << endl << "PE       X            Y            F                Fexp" << endl;
    for (int i=0; i<(int)X.size(); i++) {

        cout << mype;
        cout << setprecision(5) << setw(10) << X[i];
        cout << setprecision(5) << setw(15) << Y[i];
        cout << setprecision(5) << setw(15) << F[i];
        cout << setprecision(5) << setw(15) << Fexp[i];
        cout << endl;
    }
    //    cout << endl;
}


// ===========================================================================
// Do a debug print.
// ===========================================================================
void Ptest::debug_print(std::string label)
{
    if (!debug) return;

    for (int pe=0; pe<npes; pe++) {
        if (mype == pe) {
            std::cout << "pe=" << pe << "  " << label << std::endl;
        }
        comm->barrier();
    }

    if (mype == iope) std::cout << std::endl;
}

void Ptest::debug_print(std::string label, int value)
{
    if (!debug) return;

    for (int pe=0; pe<npes; pe++) {
        if (mype == pe) {
            std::cout << "pe=" << pe << "  " << label << " " <<
                value << std::endl;
        }
        comm->barrier();
    }

    if (mype == iope) std::cout << std::endl;
}


} // End of the namespace for this component.
