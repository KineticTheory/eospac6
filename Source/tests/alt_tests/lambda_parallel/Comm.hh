#ifndef COMM_SIMPLE_HH_INCLUDE
#define COMM_SIMPLE_HH_INCLUDE

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

namespace Comm_ns
{

class Comm
{
public:
    // Constructors.
    Comm() {};
    Comm(int &argc, char** &argv);

    // Miscellaneous inquiry functions
    int getProcRank() const { return(mype); }
    int getNumProcs() const { return(npes); }
    bool isIOProc() const {
        if (mype == iope) return true;
        return false;
    }
    int getIOProcRank() const { return(iope); }

    // Barrier.
    void barrier();

    // Broadcast an integer.
    void broadcast(int &data);

    // Broadcast a character vector.
    void broadcast(std::vector<char> &data);

    // Broadcast an array of char's from iope to all other processors.
    void broadcast(char *data, int ndata);

    // Sum an integer over all processors.
    void sum(int &data);

    // Exit the calculation gracefully.
    void finalize();

private:

    // Processor data.
    int npes;  // Number of processors.
    int mype;  // The processor index.
    int iope;  // The processor index that does the io.

};

} // end namespace Comm_ns

#endif
