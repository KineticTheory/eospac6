/*********************************************************************
 * Header file auto-generated by Makefile
 *********************************************************************/
static char *help_str =
  "SOURCE FILE: get_sesame_data.c\n"
  "\n"
  "DETAILED DESCRIPTION\n"
  "      Given a Sesame material ID and table number, extract data from Sesame database an serve\n"
  "      to stdout in a format compatible with GNUPLOT's input requirements for a 2-D plot.\n"
  "     \n"
  "PRECONDITION\n"
  "     libeospac6.a (version 6.3 or later) is required\n"
  "     \n"
  "AUTHOR\n"
  "     David A. Pimentel\n"
  "     \n"
  "STDOUT\n"
  "      Only data in an appropriate gnuplot format are returned unless <CUSTOM OPTIONS> are provided.\n"
  "     \n"
  "STDERR\n"
  "      All information and error messages are returned.\n"
  "     \n"
  "USAGE\n"
  "      1. get_sesame_data [<GENERAL OPTIONS>] <sesMaterialNum> <sesTableNum> [ <sesSubtableIndex> ]\n"
  "     \n"
  "     2. get_sesame_data [<GENERAL OPTIONS>] id [ <file> ]\n"
  "     \n"
  "     3. get_sesame_data [<GENERAL OPTIONS>] tables <sesMaterialNum> [, <sesMaterialNum> [, ... ]]\n"
  "     \n"
  "     4. get_sesame_data [<GENERAL OPTIONS>] comments <sesMaterialNum> [, <sesMaterialNum> [, ... ]]\n"
  "     \n"
  "     5. get_sesame_data [<GENERAL OPTIONS>] <CUSTOM OPTIONS> <sesMaterialNum>\n"
  "     \n"
  "     <sesMaterialNum>     Sesame material ID number\n"
  "     <sesTableNum>        Sesame table number\n"
  "     <sesSubtableIndex>   Optional Sesame subtable number (default=1).\n"
  "       \n"
  "     id                   Character string, \"id\" used as option flag to fetch material ID number list\n"
  "     <file>               Optional Sesame file name\n"
  "       \n"
  "     tables               Character string, \"tables\" used as option flag to fetch table number list\n"
  "                                and table sizes for one or more material IDs\n"
  "       \n"
  "     comments             Character string, \"comments\" used as option flag to fetch all comments\n"
  "                                for one or more material IDs\n"
  "       \n"
  "     <GENERAL OPTIONS>\n"
  "     -c <file>\n"
  "                   Specify the configuration <file>, which contains the desired options.\n"
  "                   (default: config.get_sesame_data)\n"
  "     -D <file>\n"
  "                   Create data dump file (see EOSPAC 6's TablesLoaded.dat file) with specified name, <file>.\n"
  "                   If <file> is an empty string, then the default, TablesLoaded.dat, will be used.\n"
  "                   This option is ignored for the USAGE case 4 (comments) listed above.\n"
  "     -F <file>\n"
  "                   Search named Sesame <file> exclusively.\n"
  "     -f\n"
  "                   Force the manufacture of the required table(s).\n"
  "     -G [x|y]\n"
  "                   Return either \"x\" and/or \"y\" tabulated data, which corresponds to <sesMaterialNum>,\n"
  "                   <sesTableNum> and <sesSubtableIndex>.\n"
  "     -g\n"
  "                   Return gnuplot compatible output. Metadata is prefixed with # and sent to stdout\n"
  "                   rather than stderr.\n"
  "     -h\n"
  "                   Display the basic usage of this tool.\n"
  "     -O\n"
  "                   Write to stdout a default configuration file that contains possible EOSPAC 6 options'\n"
  "                   key/value pairs.\n"
  "     -t\n"
  "                   For bivariate tables, transpose the order of data stream to vary y in each data\n"
  "                   block rather than the default behavior that varies x in each data block.\n"
  "     -V\n"
  "                   Display the version number.\n"
  "     -v\n"
  "                   Enable verbose output. Multiple instances of this option will increase the\n"
  "                   verbosity.\n"
  "     -X\n"
  "                   Force the loaded table(s) to be monotonic with respect to x.\n"
  "     -Y\n"
  "                   Force the loaded table(s) to be monotonic with respect to y.\n"
  "       \n"
  "     <CUSTOM OPTIONS>\n"
  "     -I N\n"
  "                   Number of samples, N, to take from the x-independent variable's range. Default=50.\n"
  "     -J N\n"
  "                   Number of samples, N, to take from the y-independent variable's range. Default=50.\n"
  "     -n N\n"
  "                   Number of samples, N, to take from each independent variable's range. Default=50.\n"
  "                   This option overrides the '-I' and '-J' options.\n"
  "     -r R[:R1]\n"
  "                   Specify density (g/cc).\n"
  "                   Optional range upper bound, R1, delimited with a colon, \":\".\n"
  "     -T T[:T1]\n"
  "                   Specify temperature (K) -- requires \"-r\" option.\n"
  "                   Optional range upper bound, T1, delimited with a colon, \":\".\n"
  "     -P P[:P1]\n"
  "                   Specify pressure (GPa) -- requires \"-r\" option.\n"
  "                   Optional range upper bound, P1, delimited with a colon, \":\".\n"
  "     -U U[:U1]\n"
  "                   Specify specific internal energy (MJ/kg) -- requires \"-r\" option.\n"
  "                   Optional range upper bound, U1, delimited with a colon, \":\".\n"
  "     -A A[:A1]\n"
  "                   Specify specific Helmholtz free energy (MJ/kg) -- requires \"-r\" option.\n"
  "                   Optional range upper bound, A1, delimited with a colon, \":\".\n"
  "     -S S[:S1]\n"
  "                   Specify specific entropy (MJ/kg/K) -- requires \"-r\" option.\n"
  "                   Optional range upper bound, S1, delimited with a colon, \":\".\n"
  "     \n"
  "     Note that the EOSPAC table inversion algorithm may produce incorrect\n"
  "     \n"
  "     and/or unexpected results when using any of the following options:\n"
  "     \n"
  "     -P, -U, -A, -S.\n"
  "     \n"
  "     The following are applicable values of sesSubtableIndex given sesTableNum:\n"
  "     \n"
  "       sesTableNum sesSubtableIndex (description)\n"
  "               101 <ignored>        (comment tables)\n"
  "               201 <ignored>        (general information)\n"
  "               301 1                (total pressure)\n"
  "                   2                (total internal energy)\n"
  "                   3                (total free energy)\n"
  "                   4                (total entropy -- derived from Sesame data)\n"
  "               303 1                (ion + cold curve pressure)\n"
  "                   2                (ion + cold curve internal energy)\n"
  "                   3                (ion + cold curvefree energy)\n"
  "                   4                (ion + cold entropy -- derived from Sesame data)\n"
  "               304 1                (electron pressure)\n"
  "                   2                (electron internal energy)\n"
  "                   3                (electron free energy)\n"
  "                   4                (electron entropy -- derived from Sesame data)\n"
  "               305 1                (ion pressure)\n"
  "                   2                (ion internal energy)\n"
  "                   3                (ion free energy)\n"
  "                   4                (ion entropy -- derived from Sesame data)\n"
  "               306 1                (cold curve pressure)\n"
  "                   2                (cold curve internal energy)\n"
  "                   3                (cold curve free energy)\n"
  "               311 1                (maxwell total pressure)\n"
  "                   2                (maxwell total internal energy)\n"
  "                   3                (maxwell total free energy)\n"
  "                   4                (maxwell total entropy -- derived from Sesame data)\n"
  "               321 <num>            (mass fraction data corresponding to material phase <num>)\n"
  "               401 1                (vapor pressure)\n"
  "                   2                (vapor density on coexistence line)\n"
  "                   3                (density of liquid or solid on coexistence line)\n"
  "                   4                (internal energy of vapor on coexistence line)\n"
  "                   5                (internal energy of liquid on coexistence line)\n"
  "                   6                (free energy of vapor on coexistence line)\n"
  "                   7                (free energy of liquid on coexistence line)\n"
  "               411 1                (melt temperature)\n"
  "                   2                (melt pressure)\n"
  "                   3                (internal energy of solid on the melt line)\n"
  "                   4                (free energy of solid on the melt line)\n"
  "               412 1                (melt temperature)\n"
  "                   2                (melt pressure)\n"
  "                   3                (internal energy of liquid on the melt line)\n"
  "                   4                (free energy of liquid on the melt line)\n"
  "               431 1                (shear modulus)\n"
  "               501 1                (LOG10 temperature/density pairs)\n"
  "           502-505 1                (LOG10 opacity quantities [see Ref. 1 for more detail])\n"
  "           601-605 1                (LOG10 conductivity quantities [see Ref. 1 for more detail])\n"
  "\n"
  "REFERENCES\n"
  "     1.  D. A. Pimentel, \"EOSPAC USER'S MANUAL: Version 6.3\", LA-UR-14-29289, Los Alamos National Laboratory (2014). \n"
  "     2.  S. P. Lyon, J. D. Johnson, Group T-1, \"Sesame: The Los Alamos National Laboratory Equation of State Database\", LA-UR-92-3407. \n"
  "\n"
  "RETURN VALUES\n"
  "      0     No errors encounterd\n"
  "      1     Insufficient arguments passed to this utility\n"
  "      2     Invalid sesSubtableIndex\n"
  "      3     sesSubtableIndex is valid given sesTableNum, but the subtable is unavailable\n"
  "      4     Invalid sesTableNum\n"
  "      5     Data dump error; no stdout\n"
  "      6     fprintf function error; probably system-related\n"
  "      7     memory allocation error\n"
  "      8     Invalid sesame file name\n"
  "      9     Missing sesame file\n"
  "     10     Incorrect number of independent variables specified\n"
  "     11     Invalid independent variables specified\n"
  "     12     The -r (density) independent variable is required\n"
  "     13     _QuickSort ERROR exceeded QUICKSORT_RECURSION_LIMIT\n"
  "     14     calculateCategory2 ERROR performing inverse interpolation\n"
  "    255     EOSPAC related error occurred; corresponding message written to stderr\n"
  "\n"
  ;
