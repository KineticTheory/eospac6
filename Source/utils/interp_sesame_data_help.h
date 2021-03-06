/*********************************************************************
 * Header file auto-generated by Makefile
 *********************************************************************/
static char *help_str =
  "SOURCE FILE: interp_sesame_data.c\n"
  "\n"
  "DETAILED DESCRIPTION\n"
  "      Given a Sesame material ID, a Sesame table number, an EOSPAC 6 table type and independent\n"
  "      variable values (or ranges) appropriate to the table type, interpolate the function\n"
  "      value(s) and partial derivative values.\n"
  "     \n"
  "PRECONDITION\n"
  "     libeospac6.a (version 6.3 or later) is required\n"
  "     \n"
  "AUTHOR\n"
  "     David A. Pimentel\n"
  "     \n"
  "STDOUT\n"
  "      Interpolated values are returned.\n"
  "     \n"
  "STDERR\n"
  "      All error messages are returned.\n"
  "     \n"
  "USAGE\n"
  "      interp_sesame_data [<OPTIONS>] <sesMaterialNum> <tableType> <x>[:<x1>] [ <y>[:<y1>] ]\n\n"
  "      interp_sesame_data [<OPTIONS>] <sesMaterialNum> <tableType> -i <file>\n"
  "     \n"
  "     <sesMaterialNum>\n"
  "                      Sesame material ID number\n"
  "     <tableType>\n"
  "                      EOSPAC 6 table type (case insensitive)\n"
  "     <x>\n"
  "                      First independent variable value of the table type (64-bit floating point)\n"
  "                      This argument is required unless either the '-i' or the '-x' option is used.\n"
  "                      The optional :<x1> defines an upper bound for a randomly-sampled range of\n"
  "                      values between <x> and <x1>.\n"
  "     <y>\n"
  "                      Second independent variable value of the table type (64-bit floating point)\n"
  "                      This argument is required unless either the '-i' or the '-y' option is used.\n"
  "                      The optional :<y1> defines an upper bound for a randomly-sampled range of\n"
  "                      values between <y> and <y1>.\n"
  "       \n"
  "     <OPTIONS>\n"
  "     -c <file>\n"
  "                   Specify the configuration <file>, which contains the desired options.\n"
  "     -D <file>\n"
  "                   Create data dump file (see EOSPAC 6's TablesLoaded.dat file) with specified name, <file>.\n"
  "                   If <file> is an empty string, then the default, TablesLoaded.dat, will be used.\n"
  "     -d\n"
  "                   Display double precision interpolated output.\n"
  "     -e\n"
  "                   Allow extrapolation to continue with warning(s).\n"
  "     -F <file>\n"
  "                   Search named Sesame <file> exclusively.\n"
  "     -g\n"
  "                   Return gnuplot compatible output. Metadata is prefixed with # and sent to stdout\n"
  "                   rather than stderr.\n"
  "     -h\n"
  "                   Display the basic usage of this tool.\n"
  "     -I N\n"
  "                   Number of samples, N, to take from the x-independent variable's range. Default=50.\n"
  "     -i <file>\n"
  "                   Parse <file>, which contains two columns of data where the <xValue> and <yValue>\n"
  "                   input correspond to columns 1 and 2 respectively.\n"
  "                   Text trailing a # character will be ignored as notes.\n"
  "                   If <file> is the '-' character, then stdin is parsed.\n"
  "                   This option overrides the '-x' and '-y' options.\n"
  "     -J N\n"
  "                   Number of samples, N, to take from the y-independent variable's range. Default=50.\n"
  "     -K\n"
  "                   Disable the masking of extrapolated values in the data dump file associated with the\n"
  "                   -D option.\n"
  "     -L\n"
  "                   Use EOSPAC's linear interpolation instead of the default rational interpolation.\n"
  "     -l\n"
  "                   Rather than the default latin hypercube sampling method, use a logarithmic sampling\n"
  "                   of x- and/or y-input ranges.\n"
  "     -M v\n"
  "                   Specify the independent variable, v, with respect to which EOSPAC must enforce\n"
  "                   monotonicity.\n"
  "                   Valid case-insensitive value, v: 'x' or 'y'\n"
  "     -n N\n"
  "                   Number of samples, N, to take from each independent variable's range. Default=50.\n"
  "                   This option overrides the '-I' and '-J' options.\n"
  "     -O\n"
  "                   Write to stdout a default configuration file that contains possible EOSPAC 6 options'\n"
  "                   key/value pairs.\n"
  "     -p\n"
  "                   Enable the EOSPAC6 option, EOS_INVERT_AT_SETUP, for diagnostic purposes.\n"
  "     -s\n"
  "                   Ignored unless '-g' option is used. Ensure the output will be compatible with\n"
  "                   gnuplot's 'splot' function.\n"
  "     -V\n"
  "                   Display the version number.\n"
  "     -v\n"
  "                   Enable verbose output. Multiple instances of this option will increase the\n"
  "                   verbosity.\n"
  "     -x (<file>|<list>)\n"
  "                   Parse <file>, which contains one column of <xValue> data. This overrides the <x>\n"
  "                   command line argument, subsequently making <x> not required.\n"
  "                   Text trailing a # character will be ignored as notes.\n"
  "                   If <list> is a comma-delimited list of numbers (i.e., <x>[,<x1>[,<x2>[,...]]]),\n"
  "                   then it is parsed as a list instead of a <file> containing discreet values.\n"
  "                   This option is gnored if the '-i' is used.\n"
  "     -y (<file>|<list>)\n"
  "                   Parse <file>, which contains one column of <yValue> data. This overrides the <y>\n"
  "                   command line argument, subsequently making <y> not required.\n"
  "                   Text trailing a # character will be ignored as notes.\n"
  "                   If <list> is a comma-delimited list of numbers (i.e., <y>[,<y1>[,<y2>[,...]]]),\n"
  "                   then it is parsed as a list instead of a <file> containing discreet values.\n"
  "                   This option is gnored if the '-i' is used.\n"
  "     \n"
  "REFERENCES\n"
  "     1.  D. A. Pimentel, \"EOSPAC USER'S MANUAL: Version 6.3\", LA-UR-14-29289, Los Alamos National Laboratory (2014). \n"
  "     2.  S. P. Lyon, J. D. Johnson, Group T-1, \"Sesame: The Los Alamos National Laboratory Equation of State Database\", LA-UR-92-3407, Los Alamos National Laboratory (1992). \n"
  "\n"
  "RETURN VALUES\n"
  "      0     No errors encounterd\n"
  "      1     Insufficient arguments passed to this utility\n"
  "      5     Data dump error; no stdout\n"
  "      6     fprintf function error; probably system-related\n"
  "      7     memory allocation error\n"
  "     15     Invalid table type (2nd argument)\n"
  "     16     Cannot open specified input file\n"
  "    255     EOSPAC related error occurred; corresponding message written to stderr\n"
  "\n"
  ;
