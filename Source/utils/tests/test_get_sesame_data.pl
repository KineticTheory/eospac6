eval '(exit $?0)' && eval 'exec perl -S $0 ${1+"$@"}' #-*-perl-*-
  & eval 'exec perl -S $0 $argv:q'
  if 0;

# ======================================================================
#
# CONFIGURATION SECTION
#
# ======================================================================

use strict;
use warnings;

# Get command line options
use Getopt::Long;
my $options={};
$Getopt::Long::ignorecase=0;
GetOptions($options,
	   "--exe=s",
	  );

use FindBin;
use lib "$FindBin::Bin";

use Data::Dumper;
use test_functions 'run_get_sesame_data';

# ======================================================================
#
# TESTS SECTION
#
# ======================================================================

# fetch comment tables and 201 tables
foreach my $matid ( 2140, 32140, 12140, 22141 ) {
  print run_get_sesame_data($options->{exe}, $matid, [101, 201]);
}

# fetch 300-series tables including calculated entropy
print run_get_sesame_data($options->{exe}, 2140, [301, 303, 304, 305], [1, 2, 3, 4]);

# fetch 401 tables including calculated entropy
print run_get_sesame_data($options->{exe}, 2140, [401], [1, 2, 3, 4, 5, 6, 7]);

# fetch 411 and 412 tables.pl.stdout
print run_get_sesame_data($options->{exe}, 32140, [411, 412], [1, 2, 3]);

# fetch 431 tables
print run_get_sesame_data($options->{exe}, 32140, [431]);

# fetch 500-series tables
print run_get_sesame_data($options->{exe}, 12140, [501, 502, 503, 504, 505]);

# fetch 600-series tables
print run_get_sesame_data($options->{exe}, 22141, [601, 602, 603, 604, 605]);
