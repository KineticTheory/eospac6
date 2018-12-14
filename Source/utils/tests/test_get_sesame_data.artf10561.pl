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

print run_get_sesame_data($options->{exe}, 2140, [101, 201]);
