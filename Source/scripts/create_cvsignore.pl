eval '(exit $?0)' && eval 'exec perl -S $0 ${1+"$@"}' #-*-perl-*-
& eval 'exec perl -S $0 $argv:q'
        if 0;

##################################################################
#
#  Copyright -- see file named COPYRIGHTNOTICE
#
##################################################################

die "\nUsage:\n\t$0 <cvsignore file> [SUBDIRS]\n\n" if (! @ARGV);

use File::Basename;

#
# Slurp <cvsignore file> into %s
#
$f = shift;
$s = "";
open FH, "$f";
while (<FH>) {
  chomp;
  $s{$_} = 1;
}
close FH;

#
# Include @ARGV in %s
#
for (@ARGV) {
  $s{$_} = 1;
}

#use Data::Dumper;
#print '%s = ', Dumper(\%s);

#
# Write new version of <cvsignore file>
#
END {
  open FH, ">$f";
  print FH join("\n", sort(keys %s)), "\n";;
  close FH;
}
