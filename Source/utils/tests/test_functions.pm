package test_functions;

require Exporter;

our @ISA = qw(Exporter);
our @EXPORT = qw(run_get_sesame_data);

use strict;
use warnings;

sub run {

  use IPC::Open3;

  my $cmd = shift or die "ERROR: no command defined\n";

  my $out = "";
  my $err = "";

  my $pid = open3(\*WRITER, \*READER, \*ERROR, $cmd);
  #if \*ERROR is 0, stderr goes to stdout

  while ( my $output = <READER> ) {
    $out .= $output;
  }

  while ( my $errout = <ERROR> ) {
    $err .= $errout;
  }

  waitpid( $pid, 0 ) or die "$!\n";
  my $retval =  $?;
  if (wantarray()) { return ($retval, $out, $err); }
  else             { return "$err\n$out\n";        }

}

sub run_get_sesame_data {
  my ($exe, $matid, $tables, $subtables, $options) = @_;
  exit -1 if ! -e $exe;
  # exit -2 if $matid !~ /^\d+$/;
  # exit -3 if ! scalar(@$tables);
  my $opt_str = ($options and scalar(@$options)) ? join(" ", @$options) : "";
  my $out = "";
  my $err = "";
  # print "debug: \$opt_str = $opt_str\n" if $options->{verbose};
  if (! $subtables or ! scalar(@$subtables)) {
    # print __FILE__,"(",__LINE__,")::HERE!\n";
    if ($matid and ($tables or scalar(@$tables))) {
      foreach my $tab (@$tables) {
        my $cmd = "\"$exe\" $opt_str $matid $tab";
        print "debug: \$cmd = $cmd\n" if $options->{verbose};
        $out .= "\n*** $cmd\n";
        $out .= run($cmd);
      }
    }
    else {
      my $cmd = "\"$exe\" $opt_str";
      $out .= "\n*** $cmd\n";
      $out .= run($cmd);
    }
  }
  else {
    # print __FILE__,"(",__LINE__,")::HERE!\n";
    foreach my $tab (@$tables) {
      foreach my $subtab (@$subtables) {
        my $cmd = "\"$exe\" $opt_str $matid $tab $subtab";
        $out .= "\n*** $cmd\n";
        $out .= run($cmd);
      }
    }
  }
  return $out;
}

1;
