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
  my ($exe, $matid, $tables, $subtables) = @_;
  exit -1 if ! -e $exe;
  exit -2 if $matid !~ /^\d+$/;
  exit -3 if ! scalar(@$tables);
  my $out = "";
  my $err = "";
  if (! $subtables or ! scalar(@$subtables)) {
    foreach my $tab (@$tables) {
      my $cmd = "\"$exe\" $matid $tab";
      $out .= "\n*** $cmd\n";
      $out .= run($cmd);
    }
  }
  else {
    foreach my $tab (@$tables) {
      foreach my $subtab (@$subtables) {
	my $cmd = "\"$exe\" $matid $tab $subtab";
	$out .= "\n*** $cmd\n";
	$out .= run($cmd);
      }
    }
  }
  return $out;
}

1;
