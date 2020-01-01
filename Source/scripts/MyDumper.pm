# This instructs [X]Emacs to use perl-mode #-*-perl-*-
#
# This module is a wrapper around Data::Dumper->Dump
#
# created by: David A. Pimentel
#    created: May 23, 2016
#   modified: $Date: 2017/09/15 13:45:43 MDT $
#
package MyDumper;
use strict;
use warnings;

use Exporter qw(import);

our @EXPORT_OK = qw(Dump);

sub Dump {
  use Data::Dumper;
  my ($file, $line, $ptrs, $names, $my_filter) = @_;
  local $Data::Dumper::Pad = $file . ":" . $line . ":: ";
  local $Data::Dumper::Terse = 1 if not $names;
  local $Data::Dumper::Sortkeys = $my_filter if $my_filter;
  Data::Dumper->Dump($ptrs, $names);
}

1;
