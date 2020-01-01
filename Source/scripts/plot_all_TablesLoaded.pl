eval '(exit $?0)' && eval 'exec perl -S $0 ${1+"$@"}' #-*-perl-*-
& eval 'exec perl -S $0 $argv:q'
        if 0;

use POSIX 'strftime';
use File::Copy;
use FindBin;
use File::Basename;
use lib $FindBin::Bin;
use MyDumper;
#use Data::Dumper;
#$Data::Dumper::Terse = 1;

BEGIN {
  # Define script version
  $version = '$Revision: 1.9 $';
  $version =~ s/\$Revision:\s+//;
  $version =~ s/([\d\.]+).+/$1/;

  # Determine the name of this script without the path.
  $loc = $FindBin::Bin;
  $me  = $FindBin::Script;
}

# Define brief
$brief =
"This tool is designed as a wrapper around sesplot to plot some or all of the data sets in
 one or more specified TablesLoaded.dat file(s), which is optionally-created by EOSPAC6.
";

# Define command line options
%options_definitions =
  (#KEY               DESCRIPTION
   "brief"         => "Display a brief description of this tool.",
   "count_only"    => "Count the number of data sets within the specified file(s).",
   "debug"         => "Display the sesplot commands that would be executed without this option.",
   "help"          => "Display this help.",
   "interleave"    => "Interleave plots from like-numbered data sets in each specified file(s)." .
                      " This requires that the specified file(s) have the same number of data sets.",
   "keep"          => "Keep temporary/intermediate files.",
   "matid=i@"      => "Specify the SESAME material ID <NUMBER> used to select the data sets within the specified" .
                      " file(s). Multiple instances of this option may be specified on the command line.",
   "print"         => "Send the generated plots to the printer via the 'lp' tool.",
   "postscript|ps" => "Send the generated plots to postscript file(s) in the current directory.",
   "pdf=s"         => "Send the generated plots to a PDF file, <NAME>.",
   "scale=f"       => "Specify a scaling factor to be applied to the x- and y-dimensions via the sesplot -geometry" .
                      " <NUMBER>*1024x<NUMBER>*768+0+0",
   "type=s@"       => "Specify the EOSPAC 6 table type <NAME>(s) used to select the data sets within the specified" .
                      " file(s). Multiple instances of this option may be specified on the command line.",
   "useMyEnvironment" => "Force the various required and optional utilities to be found from the user's environment" .
                         " rather than the default configuration file.",
   "verbose+"      => "Enable verbose debug output. Multiple instance of this option increase output.",
   "version"       => "Display version information."
  );

# Get command line options
use Getopt::Long;
$options={};
$Getopt::Long::ignorecase=0;
GetOptions($options,
	   keys(%options_definitions)
	  );

&doYouNeedHelp($me, $version, $brief);

die "\nERROR: At least one file name must be specified.\n\n"
  if scalar(@ARGV) == 0;

$options->{postscript}++ if $options->{pdf};

#$sesplot = "~/SCRIPTS/sesplot";
#$sesplot = "~/FILES/sesame_viz/sesplot";
$sesplot = which("sesplot");
print MyDumper::Dump(__FILE__, __LINE__, [ \$sesplot ], [ qw(*sesplot) ]) if $options->{verbose};

$printme = "";
$printme = "--print"      if $options->{print};
$printme = "--postscript" if $options->{postscript};
$keep    = "--keepFiles"  if $options->{keep};

print MyDumper::Dump(__FILE__, __LINE__, [ \$options ], [ qw(*options) ]) if $options->{verbose} > 2;
print MyDumper::Dump(__FILE__, __LINE__, [ \$printme ], [ qw(*printme) ]) if $options->{verbose} > 2;

my %MASTER = ();
my %COMMON = ();
my @COMMON = ();

for $file (@ARGV) {

  # capture all MATIDS and related meta data
  open(my $fh,"$file");
  my %MASTER = ();
  while (<$fh>) {
    if (/^TableHandle=(\d+)\s*matid\s*=\s*(\d+)\s*source\s*=\s*\S+\s*(page\s*=\s*(\d+))?/) {
      my ($h, $m, $p) = ($1, $2, $4);
      if ($p == 1 or not defined $p) {
        my $s = <$fh>;
        if ($s =~ /^Data Type\s*=\s*(\S+)/) {
          $MASTER{$file}{$m}{$h}{type} = $1;
          $s = <$fh>;
          my $o = "default";
          if ($s =~ /^Loading Options:\s*(.+)/) {
            $o = $1;
          }
          push @{$MASTER{$file}{$m}{$h}{options}}, $o;
        }
      }
    }
  }
  if ($options->{verbose} > 3) {
    local $Data::Dumper::Indent = 1;
    local $Data::Dumper::Sortkeys = sub{
      no warnings 'numeric';
      [ sort { $a <=> $b } keys %{$_[0]} ]
    };
    print MyDumper::Dump(__FILE__, __LINE__, [ \%MASTER ], [ qw(*MASTER) ]);
  }
  close $fh;

  for my $f (sort keys %MASTER) {
    for my $m (sort { $a <=> $b } keys %{$MASTER{$f}}) {
      for my $h (sort { $a <=> $b } keys %{$MASTER{$f}{$m}}) {
        my $val = "$m " . $MASTER{$f}{$m}{$h}{type};
        push @{$MATIDS_TYPES{$file}{list}}, $val;
      }
    }
  }

  chomp(@{$MATIDS_TYPES{$file}{list}});
  $MATIDS_TYPES{$file}{N} = scalar @{$MATIDS_TYPES{$file}{list}};

  #print MyDumper::Dump(__FILE__, __LINE__, [ \%MATIDS_TYPES ], [ qw(*MATIDS_TYPES) ]) if $options->{verbose} > 2;

  %h = ();
  for my $m (@{$MATIDS_TYPES{$file}{list}}) {
    $h{$file}{$m}++;
    $m .= " " . $h{$file}{$m};
  }

  print $MATIDS_TYPES{$file}{N}," data sets available in $file\n";

}

# compile hash of list for each file
for $file (@ARGV) {
  for my $m (@{$MATIDS_TYPES{$file}{list}}) {
      $MATIDS_TYPES{$file}{hash}{$m}++;
  }
}
print MyDumper::Dump(__FILE__, __LINE__, [ \%MATIDS_TYPES ], [ qw(*MATIDS_TYPES) ]) if $options->{verbose} > 2;

%h = ();
for $file (@ARGV) {
  for my $m (@{$MATIDS_TYPES{$file}{list}}) {
    push(@{$COMMON{list}}, $m) if not grep {/$m/} @{$COMMON{list}};
  }
}
$COMMON{N} = scalar @{$COMMON{list}};
print MyDumper::Dump(__FILE__, __LINE__, [ \%COMMON ], [ qw(*COMMON) ]) if $options->{verbose} > 3;

exit if $options->{count_only};

my @ps_files = ();

if ($options->{interleave}) {
  &interleavePlotOrder;
}
else {
  &defaultPlotOrder;
}

&createPDF if ($options->{pdf});

# for each file, list the tables that were not loaded
print "\n";
%NOT_LOADED = ();
for $file (@ARGV) {

  my @list = `grep 'NOT LOADED' "$file"`;
  push @{$NOT_LOADED{$file}{list}}, @list if scalar @list;

}
for $file (@ARGV) {

  if (scalar @{$NOT_LOADED{$file}{list}}) {
    print '-'x5, " $file ", '-'x5, "\n";
    print "\t", join("\t", @{$NOT_LOADED{$file}{list}}), "\n"
  }

}

####################################################################################################

sub createPDF {

  print MyDumper::Dump(__FILE__, __LINE__, [ \@ps_files ], [ qw(*ps_files) ]) if $options->{verbose} > 1;

  #push @ps_files, glob("sesplot.*.ps");

  my $gs = `which gs`;
  chomp $gs;

  if ( -x "$gs" ) {
    my $o = $options->{pdf};
    my $cmd = "$gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=$o @ps_files";

    $cmd = "set -x;$cmd" if $options->{verbose} > 1;

    system("$cmd");

    if (not $options->{keep}) {
      for my $f (@ps_files) {
        $| = 1;
        print "remove $f" if $options->{verbose} > 1;
        unlink($f) if -r $f;
        print ((-r $f) ? " ... FAILED\n" : " ... OK\n") if $options->{verbose} > 1;
      }
    }

    print "Create $o", ((-r $o) ? " ... OK" : " ... FAILED"), "\n";
  }
  else {
    warn "\nExecutable not found, $gs\n\n";
  }

}

sub defaultPlotOrder {

  my $N = undef;
  my $MATIDS_TYPES = undef;
  my @MATIDS_TYPES = ();
  my $plotted_count = 0;

  my $flabel = "file0";

  for $file (sort keys %MATIDS_TYPES) {

    $flabel++;

    $N = $MATIDS_TYPES{$file}{N} if not $N;
    $MATIDS_TYPES = join " ", @{$MATIDS_TYPES{$file}{list}} if not $MATIDS_TYPES;
    @MATIDS_TYPES = @{$MATIDS_TYPES{$file}{list}} if not scalar @MATIDS_TYPES;

    %SUBTABLE = ();
    $i = 0;

    for $p (@MATIDS_TYPES) {

      ($m,$t,$k) = split " ", $p;
      $SUBTABLE{"$m $t"}++;
      $s = $SUBTABLE{"$m $t"};

      $i++;

      next if not &is_wanted("matid", $m);
      next if not &is_wanted("type", $t);

      my $psfile = join(".", "sesplot", $m, $t, $k, $flabel, "ps");

      my $cmd = "$sesplot " . ($options->{scale} ? join(" ", "--geometry", '*' . $options->{scale}, "") : "")
        . ($options->{useMyEnvironment} ? "--useMyEnvironment " : "")
          . "--tablesloaded $file $m $t $k $printme " . (($printme=~/postscript/)?"$psfile ":"") ."$keep";
      if ($options->{verbose} or $options->{debug}) {
        printf "   #%*d of %d: %s\n", length($N), $i, $N, $cmd;
      }
      else {
        printf "\r#%d of %d ", $i, $N;
      }

      next if $options->{debug};

      $cmd = "set -x;$cmd" if $options->{verbose} > 1;

      my $filter = $options->{verbose} > 1 ? "cat" : "grep -v ': OK'";
      system("( $cmd 2>/dev/null ) | $filter");

      $plotted_count++;

      if ($options->{postscript}) {
        push @ps_files, $psfile if -r "$psfile";
      }

    }

  }

  print "\r$plotted_count plots created\n" if not ($options->{verbose} or $options->{debug});
}

sub interleavePlotOrder {

  my $N = undef;
  my $MATIDS_TYPES = undef;
  my @MATIDS_TYPES = ();

#   for $file (sort keys %MATIDS_TYPES) {

#     $N = $MATIDS_TYPES{$file}{N} if not $N;
#     $MATIDS_TYPES = join " ", @{$MATIDS_TYPES{$file}{list}} if not $MATIDS_TYPES;
#     @MATIDS_TYPES = @{$MATIDS_TYPES{$file}{list}} if not scalar @MATIDS_TYPES;

#     die "ERROR: The number of tables in all files must be identical\n"
#       if $N != $MATIDS_TYPES{$file}{N};

#     die "ERROR: The MATIDS_TYPES lists in all files must be identical\n"
#       if $MATIDS_TYPES ne join(" ", @{$MATIDS_TYPES{$file}{list}});

#   }
  #print MyDumper::Dump(__FILE__, __LINE__, [ \%MATIDS_TYPES ], [ qw(*MATIDS_TYPES) ]) if $options->{verbose} > 2;

  $i = 0;

  $N = $COMMON{N};

  for $p (@{$COMMON{list}}) {

    ($m,$t,$k) = split " ", $p;

    print MyDumper::Dump(__FILE__, __LINE__, [ \$m,\$t,\$k ], [ qw(*m *t *k) ]) if $options->{verbose} > 3;

    $i++;
    $s = $i;

    my $flabel = "file0";

    for $file (sort keys %MATIDS_TYPES) {

      $flabel++;

      if (not &is_wanted("matid", $m)) {
        print MyDumper::Dump(__FILE__, __LINE__, [ ":: skipping matid, $m" ]) if $options->{verbose} > 1;
        next;
      }
      if (not &is_wanted("type", $t)) {
        print MyDumper::Dump(__FILE__, __LINE__, [ ":: skipping type, $t" ]) if $options->{verbose} > 1;
        next;
      }

      my $psfile = join(".", "sesplot", $m, $t, $k, $flabel, "ps");

      my $cmd = "$sesplot " . ($options->{scale} ? join(" ", "--geometry", '*' . $options->{scale}, "") : "")
        . ($options->{useMyEnvironment} ? "--useMyEnvironment " : "")
          . "--tablesloaded $file $m $t $k $printme " . (($printme=~/postscript/)?"$psfile ":"") ."$keep";
      if ($options->{verbose} or $options->{debug}) {
        printf "   #%*d of %d: %s\n", length($N), $i, $N, $cmd;
      }
      else {
        printf "\r#%d of %d ", $i, $N;
      }

      next if $options->{debug};

      $cmd = "set -x;$cmd" if $options->{verbose} > 1;

      my $filter = $options->{verbose} > 1 ? "cat" : "grep -v ': OK'";
      system("( $cmd 2>/dev/null ) | $filter");

      if ($options->{postscript}) {
        push @ps_files, $psfile if -r "$psfile";
      }

    }

  }

  print "\r" if not ($options->{verbose} or $options->{debug});
}

sub is_wanted {
  my ($key, $val) = @_;
  my $wanted = 0;
  if ($options->{$key} and scalar @{$options->{$key}}) {
    for (@{$options->{$key}}) {
      if ($_ eq $val) {
        $wanted++;
      }
    }
  }
  else {
    $wanted = 1;
  }
  return $wanted;
}

sub which {
  use File::Spec;
  my @NAMES = @_;
  my @PATH = File::Spec->path();
  my @PATHEXT = ( q{} );
  push @pathext, map { lc } split /;/, $ENV{PATHEXT} if $^O eq 'MSWin32';
  my @results;

  if ($options->{"verbose"} and $options->{"verbose"} > 1) {
    print MyDumper::Dump(__FILE__, __LINE__, [ \@NAMES ], [ qw(*NAMES) ]);
  }
  for my $progname ( @NAMES ) {
    next unless $progname eq fileparse $progname;
    for my $dir ( @PATH ) {
      for my $ext ( @PATHEXT ) {
        my $f = File::Spec->catfile($dir, "$progname$ext");
        print MyDumper::Dump(__FILE__, __LINE__, [ \$f ], [ qw(*f) ]) if $options->{"verbose"} and $options->{"verbose"} > 1;
        push @results, $f if -x $f;
      }
    }
  }

  return @results if wantarray;
  return $results[0];
}

sub doYouNeedHelp {
  my ($me, $version, $purpose) = @_;
  my $n, $o;
  my $mdate = strftime '%Y-%m-%d', localtime((stat($0))[9]);
  my $perl_version = "PERL $^V";

  # Display version information
  if ($options->{"version"}) {
    print "$me $version\n";
    exit 0;
  }

  return if not $options->{"help"};

  use Term::ANSIColor qw(:constants);
  use Carp;
  sub swrite {
    croak "usage: swrite PICTURE ARGS" unless @_;
    my $format = shift;
    $^A = "";
    formline($format,@_);
    my $s = $^A;
    $^A = "";                   # reset global accumulator variable
    return $s;
  }

  my $me_version = "$me $version";

  my @args = ( $perl_version, $me_version, $mdate );
  my $banner = swrite(<<'BANNER', @args);
^<<<<<<<<<<<<<< ^||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||| ^>>>>>>>>>>
BANNER

  @args = ( $me, "[ <options> ] file [, file [, ...]]" );
  my $synopsis = swrite(<<'SYNOPSIS', @args);
  ^<<<<<<<<<<<<<<<<<<<<<<<< ^<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<~~
SYNOPSIS

  @args = ( "sesplot" );
  my $requirements = swrite(<<'REQUIREMENTS', @args);
  ^<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<~~
REQUIREMENTS

  @args = ( $purpose );
  my $description = swrite(<<'PURPOSE', @args);
  ^<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<~~
PURPOSE

  format HELP =

  ^<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<~~
  $o
              ^<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<~~
              $options_definitions{$n}
.

  do {
    local *STDOUT;

    open STDOUT, "| more";

    printf("\n%s\n", BOLD $banner, RESET);

    printf("\n%s\n", BOLD "NAME", RESET);
    my @args = ( "  $me" );
    printf("\n%s\n", @args);

    printf("\n%s\n", BOLD "SYNOPSIS", RESET);
    printf("\n%s\n", $synopsis);

    printf("\n%s\n", BOLD "REQUIREMENTS", RESET);
    printf("\n%s\n", $requirements, RESET);

    printf("%s\n", BOLD "DESCRIPTION", RESET);
    printf("\n%s", $description, RESET);

    $~ = 'HELP';
    for $n (sort keys(%options_definitions)) {
      my ($key,$opt_arg) = split /[:=]/, $n, 2;
      $opt_arg = $n =~ /:/ ? " [$opt_arg]" : " $opt_arg";
      $opt_arg =~ s/\@//g;
      $opt_arg =~ s/\b[fi]\b/<NUMBER>/g;
      $opt_arg =~ s/\b[s]\b/<NAME>/g;
      $key =~ s/\+//g;
      my @key = split /\|/, $key;
      $o = join "\r", map {"--$_$opt_arg"} @key;
      write;
    }

    printf("\n%s\n", BOLD $banner, RESET);

  };
  exit 0;
}

