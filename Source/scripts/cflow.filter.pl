eval '(exit $?0)' && eval 'exec perl -S $0 ${1+"$@"}' #-*-perl-*-
& eval 'exec perl -S $0 $argv:q'
        if 0;
#
# USAGE:
#
#    cflow.filter.pl <FILE> [<FILE> [<FILE> [...]]]
#
# EXAMPLES:
#
#    ### Print results to STDOUT for functions in multiple source files searched in multiple source files
#    cflow.filter.pl */*.[fc]*
#    cflow.filter.pl src/*.c -f '*/*.[fc]*'
#
#    ### Print results to STDOUT for functions in one source file searched in multiple source files
#    cflow.filter.pl src/eos_Interpolation.c -f '*/*.[fc]*'
#
#    ### Pretty print results with enscript for multiple source files
#    cflow.filter.pl */*.[fc]* -p


# Get command line options
use Getopt::Long;
$options={};
$Getopt::Long::ignorecase=0;
GetOptions($options,
           "--c_prototypes",
           "--extern_only",
           "--filepattern=s",
           "--help",
           "--include_guard",
           "--print",
           "--quiet",
           "--truncate:i",
           "--verbose+"
  );

use File::Basename;
use Data::Dumper;
use constant {
  _PRIVATE_ => -1,
  _PUBLIC_ => 1
};

my %h = ();
my ($rows, $cols) = (0, 0);
($rows, $cols) = split " ", `stty size` if defined $options->{truncate} and $options->{truncate} <= 0;
$cols = $options->{truncate} if defined $options->{truncate} and $options->{truncate};

print '$options = ', Dumper($options), '$rows = ', Dumper($rows), '$cols = ', Dumper($cols)
  if $options->{verbose} and ! $options->{quiet};

$cols -= 19;

open(STDOUT, "| enscript -h -F=Courier-Bold12 --ps-level=1 --download-font='Courier' --download-font='Courier-Bold12' --mark-wrapped-lines=arrow --media=Letter -fCourier7 -G")
  if $options->{print};

my @files = sort @ARGV;
@files = sort glob($options->{filepattern}) if $options->{filepattern};
print STDERR "files: @files\n" if $options->{verbose} and ! $options->{quiet};

my $c = 1;
my $original_parser = 0;

if ($original_parser) {

  my $cflow_cmd = "cflow -d 1 --brief --include=_";

  for my $f (sort @ARGV) {
    next if ! -r "$f";
    if (open(FH, "$cflow_cmd $f |")) {
      printf(STDERR "--- Parse %s (%d of %d)\n", $f, $c++, scalar(@ARGV));
      while (<FH>) {
        chomp;
        s/^\s+//;
        s/\s+\([A-Z]\):/:/;
        my @a=split(/[\s\(:>]+/);
        my $f_def = $a[$#a-1];
        warn("WARNING: $f_def != $f") if "$f_def" ne "$f";
        my $n = $a[$#a];
        unless ( -r "$f_def" and $n =~ /^\d+$/ ) {
          print STDERR "WARNING: skipping (\$f:\$n)=($f:$n)::$_\n";
          next;
        }
        print STDERR "  Searching for $a[0] (defined at ${f_def}:$n)\n" if ! $options->{quiet};
        m/<([^;]+) at /;        # extract C prototype
        $h{$f_def}{$a[0]}{c_prototype} = "$1;";

        $h{$f_def}{$a[0]}{line} = $n;
        @{$h{$f_def}{$a[0]}{where}} = split /\n+/,`grep -wl $a[0] @files | xargs -i sh -c "stripcmt {} | grep --label={} -Hwn $a[0]"`;
      }
      close FH;
    }
  }

}
else {

  my $cflow_cmd = "cflow -x -d 1 --brief --include=_";

  for my $f (sort @ARGV) {
    next if ! -r "$f";
    if (open(FH, "$cflow_cmd $f |")) {
      printf(STDERR "--- Parse %s (%d of %d)\n", $f, $c++, scalar(@ARGV));
      while (<FH>) {
        s/_EXTERN_C_(HEAD|TAIL)_//;
        next if ! /^[^\*]+ +\* +/; # skip all that are not function definitions
        chomp;
        s/^\s+//;
        my ($func, $junk, $f_def, $n, $proto_func_type, $proto_args) = split(/[\s:]+/, $_, 6);

        # hack to work around cflow parser bug that replaces "struct stat *p" function arguments with
        # "struct stat p"
        $proto = `stripcmt $f | awk 'BEGIN {ORS=" "} /${proto_func_type}[ \*]*${func}\\>/,/)/ {sub(/^[\t ]+/,"");sub(/\)[^\)]*/,")");print}'`;
        $proto =~ s/\s+$//;

        warn("WARNING: $f_def != $f") if "$f_def" ne "$f";
        unless ( -r "$f_def" and $n =~ /^\d+$/ ) {
          print STDERR "WARNING: skipping (\$f:\$n)=($f:$n)::$_\n";
          next;
        }
        print STDERR "  Searching for $func (defined at ${f_def}:$n)\n" if ! $options->{quiet};
        m/<([^;]+) at /;        # extract C prototype
        $h{$f_def}{$func}{c_prototype} = "${proto};";

        $h{$f_def}{$func}{line} = $n;
        @{$h{$f_def}{$func}{where}} = split /\n+/,`grep -wl $func @files | xargs -i sh -c "stripcmt {} | grep --label={} -Hwn $func"`;
      }
      close FH;
    }
  }

}

END {
  $Data::Dumper::Sortkeys=1;
  for my $f (keys %h) {
    for my $k (keys %{$h{$f}}) {
      $h{$f}{$k}{filecount}=scalar(grep !/$f/,@{$h{$f}{$k}{where}});
      $h{$f}{$k}{filecount}=1 if $options->{extern_only};
      if ($h{$f}{$k}{filecount} <= 0) {
        $h_onlyme_verbose{$f}{$k} = $h{$f}{$k};
        $h_onlyme{$f}{$k} = $h{$f}{$k}{where};
      }
      else {
        $h_notme_verbose{$f}{$k} = $h{$f}{$k};
        $h_notme{$f}{$k} = $h{$f}{$k}{where};
      }
    }
  }
  #print '%h = ', Dumper(\%h);

  $Data::Dumper::Terse = 1;

  do {
    print STDERR '%h_onlyme_verbose = ', Dumper(\%h_onlyme_verbose);
    print STDERR '%h_notme_verbose = ', Dumper(\%h_notme_verbose);
  } if $options->{verbose} and ! $options->{quiet};

  $Data::Dumper::Indent = 1;

  #print '%h = ', Dumper(\%h);

  if ($options->{c_prototypes}) {
    my $guard;
    my $guard_int;
    my $autogen = "Automatically generated by " . basename($0) . ", " . scalar(localtime(time));
    print <<"EOH";
      /*********************************************************************
      *
      * Filetype: (HEADER)
      *
      * $autogen
      *
      * Copyright -- see file named COPYRIGHTNOTICE
      * 
      *********************************************************************/
EOH

    for my $f (sort keys %h) {
      if ($options->{include_guard}) {
        $guard = uc(basename($f));
        $guard =~ s/\.[^\.]+$//;
        $guard =~ s/\.+$/_/g;
        $guard_int = "_${guard}_INTERNAL_PROTOTYPES";
        $guard .= "_PROTOTYPES";
        print "#ifndef $guard\n\n#define $guard\n\n";
      }

      print_proto($f, $h{$f}, "PUBLIC FUNCTION PROTOTYPES FOR " . basename($f), _PUBLIC_);

      if ($options->{include_guard}) {
        print "#ifdef ${guard_int}\n\n";
      }

      print_proto($f, $h{$f}, "PRIVATE FUNCTION PROTOTYPES FOR " . basename($f), _PRIVATE_);

      if ($options->{include_guard}) {
        print "#endif /* ${guard_int} */\n\n";
        print "#endif /* ${guard} */\n";
      }
    }
  }
  else {
    for my $f (sort keys %h) {
      print_where_output($h{$f}, "FUNCTION DEFINITION AND USAGE EXCLUSIVE TO $f", $cols, _PRIVATE_);

      print "\f";
      print_where_output($h{$f}, "FUNCTION USAGE IN $f AND ADDITIONAL FILE(S)", $cols, _PUBLIC_);
    }
  }
  close FH if $options->{print};
}

sub serialize_content {
  my ($_hash_ref, $cols) = @_;
  my $str = Dumper($_hash_ref);
  my @lines = split(/\n/, $str);
  if ($cols > 0) {
    $str = "";
    for my $s (@lines) {
      $s =~ s/\t/        /g;
      $s =~ s/^(.{$cols}).....*(.{15})$/$1 .. $2/;
      $str .= "$s\n";
    }
  }
  return $str;
}

sub print_where_output {
  my ($ref, $banner, $cols, $visibility) = @_;
  my $c = 0;
  my $str = "";
  for my $k (sort keys %{$ref}) {
    if ($visibility == _PRIVATE_ and $ref->{$k}->{filecount} <= 0) {
      $c++;
      $str .= "$k =>\n";
      $str .= serialize_content($ref->{$k}->{where}, $cols);
    }
    elsif ($visibility == _PUBLIC_ and $ref->{$k}->{filecount} > 0) {
      $c++;
      $str .= "$k =>\n";
      $str .= serialize_content($ref->{$k}->{where}, $cols);
    }
  }

  print "-"x(length($banner)),"\n$banner\n";
  print "-"x(length($banner)),"\n\n";
  print "$str\n";
}

sub print_proto {
  my ($file, $ref, $banner, $visibility) = @_;

  #print '$ref = ', Dumper($ref);

  my $c = 0;
  my $str = "";
  for my $k (sort keys %{$ref}) {
    #print STDERR "do (func,banner,visibility)=($k,'$banner',$visibility) :: ", $ref->{$k}->{c_prototype}, "\n";
    if ($visibility == _PRIVATE_ and $ref->{$k}->{filecount} <= 0) {
      $c++;
      $str .= $ref->{$k}->{c_prototype} . "\n";
    }
    elsif ($visibility == _PUBLIC_ and $ref->{$k}->{filecount} > 0) {
      $c++;
      $str .= $ref->{$k}->{c_prototype} . "\n";
    }
  }

  print "/*\n * ", "-"x(length(" $c $banner")),"\n * $c $banner\n";
  print " * ", "-"x(length(" $c $banner")),"\n */\n";
  print "$str\n";
}
