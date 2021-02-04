eval '(exit $?0)' && eval 'exec perl -w -S $0 ${1+"$@"}' #-*-perl-*-
& eval 'exec perl -w -S $0 $argv:q'
        if 0;
##################################################################
#
#  Copyright -- see file named COPYRIGHTNOTICE
#
##################################################################

BEGIN {
  # Authorized user list
  %userList = (
    120181 => "davidp"
    );

  # Verify current user is allowed use this script
  $user = `whoami`;
  chomp($user);
  @userZList = keys(%userList);
  @userMList = values(%userList);
  if (grep /$user/, @userZList or
      grep /$user/, @userMList)
  {
   $user = $userList{$user} if (grep /$user/, @userZList); # change Z no. to moniker
  }
  else
  {
    print "INVALID USER: $user\n";
    print "valid lists: @userZList\n",
          "             @userMList\n";
    exit 1;
  }
}

# specify product, default tag, and package identifiers
$prod_name = "EOSPAC";
$default_cvs_tag = "v6";
$default_version_note = "Unreleased";
$package_name = "eos";

use File::Basename;

# Get command line options
use Getopt::Long;
$options={};
$Getopt::Long::ignorecase=0;
GetOptions($options,
	   "--no-create-file",
	   "--verbose+"
	  );

use Data::Dumper;
$Data::Dumper::Terse = 1;
$level = ($options->{verbose}) ? $options->{verbose} : 0;
$Data::Dumper::Pad = "debug" . $level . ":";

print "debug" . $options->{verbose} . ":" . '%userList = ' . "\n", Dumper(\%userList) if $options->{verbose};
print "debug" . $options->{verbose} . ":" . '$user = ', "$user\n" if $options->{verbose};
print "debug" . $options->{verbose} . ":" . '@userZList = ' . "\n", Dumper(\@userZList) if $options->{verbose};
print "debug" . $options->{verbose} . ":" . '@userMList = ' . "\n", Dumper(\@userMList) if $options->{verbose};

# specify CPP macro name used by package to define the platform-dependent C function
# calling convention (i.e., __stdcall, __cdecl, ...)
${func_inter} = 'FUNC_INTER';

# specify directory on which to gather file version info
$pwd=`pwd`;
chomp($pwd);

#............................................................................
#....................assign default variables................................
#............................................................................
$current_working_directory_command = 'pwd';
$current_working_directory = `$current_working_directory_command`;
$current_working_directory =~ s/\s*$//;
if ( $current_working_directory !~ /\S/ )
  {
    $ierr = -1;
    &print_error( "Cannot find current working directory", $ierr );
    exit( $ierr );
  }

$computer_name = `uname -n`;
chop( $computer_name );
#..................................
#...parse flags then main option...
#..................................
$output_file = 'eos_version.c';
$output_dir = "./";
print "debug" . $options->{verbose} . ":" . $output_dir . "\n" if $options->{verbose};
$create_file = ($options->{"no-create-file"}) ? "false" : "true";
$output_file_full = "$output_dir/$output_file";
$output_file_full =~ s/\/+/\//g;

my $cmd = "git log --pretty='{key=>\"%H\", author=>\"%an\", date=>\"%ai\", tags=>\"%d\"}'";

print "debug" . $options->{verbose} . ":" . "\$cmd='$cmd'\n" if $options->{verbose};
@gitinfo = grep /HEAD/, `$cmd`;
chomp @gitinfo;
print "debug" . $options->{verbose} . ":" . '@gitinfo = ' . "\n", Dumper(\@gitinfo) if $options->{verbose};

$gitinfo = eval $gitinfo[0];
print "debug" . $options->{verbose} . ":" . '$gitinfo = ' . "\n", Dumper($gitinfo) if $options->{verbose};

#..............
#...get date...
#..............
$date = `date --date='$gitinfo->{date}'`;
chomp ( $date );

#......................................................................
#...parse the lines of the log into the variables and the directives...
#......................................................................

# find the current cvstag:
if ($gitinfo->{tags} =~ m/tag:\s*([\w\-]+)\s*\)/i) {
  $cvs_tag = $1;
  print "debug" . $options->{verbose} . ":" . 'tag $cvs_tag = ', "$cvs_tag\n" if $options->{verbose};
  $version_note = "";
} elsif ($gitinfo->{tags} =~ m/\s+\->\s+([^,\s]+)[,\s]*/i) {
  $cvs_tag = basename($1);
  print "debug" . $options->{verbose} . ":" . 'branch $cvs_tag = ', "$cvs_tag\n" if $options->{verbose};
  $version_note = "";
} else {
  $cvs_tag = $default_cvs_tag;
  print "debug" . $options->{verbose} . ":" . 'default $cvs_tag = ', "$cvs_tag\n" if $options->{verbose};
  $version_note = $default_version_note;
}
$cvs_tag_dots = $cvs_tag;
$cvs_tag =~ s/\-/\_/g;
$cvs_tag_dots =~ s/\-/\./g;

$version = ((${version_note})?"${version_note}_":"") . $gitinfo->{key};

${special_func_name} = "${package_name}_version_name_${prod_name}_${cvs_tag}_${version}";

my $defprefix = uc(${package_name});
$section_output{"code"} = <<EOS;
#include "eos_types.h"
#include "eos_wrappers.h"
#include <string.h>

#ifdef __cplusplus
extern "C"
{
#endif

#ifdef _${defprefix}_INCLUDE_SPECIAL_FUNCTION
void ${special_func_name}()
{
  return;
}
#endif

static EOS_CHAR *${package_name}_version_info_msg \= "${prod_name} ${cvs_tag_dots} ${version}";
EOS_INTEGER _${package_name}_get_version_length()
{
  /* include space for the '\\0' character */
  return(strlen (eos_version_info_msg) + 1);
}
void ${func_inter} ${package_name}_GetVersionLength(EOS_INTEGER *length)
{
  *length = _${package_name}_get_version_length();
#ifdef USE_SPLUNK
  if (! _eos_splunk_get_acc_disable(EOS_SPLUNK_ACCUMULATOR_EOS_GETVERSIONLENGTH)) {
    EOS_CHAR val[800];
    sprintf(val, "length=%d", *length);
    _eos_splunk(EOS_SPLUNK_ACCUMULATOR_EOS_GETVERSIONLENGTH, val);
  }
#endif
}

EOS_CHAR* _${package_name}_get_version()
{
  return(${package_name}_version_info_msg);
}
void ${func_inter} ${package_name}_GetVersion(EOS_CHAR *version)
{
  strcpy(version, _${package_name}_get_version());
#ifdef USE_SPLUNK
  if (! _eos_splunk_get_acc_disable(EOS_SPLUNK_ACCUMULATOR_EOS_GETVERSION)) {
    _eos_splunk(EOS_SPLUNK_ACCUMULATOR_EOS_GETVERSION, version);
  }
#endif
  return;
}

#ifdef __cplusplus
}
#endif
EOS

#....................................
#...DONE: assign default variables...
#....................................
#...prepare output string for printing...
#........................................
$total_output = "";
#..................
#...print header...
#..................
$toolpath = $0;
$toolpath =~ s/^.*eospac/eospac/;
$toolpath_dots = '.' x (length($toolpath)+2);
$total_output .= <<EOS;
/*
 *............................................${toolpath_dots}
 *  $output_file_full
 *    Version date: $date
 *    This file is automatically generated by ${toolpath}
EOS
foreach $key ( sort keys %predefined_options_all )
  {
    $total_output .= " *     predefined $key: $predefined_options_all{$key}\n";
  }
$total_output .= <<EOS;
 *............................................${toolpath_dots}
 */
EOS
#..............................................
#...format variable output into nice columns...
#..............................................
foreach $section ( sort keys %section_output )
  {
    if ( defined( $section_output{$section} ) ) {
      $section_results .= "$section_output{$section}";
    }
    $section_results .= "\n";
  }
$total_output .= $section_results;
#...............................

#................................
#...if printing to file, do so...
#................................
if ( $create_file eq "true" )
  {
    print "    Creating output file [$output_file_full]\n";
    print "    Variables:\n";
    foreach $command_line_variable ( sort keys %command_line_variables ) {
      print "     $command_line_variable$command_line_variables{$command_line_variable}\n";
    }
    print "\n";
    if ( ! -d $output_dir ) {
      $output = `mkdir -p $output_dir 2>&1`;
      if ( $output =~ /\S/ ) {
	$ierr = -1;
	&print_error( "Cannot create output directory [$output_dir]",
		      $output, $ierr );
	exit( $ierr );
      }
    }
    if ( ! open ( FILE, ">$output_file_full" ) ) {
      $ierr = -1;
      &print_error( "Cannot open output file [$output_file_full]",
		    "Perhaps you need to make the directory?", $ierr );
      exit( $ierr );
    }
    print FILE $total_output;
    close FILE;
  }
#.............................................................
#...otherwise, print to screen and print info about options...
#.............................................................
else
  {
    print $total_output;
  }
#.........
#...end...
#.........
exit( 0 );
#.........................................................................
