##################################################################
#
#  Copyright -- see file named COPYRIGHTNOTICE
#
##################################################################
eval 'exec perl -w -S $0 ${1+"$@"}'
  if 0;

my(
   @file_lines,			# lines of the input file
   @command_line_args,		# command line args
   $computer_name,		# COMPUTER_NAME set to this
   $continuation_line,		# the line continued by a "\" on previous line
   $create_file,		# whether to print output to file or screen
   $current_working_directory,	# DIR_TOP_SRCDIR set to this
   $current_working_directory_command, # command to get the current working dir
   $cvs_chk,			# check if cvs info is available
   $cvs_cmd,			# cvs command string including necessary cvs options
   $date,			# todays date
   $done,			# whether done with a task
   $help,			# "true" if help is the option, "false" otherwise
   $option,			# current predefined option
   $options,			# set of all defined options
   $output_file,		# relative name of output file
   $output_file_full,		# $output_dir/$output_file
   $output_dir,			# directory to make.inc
   $section,			# section output identifier
   %section_output,			# {section} = $value (printed to output file
   $total_output,		# output to be printed to output file
   %userList,			# authorized user list for this script
   $variable,			# variable for section -  input file
   %variable_assign,			# assignment of the variable (eg =,:=,...)
   %variable_group,			# {variable} = "$requirement=$value\n(another pair)\n..."
   $version,			# version number for further output
   $verlength,			# version name length
   $statusDir,			# name of file to contain version info
   $sticky_tag			# CVS tag associated with sandbox
  );

# specify product, default tag, and package identifiers
$prod_name = "EOSPAC";
$default_cvs_tag = "v6";
$default_version_note = "Unreleased";
$package_name = "eos";

$forced = 1;

# specify CPP macro name used by package to define the platform-dependent C function
# calling convention (i.e., __stdcall, __cdecl, ...)
${func_inter} = 'FUNC_INTER';

# specify directory on which to gather file version info
$pwd=`pwd`;
chomp($pwd);
$statusDir = ($pwd !~ m|src/?$|) ? "src" : ".";

# define default CVS location using $ENV{WINCVSLOCATION} if it's defined
#$cvsLoc = "";
#if (defined $ENV{OS} && defined $ENV{PROGRAMFILES} && $ENV{OS} =~ /Windows/i) {
#  $cvsLoc = $ENV{WINCVSLOCATION} if (defined $ENV{WINCVSLOCATION});
#  ${cvsLoc} =~ s/[\/\s]*$/\//;
#}

# Authorized user list
%userList = (
	     120181 => "davidp"
	    );

# Verify current user is allowed use this script
$user = `whoami`;
chomp($user);
my $userZList = '|' . join('|',keys(%userList)) . '|';
my $userMList = '|' . join('|',keys(%userList)) . '|';
if ("\|$user\|" =~ $userZList or
    "\|$user\|" =~ $userMList)
  {
    $user = $userList{$user} if ("\|$user\|" =~ $userZList); # change Z no. to moniker
  }
else
  {
    print "INVALID USER: $user\n";
    print "valid lists: $userZList\n",
          "             $userMList\n";
    exit;
  }

$cvs_cmd = "cvs -d :ext:tf.lanl.gov:/cvsroot/eospac6";

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
#print $output_dir;
$help = "false";
$create_file = "true";
$done = "false";
$output_file_full = "$output_dir/$output_file";
$output_file_full =~ s/\/+/\//g;
#..............
#...get date...
#..............
$date = `date '+%m-%d-%Y %r %Z'`;
chop ( $date );

#.......................................................................
#...parse the lines of the file into the variables and the directives...
#.......................................................................
$done = "false";
undef( %variable_group );
while ( $done eq "false" )
  {
    #.......................
    #...stop at last line...
    #.......................

    #....................
    #...CVS_ACCESSIBLE...
    #....................
    $variable = "CVS_ACCESSIBLE";
    #$cvs_chk = "false";
    if ( -d "CVS" ) {
      $output = `$cvs_cmd status -l $statusDir | grep -v 'Status: Up-to-date' 2>&1`;
      if ( ! defined( $output ) ) {
	$output = "";
      }
      die "\nCVS repository not available ... quitting.\n\n"
	if ( $output =~ /no repository/m );

      $sticky_tag = $1 if ( $output =~ /\s+Sticky Tag:\s+(\S+)/m);
      $sticky_tag = "" if ($sticky_tag =~ /none/);

      if ( $output !~ /\s+Status: (.+)/m || $forced) {
	$output = `$cvs_cmd status -l $statusDir`;
	$cvs_chk = "true";
      } else {
	$output = `$cvs_cmd status -l $statusDir | grep 'Status:' | grep -v 'Up-to-date' | grep 'Status:' 2>&1`;
	print "\nFile(s) not Up-to-date! Take action according to the following:\n",'-'x63,"\n$output\n\n";
	exit;
      }
      #print "OUTPUT:\n$output\n\n\$cvs_chk = $cvs_chk\n\n";
    }
    #....................................................................
    #....Start Section output processing
    #....................................................................

    #.....................................................................
    #...VERSION_CVS                                                    ...
    #.....................................................................
    die "\nCVS repository not available ... quitting.\n\n" if ( $cvs_chk ne "true" );

    $section = "05";

    # find the latest checkin date in the CVS repository to use as revision number:
    my $rtag = ($sticky_tag) ? "-r$sticky_tag" : "";
    my $cmd = "$cvs_cmd rlog $rtag Source 2>&1 | grep -e '^date:' 2>&1 | sort 2>&1";
    $revDates = `$cmd`;
    @revDates = split("\n",$revDates);
    ($latestRevDate,@junk)=split(/\;/,$revDates[$#revDates]);
    ($junk,$latestRevDate,$latestRevTime)=split(/\s+/,$latestRevDate);
    ($yr,$mon,$day) = split(/\//,$latestRevDate);
    ($hr,$min,$sec) = split(/\:/,$latestRevTime);

    # time stamp data must be converted to localtime values from the reported GMT values
    use Time::Local;
    ($sec,$min,$hr,$day,$mon,$yr) = localtime(timegm($sec,$min,$hr,$day-1,$mon-1,$yr-1900));
    ($day,$mon,$yr) = ($day+1,$mon+1,$yr+1900);

    # find the current cvstag:
    $cvsTags = `$cvs_cmd status -v 2>&1 | grep -e 'Sticky Tag:' 2>&1`;
    @cvsTags = split("\n",$cvsTags);
    $cvsTags[0] =~ m/Sticky Tag:\s+([\w\-]+)\s+/i;

    if ($cvsTags[0] =~ m/Sticky Tag:\s+([\w\-]+)\s+/i) {
      $cvs_tag = $1;
      $version_note = "";
    } else {
      $cvs_tag = $default_cvs_tag;
      $version_note = $default_version_note;
    }
    $cvs_tag_dots = $cvs_tag;
    $cvs_tag =~ s/\-/\_/g;
    $cvs_tag_dots =~ s/\-/\./g;

    $section_output{$section} = sprintf("r%04d%02d%02d%02d%02d%02d",${yr},${mon},${day},${hr},${min},${sec}) .
	                        ((${version_note})?"_${version_note}":"");
    $version = $section_output{$section};
    $verlength = length("${prod_name} ${cvs_tag} ${version}") + 1;

    ${special_func_name} = "${package_name}_version_name_${prod_name}_${cvs_tag}_${version}";

    my $defprefix = uc(${package_name});
    $section_output{$section} = <<EOS;
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
void ${func_inter} ${package_name}_GetVersionLength(EOS_INTEGER *length)
{
  /* include space for the '\\0' character */
  *length = strlen(${package_name}_version_info_msg) + 1;
}

void ${func_inter} ${package_name}_GetVersion(EOS_CHAR *version)
{
  strcpy(version, ${package_name}_version_info_msg);
  return;
}

#ifdef __cplusplus
}
#endif
EOS

    $done = "true";
  }

#....................................
#...DONE: assign default variables...
#....................................
#...prepare output string for printing...
#........................................
$total_output = "";
#..................
#...print header...
#..................
$total_output .= <<EOS;
/*.......................................................
 *  $output_file_full
 *    Creation date: $date
 *    This file is automatically generated by $0
EOS
foreach $key ( sort keys %predefined_options_all )
  {
    $total_output .= " *     predefined $key: $predefined_options_all{$key}\n";
  }
$total_output .= <<EOS;
 *.......................................................*/
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
    #print "    Options: [$options]\n";
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


