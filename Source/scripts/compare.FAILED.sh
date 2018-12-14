#! /bin/sh

loc=`dirname $0`
me=`basename $0`

# default values
usexemacs=0
alt_arch=
usefcmp=0
FCMP_OPT_IN=

# process command line options
while getopts a:cF:f:hx option ; do
  case "$option"
  in
    a)  alt_arch=$OPTARG;;
    c)  usefcmp=1;;
    F)  usefcmp=3;
        FCMP_OPT_IN=$OPTARG;;
    f)  usefcmp=2;
        FCMP_OPT_IN=$OPTARG;;
    x)  usexemacs=1;;
    h)  cat <<EOF >&2

Usage: ${me} [-h -f -x] [DIR [, ...]]
   -a <ARCH> define an ARCH directory other than that of the current platform
   -c        use fcmp instead of diff to compare output; the options used are those
             that are defined for the individual tests
   -F <OPTS> use fcmp instead of diff to compare output; the <OPTS> are used instead
             of those defined for the individual tests
   -f <OPTS> use fcmp instead of diff to compare output; the <OPTS> are used in
             addition to those defined for the individual tests
   -h        display this help
   -x        run 'xemacs -diff' instead of regular diff
  DIR is used to identify which compiler subdirectories to compare.
      (Default: pgi)

EOF
        exit 2
        ;;
  esac
done

shiftcount=`expr $OPTIND - 1`
shift $shiftcount

if [ $# -gt 0 ]; then
  compilers=
else
  compilers='pgi'
fi

while [ $# -gt 0 ];
  do
  compilers="$compilers $1"
  shift
done

new_files=

if [ -n "$alt_arch" ]; then
  arch=$alt_arch
else
  arch=`config.guess.wrapper`
fi

for c in $compilers;
  do

  PASSFAIL=`find tests/$arch/$c -name pass_fail_output_file.txt`
  TESTS=`grep FAILED $PASSFAIL | awk '{printf("%s ",$2)}'`
  for f in ${TESTS}
    do
    d=`grep FAILED $PASSFAIL | grep $f | grep 'baseline exception' | awk '{print $NF}'`
    if [ "x$d" != "x" ]; then
	e=`find $d -name $f | grep -w data`
    fi
    if [ "x$e" = "x" ]; then
	file1=`find tests -name $f | grep baseline`
    else
	file1=$e
    fi
    file2=`find tests -name $f | grep $c | grep -v -w data`
    new_files="$new_files $file2"

    STDOUT_FILTER='grep -v fcmp_ignore'

    if [ $usexemacs -ne 0 ]; then
      cmd="xemacs -diff $file1 $file2"
    elif [ $usefcmp -ne 0 ]; then
      FCMP_OPT="${FCMP_OPT_IN}"
      if [ $usefcmp -eq 1 ]; then
        FCMP_OPT=`grep FAILED $PASSFAIL | grep $f | awk -F: '{sub(/\)/,"");printf("%s",$3)}'`
      elif [ $usefcmp -eq 2 ]; then
        _OPT=`grep FAILED $PASSFAIL | grep $f | awk -F: '{sub(")","");printf("%s",$3)}'`
        FCMP_OPT="${FCMP_OPT_IN} ${_OPT}"
      fi
      cmd="${loc}/fcmp ${FCMP_OPT} $file1 $file2"
      STDOUT_FILTER=cat
    else
      echo
      echo '================================================================================================'
      echo
      cmd="diff $file1 $file2"
    fi
    echo '+' "$cmd"
    $cmd | ${STDOUT_FILTER}
  done

done

echo
echo '================================================================================================'
echo
echo FILES COMPARED TO BASELINE:
printf "%s\n" $new_files
