#! /bin/sh
(shopt -s igncr) 2>/dev/null && eval 'shopt -s igncr';#

# Some useful color codes, see end of file for more.
#
# NO_COLOR        \033[0m
# WHITE           \033[137m
# BLACK           \033[030m
# BLUE            \033[034m
# LIGHT_BLUE      \033[1;34m
# GREEN           \033[032m
# LIGHT_GREEN     \033[1;32m
# CYAN            \033[036m
# LIGHT_CYAN      \033[136m
# RED             \033[031m
# LIGHT_RED       \033[1;31m
# PURPLE          \033[035m
# LIGHT_PURPLE    \033[1;35m
# BROWN           \033[033m
# YELLOW          \033[1;33m
# GRAY            \033[030m
# LIGHT_GRAY      \033[037m
# 
# NORM            \033[00m
# BACKGROUND      \033[07m
# BRIGHTEN        \033[01m
# UNDERLINE       \033[04m
# BLINK           \033[05m

# The following PS4 definition will be used when the '-x' sh option is used (i.e., set -x).
# The '+' count will increase with each nested sub shell. (colorization is optional)
# PS4='+(${BASH_SOURCE}:${LINENO}):${FUNCNAME[0]:+${FUNCNAME[0]}()}: }'
PS4='+\033[035m${BASH_SOURCE}\033[0m:\033[032m${LINENO}\033[0m:\033[1;31m${FUNCNAME[0]:+${FUNCNAME[0]}()}\033[00m:'

loc=`dirname $0`
me=`basename $0`

# default values
useemacs=0
usemeld=0
ALL_arch=0
alt_arch=
usefcmp=0
DEBUG_SHORT=0
DEBUG=0
FCMP_OPT_IN=
ONLY_TABLESLOADED=0
LIST_TABLESLOADED_TABLES_THAT_FAIL=0
ONLY_STDOUT=0
LIST_DIFF_TO_PATCH_CMDS=0
LIST_FAILURES=0
LIST_FAILURE_FILES=0
LIST_TEST_CODE_NAMES=0
grep_str='^FAILED'
cgw_opts=
TEST_CODE_NAMES_LIST=
UPDATE=0
SHORT=0
UNFILTERED=0

# process command line options
while getopts Aa:CcDdEeF:f:hIiLlmnpSsTUuWwx option;
do

    case "$option"
    in

        A)  ALL_arch=1;;
        a)  alt_arch=$OPTARG;;
        C)  LIST_TEST_CODE_NAMES=1;;
        c)  usefcmp=1;;
        D)  DEBUG_SHORT=1;;
        d)  DEBUG=1;;
        E)  grep_str='^excluded';
            LIST_FAILURE_FILES=1;;
        e)  grep_str='^excluded';
            LIST_FAILURES=1;;
        F)  usefcmp=3;
            FCMP_OPT_IN=$OPTARG;;
        f)  usefcmp=2;
            FCMP_OPT_IN=$OPTARG;;
        I)  grep_str='^IGNORED';
            LIST_FAILURE_FILES=1;;
        i)  grep_str='^IGNORED';
            LIST_FAILURES=1;;
        L)  LIST_FAILURE_FILES=1;;
        l)  LIST_FAILURES=1;;
        m)  usemeld=1;;
        n)  cgw_opts="-n";;
        p)  LIST_DIFF_TO_PATCH_CMDS=1;;
        S)  ONLY_STDOUT=1;;
        s)  SHORT=1;;
        T)  ONLY_TABLESLOADED=1;;
        U)  grep_str='^\(PASSED\|FAILED\|WARNING\)';
            UNFILTERED=1;;
        u)  UPDATE=1;;
        W)  grep_str='^WARNING';
            LIST_FAILURE_FILES=1;;
        w)  grep_str='^WARNING';
            LIST_FAILURES=1;;
        x)  useemacs=1;;
        h)  cat <<EOF >&2

Usage: ${me} [options] [DIR [, ...]]
   -A        find all tests/ARCH directory(ies) and loop over them
   -a <ARCH> define an ARCH directory other than that of the current platform
   -C        list the test code names (i.e., make targets) that are failures
   -c        use fcmp instead of diff to compare output; the options used are those
             that are defined for the individual tests
   -D        display an abbreviated series of commands that would be executed; then quit
   -d        display an extended series of commands that would be executed; then quit
   -E        list the files that are excluded
   -e        list the excluded summary
   -F <OPTS> use fcmp instead of diff to compare output; the <OPTS> are used instead
             of those defined for the individual tests
   -f <OPTS> use fcmp instead of diff to compare output; the <OPTS> are used in
             addition to those defined for the individual tests
   -h        display this help
   -I        list the files that are ignored
   -i        list the ignored summary
   -L        list the files that are failures
   -l        list the failures summary
   -m        use 'meld' instead of regular diff (NOTE: meld is very slow for large files)
   -n        pass through the -n option to config.guess.wrapper, so its result
             excludes the AVX string for the current platform
             (ie., x86_64-rhel7-linux-gnu instead of x86_64-avx512f-rhel7-linux-gnu)
   -p        list the command(s) that may be used to create exception (i.e., patch) files;
             overrides options: -f, -F, -x
   -S        compare only *.stdout files
   -s        short output limited to one line per test file
   -T        compare only *.TablesLoaded.dat files
   -U        list all (unfiltered) but ignored or excluded tests, regardless of PASS/FAIL/WARNING
   -u        update baseline files
   -W        list the files that are warnings
   -w        list the warnings summary
   -x        run 'emacs -diff' instead of regular diff
  DIR is used to identify which compiler subdirectories to compare.
      (Default: pgi)

EOF
            exit 2
            ;;

    esac

done

shiftcount=`expr $OPTIND - 1`
shift $shiftcount

if [ -n "$alt_arch" ]; then

    archlist=$alt_arch

else

    archlist=`${loc}/config.guess.wrapper ${cgw_opts}`

fi

if [ $ALL_arch -ne 0 ]; then

    archlist=`ls -1 tests/*/.distclean | xargs -i sh -c 'dirname {}' | xargs -i sh -c 'basename {}'`

fi

if [ ${LIST_DIFF_TO_PATCH_CMDS} -eq 0 -a ${SHORT} -eq 0 ]; then

    echo '================================================================================================'

fi

for arch in ${archlist}; do

    # echo '------------------------------------------------------------------------------------------------------------'
    if [ ${SHORT} -eq 0 ]; then
        echo; echo '-----' ${arch}
    fi

    if [ $# -gt 0 ]; then
        compilers=
    else
        #compilers='pgi'
        compilers=`find tests/${arch} -name pass_fail_output_file.txt | sed -e "s:tests/${arch}/::;s:/pass_fail_output_file.txt::" | sort -u`
        if [ ${DEBUG} -ne 0 ]; then
            ( set -x; : compilers = ${compilers})
        fi
    fi

    while [ $# -gt 0 ];
    do

        compilers="$compilers $1"
        shift

    done

    if [ ${DEBUG_SHORT} -ne 0 -o ${DEBUG} -ne 0 ]; then
        echo
    fi

    new_files=

    if [ ${LIST_DIFF_TO_PATCH_CMDS} -ne 0 ]; then

        echo
        echo 'Possible commands to create test exception patches (listed with increasing ${arch} and compiler-specificity)'
        echo '------------------------------------------------------------------------------------------------------------'

    fi

    _DO_COMPILER_HEADER_=0

    for c in $compilers;
    do

        if [ ${LIST_DIFF_TO_PATCH_CMDS} -ne 0 ]; then
            _DO_COMPILER_HEADER_=1
        fi

        PASSFAIL=`find tests/$arch/$c -name pass_fail_output_file.txt`
        if   [ ${ONLY_STDOUT} -ne 0 ]; then
            TESTS=`grep $grep_str $PASSFAIL | grep '\.stdout' | awk '{printf("%s ",$2)}'`
        elif [ ${ONLY_TABLESLOADED} -ne 0 ]; then
            TESTS=`grep $grep_str $PASSFAIL | grep '\.TablesLoaded\.dat' | awk '{printf("%s ",$2)}'`
        else
            TESTS=`grep $grep_str $PASSFAIL | awk '{printf("%s ",$2)}'`
        fi

        if [ ${LIST_FAILURES} -ne 0 ]; then
            more_grep_str=`echo ${TESTS} | sed 's/  */ -e /g'`
            if [ -n "${more_grep_str}" ]; then more_grep_str='-e '${more_grep_str}; else more_grep_str='-e .'; fi
            if [ ${SHORT} -eq 0 ]; then
                echo
                echo '  ***' $c
                echo
                grep $grep_str $PASSFAIL | grep $more_grep_str | cat -n
            else
                grep $grep_str $PASSFAIL | grep $more_grep_str
            fi
        fi

        if [ ${LIST_FAILURE_FILES} -ne 0 ]; then
            more_grep_str=`echo ${TESTS} | sed 's/  */ -e /g'`
            if [ -n "${more_grep_str}" ]; then more_grep_str='-e '${more_grep_str}; else more_grep_str='-e .'; fi
            if [ ${SHORT} -eq 0 ]; then
                echo
                echo '  ***' $c
                echo
                grep $grep_str $PASSFAIL | grep $more_grep_str | awk -v d="tests/$arch/$c/" '{print d $2}' | cat -n
            else
                grep $grep_str $PASSFAIL | grep $more_grep_str | awk -v d="tests/$arch/$c/" '{print d $2}'
            fi
        fi

        if [ ${usefcmp} -ne 0 ]; then
            echo
            echo '  ***' $c
            echo
        fi

        if [ ${LIST_TEST_CODE_NAMES} -ne 0 ]; then
            if [ ${SHORT} -eq 0 ]; then
                echo
                echo $c
            fi
            FILES=`grep $grep_str $PASSFAIL | awk '{print $2}'`
            ARGS=`echo $FILES | sed -e 's/  */ -o -name /g'`
            if [ ${SHORT} -eq 0 ]; then
                printf "     "
            fi
            echo `find tests/$arch/$c -name $ARGS | awk -F\/ '{print $NF}' | sed -e 's/\.stdout//;s/\.TablesLoaded\.dat//' | sort -u`
        fi

        if [ ${LIST_FAILURES} -ne 0 -o ${LIST_FAILURE_FILES} -ne 0 -o ${LIST_TEST_CODE_NAMES} -ne 0 ]; then
            continue
        fi

        LENGTH=0
        for f in ${TESTS}
        do
            n=`basename $f | wc -c`
            if [ $n -gt ${LENGTH} ]; then
                LENGTH=$n
            fi
        done

        CNT=0
        for f in ${TESTS}
        do

            CNT=`expr ${CNT} + 1`
            d=`grep $grep_str $PASSFAIL | grep $f | grep 'baseline exception' | awk '{print $NF}'`

            if [ "x$d" != "x" ]; then

                e=`find $d -name $f | grep -w data`

            else

                e=""

            fi

            if [ "x$e" = "x" ]; then

                file1=`find tests -name $f | grep baseline`

            else

                file1=$e

            fi

            if [ ${DEBUG} -ne 0 ]; then
                ( set -x; find tests -name $f | grep $c | grep -v -w data; : )
            fi

            file2=`find tests -name $f | grep $c | grep -v -w data`
            new_files="$new_files $file2"

            STDOUT_FILTER='grep -v fcmp_ignore'

            if [ ${LIST_DIFF_TO_PATCH_CMDS} -ne 0 ]; then

                if [ ${_DO_COMPILER_HEADER_} -ne 0 ]; then
                    echo
                    echo $c
                fi
                _DO_COMPILER_HEADER_=0

                if [ ${DEBUG} -ne 0 ]; then
                    echo file2="'$file1'"
                    echo file2="'$file2'"
                fi

                ARCHES=`( ${loc}/config.guess; ${loc}/config.guess.wrapper ${cgw_opts} ) | uniq`
                for a in ${ARCHES}
                do
                    _DO_ARCH_HEADER_=1
                    for d in `echo ${c} | tr '/' ' '`
                    do
                        if [ ${_DO_ARCH_HEADER_} -ne 0 ]; then
                            echo "  $a"
                        fi
                        echo "     diff $file1 $file2 > tests/data/${a}/${d}/${f}.ndiff"
                        _DO_ARCH_HEADER_=0
                    done
                done

                if [ ${DEBUG} -ne 0 ]; then
                    echo
                fi

            elif [ $usemeld -ne 0 ]; then

                cmd="meld $file1 $file2"

            elif [ $useemacs -ne 0 ]; then

                cmd="emacs -diff $file1 $file2"

            elif [ $UPDATE -ne 0 ]; then

                cmd="cp -p $file2 $file1"

            elif [ $usefcmp -ne 0 ]; then

                FCMP_OPT="${FCMP_OPT_IN}"

                if [ $usefcmp -eq 1 ]; then

                    FCMP_OPT=`grep $grep_str $PASSFAIL | grep $f | awk -F: '{sub(/\).*/,"");printf("%s",$3)}'`

                elif [ $usefcmp -eq 2 ]; then

                    _OPT=`grep $grep_str $PASSFAIL | grep $f | awk -F: '{sub(/\).*/,"");printf("%s",$3)}'`
                    FCMP_OPT="${_OPT} ${FCMP_OPT_IN}"

                fi

                cmd="${loc}/fcmp ${FCMP_OPT} $file1 $file2"
                STDOUT_FILTER=cat

            else

                if [ ${DEBUG_SHORT} -eq 0 -a ${DEBUG} -eq 0 -a ${SHORT} -eq 0 ]; then

                    echo
                    echo '================================================================================================'
                    echo

                fi

                cmd="diff $file1 $file2"

            fi

            if [ ${LIST_DIFF_TO_PATCH_CMDS} -eq 0 ]; then

                if [ ${DEBUG_SHORT} -ne 0 ]; then
                    echo "$cmd"
                elif [ ${SHORT} -ne 0 ]; then
                    : -s, do nothing here
                else
                    echo '+' "$cmd"
                fi

                if [ ${DEBUG_SHORT} -eq 0 -a ${DEBUG} -eq 0 ]; then

                    if [ ${SHORT} -eq 0 ]; then
                        $cmd | ${STDOUT_FILTER}
                        echo ---
                    else
                        l=${LENGTH}
                        printf "%-*s: %s\n" ${LENGTH} `basename $file1` "`$cmd | grep -v '^ *$' | sed 's/\n/; /g' | ${STDOUT_FILTER}`"
                    fi

                fi

            fi

        done

    done

    if [ ${SHORT} -eq 0 ]; then
        echo
    fi

done

if [ ${DEBUG_SHORT} -ne 0 -o ${DEBUG} -ne 0 ]; then
    exit
fi

if [ ${LIST_DIFF_TO_PATCH_CMDS} -eq 0 -a ${SHORT} -eq 0 ]; then

    echo '================================================================================================'
    echo
    echo FILES COMPARED TO BASELINE:
    printf "%s\n" $new_files

fi
