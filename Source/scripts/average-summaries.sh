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

#
# This script is used to calculate statistics of test execution times for available
# timing data.
#

loc=`dirname $0`
me=`basename $0`

ALL_arch=0
alltests=0

# process command line options
while getopts Aah option;
do

  case "$option"
  in

    A)  ALL_arch=1;;
    a)  alltests=1;;
    h)  cat <<EOF >&2

Usage: ${me} [options]
   -A        Assess tests for all discovered architectures
   -a        Assess all discovered tests
   -h        display this help

EOF
        exit 2
        ;;

  esac

done

shiftcount=`expr $OPTIND - 1`
shift $shiftcount

COMPILERLIST="ibm gcc intel pgi absoft pathscale nagfor lahey f90 g95 xlf90 ppu-gfortran"
ARCHLIST='x86 power8 power9'
TESTLIST=test_artf34116.test_times_portable.txt

STARTDIR=`pwd`
echo

if [ ${alltests} -ne 0 ]; then
    cd ${loc}/../tests
    TESTLIST=`echo test*.[cf]* | perl -ne 'use File::Basename;s/\s+/\n/g;@f=split /\n/;for $f (@f){$f=basename $f;@a=split /\./,$f;pop @a;print join(".", @a), ".test_times_portable.txt "}'`
    cd ${STARTDIR}
fi

if [ ${ALL_arch} -ne 0 ]; then
    cd ${loc}/..
    ARCHLIST=`find tests -maxdepth 2 -name .distclean | xargs dirname | xargs basename`
    cd ${STARTDIR}
fi

#
# Build input file for parallel script
#
rm -rf *.parallel.input.txt *.parallel.output.dir
OUTDIR=${me}.parallel.output.dir
mkdir ${OUTDIR}

for a in ${ARCHLIST}; do

    PARALLELINPUT=${OUTDIR}/${me}.${a}.parallel.input.txt

    for c in ${COMPILERLIST}; do

        d=`ls tests/${a}*/${c}* 2>/dev/null`
        if [ -n "${d}" ]; then

            i=`find ${loc}/../lib/${a}*/${c}* -name compiler_info.txt`
            grep CPUFLAGS ${i} | sed 's/^[^:]*://' | sort -u > ${OUTDIR}/CPUFLAGS.${a}.${c}.txt

            j=0

            for t in ${TESTLIST}; do
                j=`expr $j + 1`

                printf "\rBuilding parallel script for %s %s: %5d" ${a} ${c} ${j}
                jpad=`printf "%05d" ${j}`
                OUTPUT=${OUTDIR}/${jpad}.${a}.${c}.output
                cat <<EOF >> ${PARALLELINPUT}
( echo ${j}. ${t}; perl -ne 'BEGIN{\$ss=0;\$s=0;\$n=0} if(/^real/){@a=split " ";\$ss+=\$a[1]**2;\$s+=\$a[1];\$n++;print "@a \$ARGV\n"}END{\$v=(\$ss-\$s*\$s/\$n)/(\$n-1);printf " avg = %g sd = %.3g\n",\$s/\$n,sqrt(\$v)}' \`find tests/${a}*/${c}* -name ${t}\`; echo ) > ${OUTPUT}
EOF
            done

            echo

        fi

    done
done

#
# Execute parallel script
#
j=0
corecount=`${loc}/parallel --number-of-cores`

for a in ${ARCHLIST}; do

    PARALLELINPUT=${OUTDIR}/${me}.${a}.parallel.input.txt
    PARALLELOUTPUT=${OUTDIR}/${me}.${a}.parallel.stdout
    j=`expr $j + 1`
    printf "Executing parallel script for ${a}"
    ( ${loc}/parallel --joblog ${OUTDIR}/${a}.parallel.job.log -j ${corecount} -t < ${PARALLELINPUT} 2>&1 ) > ${PARALLELOUTPUT}
    echo " ... done"

    for c in ${COMPILERLIST}; do
        FILES=`ls ${OUTDIR}/*.${a}.${c}.output 2>/dev/null`
        C=`echo ${FILES} | wc -w`
        if [ ${C} -gt 0 ]; then
            ( echo; \
              echo '-------------------------------------------------------'; \
              cat ${OUTDIR}/CPUFLAGS.${a}.${c}.txt; \
              echo '-------------------------------------------------------'; \
              cat ${FILES}; \
            ) > ${OUTDIR}/${me}.${a}.parallel.output.txt
        fi
    done

    rm ${OUTDIR}/*.output

    perl -ne 'BEGIN{$ss=0;$s=0;$n=0} if(/^real/){@a=split " ";$ss+=$a[1]**2;$s+=$a[1];$n++}END{$v=($ss-$s*$s/$n)/($n-1);printf " OVERALL avg = %.3g sd = %.3g\n",$s/$n,sqrt($v)}' ${OUTDIR}/${me}.${a}.parallel.output.txt > ${OUTDIR}/${me}.${a}.overall.parallel.output.txt

done

echo
echo Results are in the following files:
j=0
for a in ${ARCHLIST}; do

    j=`expr $j + 1`
    jpad="     $j. "
    pad="`echo ${jpad} | sed 's/./ /g'`"
    printf "%s%s\n     ${pad}" "${jpad}" ${OUTDIR}/${me}.${a}.parallel.output.txt
    cat ${OUTDIR}/${me}.${a}.overall.parallel.output.txt

done

echo
