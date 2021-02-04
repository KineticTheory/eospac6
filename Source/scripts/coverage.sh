#!/bin/sh

loc=`dirname $0`

which lcov 1>/dev/null 2>&1
if [ $? != 0 ]
then
    echo "You need to have lcov installed in order to generate the test coverage report"
    exit 1
fi

# Get latest gcc module (not necessarily default)
. ${loc}/setup_modulecmd
alias modavails='(${MODULECMD} sh -t avail 2>&1) | grep -F -v ":" | grep -v no_packages_available | sed -e "s/\/*(default)//" | sort'
gccmod=`modavails | grep -w gcc | tail -n1`

echo Use $gccmod

# Generate gcov output
#module load gcc
#make -j`corecount` distclean
#make -j`corecount` F90=gfortran MORE_COMPILE_OPTS="-fprofile-arcs -ftest-coverage" MORE_LD_OPTS="-fprofile-arcs -ftest-coverage" check

# Generate html report
#lcov --base-directory . --directory . --zerocounters -q
#${MAKE} check
lcov --base-directory ./src --directory _OBJ/x86_64-avx512f-rhel7-linux-gnu/gcc/* -c -o eospac6_test.info
lcov --extract eospac6_test.info "*/eos_*.c" -o eospac6_src.info # extract src output
cp -av ${loc}/style.css ../../eospac6-coverage/public/.
genhtml -o ../../eospac6-coverage/public -t "EOSPAC 6" --html-prolog ${loc}/navbar.inc eospac6_src.info

# Clean work space
#cd .. && rm -rf $srcdir
