#!/bin/tcsh
#
# USAGE:
#
#  setenv F90 ifort
#  setenv STARTDIR /usr/projects/packages/davidp/eospac6/Source
#  setenv NPROC 36
#  sbatch -D ${STARTDIR} -p power9 -o compile_and_run_tests_on_power9_ibmxlc-16.1.0-xlf-16.1.0_slurm_node.sbatch.out scripts/compile_and_run_tests_on_slurm_node.sbatch
#
#  inherit environment variables
#SBATCH --export=STARTDIR,MODULES2LOAD,NPROC,F90_OVERRIDE,MORE_COMPILE_OPTS,SESAMEPATH

echo "----- BEGIN ENVIRONMENT -----"
printenv
set 
echo "-----  END ENVIRONMENT  -----" 

#set echo

set start=`date`

if ( $?STARTDIR ) then
    if ( -d "${STARTDIR}" ) cd "${STARTDIR}"
else
#    echo FATAL: Definition of STARTDIR is required
#    exit 1
endif

if ( $?MODULES2LOAD ) then
    module load ${MODULES2LOAD}
    module list
else
    echo FATAL: Definition of MODULES2LOAD is required
    exit 1
endif

#set echo

if ( $?F90_OVERRIDE ) then
    setenv F90 "${F90_OVERRIDE}"
else
    echo FATAL: Definition of F90_OVERRIDE is required
    exit 1
endif

if ( $?MORE_COMPILE_OPTS ) then
    setenv MORE_COMPILE_FLAGS "${MORE_COMPILE_OPTS}"
else
    setenv MORE_COMPILE_FLAGS
endif

#Determine how many processors are available on the current machine
if ( $?NPROC ) then
    set CORECOUNT=${NPROC}
else
    set CORECOUNT=`scripts/parallel --number-of-cores`
endif

set CWD=`pwd`
set CWD_r=`basename ${CWD}`

if ( $?STARTDIR ) then
    echo "STARTDIR           = ${STARTDIR}"
else
    echo "STARTDIR           = "
endif

if ( $?MODULES2LOAD ) then
    echo "MODULES2LOAD       = ${MODULES2LOAD}"
else
    echo "MODULES2LOAD       = "
endif

if ( $?CORECOUNT ) then
    echo "CORECOUNT          = ${CORECOUNT}"
else
    echo "CORECOUNT          = "
endif

if ( $?F90_OVERRIDE ) then
    echo "F90_OVERRIDE       = ${F90_OVERRIDE}"
else
    echo "F90_OVERRIDE       = "
endif

if ( $?MORE_COMPILE_OPTS ) then
    echo "MORE_COMPILE_OPTS = ${MORE_COMPILE_OPTS}"
else
    echo "MORE_COMPILE_OPTS = "
endif

if ( $?SESAMEPATH ) then
    echo "SESAMEPATH         = ${SESAMEPATH}"
else
    echo "SESAMEPATH         = "
endif

set echo

pwd

: START TIME: ${start}
: -----------------------------------------------------

pwd
if ( $?F90 && ( -d "tests" || ${CWD_r} == 'tests' ) ) then
    which make
    make --version
    make -j ${CORECOUNT} F90=${F90_OVERRIDE} MORE_COMPILE_FLAGS="${MORE_COMPILE_FLAGS}" check
else
    : Do nothing
endif

set end=`date`

: -----------------------------------------------------
: START TIME: ${start}
:   END TIME: ${end}
