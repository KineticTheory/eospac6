#!/bin/tcsh
#
# USAGE:
#
#  setenv F90 ifort
#  setenv STARTDIR /usr/projects/packages/davidp/eospac6/Source
#  setenv NPROC 36
#  make -j`corecount` F90=${F90} XCOMPILE=1
#  sbatch -d ${STARTDIR} -K scripts/run_tests_on_slurm_node.sbatch
#
#  predict the duration of the job
#SBATCH -t 1:00:00
#
#  specify the pathname for stdout and stderr
#SBATCH -o run_tests_on_slurm_node.sbatch.out
#
#  specify the interactive QoS, which is not the default on most LANL HPC clusters
#SBATCH --qos=interactive

if ( ${?PrgEnv} ) then
    set old_PrgEnv = `(module list -t) |& cat | grep PrgEnv`
    echo "module swap ${old_PrgEnv} ${PrgEnv}"
    module swap ${old_PrgEnv} ${PrgEnv}
endif

set start=`date`
echo

if ( ${?STARTDIR} ) then
  if ( -d "${STARTDIR}" ) cd "${STARTDIR}"
endif

#Determine how many processors are available on the current machine
set CORECOUNT=`"${BASEDIR}/scripts/parallel" --number-of-cores`

set echo

pwd

: START TIME: ${start}
: -----------------------------------------------------
#set | sort
#printenv | sort
#
# https://hpc.lanl.gov/files/srun.txt
#
pwd
set SRUN_OPTS = "-c ${CORECOUNT}"
#set SRUN_OPTS = ""
if ( ${?F90} && -d "${BASEDIR}/tests" ) then
    srun ${SRUN_OPTS} "${BASEDIR}/scripts/parallel" --joblog parallel.job.log -j ${CORECOUNT} -t < parallel.job.list
else
    : Do nothing
endif

set end=`date`

: -----------------------------------------------------
: START TIME: ${start}
:   END TIME: ${end}
