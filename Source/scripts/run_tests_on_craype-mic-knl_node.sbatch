#!/bin/tcsh
#
# USAGE:
#
#  setenv F90 ifort
#  setenv STARTDIR /usr/projects/packages/davidp/eospac6/Source
#  make -j`corecount` F90=${F90} XCOMPILE=1
#  sbatch -D ${STARTDIR} -W scripts/run_tests_on_craype-mic-knl_node.sbatch
#
#SBATCH --time=1:00:00                                        # walltime
#SBATCH --nodes=1                                             # number of nodes
#SBATCH --ntasks=68                                           # number of processor cores (i.e. tasks)
#SBATCH -p knl                                                # partition queue in which to run the job
#SBATCH --job-name=craype-mic-knl                             # job name
#SBATCH --output=run_tests_on_craype-mic-knl_node.slurm.out   # output file name
#SBATCH --signal=23@60                                        # send signal to job at [seconds] before end
#

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
set SRUN_OPTS = "-n 1 -c 68"
#set SRUN_OPTS = ""
if ( ${?F90} && -d "${BASEDIR}/tests" ) then
    srun ${SRUN_OPTS} "${BASEDIR}/scripts/parallel" --joblog parallel.job.log -j 68 -t < parallel.job.list
else
    : Do nothing
endif

set end=`date`

: -----------------------------------------------------
: START TIME: ${start}
:   END TIME: ${end}
