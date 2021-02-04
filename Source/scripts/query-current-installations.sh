#!/bin/sh
#
# This script is to be run (using CRON or AT) on selected machines to notify the USER of missing EOSPAC6
# installations.
#
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

RECIPIENT=${USER}@lanl.gov
RECIPIENT=eospac-help@lanl.gov

# define known possible locations for modulecmd and initialize module command
. ${loc}/setup_modulecmd

module load eospac/latest
EOSPAC_VERSION=`( module what eospac/latest 2>&1 ) | grep -v '^ *$' | awk -F: '/: *EOSPAC v/{sub(/^[ \t]*/,"",$2);print $2;next}/EOSPAC v/{sub(/^[ \t]*/,"");print}'`

if [ -z "${EOSPAC6_LIB}" ]; then
    echo
    echo "     I failed to load the eospac/latest module" >&2
    echo
    exit 1
fi

# detect Cray PrgEnv module(s)
CRAY_PRGENV=`( module -t avail 2>&1 ) | grep PrgEnv`

if [ -z "${CRAY_PRGENV}" ]; then
    # not Cray
    mprev=`( module -t list 2>&1 ) | grep gcc`
    if [ -z "${mprev}" ]; then
        echo
        echo "     I failed to load a gcc module" >&2
        echo
        exit 1
    fi
    COMPILERS=`${loc}/buildall -l 2>&1 | grep -v -e '^ *$' -e 'Available Fortran modules' | tail -n1`
else
    # detected Cray, so find supported default compiler module names: allinea, cce, gcc, intel
    COMPILERS=`( module -t avail 2>&1 ) | perl -ne 'if((m:^allinea/: or m:^cce/: or m:^gcc/: or m:^intel/:) and m:default:){s:\(default\)::g;print}'`
fi
arch=`config.guess.wrapper`

# list all available libeospac6.a installations (for ${arch})
FILES=`find ${EOSPAC6} -name libeospac6.a | grep "${arch}"`

if [ -z "${FILES}" ]; then
    ( echo From: noreply@lanl.gov; \
      echo To: ${RECIPIENT}; \
      echo 'Subject: ATTENTION! Missing eospac6 installations'; \
      echo 'X-Priority: 1'; \
      echo; \
      echo '*** HEY! WAKE UP! ***'; \
      echo; \
      echo There are no ${EOSPAC_VERSION} installations available for ${arch} on `hostname`; \
      echo Once this deficiency is corrected, consider updating the installation of SOPES; \
    ) | /usr/sbin/sendmail -t
    exit 1
fi



c=0

for c in ${COMPILERS}; do

    n=`find ${EOSPAC6} -name libeospac6.a | grep "${arch}" | grep ${c}`
    if [ -z "${n}" ]; then
        ( echo From: noreply@lanl.gov; \
          echo To: ${RECIPIENT}; \
          echo 'Subject: ATTENTION! Missing compiler-specific eospac6 installation'; \
          echo 'X-Priority: 1'; \
          echo; \
          echo '*** HEY! WAKE UP! ***'; echo; \
          echo There is not an ${EOSPAC_VERSION} installation available for ${arch}/${c} on `hostname`; \
          echo Once this deficiency is corrected, consider updating the installation of SOPES; \
        ) | /usr/sbin/sendmail -t
    fi

done
