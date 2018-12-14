#! /bin/sh

loc=`dirname $0`
me=`basename $0`

# default values
exe=
PATCHEXT=ed
verbose=0

# process command line options
while getopts ht:v option ; do
  case "$option"
  in
    h)  cat <<EOF >&2

Usage: ${me} [-h] [-t TYPE] EXE

  EXE    is a test executable name without a directory component (required)

   -h        display this help
   -t TYPE   optional exception patch file type with following possible values:
                 cdiff              context diff output
                 ed                 ed diff output
                 ndiff              normal diff output
                 udiff              unified diff output
   -v        enable verbose output

  The following environment variables must be defined appropriately:
      ACCEPTANCE_REL_DIFF
      ARCH
      BASEDIR
      DATADUMPFILE
      EXEDIR
      FCMP_DEFAULT
      FCMP_OPT
      FCMP
      JUNKFILE
      REGRESSION_DATA_DIR
      REGRESSION_DATA_EXCEPTIONS_DIR

EOF
        exit 2
        ;;
    t)  PATCHEXT=${OPTARG};
        ;;
    v)  verbose=1;
        ;;
  esac
done

shiftcount=`expr $OPTIND - 1`
shift $shiftcount

if [ $shiftcount -lt 0                      ]; then exit; fi

exe=$1

#echo compare $exe output; \
#exit

if [ -z "${exe}"                            ]; then echo "EXE"                            is undefined && exit; fi
if [ -z "${PATCHEXT}"                       ]; then echo "PATCHEXT"                       is undefined && exit; fi
if [ -z "${ACCEPTANCE_REL_DIFF}"            ]; then echo "ACCEPTANCE_REL_DIFF"            is undefined && exit; fi
if [ -z "${ARCH}"                           ]; then echo "ARCH"                           is undefined && exit; fi
if [ -z "${BASEDIR}"                        ]; then echo "BASEDIR"                        is undefined && exit; fi
if [ -z "${DATADUMPFILE}"                   ]; then echo "DATADUMPFILE"                   is undefined && exit; fi
if [ -z "${EXEDIR}"                         ]; then echo "EXEDIR"                         is undefined && exit; fi
if [ -z "${FCMP_DEFAULT}"                   ]; then echo "FCMP_DEFAULT"                   is undefined && exit; fi
if [ -z "${FCMP_OPT}"                       ]; then echo "FCMP_OPT"                       is undefined && exit; fi
if [ -z "${FCMP}"                           ]; then echo "FCMP"                           is undefined && exit; fi
#if [ -z "${FILTERED_EXE}"                   ]; then echo "FILTERED_EXE"                   is undefined && exit; fi
if [ -z "${JUNKFILE}"                       ]; then echo "JUNKFILE"                       is undefined && exit; fi
if [ -z "${REGRESSION_DATA_DIR}"            ]; then echo "REGRESSION_DATA_DIR"            is undefined && exit; fi
#if [ -z "${_MANDATORY_FCMP_FLAGS}"          ]; then echo "_MANDATORY_FCMP_FLAGS"          is undefined && exit; fi

if [ ! -d "${EXEDIR}" ]; then
    echo "${EXEDIR}" does not exist
    exit
fi

#set -x
: EXE                             = "${exe}"
: PATCHEXT                        = "${PATCHEXT}"
: ACCEPTANCE_REL_DIFF             = "${ACCEPTANCE_REL_DIFF}"
: ARCH                            = "${ARCH}"
: BASEDIR                         = "${BASEDIR}"
: DATADUMPFILE                    = "${DATADUMPFILE}"
: EXEDIR                          = "${EXEDIR}"
: FCMP_DEFAULT                    = "${FCMP_DEFAULT}"
: FCMP_OPT                        = "${FCMP_OPT}"
: FCMP                            = "${FCMP}"
#: FILTERED_EXE                    = "${FILTERED_EXE}"
: JUNKFILE                        = "${JUNKFILE}"
: REGRESSION_DATA_DIR             = "${REGRESSION_DATA_DIR}"
: REGRESSION_DATA_EXCEPTIONS_DIR  = "${REGRESSION_DATA_EXCEPTIONS_DIR}"
: _MANDATORY_FCMP_FLAGS           = "${_MANDATORY_FCMP_FLAGS}"
if [ ! $verbose ]; then set +x; fi

WORKDIR=${EXEDIR}/${exe}.workdir

test_result_file=${WORKDIR}/test_result.txt
rm -f ${test_result_file}
touch ${test_result_file}

    for ext in stdout ${DATADUMPFILE}; do
	accept=${ACCEPTANCE_REL_DIFF}
	if [ "$ext" != "stdout" ]; then
	    accept=1.0e-7
	fi
	fcmp_status=0
	baseline_dir=`${BASEDIR}/scripts/find_exception -d ${REGRESSION_DATA_EXCEPTIONS_DIR} $exe.${ext}.${PATCHEXT}`
	if [ "x$baseline_dir" = "x" ]; then
	    baseline_dir=${REGRESSION_DATA_DIR}
	    exception_message=""
	else
	    exception_message=", baseline exception in $baseline_dir"
	fi
	if [ -s "${EXEDIR}/$exe.status_error" ]; then
	    exe_status_error=', '`cat ${EXEDIR}/$exe.status_error`
	else
	    exe_status_error=''
	fi
	if [ -s $baseline_dir/$exe.$ext ]; then
	    if [ -s ${EXEDIR}/$exe.$ext ]; then
		fcmp_cmd="${FCMP} -r $accept ${FCMP_DEFAULT} ${_MANDATORY_FCMP_FLAGS} ${ARCH}/$exe.$ext $baseline_dir/$exe.$ext"
		opts_str="  (fcmp options: -r $accept ${FCMP_DEFAULT} ${_MANDATORY_FCMP_FLAGS})"
		if [ -s ${FCMP_OPT} ]; then
		    alt_opts=""
		    fcmp_opt_list=""
		    if grep $exe.$ext ${FCMP_OPT} > /dev/null 2>&1; then
			alt_opts="${_MANDATORY_FCMP_FLAGS} -k $exe.$ext"
			opts_str="  (fcmp options: `${FCMP} -p -k $exe.$ext -o ${FCMP_OPT}` ${_MANDATORY_FCMP_FLAGS})"
			fcmp_cmd="${FCMP} $alt_opts ${ARCH}/$exe.$ext $baseline_dir/$exe.$ext"
			fcmp_opt_list="`${FCMP} $alt_opts -p ${ARCH}/$exe.$ext $baseline_dir/$exe.$ext`"
		    fi
		fi
		rm -f ${JUNKFILE}.$exe.$ext
		if diff -wib ${ARCH}/$exe.$ext $baseline_dir/$exe.$ext > ${JUNKFILE}.$exe.$ext 2>&1 \
		    && perl -e 'exit ($ARGV[0] =~ /Q/)?0:1' "'$fcmp_opt_list'"
		    then
		    echo "PASSED:   $exe.$ext $opts_str$exception_message (identical)" | tee -a ${test_result_file}
		else
		    $fcmp_cmd > ${JUNKFILE}.$exe.$ext 2>&1
		    fcmp_status=$?
		    if [ $fcmp_status -lt 255 -a $fcmp_status -ne 0 ]; then
			echo "FAILED: $exe.$ext $opts_str$exception_message$exe_status_error" `grep 'sizes are different' ${JUNKFILE}.$exe.$ext` | tee -a ${test_result_file}
		    elif [ $fcmp_status -eq 0 ]; then
			echo "PASSED:   $exe.$ext $opts_str$exception_message" | tee -a ${test_result_file}
		    else
			echo "excluded: $exe.$ext from comparison per options in " ${FCMP_OPT} "A.$fcmp_status$exception_message" | tee -a ${test_result_file}
		    fi
		fi
		rm -f ${JUNKFILE}.$exe.$ext
	    else
		if grep $exe.$ext ${FCMP_OPT} > /dev/null 2>&1; then
		    alt_opts="-k $exe.$ext"
		    fcmp_cmd="${FCMP} $alt_opts ${_MANDATORY_FCMP_FLAGS} ${ARCH}/$exe.$ext $baseline_dir/$exe.$ext"
		    $fcmp_cmd > ${JUNKFILE}.$exe.$ext 2>&1
		    fcmp_status=$?
		    rm -f ${JUNKFILE}.$exe.$ext
		fi
		if [ $fcmp_status -lt 255 ]; then
		    echo "FAILED: $exe.$ext" ${EXEDIR}/$exe.$ext, "File is missing or empty$exception_message$exe_status_error" | tee -a ${test_result_file}
		else
		    echo "excluded: $exe.$ext from comparison per options in" ${FCMP_OPT} "B.$fcmp_status$exception_message" | tee -a ${test_result_file}
		fi
	    fi
	else
	    if grep $exe.$ext ${FCMP_OPT} > /dev/null 2>&1; then
		alt_opts="-k $exe.$ext"
		fcmp_cmd="${FCMP} $alt_opts ${_MANDATORY_FCMP_FLAGS} ${ARCH}/$exe.$ext $baseline_dir/$exe.$ext"
		$fcmp_cmd > ${JUNKFILE}.$exe.$ext 2>&1
		fcmp_status=$?
		rm -f ${JUNKFILE}.$exe.$ext
	    fi
	    if [ $fcmp_status -lt 255 ]; then
		echo "WARNING:  $exe.$ext" $baseline_dir/$exe.$ext, "File is missing or empty$exception_message" | tee -a ${test_result_file}
	    else
		echo "excluded: $exe.$ext from comparison per options in" ${FCMP_OPT} "C.$fcmp_status$exception_message" | tee -a ${test_result_file}
	    fi
	fi
    done
