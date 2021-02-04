#! /bin/sh

loc=`dirname $0`
me=`basename $0`

# default values
exe=
PATCHEXT=ed
verbose=0
device=

# process command line options
while getopts hv option ; do
    case "$option"
        in
        h)  cat <<EOF >&2

        Usage: ${me} [-h] [-v]

        Detect the existence of a usable GPU:
          NVIDIA, AMD, VEGA, RADEON

        -h    Display this help
        -v    Verbose output

EOF
        exit 2
        ;;
        v)  verbose=1;
        ;;
    esac
done

shiftcount=`expr $OPTIND - 1`
shift $shiftcount

if [ $verbose -ne 0 ]; then set -x; fi

# list all PCI devices of interest
type=`lspci | grep -m1 -io -E '(nvidia|radeon|vega)' | tr "[a-z]" "[A-Z]"`

case "${type}" in
    NVIDIA)
        # Ensure viability of NVIDIA GPU
        nvidia-smi >/dev/null 2>&1
        if [ $? -eq 0 ]; then
            device=${type}
        fi
        ;;
    RADEON)
        device=${type}
        ;;
    VEGA)
        device=${type}
        ;;
    *)
        device=
        ;;
esac

echo ${device}
