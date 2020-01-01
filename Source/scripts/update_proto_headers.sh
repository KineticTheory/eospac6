#! /bin/bash

me=`basename $0`

# whereis $0
loc=`dirname "$0"`
if ! echo ${loc} | egrep '^\/' > /dev/null; then
    loc=`pwd`/${loc}
    loc=`echo ${loc} | sed -e 's/\/\.$//'`
fi

if [ $# -le 0 ]; then
  SHOW_HELP=1
  err=1
else
  SHOW_HELP=0
  err=0
fi
REPLACE_ORIGINALS=1
VERBOSE=
EXTERN=

# process command line options
while getopts ehRv option ; do
  case "$option"
  in
    e)  EXTERN='--extern_only';;
    h)  SHOW_HELP=1;;
    R)  REPLACE_ORIGINALS=0;;
    v)  VERBOSE='-v -v';;
  esac
done

#shift $(($OPTIND - 1))  # this does not work for sh on sparc-sun-solaris
shift `expr $OPTIND - 1`

# display usage
if [ $SHOW_HELP -ne 0 ]; then
    cat <<EOF

Update and/or create a prototype header file for each of the FILES. The prototype header file(s)
will be collocated with the original source FILES, and they will be named like the following
pattern transformation demonstrates:

          foo.c -> foo.proto.h

USAGE: $0 [OPTIONS] FILES

OPTIONS:

       -e             force all generated prototypes to be external, appropriately wrapped with extern "C"
       -h             display this help
       -R             do not replace original prototype FILES
       -v             enable verbose output

EOF
    exit ${err}

fi

# define list of FILES
if [ $# -gt 0 ]; then
    FILES=$*
else
    FILES=src/eos*.c
fi
echo `echo ${FILES} | wc -w` Files:
printf "\t%s\n" ${FILES}
echo

for f in ${FILES}; do

    if [ ! -f ${f} ]; then
	echo ${f} is not a valid file
    else
	echo ${f} ... ok
    fi

done

NOTES=
for f in ${FILES}; do

    if [ ! -f ${f} ]; then
	echo '---' ${f} is not a valid file
	continue
    fi

    h=`echo ${f} | sed -e 's/\.[^\.]\{1,\}$//'`.proto.h
    cflow.filter.pl ${VERBOSE} ${EXTERN} -f '*/*.[fc]*' -c -i ${f} | indent -l115 > ${h}.new
    diff -sq -I 'Automatically generated' ${h} ${h}.new
    if [ $? -ne 0 ]; then
	if [ ${REPLACE_ORIGINALS} -ne 0 ]; then
	    NOTES="${NOTES}Updated ${h}\n"
	    printf "\t"
	    mv -v ${h}.new ${h}
	else
	    NOTES="${NOTES}Created ${h}.new\n"
	fi
    else
	NOTES="${NOTES}No changes found for ${h}\n"
	rm -f ${h}.new
    fi

done

printf "\nSUMMARY:\n${NOTES}"
