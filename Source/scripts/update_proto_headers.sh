#! /bin/bash
FILES='
  src/eos_Access.c
  src/eos_Data.c
  src/eos_DataMap.c
  src/eos_ErrorHandler.c
  src/eos_Interpolation.c
  src/eos_RecordType1.c
  src/eos_RecordType2.c
  src/eos_RecordType3.c
  src/eos_RecordType4.c
  src/eos_RecordType5.c
  src/eos_RecordType6.c
  src/eos_SesUtils.c
  src/eos_Taylor.c
  src/eos_Utils.c
  src/eos_UtilsRage.c
'
NOTES=
for f in ${FILES}; do
    h=`echo ${f} | sed -e 's/\.[^\.]\{1,\}$//'`.proto.h
    cflow.filter.pl -f '*/*.[fc]*' -c -i ${f} | indent -l115 > ${h}.new
    diff -q -I 'Automatically generated' ${h} ${h}.new
    if [ $? -ne 0 ]; then
	NOTES="${NOTES}Updated ${h}\n"
	mv -v ${h}.new ${h}
  else
	NOTES="${NOTES}No changes found for ${h}\n"
	rm -f ${h}.new
  fi
done

printf "\nSUMMARY:\n${NOTES}"
