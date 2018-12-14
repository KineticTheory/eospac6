
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"


#include <string.h>

#define print_index_record HEADER(print_index_record)

ses_string print_index_record(ses_file_handle the_handle, ses_material_id mid, ses_table_id tid) {

  ses_string return_value = (ses_string)NULL;
  return_value = malloc(sizeof(char) * 500);

  ses_table_id_reference dummy1 = (ses_table_id_reference)NULL;
  long* dummy2 = (long*)NULL; 
  long* dummy3 = (long*)NULL;
  long in_date1;
  long in_date2;
  long in_version1;
  long size = ses_access_table_index(the_handle,        
		  	 &dummy1, &dummy2, &dummy3,
			 &in_date1, &in_date2, &in_version1);

  int i = 0;
  for (i=0; i < size; i++) {


	if (dummy1[i] == tid) {
  		sprintf(return_value, "table id %ld nwds %ld iadr %ld date1 %ld date2 %ld version %ld\n", dummy1[i],
			dummy2[i], dummy3[i], in_date1, in_date2, in_version1);
	}
  }

		

  return return_value;

}


