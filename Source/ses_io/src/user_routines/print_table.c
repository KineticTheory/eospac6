
#include "ses_defines.h"

#include <string.h>

#define print_table HEADER(print_table)


/*void print_table(ses_string sesame_file, ses_material_id mid) {

  if (sesame_file != (ses_string)NULL) {

	printf("print_table:  Material %ld\n\n", (long)mid);

        ses_file_handle the_handle = ses_open(sesame_file, 'R');
	if (ses_is_valid(the_handle) == SES_TRUE) {

  			long date1 = 0;
  			long date2 = 0;
  			long version = 0;
  			ses_table_id_reference tblid = (ses_table_id_reference)NULL;
  			long* nwds = (long*)NULL;
  			long* iadr = (long*)NULL;

  			long ntables = ses_access_table_index(the_handle, &tblid,
					&nwds, &iadr, &date1, &date2, &version);

			  if (iadr == (long*)NULL) {
				printf("ERROR:  iadr is NULL\n");
			  }
			  if (nwds == (long*)NULL) {
				printf("ERROR:  nwds is NULL\n");
			  }

			  printf("Index Record Information: \n\n");
			  printf("  Date1:  %ld\n", (long)date1);
			  printf("  Date2:  %ld\n", (long)date2);
			  printf("  Version: %ld\n", (long)version);
			  printf("  NTables:  %ld\n", (long)ntables);
			  long i = 0;
			  
			  for (i = 0;  i < ntables; i++) {
			    printf("  For Tblid[i] = %ld, nwds is %ld and iadr is %ld\n", tblid[i], nwds[i], iadr[i]);
			  }
	}

	ses_error_flag didit_close = ses_close(the_handle);
        if (didit_close != SES_NO_ERROR) {
		ses_string the_error = ses_print_error_condition(the_handle);
		printf("print_table: error on close:  %s\n", the_error);
	}

  }
  else {
	printf("\n\nprint_table: null filename passed to print_materials_on_file\n");	
  }

}

*/

ses_string print_table(ses_material_id the_mid) {

  ses_string sesame_file = "sesame";
  ses_string return_value = malloc(sizeof(char) * 1000);
  strcpy(return_value, "");
  if (sesame_file != (ses_string)NULL) {

	printf("print_table:  Material %ld\n\n", (long)the_mid);

        ses_file_handle the_handle = ses_open(sesame_file, 'R');
	if (ses_is_valid(the_handle) == SES_TRUE) {

  			long date1 = 0;
  			long date2 = 0;
  			long version = 0;
  			ses_table_id_reference tblid = (ses_table_id_reference)NULL;
  			long* nwds = (long*)NULL;
  			long* iadr = (long*)NULL;

  			long ntables = ses_access_table_index(the_handle, &tblid,
					&nwds, &iadr, &date1, &date2, &version);

			  if (iadr == (long*)NULL) {
				printf("ERROR:  iadr is NULL\n");
			  }
			  if (nwds == (long*)NULL) {
				printf("ERROR:  nwds is NULL\n");
			  }

			  printf("Index Record Information: \n\n");
			  printf("  Date1:  %ld\n", (long)date1);
			  printf("  Date2:  %ld\n", (long)date2);
			  printf("  Version: %ld\n", (long)version);
			  printf("  NTables:  %ld\n", (long)ntables);
			  long i = 0;
			  
			  for (i = 0;  i < ntables; i++) {
			    printf("  For Tblid[i] = %ld, nwds is %ld and iadr is %ld\n", tblid[i], nwds[i], iadr[i]);
			  }
	}

	ses_error_flag didit_close = ses_close(the_handle);
        if (didit_close != SES_NO_ERROR) {
		ses_string the_error = ses_print_error_condition(the_handle);
		printf("print_table: error on close:  %s\n", the_error);
	}

  }
  else {
	printf("\n\nprint_table: null filename passed to print_materials_on_file\n");	
  }

  return return_value;
}
