

#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_internals.h"
#include "ses_externs.h"

#include "stdio.h"
#include <stdlib.h>

#include <stdio.h>

#undef DEBUG_WRAP

#ifdef LC_UNDER
ses_number ses_access_table_index_(ses_file_handle* pt_handle, ses_table_id_reference tblid1, long* nwds1, long* iadr1, long* date1, long* date2, long* version1) {
#else

#ifdef UC_UNDER
  ses_number SES_ACCESS_TABLE_INDEX_(ses_file_handle* pt_handle, ses_table_id_reference tblid1, long* nwds1, long* iadr1, long* date1, long* date2, long* version1) {
#else

#ifdef LC_NOUNDER
    ses_number ses_access_table_index(ses_file_handle* pt_handle, ses_table_id_reference tblid1, long* nwds1, long* iadr1, long* date1, long* date2, long* version1) {
#else

#ifdef UC_NOUNDER
      ses_number SES_ACCESS_TABLE_INDEX(ses_file_handle* pt_handle, ses_table_reference tblid1, long* nwds1, long* iadr1, long* date1, long* date2, long* version1) {
#endif

#endif
#endif
#endif

  ses_number return_value = 0;
  
  ses_file_handle the_handle = *pt_handle;
#ifdef DEBUG_WRAP
  printf("wrap_ses_access_table_index.c:  the_handle is %d\n", the_handle);
  printf("wrap_ses_access_table_index.c:  matid1 is %d\n", matid1);
  printf("wrap_ses_access_table_index.c:  nwds is %d\n", nwds1);
  printf("wrap_ses_access_table_index.c:  date1 is %d\n", date1);
  printf("wrap_ses_access_table_index.c:  date2 is %d\n", date2);
  printf("wrap_ses_access_table_index.c:  version1 is %d\n", version1);
#endif

  ses_table_id_reference return_tblid = (ses_table_id_reference)NULL;
  long* nwds = (long*)NULL;
  long* iadr = (long*)NULL;
  long date11 = 0;
  long date12 = 0;
  long version = 0;
  
  return_value = ses_access_table_index(the_handle, &return_tblid, &nwds, &iadr, 
					&date11, &date12, &version);
#ifdef DEBUG_WRAP 
  printf("wrap_ses_access_table_index.c:  return_value is %d\n", return_value);
  int i = 0;
  for (i = 0; i < return_value; i++) {
    printf("wrap_ses_access_table_index.c:  tblid[%d] is %d\n", i, return_tblid[i]);
    printf("wrap_ses_access_table_index.c:  nwds[%d] is %d\n", i, nwds[i]);
    printf("wrap_ses_access_table_index.c:  iadr[%d\ is %d\n", i, iadr[i]);
  }
  printf("wrap_ses_access_table_index.c:  date1 is %d\n", date11);
  printf("wrap_ses_access_table_index.c:  date2 is %d\n", date12);
  printf("wrap_ses_access_table_index.c:  version is %d\n", version);
  
#endif

  /*  put return values into passed-in arrays */
  int i1 = 0;
  for (i1 = 0; i1 < return_value; i1++) {
    tblid1[i1]  = return_tblid[i1];
    nwds1[i1] = nwds[i1];
    iadr1[i1] = iadr[i1];
  }
  *date1 = date11;
  *date2 = date12;
  *version1 = version; 

  /*  remove excess memory */

  //free(return_tblid);
  //free(nwds);
  //free(iadr);


  return return_value;

}

 
