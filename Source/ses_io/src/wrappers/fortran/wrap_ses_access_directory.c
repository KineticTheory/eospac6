

#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_internals.h"
#include "ses_externs.h"

#include "stdio.h"
#include <stdlib.h>

#include <stdio.h>

#undef DEBUG_WRAP

#ifdef LC_UNDER
ses_number ses_access_directory_(ses_file_handle* pt_handle, ses_material_id_reference matid1, long* nwds1, long* iadr1, long* date1, long* version1) {
#else

#ifdef UC_UNDER
  ses_number SES_ACCESS_DIRECTORY_(ses_file_handle* pt_handle, ses_material_id_reference matid1, long* nwds1, long* iadr1, long* date1, long* version1) {
#else

#ifdef LC_NOUNDER
    ses_number ses_access_directory(ses_file_handle* pt_handle, ses_material_id_reference matid1, long* nwds1, long* iadr1, long* date1, long* version1) {
#else

#ifdef UC_NOUNDER
      ses_number SES_ACCESS_DIRECTORY(ses_file_handle* pt_handle, ses_material_id_reference matid1, long* nwds1, long* iadr1, long* date1, long* version1) {
#endif

#endif
#endif
#endif

  ses_number return_value = 0;
  
  ses_file_handle the_handle = *pt_handle;
#ifdef DEBUG_WRAP
  printf("wrap_ses_access_directory.c:  the_handle is %d\n", the_handle);
  printf("wrap_ses_access_directory.c:  matid1 is %d\n", matid1);
  printf("wrap_ses_access_directory.c:  nwds is %d\n", nwds1);
  printf("wrap_ses_access_directory.c:  date1 is %d\n", date1);
  printf("wrap_ses_access_directory.c:  version1 is %d\n", version1);
#endif

  ses_material_id_reference return_matid = (ses_material_id_reference)NULL;
  long* nwds = (long*)NULL;
  long* iadr = (long*)NULL;
  long date = 0;
  long version = 0;
  
  return_value = ses_access_directory(the_handle, &return_matid, &nwds, &iadr, 
				      &date, &version);
#ifdef DEBUG_WRAP 
  printf("wrap_ses_access_directory.c:  return_value is %d\n", return_value);
  int i = 0;
  for (i = 0; i < return_value; i++) {
    printf("wrap_ses_access_directory.c:  matid[%d] is %d\n", i, return_matid[i]);
    printf("wrap_ses_access_directory.c:  nwds[%d] is %d\n", i, nwds[i]);
    printf("wrap_ses_access_directory.c:  iadr[%d\ is %d\n", i, iadr[i]);
  }
  printf("wrap_ses_access_directory.c:  date is %d\n", date);
  printf("wrap_ses_access_directory.c:  version is %d\n", version);
  
#endif

  /*  put return values into passed-in arrays */
  int i1 = 0;
  for (i1 = 0; i1 < return_value; i1++) {
    matid1[i1]  = return_matid[i1];
    nwds1[i1] = nwds[i1];
    iadr1[i1] = iadr[i1];
  }
  *date1 = date;
  *version1 = version; 



  return return_value;

}

 
