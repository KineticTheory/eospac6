
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include <stdio.h>
#include <string.h>

#include "tags.h"
#include "../xml_utilities.h"


ses_boolean _get_grid_xml(ses_file_handle the_handle, ses_material_id the_mid, 
		      ses_table_id the_tid, long* nr, long* nt, long* ntab) {

  /*  THIS ROUTINE GOES TO THE DATA RECORD FOR the_mid, the_tid, and returns
      nr and nt */ 

  ses_boolean return_value = SES_TRUE;


  long my_nr = 0;
  long my_nt = 0;

  /*  get the c file handle */

   struct _ses_file_handle* pSFH = FILE_LIST[the_handle]->_the_handle;
   if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
      printf("_get_grid_xml: ses file handle null in _get_grid\n");
#endif
      return SES_FALSE;
   }

   FILE* pFILE = pSFH->_c_file_handle;
   if (pFILE == (FILE*)NULL) {
#ifdef DEBUG_PRINT
      printf("_get_grid_xml: c file handle null _get_grid \n");
#endif
      return SES_FALSE;
   } 
 
   /*  from the c file handle, find the grid tag for the_mid, the_tid */

   ses_boolean didit_findit = _find_grid_tag(pFILE, the_mid, the_tid);
   if (didit_findit == SES_FALSE) {
#ifdef DEBUG_PRINT
     printf("_get_grid_xml:  could not find grid tag for %ld , %ld \n", the_mid, the_tid);
#endif
     return SES_FALSE;
   }

   /*  read the the dimensions, from the grid tag */

   long dim1, dim2;
   dim1 = _read_long_tag(pFILE, "dim1");
   dim2 = _read_long_tag(pFILE, "dim2");
   
   my_nr = dim1;
   my_nt = dim2;

   if (my_nr <= 0) {
       my_nr = 1;
   }
   if (my_nt <= 0) {
       my_nt = 1;
   }


   /*  END PUT CODE HERE */
 
   *nr = my_nr;
   *nt = my_nt;

   return return_value;
}
