
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include "tags.h"
#include "../xml_utilities.h"


ses_error_flag _read_data_record_xml(struct _ses_data_record* ptDR, struct _ses_file_handle* pSFH, unsigned int nsig, ses_boolean do_valid) {

  ses_error_flag return_value = SES_NO_ERROR;

  FILE* pFILE = pSFH->_c_file_handle;

   /*  from the c file handle, find the grid tag for the_mid, the_tid */

   ses_boolean didit_findit = _find_grid_tag(pFILE, ptDR->_mid, ptDR->_tid);
   if (didit_findit == SES_FALSE) {
#ifdef DEBUG_PRINT
     printf("_read_data_record_xml:  could not find grid tag for %ld , %ld \n", ptDR->_mid, ptDR->_tid);
#endif
     return SES_FALSE;
   }

   /*  read the the dimensions, from the grid tag */

   /* double* buffer = (double*)NULL; */
   long dim1, dim2;
   dim1 = _read_long_tag(pFILE, "dim1");
   dim2 = _read_long_tag(pFILE, "dim2");
   /* char* the_tag = (char*)NULL; */
   if ((dim1 > 0) && (dim2 > 0)) {
   
	/* buffer = malloc(sizeof(ses_word)*dim1*dim2); */
   	/* the_tag = */ _skip_tag(pFILE, "</grid>");
	/* buffer = (double*)_read_word_list_pFILE_xml(pFILE, dim1*dim2); */

   }
 
  return return_value;
}
