
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include <stdio.h>
#include <string.h>

#include "llnl_tags.h"

#define _get_grid_llnl_xml HEADER(_get_grid_llnl_xml)
#define check_errors_gglx HEADER(check_errors_gglx)


//////////////////DEFINES///////////////////////
//USE_LIBXML - turned on in libxml2.a available, used with llnl xml format
////////////////////////////////////////////////


#ifdef USE_LIBXML2
#include "libxml/parser.h"
#include <libxml/xmlmemory.h>
#include "libxml/xmlstring.h"
#endif


ses_boolean _get_grid_llnl_xml(ses_file_handle the_handle, ses_material_id the_mid, 
		      ses_table_id the_tid, long* nr, long* nt, long* ntab) {

  //  function prototypes

  ses_error_flag check_errors_gglx(struct _ses_index_record* ptIR, ses_table_id the_tid, struct _ses_file_handle* pSFH);

  //  end function prototypes

  ses_boolean return_value = SES_TRUE;
  long my_nr = 0;
  long my_nt = 0;
  *nr = my_nr;
  *nt = my_nt;

  ses_error_flag error_check = check_errors_gglx(FILE_LIST[the_handle]->_current_index_record, the_tid, FILE_LIST[the_handle]->_the_handle);
  if (error_check != SES_NO_ERROR) {
	return return_value;
  }

  /*  THIS ROUTINE GOES TO THE current setup index record and returns
      nr and nt */ 

  int index = 0;
  index = _get_table_index(FILE_LIST[the_handle]->_current_index_record, the_tid);

  my_nr = FILE_LIST[the_handle]->_current_index_record->_nr[index];
  my_nt = FILE_LIST[the_handle]->_current_index_record->_nt[index];

  *nr = my_nr;
  *nt = my_nt;

  return return_value;
}

ses_error_flag check_errors_gglx( struct _ses_index_record* ptIR, ses_table_id the_tid, struct _ses_file_handle* pSFH) {

  /*  get the c file handle */

   if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
      printf("_get_grid_llnl_xml: ses file handle null in _get_grid\n");
#endif
      return SES_NULL_OBJECT_ERROR;
   }
   FILE* pFILE = pSFH->_c_file_handle;
   if (pFILE == (FILE*)NULL) {
#ifdef DEBUG_PRINT
      printf("_get_grid_xml: c file handle null _get_grid \n");
#endif
      return SES_NULL_OBJECT_ERROR;
   } 
   if (ptIR == (struct _ses_index_record*)NULL) {
#ifdef DEBUG_PRINT
      printf("_get_grid_xml: current_index_record NULL \n");
#endif
      return SES_NULL_OBJECT_ERROR;
   } 
   int index = _get_table_index(ptIR, the_tid);
   if (index < 0) {
#ifdef DEBUG_PRINT
      printf("_get_grid_xml: table index negative\n");
#endif
      return SES_OBJECT_OUT_OF_RANGE;
   } 
   

   return SES_NO_ERROR;
}

