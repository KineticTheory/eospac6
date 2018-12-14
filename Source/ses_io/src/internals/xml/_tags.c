#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include "tags.h"
#include "../xml_utilities.h"

#include <string.h>


ses_boolean _find_grid_tag(FILE* pFILE, ses_material_id the_mid, ses_table_id the_tid) {

  /*  position the file pointer after the grid tab for the mid, tid pair */


  ses_boolean return_value = SES_TRUE;

  char* the_tag = NULL;

  /*  start from the beginning */

  rewind(pFILE);

  /*  skip through the file until the "grid" tag is found  */

  char* comparison_string = malloc(sizeof(char)*35);
  sprintf(comparison_string, "<grid mid = %ld tid = %ld>", the_mid, the_tid); 
  while ((!feof(pFILE)) && (the_tag = _skip_tag(pFILE, "")) && (strstr(the_tag, comparison_string) == NULL)) {
    free(the_tag);
    the_tag = (char*)NULL;
  }
  free(the_tag);
  the_tag = (char*)NULL;
  
  free(comparison_string);

  if (feof(pFILE) == EOF) {
    return_value = SES_FALSE;
  }

  return return_value;

}
ses_boolean _find_data_record_tag(FILE* pFILE, ses_material_id the_mid, ses_table_id the_tid) {

  /*  position the file pointer after the grid tab for the mid, tid pair */


  ses_boolean return_value = SES_TRUE;

  char* the_tag = NULL;

  /*  start from the beginning */

  rewind(pFILE);

  /*  skip through the file until the "data_record" tag is found  */

  char* comparison_string = malloc(sizeof(char)*35);
  sprintf(comparison_string, "<data_record mid = %ld tid = %ld>", the_mid, the_tid); 
  while ((!feof(pFILE)) && (the_tag = _skip_tag(pFILE,"")) && (strstr(the_tag, comparison_string) == NULL)) {
    free(the_tag);
    the_tag = (char*)NULL;
  }
  free(the_tag);
  the_tag = (char*)NULL;

  free(comparison_string);

  if (feof(pFILE) == EOF) {
    return_value = SES_FALSE;
  }

  return return_value;

}


ses_boolean _find_index_record_tag(FILE* pFILE, ses_material_id the_mid) {

  /*  position the file pointer after the grid tab for the mid, tid pair */


  ses_boolean return_value = SES_TRUE;

  char* the_tag = NULL;

  /*  start from the beginning */

  rewind(pFILE);

  /*  skip through the file until the "index_record" tag is found  */

  char* comparison_string = malloc(sizeof(char)*35);
  sprintf(comparison_string, "<index_record mid = %ld>", the_mid); 

  while ((!feof(pFILE)) && (the_tag = _skip_tag(pFILE,"")) && (strstr(the_tag, comparison_string) == NULL)) {
    free(the_tag);
    the_tag = (char*)NULL;
  }
  free(the_tag);
  the_tag = (char*)NULL;

  free(comparison_string);

  if (feof(pFILE) == EOF) {
    return_value = SES_FALSE;
  }

  return return_value;

}



