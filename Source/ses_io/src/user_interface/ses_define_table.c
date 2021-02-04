#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include <string.h>

#undef DEBUG_PRINT

ses_error_flag ses_define_table(ses_table_id tid, ses_label description, long nr, long nt, long num_independent, long num_arrays, char** size_arrays, ses_label* labels) {
 
  /*  function prototypes */

  int my_get_index(ses_table_id the_tid);

  /*  end function prototypes */

#ifdef DEBUG_WRAP
   printf("ses_define_table:  tid is %d\n", tid);
   printf("ses_define_table:  description is %s\n", description);
   printf("ses_define_table:  nr is %d\n", nr);
   printf("ses_define_table:  nt is %d\n", nt);
   printf("ses_define_table:  num_independent is %d\n", num_independent);
   printf("ses_define_table:  num_arrays is %d\n", num_arrays);
   printf("ses_define_table:  size_arrays[0] is %s\n", size_arrays[0]);
   printf("ses_define_table:  labels[0] is %s\n", labels[0]);
#endif

   ses_error_flag return_value = SES_NO_ERROR;

   ses_error_flag didit_add = _add_user_table(tid);
   if (didit_add != SES_NO_ERROR) {
#ifdef DEBUG_PRINT
      printf("ses_define_table:  _add_standard_table FUNCTION FAILED with %d\n", didit_add);
#endif
      _set_latest_error(didit_add);
      return didit_add;
   }

  int new_table_index = my_get_index(tid);

  if (new_table_index < 0) {
#ifdef DEBUG_PRINT
    printf("ses_define_table:  could not find table \n");
#endif
    _set_latest_error(SES_OBJECT_OUT_OF_RANGE);
    return SES_OBJECT_OUT_OF_RANGE;
  }

  if (description == NULL) {
#ifdef DEBUG_PRINT
    printf("ses_define_table:  description is not set.\n");
#endif
    _set_latest_error(SES_OBJECT_OUT_OF_RANGE);
    return SES_OBJECT_OUT_OF_RANGE;
  }


  _the_tables[new_table_index]->_num_arrays = num_arrays;

  /*  create labels and descriptions for the new table */

  _the_tables[new_table_index]->_label = malloc(sizeof(ses_label)*num_arrays);

  int j = 0;
  int length = 0;
  int desc_length = 0;

  for (j=0; j<num_arrays; j++) {

    length = strlen(labels[j]) + 3; /* add 3 to length allow wrapped in quotes, ", and \0 */
    _the_tables[new_table_index]->_label[j] = malloc(sizeof(char)*length);

    strcpy(_the_tables[new_table_index]->_label[j], "\"");
    strcat(_the_tables[new_table_index]->_label[j], labels[j]);
    strcat(_the_tables[new_table_index]->_label[j], "\"");
  }
  desc_length = strlen(description) + 3;
  _the_tables[new_table_index]->_description = malloc(sizeof(char)*desc_length);
  strcpy(_the_tables[new_table_index]->_description, "\"");
  strcat(_the_tables[new_table_index]->_description, description);
  strcat(_the_tables[new_table_index]->_description, "\"");
  
  /*  set is_user_defined */

  _the_tables[new_table_index]->_size_arrays = malloc(sizeof(char*)*num_arrays);


  int i = 0;
  for (i = 0; i < num_arrays; i++) {
#ifdef DEBUG_PRINT
    printf("ses_define_table: size_arrays[%d] =  %s\n", i, size_arrays[i]);
#endif
      _the_tables[new_table_index]->_size_arrays[i] = malloc(sizeof(char) * strlen(size_arrays[i]) + 1);
      strcpy(_the_tables[new_table_index]->_size_arrays[i], size_arrays[i]);
  }

  _the_tables[new_table_index]->_nr = nr;
  _the_tables[new_table_index]->_nt = nt;
  _the_tables[new_table_index]->_num_independent = num_independent;
  _the_tables[new_table_index]->_is_user_defined = SES_TRUE;




  return return_value;
}


