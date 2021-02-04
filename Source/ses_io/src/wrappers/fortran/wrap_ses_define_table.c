

#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_internals.h"
#include "ses_externs.h"

#include "stdio.h"
#include <stdlib.h>

#include <stdio.h>
#include <string.h>

#undef DEBUG_WRAP


#ifdef LC_UNDER
ses_error_flag ses_define_table_(ses_table_id* pt_the_tid, ses_label description, long* pt_nr, long* pt_nt, long* pt_num_independent, long* pt_num_arrays, long** pt_size_arrays[], int* pt_nsize, char labels [], int len[]) { 
#else

#ifdef UC_UNDER
  ses_error_flag SES_DEFINE_TABLE_(ses_table_id* pt_the_tid, ses_label description, long*  pt_nr, long* pt_nt, long* pt_num_independent, long* pt_num_arrays, long** pt_size_arrays, int* pt_nsize, char labels[], int len[]) { 
#else

#ifdef LC_NOUNDER
    ses_error_flag ses_define_table(ses_table_id* pt_the_tid, ses_label description, long* pt_nr, long* pt_nt, long* pt_num_independent, long* pt_num_arrays, long** pt_size_arrays, int* pt_nsize, char labels[], int len[]) { 
#else

#ifdef UC_NOUNDER
      ses_error_flag SES_DEFINE_TABLE(ses_table_id* pt_the_tid, ses_label description, long* pt_nr, long* pt_nt, long* pt_num_independent, long* pt_num_arrays, long** pt_size_arrays, int* pt_nsize, char labels[], int len[]) { 
#endif

#endif
#endif
#endif

#ifdef DEBUG_WRAP
  printf("wrap_ses_define_table.c:  entered\n");
#endif

  ses_error_flag return_value = SES_NO_ERROR;

  ses_table_id the_tid = *pt_the_tid;
  long nr = *pt_nr;
  long nt = *pt_nt;
  long num_independent = *pt_num_independent;
  long num_arrays = *pt_num_arrays;
  int nsize = *pt_nsize;
  int i, j;
  ses_label* c_labels = malloc(sizeof(ses_label)*num_arrays);
  ses_label c_description = malloc(sizeof(ses_label)*strlen(description) +1);

  char** size_arrays = malloc(sizeof(char*) * num_arrays);

  for (i = 0; i < num_arrays; i++){
    size_arrays[i] = malloc(sizeof(char) * 10);
    sprintf(size_arrays[i], "(%ld)", (long)pt_size_arrays[i]);
  }
          
  strcpy(c_description, description);
 
#ifdef DEBUG_WRAP

  printf("wrap_ses_define_table.c:  the_tid is %ld\n", (long)the_tid);
  printf("wrap_ses_define_table.c:  description is %s\n", c_description);
  printf("wrap_ses_define_table.c:  nt is %ld\n", nr);
  printf("wrap_ses_define_table.c:  nr is %ld\n", nt);
  printf("wrap_ses_define_table.c:  num_independent is %ld\n", num_independent);
  printf("wrap_ses_define_table.c:  num_arrays is %ld\n", num_arrays);

  for (i = 0; i < num_arrays; i++) {
    printf("wrap_ses_define_table.c:  size_arrays[%d] is %s\n", i, size_arrays[i]);
  }
  printf("wrap_ses_define_table.c:  nsize is %d\n", nsize);

#endif

  char incoming_array[nsize*num_arrays];
  for (j = 0; j < nsize; j++) {
     for (i = 0;  i < num_arrays; i++) {
        incoming_array[i + j * num_arrays] = labels[i + j * num_arrays];
     }
  }
 
  for (i = 0; i < num_arrays; i++) {
    incoming_array[i*nsize + nsize - 1] = '\0';
#ifdef DEBUG_WRAP
    printf("labels[%d] is:  %s\n", i, &incoming_array[i*nsize]);
#endif
  }

  for (i = 0; i < num_arrays; i++) {
     c_labels[i] = malloc(sizeof(char)*nsize+1);
     strcpy(c_labels[i], &incoming_array[i*nsize]);
#ifdef DEBUG_WRAP
     printf("c_labels[%d] is %s\n", i, c_labels[i]);
#endif
     
  }

  return_value = ses_define_table(the_tid, c_description, nr, nt, num_independent, num_arrays, size_arrays, c_labels);
#ifdef DEBUG_WRAP 
  printf("wrap_ses_define_table.c:  return_value is %d\n", return_value);
#endif

  // free allocated spaces:
  for (i = 0; i < num_arrays; i++){
    free(size_arrays[i]);
    free(c_labels[i]);
   }
   free(size_arrays);
   free(c_labels);
   free(c_description);
  
  return return_value;

}

 
