

#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_internals.h"
#include "ses_externs.h"

#include "stdio.h"
#include <stdlib.h>

#include <stdio.h>
#include <string.h>

#define DEBUG_WRAP


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
  ses_label* c_labels = malloc(sizeof(ses_label)*num_arrays);

  char** size_arrays = malloc(sizeof(char*) * num_arrays);

  size_arrays[0] = malloc(sizeof(char) * 10);
  size_arrays[1] = malloc(sizeof(char) * 10);
  size_arrays[2] = malloc(sizeof(char) * 10);
  size_arrays[3] = malloc(sizeof(char) * 10);
  sprintf(size_arrays[0], "(%ld)", (long)pt_size_arrays[0]);
  sprintf(size_arrays[1], "(%ld)", (long)pt_size_arrays[1]);
  sprintf(size_arrays[2], "(%ld)", (long)pt_size_arrays[2]);
  sprintf(size_arrays[3], "(%ld)", (long)pt_size_arrays[3]);
 
#ifdef DEBUG_WRAP

  printf("wrap_ses_define_table.c:  the_tid is %ld\n", (long)the_tid);
  printf("wrap_ses_define_table.c:  description is %s\n", description);
  printf("wrap_ses_define_table.c:  nt is %ld\n", nr);
  printf("wrap_ses_define_table.c:  nr is %ld\n", nt);
  printf("wrap_ses_define_table.c:  num_independent is %ld\n", num_independent);
  printf("wrap_ses_define_table.c:  num_arrays is %ld\n", num_arrays);
  int p = 0;
  for (p = 0; p < num_arrays; p++) {
    printf("wrap_ses_define_table.c:  size_arrays[%d] is %s\n", p, size_arrays[p]);
  }
  printf("wrap_ses_define_table.c:  nsize is %d\n", nsize);

#endif

  int i = 0;
  int j = 0;
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
     c_labels[i] = malloc(sizeof(char)*nsize);
     strcpy(c_labels[i], &incoming_array[i*nsize]);
#ifdef DEBUG_WRAP
     printf("c_labels[%d] is %s\n", i, c_labels[i]);
#endif
     
  }

  return_value = ses_define_table(the_tid, description, nr, nt, num_independent, num_arrays, size_arrays, c_labels);
#ifdef DEBUG_WRAP 
  printf("wrap_ses_define_table.c:  return_value is %d\n", return_value);
#endif

  
  return return_value;

}

 
