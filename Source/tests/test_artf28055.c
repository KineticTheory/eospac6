/*********************************************************************
 * Test Program
 * ---------------------------------------------------------
 * Filetype: (SOURCE)
 * 
 * Copyright -- see file named COPYRIGHTNOTICE
 * 
 ********************************************************************/

/*! \file
 *  \ingroup tests quick
 *  \brief Test the following function with a custom generated sesame material
 *         containing random comment tables:
 *         ses_write_comments, ses_get_comments, ses_comments, ses_read_next,
 *         eos_GetTableCmnts
 *
 *  See SourceForge issue 
 *  <a href="https://tf.lanl.gov/sf/go/artf28055">artf28055</a>
 *  for details.
 *
 * \note
 * MATIDS TO TEST: none
 */

#include "TEST_FUNCTIONS.h"
#include "eos_Interface.h"
#include "ses_defines.h"
#include "stdlib.h"

#include <string.h>

ses_material_id mid = 99999;
ses_string filename = "sesame_with_comments";

void test_ses_write_comments(ses_file_type the_type) {

  /*  test write for a single material, multiple tables with the iterator interface  */

  printf("TEST: Testing ses_write_comments with random strings\n\n");  

  /* define pseudo-random string lengths for all comments */
  int length_list[] = {
    873,    874,    875,    876,    877,    878,    879,    880,    287,    573,    494,    650,
    377,    531,    985,    947,    657,    742,    146,    628,    17,     251,    142,    832,
    162,    415,    134,    113,    1033,   226,    530,    868,    633,    306,    659,    542,
    510,    1006,   302,    798,    545,    796,    414,    922,    293,    364,    835,    950,
    72,     982,    544,    89,     199,    686,    921,    361,    66,     21,     473,    65,
    246,    1004,   933,    880,    276,    558,    388,    786,    530,    690,    550,    41,
    453,    964,    962,    745,    294,    764,    662,    366,    711,    172,    455,    910,
    857,    342,    237,    924,    362,    710,    989,    609,    680,    888,    455,    955,
    412,    842,    707
  };

  /*  open the file */

  ses_open_type wopen_flag = 'W';
  
  ses_file_handle wHandle = ses_open(filename, wopen_flag);
  ses_set_format(wHandle, the_type);
  
  /*  setup the file for writing */

  ses_table_id tid = 201;

  ses_write_setup(wHandle, mid, tid, 5, 1, -1);
  ses_write_number(wHandle, 4.0);
  ses_write_number(wHandle, 4.0);
  ses_write_number(wHandle, 4.0);
  ses_write_number(wHandle, 4.0);
  ses_write_number(wHandle, 4.0);

  char *lorem="BEGIN -- Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam laoreet porta risus et commodo. Mauris non lorem magna. Etiam gravida ultricies tortor, ac scelerisque sem semper vel. Nulla sed urna felis. Mauris ut placerat leo. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; Aliquam erat volutpat. Nullam quis nisi a massa imperdiet commodo. Donec nec mi erat. Nullam nibh mauris, porttitor vel egestas eget, tempor eget odio. Vivamus a fermentum eros. Donec elementum, erat congue varius pretium, sapien felis iaculis ligula, a fringilla lacus metus posuere ante. Sed magna nisi, mattis eget mollis quis, luctus a leo. Nullam molestie, purus in semper malesuada, mauris velit pretium arcu, quis porta nunc purus a felis. Phasellus scelerisque, mauris in ultricies posuere, mi tortor scelerisque odio, nec pharetra dui ligula in eros. Sed non facilisis diam. Curabitur porttitor pulvinar augue condimentum ullamcorper. Fusce consequat, tellus sit amet mattis tristique, urna orci sed.";

  printf(" %s -- END\n", lorem);

  for ( tid = 101; tid < 200; tid++) {

    printf("\nSetting up the file for writing wHandle is %d\n", wHandle);

    ses_string comments = calloc(strlen(lorem)+25,sizeof(char));

    //int minrand = 0;
    //int random_number = minrand + (int)((rand() / (double)RAND_MAX)*(strlen(lorem) - minrand) + 0.5);
    int random_number = length_list[tid-101];

    printf("table %d random number is %d\n", (int)tid, random_number);

    strncpy(comments, lorem, random_number);
    strcat(comments, " -- END");
    printf("%d: %s\n", (int)tid, comments);

    long nt = 1;
    long ntab = -1;
    long nr = strlen(comments);

    ses_write_setup(wHandle, mid, tid, nr, nt, ntab);

    printf("table %d is at least %d bytes\n", (int)tid, (int)nr);
    ses_write_comments(wHandle, comments, nr);

    free(comments);
    comments = (ses_string)NULL;

  }
    
  /*  close the file */

  ses_close(wHandle);
  
}

ses_boolean test_ses_get_comments() {

  ses_boolean test_result = SES_TRUE;
  printf("-----\nTEST: Testing ses_get_comments with invalid file handle -- should return null string\n\n");  

  ses_file_handle the_handle = 0;

  ses_string* the_string = malloc(sizeof(ses_string)*1);
  *the_string = (ses_string)NULL;
  ses_error_flag the_error_flag = SES_NO_ERROR;
  the_error_flag = ses_get_comments(the_handle, the_string);
  if (the_error_flag == SES_INVALID_FILE_HANDLE) {
    printf("TEST: returned SES_INVALID_FILE_HANDLE on invalid file handle -- correct\n");
  }
  else {
    printf("TEST: returned %s -- incorrect \n", *the_string); 
    test_result = SES_FALSE;
  }
  free(the_string[0]);
  the_string[0] = (ses_string)NULL;
  free(the_string);
  the_string = (ses_string*)NULL;

  /*  call with valid file handle -- should return string */

  printf("\nTEST: Testing ses_get_comments with valid file handle, IS setup\n\n");

  ses_open_type open_flag = 'R'; 

  printf("TEST:  before open\n");
  the_handle = ses_open(filename, open_flag);
  printf("TEST:  after open, the_handle is %d\n", the_handle);

  ses_error_flag didit_setup = ses_setup(the_handle, mid, 122);
  printf("TEST:  didit_setup is %d\n", didit_setup);

  the_string= malloc(sizeof(ses_string)*1);
  *the_string = (ses_string)NULL;
  the_error_flag = ses_get_comments(the_handle, the_string);  //  writes out all the comments
  if (the_error_flag == SES_NO_ERROR) {
    printf("TEST: returned SES_NO_ERROR -- correct\n");
    printf("TEST:  the_comments are \n%s\n", *the_string);
    free(*the_string);
    *the_string = (ses_string)NULL;
  }
  else {
    printf("TEST: returned %d -- incorrect \n", the_error_flag); 
    printf("TEST:  error is %s\n", ses_print_error_message(the_error_flag));
    test_result = SES_FALSE;
  }


  ses_close(the_handle);

  free(the_string[0]);
  the_string[0] = (ses_string)NULL;
  free(the_string);
  the_string = (ses_string*)NULL;
  
  return test_result;

}

ses_boolean test_ses_comments() {

  ses_boolean test_result = SES_TRUE;
  printf("-----\nTEST: Testing ses_comments with invalid file handle -- should return null string\n\n");  

  ses_file_handle the_handle = 0;

  ses_string* the_string = malloc(sizeof(ses_string)*1);
  *the_string = (ses_string)NULL;
  ses_error_flag the_error_flag = SES_NO_ERROR;
  the_error_flag = ses_comments(the_handle, the_string);
  if (the_error_flag == SES_INVALID_FILE_HANDLE) {
    printf("TEST: returned SES_INVALID_FILE_HANDLE on invalid file handle -- correct\n");
  }
  else {
    printf("TEST: returned %s -- incorrect \n", *the_string); 
    test_result = SES_FALSE;
  }
  free(the_string[0]);
  the_string[0] = (ses_string)NULL;
  free(the_string);
  the_string = (ses_string*)NULL;

  /*  call with valid file handle -- should return string */

  printf("\nTEST: Testing ses_comments with valid file handle, IS setup\n\n");

  ses_open_type open_flag = 'R'; 

  printf("TEST:  before open\n");
  the_handle = ses_open(filename, open_flag);
  printf("TEST:  after open, the_handle is %d\n", the_handle);

  ses_table_id tid;
  for ( tid = 101; tid < 200; tid++) {

    ses_setup(the_handle, mid, tid);

    the_string= malloc(sizeof(ses_string)*1);
    *the_string = (ses_string)NULL;
    the_error_flag = ses_comments(the_handle, the_string);
    if (the_error_flag == SES_NO_ERROR) {
      printf("\n%d: %s\n", (int)tid, *the_string);
      free(*the_string);
      *the_string = (ses_string)NULL;
    }
    else {
      printf("TEST: returned %d -- incorrect \n", the_error_flag); 
      printf("TEST:  error is %s\n", ses_print_error_message(the_error_flag));
      test_result = SES_FALSE;
      break;
    }

  }

  ses_close(the_handle);

  free(the_string[0]);
  the_string[0] = (ses_string)NULL;
  free(the_string);
  the_string = (ses_string*)NULL;
  
  return test_result;

}

ses_boolean test_ses_read_next() {

  ses_boolean test_result = SES_TRUE;
  printf("-----\nTEST: Testing ses_read_next with invalid file handle -- should return null string\n\n");  

  ses_file_handle the_handle = 0;

  ses_string* the_string = malloc(sizeof(ses_string)*1);
  *the_string = (ses_string)NULL;
  ses_error_flag the_error_flag = SES_NO_ERROR;
  the_error_flag = ses_get_comments(the_handle, the_string);
  if (the_error_flag == SES_INVALID_FILE_HANDLE) {
    printf("TEST: returned SES_INVALID_FILE_HANLDE on invalid file handle -- correct\n");
  }
  else {
    printf("TEST: returned %s -- incorrect \n", *the_string); 
    test_result = SES_FALSE;
  }
  free(the_string[0]);
  the_string[0] = (ses_string)NULL;
  free(the_string);
  the_string = (ses_string*)NULL;

  /*  call with valid file handle -- should return string */

  printf("\nTEST: Testing ses_read_next with valid file handle, IS setup\n\n");

  ses_open_type open_flag = 'R'; 

  printf("TEST:  before open\n");
  the_handle = ses_open(filename, open_flag);
  printf("TEST:  after open, the_handle is %d\n", the_handle);

  ses_table_id tid;
  for ( tid = 101; tid < 200; tid++) {

    ses_setup(the_handle, mid, tid);

    the_string= malloc(sizeof(ses_string)*1);
    *the_string = (ses_string)NULL;
    the_error_flag = SES_NO_ERROR;
    ses_word_reference the_buffer = ses_read_next(the_handle);

    *the_string = (ses_string)the_buffer;

    if (the_error_flag == SES_NO_ERROR) {
      printf("\n%d: %s\n", (int)tid, *the_string);
      free(*the_string);
      *the_string = (ses_string)NULL;
    }
    else {
      printf("TEST: returned %d -- incorrect \n", the_error_flag); 
      printf("TEST:  error is %s\n", ses_print_error_message(the_error_flag));
      test_result = SES_FALSE;
      break;
    }

  }

  ses_close(the_handle);

  free(the_string[0]);
  the_string[0] = (ses_string)NULL;
  free(the_string);
  the_string = (ses_string*)NULL;
  

  return test_result;

}

EOS_BOOLEAN test_eos_GetTableCmnts() {

  EOS_INTEGER sesMaterialNum = (EOS_INTEGER)mid;
  EOS_INTEGER sesTableNum = 101;
  EOS_INTEGER sesSubtableIndex = 1;
  EOS_INTEGER tableType;

  EOS_INTEGER err = EOS_OK;
  EOS_INTEGER commentInfoItems[1] = {
    EOS_Cmnt_Len
  };
  EOS_REAL infoVals[1];
  EOS_INTEGER Cmnt_Len=0;
  EOS_CHAR *cmntStr=NULL;

  EOS_INTEGER one   = 1;
  EOS_INTEGER th;
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];

  printf("-----\nTEST: Testing eos_GetTableCmnts\n\n");

  err = get_DataType(sesTableNum, sesSubtableIndex, &tableType);
  if (err != EOS_OK) {
    printf ("get_DataType error!\n");
    return EOS_FALSE;
  }

  /* Initialize the table handle */
  eos_CreateTables (&one, &tableType, &sesMaterialNum, &th, &err);
  eos_SetDataFileName (&th, &sesMaterialNum, &tableType, filename, &err);
  if (err != EOS_OK) {
    eos_GetErrorMessage (&err, errorMessage);
    printf ("eos_SetDataFileName ERROR %d: %s\n", err, errorMessage);
    return EOS_FALSE;
  }

  /* Load data */
  eos_LoadTables (&one, &th, &err);
  if (err != EOS_OK) {
    eos_GetErrorMessage (&err, errorMessage);
    printf ("eos_LoadTables ERROR %d: %s\n", err, errorMessage);
    return EOS_FALSE;
  }

  eos_GetTableInfo (&th, &one, commentInfoItems, infoVals, &err);
  if (err == EOS_OK) {
    Cmnt_Len = (EOS_INTEGER) (infoVals[0]);
    cmntStr = (EOS_CHAR *) calloc (Cmnt_Len, sizeof (EOS_CHAR));
    if (! cmntStr) {
      printf ("Memory allocation error!\n");
      return EOS_FALSE;
    }
    eos_GetTableCmnts (&th, cmntStr, &err);
    printf ("Comments:\n%s\n", cmntStr);
    free(cmntStr);
    cmntStr = (EOS_CHAR*)NULL;
  }

  return (err == EOS_OK);

}

int main() {
  int rval = 0;

  test_ses_write_comments('B');
  printf("\n\n");

  rval += (test_ses_get_comments() ) ? 0 : 1;
  //printf("\n\n");

  rval += (test_ses_comments()     ) ? 0 : 2;
  printf("\n\n");

  rval += (test_ses_read_next()    ) ? 0 : 4;
  printf("\n\n");

  rval += (test_eos_GetTableCmnts()) ? 0 : 8;

  return rval;
}
