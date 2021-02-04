#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include <stdio.h>
#include <string.h>

#include "_ses_table.h"

//////////////////DEFINES///////////////////////
//DEBUG_MY_READ_TABLES - left in for convenience
//#define DEBUG_MY_READ_TABLES
////////////////////////////////////////////////
#undef DEBUG_PRINT

#define _add_standard_table HEADER(_add_standard_table)


ses_boolean _construct_standard_tables(void) {

  /*  construct the standard tables */

  ses_boolean return_value = SES_TRUE;

  /*  read in information for the tables */

  /*  read in the number of tables, NUMBER_TABLES */
  
  NUMBER_TABLES = 123;
  if (NUMBER_TABLES <= 0) {
#ifdef DEBUG_PRINT
    printf("_construct_standard_tables: number tables in <= 0\n");
#endif
    _set_latest_error(SES_READ_ERROR);
    return SES_FALSE;
  }

  /*    create memory for the standard tables */


  _the_tables = malloc(sizeof(struct _standard_table*)*NUMBER_TABLES);

  if (_the_tables == (struct _standard_table**)NULL) {
#ifdef DEBUG_PRINT
    printf("_construct_standard_tables: memory allocation error in _construct_standard_tables\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;

  }

  /*    construct the tables */

  ses_boolean didit_read = SES_TRUE;

  didit_read = my_read_tables();

  if (didit_read == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("_construct_standard_tables: standard table read error\n");
#endif

    /*  release memory on error */
    /* ses_boolean didit_destruct = SES_FALSE; */
    /* didit_destruct = */ _destruct_standard_tables();

    _set_latest_error(SES_READ_ERROR);
    return SES_FALSE;
  }


  return return_value;
}

ses_boolean _destruct_standard_tables(void) {

  //  destruct the standard table list

  ses_boolean return_value = SES_TRUE;

  int i=0;
  ses_boolean didit_destruct  = SES_TRUE;


  for (i=0; i < NUMBER_TABLES; i++) {
    didit_destruct = _destruct_standard_table(_the_tables[i]);
    if (didit_destruct == SES_FALSE) {
#ifdef DEBUG_PRINT
      printf("_destruct_standard_tables: standard table destruct error\n");
#endif
      _set_latest_error(SES_OBJECT_DESTRUCTION_ERROR);
      return SES_FALSE;
    }
    free(_the_tables[i]);
    _the_tables[i] = (struct _standard_table*)NULL;
  }

  free(_the_tables);
  _the_tables = (struct _standard_table**)NULL;


  return return_value;
}


long _get_standard_num_arrays(ses_table_id the_tid) {

  /*  return the standard number of arrays */

#ifdef DEBUG_PRINT
    printf("\n_ses_standard::_get_standard_num_arrays: tid %d \n", (int)the_tid);
#endif
 
  long return_value = 0;

  if (_is_valid_tid(the_tid) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("_get_standard_num_arrays: invalid tid %d in _get_standard_num_arrays\n", (int)the_tid);
#endif
    _set_latest_error(SES_INVALID_TID);
    return (long)0;
  }


#ifdef DEBUG_PRINT
  printf("_get_standard_num_arrays: call my_get_index\n");
#endif
   int index = my_get_index(the_tid);
#ifdef DEBUG_PRINT
   printf("_get_standard_num_arrays: my_get_index: index: %d\n",index);
#endif

  if (index < 0) {
#ifdef DEBUG_PRINT
    printf("_get_standard_num_arrays: my_get_index failed in _get_standard_num_arrays, index: %d\n", index);
#endif
    _set_latest_error(SES_FUNCTION_FAIL_ERROR);
    return (long)0;
  }

#ifdef DEBUG_PRINT
  printf("_get_standard_num_arrays: call _get_standard_num_array_for_table _the_tid = %ld\n", the_tid);
#endif

  return_value = _get_standard_num_arrays_for_table(_the_tables[index]);

#ifdef DEBUG_PRINT
  printf("_get_standard_num_arrays: _get_standard_num_array_for_table return_value= %ld\n", return_value);
#endif

  return return_value;
}




long*      _get_standard_relative_addresses(ses_table_id the_tid, long nr, long nt, long ntab) {

  /*  get the standard relative addresses */


  /*  error check the arguments */

  if (_is_valid_tid(the_tid) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("_get_standard_relative_addresses: invalid tid in _get_standard_relative_addresses\n");
#endif
    _set_latest_error(SES_INVALID_TID);
    return (long*)NULL;
  }

  if (nr < 0) {
#ifdef DEBUG_PRINT
    printf("_get_standard_relative_addresses: nr < 0 in _get_standard_relative_addresses\n");
#endif
    _set_latest_error(SES_OBJECT_OUT_OF_RANGE);
    return (long*)NULL;
  }

  if (nt < 0) {
#ifdef DEBUG_PRINT
    printf("_get_standard_relative_addresses: nt < 0 in _get_standard_relative_addresses\n");
#endif
    _set_latest_error(SES_OBJECT_OUT_OF_RANGE);
    return (long*)NULL;
  }



  /*  get the index of the table tid */

  long* address_arrays = NULL;

  int index = my_get_index(the_tid);

  address_arrays = _get_standard_relative_addresses_for_table(_the_tables[index], nr, nt, ntab);
  return address_arrays;



}



char**     _get_standard_sizes_as_chars(ses_table_id the_tid, long nr, long nt, long ntab) {
  /*  error check the arguments */

  if (_is_valid_tid(the_tid) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("_get_standard_sizes: invalid tid in _get_standard_sizes\n");
#endif
    _set_latest_error(SES_FUNCTION_FAIL_ERROR);
    return (char**)NULL;
  }

  if (nr < 0) {
#ifdef DEBUG_PRINT
    printf("_get_standard_sizes: nr < 0 in _get_standard_sizes\n");
#endif
    _set_latest_error(SES_OBJECT_OUT_OF_RANGE);
    return (char**)NULL;
  }

  if (nt < 0) {
#ifdef DEBUG_PRINT
    printf("_get_standard_sizes: nt < 0 in _get_standard_sizes\n");
#endif
    _set_latest_error(SES_OBJECT_OUT_OF_RANGE);
    return (char**)NULL;
  }
  int index = my_get_index(the_tid);
  if (index < 0) {
#ifdef DEBUG_PRINT
    printf("_get_standard_sizes: my_get_index failed in _get_standard_sizes with tid = %ld\n", the_tid);
#endif
    _set_latest_error(SES_FUNCTION_FAIL_ERROR);
    return (char**)NULL;
  }

  /*  get the standard sizes array */

  char** size_arrays = (char**)NULL;
  size_arrays = _get_standard_sizes_for_table_as_char(_the_tables[index], nr, nt, ntab);

  return size_arrays;
}


long*      _get_standard_sizes(ses_table_id the_tid, long nr, long nt, long ntab){ 


  /*  error check the arguments */

  if (_is_valid_tid(the_tid) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("_get_standard_sizes: invalid tid in _get_standard_sizes\n");
#endif
    _set_latest_error(SES_FUNCTION_FAIL_ERROR);
    return (long*)NULL;
  }

  if (nr < 0) {
#ifdef DEBUG_PRINT
    printf("_get_standard_sizes: nr < 0 in _get_standard_sizes\n");
#endif
    _set_latest_error(SES_OBJECT_OUT_OF_RANGE);
    return (long*)NULL;
  }

  if (nt < 0) {
#ifdef DEBUG_PRINT
    printf("_get_standard_sizes: nt < 0 in _get_standard_sizes\n");
#endif
    _set_latest_error(SES_OBJECT_OUT_OF_RANGE);
    return (long*)NULL;
  }
  int index = my_get_index(the_tid);
  if (index < 0) {
#ifdef DEBUG_PRINT
    printf("_get_standard_sizes: my_get_index failed in _get_standard_sizes with tid = %ld\n", the_tid);
#endif
    _set_latest_error(SES_FUNCTION_FAIL_ERROR);
    return (long*)NULL;
  }

  /*  get the standard sizes array */

  long* size_arrays = (long*)NULL;
  size_arrays = _get_standard_sizes_for_table(_the_tables[index], nr, nt, ntab);


  return size_arrays;
}



ses_label _get_standard_label_at_index(ses_table_id the_tid, int index) {

  ses_label return_value = (ses_label)NULL;

  if ((the_tid >= 300) && (the_tid <= 306)) {

    if (index < 7) {
      return_value = malloc(sizeof(char)*SES_MAX_LABEL_SIZE);
    }

    if (index == 0) {
      strcpy(return_value , "nr (number densities)");
    }
    if (index == 1) {
      strcpy(return_value , "nt (number temperatures)");
    }
    if (index == 2) {
      strcpy(return_value , "r - density (Mg/m3)");
    }
    if (index == 3) {
      strcpy(return_value , "t - temperature (K)");
    }
    if (index == 4) {
      strcpy(return_value , "p - pressure (GPa)");
    }
    if (index == 5) {
      strcpy(return_value , "e - energy (MJ/kg)");
    }
    if (index == 6) {
      strcpy(return_value , "a - free energy (MJ/kg)");
    }
  }

  return return_value;

}



ses_label* _get_standard_labels(ses_table_id the_tid) {


  ses_label* label_array = (ses_label*)NULL;

  /*  error check the arguments */

  if (_is_valid_tid(the_tid) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("_get_standard_labels: invalid tid in _get_standard_labels\n");
#endif
    _set_latest_error(SES_FUNCTION_FAIL_ERROR);
    return (ses_label*)NULL;
  }

  /*  get the index of the table */

  int index = my_get_index(the_tid);
  if (index < 0) {
#ifdef DEBUG_PRINT
    printf("_get_standard_labels: my_get_index failed in _get_standard_labels\n");
#endif
    _set_latest_error(SES_FUNCTION_FAIL_ERROR);
    return (ses_label*)NULL;
  }

  label_array = _get_standard_labels_for_table(_the_tables[index]);

  return label_array;
}


/*-----------------*/

int my_get_index(ses_table_id the_tid) {

  /*  find the table index that corresponds to the given table id */

  /* #ifdef DEBUG_PRINT */
  /*   printf("_ses_standard::my_get_index: tid: %ld\n", the_tid); */
  /* #endif */

#ifdef DEBUG_PRINT
    printf("my_get_index: the_tid: %ld \n", the_tid);
#endif

  if (_is_valid_tid(the_tid) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("my_get_index: Invalid tid passed into my_get_index\n");
#endif
    _set_latest_error(SES_INVALID_TID);
    return -1;
  }


  int return_value = -1;

  int i = 0;

#ifdef DEBUG_PRINT
  printf("_ses_standard::my_get_index: number of tables: %d\n", NUMBER_TABLES);
#endif

  for (i=0; i < NUMBER_TABLES; i++) {
#ifdef DEBUG_PRINT
      printf("_ses_standard::my_get_index: _the_tables[%i]->_the_tid: %ld\n", i, _the_tables[i]->_the_tid);
#endif
    if (_the_tables[i]->_the_tid == the_tid) {
      return_value = (int)i;
    }

//#ifdef DEBUG_PRINT /* GINGER comment this out! */
//    printf("_ses_standard::my_get_index: _the_tables[%d]->the_tid = %ld\n", i, _the_tables[i]->_the_tid); /**/
//#endif /**/

  }


  return return_value;

}




ses_boolean my_read_tables(void) {

  ses_boolean return_value = SES_TRUE;

  /*  put data into table default values */


  int index = 0;

  int MAX_LENGTH_STRING = 1000;

  char* the_string = (char*)NULL;  		         //  initialize 
  
  the_string =  malloc(sizeof(char) * MAX_LENGTH_STRING);  //  create memory
  strcpy(the_string, "{ tid: 100, num_arrays : 2, description : \"table that contains descriptions of other tables\", sizes : [ (1), (nr) ]  , labels: [ \"number of table descriptions\",\"string containing table descriptions\"]}");

  _the_tables[index] = _construct_standard_table((ses_string)the_string);
#ifdef DEBUG_MY_READ_TABLES
   _print_standard_table(_the_tables[index]);
#endif
  free(the_string);
  the_string = (char*)NULL;
  index++;


  int i = 0;
  for (i=101; i < 200; i++) {
     the_string =  malloc(sizeof(char) * MAX_LENGTH_STRING);  //  create memory
    
     sprintf(the_string, "{ tid: %d , num_arrays : 1, description : \"comments table\", sizes : [ (nr) ]  , labels: [ \"comments %d \"]}", i, i);

     _the_tables[index] = _construct_standard_table((ses_string)the_string);
#ifdef DEBUG_MY_READ_TABLES
     _print_standard_table(_the_tables[index]);
#endif
    free(the_string);
    the_string = (char*)NULL;
    
    index++;

  }

  the_string =  malloc(sizeof(char) * MAX_LENGTH_STRING);  //  create memory
  strcpy(the_string, "{ tid: 201, num_arrays : 5, description : \"Atomic Number Atomic Mass  Normal Density\", sizes : [ (1), (1), (1), (1), (1) ]  , labels: [ \"zbar - mean atomic number\",\"abar - mean atomic mass\", \"rho0 - normal (solid) density\", \"b0 - solid bulk modulus\", \"cex - exchange coefficient\"]}");

  _the_tables[index] = _construct_standard_table((ses_string)the_string);
#ifdef DEBUG_MY_READ_TABLES
   _print_standard_table(_the_tables[index]);
#endif
  free(the_string);
  the_string = (char*)NULL;
  index++;

  the_string =  malloc(sizeof(char) * MAX_LENGTH_STRING);  //  create memory
  strcpy(the_string, "{ tid: 301, num_arrays : 7, num_independent : 2, description : \"Total EOS (304 + 305 + 306)\", sizes : [ (1), (1), (nr), (nt), (nr*nt), (nr*nt), (nr*nt) ]  , labels: [ \"nr (number densities)\",\"nt (number temperatures)\", \"r - density (Mg/m3)\", \"t - temperature (K)\", \"p - pressure (GPa)\", \"e - energy (MJ/kg)\", \"a - free energy (MJ/kg)\"] }");

  _the_tables[index] = _construct_standard_table((ses_string)the_string);
#ifdef DEBUG_MY_READ_TABLES
   _print_standard_table(_the_tables[index]);
#endif
  free(the_string);
  the_string = (char*)NULL;
  index++;

  the_string =  malloc(sizeof(char) * MAX_LENGTH_STRING);  //  create memory
  strcpy(the_string, "{ tid: 303, num_arrays : 7, num_independent : 2, description : \"Total Ion EOS Plus Cold Curve (305+306)\", sizes : [ (1), (1), (nr), (nt), (nr*nt), (nr*nt), (nr*nt) ]  , labels: [ \"nr (number densities)\",\"nt (number temperatures)\", \"r - density (Mg/m3)\", \"t - temperature (K)\", \"p - pressure (GPa)\", \"e - energy (MJ/kg)\", \"a - free energy (MJ/kg)\"]}");

  _the_tables[index] = _construct_standard_table((ses_string)the_string);
#ifdef DEBUG_MY_READ_TABLES
   _print_standard_table(_the_tables[index]);
#endif
  free(the_string);
  the_string = (char*)NULL;
  index++;


  the_string =  malloc(sizeof(char) * MAX_LENGTH_STRING);  //  create memory
  strcpy(the_string, "{ tid: 304, num_arrays : 7, num_independent : 2, description : \"Electron EOS\", sizes : [ (1), (1), (nr), (nt), (nr*nt), (nr*nt), (nr*nt) ]  , labels: [ \"nr (number densities)\",\"nt (number temperatures)\", \"r - density (Mg/m3)\", \"t - temperature (K)\", \"p - pressure (GPa)\", \"e - energy (MJ/kg)\", \"a - free energy (MJ/kg)\"]}");

  _the_tables[index] = _construct_standard_table((ses_string)the_string);
#ifdef DEBUG_MY_READ_TABLES
   _print_standard_table(_the_tables[index]);
#endif
  free(the_string);
  the_string = (char*)NULL;
  index++;


  the_string =  malloc(sizeof(char) * MAX_LENGTH_STRING);  //  create memory
  strcpy(the_string, "{ tid: 305, num_arrays : 7, num_independent : 2, description : \"Ion EOS (including Zero Point)\", sizes : [ (1), (1), (nr), (nt), (nr*nt), (nr*nt), (nr*nt) ]  , labels: [ \"nr (number densities)\",\"nt (number temperatures)\", \"r - density (Mg/m3)\", \"t - temperature (K)\", \"p - pressure (GPa)\", \"e - energy (MJ/kg)\", \"a - free energy (MJ/kg)\"]}");

  _the_tables[index] = _construct_standard_table((ses_string)the_string);
#ifdef DEBUG_MY_READ_TABLES
   _print_standard_table(_the_tables[index]);
#endif
  free(the_string);
  the_string = (char*)NULL;
  index++;


  the_string =  malloc(sizeof(char) * MAX_LENGTH_STRING);  //  create memory
  strcpy(the_string, "{ tid: 306, num_arrays : 7, num_independent : 2, description : \"Cold Curve (No Zero Point)\", sizes : [ (1), (1), (nr), (nt), (nr*nt), (nr*nt), (nr*nt) ]  , labels: [ \"nr (number densities)\",\"nt (number temperatures)\", \"r - density (Mg/m3)\", \"t - temperature (K)\", \"p - pressure (GPa)\", \"e - energy (MJ/kg)\", \"a - free energy (MJ/kg)\"]}");

  _the_tables[index] = _construct_standard_table((ses_string)the_string);
#ifdef DEBUG_MY_READ_TABLES
   _print_standard_table(_the_tables[index]);
#endif
  free(the_string);
  the_string = (char*)NULL;
  index++;

  the_string =  malloc(sizeof(char) * MAX_LENGTH_STRING);  //  create memory
  strcpy(the_string, "{ tid: 311, num_arrays : 7, num_independent : 2, description : \"Maxwell Constructed Data\", sizes : [ (1), (1), (nr), (nt), (nr*nt), (nr*nt), (nr*nt) ]  , labels: [ \"nr (number densities)\",\"nt (number temperatures)\", \"r - density (Mg/m3)\", \"t - temperature (K)\", \"p - pressure (GPa)\", \"e - energy (MJ/kg)\", \"a - free energy (MJ/kg)\"]}");

  _the_tables[index] = _construct_standard_table((ses_string)the_string);
#ifdef DEBUG_MY_READ_TABLES
   _print_standard_table(_the_tables[index]);
#endif
  free(the_string);
  the_string = (char*)NULL;
  index++;


  the_string =  malloc(sizeof(char) * MAX_LENGTH_STRING);  //  create memory
  strcpy(the_string, "{ tid: 321, num_arrays : 5, num_independent : 2, description : \"Phase-Specific Mass Fraction\", sizes : [ (1), (1), (nr), (nt), (nr*nt)  ]  , labels: [ \"nr (number densities)\", \"nt(number temperatures)\", \"r - density (Mg/m3)\", \"t - temperature (K)\" , \"mass fraction: phase 1\"]}");

  _the_tables[index] = _construct_standard_table((ses_string)the_string);
#ifdef DEBUG_MY_READ_TABLES
   _print_standard_table(_the_tables[index]);
#endif
  free(the_string);
  the_string = (char*)NULL;
  index++;
 
  the_string =  malloc(sizeof(char) * MAX_LENGTH_STRING);  //  create memory
  strcpy(the_string, "{ tid: 401, num_arrays : 9, num_independent : 1, description : \"Vaporization Table\", sizes : [ (1), (nt), (nt), (nt), (nt), (nt), (nt), (nt), (nt)]  , labels: [ \"nt (number temperatures)\",\"p - vapor pressure (GPa)\", \"t - temperature (K)\", \"rg - vapor density on coexistence line (Mg/m3)\", \"rl - density of liquid or solid on coexistence line (Mg/m3)\", \"eg - internal energy of vapor on coexistence line (MJ/kg)\", \"el - internal energy of liquid or solid on coexistence line (MJ/kg)\", \"ag - free energy of vapor on coexistence line (MJ/kg)\", \"al - free energy of liquid or solid on coexistence line (MJ/kg)\"]}");
  _the_tables[index] = _construct_standard_table((ses_string)the_string);
#ifdef DEBUG_MY_READ_TABLES
   _print_standard_table(_the_tables[index]);
#endif
  free(the_string);
  the_string = (char*)NULL;
  index++;

  the_string =  malloc(sizeof(char) * MAX_LENGTH_STRING);  //  create memory
  strcpy(the_string, "{ tid: 411, num_arrays : 8, num_independent: 2, description : \"Solid Melt Table\", sizes : [ (1), (1), (nr), (nt), (nr), (nr), (nr), (nr)]  , labels: [ \"nr (number densities)\",\"nt = 1 (number temperatures)\", \"rs - density of solid on melt (Mg/m3)\", \"t = 0 temperature (K)\", \"tm - Melt temperature (K)\", \"pm - Melt pressure (GPa)\", \"es - Internal energy of solid on the melt line (MJ/kg)\", \"as - Free energy of solid on the melt line (MJ/kg)\"]}");

  _the_tables[index] = _construct_standard_table((ses_string)the_string);
#ifdef DEBUG_MY_READ_TABLES
   _print_standard_table(_the_tables[index]);
#endif
  free(the_string);
  the_string = (char*)NULL;
  index++;
 
  the_string =  malloc(sizeof(char) * MAX_LENGTH_STRING);  //  create memory
  strcpy(the_string, "{ tid: 412, num_arrays : 8, num_independent : 2, description : \"Liquid Melt Table\", sizes : [ (1), (1), (nr), (nt), (nr), (nr), (nr), (nr) ]  , labels: [ \"nr (number densities)\",\"nt = 1 (number temperatures)\", \"rs - density of liquid on melt (Mg/m3)\", \"t = 0 temperature (K)\", \"tm - Melt temperature (K)\", \"pm - Melt pressure (GPa)\", \"es - Internal energy of liquid on the melt line (MJ/kg)\", \"as - Free energy of liquid on the melt line (MJ/kg)\"]}");

  _the_tables[index] = _construct_standard_table((ses_string)the_string);
#ifdef DEBUG_MY_READ_TABLES
   _print_standard_table(_the_tables[index]);
#endif
  free(the_string);
  the_string = (char*)NULL;
  index++;


  the_string =  malloc(sizeof(char) * MAX_LENGTH_STRING);  //  create memory
  strcpy(the_string, "{ tid: 431, num_arrays : 5, num_independent : 2, description : \"Shear Modulus Table\", sizes : [ (1), (1), (nr), (nt), (nr)]  , labels: [ \"nr (number densities)\",\"nt = 1 (number temperatures)\", \"r - density (Mg/m3)\", \"t = 0 temperature (K)\", \"g - shear modulus (GPa)\"]}");

  _the_tables[index] = _construct_standard_table((ses_string)the_string);
#ifdef DEBUG_MY_READ_TABLES
   _print_standard_table(_the_tables[index]);
#endif
  free(the_string);
  the_string = (char*)NULL;
  index++;

  the_string =  malloc(sizeof(char) * MAX_LENGTH_STRING);  //  create memory
  strcpy(the_string, "{ tid: 432, num_arrays : 6, num_independent : 2, description : \"Shear Modulus Table with Melt Correlation\", sizes : [ (1), (1), (nr), (nt), (nr), (nr)]  , labels: [ \"nr (number densities)\",\"nt = 1 (number temperatures)\", \"r - density (Mg/m3)\", \"t = 0 temperature (K)\", \"g - shear modulus (GPa)\", \"g - shear modulus (GPa) @ melt\"]}");

  _the_tables[index] = _construct_standard_table((ses_string)the_string);
#ifdef DEBUG_MY_READ_TABLES
   _print_standard_table(_the_tables[index]);
#endif
  free(the_string);
  the_string = (char*)NULL;
  index++;
  

  //   NOT CURRENTLY READING 2nd label correctly -- correct if no comma in label string (HMA 12/11/12)
  the_string =  malloc(sizeof(char) * MAX_LENGTH_STRING);  //  create memory
  strcpy(the_string, "{ tid: 501, num_arrays : 2, description : \"Opacity Grid Boundary: Calculated vs Interpolated\", sizes : [ (1), (2*nr) ]  , labels: [ \"n (number of temperature-density pairs)\", \"(t r) - (log-10 temperature K   log-10 density Mg/m3) pair\" ] } ");

  _the_tables[index] = _construct_standard_table((ses_string)the_string);
#ifdef DEBUG_MY_READ_TABLES
   _print_standard_table(_the_tables[index]);
#endif
  free(the_string);
  the_string = (char*)NULL;

  index++;
  
  
  the_string =  malloc(sizeof(char) * MAX_LENGTH_STRING);  //  create memory
  strcpy(the_string, "{ tid: 502, num_arrays : 5, num_independent : 2, description : \"Rosseland Mean Opacity (cm2/g)\", sizes : [ (1), (1), (nr), (nt), (nr*nt) ]  , labels: [ \"nr (number densities)\",\"nt (number temperatures)\", \"r - log-10 density (g/cm3)\", \"t - log-10 density (eV)\", \"x - log-10 Rosseland Mean Opacity (cm2/g)\"]}");

  _the_tables[index] = _construct_standard_table((ses_string)the_string);
#ifdef DEBUG_MY_READ_TABLES
   _print_standard_table(_the_tables[index]);
#endif
  free(the_string);
  the_string = (char*)NULL;
  index++;
 
  the_string =  malloc(sizeof(char) * MAX_LENGTH_STRING);  //  create memory
  strcpy(the_string, "{ tid: 503, num_arrays : 5,  num_independent : 2, description : \"Electron Conductive Opacity1(cm2/g)\", sizes : [ (1), (1), (nr), (nt), (nr*nt) ]  , labels: [ \"nr (number densities)\",\"nt (number temperatures)\", \"r - log-10 density (g/cm3)\", \"t - log-10 density (eV)\", \"x - log-10 Electron Conductive Opacity1 (cm2/g)\"]}");

  _the_tables[index] = _construct_standard_table((ses_string)the_string);
#ifdef DEBUG_MY_READ_TABLES
   _print_standard_table(_the_tables[index]);
#endif
  free(the_string);
  the_string = (char*)NULL;
  index++;
 
  the_string =  malloc(sizeof(char) * MAX_LENGTH_STRING);  //  create memory
  strcpy(the_string, "{ tid: 504, num_arrays : 5, num_independent : 2, description : \"Mean Ion Charge1 (free electrons per atom)\", sizes : [ (1), (1), (nr), (nt), (nr*nt) ]  , labels: [ \"nr (number densities)\",\"nt (number temperatures)\", \"r - log-10 density (g/cm3)\", \"t - log-10 density (eV)\", \"x - log-10 Mean Ion Charge1 (free electrons per atom)\"]}");

  _the_tables[index] = _construct_standard_table((ses_string)the_string);
#ifdef DEBUG_MY_READ_TABLES
   _print_standard_table(_the_tables[index]);
#endif
  free(the_string);
  the_string = (char*)NULL;
  index++;


  the_string =  malloc(sizeof(char) * MAX_LENGTH_STRING);  //  create memory
  strcpy(the_string, "{ tid: 505, num_arrays : 5, num_independent : 2, description : \"Planck Mean Opacity (cm2/g)\", sizes : [ (1), (1), (nr), (nt), (nr*nt) ]  , labels: [ \"nr (number densities)\",\"nt (number temperatures)\", \"r - log-10 density (g/cm3)\", \"t - log-10 density (eV)\", \"x - log-10 Planck Mean Opacity (cm2/g)\"]}");

  _the_tables[index] = _construct_standard_table((ses_string)the_string);
#ifdef DEBUG_MY_READ_TABLES
   _print_standard_table(_the_tables[index]);
#endif
  free(the_string);
  the_string = (char*)NULL;
  index++;

  the_string =  malloc(sizeof(char) * MAX_LENGTH_STRING);  //  create memory
  strcpy(the_string, "{ tid: 601, num_arrays : 5, num_independent : 2, description : \"Mean Ion Charge2 (free electrons per atom)\", sizes : [ (1), (1), (nr), (nt), (nr*nt) ]  , labels: [ \"nr (number densities)\",\"nt (number temperatures)\", \"r - log-10 density (g/cm3)\", \"t - log-10 density (eV)\", \"x - log-10 Mean Ion Charg2 (cm2/g)\"]}");

  _the_tables[index] = _construct_standard_table((ses_string)the_string);
#ifdef DEBUG_MY_READ_TABLES
   _print_standard_table(_the_tables[index]);
#endif
  free(the_string);
  the_string = (char*)NULL;
  index++;

  the_string =  malloc(sizeof(char) * MAX_LENGTH_STRING);  //  create memory
  strcpy(the_string, "{ tid: 602, num_arrays : 5, num_independent : 2, description : \"Electrical Conductivity (per sec)\", sizes : [ (1), (1), (nr), (nt), (nr*nt) ]  , labels: [ \"nr (number densities)\",\"nt (number temperatures)\", \"r - log-10 density (g/cm3)\", \"t - log-10 density (eV)\", \"x - log-10 Electrical Conductivity (per sec)\"]}");

  _the_tables[index] = _construct_standard_table((ses_string)the_string);
#ifdef DEBUG_MY_READ_TABLES
   _print_standard_table(_the_tables[index]);
#endif
  free(the_string);
  the_string = (char*)NULL;
  index++;

  the_string =  malloc(sizeof(char) * MAX_LENGTH_STRING);  //  create memory
  strcpy(the_string, "{ tid: 603, num_arrays : 5, num_independent : 2, description : \"Thermal Conductivity (per cm-sec)\", sizes : [ (1), (1), (nr), (nt), (nr*nt) ]  , labels: [ \"nr (number densities)\",\"nt (number temperatures)\", \"r - log-10 density (g/cm3)\", \"t - log-10 density (eV)\", \"x - log-10 Thermal Conductivity (per cm-sec)\"]}");

  _the_tables[index] = _construct_standard_table((ses_string)the_string);
#ifdef DEBUG_MY_READ_TABLES
   _print_standard_table(_the_tables[index]);
#endif
  free(the_string);
  the_string = (char*)NULL;
  index++;

  the_string =  malloc(sizeof(char) * MAX_LENGTH_STRING);  //  create memory
  strcpy(the_string, "{ tid: 604, num_arrays : 5, num_independent : 2, description : \"Thermoelectric Coefficient(per cm-sec)\", sizes : [ (1), (1), (nr), (nt), (nr*nt) ]  , labels: [ \"nr (number densities)\",\"nt (number temperatures)\", \"r - log-10 density (g/cm3)\", \"t - log-10 density (eV)\", \"x - log-10 Thermoelectric Coefficient (per cm-sec)\"]}");

  _the_tables[index] = _construct_standard_table((ses_string)the_string);
#ifdef DEBUG_MY_READ_TABLES
   _print_standard_table(_the_tables[index]);
#endif
  free(the_string);
  the_string = (char*)NULL;
  index++;

  the_string =  malloc(sizeof(char) * MAX_LENGTH_STRING);  //  create memory
  strcpy(the_string, "{ tid: 605, num_arrays : 5, num_independent : 2, description : \"Electron Conductive Opacity2(cm2/g)\", sizes : [ (1), (1), (nr), (nt), (nr*nt) ]  , labels: [ \"nr (number densities)\",\"nt (number temperatures)\", \"r - log-10 density (g/cm3)\", \"t - log-10 density (eV)\", \"x - log-10 Electron Conductive Opacity2(cm2/g)\"]}");

  _the_tables[index] = _construct_standard_table((ses_string)the_string);
#ifdef DEBUG_MY_READ_TABLES
   _print_standard_table(_the_tables[index]);
#endif
  free(the_string);
  the_string = (char*)NULL;
  index++;


  return return_value;
}


ses_boolean _is_valid_mid(ses_material_id the_mid) {

  /*  return whether the mid is a valid mid */

  ses_boolean return_value = SES_TRUE;
  if (the_mid <= 0) {
    return_value = SES_FALSE;
  }

  return return_value;
}

ses_boolean _is_valid_tid(ses_table_id the_tid){

        ses_boolean return_value = SES_TRUE;
        if (the_tid < 100) return_value = SES_FALSE;
	return return_value;
}

ses_boolean _is_valid_open_flag(ses_open_type open_flag) {

  /*  return whether the open_flag is a valid open flag */

  ses_boolean return_value = SES_FALSE;

  if ((open_flag == 'R') || (open_flag == 'W') || (open_flag == 'A') || (open_flag == 'C')) {
    return_value = SES_TRUE;
  }
  return return_value;

}
ses_boolean _is_valid_file_type(ses_file_type the_type) {

  /*  return whether the file type is a valid file type */

  ses_boolean return_value = SES_FALSE;


  if ((the_type == BINARY_TYPE) || (the_type == ASCII_TYPE) || (the_type == XML_TYPE) ) {
    return_value = SES_TRUE;
  }
  if (the_type == LLNL_TYPE) {
	return_value = SES_TRUE;
  }



  return return_value;
}





ses_error_flag _add_user_table(ses_table_id the_tid) {

  ses_error_flag return_value = SES_NO_ERROR;

  int new_table_index = NUMBER_TABLES;

#ifdef DEBUG_PRINT
    printf("\n_ses_standard::_add_user_table. NUMBER_TABLES: %d\n", NUMBER_TABLES);
#endif


  /*  increase the memory for the tables array by 1 */

  _the_tables = (struct _standard_table**)realloc(_the_tables, sizeof(struct _standard_table)*(NUMBER_TABLES + 1));
  if (_the_tables == (struct _standard_table**)NULL) {
#ifdef DEBUG_PRINT
    printf("_add_user_table:  memory reallocation error \n");
#endif
    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);
    return SES_MEMORY_ALLOCATION_ERROR;
  }


  NUMBER_TABLES++;

  _the_tables[new_table_index] = my_construct_standard_table();

  _the_tables[new_table_index]->_the_tid = the_tid;

#ifdef DEBUG_PRINT
    printf("_ses_standard::_add_user_table. NUMBER_TABLES: %d, the_tid: %ld, new_table_index: %d\n", NUMBER_TABLES, the_tid, new_table_index);
#endif


  return return_value;
}
ses_boolean _is_standard_table_defined(ses_table_id the_tid) {
  /*  find the_tid in the table */

  ses_boolean return_value = SES_FALSE;

  int i = 0;
  for (i = 0; i < NUMBER_TABLES; i++) {
    if (the_tid == _the_tables[i]->_the_tid) {
      return_value = SES_TRUE;
    }
  }


  return return_value;
}

ses_error_flag _add_standard_table(struct _standard_table* the_table) {
	
  ses_error_flag return_value = SES_NO_ERROR;

  if (_is_standard_table_defined(the_table->_the_tid) == SES_FALSE) {
    int new_table_index = NUMBER_TABLES;

    /*  increase the memory for the tables array by 1 */

    _the_tables = (struct _standard_table**)realloc(_the_tables, sizeof(struct _standard_table*)*(NUMBER_TABLES + 1));
    if (_the_tables == (struct _standard_table**)NULL) {
#ifdef DEBUG_PRINT
      printf("_add_standard_table:  memory reallocation error \n");
#endif
      _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);
      return SES_MEMORY_ALLOCATION_ERROR;
    }


    NUMBER_TABLES++;

#ifdef DEBUG_PRINT
      //GINGER!
    printf ("_ses_standard::_add_standard_table: call _copy_standard_table, NUMBER_TABLES: %d, table_index: %d\n", NUMBER_TABLES, new_table_index);
      printf("\tthe_table's tid: %ld, nr: %ld, nt: %ld\n", the_table->_the_tid, the_table->_nr, the_table->_nt);
#endif

    _the_tables[new_table_index] = _copy_standard_table(the_table);
    //_destruct_standard_table(the_table);
    //free(the_table);
    the_table = (struct _standard_table*)NULL;
    return_value = SES_NO_ERROR;


  }	

  return return_value;
}

/*  removing function 4/2/2013 */
/*ses_material_id _get_material_id_from_name(ses_string material_name) {


  ses_boolean my_islike(ses_string table_name, ses_string possibility);


  ses_material_id return_value = 0;


  struct _id_name {
      ses_material_id mid;
      ses_string name;
  };


  const int NUM_MATERIALS = 2;
  struct _id_name the_materials[NUM_MATERIALS];

  the_materials[0].mid = 3200;
  the_materials[0].name = "lead";

  the_materials[1].mid = 3332;
  the_materials[1].name = "copper";

  int i=0;

  for (i=0; i<NUM_MATERIALS; i++) {

    if (my_islike(the_materials[i].name, material_name)) {
      return_value = the_materials[i].mid;
      break;
    }
  }


  return return_value;
}
*/
/*ses_boolean my_islike(ses_string table_name, ses_string possibility) {

  ses_material_id return_value = SES_TRUE;
  char* ptr = strstr(table_name, possibility);

  if (ptr == (char*)NULL) {
    return_value = SES_FALSE;
  }
  return return_value;

}
*/
ses_string* _get_user_defined_table_strings(int* number_strings)
{
  ses_string* return_value = (ses_string*)NULL;

  //  count the number of user defined tables
  int i = 0;
  int num = 0;
  for (i = 0; i < NUMBER_TABLES; i++) {
    if (_the_tables[i]->_is_user_defined == SES_TRUE) {

      num++;
    }
  }
  *number_strings = num;

  if (num > 0) {
	
    //  create the return strings array
    return_value = calloc(num, sizeof(ses_string));
    int index = 0;
    for (i = 0; i < NUMBER_TABLES; i++) {

      //  create the string from the table
      if (_the_tables[i]->_is_user_defined == SES_TRUE) {

	ses_string the_string = _get_table_string(*_the_tables[i]);
        return_value[index] = the_string;
	index++;
      }
			
    }
  }

  return return_value;
}
ses_boolean _get_more_table_definitions(ses_file_handle the_handle) {
    
    //  called right at the end of ses_open, the _ses_standard tables data structure
    //  has been loaded with the standard table definitions
    
    //  setup to 100 table
    
    //  if it did the setup correctly, read table definitions
    //  add those table definitions to the standard tables
    
    ses_boolean return_value = SES_TRUE;
    ses_boolean read_100 = SES_FALSE;
    if (((FILE_LIST[the_handle]->_the_handle)->_the_open_mode != 'W') && (FILE_LIST[the_handle]->_the_handle->_the_open_mode != 'A') && (ses_is_valid(the_handle) == SES_TRUE)) {
        
        //  get the materials on the handle
        
        long size = 0;
        ses_material_id_reference the_materials = ses_get_materials(the_handle, &size);
#ifdef DEBUG_PRINT
        printf("_get_more_table_definitions:: ses_get_material:size: %ld\n", size);
#endif
        
        if (the_materials == (ses_material_id_reference)NULL) {
            /*  error -- no materials on the handle */
            return SES_FALSE;
        }
        
        //  for each material, tid=100 pair, set up to it
        
        int i = 0;
        int dim = 0;
        ses_table_id my_table_id = 100;
        
        for (i = 0; i < size; i++) {
            
            ses_error_flag didit_setup = ses_setup(the_handle, the_materials[i], my_table_id);
            ses_string the_string = (ses_string)NULL;
            if (didit_setup == SES_NO_ERROR) {
                
                read_100 = SES_FALSE;
                /*  ready to read the 100 table */
                int i2 = 0;
                ses_number number_tables_defined = 0;
                ses_word_reference the_data = (ses_word_reference)NULL;
                
                while (ses_has_next(the_handle) == SES_TRUE) {
                    
                    ses_number the_size = ses_array_size_next(the_handle);   /*  number of words in the array */
#ifdef DEBUG_PRINT
                    printf("_get_more_table_definitions:: the_size: %ld\n", the_size);
#endif
                    
                    if (i2 == 0) {
                        the_data = ses_read_next(the_handle);
#ifdef DEBUG_PRINT
                        printf("_get_more_table_definitions: i2 ==0: the_data: %s\n", (char*)the_data);
#endif
                        
                        if (the_data != (ses_word_reference)NULL) {
                            number_tables_defined = (ses_number)(the_data[0]);    //  number of tables defined
                            free(the_data);
                            the_data = (ses_word_reference)NULL;
                        }
                        
                    }
                    else {
                        
                        /*  for the character string, never flip */
                        ses_boolean needs_flip = FILE_LIST[the_handle]->_the_handle->_needs_flip;
                        FILE_LIST[the_handle]->_the_handle->_needs_flip = SES_FALSE;
                        
                        /*  add the table definitions */
                        the_data = ses_read_next(the_handle);
                        
                        FILE_LIST[the_handle]->_the_handle->_needs_flip = needs_flip;
                        int last_index = 0;
                        
                        // It could be either a binary or ascii file we are looking at:
                        if (FILE_LIST[the_handle]->_the_handle->_filetype == BINARY_TYPE){

                            /*  turn the data into a string */
                            dim = ((the_size)*sizeof(ses_word))/number_tables_defined;
                            
                            /*  compute the size of each string to an 8-byte boundary */
                            the_string = malloc(sizeof(char) * (dim) * number_tables_defined + 1);
                            int k6 = 0;
                            for (k6=0; k6 < sizeof(char)*dim*number_tables_defined + 1; k6++) {
                                the_string[k6] = ' ';
                            }
                            //  add an extra for the null terminator
                        
                        
                            if (the_data != (ses_word_reference)NULL) {
                                
                                int k = 0;
                                
                                union {
                                    double the_double;
                                    char the_chars[8];
                                } my_union;
                                
                                int m = 0;

                                for (k = 0; k < the_size; k++) {

                                    my_union.the_double = the_data[k];
                                    int l = 0;
                                    for (l = 0; l < 8; l++) {
                                        the_string[m + l] = my_union.the_chars[l];
                                    }
                                    m = m + 8;
                                    
                                }
                                last_index = dim*number_tables_defined;
                                the_string[last_index] = '\0';
                                
                                
                            }
                            else {
                                the_string[0] = '\0';
                            }
                        }
                        else{
                            dim = strlen((char*)the_data);
                            last_index = dim;
                            the_string = strdup((char *)the_data);
                        }
                        free(the_data);
                        the_data = (ses_word_reference)NULL;
                        
                        int strings_left = number_tables_defined;
                        ses_string the_string1 = NULL;
                        ses_string the_string2 = NULL;
                        while (strings_left > 0) {
                            
                            
                            /*  turn the string into strings for each of the defined tables */
#ifdef DEBUG_PRINT
                            printf("_get_more_table_definitions:: dim: %d\n", dim);
#endif
                            int str_len = 0;
                            if (FILE_LIST[the_handle]->_the_handle->_filetype == BINARY_TYPE){
                                str_len = dim * 8 + 1;
                                the_string1 = malloc(sizeof(char) * str_len);
                                the_string2 = malloc(sizeof(char) * str_len);
                            }
                            else{
                                str_len = dim;
                                the_string1 = malloc(sizeof(char) * str_len);
                                the_string2 = malloc(sizeof(char) * str_len);
                                the_string1[0] = '\0';
                                the_string2[0] = '\0';
                            }
                            
                            int k7 = 0;
                            for (k7 = 0; k7 < sizeof(char) * str_len; k7++) {
                                the_string1[k7] = ' ';
                                the_string2[k7] = ' ';
                            }
                            
                            //  take out all the '\n'
                            int k2 = 0;
                            int s = 0;
#ifdef DEBUG_PRINT
                            printf("_get_more_table_definitions:: last_index: %d, strlen(the_string): %ld\n", last_index, strlen(the_string));
#endif
                            for (k2 = 0; k2 < last_index; k2++) {
                                if (the_string[k2] == '\n') {
                                }
                                else {
                                    the_string[s] = the_string[k2];
                                    s++;
                                }
                            }
                            
                            int start1 = 0;
                            int start2 = 0;
                            ses_boolean seen_non_delim = SES_FALSE;
                            ses_boolean seen_delim = SES_FALSE;
                            for (k2 = 0; k2 < last_index + 1; k2++) {
                                
                                if (seen_delim == SES_FALSE) {
                                    if (the_string[k2] == '@') {
                                        if (seen_non_delim == SES_TRUE) {
                                            the_string1[start1] = '\0';
                                            seen_delim = SES_TRUE;
                                        }
                                        else {
                                            k2++;
                                        }
                                    }
                                    else {
                                        seen_non_delim = SES_TRUE;
                                        the_string1[start1] = the_string[k2];
                                        start1++;
                                    }
                                }
                                else {
                                    the_string2[start2] = the_string[k2];
                                    start2++;
                                }
                                
                            } 
                            the_string2[start2] = '\0';
                            
                            free(the_string);
                            the_string = (ses_string)NULL;
                            
                            ses_boolean didit_define_error = SES_NO_ERROR;
#ifdef DEBUG_PRINT
                            printf("_get_more_table_definitions:: the_string1: %s\n", the_string1);
#endif
                            didit_define_error  = _add_table_definition(the_string1);
                            if (didit_define_error == SES_NO_ERROR) {
                                read_100 = SES_TRUE;
                                return_value = SES_TRUE;
                            }  //  if ses_no_error
                            the_string = malloc(sizeof(char) * (dim) * number_tables_defined + 1); 
                            int k6 = 0;
                            for (k6=0; k6 < sizeof(char)*dim*number_tables_defined; k6++) {
                                the_string[k6] = ' ';
                            }
#ifdef DEBUG_PRINT
                            printf("_get_more_table_definitions:: the_string2: %s\n", the_string2);
#endif
                            strcpy(the_string, the_string2);
                            
                            strings_left--;   
                            if (the_string1 != NULL) {
                                free(the_string1);
                                the_string1 = (ses_string)NULL;
                            }
                            if (the_string2 != NULL) {
                                free(the_string2);
                                the_string2 = (ses_string)NULL;
                            }
                            
                        }
                        
                        free(the_string);
                        the_string = (ses_string)NULL;
                        free(the_string1);
                        the_string1 = (ses_string)NULL;
                        free(the_string2);
                        the_string2 = (ses_string)NULL;
                        
                        
                    }
                    i2++;

                }
	/*  clear the setup object */

	_destruct_ses_setup(FILE_LIST[the_handle]->_the_setup);

      }

      if (the_string != NULL) {
         free(the_string);
         the_string = (ses_string)NULL;
      }
     }

    free(the_materials);
    the_materials = (ses_material_id_reference)NULL;

  }

  /*  if the file tables_def.ses_io exists, read in the new table definitions */
  
  if (read_100 == SES_FALSE) {
    ses_boolean didit_read = SES_FALSE;
    didit_read = _read_and_define_tables_from_file();
    if (didit_read == SES_FALSE) {
	return_value = SES_FALSE;
    }
  }


  return return_value;
}

ses_boolean _add_table_definition(ses_string the_data) {


  ses_boolean return_value = SES_TRUE;

  struct _standard_table* new_table = (struct _standard_table*)NULL;

#ifdef DEBUG_PRINT
  printf("_ses_standard::_add_table_definition\n");
#endif
    
  new_table = _construct_standard_table(the_data);
  new_table->_is_user_defined = SES_TRUE;
#ifdef DEBUG_PRINT
      printf("_ses_standard::After _construct_standard_table\n");
#endif

  if (new_table == (struct _standard_table*)NULL) {
    return SES_FALSE;
  }
  else {
    //  add the new table to the standard table list
    ses_error_flag didit_addit = SES_FALSE;
#ifdef DEBUG_PRINT
    printf("_ses_standard::_add_table_definition: call _add_standard_table the_data: %s\n", the_data);
      // GINGER
    printf("\t NEW_table's tid: %ld, nr: %ld, nt: %ld\n", new_table->_the_tid, new_table->_nr, new_table->_nt);

#endif
    didit_addit = _add_standard_table(new_table);
    if (didit_addit != SES_NO_ERROR) {
#ifdef DEBUG_PRINT
        printf("_ses_standard::_add_table_definition: Error adding a table. Not passed back: %s\n", the_data);
#endif

    }
  }
  _destruct_standard_table(new_table);
  free(new_table);

  new_table = (struct _standard_table*)NULL;

	
	
  return return_value;
}

ses_boolean _read_and_define_tables_from_file(void) {

  ses_boolean return_value = SES_FALSE;

  char* filename = "table_defs.sesio";
  FILE* cfh = fopen(filename, "r");  //  Note file not exist returns 0
  if (cfh == NULL) {
  }
  else {
    //  read till the eof -- each line is a table definition

    char line[10000];
    int i = 0;
    for (i = 0; i < 10000; i++) {
      line[i] = '\0';
    }
    /* ses_boolean didit_define = SES_FALSE; */
    while (fgets(line, sizeof(line), cfh) != NULL) {
      ses_string the_string = &line[0];

#ifdef DEBUG_PRINT
      printf("\n_ses_standard::_read_and_define_tables_from_file: line: %s\n", line);
#endif

      //  if not a comment, add the table definition
      if (line[0] != '#') {
      	/* didit_define = */ _add_table_definition(the_string);
      }
			
    }

    fclose(cfh);
    cfh = (FILE*)NULL;
    return_value = SES_TRUE;
  }

	
  return return_value;
}




