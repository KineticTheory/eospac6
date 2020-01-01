/*********************************************************************
 * Unit Test API Functions
 * ---------------------------------------------------------
 * Filetype: (SOURCE)
 *
 * Copyright -- see file named COPYRIGHTNOTICE
 *
 ********************************************************************/

#include "TEST_FUNCTIONS.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>
#include <math.h>
#include <unistd.h>
#include "eos_types_internal.h"
#include "eos_DataMap.h"
#include "eos_RecordType1.h"
#include "eos_RecordType4.h"
#include "eos_Interface.proto.h"

#include <limits.h>
#ifndef PATH_MAX
#define PATH_MAX 255
#endif

#include "eos_Utils.h"
#include "eos_SesUtils.h"
#include "ses_globals.h"

_EXTERN_C_HEAD_

/*!This function returns a list of all tableTypes associated with the specified tableNum */
EOS_INTEGER get_tableTypesFromSesameTableNumber (EOS_INTEGER tableNum, EOS_INTEGER *tableTypes) {
  EOS_INTEGER i, j=0;

  for (i=0; i<MAX_TYPES; i++) {
    if (tableNum == eos_TableList[i].tableNum && eos_TableList[i].subCategory != EOS_INTERNAL) {
      tableTypes[j] = eos_TableList[i].eosTableType;
      j++;
    }
  }
  return (j);
}

/*!This function is a wrapper around _eos_SetTableListReverseMap() */
void eos_SetTableListReverseMap()
{
  _eos_SetTableListReverseMap();
}

/*!This function is a wrapper around _eos_DestroyTableListReverseMap() */
void eos_DestroyTableListReverseMap()
{
  _eos_DestroyTableListReverseMap();
}

/*!This function gets the actual eos_Data object and the record type and pointer for
   the record associated with each table handle. */
void get_data(void)
{
  printf("*******************\n");
  printf("number of tables globally is %i \n", gEosDataMap.nTables);
}

/*!This function is a local implementation of the non-standard stricmp function */
int insensitive_strcmp(const char * cs,const char * ct)
{
  register signed char __res;

  while (1) {
    if ((__res = toupper( *cs ) - toupper( *ct++ )) != 0 || !*cs++)
      break;
  }

  return __res;
}

/*!This function gets the string representation of the data type, t. */
EOS_CHAR* get_tableType_str(EOS_INTEGER t)
{
  EOS_CHAR *s = NULL;
  if (! eos_TableListReverseMap) _eos_SetTableListReverseMap(); /* build reverse lookup for eos_TableList[] */
  s = (EOS_CHAR*)EOS_TYPE_TO_STRING(t);
  return s;
}

/*!This function gets the data type of the case-insensitive string representation, s. */
EOS_INTEGER get_tableType_FromStr(EOS_CHAR *s)
{
  int i;
  for (i = 0; i < MAX_TYPES; i++) {
    if (! insensitive_strcmp(eos_TableList[i].eosTableType_s, s))
      return eos_TableList[i].eosTableType;
  }
  return EOS_NullTable;
}

/*!This function gets the option flag of the case-insensitive string representation, s. */
EOS_INTEGER get_optionFlag_FromStr(EOS_CHAR *s)
{
  int i;
  for (i = 0; i < EOS_TOTAL_TABLE_OPTIONS; i++) {
    if (! insensitive_strcmp(eos_OptionFlags_str[i], s))
      return eos_OptionFlags[i];
  }
  return EOS_NullTable;
}

/*!This function gets the option flag of the case-insensitive string representation, s. */
EOS_CHAR* get_optionStr_FromFlag(EOS_INTEGER f)
{
  int i;
  for (i = 0; i < EOS_TOTAL_TABLE_OPTIONS; i++) {
    if (eos_OptionFlags[i] == f)
      return eos_OptionFlags_str[i];
  }
  return EOS_NullTable;
}

/*!This function returns a boolean if the specified option has been changed from the default. */
EOS_BOOLEAN isOptionFlagSet(EOS_INTEGER th, EOS_INTEGER flag)
{
  int i;
  EOS_INTEGER err=EOS_OK;
  EOS_INTEGER default_flags[EOS_TOTAL_TABLE_OPTIONS];
  EOS_REAL    default_flags_val[EOS_TOTAL_TABLE_OPTIONS];
  EOS_BOOLEAN retval = EOS_FALSE;

  /* populate arrays with default values */
  get_defaultOptionFlags_KeyValue(NULL, default_flags, default_flags_val);

  for (i = 0; i < EOS_TOTAL_TABLE_OPTIONS; i++) {
    EOS_INTEGER o = default_flags[i];
    eos_OptionValue *optVal;

    if (o != flag) continue;
    if (EOS_IS_HIDDEN_OPTION(o)) continue;

    if (EOS_IS_GENERAL_OPTION(o)) {
      eos_GetOptionEosDataMap (&gEosDataMap, th, o, &optVal, &err);

      switch(o) {
      case EOS_X_CONVERT:
      case EOS_Y_CONVERT:
      case EOS_F_CONVERT:
        retval = (default_flags_val[i] != optVal->rval) ? EOS_TRUE : EOS_FALSE;
        goto EXIT;
      default:
        retval = ((EOS_BOOLEAN)default_flags_val[i] != optVal->bval) ? EOS_TRUE : EOS_FALSE;
        goto EXIT;
      }
    }
    else if (EOS_IS_LOADING_OPTION(o)) {/* loading option */
      eos_GetOptionEosDataMap (&gEosDataMap, th, o, &optVal, &err);

      if (EOS_IS_LOADING_OPTION_WITH_REAL_VALUE(o)) {
        retval = (default_flags_val[i] != optVal->rval) ? EOS_TRUE : EOS_FALSE;
        goto EXIT;
      }
      else if (EOS_IS_LOADING_OPTION_WITH_INTEGER_VALUE(o)) {
        retval = ((EOS_INTEGER)default_flags_val[i] != optVal->ival) ? EOS_TRUE : EOS_FALSE;
        goto EXIT;
     }
      else { /* EOS_BOOLEAN option */
        retval = ((EOS_BOOLEAN)default_flags_val[i] != optVal->bval) ? EOS_TRUE : EOS_FALSE;
        goto EXIT;
      }
    }
    else if (EOS_IS_INTERPOLATION_OPTION(o)) {
      EOS_BOOLEAN optVal_b;
      eos_GetOptionEosInterpolation (&gEosInterpolation, th, o, &optVal_b, &err);
      retval = ((EOS_BOOLEAN)default_flags_val[i] != optVal_b) ? EOS_TRUE : EOS_FALSE;
      goto EXIT;
    }
  }

 EXIT:
  return(retval);
}

/*!This function gets all the default option flag string, flag and value triplets. */
void get_defaultOptionFlags_KeyValue(EOS_CHAR flags_str[][50], EOS_INTEGER *flags, EOS_REAL *flags_val)
{
  int i, j;
  EOS_INTEGER one=1, t=EOS_Pt_DT, m=0, th, err=EOS_OK;
  EOS_INTEGER _flags[EOS_TOTAL_TABLE_OPTIONS], temp_int;
  EOS_REAL    _flags_val[EOS_TOTAL_TABLE_OPTIONS];
  EOS_CHAR    _flags_str[EOS_TOTAL_TABLE_OPTIONS][50], temp_str[50];

  /* create a table handle and associated dummy objects in gEosDataMap and gEosInterpolation. Ignore error. */
  eos_CreateTables(&one, &t, &m, &th, &err);

  /* initialize local arrays for lexical sorting */
  for (i = 0; i < EOS_TOTAL_TABLE_OPTIONS; i++) {
    _flags[i] = eos_OptionFlags[i];
    strcpy(_flags_str[i], eos_OptionFlags_str[i]);
  }

  /* lexigraphically-sort _flags_str[] array and update corresponding _flags[i] array of option flags */
  for(i=0; i<EOS_TOTAL_TABLE_OPTIONS-1; ++i) {
    for(j=i+1; j<EOS_TOTAL_TABLE_OPTIONS ; ++j) {
      if(strcmp(_flags_str[i], _flags_str[j])>0) {
        strcpy(temp_str, _flags_str[i]);      temp_int  = _flags[i];
        strcpy(_flags_str[i], _flags_str[j]); _flags[i] = _flags[j];
        strcpy(_flags_str[j], temp_str);      _flags[j] = temp_int;
      }
    }
    _flags_val[i] = (EOS_REAL)0;
  }

  for (i = 0; i < EOS_TOTAL_TABLE_OPTIONS; i++) {
    EOS_INTEGER o = _flags[i];
    eos_OptionValue *optVal;

    if (EOS_IS_HIDDEN_OPTION(o)) continue;

    if (EOS_IS_GENERAL_OPTION(o)) {
      eos_GetOptionEosDataMap (&gEosDataMap, th, o, &optVal, &err);

      switch(o) {
      case EOS_X_CONVERT:
      case EOS_Y_CONVERT:
      case EOS_F_CONVERT:
        _flags_val[i] = optVal->rval;
        break;
      default:
        _flags_val[i] = (EOS_REAL)optVal->bval;
        break;
      }
    }
    else if (EOS_IS_LOADING_OPTION(o)) {/* loading option */
      eos_GetOptionEosDataMap (&gEosDataMap, th, o, &optVal, &err);

      if (EOS_IS_LOADING_OPTION_WITH_REAL_VALUE(o)) {
        _flags_val[i] = optVal->rval;
      }
      else if (EOS_IS_LOADING_OPTION_WITH_INTEGER_VALUE(o)) {
        _flags_val[i] = (EOS_REAL)optVal->ival;
      }
      else { /* EOS_BOOLEAN option */
        _flags_val[i] = (EOS_REAL)optVal->bval;
      }
    }
    else if (EOS_IS_INTERPOLATION_OPTION(o)) {
      EOS_BOOLEAN optVal_b;
      eos_GetOptionEosInterpolation (&gEosInterpolation, th, o, &optVal_b, &err);
      _flags_val[i] = (EOS_REAL)optVal_b;
    }
  }

  for (i = 0; i < EOS_TOTAL_TABLE_OPTIONS; i++) {
    /* printf("debug: %-30s = %g\n", _flags_str[i], _flags_val[i]); */
    flags[i]           = _flags[i];
    flags_val[i]       = _flags_val[i];
    if (flags_str) strcpy(flags_str[i], _flags_str[i]);
  }

  /* destroy table handle and associated dummy objects in gEosDataMap and gEosInterpolation. Ignore error. */
  eos_DestroyTables(&one, &th, &err);
}

/*!This function writes to <FILE *fp> all the default option flag key/value pairs. */
void write_defaultOptionFlags_KeyValue(FILE *fp, EOS_CHAR *caller)
{
  int i;
  EOS_INTEGER _flags[EOS_TOTAL_TABLE_OPTIONS];
  EOS_REAL    _flags_val[EOS_TOTAL_TABLE_OPTIONS];
  EOS_CHAR    _flags_str[EOS_TOTAL_TABLE_OPTIONS][50];

  /* populate arrays with default values */
  get_defaultOptionFlags_KeyValue(_flags_str, _flags, _flags_val);

  fprintf(fp, "################################################################################\n");
  fprintf(fp, "# This file contains setup and interpolation options used by EOSPAC 6 related   \n");
  fprintf(fp, "# tool named %s, when executed in the current directory.\n", caller);
  fprintf(fp, "# For more details about the various options, please reference the EOSPAC 6     \n");
  fprintf(fp, "# User Manual (LA-UR-14-29289).                                                 \n");
  fprintf(fp, "################################################################################\n");
  fprintf(fp, "\n#\n# GENERAL OPTIONS (e.g., these affect both SETUP and INTERPOLATION):\n#\n");
  fprintf(fp, "# %-49s%s\n", "Option Key", "Allowed value(s)");
  for (i = 0; i < EOS_TOTAL_TABLE_OPTIONS; i++) {
    EOS_INTEGER o = _flags[i];

    if (EOS_IS_HIDDEN_OPTION(o)) continue;

    if (EOS_IS_GENERAL_OPTION(o)) {
      switch(o) {
      case EOS_X_CONVERT:
      case EOS_Y_CONVERT:
      case EOS_F_CONVERT:
        fprintf(fp, "%-30s = %-15.6f %s (default=%f)\n", _flags_str[i], _flags_val[i], "# floating point number", _flags_val[i]);
        break;
      default:
        fprintf(fp, "%-30s = %-15d %s (default=%d)\n", _flags_str[i], (EOS_BOOLEAN)_flags_val[i], "# 0 or 1", (EOS_BOOLEAN)_flags_val[i]);
        break;
      }
    }
  }

  fprintf(fp, "\n#\n# SETUP OPTIONS:\n#\n");
  fprintf(fp, "# %-49s%s\n", "Option Key", "Allowed value(s)");
  for (i = 0; i < EOS_TOTAL_TABLE_OPTIONS; i++) {
    EOS_INTEGER o = _flags[i];

    if (EOS_IS_HIDDEN_OPTION(o)) continue;

    if (EOS_IS_LOADING_OPTION(o)) {/* loading option */
      if (EOS_IS_LOADING_OPTION_WITH_REAL_VALUE(o)) {
        fprintf(fp, "%-30s = %-15.6f %s (default=%f)\n", _flags_str[i], _flags_val[i], "# floating point number", _flags_val[i]);
      }
      else if (EOS_IS_LOADING_OPTION_WITH_INTEGER_VALUE(o)) {
        fprintf(fp, "%-30s = %-15d %s (default=%d)\n", _flags_str[i], (EOS_INTEGER)_flags_val[i], "# integer", (EOS_INTEGER)_flags_val[i]);
      }
      else { /* EOS_BOOLEAN option */
        fprintf(fp, "%-30s = %-15d %s (default=%d)\n", _flags_str[i], (EOS_BOOLEAN)_flags_val[i], "# 0 or 1", (EOS_BOOLEAN)_flags_val[i]);
      }
    }
  }

  fprintf(fp, "\n#\n# INTERPOLATION OPTIONS:\n#\n");
  fprintf(fp, "# %-49s%s\n", "Option Key", "Allowed value(s)");
  for (i = 0; i < EOS_TOTAL_TABLE_OPTIONS; i++) {
    EOS_INTEGER o = _flags[i];

    if (EOS_IS_HIDDEN_OPTION(o)) continue;

    if (EOS_IS_INTERPOLATION_OPTION(o)) {
      fprintf(fp, "%-30s = %-15d %s (default=%d)\n", _flags_str[i], (EOS_BOOLEAN)_flags_val[i], "# 0 or 1", (EOS_BOOLEAN)_flags_val[i]);
    }
  }
}

/*!This function returns the EOS_TOTAL_TABLE_OPTIONS value. */
EOS_INTEGER get_optionFlags_size()
{
  return(EOS_TOTAL_TABLE_OPTIONS);
}

/*!This function parses the file named fn for the user-defined option flag key/value pairs.
 * Then it conditionally-sets all of the dicovered options to the supplied table handles(s). */
EOS_INTEGER parse_optionFlags_KeyValue(EOS_CHAR *fn, EOS_INTEGER *flags, EOS_REAL *flags_val,
                                       EOS_INTEGER th_sz, EOS_INTEGER *th, EOS_BOOLEAN verbose)
{
  int i, j;
  EOS_CHAR line[200];
  FILE *fp = NULL;
  EOS_BOOLEAN fileExists = EOS_FALSE;
  EOS_INTEGER _flags[EOS_TOTAL_TABLE_OPTIONS];
  EOS_REAL    _flags_val[EOS_TOTAL_TABLE_OPTIONS];
  EOS_INTEGER default_flags[EOS_TOTAL_TABLE_OPTIONS];
  EOS_REAL    default_flags_val[EOS_TOTAL_TABLE_OPTIONS];
  struct stat file_statbuf;

  /* populate arrays with default values */
  get_defaultOptionFlags_KeyValue(NULL, default_flags, default_flags_val);
  for (i = 0; i < EOS_TOTAL_TABLE_OPTIONS; i++) {
    _flags[i]     = default_flags[i];
    _flags_val[i] = default_flags_val[i];
  }

  fileExists = (!((EOS_BOOLEAN) stat (fn, &file_statbuf)))?EOS_TRUE:EOS_FALSE;
  if (fileExists) {
    fp = fopen (fn, "r");  /* open indexFileName */
    if (! fp) { /* fopen failed */
      fprintf(stderr, "WARNING: fopen failed for %s\n", fn);
      return(-1);
    }

    /* parse config file */
    while (fgets (line, 200, fp)) {
      EOS_CHAR *p = NULL;
      EOS_CHAR *delim = "=";
      EOS_CHAR **split_result = NULL;
      EOS_CHAR key[50], val[50];
      if (strlen(line) < 1) continue;
      for (p=line; p[0] != '\0'; p++)
        if (p[0] != ' ' && p[0] != '\t') break; /* ignore leading white space */
      if (p[0] == '#') continue; /* skip comment lines */
      if (p[0] == '\n' || p[0] == '\r' || p[0] == '\f') continue; /* skip empty lines */

      split (p, delim, &split_result);
      strncpy(key, split_result[0], 49);
      for (i = 0; i < strlen(key); i++) {
        if (key[i] == ' ' || key[i] == '\t') { /* find first space or tab */
          key[i] = '\0';
          break;
        }
      }
      strncpy(val, split_result[1], 49);
      for (i = 0; i < EOS_TOTAL_TABLE_OPTIONS; i++) {
        EOS_INTEGER iflag = get_optionFlag_FromStr(key);
        if (iflag == eos_OptionFlags[i]) {
          char *endp;
          _flags[i] = eos_OptionFlags[i];
          _flags_val[i] = strtod(val, &endp);
          break;
        }
      }
    }
  }

  if (flags)
    memcpy (flags, _flags, sizeof (EOS_INTEGER));;
  if (flags_val)
    memcpy (flags_val, _flags_val, sizeof (EOS_REAL));;

  if (th_sz > 0 && th) {
    int cnt = 0;
    for (i = 0; i < EOS_TOTAL_TABLE_OPTIONS; i++) {
      EOS_INTEGER err;
      if (_flags[i] <= 0) continue;
      for (j = 0; j < EOS_TOTAL_TABLE_OPTIONS; j++) {
        if (_flags[i] == default_flags[j]) break;
      }
      if (j >= EOS_TOTAL_TABLE_OPTIONS) {
        printf("# %s = %g ... option not found in default list\n", get_optionStr_FromFlag(_flags[i]), _flags_val[i]);
        continue;
      }
      if (_flags_val[i] == default_flags_val[j]) continue;
      eos_SetOption (th, &_flags[i], &_flags_val[i], &err);
      if (err != EOS_OK) {
        EOS_CHAR errorMessage[EOS_MaxErrMsgLen];

        eos_GetErrorMessage (&err, errorMessage);
        fprintf (stderr, "eos_SetOption WARNING %i: OPTION=%s: %s\n", err, get_optionStr_FromFlag(_flags[i]), errorMessage);

        return((EOS_INTEGER)err);
      }
      if (verbose) {
        if (! cnt)
          printf("# Non-default configuration options (%s):\n", fn);
        cnt++;
        printf("#      %s = %g\n", get_optionStr_FromFlag(_flags[i]), _flags_val[i]);
      }
    }
  }

  fclose(fp);
  return(EOS_OK);
}

/*! \brief Split str into an array of char* tokens within result; split using characters in delim */
int split (char *str, char *delim, char ***result)
{
  char *cp;
  int i = 0;

  cp = strtok (str, delim);
  while (cp != NULL) {
    if (i == 0) *result = (char**) safe_malloc(1, sizeof(char*));
    else        *result = (char**) realloc(*result, (i+1)*sizeof(char*));
    (*result)[i] = (char*) safe_malloc(strlen(cp), sizeof(char));
    strcpy((*result)[i], cp);
    i++;
    cp = strtok (NULL, delim);
  }

  return i;
}

/*!This function gets the description string representation of the data type, t. */
EOS_CHAR* get_tableType_description(EOS_INTEGER t)
{
  EOS_CHAR *s = NULL;
  if (! eos_TableListReverseMap) _eos_SetTableListReverseMap(); /* build reverse lookup for eos_TableList[] */
  s = (EOS_CHAR*)EOS_TYPE_TO_TAB_NAME(t);
  return s;
}

/*!This function gets the number of tables from gEosDataMap. */
EOS_INTEGER get_no_tables(void)
{
  return gEosDataMap.nTables;
}

/*!This function gets the number of handles from gEosDataMap. */
EOS_INTEGER get_no_handles(void)
{
  return gEosDataMap.nHandles;
}

/*!This function gets the number of table entries allocated in gEosDataMap. */
EOS_INTEGER get_no_alloc(void)
{
  return gEosDataMap.nAlloc;
}

/*!This function gets the number of general options allocated in gEosDataMap. */
EOS_INTEGER get_no_generalOptions(void)
{
  return EOS_NUM_GENERAL_OPTIONS;
}

/*!This function gets the number of data table options allocated in
   and gEosDataMap.dataObject. */
EOS_INTEGER get_no_tableOptions(void)
{
  return EOS_NUM_LOADING_OPTIONS;
}

/*! This function returns the gEosDataMap.generalOptions[i][th].ival given a table handle, th. */
EOS_BOOLEAN get_generalOptions_bval(EOS_INTEGER th, EOS_INTEGER i)
{
  EOS_INTEGER th_ref;
  th_ref = gEosDataMap.tableHandlesMap[th];
  if (th_ref < 0) return EOS_FALSE; /* table handle is an invalid reference */
  return (EOS_BOOLEAN) gEosDataMap.generalOptions[i][th].bval;
}

/*! This function returns the gEosDataMap.generalOptions[i][th].ival given a table handle, th. */
EOS_INTEGER get_generalOptions_ival(EOS_INTEGER th, EOS_INTEGER i)
{
  EOS_INTEGER th_ref;
  th_ref = gEosDataMap.tableHandlesMap[th];
  if (th_ref < 0) return EOS_INVALID; /* table handle is an invalid reference */
  return (EOS_INTEGER) gEosDataMap.generalOptions[i][th].ival;
}

/*! This function returns the gEosDataMap.generalOptions[i][th].ival given a table handle, th. */
EOS_REAL get_generalOptions_rval(EOS_INTEGER th, EOS_INTEGER i)
{
  EOS_INTEGER th_ref;
  th_ref = gEosDataMap.tableHandlesMap[th];
  if (th_ref < 0) return -9999.9; /* table handle is an invalid reference */
  return (EOS_REAL) gEosDataMap.generalOptions[i][th].rval;
}

/*! This function returns the gEosDataMap.generalOptions[i][th].ival given a table handle, th. */
EOS_CHAR* get_generalOptions_cval(EOS_INTEGER th, EOS_INTEGER i)
{
  EOS_INTEGER th_ref;
  th_ref = gEosDataMap.tableHandlesMap[th];
  if (th_ref < 0) return NULL; /* table handle is an invalid reference */
  return (EOS_CHAR*) gEosDataMap.generalOptions[i][th].cval;
}

/*! This function returns the eos_DefaultTableOptions[i].optionType given the general option index, i. */
EOS_INTEGER get_generalOption_type(EOS_INTEGER i)
{
  EOS_INTEGER ind;
  ind = EOS_OPTION_FLAG_TO_INDEX(EOS_GENERAL_INDEX_TO_OPTION_FLAG(i));
  return eos_DefaultTableOptions[ind].optionType;
}

/*! This function returns the general optionFlag given the general option index, i. */
EOS_CHAR* get_generalOption_flag(EOS_INTEGER i)
{
  EOS_INTEGER ind1, ind2;
  EOS_CHAR *s = NULL;
  ind1 = EOS_GENERAL_INDEX_TO_OPTION_FLAG(i);
  ind2 = EOS_OPTION_FLAG_TO_INDEX(ind1);
  s = (EOS_CHAR*)eos_OptionFlags_str[ind2];
  return s;
}

/*! This function returns the gEosDataMap.dataObjects[th]->tableOptions[i].bval given both
    the table handle, th, and the table option index, i. */
EOS_BOOLEAN get_tableOptions_bval(EOS_INTEGER th, EOS_INTEGER i)
{
  EOS_INTEGER th_ref;
  th_ref = gEosDataMap.tableHandlesMap[th];
  if (th_ref < 0) return EOS_INVALID; /* table handle is an invalid reference */
  return (EOS_BOOLEAN) gEosDataMap.dataObjects[th_ref]->tableOptions[i].optionValue.bval;
}

/*! This function returns the gEosDataMap.dataObjects[th]->tableOptions[i].ival given both
    the table handle, th, and the table option index, i. */
EOS_INTEGER get_tableOptions_ival(EOS_INTEGER th, EOS_INTEGER i)
{
  EOS_INTEGER th_ref;
  th_ref = gEosDataMap.tableHandlesMap[th];
  if (th_ref < 0) return -99999; /* table handle is an invalid reference */
  return (EOS_INTEGER) gEosDataMap.dataObjects[th_ref]->tableOptions[i].optionValue.ival;
}

/*! This function returns the interpolation option's state given both
    the table handle, th, and the table option flag, optionFlag. */
EOS_BOOLEAN get_interpolationOptions_bval(EOS_INTEGER th, EOS_INTEGER optionFlag)
{
  EOS_BOOLEAN optionValue;
  EOS_INTEGER err = EOS_OK;
  eos_GetOptionEosInterpolation (&gEosInterpolation, th, optionFlag, &optionValue, &err);
  return optionValue;
}

/*! This function returns the gEosDataMap.dataObjects[th]->tableOptions[i].rval given both
    the table handle, th, and the table option index, i. */
EOS_REAL get_tableOptions_rval(EOS_INTEGER th, EOS_INTEGER i)
{
  EOS_INTEGER th_ref;
  th_ref = gEosDataMap.tableHandlesMap[th];
  if (th_ref < 0) return -9999.9; /* table handle is an invalid reference */
  return (EOS_REAL) gEosDataMap.dataObjects[th_ref]->tableOptions[i].optionValue.rval;
}

/*! This function returns the gEosDataMap.dataObjects[th].eosData.tableOptions[i].optionType given both
    the table handle, th, and the table option index, i. */
EOS_INTEGER get_tableOption_type(EOS_INTEGER th, EOS_INTEGER i)
{
  EOS_INTEGER th_ref;
  th_ref = gEosDataMap.tableHandlesMap[th];
  if (th_ref < 0) return -99999; /* table handle is an invalid reference */
  return gEosDataMap.dataObjects[th_ref]->tableOptions[i].optionType;
}

/*! This function returns the data table optionFlag given both
    the table handle, th, and the table option index, i. */
EOS_CHAR* get_tableOption_flag(EOS_INTEGER th, EOS_INTEGER i)
{
  EOS_INTEGER ind, th_ref;
  static EOS_CHAR str[50];
  th_ref = gEosDataMap.tableHandlesMap[th];
  if (th_ref < 0) return "INVALID"; /* table handle is an invalid reference */
  for (ind = 0; ind < EOS_TOTAL_TABLE_OPTIONS; ind++) {
    if (gEosDataMap.dataObjects[th_ref]->tableOptions[i].optionFlag ==
	eos_OptionFlags[ind]) {
      break;
    }
  }
  if (ind == EOS_NUM_LOADING_OPTIONS) {
    sprintf(str, "UNKNOWN(%i)",
	    gEosDataMap.dataObjects[th_ref]->tableOptions[i].optionFlag);
    return str;
  }
  return eos_OptionFlags_str[ind];
}

/*! This function returns the table type for a specific table given its table handle. */
EOS_INTEGER get_table_type(EOS_INTEGER th)
{
  return gEosDataMap.tableTypes[th];
}

/*! This function returns the material ID for a specific material given its table handle. */
EOS_INTEGER get_matID(EOS_INTEGER th)
{
  EOS_INTEGER th_ref;
  th_ref = gEosDataMap.tableHandlesMap[th];
  return gEosDataMap.dataObjects[th_ref]->materialID;
}

/*! This function is a wrapper for the internal _eos_get_field_value function defined in eos_RecordType4.c */
int eos_getFieldValue(EOS_CHAR *str, EOS_CHAR *keyword, EOS_CHAR *oStr)
{
  return _eos_get_field_value(str, keyword, oStr);
}

/*! This function returns the comment string for a specific material id number. */
EOS_CHAR* get_commentStr(EOS_INTEGER matid)
{
  EOS_INTEGER err = EOS_OK;
  EOS_INTEGER one = 1;
  EOS_REAL infoVals[1];
  EOS_INTEGER commentInfoItems[1] = {
    EOS_Cmnt_Len
  };
  EOS_INTEGER Cmnt_Len=0;
  EOS_CHAR *cmntStr=NULL;

  EOS_INTEGER th, tableType;
  EOS_INTEGER sesTableNum = (EOS_INTEGER)101;
  EOS_INTEGER sesSubtableIndex = 1;
  err = get_DataType(sesTableNum, sesSubtableIndex, &tableType);
  if (err)
    return cmntStr;

  /* Initialize the table handle */
  eos_CreateTables (&one, &tableType, &matid, &th, &err);
  if (err != EOS_OK)
    return cmntStr;

  /* Load data */
  eos_LoadTables (&one, &th, &err);
  if (err != EOS_OK)
    return cmntStr;

  eos_GetTableInfo (&th, &one, commentInfoItems, infoVals, &err);
  if (err == EOS_OK) {
    Cmnt_Len = (EOS_INTEGER) (infoVals[0]);
    cmntStr = (EOS_CHAR *) malloc (sizeof (EOS_CHAR) * Cmnt_Len);
    if (! cmntStr)
      return cmntStr;

    eos_GetTableCmnts (&th, cmntStr, &err);
  }
  eos_DestroyTables(&one, &th, &err);

  return cmntStr;
}

/*! This function returns the material creation date. */
EOS_INTEGER get_creationDate(EOS_INTEGER th)
{
  EOS_INTEGER th_ref;
  th_ref = gEosDataMap.tableHandlesMap[th];
  return gEosDataMap.dataObjects[th_ref]->creationDate;
}

/*! This function returns the material modification date. */
EOS_INTEGER get_modificationDate(EOS_INTEGER th)
{
  EOS_INTEGER th_ref;
  th_ref = gEosDataMap.tableHandlesMap[th];
  return gEosDataMap.dataObjects[th_ref]->modificationDate;
}

/*! This function returns a list of all the material ID's found in the specified file. */
#include <sys/types.h>
#include <sys/stat.h>
EOS_INTEGER get_matIdListFromFile(EOS_INTEGER** list, EOS_CHAR *fn)
{
  int i, ierr;
  EOS_INTEGER jfile;
  EOS_INTEGER nmats;
  ses_file_handle sesFile;
  ses_material_id_reference indexdata;
  EOS_BOOLEAN fileExists;
  struct stat file_statbuf;

  if (strlen(fn) > PATH_MAX) {
    printf("File name too long! %d > %d\n", (EOS_INTEGER)strlen(fn), (EOS_INTEGER)PATH_MAX);
    return -1;
  }

  fileExists = (!((EOS_BOOLEAN) stat (fn, &file_statbuf)))?EOS_TRUE:EOS_FALSE;
  if (! fileExists) {
    printf("Files found:\n");
    for (i = 0; i < sesameFilesL; i++)
      printf("  %i. %s\n", i+1, sesameFiles[i]);
    printf("\n");
    return -3;
  }

  /* create a list of data file names */
  if (sesameFilesL <= 0) {      /* call this function once only */
    ierr = eos_getSesameFileNames (&sesameFiles, &sesameFilesL, NULL);
    if (ierr) return (ierr);
  }

  /* find fn in current list of data file names */
  for (jfile = 0; jfile < sesameFilesL; jfile++) {
    if (!strcmp(sesameFiles[jfile], fn)) /* files are identical */
      break;                  /* leave jfile loop */
  }

  if (jfile >= sesameFilesL) {
    /* store fn in sesameFiles[0] */
    sesameFiles[0] = (EOS_CHAR *) malloc ((PATH_MAX + 1) * sizeof (EOS_CHAR));
    strcpy (sesameFiles[0], fn);

    /* use fn index */
    jfile = 0;
  }

  /* get the info from cache */
  sesFile = 0;
  ierr = eos_SesGetFileInfoFromCache (jfile, &nmats, &indexdata, &sesFile);
  if (eos_GetStandardErrorCodeFromCustomErrorCode(ierr) != EOS_OK)
    return ierr;

  /* store material id's found in jfile */
  *list = (EOS_INTEGER*) malloc((nmats + 1) * sizeof (EOS_INTEGER));
  if (! *list) return -7; /* exit if malloc fails */

  /* sort the indexdata list */
  _eos_SesQuickSort (nmats, indexdata, 0, &ierr, NULL);
  if (eos_GetStandardErrorCodeFromCustomErrorCode(ierr) != EOS_OK)
    return ierr;

  for (i = 0; i < nmats; i++)
    (*list)[i] = (EOS_INTEGER) (indexdata[i]);
  (*list)[i] = -9999999; /* array bound marker */

  return EOS_OK;
}

/*! This function returns a list of all the unique material ID's found in all known files. */
EOS_INTEGER get_matIdList(EOS_INTEGER** list)
{
  int i, j, k, i0=0, nmats, ierr;
  EOS_INTEGER *indexdata = NULL;
  EOS_REAL *tmp_list = NULL;

  /* create a list of data file names */
  if (sesameFilesL <= 0) {      /* call this function once only */
    ierr = eos_getSesameFileNames (&sesameFiles, &sesameFilesL, NULL);
    if (ierr) return (ierr);
  }

  for (i = 0; i < sesameFilesL; i++) {

    ierr = get_matIdListFromFile(&indexdata, sesameFiles[i]);
    if (eos_GetStandardErrorCodeFromCustomErrorCode(ierr) != EOS_OK && eos_GetStandardErrorCodeFromCustomErrorCode(ierr) != EOS_READ_TOTAL_MATERIALS_FAILED)
      return ierr;
    else if (eos_GetStandardErrorCodeFromCustomErrorCode(ierr) == EOS_READ_TOTAL_MATERIALS_FAILED) {
      fprintf(stderr, "WARNING: read total materials failed for file, %s\n", sesameFiles[i]);
      ierr = EOS_OK;
      continue;
    }

    for (nmats=0;1;nmats++) /* count material id's */
      if (indexdata[nmats] < 0) break;

    if (i == 0) {
      *list = (EOS_INTEGER*) malloc((nmats + 1) * sizeof (EOS_INTEGER));
      if (! *list)
	return -7; /* exit if malloc fails */
    }
    else {
      *list = (EOS_INTEGER*) realloc(*list, (i0 + nmats + 1) * sizeof (EOS_INTEGER));
      if (! *list)
	return -7; /* exit if realloc fails */
    }
    for (j = 0; j < nmats; j++)
      (*list)[i0 + j] = indexdata[j];
    (*list)[i0 + j] = -9999999; /* array bound marker */

    i0 = i0 + nmats;

    if (indexdata) EOS_FREE(indexdata);
  }

  tmp_list = (EOS_REAL*) malloc(i0 * sizeof (EOS_REAL));
  if (! tmp_list)
    return -7; /* exit if malloc fails */
  for (j = 0; j < i0; j++)
    tmp_list[j] = (EOS_REAL) (*list)[j];

  /* sort the tmp_list */
  _eos_QuickSort (i0, tmp_list, 0, &ierr, NULL, NULL);
  if (eos_GetStandardErrorCodeFromCustomErrorCode(ierr) != EOS_OK)
    return ierr;

  /* remove duplicates from a[] */
  k = 1;
  for (i = 1; i < i0; i++) {
    if (tmp_list[i] != tmp_list[k - 1]) {
      tmp_list[k] = tmp_list[i];
      k++;
    }
  }
  i0 = k;

  *list = (EOS_INTEGER*) realloc(*list, (i0 + 1) * sizeof (EOS_INTEGER));
  if (! *list)
    return -7; /* exit if realloc fails */

  for (j = 0; j < i0; j++)
    (*list)[j] = (EOS_INTEGER) tmp_list[j];
  (*list)[j] = -9999999; /* array bound marker */

  if (tmp_list) EOS_FREE(tmp_list);

  return EOS_OK;
}

/*! This function returns a filename containing the material ID. */
EOS_INTEGER get_matidFileName (EOS_INTEGER mat, EOS_CHAR **fn)
{
  int i, j, len, ierr = EOS_OK;
  EOS_INTEGER* indexdata;

  /* create a list of data file names */
  if (sesameFilesL <= 0) {      /* call this function once only */
    ierr = eos_getSesameFileNames (&sesameFiles, &sesameFilesL, NULL);
    if (ierr) return ierr;
  }

  /* find mat in list of data files */
  for (i = 0; i < sesameFilesL; i++) {
    ierr = get_matIdListFromFile(&indexdata, sesameFiles[i]);
    if (eos_GetStandardErrorCodeFromCustomErrorCode(ierr) != EOS_OK)
      return ierr;

    for (j=0;1;j++) { /* find material id's */
      if (indexdata[j] < 0) break;
      if (indexdata[j] == mat) {
	len = strlen(sesameFiles[i]);
	*fn = (EOS_CHAR*) malloc((len+1) * sizeof(EOS_CHAR));
	if (! *fn)
	  return -7; /* exit if malloc fails */
	strcpy(*fn, sesameFiles[i]);
	return EOS_OK;
      }
    }
  }
  return EOS_MATERIAL_NOT_FOUND;
}

/*! This function returns the file name associated with a table handle. */
EOS_CHAR* get_tableHandleFileName (EOS_INTEGER th)
{
  EOS_CHAR *s = NULL;
  int i, j;
  if (! eos_IsHandleValid (th)) return "";
  j = gEosDataMap.tableHandlesMap[th];
  if (j < 0) {
    printf("ERROR: th %d references an invalid data object.\n", th);
    assert(gEosDataMap.tableHandlesMap[th] >= 0);
  }
  i = gEosDataMap.dataObjects[j]->dataFileIndex;
  if (i < 0) return "";
  s = (EOS_CHAR*)sesameFiles[gEosDataMap.dataObjects[gEosDataMap.tableHandlesMap[th]]->dataFileIndex];
  return s;
}

/*! This function prints the list of file names found.
    If limit>0, then append "\tfcmp_ignore" to file names with i>limit."
    If limit<0, then append "\tfcmp_ignore" to absolute file names."

    If filter != NULL, then use strspn to replace file name prefix containing
    a sequence of [.\/] characters.

    If sesameFiles[] is not already allocated, then it will be deallocated prior to exit.
 */
EOS_INTEGER print_FileList (EOS_INTEGER limit, EOS_CHAR *filter)
{
  int i, ierr = EOS_OK;
  int doCleanup = (sesameFiles && sesameFilesL > 0) ? 0 : 1;

  /* create/update the list of data file names */
  ierr = eos_getSesameFileNames (&sesameFiles, &sesameFilesL, NULL);
  if (ierr) return ierr;

  /* find mat in list of data files */
  for (i = 0; i < sesameFilesL; i++) {
    char *set = "./";
    int idx = 0;
    EOS_BOOLEAN absolutefn = _eos_is_absoluteFileName(sesameFiles[i]);
    if (filter != NULL && ! absolutefn) {
      idx = strspn(sesameFiles[i], set);
      printf ("%3d. %s%s", i, filter, &(sesameFiles[i])[idx]);
    }
    else {
      printf ("%3d. %s", i, &(sesameFiles[i])[idx]);
    }
    if (limit > 0 && i > limit)
      printf ("\tfcmp_ignore");

    if (limit < 0 && absolutefn) {
      /* one of the following absolute reference formats is detected:
       *   Unix        --> '/'
       *   DOS/Windows --> '//X'
       *   DOS/Windows --> '\\X'
       *   DOS/Windows --> 'X:\'
       */
      printf ("\tfcmp_ignore");
    }
    printf ("\n");
  }

  if (doCleanup) {
    for (i = 0; i < sesameFilesL; i++)
      EOS_FREE (sesameFiles[i]);
    EOS_FREE (sesameFiles);
    sesameFilesL = 0;
    for (i = 0; i < sesameIndexLocationsL; i++)
      EOS_FREE (sesameIndexLocations[i]);
    EOS_FREE (sesameIndexLocations);
    sesameIndexLocationsL = 0;
  }

  return ierr;
}

/*! \brief Return a list of tables and their 64-bit-word sizes available for given mat */
EOS_INTEGER get_matidTableInfo (EOS_INTEGER mat, EOS_INTEGER **tableList, EOS_INTEGER **tableSize)
{
  /*  function prototypes */
  ses_boolean    _do_setup_for_read(ses_file_handle the_handle, ses_material_id the_mid, ses_table_id the_tid);
  long* _get_nwds_index_record(struct _ses_index_record* the_index_record);
  /*  end function prototypes */

  EOS_CHAR *sesFileName = NULL;
  EOS_INTEGER jfile, i, err;
  ses_file_handle sesFile;

  err = EOS_OK;

  /* determine location of mat */
  err = get_matidFileName(mat, &sesFileName);
  if (err)
    return (err);

  /* find fn in list of data file names */
  for (jfile = 0; jfile < sesameFilesL; jfile++)
    if (!strcmp(sesameFiles[jfile], sesFileName)) /* files are identical */
      break;                  /* leave jfile loop */

  if (jfile >= sesameFilesL) {
    printf("Files found:\n");
    for (i = 0; i < sesameFilesL; i++)
      printf("  %i. %s\n", i+1, sesameFiles[i]);
    printf("\n\n");
    return -3;
  }

  /* open file for reading */
  sesFile = ses_open(sesameFiles[jfile], 'R');
  if (ses_is_valid(sesFile) == SES_FALSE)
    return SES_OPEN_ERROR;

  /* setup SES_IO data using an expected 201 table */
  if (ses_setup(sesFile, mat, 201) != SES_NO_ERROR)
    return SES_SETUP_ERROR;
  
  /* get table numbers for mat */
  ses_table_id_reference tableIds;
  long tableIdsL;
  tableIds = ses_get_table_ids(sesFile, mat, &tableIdsL);

  /* get table sizes for mat */
  ses_table_sizes_reference sizes;
  long sizesL;
  sizes = ses_get_table_sizes(sesFile, mat, &sizesL);

  /* build list of table numbers */
  *tableList = NULL;
  *tableList = (EOS_INTEGER*) malloc ((tableIdsL + 1) * sizeof (EOS_INTEGER));
  if (! *tableList)
    return -7;
  *tableSize = NULL;
  *tableSize = (EOS_INTEGER*) malloc ((tableIdsL + 1) * sizeof (EOS_INTEGER));
  if (! *tableSize)
    return -7;

  for (i = 0; i < tableIdsL; i++) {
    (*tableList)[i] = (EOS_INTEGER)tableIds[i];
    (*tableSize)[i] = (EOS_INTEGER)sizes[i];
  }
  (*tableList)[i] = -9999999; /* array bound marker */
  (*tableSize)[i] = -9999999; /* array bound marker */

  return (EOS_OK);
}

/*!This function gets the void* pointer value of gEosDataMap.dataObjects. */
void* get_dataObjects_ptr(void)
{
  return (void*) gEosDataMap.dataObjects;
}

/*!This function gets the void* pointer value of gEosDataMap.tableHandlesMap. */
void* get_tableHandlesMap_ptr(void)
{
  return (void*) gEosDataMap.tableHandlesMap;
}

/*!This function gets the value of gEosDataMap.tableHandlesMap[i]. */
EOS_INTEGER get_tableHandlesMap_val(EOS_INTEGER i)
{
  return (EOS_INTEGER) gEosDataMap.tableHandlesMap[i];
}

/*!This function gets the void* pointer value of gEosDataMap.tableTypes[i]. */
void* get_tableTypes_ptr(void)
{
  return (void*) gEosDataMap.tableTypes;
}

/*!This function gets the value of gEosDataMap.tableTypes[i]. */
EOS_INTEGER get_tableTypes_val(EOS_INTEGER i)
{
  return (EOS_INTEGER) gEosDataMap.tableTypes[i];
}

/*!This function gets the void* pointer value of gEosDataMap.errorCodes[i]. */
void* get_errorCodes_ptr(void)
{
  return (void*) gEosDataMap.errorCodes;
}

/*!This function gets the value of gEosDataMap.errorCodes[i]. */
EOS_INTEGER get_errorCodes_val(EOS_INTEGER i)
{
  return (EOS_INTEGER) gEosDataMap.errorCodes[i];
}

/*!This function gets the void* pointer value of gEosDataMap.isHandlePublic. */
void* get_isHandlePublic_ptr(void)
{
  return (void*) gEosDataMap.isHandlePublic;
}

/*! Get the list of X, Y and F values for Record type 1 */

void get_RecordType1_XYF(EOS_INTEGER tableHandle, EOS_REAL **X, EOS_REAL **Y, EOS_REAL ***F, EOS_INTEGER *nX, EOS_INTEGER *nY)
{
  eos_Data *eosData;
  EOS_REAL *coldCurve;
  EOS_INTEGER dataType, err = EOS_OK, subTableNum, errorCode;
  eos_RecordType1 *me;

  if (! eos_TableListReverseMap) _eos_SetTableListReverseMap(); /* build reverse lookup for eos_TableList[] */
  
  eosData =
    eos_GetEosDataEosDataMap (&gEosDataMap, tableHandle, &dataType, &err);
  me = (eos_RecordType1 *) eosData;
  subTableNum = EOS_TYPE_TO_SUB_TAB_NUM (dataType);
  /* get the size of the data */
  eos_GetSizeRecordType1 (me, nX, nY);
  /* make sure the data is record type 1 */
  if (EOS_TYPE_TO_RECORD_TYPE (dataType) != EOS_RECORD_TYPE1) {
    errorCode = EOS_INVALID_TABLE_HANDLE;
    ((eos_ErrorHandler *) me)->HandleError (me, tableHandle, errorCode);
    return;
  }

  if (me->eosData.numSubtablesLoaded < subTableNum) {
    errorCode = EOS_DATA_TYPE_NOT_FOUND;
    ((eos_ErrorHandler *) me)->HandleError (me, tableHandle, errorCode);
    return;
  }
  _eos_GetDataRecordType1 (me, X, Y, F, &coldCurve, subTableNum);
}


/*! get the upper and lower bound on X, Y for a table of RecordType1, Cat 0 */
void get_UpperLowerBndsRecordType1_XY(EOS_INTEGER tableHandle, EOS_REAL *xMin, EOS_REAL *xMax, EOS_REAL *yMin, EOS_REAL *yMax )
{
  EOS_REAL *X, *Y, **F;
  EOS_INTEGER nX, nY;
  get_RecordType1_XYF(tableHandle, &X, &Y, &F, &nX, &nY);

  *xMin = X[0];
  *yMin = Y[0];
  *xMax = X[nX-1];
  *yMax = Y[nY-1];
}

/*! generate a list of random points on [xMin, xMax], [yMin, yMax]
 * xList and yList are expected to be allocated already
*/
#ifdef _WIN32
/* These macros are required for the MS Visual Studio C compiler since
   it is more strictly ANSI-C compliant than *nix compilers. */
#  ifndef random
#    define random rand
#  endif
#  ifndef srandom
#    define srandom srand
#  endif
#endif /* _WIN32 */
void generate_RandomPoints(EOS_REAL *xList, EOS_REAL *yList, EOS_INTEGER numPts,
			   EOS_REAL xMin, EOS_REAL xMax, EOS_REAL yMin, EOS_REAL yMax, EOS_BOOLEAN dosort)
{
  int i;
  EOS_INTEGER ierr = EOS_OK;
  if(xMin == 0)
    xMin = .0000001;
  if(yMin == 0)
    yMin = .0000001;
  srandom(145436);
  for(i=0;i<numPts;i++) {
    xList[i] = pow((xMax/xMin),(EOS_REAL)(random())/RAND_MAX)*xMin;
    yList[i] = pow((yMax/yMin),(EOS_REAL)(random())/RAND_MAX)*yMin;
  }
  if (dosort) {
    /* sort the xList */
    _eos_QuickSort (numPts, xList, 0, &ierr, NULL, NULL);
    assert(!ierr);
    /* sort the yList */
    _eos_QuickSort (numPts, yList, 0, &ierr, NULL, NULL);
    assert(!ierr);
  }
}

/*! generate a list of points linearly-distributed on [log10(xMin), log10(xMax)], [log10(yMin), log10(yMax)]
 * xList and yList are expected to be allocated already; however, yList will be ignored if it is NULL
*/
void generate_Log10DistributedPoints(EOS_REAL *xList, EOS_REAL *yList, EOS_INTEGER numPts,
				     EOS_REAL xMin, EOS_REAL xMax, EOS_REAL yMin, EOS_REAL yMax)
{
  int i;
  EOS_REAL minX = log10(MAX(0.0000001,xMin));
  EOS_REAL maxX = log10(MAX(0.0000001,xMax));
  EOS_REAL minY = log10(MAX(0.0000001,yMin));
  EOS_REAL maxY = log10(MAX(0.0000001,yMax));

  for(i=0;i<numPts;i++)
    xList[i] = MAX(xMin, MIN(xMax, pow(10.0,(maxX-minX) * (EOS_REAL)i / (EOS_REAL)(numPts-1) + minX)));

  if (yList) {
    for(i=0;i<numPts;i++)
      yList[i] = MAX(yMin, MIN(yMax, pow(10.0,(maxY-minY) * (EOS_REAL)i / (EOS_REAL)(numPts-1) + minY)));
  }
}

/*! generate a list of random points on [v_lower, v_upper]
 *  v is expected to be allocated already
*/
EOS_INTEGER max_recursion_level=0;
int getSamplesLatinHyperCube(int N, EOS_REAL v_lower, EOS_REAL v_upper, EOS_REAL *v)
{
  int i, nparam=1;
  double *vdata, a=0, b=1;
  EOS_INTEGER err = EOS_OK;
  vdata = (EOS_REAL*)malloc(N*nparam*sizeof(EOS_REAL));
  latinCube(vdata, &a, &b, nparam, N);
  for (i=0;i<N;i++)
    v[i] = MIN(v_upper, v_lower + (v_upper - v_lower) * (EOS_REAL)vdata[i]);
  EOS_FREE(vdata);
  max_recursion_level = _eos_QuickSort (N, v, 0, &err, NULL, NULL);

  return(err);
}

/*!Like the internal function named eos_ExpandGridInterpolate, this function
 * performs a grid expansion inserting nAdd points in X-direction increasing
 * nX grid to (nX + (nX-1)*nAdd).
 * arguments --
 *    nAdd        = number of points to insert.
 *    nX          = in/out  integer number of data table x values.
 *    X           = in/out  real(r8kind) array containing data table x values.
 *
 *    output integer error code:
 *        -1      memory allocation failed
 *
 * NOTE: This function is not callable from Fortran due to manipulation of
 *       the interface pointer values!
 */
EOS_INTEGER __eos_GetExpandedGrid (EOS_INTEGER nAdd, EOS_INTEGER *nX, EOS_REAL **X)
{
  EOS_INTEGER j, k, l;
  EOS_REAL *newX=NULL;

  /* allocate new memory */
  newX = (EOS_REAL*) malloc((*nX + (*nX-1)*nAdd) * sizeof(EOS_REAL));
  if (!newX) return(-1);

  for (j = 0; j < *nX - 1; j++) {
    newX[j * (nAdd + 1)] = (*X)[j];
    for (k = 0; k <= nAdd; k++) {
      for (l = 0; l <= nAdd; l++) {
	/* fill in the interior and the top and left edge */
	if (l > 0)
	  newX[j * (nAdd + 1) + l] =
	    (*X)[j] + l * ((*X)[j + 1] - (*X)[j]) / (EOS_REAL) (nAdd + 1);

      }                       /* l - loop */
    }                         /* k - loop */
  }                           /* j - loop */

  /* assign end points */
  newX[*nX + (*nX - 1) * nAdd - 1] = (*X)[*nX - 1];

  /* reassign array pointers */
  free(*X);
  *X = newX;
  *nX = *nX + (*nX-1)*nAdd;

  return(EOS_OK);
}

/*! This function returns the interpolation category associated with its data type (t). */
EOS_INTEGER get_dataTypeCategory(EOS_INTEGER t)
{
  if (! eos_TableListReverseMap) _eos_SetTableListReverseMap(); /* build reverse lookup for eos_TableList[] */
  return EOS_CATEGORY(t);
}

/*! This function returns the subcategory associated with its data type (t). */
EOS_INTEGER get_dataTypeSubCategory(EOS_INTEGER t)
{
  if (! eos_TableListReverseMap) _eos_SetTableListReverseMap(); /* build reverse lookup for eos_TableList[] */
  return EOS_SUBCATEGORY(t);
}

/*! This function returns the data type's (t) Sesame subtable index. */
EOS_INTEGER get_dataTypeSubTable(EOS_INTEGER t)
{
  if (! eos_TableListReverseMap) _eos_SetTableListReverseMap(); /* build reverse lookup for eos_TableList[] */
  return EOS_TYPE_TO_SUB_TAB_NUM(t);
}

/*! This function returns the data type's (t) first cross-referenced data type. */
EOS_INTEGER get_dataTypeReference1(EOS_INTEGER t)
{
  if (! eos_TableListReverseMap) _eos_SetTableListReverseMap(); /* build reverse lookup for eos_TableList[] */
  return EOS_EOS_TABLE_TYPE_REF1(t);
}

/*! This function returns the data type's (t) second cross-referenced data type. */
EOS_INTEGER get_dataTypeReference2(EOS_INTEGER t)
{
  if (! eos_TableListReverseMap) _eos_SetTableListReverseMap(); /* build reverse lookup for eos_TableList[] */
  return EOS_EOS_TABLE_TYPE_REF2(t);
}

/*! This function returns the data type's (t) dependent variable's data type. */
EOS_INTEGER get_dataTypeDepVar(EOS_INTEGER t)
{
  if (! eos_TableListReverseMap) _eos_SetTableListReverseMap(); /* build reverse lookup for eos_TableList[] */
  return EOS_TYPE_TO_DEP_VAR(t);
}

/*! This function returns the data type's (t) dependent variable's data type as a string. */
EOS_CHAR* get_dataTypeDepVar_str(EOS_INTEGER t)
{
  int i;
  if (! eos_TableListReverseMap) _eos_SetTableListReverseMap(); /* build reverse lookup for eos_TableList[] */
  i = get_VarListIndex(EOS_TYPE_TO_DEP_VAR(t));
  return eos_VarList[i].eosVarType_s;
}

/*! This function returns the data type's (t) dependent variable's data type as a short string. */
EOS_CHAR* get_dataTypeDepVar_short_str(EOS_INTEGER t)
{
  int i;
  if (! eos_TableListReverseMap) _eos_SetTableListReverseMap(); /* build reverse lookup for eos_TableList[] */
  i = get_VarListIndex(EOS_TYPE_TO_DEP_VAR(t));
  return eos_VarList[i].eosVarType_short_s;
}

/*! This function returns the data type's (t) first independent variable's data type. */
EOS_INTEGER get_dataTypeIndepVar1(EOS_INTEGER t)
{
  if (! eos_TableListReverseMap) _eos_SetTableListReverseMap(); /* build reverse lookup for eos_TableList[] */
  return EOS_TYPE_TO_INDEP_VAR1(t);
}

/*! This function returns the data type's (t) first independent variable's data type as a string. */
EOS_CHAR* get_dataTypeIndepVar1_str(EOS_INTEGER t)
{
  int i;
  if (! eos_TableListReverseMap) _eos_SetTableListReverseMap(); /* build reverse lookup for eos_TableList[] */
  i = get_VarListIndex(EOS_TYPE_TO_INDEP_VAR1(t));
  return eos_VarList[i].eosVarType_s;
}

/*! This function returns the data type's (t) first independent variable's data type as a short string. */
EOS_CHAR* get_dataTypeIndepVar1_short_str(EOS_INTEGER t)
{
  int i;
  if (! eos_TableListReverseMap) _eos_SetTableListReverseMap(); /* build reverse lookup for eos_TableList[] */
  i = get_VarListIndex(EOS_TYPE_TO_INDEP_VAR1(t));
  return eos_VarList[i].eosVarType_short_s;
}

/*! This function returns the data type's (t) second independent variable's data type. */
EOS_INTEGER get_dataTypeIndepVar2(EOS_INTEGER t)
{
  if (! eos_TableListReverseMap) _eos_SetTableListReverseMap(); /* build reverse lookup for eos_TableList[] */
  return EOS_TYPE_TO_INDEP_VAR2(t);
}

/*! This function returns the data type's (t) second independent variable's data type as a string. */
EOS_CHAR* get_dataTypeIndepVar2_str(EOS_INTEGER t)
{
  int i;
  if (! eos_TableListReverseMap) _eos_SetTableListReverseMap(); /* build reverse lookup for eos_TableList[] */
  i = get_VarListIndex(EOS_TYPE_TO_INDEP_VAR2(t));
  return eos_VarList[i].eosVarType_s;
}

/*! This function returns the data type's (t) second independent variable's data type as a short string. */
EOS_CHAR* get_dataTypeIndepVar2_short_str(EOS_INTEGER t)
{
  int i;
  if (! eos_TableListReverseMap) _eos_SetTableListReverseMap(); /* build reverse lookup for eos_TableList[] */
  i = get_VarListIndex(EOS_TYPE_TO_INDEP_VAR2(t));
  return eos_VarList[i].eosVarType_short_s;
}

/*!This function compares two floating point numbers to determine if they are the "same." */
int fcmp(EOS_REAL u, EOS_REAL v, EOS_REAL rel_diff, EOS_REAL abs_diff) {
  /* See Knuth V2 pg. 234 */

  /*  EOS_REAL rel_diff = 1.0e-10; relative difference tolerance */
  /*  EOS_REAL abs_diff = 1.0e-20; absolute difference tolerance */

  EOS_REAL diff = u - v;    /* difference */
  EOS_REAL delta = rel_diff * (fabs(u) > fabs(v) ? fabs(u) : fabs(v)) + abs_diff;

  if (diff > delta) {
    /* printf (" (%e > %e)", diff, delta); */
    return +1;              /* u > v */
  }
  else if (diff < -delta) {
    /* printf (" (%e < -%e)", diff, delta); */
    return -1;              /* u < v */
  }
  else {                    /* -delta <= diff <= delta */
    return 0;               /* u ~ v */
  }
}

/*!This function prints the values of me->NR and me->NT to stdout where me is
   defined as an eos_RecordType1 object and th is the associated table handle. */
void print_nr_and_nt(EOS_INTEGER th)
{
  static EOS_INTEGER nr0=0, nt0=0;
  EOS_INTEGER nr=0, nt=0, item, errorCode=EOS_OK;
  EOS_REAL val=0.0;

  item = EOS_NR;
  eos_GetTableInfoEosDataMap (&gEosDataMap, th, 1,
                              &item, &val, &errorCode);
  if (eos_GetStandardErrorCodeFromCustomErrorCode(errorCode) != EOS_OK)
    printf ("%d: %s\n", errorCode, eos_GetErrorMsg (errorCode));
  nr = (EOS_INTEGER)val;

  item = EOS_NT;
  eos_GetTableInfoEosDataMap (&gEosDataMap, th, 1,
                              &item, &val, &errorCode);
  if (eos_GetStandardErrorCodeFromCustomErrorCode(errorCode) != EOS_OK)
    printf ("%d: %s\n", errorCode, eos_GetErrorMsg (errorCode));
  nt = (EOS_INTEGER)val;

  printf("th %i: NR = %i and NT = %i", th, nr, nt);
  if (nr0 != nr)
    printf("\tNR changed from %i", nr0);
  if (nt0 != nt)
    printf("\tNT changed from %i", nt0);
  printf("\n");

  nr0 = nr;
  nt0 = nt;
}


/***********************************************************************/
/*!
 * \brief Get the category 0 data type corresponding to table and subtable numbers
 *
 * \param[in]     table     - EOS_INTEGER : specified table number
 * \param[in]     subtable  - EOS_INTEGER : specified subtable number
 * \param[out]    *type     - EOS_INTEGER : data type value
 *
 * \return        errorCode -  0: no error,
 *                            -1: invalid table number,
 *                            -2: invalid subtable number
 *
 ***********************************************************************/

EOS_INTEGER get_DataType(EOS_INTEGER table, EOS_INTEGER subtable, EOS_INTEGER *type)
{
  EOS_INTEGER valid_table = EOS_FALSE;
  EOS_INTEGER valid_subtable = EOS_FALSE;
  int i;

  if (! eos_TableListReverseMap) _eos_SetTableListReverseMap(); /* build reverse lookup for eos_TableList[] */

  for(i=0;i<MAX_TYPES;i++) {
    if (table == EOS_TYPE_TO_TAB_NUM(eos_TableList[i].eosTableType)) {
      valid_table = EOS_TRUE;
      if (subtable == EOS_TYPE_TO_SUB_TAB_NUM(eos_TableList[i].eosTableType))
	valid_subtable = EOS_TRUE;
    }

    if (table == EOS_TYPE_TO_TAB_NUM(eos_TableList[i].eosTableType) &&
	(subtable == EOS_TYPE_TO_SUB_TAB_NUM(eos_TableList[i].eosTableType) ||
	 EOS_TYPE_TO_SUB_TAB_NUM(eos_TableList[i].eosTableType) == 0)) {
      *type = eos_TableList[i].eosTableType;
      break;
    }
  }

  if (! valid_table)
    return -1;

  if (! valid_subtable && table > 300)
    return -2;

  return EOS_OK;
}

/***********************************************************************/
/*!
 * \brief Get the data type description corresponding to the provided type, t.
 *
 * \param[in]     t           - EOS_INTEGER : specified type
 *
 * \return        description - EOS_CHAR*   : eos_TableList[i].eosTableName
 *
 ***********************************************************************/
EOS_CHAR* get_DataTypeDescription(EOS_INTEGER t)
{
  EOS_INTEGER i;

  if (! eos_TableListReverseMap) _eos_SetTableListReverseMap(); /* build reverse lookup for eos_TableList[] */

  i = EOS_TYPE_TO_LIST_INDEX(t, "get_DataTypeDescription");
  return eos_TableList[i].eosTableName;
}

/***********************************************************************/
/*!
 * \brief Get the data type description corresponding to the provided table handle, th.
 *
 * \param[in]     th          - EOS_INTEGER : specified table handle
 *
 * \return        description - EOS_CHAR*   : eos_TableList[i].eosTableName; possibly modified from default
 *
 ***********************************************************************/
EOS_CHAR* get_DataTypeDescriptionFromTableHandle(EOS_INTEGER th)
{
  return eos_GetDataTypeDescriptionFromTableHandle(th);
}

/***********************************************************************/
/*!
 * \brief Get the data type corresponding to index
 *
 * \param[in]     i        - EOS_INTEGER : specified index of eos_TableList[]
 *
 * \return        datatype - EOS_INTEGER : eos_TableList[i].eosTableType
 *
 ***********************************************************************/
EOS_INTEGER get_eosDataType(EOS_INTEGER i)
{
  return eos_TableList[i].eosTableType;
}

/***********************************************************************/
/*!
 * \brief Get the Sesame table number corresponding to index
 *
 * \param[in]     i        - EOS_INTEGER : specified index of eos_TableList[]
 *
 * \return        tableNum - EOS_INTEGER : eos_TableList[i].tableNum
 *
 ***********************************************************************/
EOS_INTEGER get_eosTableNum(EOS_INTEGER i) {
  return eos_TableList[i].tableNum;
}

/***********************************************************************/
/*!
 * \brief Get the Sesame subtable number corresponding to index
 *
 * \param[in]     i           - EOS_INTEGER : specified index of eos_TableList[]
 *
 * \return        subTableNum - EOS_INTEGER : eos_TableList[i].subTableNum
 *
 ***********************************************************************/
EOS_INTEGER get_eosSubTableNum(EOS_INTEGER i) {
  return eos_TableList[i].subTableNum;
}

/***********************************************************************/
/*!
 * \brief Read and return as EOS_CHAR* the contents of the specified file
 *
 * \param[in]     *fn         - EOS_CHAR*  : file name
 * \param[out]    **str       - EOS_CHAR** : contents of specified file name
 *
 * \return        errorCode -  0: no error
 *                            -1: fopen failed
 *
 ***********************************************************************/
EOS_INTEGER get_fileContent(EOS_CHAR *fn, EOS_CHAR **str) {
  int i;
  int err = EOS_OK;
  FILE *fp = NULL;

  /*  Read file into memory */
  fp = fopen (fn, "r");  /* open indexFileName */
  *str = (EOS_CHAR*) malloc(1 * sizeof (EOS_CHAR));
  *str = strcpy(*str, "");
  i = 0;
  if (fp) {
    EOS_CHAR s[1024];
    while (fgets(s, 1024, fp)) {
      /* this reads the entire file into str[] */
      i = strlen(*str) + strlen(s) + 1;
      *str = realloc(*str, (i * sizeof (EOS_CHAR)));
      strcat(*str, s);
    }
    fclose (fp);              /* close indexFileName */
  }
  else {
    err = -1;
  }
  return(err);
}

/* This function implements the Mersenne twister algorithm for generation of pseudorandom numbers.
 * The program returns random integers in the range 0 to 2^32-1 (this holds even if a long int is
 * larger than 32 bits). Timing with gcc indicates that it is about twice as fast as the built in 
 * rand function. The original code was written by Michael Brundage and has been placed in the 
 * public domain. There are a three minor changes here: 
 * (1) This comment has been added to the program.
 * (2) Type specifiers (ul) have been appended to constants.
 * (3) This now returns a double value in the range 0 to 1 instead of the original unsigned long.
 */
#define MT_LEN 624
#include <stdlib.h>

int mt_index;
unsigned long mt_buffer[MT_LEN];

void mt_init(void) {
  int i;
  for (i = 0; i < MT_LEN; i++)
    mt_buffer[i] = rand();
  mt_index = 0;
}

#define MT_IA           397
#define MT_IB           (MT_LEN - MT_IA)
#define UPPER_MASK      0x80000000
#define LOWER_MASK      0x7FFFFFFF
#define MATRIX_A        0x9908B0DF
#define TWIST(b,i,j)    ((b)[i] & UPPER_MASK) | ((b)[j] & LOWER_MASK)
#define MAGIC(s)        (((s)&1)*MATRIX_A)

/* unsigned long mt_random() { */
double mt_random(void) {
  unsigned long * b = mt_buffer;
  int idx = mt_index;
  unsigned long s;
  int i;
	
  if (idx == MT_LEN*sizeof(unsigned long))
    {
      idx = 0;
      i = 0;
      for (; i < MT_IB; i++) {
	s = TWIST(b, i, i+1);
	b[i] = b[i + MT_IA] ^ (s >> 1) ^ MAGIC(s);
      }
      for (; i < MT_LEN-1; i++) {
	s = TWIST(b, i, i+1);
	b[i] = b[i - MT_IB] ^ (s >> 1) ^ MAGIC(s);
      }
        
      s = TWIST(b, MT_LEN-1, 0);
      b[MT_LEN-1] = b[MT_IA-1] ^ (s >> 1) ^ MAGIC(s);
    }
  mt_index = idx + sizeof(unsigned long);
  /* return *(unsigned long *)((unsigned char *)b + idx); */
  s = *(unsigned long *)((unsigned char *)b + idx);
  return((double)s / 4294967295.);
}

/**********************************************************************
* Latin Hypercube sampling a la McKay et al
* kp 20 July 99
* set up nparam dice of nlev faces each
**********************************************************************/
void latinCube(double *vdata, const double *a, const double *b, int nparam, int nlev/* , unsigned short xsubi[3] */) {
  double *vrnd;
  double *dx;
  int *irnd;
  int i, j, k;

  vrnd = (double *)malloc(nparam*sizeof(double));
  irnd = (int *)malloc(nparam*sizeof(int));
  dx = (double *)malloc(nparam*sizeof(double));

  /* initialize mt_random */
  mt_init();

  /* for each parameter, sample once in each level and                  */
  /* scale the rand num so it lives in the interval (a+dx*i,a+dx*(i+1)) */

  for (j = 0; j < nparam; ++j)
    dx[j] = (b[j]-a[j])/nlev;
  for (i = 0; i < nlev; ++i)
    for (j = 0; j < nparam; ++j)
      vdata[j+nparam*i] = (a[j] + i*dx[j]) + mt_random()*dx[j];

  for (i = 0; i < nlev-1; ++i) {
    /* generate nparam random numbers between 0 and k-1 */

    k = nlev-1-i;
    for (j = 0; j < nparam; ++j)
      irnd[j] = floor(k*mt_random());
    for (j = 0; j < nparam; ++j) {
      vrnd[j] = vdata[j+nparam*irnd[j]];

      /* move the last element up to I(i),     */
      /* if irnd(i) isn't already the last one */

      if (irnd[j] < k)
	vdata[j+nparam*irnd[j]] = vdata[j+nparam*k];
    }
    for (j = 0; j < nparam; ++j)
      vdata[j+nparam*k] = vrnd[j];

  }

  /* the first column is already there.                         */
  /* now there's a set of nparam random numbers in each column. */

  free(vrnd);
  free(dx);
  free(irnd);
}

/* Display gEosDataMap.tableHandlesMap details */
void display_gEosDataMap_tableHandlesMap_details(void)
{
  EOS_INTEGER i, k;
  EOS_INTEGER no_tables, no_handles, no_alloc, no_generalOptions, no_tableOptions;
  no_tables = get_no_tables();
  no_handles = get_no_handles();
  no_alloc = get_no_alloc();
  no_generalOptions =  get_no_generalOptions();
  no_tableOptions =  get_no_tableOptions();

  printf ("no. tables:         %d\n", no_tables);
  printf ("no. handles:        %d\n", no_handles);
  printf ("no. alloc:          %d\n", no_alloc);
  printf ("no. generalOptions: %d\n", no_generalOptions);
  printf ("no. tableOptions:   %d\n", no_tableOptions);
  printf ("\n");

  for (i = 0; i < no_handles; i++) {

    EOS_INTEGER mapVal, typeVal, errorCode;

    if (i > 0) printf ("\n");

    if (get_tableHandlesMap_val(i) < 0) {
      /* table handle is an invalid reference */
      printf ("tableHandlesMap[%d] = %d   *** INVALID TABLE HANDLE ***\n", i, get_tableHandlesMap_val(i));
      continue;
    }

    mapVal = get_tableHandlesMap_val(i);
    typeVal = get_tableTypes_val(i);
    errorCode = get_errorCodes_val(i);

    printf ("tableHandlesMap[%d] = %d tableTypes[%d] = %d errorCodes[%d] = %d\n",
	    i, mapVal, i, typeVal, i, errorCode);

    // print the general options
    printf ("\tgeneralOptions[%d] -> {", i);
    for (k = 0; k < no_generalOptions; k++) {
      if (k > 0)
	printf ("\t                      ");
      printf ("%s -> ", get_generalOption_flag(k));
      switch (get_generalOption_type(k)) {
      case _BOOLEAN_TYPE_INDEX:
	printf ("%d", get_generalOptions_bval(i,k));
	break;
      case _INTEGER_TYPE_INDEX:
	printf ("%d", get_generalOptions_ival(i,k));
	break;
      case _REAL_TYPE_INDEX:
	printf ("%f", get_generalOptions_rval(i,k));
      }
      printf ("%s\n", ((k<(no_generalOptions-1))?",":""));
    }
    printf ("\t                     }\n");

    // print the data table options
    printf ("\t  tableOptions[%d] -> {", i);
    for (k = 0; k < no_tableOptions; k++) {
      if (k > 0)
	printf ("\t                      ");
      printf ("%s -> ", get_tableOption_flag(i,k));
      switch (get_tableOption_type(i,k)) {
      case _BOOLEAN_TYPE_INDEX:
	printf ("%d", get_tableOptions_bval(i,k));
	break;
      case _INTEGER_TYPE_INDEX:
	printf ("%d", get_tableOptions_ival(i,k));
	break;
      case _REAL_TYPE_INDEX:
	printf ("%f", get_tableOptions_rval(i,k));
      }
      printf ("%s\n", ((k<(no_tableOptions-1))?",":""));
    }
    printf ("\t                     }\n");

  }
}

void test_if_globals_are_free(void)
{
  /* test various globals */
  if (gEosDataMap.dataObjects    ) printf ("gEosDataMap.dataObjects     is still defined\n");
  if (gEosDataMap.customErrorMsg ) printf ("gEosDataMap.customErrorMsg  is still defined\n");
  if (gEosDataMap.tableTypes     ) printf ("gEosDataMap.tableTypes      is still defined\n");
  if (gEosDataMap.tableHandlesMap) printf ("gEosDataMap.tableHandlesMap is still defined\n");
  if (gEosDataMap.errorCodes     ) printf ("gEosDataMap.errorCodes      is still defined\n");
  if (gEosDataMap.isHandlePublic ) printf ("gEosDataMap.isHandlePublic  is still defined\n");

  if (gEosInterpolation.interpolationDataList) printf ("gEosInterpolation.interpolationDataList is still defined\n");

  if (gCustomErrorMsg            ) printf ("gCustomErrorMsg             is still defined\n");
  if (eos_DefaultTableOptions    ) printf ("eos_DefaultTableOptions     is still defined\n");
  if (sesameIndexLocations       ) printf ("sesameIndexLocations        is still defined\n");
  if (sesameIndexLocationsL      ) printf ("sesameIndexLocationsL       is still defined\n");
  if (sesameFiles                ) printf ("sesameFiles                 is still defined\n");
  if (sesameFilesL               ) printf ("sesameFilesL                is still defined\n");
  if (sesameFileCache            ) printf ("sesameFileCache             is still defined\n");
}

_EXTERN_C_TAIL_
