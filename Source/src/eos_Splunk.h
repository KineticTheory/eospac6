/*********************************************************************
 * Class Name : eos_Splunk
 * ---------------------------------------------------------
 * Filetype: (HEADER)
 * 
 * Copyright -- see file named COPYRIGHTNOTICE
 * 
 ********************************************************************/

/* taken from TEST_FUNCTIONS.proto.h */
EOS_CHAR* get_optionStr_FromFlag(EOS_INTEGER f);

#ifndef EOS_SPLUNK_H
#define EOS_SPLUNK_H

#define EOS_SPLUNK_ACCUMULATOR_MIN_KEY 999900

#define EOS_SPLUNK_ACCUMULATOR_EOS_CHECKEXTRAP              999900
#define EOS_SPLUNK_ACCUMULATOR_EOS_CREATETABLES             999901 
#define EOS_SPLUNK_ACCUMULATOR_EOS_DESTROYALL               999902 
#define EOS_SPLUNK_ACCUMULATOR_EOS_DESTROYTABLES            999903 
#define EOS_SPLUNK_ACCUMULATOR_EOS_GETERRORCODE             999904 
#define EOS_SPLUNK_ACCUMULATOR_EOS_GETERRORMESSAGE          999905 
#define EOS_SPLUNK_ACCUMULATOR_EOS_GETPACKEDTABLES          999906 
#define EOS_SPLUNK_ACCUMULATOR_EOS_GETPACKEDTABLESSIZE      999907 
#define EOS_SPLUNK_ACCUMULATOR_EOS_GETMETADATA              999908 
#define EOS_SPLUNK_ACCUMULATOR_EOS_GETTABLEMETADATA         999909 
#define EOS_SPLUNK_ACCUMULATOR_EOS_GETTABLEINFO             999910 
#define EOS_SPLUNK_ACCUMULATOR_EOS_GETTABLECMNTS            999911 
#define EOS_SPLUNK_ACCUMULATOR_EOS_INTERPOLATE              999912 
#define EOS_SPLUNK_ACCUMULATOR_EOS_LOADTABLES               999913 
#define EOS_SPLUNK_ACCUMULATOR_EOS_MIX                      999914 
#define EOS_SPLUNK_ACCUMULATOR_EOS_SETPACKEDTABLES          999915 
#define EOS_SPLUNK_ACCUMULATOR_EOS_SETOPTION                999916 
#define EOS_SPLUNK_ACCUMULATOR_EOS_RESETOPTION              999917 
#define EOS_SPLUNK_ACCUMULATOR_EOS_GETMAXDATAFILENAMELENGTH 999918 
#define EOS_SPLUNK_ACCUMULATOR_EOS_SETDATAFILENAME          999919 
#define EOS_SPLUNK_ACCUMULATOR_EOS_ERRORCODESEQUAL          999920 
#define EOS_SPLUNK_ACCUMULATOR_EOS_SETDATAFILENAME_CWRAPPER 999921 
#define EOS_SPLUNK_ACCUMULATOR_EOS_TIME                     999922 
#define EOS_SPLUNK_ACCUMULATOR_EOS_GETVERSIONLENGTH         999923 
#define EOS_SPLUNK_ACCUMULATOR_EOS_GETVERSION               999924 

#define EOS_SPLUNK_ACCUMULATOR_MAX_KEY                      999924 /* This key must be same as last */

#define EOS_SPLUNK_ACCUMULATOR_COUNT EOS_SPLUNK_ACCUMULATOR_MAX_KEY-EOS_SPLUNK_ACCUMULATOR_MIN_KEY+1

#define EOS_SPLUNK_ACCUMULATOR_STR_LEN 1024
#define EOS_SPLUNK_ACCUMULATOR_BUF_LEN 100

typedef struct {
    EOS_CHAR *functionName;         /* what is the name of the EOSPAC6 API function? */
    EOS_BOOLEAN acc_active;         /* is this accumulator currently used during execution? */
    EOS_BOOLEAN acc_disable;        /* is _eos_splunk and this accumulator to be used? */
    EOS_INTEGER min_N;              /* minimum value of N (i.e., NXYPairs) */
    EOS_INTEGER max_N;              /* maximum value of N (i.e., NXYPairs) */
    EOS_INTEGER callCounter;        /* count of calls to functionName */
    EOS_INTEGER ibuf;               /* next available msg_buffer[ibuf] */
    EOS_CHAR msg_buffer[EOS_SPLUNK_ACCUMULATOR_BUF_LEN][EOS_SPLUNK_ACCUMULATOR_STR_LEN]; /* array of strings to buffer up to 100 syslog messages of
                                                                                            length EOS_SPLUNK_ACCUMULATOR_STR_LEN */
} EOS_SPLUNK_ACCUMULATOR_T;

#ifdef _EOS_SPLUNK_INTERNAL_

EOS_SPLUNK_ACCUMULATOR_T eos_SplunkAccumulator[EOS_SPLUNK_ACCUMULATOR_COUNT] = {
  /* functionName                   acc_active  acc_disable  min_N   max_N  callCounter  ibuf msg_buffer */
  { "eos_CheckExtrap",              EOS_FALSE,  EOS_TRUE,    999999, 0,     0,           0,   {""}}, /* EOS_SPLUNK_ACCUMULATOR_EOS_CHECKEXTRAP              */ 
  { "eos_CreateTables",             EOS_FALSE,  EOS_TRUE,    999999, 0,     0,           0,   {""}}, /* EOS_SPLUNK_ACCUMULATOR_EOS_CREATETABLES             */ 
  { "eos_DestroyAll",               EOS_FALSE,  EOS_TRUE,    999999, 0,     0,           0,   {""}}, /* EOS_SPLUNK_ACCUMULATOR_EOS_DESTROYALL               */ 
  { "eos_DestroyTables",            EOS_FALSE,  EOS_TRUE,    999999, 0,     0,           0,   {""}}, /* EOS_SPLUNK_ACCUMULATOR_EOS_DESTROYTABLES            */ 
  { "eos_GetErrorCode",             EOS_FALSE,  EOS_TRUE,    999999, 0,     0,           0,   {""}}, /* EOS_SPLUNK_ACCUMULATOR_EOS_GETERRORCODE             */ 
  { "eos_GetErrorMessage",          EOS_FALSE,  EOS_TRUE,    999999, 0,     0,           0,   {""}}, /* EOS_SPLUNK_ACCUMULATOR_EOS_GETERRORMESSAGE          */ 
  { "eos_GetPackedTables",          EOS_FALSE,  EOS_TRUE,    999999, 0,     0,           0,   {""}}, /* EOS_SPLUNK_ACCUMULATOR_EOS_GETPACKEDTABLES          */ 
  { "eos_GetPackedTablesSize",      EOS_FALSE,  EOS_TRUE,    999999, 0,     0,           0,   {""}}, /* EOS_SPLUNK_ACCUMULATOR_EOS_GETPACKEDTABLESSIZE      */ 
  { "eos_GetMetaData",              EOS_FALSE,  EOS_TRUE,    999999, 0,     0,           0,   {""}}, /* EOS_SPLUNK_ACCUMULATOR_EOS_GETMETADATA              */ 
  { "eos_GetTableMetaData",         EOS_FALSE,  EOS_TRUE,    999999, 0,     0,           0,   {""}}, /* EOS_SPLUNK_ACCUMULATOR_EOS_GETTABLEMETADATA         */ 
  { "eos_GetTableInfo",             EOS_FALSE,  EOS_TRUE,    999999, 0,     0,           0,   {""}}, /* EOS_SPLUNK_ACCUMULATOR_EOS_GETTABLEINFO             */ 
  { "eos_GetTableCmnts",            EOS_FALSE,  EOS_TRUE,    999999, 0,     0,           0,   {""}}, /* EOS_SPLUNK_ACCUMULATOR_EOS_GETTABLECMNTS            */ 
  { "eos_Interpolate",              EOS_FALSE,  EOS_TRUE,    999999, 0,     0,           0,   {""}}, /* EOS_SPLUNK_ACCUMULATOR_EOS_INTERPOLATE              */ 
  { "eos_LoadTables",               EOS_FALSE,  EOS_TRUE,    999999, 0,     0,           0,   {""}}, /* EOS_SPLUNK_ACCUMULATOR_EOS_LOADTABLES               */ 
  { "eos_Mix",                      EOS_FALSE,  EOS_TRUE,    999999, 0,     0,           0,   {""}}, /* EOS_SPLUNK_ACCUMULATOR_EOS_MIX                      */ 
  { "eos_SetPackedTables",          EOS_FALSE,  EOS_TRUE,    999999, 0,     0,           0,   {""}}, /* EOS_SPLUNK_ACCUMULATOR_EOS_SETPACKEDTABLES          */ 
  { "eos_SetOption",                EOS_FALSE,  EOS_FALSE,   999999, 0,     0,           0,   {""}}, /* EOS_SPLUNK_ACCUMULATOR_EOS_SETOPTION                */ 
  { "eos_ResetOption",              EOS_FALSE,  EOS_FALSE,   999999, 0,     0,           0,   {""}}, /* EOS_SPLUNK_ACCUMULATOR_EOS_RESETOPTION              */ 
  { "eos_GetMaxDataFileNameLength", EOS_FALSE,  EOS_TRUE,    999999, 0,     0,           0,   {""}}, /* EOS_SPLUNK_ACCUMULATOR_EOS_GETMAXDATAFILENAMELENGTH */ 
  { "eos_SetDataFileName",          EOS_FALSE,  EOS_FALSE,   999999, 0,     0,           0,   {""}}, /* EOS_SPLUNK_ACCUMULATOR_EOS_SETDATAFILENAME          */ 
  { "eos_ErrorCodesEqual",          EOS_FALSE,  EOS_TRUE,    999999, 0,     0,           0,   {""}}, /* EOS_SPLUNK_ACCUMULATOR_EOS_ERRORCODESEQUAL          */ 
  { "eos_SetDataFileName_Cwrapper", EOS_FALSE,  EOS_TRUE,    999999, 0,     0,           0,   {""}}, /* EOS_SPLUNK_ACCUMULATOR_EOS_SETDATAFILENAME_CWRAPPER */ 
  { "eos_Time",                     EOS_FALSE,  EOS_TRUE,    999999, 0,     0,           0,   {""}}, /* EOS_SPLUNK_ACCUMULATOR_EOS_TIME                     */ 
  { "eos_GetVersionLength",         EOS_FALSE,  EOS_TRUE,    999999, 0,     0,           0,   {""}}, /* EOS_SPLUNK_ACCUMULATOR_EOS_GETVERSIONLENGTH         */ 
  { "eos_GetVersion",               EOS_FALSE,  EOS_TRUE,    999999, 0,     0,           0,   {""}}  /* EOS_SPLUNK_ACCUMULATOR_EOS_GETVERSION               */ 
};

#else // if not _EOS_SPLUNK_INTERNAL_

extern EOS_SPLUNK_ACCUMULATOR_T eos_SplunkAccumulator;

#endif

#endif /* EOS_SPLUNK_H */

/* Function prototypes */
EOS_BOOLEAN _eos_splunk_get_acc_disable(EOS_INTEGER key);
EOS_BOOLEAN _eos_splunk_get_acc_active(EOS_INTEGER key);
void _eos_splunk(EOS_INTEGER key, EOS_CHAR *val);
void _eos_splunk_flush(EOS_INTEGER key);
