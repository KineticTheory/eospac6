/*********************************************************************
 * C++ Class Name : eos_DataMap 
 * ---------------------------------------------------------
 * Filetype: (HEADER)
 * 
 * Copyright -- see file named COPYRIGHTNOTICE
 * 
 *********************************************************************/
#ifndef  EOS_DATAMAP_H
#define  EOS_DATAMAP_H

#include "eos_Data.h"

typedef struct
{
  eos_ErrorHandler eosErrorHandler;     /* must be the FIRST, DO NOT MOVE! */
  eos_Data **dataObjects;       /* eos_Data instances, zero or 1 for each table handle */
  EOS_INTEGER *tableHandlesMap; /* maps table handles to dataObjects array index */
  EOS_INTEGER *tableTypes;      /* for example EOS_Zfc_DT */
  EOS_INTEGER *errorCodes;      /* stores error codes corresponding to each table handle */
  EOS_CHAR ***customErrorMsg;   /* stores custom error messages corresponding to each table handle and error code */
  EOS_INTEGER *isHandlePublic;  /* whether or not the table at this handle is external or internal */
  eos_OptionValue *generalOptions[EOS_NUM_GENERAL_OPTIONS];     /* for each gen. option we store list of option
                                                                   values for each handle */
  EOS_INTEGER nTables;          /* number of non-NULL entries in the dataObjects array */
  EOS_INTEGER nHandles;         /* number of valid tableHandles in use */
  EOS_INTEGER nAlloc;           /* allocated size of the dataObjects array; never decreases so a
                                   tableHandle value is never re-used during execution */
} eos_DataMap;

#include "eos_DataMap.proto.h"

extern void eos_CleanUpColdCurveRecordType1 (void *ptr, EOS_INTEGER *err);

#endif
