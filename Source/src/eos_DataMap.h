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

/* This container is designed to hold the data range boundaries based upon the non-inverted tables.
 * Note that all data stored in this container will be of the default SESAME units. In other words,
 * No transforms will be applied as is done to selected tabulated inverse data tables.
 */
typedef struct
{
    EOS_BOOLEAN stored;
    EOS_INTEGER nx;
    EOS_INTEGER ny;
    EOS_REAL *x; /* independent variable conditionally used for either x or y bounds
                    defined in the next 4 arrays; the following 4 arrays may contain either
                    a constant or a list of values depending upon the associated table type: */
    EOS_REAL *xLo; /* lower bound for table type's x-independent variable; */
    EOS_REAL *yLo; /* lower bound for table type's y-independent variable; */
    EOS_REAL *xHi; /* upper bound for table type's x-independent variable; */
    EOS_REAL *yHi; /* upper bound for table type's y-independent variable; */
} eos_ExtrapolationBoundsEosDataMap;

typedef struct
{
    eos_ErrorHandler eosErrorHandler;     /* must be the FIRST, DO NOT MOVE! */
    EOS_BOOLEAN initialized;              /* is this eos_DataMap initialized and ready for population */
    eos_Data **dataObjects;               /* eos_Data instances, zero or 1 for each table handle */
    EOS_INTEGER *tableHandlesMap;         /* maps table handles to dataObjects array index */
    EOS_INTEGER *tableTypes;              /* for example EOS_Zfc_DT */
    EOS_INTEGER *errorCodes;              /* stores error codes corresponding to each table handle */
    eos_ExtrapolationBoundsEosDataMap *extrapolationBounds; /* store pointers to eos_ExtrapolationBoundsRecordType[126]
                                                            as necessary for each table handle */
    EOS_CHAR ***customErrorMsg;           /* stores custom error messages corresponding to each table handle and error code */
    EOS_INTEGER *isHandlePublic;          /* whether or not the table at this handle is external or internal */
    eos_OptionValue *generalOptions[EOS_NUM_GENERAL_OPTIONS]; /* for each gen. option we store list of option
                                                                 values for each handle */
    EOS_INTEGER nTables;                  /* number of non-NULL entries in the dataObjects array */
    EOS_INTEGER nHandles;                 /* number of valid tableHandles in use */
    EOS_INTEGER nAlloc;                   /* allocated size of the dataObjects array; in order that a
                                             tableHandle value is not re-used during execution, nAlloc is not decreased
                                             until either eos_DestroyAll is called or code execution terminates */
} eos_DataMap;

#include "eos_DataMap.proto.h"

#endif
