/*********************************************************************
 * Class Name : eos_Data
 * ---------------------------------------------------------
 * Filetype: (HEADER)
 * 
 * Copyright -- see file named COPYRIGHTNOTICE
 * 
 *********************************************************************/

#ifndef  EOS_DATA_H
#define  EOS_DATA_H

#include "eos_types.h"
#include "eos_ErrorHandler.h"
#include "eos_universal_types.h"
#include "ses_defines.h"
/* #include "eos_types_internal.h" */

#define _MIN_FIELD_WIDTH 23     /* Define the minimum field width of real numbers written to the output file.
                                   This is used by Print functions for all data objects. */

typedef struct
{
  eos_ErrorHandler eosErrorHandler;     /* must be the FIRST, DO NOT MOVE */
  EOS_INTEGER tableHandle;      /* This is a unique handle associated with a specific data table object. */
  EOS_INTEGER materialID;       /* This is the specific Sesame material ID associated with the data stored in an instantiation of this container. */
  EOS_INTEGER creationDate;
  EOS_INTEGER modificationDate;
  EOS_REAL latestVersion;
  EOS_INTEGER recordType;       /* 1, 2, 3, 4, 5, 6 */
  long dataFileOffset;          
  EOS_INTEGER dataFileIndex;    /* index of the sesame file for reading the data */
  EOS_BOOLEAN userDefinedDataFile;    /* is the associated sesame file specified by the user? */
  EOS_INTEGER dataSize;         /* total size of data for this table in sesame file */
  EOS_INTEGER destructing;      /* to prevent circular calls */
  EOS_INTEGER refCounter;       /* number of handles to this object */
  EOS_INTEGER isLoaded;         /* 0 when not loaded, 1 when loaded, -1 when can't be loaded */
  EOS_INTEGER tableNum;
  EOS_INTEGER numSubtablesLoaded;
  EOS_BOOLEAN coldCurveIsLoaded;
  EOS_BOOLEAN forceCreate;
  EOS_BOOLEAN dumpNotLoadedMsg; /* NOT LOADED message is to be written to TablesLoaded.dat file */
  eos_Option tableOptions[EOS_NUM_LOADING_OPTIONS];
  EOS_CHAR *altDataSource;      /* string to contain description of alternative data source (i.e., analytical model) */
  EOS_INTEGER varOrder;         /* define the variable order flag for interpolation-related functions  */
  EOS_INTEGER tmpVarOrder;      /* define a temporary variable order flag for interpolation-related functions  */

  /* Virtual function prototypes */
  void (*Load) (void *me, EOS_INTEGER th);
  void (*Create) (void *me, EOS_INTEGER th);
  void (*Destroy) (void *ptr);
  void (*SetFileIndexes) (void *me, EOS_INTEGER th);
  void (*MakeSmooth) (void *me, EOS_INTEGER th, EOS_INTEGER dataType,
                      EOS_BOOLEAN ptSmooth, EOS_INTEGER *err);
  void (*IsMonotonic) (void *me, EOS_INTEGER dataType,
                       EOS_BOOLEAN *isMonotonic, EOS_BOOLEAN inX,
                       EOS_BOOLEAN inY, EOS_INTEGER *err);
  void (*MakeMonotonic) (void *me, EOS_INTEGER th, EOS_INTEGER dataType,
                         EOS_BOOLEAN inX, EOS_BOOLEAN inY, EOS_INTEGER *err);
  void (*Print) (void *me, EOS_INTEGER th, EOS_CHAR *fname, EOS_INTEGER append, EOS_INTEGER *err);
  void (*GetPackedTable) (void *me, EOS_INTEGER th, EOS_CHAR *packedTable,
                          EOS_INTEGER *errorCode);
  void (*SetPackedTable) (void *me, EOS_INTEGER th,
                          EOS_CHAR *packedTable,
                          EOS_INTEGER *errorCode);
  void (*GetPackedTableSize) (void *me, EOS_INTEGER th,
                              EOS_INTEGER *packedTableSize,
                              EOS_INTEGER *errorCode);
  void (*GetTableInfo) (void *me, EOS_INTEGER th, EOS_INTEGER numInfoItems,
                        EOS_INTEGER *infoItems, EOS_REAL *infoVals,
                        EOS_INTEGER *err);
  void (*GetTableCmnts) (void *me, EOS_CHAR *cmntStr, EOS_INTEGER *err);
  void (*GetTableMetaData) (void *me, EOS_INTEGER infoItem, EOS_CHAR *infoStr, EOS_INTEGER *err);
  void (*GetLoadedBulkData) (void *me, EOS_REAL *zbar, EOS_REAL *abar,
                             EOS_REAL *dens0, EOS_INTEGER *errorCode);
  void (*SetMonotonicity) (void *me, EOS_INTEGER dataType, EOS_INTEGER inX,
                           EOS_INTEGER inY);
  void (*GetMonotonicity) (void *me, EOS_INTEGER dataType, EOS_INTEGER *inX,
                           EOS_INTEGER *inY);
  void (*AreMonotonicRequirementsCompatible) (void *me, EOS_INTEGER dataType,
                                              EOS_INTEGER inX,
                                              EOS_INTEGER inY,
                                              EOS_BOOLEAN *compatible);
  void (*SetSmoothing) (void *me, EOS_INTEGER smooth, EOS_INTEGER pt_smooth,
                        EOS_INTEGER dataType);
  void (*GetSmoothing) (void *me, EOS_INTEGER dataType,
                        EOS_INTEGER *makeSmooth, EOS_INTEGER *pt_smooth);
  void (*AreSmoothingRequirementsCompatible) (void *me, EOS_INTEGER dataType,
                                              EOS_INTEGER makeSmooth,
                                              EOS_INTEGER makePtSmooth,
                                              EOS_BOOLEAN *compatible);
  void (*SetExtrapolationBounds) (void *ptr, EOS_INTEGER dataType);
  void (*InvertAtSetup) (void *ptr, EOS_INTEGER th, EOS_INTEGER dataType, EOS_INTEGER *errorCode);
  void (*Interpolate) (void *ptr, EOS_INTEGER th, EOS_INTEGER dataType,
		       EOS_INTEGER nXYPairs, EOS_REAL *srchX,
		       EOS_REAL *srchY, EOS_REAL *fVals,
		       EOS_REAL *dFx, EOS_REAL *dFy,
		       EOS_INTEGER *xyBounds,
		       EOS_INTEGER *errorCode);
  void (*CheckExtrap) (void *ptr, EOS_INTEGER th, EOS_INTEGER dataType,
		       EOS_INTEGER nXYPairs, EOS_REAL *srchX,
		       EOS_REAL *srchY, EOS_INTEGER *xyBounds,
		       EOS_INTEGER *errorCode);
  EOS_BOOLEAN (*IsaShareableObject) (void *ptr);
} eos_Data;

#include "eos_Data.proto.h"

#endif
