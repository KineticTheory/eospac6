/*********************************************************************
 * Class Name : eos_Data
 * ---------------------------------------------------------
 * Filetype: (SOURCE)
 *
 * Copyright -- see file named COPYRIGHTNOTICE
 *
 *********************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "eos_ErrorHandler.h"

#define _EOS_DATA_INTERNAL_PROTOTYPES
#include "eos_types_internal.h"
#include "eos_Data.h"

#include "eos_Utils.h"

#include "eos_RecordType1.h"
#include "eos_RecordType2.h"
#include "eos_RecordType3.h"
#include "eos_RecordType4.h"
#include "eos_RecordType5.h"
#include "eos_RecordType6.h"

/************************************************************************
 * 
 * eos_Data class destructor
 * 
 * Returned Values: none
 *
 * Input Value:
 * eos_Data **me  - this pointer (pointer to the instance of type eos_Data)
 * 
 ************************************************************************/
void eos_DestroyEosData (eos_Data *ptr)
{
  void *p = (void*)ptr;

  if (!ptr && ptr->refCounter > 0)
    return;
  ptr->destructing = 1;         /* to prevent circular calls */
  eos_DestroyEosErrorHandler (&(ptr->eosErrorHandler));

  EOS_FREE(ptr->altDataSource);

  ptr->Destroy(p);
}

/************************************************************************
 * 
 * eosData class constructor
 * 
 * Returned Values: none
 *
 * Input Value:
 * eos_RecordType1 *me         - this pointer (pointer to the instance of type eos_eosData
 * EOS_INTEGER      materialID - id of material to load.
 * EOS_INTEGER      th  - table handle.
 * 
 ************************************************************************/
void eos_ConstructEosData (eos_Data *me, EOS_INTEGER th,
                           EOS_INTEGER materialID)
{
  int i, ind, tableType;

  tableType = eos_GetDataTypeFromTableHandle (th, &i);

  me->tableHandle = 0;
  me->numSubtablesLoaded = 0;
  me->coldCurveIsLoaded = EOS_FALSE;
  me->materialID = materialID;
  me->creationDate = 0;
  me->modificationDate = 0;
  me->latestVersion = 0;
  me->recordType = EOS_TYPE_TO_RECORD_TYPE (tableType);
  me->Load = NULL;                               /* pure virtual function */
  me->Create = NULL;                             /* pure virtual function */
  me->Destroy = NULL;                            /* pure virtual function */
  me->Print = NULL;                              /* pure virtual function */
  me->GetLoadedBulkData = NULL;                  /* pure virtual function */
  me->GetPackedTable = NULL;                     /* pure virtual function */
  me->GetPackedTableSize = NULL;                 /* pure virtual function */
  me->SetPackedTable = NULL;                     /* pure virtual function */
  me->IsMonotonic = NULL;                        /* pure virtual function */
  me->MakeMonotonic = NULL;                      /* pure virtual function */
  me->MakeSmooth = NULL;                         /* pure virtual function */
  me->GetTableInfo = NULL;                       /* pure virtual function */
  me->GetTableCmnts = NULL;                      /* pure virtual function */
  me->GetTableMetaData = NULL;                   /* pure virtual function */
  me->SetMonotonicity = NULL;                    /* pure virtual function */
  me->GetMonotonicity = NULL;                    /* pure virtual function */
  me->GetSmoothing = NULL;                       /* pure virtual function */
  me->AreMonotonicRequirementsCompatible = NULL; /* pure virtual function */
  me->SetSmoothing = NULL;                       /* pure virtual function */
  me->AreSmoothingRequirementsCompatible = NULL; /* pure virtual function */
  me->InvertAtSetup = NULL;                      /* pure virtual function */
  me->AllocateColdCurve = NULL;                  /* pure virtual function */
  me->CleanUpColdCurve = NULL;                   /* pure virtual function */
  me->eos_IsRequiredDataLoaded = NULL;           /* pure virtual function */
  me->IsaShareableObject = _eos_IsaShareableObjectEosData; /* pure virtual function */
  me->DumpExpandedGrid = NULL;                   /* pure virtual function */
  me->AreGhostDataRequired = NULL;               /* pure virtual function */
  me->SetUseTmpGhostData = NULL;                 /* pure virtual function */
  me->GenerateHashTables = NULL;                 /* pure virtual function */
#ifdef DO_OFFLOAD
  me->GpuOffloadData = NULL;                     /* pure virtual function */
#endif /* DO_OFFLOAD */

  eos_ConstructEosErrorHandler ((eos_ErrorHandler *) me);
  me->eosErrorHandler.HandleError = eos_HandleErrorEosData;     /* derived virtual function */
  me->eosErrorHandler.errorCode = EOS_OK;                       /* initialize error code */
  me->destructing = 0;          /* to prevent circular calls to destructor */
  me->refCounter = 0;
  me->isAllocated = 0;
  me->isLoaded = 0;
  me->forceCreate = EOS_FALSE;
  me->dumpNotLoadedMsg = EOS_FALSE;
  me->tableNum = EOS_TYPE_TO_TAB_NUM (tableType);
  me->userDefinedDataFile = EOS_FALSE;
  for (i = 0; i < EOS_TOTAL_TABLE_OPTIONS; i++) {
    if (!EOS_IS_LOADING_OPTION (eos_DefaultTableOptions[i].optionFlag))
      continue;

    ind =
      EOS_LOADING_OPTION_FLAG_TO_INDEX (eos_DefaultTableOptions[i].
                                        optionFlag);
    me->tableOptions[ind].optionFlag = eos_DefaultTableOptions[i].optionFlag;
    me->tableOptions[ind].optionType = eos_DefaultTableOptions[i].optionType;
    me->tableOptions[ind].optionValue =
      eos_DefaultTableOptions[i].optionValue;
  }
  me->altDataSource = NULL;
}

/************************************************************************
 * 
 * function that allocates the object of eos_Data type by actually going
 * in and calling a constructor for the object's specific derived type,
 * returns the opbject pointer.
 * 
 * Returned Values: 
 * eos_Data **me  - this pointer to pointer to the new instance of type eos_Data
 *
 * Input Value:
 * eos_Data **me  - this pointer to pointer to where instance of type eos_Data shpuld be stored
 * EOS_INTEGER  materialID
 * EOS_INTEGER  th - table handle
 * 
 ************************************************************************/
eos_Data *eos_AllocEosData (EOS_INTEGER materialID, EOS_INTEGER th)
{
  EOS_INTEGER recordType, tableType, err;
  eos_Data *ptr = NULL;

  tableType = eos_GetDataTypeFromTableHandle (th, &err);
  if (eos_GetStandardErrorCodeFromCustomErrorCode(err) != EOS_OK)
    return NULL;

  recordType = EOS_TYPE_TO_RECORD_TYPE (tableType);
  switch (recordType) {
  case EOS_RECORD_TYPE1:
    ptr = (eos_Data *) malloc (sizeof (eos_RecordType1));
    eos_ConstructRecordType1 ((eos_RecordType1 *) ptr, th, materialID);
    break;
  case EOS_RECORD_TYPE2:
    ptr = (eos_Data *) malloc (sizeof (eos_RecordType2));
    eos_ConstructRecordType2 ((eos_RecordType2 *) ptr, th, materialID);
    break;
  case EOS_RECORD_TYPE3:
    ptr = (eos_Data *) malloc (sizeof (eos_RecordType3));
    eos_ConstructRecordType3 ((eos_RecordType3 *) ptr, th, materialID);
    break;
  case EOS_RECORD_TYPE4:
    ptr = (eos_Data *) malloc (sizeof (eos_RecordType4));
    eos_ConstructRecordType4 ((eos_RecordType4 *) ptr, th, materialID);
    break;
  case EOS_RECORD_TYPE5:
    ptr = (eos_Data *) malloc (sizeof (eos_RecordType5));
    eos_ConstructRecordType5 ((eos_RecordType5 *) ptr, th, materialID);
    break;
  case EOS_RECORD_TYPE6:
    ptr = (eos_Data *) malloc (sizeof (eos_RecordType6));
    eos_ConstructRecordType6 ((eos_RecordType6 *) ptr, th, materialID);
    break;
  }
  return ptr;
}

/************************************************************************
 * 
 * Function that performs a protected deallocation the eos_Data object. The protection
 * is provide by checking (*ptr)->refCounter value.
 * Sets pointer, *ptr, to NULL.
 * 
 * Returned Values: none
 *
 * Input Value:
 * eos_Data **ptr  - this pointer to instance of type eos_Data to be deleted
 * 
 ************************************************************************/
void eos_FreeEosData (eos_Data **ptr, EOS_INTEGER recordType)
{
  EOS_INTEGER i;

  if (!ptr)
    return;
  /* Search gEosDataMap.dataObjects[] for *ptr value and deallocate it if refCounter<=0. */
  for (i = 0; i < gEosDataMap.nAlloc; i++) {
    if (! gEosDataMap.dataObjects[i]) continue; /* skip previously-nullified pointers */
    if (*ptr && *ptr == gEosDataMap.dataObjects[i])
      if ((*ptr)->refCounter <= 0) {
	EOS_FREE (*ptr);
	gEosDataMap.dataObjects[i] = *ptr;
      }
  }
}


/************************************************************************
 * 
 * virtual method function that handles specific error code.
 * 
 * Returned Values: none
 *
 * Input Value:
 * eos_ErrorHandler *me  - pointer to an instance of type eos_ErrorHandler.
 * 
 ************************************************************************/
void eos_HandleErrorEosData (void *ptr, EOS_INTEGER th, EOS_INTEGER errorCode)
{
  eos_ErrorHandler *me = (eos_ErrorHandler *) ptr;
  gEosDataMap.errorCodes[th] = errorCode;
  me->errorCode = errorCode;
#ifdef DEBUG
  printf ("eos_Data: error is: %s\n", eos_GetErrorMsg (errorCode));
#endif
}

/************************************************************************
 * 
 * virtual method function that set table-specific options.
 * 
 * Returned Values:
 * EOS_INTEGER error code
 *
 * Input Value:
 * eos_Data *me  - pointer to an instance of type eos_Data.
 * EOS_INTEGER monDataType - data Type for monotonicity
 * EOS_INTEGER dataType   - data type that is being set (not always equal monDataType
 * EOS_INTEGER tableOption  - option to set
 * EOS_REAL    tableOptionValue - optional option value to set
 * 
 ************************************************************************/
void eos_SetOptionEosData (eos_Data *me, EOS_INTEGER dataType,
                           EOS_INTEGER monDataType, EOS_INTEGER tableOption,
                           EOS_REAL tableOptionVal, EOS_INTEGER *errorCode)
{
  EOS_INTEGER cat, subType1, subType2, optInd, pt_smooth = 0, monotonicX =
    0, monotonicY = 0, smooth = 0;
  *errorCode = EOS_OK;

  optInd = EOS_LOADING_OPTION_FLAG_TO_INDEX (tableOption);
  if (optInd < 0 || optInd > EOS_NUM_LOADING_OPTIONS - 1) {
    *errorCode = EOS_INVALID_OPTION_FLAG;
    return;
  }

  switch (tableOption) {
  case EOS_INSERT_DATA:
    me->tableOptions[optInd].optionValue.ival = (EOS_INTEGER) tableOptionVal;
    break;
  case EOS_ADJUST_VAP_PRES:
    me->tableOptions[optInd].optionValue.rval = (EOS_REAL) tableOptionVal;
    break;
  case EOS_MONOTONIC_IN_X:
    monotonicX = 1;
    if (monDataType < 0)
      me->tableOptions[optInd].optionValue.bval = (EOS_BOOLEAN) EOS_TRUE;
    break;
  case EOS_MONOTONIC_IN_Y:
    monotonicY = 1;
    if (monDataType < 0)
      me->tableOptions[optInd].optionValue.bval = (EOS_BOOLEAN) EOS_TRUE;
    break;
  default:
    me->tableOptions[optInd].optionValue.bval = (EOS_BOOLEAN) EOS_TRUE;
  }

  if (tableOption == EOS_MONOTONIC_IN_X || tableOption == EOS_MONOTONIC_IN_Y) {
    cat = EOS_CATEGORY (dataType);
    /* if monotonicity is requested for the entire cat 3 or 4 type */
    if (monDataType < 0 && (cat == 3 || cat == 4)) {
      subType1 = EOS_EOS_TABLE_TYPE_REF1 (dataType);
      me->SetMonotonicity (me, subType1, monotonicX, monotonicY);
      subType2 = EOS_EOS_TABLE_TYPE_REF2 (dataType);
      me->SetMonotonicity (me, subType2, monotonicX, monotonicY);
    }
    else {
      if (monDataType < 0)
        monDataType = dataType;
      me->SetMonotonicity (me, monDataType, monotonicX, monotonicY);
    }
  }
  if (tableOption == EOS_SMOOTH || tableOption == EOS_PT_SMOOTHING) {
    smooth =
      (me->tableOptions[EOS_LOADING_OPTION_FLAG_TO_INDEX (EOS_SMOOTH)].
       optionValue.bval) ? 1 : 0;
    pt_smooth =
      (me->tableOptions[EOS_LOADING_OPTION_FLAG_TO_INDEX (EOS_PT_SMOOTHING)].
       optionValue.bval) ? 1 : 0;
    if (smooth || pt_smooth)
      me->SetSmoothing (me, dataType, smooth, pt_smooth);
  }
}

/************************************************************************
 * 
 * virtual method function that resets table-specific options to default.
 * 
 * Returned Values:
 * EOS_INTEGER error code
 *
 * Input Value:
 * eos_Data *me  - pointer to an instance of type eos_Data.
 * EOS_INTEGER monDataType - data Type for monotonicity
 * EOS_INTEGER dataType   - data type that is being set (not always equal monDataType
 * EOS_INTEGER tableOption  - option to reset
 * 
 ************************************************************************/
void eos_ResetOptionEosData (eos_Data *me, EOS_INTEGER dataType,
                             EOS_INTEGER monDataType, EOS_INTEGER tableOption,
                             EOS_INTEGER *errorCode)
{
  EOS_INTEGER cat, subType1, subType2, optInd, monotonicX, monotonicY,
    pt_smooth, smooth;
  *errorCode = EOS_OK;

  optInd = EOS_LOADING_OPTION_FLAG_TO_INDEX (tableOption);
  if (optInd < 0 || optInd > EOS_NUM_LOADING_OPTIONS - 1) {
    *errorCode = EOS_INVALID_OPTION_FLAG;
    return;
  }

  switch (tableOption) {
  case EOS_INSERT_DATA:
    me->tableOptions[optInd].optionValue.ival =
      eos_DefaultTableOptions[optInd].optionValue.ival;
    break;
  case EOS_ADJUST_VAP_PRES:
    me->tableOptions[optInd].optionValue.rval =
      eos_DefaultTableOptions[optInd].optionValue.rval;
    break;
  case EOS_MONOTONIC_IN_X:
    monotonicX = (!eos_DefaultTableOptions[optInd].optionValue.bval) ? 0 : 1;
    if (monDataType < 0)
      me->tableOptions[optInd].optionValue.bval =
        eos_DefaultTableOptions[optInd].optionValue.bval;
    break;
  case EOS_MONOTONIC_IN_Y:
    monotonicY = (!eos_DefaultTableOptions[optInd].optionValue.bval) ? 0 : 1;
    if (monDataType < 0)
      me->tableOptions[optInd].optionValue.bval =
        eos_DefaultTableOptions[optInd].optionValue.bval;
  default:
    me->tableOptions[optInd].optionValue.bval =
      eos_DefaultTableOptions[optInd].optionValue.bval;
  }

  if (tableOption == EOS_MONOTONIC_IN_X || tableOption == EOS_MONOTONIC_IN_Y) {
    cat = EOS_CATEGORY (dataType);
    /* if monotonicity is requested for the entire cat 3 or 4 type */
    if (monDataType < 0 && (cat == 3 || cat == 4)) {
      subType1 = EOS_EOS_TABLE_TYPE_REF1 (dataType);
      me->SetMonotonicity (me, subType1, monotonicX, monotonicY);
      subType2 = EOS_EOS_TABLE_TYPE_REF2 (dataType);
      me->SetMonotonicity (me, subType2, monotonicX, monotonicY);
    }
    else {
      if (monDataType < 0)
        monDataType = dataType;
      me->SetMonotonicity (me, monDataType, monotonicX, monotonicY);
    }
  }
  if (tableOption == EOS_SMOOTH || tableOption == EOS_PT_SMOOTHING) {
    smooth =
      (me->tableOptions[EOS_LOADING_OPTION_FLAG_TO_INDEX (EOS_SMOOTH)].
       optionValue.bval) ? 1 : 0;
    pt_smooth =
      (me->tableOptions[EOS_LOADING_OPTION_FLAG_TO_INDEX (EOS_PT_SMOOTHING)].
       optionValue.bval) ? 1 : 0;
    if (smooth || pt_smooth)
      me->SetSmoothing (me, dataType, smooth, pt_smooth);
  }
}

/************************************************************************
 * 
 * virtual method function that packs table-specific options into data array.
 * 
 * Returned Values:
 * EOS_INTEGER error code
 * EOS_INTEGER byteCount  - number of bytes needed to store packed options.
 *
 * Input Value:
 * eos_Data *me  - pointer to an instance of type eos_Data.
 * EOS_CHAR *packedData
 *                 - if NULL, then no data is packed, just the byteCount
 *                   is calculated and returned
 * 
 ************************************************************************/
void eos_PackOptionsEosData (eos_Data *me, EOS_CHAR *packedData,
                             EOS_INTEGER *byteCount, EOS_INTEGER *errorCode)
{
  EOS_INTEGER i, tableOption;
  EOS_BOOLEAN getSizeOnly = EOS_FALSE;

  /* determine if only packed tables' size is requested */
  getSizeOnly = (packedData == NULL) ? EOS_TRUE : EOS_FALSE;

  *byteCount = 0;
  *errorCode = EOS_OK;

  for (i = 0; i < EOS_NUM_LOADING_OPTIONS; i++) {
    tableOption = me->tableOptions[i].optionFlag;
    switch (tableOption) {
    case EOS_INSERT_DATA:
      if (!getSizeOnly)
	memcpy (packedData + *byteCount,
		&(me->tableOptions[i].optionValue.ival), sizeof (EOS_INTEGER));
      *byteCount += sizeof (EOS_INTEGER);
      break;
    default:
      if (!getSizeOnly)
	memcpy (packedData + *byteCount,
		&(me->tableOptions[i].optionValue.bval), sizeof (EOS_BOOLEAN));
      *byteCount += sizeof (EOS_BOOLEAN);
      break;
    }
  }
}

/************************************************************************
 * 
 * virtual method function that unpacks table-specific options from given data array.
 * 
 * Returned Values:
 * EOS_INTEGER error code
 *
 * Input Value:
 * eos_Data *me  - pointer to an instance of type eos_Data.
 * EOS_CHAR *packedData
 * 
 ************************************************************************/
void eos_UnpackOptionsEosData (eos_Data *me, EOS_CHAR *packedData,
                               EOS_INTEGER *errorCode)
{
  EOS_INTEGER i, tableOption, byteCount = 0;
  *errorCode = EOS_OK;

  for (i = 0; i < EOS_NUM_LOADING_OPTIONS; i++) {
    tableOption = me->tableOptions[i].optionFlag;
    switch (tableOption) {
    case EOS_INSERT_DATA:
      memcpy (&(me->tableOptions[i].optionValue.ival), packedData + byteCount,
              sizeof (EOS_INTEGER));
      byteCount += sizeof (EOS_INTEGER);
      break;

    default:
      memcpy (&(me->tableOptions[i].optionValue.bval), packedData + byteCount,
              sizeof (EOS_BOOLEAN));
      byteCount += sizeof (EOS_BOOLEAN);
      break;
    }
  }
}

/************************************************************************
 * 
 * virtual method function that returns the size of char array needed to pack the options.
 * 
 * Returned Values:
 * EOS_INTEGER error code
 * EOS_INTEGER byteCount  - out number of bytes needed to store packed options.
 *
 * Input Value:
 * eos_Data *me  - pointer to an instance of type eos_Data.
 * 
 ************************************************************************/
void eos_GetPackedOptionsSizeEosData (eos_Data *me, EOS_INTEGER *byteCount,
                                      EOS_INTEGER *errorCode)
{
  EOS_INTEGER count = 0;
  *errorCode = EOS_OK;

  *byteCount = 0;
  *byteCount += sizeof (EOS_INTEGER);
  count++;                      /* EOS_INSERT_DATA */
  *byteCount += (EOS_NUM_LOADING_OPTIONS - count) * sizeof (EOS_BOOLEAN);
}

/************************************************************************
 * 
 * internal function that returns a pointer to the option value of eosData 
 * given option flag.
 * 
 * Returned Values:
 * EOS_INTEGER pointer to the option value
 *
 * Input Value:
 * eos_Data *me  - pointer to an instance of type eos_Data.
 * EOS_INTEGER optFlag  - option to get
 * 
 ************************************************************************/
eos_OptionValue *_eos_getOptionEosData (eos_Data *me, EOS_INTEGER optFlag)
{
  EOS_INTEGER optInd;
  optInd = EOS_LOADING_OPTION_FLAG_TO_INDEX (optFlag);
  if (optInd < 0 || optInd > EOS_NUM_LOADING_OPTIONS - 1)
    return NULL;
  return &(me->tableOptions[optInd].optionValue);
}

/************************************************************************
 * 
 * checks if the eosData object is compatible for sharing with multiple table handles
 * 
 * Returned Values:
 * EOS_BOOLEAN EOS_TRUE or EOS_FALSE
 *
 * Input Value:
 * eos_Data *obj         - pointer to an instance of type eos_Data.
 * 
 ************************************************************************/
EOS_BOOLEAN _eos_IsaShareableObjectEosData (void *ptr)
{
  EOS_INTEGER i, of;
  EOS_BOOLEAN oval;
  eos_Data *me = (eos_Data*) ptr;

  for (i = 0; i < EOS_NUM_LOADING_OPTIONS; i++) {

    of = me->tableOptions[i].optionFlag;

    switch (of) {
    case EOS_INVERT_AT_SETUP:
      oval = me->tableOptions[i].optionValue.bval;
      if (oval) return EOS_FALSE;
      break;
    }

  }

  /* no relevant loading option prevents object sharing */
  return EOS_TRUE;
}

/************************************************************************
 * 
 * checks if the options of the first eosData object compatible for loading to the 
 * options of the second, given option flag. Also checks if the monotonicity matches
 * for all the relevant subtables in eosData
 * 
 * Returned Values:
 * EOS_BOOLEAN EOS_TRUE or EOS_FALSE
 *
 * Input Value:
 * eos_Data *obj1, *obj2  - pointers to 2 instances of type eos_Data.
 * EOS_INTEGER monDataType- data type that needs to be monotonic in new object, if -1 assume the entire table!
 * EOS_INTEGER dataType   - data type of the obj1
 * EOS_INTEGER *optFlags  - list of new options to compare, User wants to set these options 
 *                          for object #2, so in place of existing object #2 options, compare these
 *                          new options.
 * EOS_BOOLEAN *bval      - list of relevant new boolean option values to compare
 * EOS_INTEGR nopt        - number of new options to compare
 * 
 ************************************************************************/
EOS_BOOLEAN _eos_AreOptionsCompatibleEosData (eos_Data *obj1, eos_Data *obj2,
                                              EOS_INTEGER dataType,
                                              EOS_INTEGER monDataType,
                                              EOS_INTEGER *optFlags,
                                              EOS_BOOLEAN *bval,
                                              EOS_INTEGER nopt)
{
  EOS_INTEGER i, j, cat, of, subType1, subType2, monotonicX = 0, monotonicY =
    0, monotonicX2 = 0, monotonicY2 = 0, monotonicXSet = 0, monotonicYSet =
    0, svMonotonicX = 0, svMonotonicY = 0, smooth = 0, pt_smoothSet =
    0, smoothSet = 0, smooth2, pt_smooth, pt_smooth2;
  EOS_BOOLEAN oval, compatible = EOS_FALSE;

  if (obj1->dataFileIndex != obj2->dataFileIndex)
    return EOS_FALSE;

  for (i = 0; i < EOS_NUM_LOADING_OPTIONS; i++) {

    of = obj1->tableOptions[i].optionFlag;

    /* compare either with the passed in new option value, or with existing opt value of the second object */
    oval = obj2->tableOptions[i].optionValue.bval;

    if (optFlags != NULL) {

      for (j = 0; j < nopt; j++) {
	if (!EOS_IS_LOADING_OPTION (optFlags[j]) || optFlags[j] != of)
	  continue;
	/* new option is requested */
	oval = bval[j];
	/* only consider new monotonicity, and smoothing options, not the ones already stored in obj2,
	   those will be handled by either eosData->GetMonotonicity() or eosData->GetSmoothing */
	switch (of) {
	case EOS_MONOTONIC_IN_X:
	  monotonicX = (oval) ? 1 : 0;
	  monotonicXSet = 1;
	  break;
	case EOS_MONOTONIC_IN_Y:
	  monotonicY = (oval) ? 1 : 0;
	  monotonicYSet = 1;
	  break;
	case EOS_SMOOTH:
	  smooth = (oval) ? 1 : 0;
	  smoothSet = 1;
	  break;
	case EOS_PT_SMOOTHING:
	  pt_smooth = (oval) ? 1 : 0;
	  pt_smoothSet = 1;
	  break;
	}
	break; /* I found a boolean loading option */
      }

    }

    /* if type is not alternative, compare stored and set options */
    if (monDataType < 0 && obj1->tableOptions[i].optionValue.bval != oval)
      return EOS_FALSE;
  }

  /* now check if the monotonicity matches to the new one requested */

  cat = EOS_CATEGORY (dataType);
  /* if monotonicity is requested for the entire cat 3 or 4 type */
  if (monDataType < 0 && (cat == 3 || cat == 4)) {      /* check if both subtypes have the required monotonicity options */
    subType1 = EOS_EOS_TABLE_TYPE_REF1 (dataType);
    obj2->GetMonotonicity (obj2, subType1, &monotonicX2, &monotonicY2);
    /* if monotonicX hasn't been set, take the existing one for this subtype */
    svMonotonicX = monotonicX;
    svMonotonicY = monotonicY;
    if (!monotonicXSet)
      monotonicX = monotonicX2;
    if (!monotonicXSet)
      monotonicY = monotonicY2;
    obj1->AreMonotonicRequirementsCompatible (obj1, subType1, monotonicX,
                                              monotonicY, &compatible);
    if (!compatible)
      return EOS_FALSE;
    monotonicX = svMonotonicX;
    monotonicY = svMonotonicY;

    subType2 = EOS_EOS_TABLE_TYPE_REF2 (dataType);
    obj2->GetMonotonicity (obj2, subType2, &monotonicX2, &monotonicY2);
    /* if monotonicX hasn't been set, take the existing one for this subtype */
    if (!monotonicXSet)
      monotonicX = monotonicX2;
    if (!monotonicYSet)
      monotonicY = monotonicY2;
    obj1->AreMonotonicRequirementsCompatible (obj1, subType2, monotonicX,
                                              monotonicY, &compatible);
    if (!compatible)
      return EOS_FALSE;
  }
  else {
    if (monDataType < 0)
      monDataType = dataType;
    obj2->GetMonotonicity (obj2, monDataType, &monotonicX2, &monotonicY2);
    /* if monotonicX hasn't been set, take the existing one for this subtype */
    if (!monotonicXSet)
      monotonicX = monotonicX2;
    if (!monotonicYSet)
      monotonicY = monotonicY2;
    obj1->AreMonotonicRequirementsCompatible (obj1, monDataType, monotonicX,
                                              monotonicY, &compatible);
    if (!compatible)
      return EOS_FALSE;
  }

  if (monDataType < 0)
    monDataType = dataType;
  obj2->GetSmoothing (obj2, monDataType, &smooth2, &pt_smooth2);
  /* if smoothing hasn't been set, take the existing one for this subtype */
  if (!smoothSet)
    smooth = smooth2;
  if (!pt_smoothSet)
    pt_smooth = pt_smooth2;
  obj1->AreSmoothingRequirementsCompatible (obj1, monDataType, smooth,
                                            pt_smooth, &compatible);
  if (!compatible)
    return EOS_FALSE;

  /* Also check for the subtyppes if cat 3 or 4 */

  /* haven't found the relevant loading option that doesn't match */
  return EOS_TRUE;
}

/************************************************************************
 * 
 * function that checks if the  options that affect loading of 
 * eosData object are default or not
 * 
 * Returned Values:
 * EOS_BOOLEAN EOS_TRUE or EOS_FALSE
 *
 * Input Value:
 * eos_Data *obj  - pointer to an instance of type eos_Data.
 * 
 ************************************************************************/
EOS_BOOLEAN _eos_AreOptionsDefaultEosData (eos_Data *obj1)
{
  EOS_INTEGER i, inX, inY, of, ind;
  EOS_BOOLEAN areDef = EOS_TRUE;

  for (i = 0; i < EOS_TOTAL_TABLE_OPTIONS; i++) {
    of = eos_DefaultTableOptions[i].optionFlag;
    if (!EOS_IS_LOADING_OPTION (of))
      continue;

    ind = EOS_LOADING_OPTION_FLAG_TO_INDEX (of);
    /* compare with default value */

    if (of == EOS_MONOTONIC_IN_X)
      inX = (eos_DefaultTableOptions[i].optionValue.bval) ? 1 : 0;
    else if (of == EOS_MONOTONIC_IN_Y)
      inY = (eos_DefaultTableOptions[i].optionValue.bval) ? 1 : 0;
    else if (of == EOS_INSERT_DATA &&
             obj1->tableOptions[ind].optionValue.ival != eos_DefaultTableOptions[i].optionValue.ival)
      return EOS_FALSE;
    else if (obj1->tableOptions[ind].optionValue.bval != eos_DefaultTableOptions[i].optionValue.bval)
      return EOS_FALSE;
  }

  /* check if all monotonicity options are default, -1 means all subtables */
  obj1->AreMonotonicRequirementsCompatible (obj1, -1, inX, inY, &areDef);

  /* haven't found the relevant option that doesn't match */
  return areDef;
}

/************************************************************************
 * 
 * function that checks if the specified loading option is default for
 * current object
 * 
 * Returned Values:
 * EOS_BOOLEAN EOS_TRUE or EOS_FALSE
 *
 * Input Value:
 * eos_Data *obj  - pointer to an instance of type eos_Data.
 * EOS_INTEGER of - option flag
 * EOS_INTEGER dataType - table handle's associated data type
 *
 ************************************************************************/
EOS_BOOLEAN _isOptionDefaultEosData (eos_Data *obj1, EOS_INTEGER of, EOS_INTEGER dataType)
{
  EOS_INTEGER i, ind = EOS_LOADING_OPTION_FLAG_TO_INDEX (of);

  if (!EOS_IS_LOADING_OPTION (of)) return EOS_TRUE; /* not a loading option */

  for (i = 0; i < EOS_TOTAL_TABLE_OPTIONS; i++) {
    if (of == eos_DefaultTableOptions[i].optionFlag) break;

  }

  if (i > EOS_TOTAL_TABLE_OPTIONS) return EOS_TRUE; /* not a valid option */

  /* compare with default value */

  if (of == EOS_INSERT_DATA &&
      obj1->tableOptions[ind].optionValue.ival != eos_DefaultTableOptions[i].optionValue.ival)
    return EOS_FALSE;
  else if (obj1->tableOptions[ind].optionValue.bval != eos_DefaultTableOptions[i].optionValue.bval)
    return EOS_FALSE;

  return EOS_TRUE;
}

/************************************************************************
 * 
 * function that copies options from one eos_Data object to another
 * 
 * Returned Values: none
 *
 * Input Value:
 * eos_Data *obj1  - pointer to an instance of type eos_Data to copy options to.
 * eos_Data *obj2  - pointer to an instance of type eos_Data to copy options from.
 * 
 ************************************************************************/
void _eos_CopyOptionsEosData (eos_Data *obj1, eos_Data *obj2)
{
  EOS_INTEGER i;

  for (i = 0; i < EOS_NUM_LOADING_OPTIONS; i++) {
    obj1->tableOptions[i].optionValue.bval =
      obj2->tableOptions[i].optionValue.bval;
  }
}
