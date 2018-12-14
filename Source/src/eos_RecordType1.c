/*********************************************************************
 * Class Name : eos_RecordType1
 * ---------------------------------------------------------
 * Filetype: (SOURCE)
 *
 * Copyright -- see file named COPYRIGHTNOTICE
 *
 *********************************************************************/
 
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>
#include <float.h>
#ifndef _POSIX_
#define _POSIX_
#endif
#include <sys/types.h>
#include <sys/stat.h>
 
#define _EOS_RECORDTYPE1_INTERNAL_PROTOTYPES
#include "eos_types_internal.h"
#include "eos_RecordType1.h"
 
#include "eos_RecordType2.h"
 
#include "eos_Utils.h"
 
#include "eos_UtilsRage.h"
#include "eos_Data.h"
#include "eos_Interpolation.h"
 
#undef MY_DEBUG
 
static EOS_REAL *EOS_NullPtr = NULL;
static const EOS_REAL pi = (EOS_REAL) 3.1415926535897932384626433832795;        // Pi
static const EOS_REAL ONE = (EOS_REAL) 1;
static const EOS_REAL TWO = (EOS_REAL) 2;
static const EOS_REAL THREE = (EOS_REAL) 3;
static const EOS_REAL FOUR = (EOS_REAL) 4;
static const EOS_REAL FIVE = (EOS_REAL) 5;
static const EOS_REAL SIX = (EOS_REAL) 6;
static const EOS_REAL SEVEN = (EOS_REAL) 7;
//static const EOS_REAL EIGHT = (EOS_REAL) 8;
static const EOS_REAL NINE = (EOS_REAL) 9;
static const EOS_REAL ZERO = (EOS_REAL) 0;
 
/***********************************************************************/
/*!
 * \brief RecordType1 class constructor
 *
 * \param[in,out] *me        - eos_RecordType1 : data object pointer;
 *                                               contents of object are initialized
 * \param[in]     materialID - EOS_INTEGER     : id of material to load
 * \param[in]     th         - EOS_INTEGER     : table handle
 *
 * \return none
 * 
 ***********************************************************************/
void eos_ConstructRecordType1 (eos_RecordType1 *me, EOS_INTEGER th,
			       EOS_INTEGER materialID)
{
  int i;
  me->R = NULL;
  me->NR = 0;
  me->NT = 0;
  me->avgAtomicNumber = (EOS_REAL) 0;
  me->avgAtomicWgt = (EOS_REAL) 0;
  me->refDensity = (EOS_REAL) 0;
  me->solidBulkModulus = (EOS_REAL) 0;
  me->exchangeCoefficient = (EOS_REAL) 0;
  me->T = NULL;
  me->table1 = NULL;
  me->table2 = NULL;
  me->table3 = NULL;
  me->table4 = NULL;
  me->coldCurve1 = NULL;
  me->coldCurve2 = NULL;
  me->coldCurve3 = NULL;
  me->coldCurve4 = NULL;
  me->extrapolationBounds.stored = EOS_FALSE;
  me->extrapolationBounds.nx     = 0;
  me->extrapolationBounds.ny     = 0;
  me->extrapolationBounds.x      = NULL;
  me->extrapolationBounds.xLo    = NULL;
  me->extrapolationBounds.yLo    = NULL;
  me->extrapolationBounds.xHi    = NULL;
  me->extrapolationBounds.yHi    = NULL;
  me->temporary = NULL; /* used exclusively for temporary data exchange; not packed/unpacked */
  me->Taylor_objects = NULL;
  me->TX = NULL;
  me->TY = NULL;
  me->tabulated_rhozero_exists = EOS_FALSE;
  for(i=0; i<4; i++) me->_eos_EvaluateTaylor[i] = NULL;
  me->isMonotonicX1 = -1;
  me->isMonotonicX2 = -1;
  me->isMonotonicX3 = -1;
  me->isMonotonicX4 = -1;
  me->isMonotonicY1 = -1;
  me->isMonotonicY2 = -1;
  me->isMonotonicY3 = -1;
  me->isMonotonicY4 = -1;
  me->shouldBeMonotonicX1 = 0;
  me->shouldBeMonotonicX2 = 0;
  me->shouldBeMonotonicX3 = 0;
  me->shouldBeMonotonicX4 = 0;
  me->shouldBeMonotonicY1 = 0;
  me->shouldBeMonotonicY2 = 0;
  me->shouldBeMonotonicY3 = 0;
  me->shouldBeMonotonicY4 = 0;
  me->shouldBeSmooth1 = 0;
  me->shouldBeSmooth2 = 0;
  me->shouldBeSmooth3 = 0;
  me->shouldBeSmooth4 = 0;
  me->shouldBePtSmooth1 = 0;
  me->shouldBePtSmooth2 = 0;
  me->shouldBePtSmooth3 = 0;
  me->shouldBePtSmooth4 = 0;
  me->eosData.dataFileOffset = -1;
  me->eosData.dataFileIndex = -1;
  me->eosData.dataSize = 0;
  eos_ConstructEosData ((eos_Data *) me, th, materialID);
  me->eosData.Load = eos_LoadRecordType1;
  me->eosData.Create = eos_CreateRecordType1;
  me->eosData.Destroy = eos_DestroyRecordType1;
  me->eosData.SetFileIndexes = eos_SetFileIndexesRecordType1;
  me->eosData.Print = eos_PrintRecordType1;
  me->eosData.GetPackedTable = eos_GetPackedTableRecordType1;
  me->eosData.SetPackedTable = eos_SetPackedTableRecordType1;
  me->eosData.GetPackedTableSize = eos_GetPackedTableSizeRecordType1;
  me->eosData.IsMonotonic = eos_IsMonotonicRecordType1;
  me->eosData.MakeMonotonic = eos_MakeMonotonicRecordType1;
  me->eosData.MakeSmooth = eos_MakeSmoothRecordType1;
  me->eosData.GetTableInfo = eos_GetTableInfoRecordType1;
  me->eosData.GetTableMetaData = eos_GetTableMetaDataRecordType1;
  me->eosData.GetLoadedBulkData = eos_GetLoadedBulkDataRecordType1;
  me->eosData.SetMonotonicity = eos_SetMonotonicityRecordType1;
  me->eosData.GetMonotonicity = eos_GetMonotonicityRecordType1;
  me->eosData.GetSmoothing = eos_GetSmoothingRecordType1;
  me->eosData.AreMonotonicRequirementsCompatible =
    eos_AreMonotonicRequirementsCompatibleRecordType1;
  me->eosData.SetSmoothing = eos_SetSmoothingRecordType1;
  me->eosData.AreSmoothingRequirementsCompatible =
    eos_AreSmoothingRequirementsCompatibleRecordType1;
  me->eosData.Interpolate = eos_InterpolateRecordType1;
  /*   me->eosData.CheckExtrap = eos_CheckExtrapRecordType1; */
  me->eosData.CheckExtrap = eos_CheckExtrapRecordType1_using_extrapolationBounds;
  me->eosData.InvertAtSetup = eos_InvertAtSetupRecordType1;
  me->eosData.SetExtrapolationBounds = eos_SetExtrapolationBoundsRecordType1;
  me->eosData.varOrder = X_Y_F;
  me->eosData.tmpVarOrder = -1;
  me->rt2_handle = -1;
  me->found_401 = EOS_FALSE;
  me->isInvertedAtSetup = EOS_FALSE;
}
 
/***********************************************************************/
/*! 
 * \brief RecordType1 class destructor
 * 
 * \param[in,out] *me        - eos_RecordType1 : data object pointer;
 *                             contents of *me are destroyed
 * 
 * \return none
 *
 ***********************************************************************/
void eos_DestroyRecordType1 (void* ptr)
{
  eos_RecordType1 *me = (eos_RecordType1*) ptr;
 
  int i, j;
 
  if (!me->table1 && !me->table2 && !me->table3 && !me->table4) {
    if (!me->eosData.destructing)       /* to prevent circular calls to destructor */
      eos_DestroyEosData (&(me->eosData));
    return;
  }
 
  if (me->Taylor_objects) {
    for (i = 0; i < 4; i++) {
      for (j=0; j<(me->M * me->N); j++) {
	eos_Taylor *p = NULL;
	p = (me->Taylor_objects[i])[j];
	if (p) p->Destroy(p);
      }
    }
    EOS_FREE(me->Taylor_objects);
    EOS_FREE(me->TX);
    EOS_FREE(me->TY);
  }
 
  _eos_DestroyExtrapolationBounds(me);
 
  eos_SetSizeRecordType1 (me, 0, 0, 0);
 
  if (!me->eosData.destructing) /* avoid circular destructor calls */
    eos_DestroyEosData (&(me->eosData));
}
 
/***********************************************************************/
/*! 
 * \brief This function allocates data of RecordType1 for loading
 * 
 * \param[in,out] *ptr       - void : data object pointer;
 *                             internally recast to eos_RecordType1*
 *                             contents are allocated
 * \param[in]     th         - EOS_INTEGER : table handle
 * 
 * \return none
 *
 ***********************************************************************/
void eos_CreateRecordType1 (void *ptr, EOS_INTEGER th)
{
  eos_RecordType1 *me;
 
  me = (eos_RecordType1 *) ptr;
 
  /* set the sesame file indexes and offsets for RecordType1 */
  eos_SetFileIndexesRecordType1 (me, th);
 
  if ((eos_GetStandardErrorCodeFromCustomErrorCode(gEosDataMap.errorCodes[th]) == EOS_DATA_TYPE_NOT_FOUND) &&
      !me->eosData.forceCreate) {
    // DAP - no error handling is done here so that eos_LoadRecordType1 can decide what to do in the case
    //       that a Sesame table is missing.
    ((eos_ErrorHandler *) me)->HandleError (me, th, EOS_OK);
    return;
  }
 
  if (gEosDataMap.errorCodes[th]) /* return previous error to parent */
    return;
 
  /* check if there is enough data */
  if (me->eosData.dataSize < 2 + me->NR + me->NT + me->NR * me->NT) {   /* at least 1 subtable! */
    ((eos_ErrorHandler *) me)->HandleError (me, th, EOS_READ_DATA_FAILED);
    return;
  }
 
  // allocate enough memory
  eos_SetSizeRecordType1 (me, me->NR, me->NT, me->eosData.tableNum);
}
 
/***********************************************************************/
/*! 
 * \brief This function sets the sesame file indexes and offsets for RecordType1
 * 
 * \param[in,out] *ptr       - void : data object pointer;
 *                             internally recast to eos_RecordType1*
 *                             contents are allocated
 * \param[in]     th         - EOS_INTEGER : table handle
 * 
 * \return none
 *
 ***********************************************************************/
void eos_SetFileIndexesRecordType1 (void *ptr, EOS_INTEGER th)
{
  eos_RecordType1 *me;
  EOS_INTEGER matid, count;
  EOS_INTEGER nreals_to_read;
  EOS_INTEGER ierr = EOS_OK;
  EOS_REAL *read_data;
  EOS_CHAR *errMsg = NULL;
 
  me = (eos_RecordType1 *) ptr;
  matid = me->eosData.materialID;
  read_data = NULL;
 
  {
    /* get the file offset for reading data and get back 6 reals: NR, NT, R[0], date1, date2 and version */
    ses_material_id mid = (ses_material_id)me->eosData.materialID;
    ses_table_id    tid = (ses_table_id)me->eosData.tableNum;
    nreals_to_read = 6;
    ierr = eos_SesSeekToDataTable (matid, me->eosData.tableNum, &read_data, nreals_to_read,
				   &(me->eosData.dataFileIndex), &mid, &tid,
				   &(me->eosData.dataSize), me->eosData.userDefinedDataFile, &errMsg);
    if (errMsg) ierr = eos_SetCustomErrorMsg(th, ierr, "%s", errMsg);
    EOS_FREE(errMsg);
  }
  if ((eos_GetStandardErrorCodeFromCustomErrorCode(ierr) == EOS_DATA_TYPE_NOT_FOUND) && !me->eosData.forceCreate) {
    // DAP - no error handling is done here so that eos_LoadRecordType1 can decide what to do in the case
    //       that a Sesame table is missing.
    /* deallocate read_data[], which was allocated by eos_SeekToDataTable() above */
    ((eos_ErrorHandler *) me)->HandleError (me, th, ierr);
    EOS_FREE (read_data);
    return;
  }
  else if (ierr) {
    /* deallocate read_data[], which was allocated by eos_SeekToDataTable() above */
    EOS_FREE (read_data);
    ((eos_ErrorHandler *) me)->HandleError (me, th, ierr);
    return;
  }
 
  // determine how many subtables are available;
  // return error code if requested is not available
  count = 0;
  me->NR = (EOS_INTEGER) read_data[count++];
  me->NT = (EOS_INTEGER) read_data[count++];
  if (nreals_to_read > 5) {
    /* read minimum density value and set flag appropriately */
    me->tabulated_rhozero_exists = (read_data[count++] > 0.0) ? EOS_FALSE : EOS_TRUE;
  }
  /* read creation date */
  me->eosData.creationDate = (EOS_INTEGER) read_data[count++];
  /* read modification date */
  me->eosData.modificationDate = (EOS_INTEGER) read_data[count++];
  if (me->eosData.modificationDate == 0)
    me->eosData.modificationDate = me->eosData.creationDate;
  /* read version number */
  me->eosData.latestVersion = (EOS_INTEGER) read_data[count++];
 
  /* deallocate read_data[], which was allocated by eos_SeekToDataTable() above */
#ifdef MY_DEBUG
  printf("In SetFileIndexes Record Type 1 before EOS_FREE read_data\n");
#endif
  EOS_FREE (read_data);
#ifdef MY_DEBUG
  printf("In SetFileIndexes Record Type 1 after EOS_FREE read_data returning\n");
#endif
}
 
/***********************************************************************/
/*! 
 * \brief This an internal function that deallocates the unused data arrays.
 * 
 * \param[in,out] *me        - eos_RecordType1 : data object pointer;
 *                             contents are populated with data
 * 
 * \return none
 *
 ***********************************************************************/
void _eos_FreeUnusedArraysRecordType1 (eos_RecordType1 *me)
{
  /* free unused memory if subtables not loaded */
  if (me->eosData.numSubtablesLoaded < 4 && me->table4) {
    EOS_FREE (me->table4);
    EOS_FREE (me->coldCurve4);
  }
  if (me->eosData.numSubtablesLoaded < 3 && me->table3) {
    EOS_FREE (me->table3);
    EOS_FREE (me->coldCurve3);
  }
  if (me->eosData.numSubtablesLoaded < 2 && me->table2) {
    EOS_FREE (me->table2);
    EOS_FREE (me->coldCurve2);
  }
  if (me->eosData.numSubtablesLoaded < 1 && me->table1) {
    EOS_FREE (me->table1);
    EOS_FREE (me->coldCurve1);
  }
}
 
/***********************************************************************/
/*! 
 * \brief This function loads data of RecordType1 and stores the data in the class's
 *  data structures.
 * 
 * \param[in,out] *ptr       - void : data object pointer;
 *                             internally recast to eos_RecordType1*
 *                             contents are populated with data
 * \param[in]     th         - table handle
 * 
 * \return none
 *
 ***********************************************************************/
void eos_LoadRecordType1 (void *ptr, EOS_INTEGER th)
{
  eos_RecordType1 *me;
  EOS_INTEGER tableNum, i, j, count, index1, index2,
    index3, index4;
  EOS_INTEGER ierr = EOS_OK;
  EOS_REAL *read_data, *dens, *temp, *enion, *anion, *s_ion;
  eos_OptionValue *optVal = NULL;
  eos_OptionValue *optValForced = NULL;
  EOS_INTEGER splitOptFlag;
  EOS_BOOLEAN freeEnergyIsMissing, use_taylor_fit = EOS_FALSE, use_maxwell_table = EOS_FALSE;
  EOS_CHAR *errMsg = NULL;
  //#define DEBUG_EOS_LOADRECORDTYPE1
#ifdef DEBUG_EOS_LOADRECORDTYPE1
  EOS_INTEGER k, l;
#endif
 
  me = (eos_RecordType1 *) ptr;
 
  optVal = _eos_getOptionEosData (&(me->eosData), EOS_USE_MAXWELL_TABLE);
  use_maxwell_table = optVal->bval;
  if (use_maxwell_table && me->eosData.tableNum == 301) {
    me->eosData.tableNum = 311; /* modify table reference if appropriate */
  }
 
  optVal = _eos_getOptionEosData (&(me->eosData), EOS_USE_TAYLOR_FIT);
  use_taylor_fit = optVal->bval;
  if (use_taylor_fit) {
    me->eosData.tableNum += 400; /* modify table reference */
  }
 
  tableNum = me->eosData.tableNum;
  read_data = NULL;
 
  /* check handle's error code; this was moved from Create upon the introduction
   * of the eos_SetDataFileName function */
  if (eos_GetStandardErrorCodeFromCustomErrorCode(gEosDataMap.errorCodes[th]) == EOS_MATERIAL_NOT_FOUND) {
    ierr = gEosDataMap.errorCodes[th];
    ((eos_ErrorHandler *) me)->HandleError (me, th, ierr);
    return;
  }
 
  // load bulk data from 201 table
  ierr =
    eos_SesGetBulkData (me->eosData.materialID, me->eosData.userDefinedDataFile, me->eosData.dataFileIndex,
			&(me->avgAtomicNumber), &(me->avgAtomicWgt), &(me->refDensity),
			&(me->solidBulkModulus), &(me->exchangeCoefficient), &errMsg);
  if (errMsg) ierr = eos_SetCustomErrorMsg(th, ierr, "%s", errMsg);
  EOS_FREE(errMsg);
  if (ierr) {
    ((eos_ErrorHandler *) me)->HandleError (me, th, ierr);
    ierr = eos_SetCustomErrorMsg(th, ierr,
				 "eos_RecordType1::eos_GetBulkData ERROR, bulk data not available for table handle, %i", th);
    return;
  }
 
  /* Load Taylor polynomial fit data if requested; then return */
  if (use_taylor_fit) {
    /* is the datatpe a function with one independent variable? */
    EOS_INTEGER dataType = eos_GetDataTypeFromTableHandle (th, &ierr);
    EOS_BOOLEAN isOneDimDatatype = EOS_IS_ONE_DIM_TYPE (dataType);
 
    me->Taylor_objects = (eos_Taylor***) malloc(4 * sizeof(eos_Taylor**));
 
    /* allocate and initialize pointers in Taylor_objects array */
    for (i = 0; i < 4; i++) {
      me->Taylor_objects[i] = (eos_Taylor**) malloc(sizeof(eos_Taylor*));
      assert(me->Taylor_objects[i] != NULL);
    }
 
    /* create and populate Taylor objects for each P, U, A and S as required for available data */
    if (isOneDimDatatype) {
      for (i = 0; i < 4; i++) {
	ierr = eos_Load1DTaylor(&(me->eosData), (ses_material_id)me->eosData.materialID, (ses_table_id)tableNum, i+1, &(me->TX), &(me->M), &(me->Taylor_objects[i]));
	if (eos_GetStandardErrorCodeFromCustomErrorCode(ierr) == EOS_NO_DATA_TABLE) {
	  EOS_FREE(me->Taylor_objects[i]); /* free memory and nullify pointer */
	  continue; /* skip remaining */
	}
	if (ierr) break; /* exit loop and handle error */
	me->eosData.numSubtablesLoaded++;
      }
    }
    else {
      for (i = 0; i < 4; i++) {
	ierr = eos_Load2DTaylor(&(me->eosData), (ses_material_id)me->eosData.materialID, (ses_table_id)tableNum, i+1, &(me->TX), &(me->TY), &(me->M), &(me->N), &(me->Taylor_objects[i]));
	if (eos_GetStandardErrorCodeFromCustomErrorCode(ierr) == EOS_NO_DATA_TABLE) {
	  EOS_FREE(me->Taylor_objects[i]); /* free memory and nullify pointer */
	  continue; /* skip remaining */
	}
	if (ierr) break; /* exit loop and handle error */
	me->eosData.numSubtablesLoaded++;
      }
    }
 
    if (me->eosData.numSubtablesLoaded > 0) {
      ierr = EOS_OK;
      me->eosData.isLoaded = 1;
    }
 
    if (ierr) {
      ((eos_ErrorHandler *) me)->HandleError (me, th, ierr);
      ierr = eos_SetCustomErrorMsg(th, ierr,
				   "eos_LoadRecordType1 ERROR, no Taylor polynomial fit data available for table handle, %i", th);
      return;
    }
 
    /* Deallocate table arrays, since they will be unused */
    eos_SetSizeRecordType1 (me, 0, 0, tableNum);
 
    /* dataType-dependent evaluation function assignments */
    for (i = 0; i < 4; i++) {
      if (me->Taylor_objects[i]) me->_eos_EvaluateTaylor[i] = _eos_EvaluateTaylorFor_dataType_DT;
    }
    i = 0; if (!me->Taylor_objects[i] &&  me->Taylor_objects[2])
	     me->_eos_EvaluateTaylor[i] = _eos_EvaluateTransformedTaylorFor_Pt_DT;
    i = 1; if (!me->Taylor_objects[i]) me->_eos_EvaluateTaylor[i] = _eos_EvaluateTransformedTaylorFor_Ut_DT;
    i = 3; if (!me->Taylor_objects[i]) me->_eos_EvaluateTaylor[i] = _eos_EvaluateTransformedTaylorFor_St_DT;
 
    /* all done */
    return;
  }
 
  /* Load the read_data array with all of the available data. If necessary,
     use the selected analytical model for ions to create 303 or 304 Sesame table data */
  splitOptFlag = -1;
  if (tableNum == 303 || tableNum == 304 || tableNum == 305)    /* model only valid to create 303, 304 or 305 data */
    eos_GetAnalyticalEOSFlags (me, &splitOptFlag, &optValForced);
 
  if (splitOptFlag != -1 && optValForced && optValForced->bval) {
    /* Forced-usage of the selected analytical model to create table data */
 
    // initialize error code
    if (eos_GetStandardErrorCodeFromCustomErrorCode(gEosDataMap.errorCodes[th]) == EOS_DATA_TYPE_NOT_FOUND)
      gEosDataMap.errorCodes[th] = EOS_OK;
 
    ierr = eos_AnalyticalEOS (me, splitOptFlag, &read_data, &errMsg);
    if (errMsg) ierr = eos_SetCustomErrorMsg(th, ierr, "%s", errMsg);
    EOS_FREE(errMsg);
    if (ierr) {
      ((eos_ErrorHandler *) me)->HandleError (me, th, ierr);
      return;
    }
  }
 
  if (!(optValForced && optValForced->bval)) {
    /* just load data from Sesame file, if available, or do unforced splitting if requested */
 
    if (me->eosData.dataSize < 2) {     /* eos_CreateRecordType1 failed to load data */
 
      if (splitOptFlag != -1 &&
	  (tableNum == 303 || tableNum == 304)) {
	/* do unforced splitting if requested */
	ierr = eos_AnalyticalEOS (me, splitOptFlag, &read_data, &errMsg);
	if (errMsg) ierr = eos_SetCustomErrorMsg(th, ierr, "%s", errMsg);
	EOS_FREE(errMsg);
	if (ierr) {
	  ((eos_ErrorHandler *) me)->HandleError (me, th, ierr);
	  return;
	}
      }
      else {                    /* tableNum doesn't exist in Sesame for current matID and no splitting requested */
	ierr = EOS_DATA_TYPE_NOT_FOUND;
      }
 
    }
    else {
 
      // load available data tables from Sesame file
      ierr = eos_SesLoadSesameFiles (me->eosData.materialID, me->eosData.tableNum, me->eosData.dataFileIndex, &read_data, me->eosData.dataSize - 2); /* DATA SIZE INCLUDES DATA ALREADY READ */
      if (ierr) {
	((eos_ErrorHandler *) me)->HandleError (me, th, ierr);
	return;
      }
 
      if ((tableNum >= 301 && tableNum <= 306) || (tableNum >= 311 && tableNum <= 316)) {
	/* load entropy subtable if possible */
 
	EOS_REAL *temp_mod = NULL, *enion_mod = NULL, *anion_mod = NULL, *s_ion_mod = NULL;
	EOS_BOOLEAN create_T0_data = EOS_FALSE;
 
	freeEnergyIsMissing = EOS_FALSE;
	if (me->eosData.dataSize <= (2 + me->NR + me->NT + 2 * me->NR * me->NT)) {
	  // Free energy table not in Sesame file.
	  freeEnergyIsMissing = EOS_TRUE;
	}
 
	optVal = _eos_getOptionEosData (&(me->eosData), EOS_CALC_FREE_ENERGY);
	if (optVal->bval) {
	  EOS_CHAR *eos_appendedSourceStr = " (calculated using A=U-TS)";
	  EOS_CHAR *sesame_fname = NULL;
	  EOS_INTEGER eos_appendedSourceStrL, sesame_fnameL;
	  sesame_fname = _eos_GetSesameFileName (&(me->eosData));
	  sesame_fnameL = strlen(sesame_fname);
	  eos_appendedSourceStrL = strlen(eos_appendedSourceStr);
	  // Free energy is to be calculated.
	  freeEnergyIsMissing = EOS_TRUE;
	  /* store the description of this alternative data source */
	  EOS_FREE(me->eosData.altDataSource);
	  me->eosData.altDataSource = (EOS_CHAR*) malloc((eos_appendedSourceStrL + sesame_fnameL + 1) *sizeof(EOS_CHAR));
	  me->eosData.altDataSource = strcpy(me->eosData.altDataSource, sesame_fname);
	  me->eosData.altDataSource = strcat(me->eosData.altDataSource, eos_appendedSourceStr);
	}
 
	/* are the cold curve arrays allocated for 301 and 303 data? */
	if ((tableNum == 301 || tableNum == 303) && tableNum != 306 &&
	    me->coldCurve1 && me->coldCurve2 && me->coldCurve3
	    && me->coldCurve4)
	  me->eosData.coldCurveIsLoaded = EOS_TRUE;
 
	/* set pointer values for independent variable data */
	dens = &(read_data[0]);
	temp = &(read_data[me->NR]);
 
	optVal = _eos_getOptionEosData (&(me->eosData), EOS_CREATE_TZERO);
	if (freeEnergyIsMissing && temp[0] > ZERO && optVal->bval) {
	  /* cold curve data and Free energy are unavailable, so linearly extrapolate U(rho,T=0)
	   * to allow entropy and free energy tables to be created
	   */
	  create_T0_data = EOS_TRUE;
	  temp_mod = (EOS_REAL*) malloc((me->NT+1) * sizeof(EOS_REAL)); // allocate temporary T array to include T=0
 
	  /* set temp_mod values */
	  temp_mod[0] = 0.0;
	  for (j = 0; j < me->NT; j++)
	    temp_mod[j+1] = temp[j];
 
	  temp = temp_mod; // reset pointer to temporary array
	}
 
	if ((freeEnergyIsMissing && temp[0] <= ZERO) || create_T0_data) {
	  // Free energy table is not in Sesame file
	  // cold curve data is available so free energy and entropy tables can be constructed

	  // Store the total size of the data to be stored for all subtables:
	  // NR, NT, R[], T[], table1[], table2[], table3[], and table4[]
	  me->eosData.dataSize = 2 + me->NR + me->NT + MAX_TABLES_RECORDTYPE1 * me->NR * me->NT;
 
	  // reallocate read_data to store three subtables: enion, anion, s_ion
	  read_data =
	    realloc (read_data, (me->eosData.dataSize - 2) * sizeof (EOS_REAL));
 
	  /* set pointer values for data tables to be calculated */
	  enion = &(read_data[me->NR + me->NT + me->NR * me->NT]);
	  anion = &(read_data[me->NR + me->NT + 2 * me->NR * me->NT]);
	  s_ion = &(read_data[me->NR + me->NT + 3 * me->NR * me->NT]);
 
	  if (create_T0_data) {
	    enion_mod = (EOS_REAL*) malloc(me->NR * (me->NT+1) * sizeof(EOS_REAL));
	    anion_mod = (EOS_REAL*) malloc(me->NR * (me->NT+1) * sizeof(EOS_REAL));
	    s_ion_mod = (EOS_REAL*) malloc(me->NR * (me->NT+1) * sizeof(EOS_REAL));
 
	    enion = &(read_data[me->NR + me->NT + me->NR * me->NT]);
 
	    /* copy data to temporary array */
	    for (i = 0; i < me->NR; i++)
	      for (j = 0; j < me->NT; j++)
		enion_mod[me->NR * (j+1) + i] = enion[me->NR * j + i];
 
	    for (i = 0; i < me->NR; i++) {
	      /* linearly extrapolate enion at [j=0,i] */
	      enion_mod[i] = enion_mod[me->NR + i] - enion_mod[me->NR * 2 + i];
	      enion_mod[i] /= temp[1] - temp[2];
	      enion_mod[i] *= temp[0] - temp[2];
	      enion_mod[i] += enion_mod[me->NR + i];
	    }
	  }
	  else {
	    /* reset pointer values for independent variable data */
	    dens = &(read_data[0]);
	    temp = &(read_data[me->NR]);
	  }
 
	  /* calculate entropy */
 
	  if (create_T0_data)
	    ierr = eos_Entropy (me, me->NR, me->NT+1, enion_mod, EOS_NullPtr, temp, dens, s_ion_mod);
	  else
	    ierr = eos_Entropy (me, me->NR, me->NT, enion, EOS_NullPtr, temp, dens, s_ion);
	  if (eos_GetStandardErrorCodeFromCustomErrorCode(ierr) != EOS_OK) {
	    /* ignore the error, just set the dataSize and numTables loaded */
	    me->eosData.dataSize = 2 + me->NR + me->NT + 2 * me->NR * me->NT;     /* only TWO subtables */
	    me->eosData.numSubtablesLoaded = 2;
	    ierr = EOS_OK;
	  }
	  else {
	    if (create_T0_data) {
	      /* copy entropy data */
	      for (i = 0; i < me->NR; i++)
		for (j = 0; j < me->NT; j++)
		  s_ion[me->NR * j + i] = s_ion_mod[me->NR * (j+1) + i];
	      temp = &(read_data[me->NR]);
	    }
	    /* calculate free energy */
	    for (i = 0; i < me->NR; i++)
	      for (j = 0; j < me->NT; j++)
		anion[i + j * me->NR] =
		  enion[i + j * me->NR] - temp[j] * s_ion[i + j * me->NR];
	  }
	}
	else if (freeEnergyIsMissing && temp[0] > ZERO) {
	  // cold curve data and Free energy are unavailable
	  me->eosData.coldCurveIsLoaded = EOS_FALSE;
	  /* set the dataSize and numTables loaded */
	  me->eosData.dataSize = 2 + me->NR + me->NT + 2 * me->NR * me->NT;     /* only TWO subtables */
	  me->eosData.numSubtablesLoaded = 2;
	}
	else {
	  // Free energy table is in Sesame file
	  // cold curve data is available so free energy and entropy tables can be constructed

	  // Store the total size of the data to be stored for all subtables:
	  // NR, NT, R[], T[], table1[], table2[], table3[], and table4[]
	  me->eosData.dataSize = 2 + me->NR + me->NT + MAX_TABLES_RECORDTYPE1 * me->NR * me->NT;
 
	  // reallocate read_data to store three subtables: enion, anion, s_ion
	  read_data =
	    realloc (read_data, (me->eosData.dataSize - 2) * sizeof (EOS_REAL));
 
	  /* reset pointer values for independent variable data */
	  dens = &(read_data[0]);
	  temp = &(read_data[me->NR]);
 
	  /* set pointer values for data tables to be calculated */
	  enion = &(read_data[me->NR + me->NT + me->NR * me->NT]);
	  anion = &(read_data[me->NR + me->NT + 2 * me->NR * me->NT]);
	  s_ion = &(read_data[me->NR + me->NT + 3 * me->NR * me->NT]);
 
	  /* calculate entropy using free energy data */
	  ierr = eos_Entropy (me, me->NR, me->NT, enion, anion, temp, dens, s_ion);
	  if (eos_GetStandardErrorCodeFromCustomErrorCode(ierr) != EOS_OK) {
	    /* ignore the error, just set the dataSize and numTables loaded */
	    me->eosData.dataSize = 2 + me->NR + me->NT + 3 * me->NR * me->NT;     /* only THREE subtables */
	    me->eosData.numSubtablesLoaded = 3;
	    ierr = EOS_OK;
	  }
	}
 
	/* reset pointer values for independent variable data */
	dens = &(read_data[0]);
	temp = &(read_data[me->NR]);
 
	EOS_FREE (temp_mod); /* deallocate local memory */
	EOS_FREE (enion_mod);
	EOS_FREE (anion_mod);
	EOS_FREE (s_ion_mod);
 
      } /* end (tableNum == 301 || tableNum == 303 || tableNum == 304 || tableNum == 305 ||
	   tableNum == 306) */
 
        /* All other data (i.e., 411 and 412 sesame tables) require no additional modifications or calculations,
         * and it is already loaded into the read_data[] array by eos_LoadSesameFiles() above.
         */
 
        // store the number of subTables stored in memory
      me->eosData.numSubtablesLoaded = (EOS_INTEGER)
	MIN(MAX_TABLES_RECORDTYPE1,
	    ((me->eosData.dataSize - (2 + me->NR + me->NT)) / (me->NR * me->NT)));
    }
  }                             /* end !(optValForced && optValForced->bval) */
 
  if (eos_GetStandardErrorCodeFromCustomErrorCode(ierr) != EOS_OK) {
    ((eos_ErrorHandler *) me)->HandleError (me, th, ierr);
    return;
  }
 
  if (eos_GetStandardErrorCodeFromCustomErrorCode(ierr) != EOS_OK) {
    ((eos_ErrorHandler *) me)->HandleError (me, th, ierr);
    return;
  }
 
  count = 0;
 
  for (i = 0; i < me->NR; i++)
    me->R[i] = read_data[count++];      /* 0 to NR-1 */
 
  for (i = 0; i < me->NT; i++)
    me->T[i] = read_data[count++];      /* NR  to  NR+NT-1 */
 
  for (j = 0; j < me->NT; j++) {
    for (i = 0; i < me->NR; i++) {
      /* count is already NR+NT */
      index1 = count + me->NR * j + i;
      index2 = count + me->NR * me->NT + me->NR * j + i;
      index3 = count + 2 * me->NR * me->NT + me->NR * j + i;
      index4 = count + 3 * me->NR * me->NT + me->NR * j + i;
      me->table1[j][i] =
	(index1 < me->eosData.dataSize - 2) ? read_data[index1] : ZERO;
      me->table2[j][i] =
	(index2 < me->eosData.dataSize - 2) ? read_data[index2] : ZERO;
      me->table3[j][i] =
	(index3 < me->eosData.dataSize - 2) ? read_data[index3] : ZERO;
      me->table4[j][i] =
	(index4 < me->eosData.dataSize - 2) ? read_data[index4] : ZERO;
    }
  }
 
  /* free unused memory if subtables not loaded */
  _eos_FreeUnusedArraysRecordType1(me);
 
  //#define ENABLE_COLD_CURVE_REMOVAL
#ifdef ENABLE_COLD_CURVE_REMOVAL
 
  /* if EOS_PT_SMOOTHING option is set, then do not subtract cold curves */
  optVal = _eos_getOptionEosData (&(me->eosData), EOS_PT_SMOOTHING);
 
  if (optVal->bval) {
#endif /* ENABLE_COLD_CURVE_REMOVAL */
 
    EOS_FREE (me->coldCurve1);
    EOS_FREE (me->coldCurve2);
    EOS_FREE (me->coldCurve3);
    EOS_FREE (me->coldCurve4);
    me->eosData.coldCurveIsLoaded = EOS_FALSE;
 
#ifdef ENABLE_COLD_CURVE_REMOVAL
  }
#endif /* ENABLE_COLD_CURVE_REMOVAL */
 
#ifdef ENABLE_COLD_CURVE_REMOVAL
  /* if table is 301 or 303, and coldcurve arrays are allocated, subtract cold curve for all tables */
  for (j = 0; j < me->NT; j++) {
    for (i = 0; i < me->NR; i++) {
      /* first subtract cold curve */
      if ((tableNum == 301 || tableNum == 303) && tableNum != 306
	  && me->coldCurve1 && me->coldCurve2 && me->coldCurve3
	  && me->coldCurve4) {
	if (j == 0) {
	  me->coldCurve1[i] =
	    (me->table1
	     && me->eosData.coldCurveIsLoaded) ? me->table1[0][i] : ZERO;
	  me->coldCurve2[i] = (me->table2
			       && me->eosData.coldCurveIsLoaded) ? me->
	    table2[0][i] : ZERO;
	  me->coldCurve3[i] = (me->table3
			       && me->eosData.coldCurveIsLoaded) ? me->
	    table3[0][i] : ZERO;
	  me->coldCurve4[i] = (me->table4
			       && me->eosData.coldCurveIsLoaded) ? me->
	    table4[0][i] : ZERO;
	}
 
	if (me->table1 && me->coldCurve1)
	  me->table1[j][i] = me->table1[j][i] - me->coldCurve1[i];
	if (me->table2 && me->coldCurve2)
	  me->table2[j][i] = me->table2[j][i] - me->coldCurve2[i];
	if (me->table3 && me->coldCurve3)
	  me->table3[j][i] = me->table3[j][i] - me->coldCurve3[i];
	if (me->table4 && me->coldCurve4)
	  me->table4[j][i] = me->table4[j][i] - me->coldCurve4[i];
      }
    }
  }
#endif /* ENABLE_COLD_CURVE_REMOVAL */
 
  EOS_FREE (read_data);
  me->eosData.isLoaded = 1;
 
  /* if needed, expand grid */
  eos_ExpandGridRecordType1 (me, &ierr);
  if (eos_GetStandardErrorCodeFromCustomErrorCode(ierr) != EOS_OK) {
    ((eos_ErrorHandler *) me)->HandleError (me, th, ierr);
  }
}
 
/***********************************************************************/
/*!
 * \brief This helper function returns a pointer to a new eos_RecordType1 object,
 *        which contains a copy of the data stored in me.
 *
 * \param[in,out] *me        - eos_RecordType1 : data object pointer;
 *                             contents are populated with data
 * \param[in]     dataType   - EOS_INTEGER : data type
 *
 * \return EOS_INTEGER : table handle associated with new_me (th < 0 if error occurs)
 *
 ***********************************************************************/
EOS_INTEGER _eos_createObjectDataCopyRecordType1 (eos_RecordType1 *me, EOS_INTEGER dataType)
{
  EOS_INTEGER th = -1; /* default is invalid handle */
  EOS_INTEGER err = EOS_OK;
  EOS_INTEGER i, j, k;
  EOS_REAL *R, *T, **F, *coldCurve;
  EOS_REAL *new_R, *new_T, **new_F, *new_coldCurve;
  eos_RecordType1 *new_me = NULL;
 
  /*
    void eos_CreateTablesEosDataMap (eos_DataMap *me, EOS_INTEGER nTables,
    EOS_INTEGER tableType[], EOS_INTEGER matID[],
    EOS_INTEGER tableHandles[],
    EOS_INTEGER doCreate, EOS_INTEGER isPublic,
    EOS_INTEGER updateOnly, EOS_INTEGER userDefinedDataFileIndex,
    EOS_INTEGER *errorCode)
  */
 
  /* first construct an empty object w/o creating it (in case the table can't be created, we still want an empty object */
  eos_CreateTablesEosDataMap (&gEosDataMap, 1, &dataType, &me->eosData.materialID,
			      &th, EOS_FALSE, 0, EOS_FALSE, -1, &err);
 
  if (eos_GetStandardErrorCodeFromCustomErrorCode(err) == EOS_OK && th >= 0 && gEosDataMap.tableHandlesMap[th]) {
 
    /* all is well so far */
 
    new_me = (eos_RecordType1 *) gEosDataMap.dataObjects[gEosDataMap.tableHandlesMap[th]];
 
    /* allocate memory in new_me */
    eos_SetSizeRecordType1 (new_me, me->NR, me->NT, me->eosData.tableNum);
 
    new_me->eosData.numSubtablesLoaded = me->eosData.numSubtablesLoaded;
 
    /* copy current data into new_me for all tables */
    _eos_GetDataRecordType1 (me, &R, &T, &F, &coldCurve, 1);
    _eos_GetDataRecordType1 (new_me, &new_R, &new_T, &new_F, &new_coldCurve, 1);
    for (j = 0; j < me->NT; j++)
      new_T[j] = T[j];
    for (k = 0; k < me->NR; k++)
      new_R[k] = R[k];
 
    for (i = 0; i < MAX_TABLES_RECORDTYPE1; i++) {
 
      if (me->eosData.numSubtablesLoaded < i + 1)
	break;
 
      _eos_GetDataRecordType1 (me, &R, &T, &F, &coldCurve, i + 1);
      _eos_GetDataRecordType1 (new_me, &new_R, &new_T, &new_F, &new_coldCurve, i + 1);
 
      /* get data of i-th subtable */
 
      for (j = 0; j < me->NT; j++) {
	for (k = 0; k < me->NR; k++)
	  new_F[j][k] = F[j][k];
      }
 
      if ((me->eosData.tableNum == 301 || me->eosData.tableNum == 303) && coldCurve) {
	for (k = 0; k < me->NR; k++)
	  new_coldCurve[k] = coldCurve[k];
      }
 
    }
 
  }
 
  return th;
}
 
/***********************************************************************/
/*!
 * \brief This helper function resizes an eos_RecordType1 object, me,
 *        while retaining the data stored in me.
 *
 * \param[in,out] *me        - eos_RecordType1 : data object pointer;
 *                             contents are populated with data
 * \param[in]     NR  - EOS_INTEGER : size of R array
 * \param[in]     NT  - EOS_INTEGER : size of T array
 * \param[out]   *err - EOS_INTEGER : error code
 * 
 * \return none
 *
 ***********************************************************************/
void _eos_resizeRecordType1 (eos_RecordType1 *me, EOS_INTEGER NR, EOS_INTEGER NT, EOS_INTEGER *err)
{
  EOS_INTEGER oldNT, oldNR, i, j, k;
  EOS_REAL *oldR, *oldT, **oldF[MAX_TABLES_RECORDTYPE1], *oldColdCurve[MAX_TABLES_RECORDTYPE1];
  EOS_REAL *R, *T, **F, *coldCurve;
  EOS_REAL *ptr1;
  *err = EOS_OK;
 
  if (me->NR == NR && me->NT == NT)
    return;                     /* nothing to do */
 
  oldNT = me->NT;
  oldNR = me->NR;
  oldR = (EOS_REAL *) malloc (oldNR * sizeof (EOS_REAL));
  oldT = (EOS_REAL *) malloc (oldNT * sizeof (EOS_REAL));
 
  /* first copy current data into temp arrays for all tables */
  for (k = 0; k < oldNR; k++)
    oldR[k] = me->R[k];
  for (k = 0; k < oldNT; k++)
    oldT[k] = me->T[k];
 
  for (i = 0; i < MAX_TABLES_RECORDTYPE1; i++) {
    if (me->eosData.numSubtablesLoaded < i + 1)
      break;
 
    _eos_GetDataRecordType1 (me, &R, &T, &F, &coldCurve, i + 1);
 
    oldF[i] = (EOS_REAL **) malloc (oldNT * sizeof (EOS_REAL *));
 
    /* allocate memory continuously */
    ptr1 = (EOS_REAL *) malloc (sizeof (EOS_REAL) * oldNT * oldNR);
 
    /* get data of i-th subtable */
 
    for (j = 0; j < oldNT; j++) {
      oldF[i][j] = ptr1 + j * oldNR;
      for (k = 0; k < oldNR; k++)
	oldF[i][j][k] = F[j][k];
    }
 
    if ((me->eosData.tableNum == 301 || me->eosData.tableNum == 303) && coldCurve) {
      oldColdCurve[i] = (EOS_REAL *) malloc (oldNR * sizeof (EOS_REAL));
      for (k = 0; k < oldNR; k++)
	oldColdCurve[i][k] = coldCurve[k];
    }
    else {
      oldColdCurve[i] = NULL;
    }
  }
 
  /* reallocate memory in new_me */
  eos_SetSizeRecordType1 (me, NR, NT, me->eosData.tableNum);
 
  /* copy data into me for all tables */
  for (k = 0; k < oldNR; k++)
    me->R[k] = oldR[k];
  for (k = 0; k < oldNT; k++)
    me->T[k] = oldT[k];
 
  for (i = 0; i < MAX_TABLES_RECORDTYPE1; i++) {
    if (me->eosData.numSubtablesLoaded < i + 1)
      break;
 
    _eos_GetDataRecordType1 (me, &R, &T, &F, &coldCurve, i + 1);
 
    /* get data of i-th subtable */
 
    for (j = 0; j < oldNT; j++) {
      for (k = 0; k < oldNR; k++)
	F[j][k] = oldF[i][j][k];
    }
 
    if ((me->eosData.tableNum == 301 || me->eosData.tableNum == 303) && coldCurve) {
      for (k = 0; k < oldNR; k++)
	coldCurve[k] = oldColdCurve[i][k];
    }
    else {
      oldColdCurve[i] = NULL;
    }
  }
 
  EOS_FREE(oldR);
  EOS_FREE(oldT);
  EOS_FREE(ptr1);
}
 
/***********************************************************************/
/*!
 * \brief This helper function import new data into me after me is resized
 *        appropriately.
 *
 * \param[in,out] *me         - eos_RecordType1 : data object pointer;
 *                              contents are populated with data
 * \param[in]     subTableNum - EOS_INTEGER : subtable number
 * \param[in]     NR          - EOS_INTEGER : size of xtbls array
 * \param[in]     NT          - EOS_INTEGER : size of ytbls array
 * \param[in]     xtbl        - EOS_REAL**  : X-values to import
 * \param[in]     ytbl        - EOS_REAL**  : Y-values to import
 * \param[in]     ftbl        - EOS_REAL**  : F-values to import
 * 
 * \return none
 *
 ***********************************************************************/
void _eos_importTableRecordType1 (eos_RecordType1 *me, EOS_INTEGER subTableNum, EOS_INTEGER NR, EOS_INTEGER NT,
				  EOS_REAL *xtbl, EOS_REAL *ytbl, EOS_REAL *ftbl, EOS_REAL *CC)
{
  EOS_INTEGER j, k;
  EOS_REAL *R, *T, **F, *coldCurve;
 
  if (me->NR == NR || me->NT == NT) { 
    /* reallocate memory in new_me */
    eos_SetSizeRecordType1 (me, NR, NT, subTableNum);
  }

  /* require input */
  assert(xtbl);
  assert(ytbl);
  assert(ftbl);

  /* fetch data pointers */
  _eos_GetDataRecordType1 (me, &R, &T, &F, &coldCurve, subTableNum);
 
  /* copy data into me for subTableNum */
  for (k = 0; k < me->NR; k++)
    R[k] = xtbl[k];
  for (k = 0; k < me->NT; k++)
    T[k] = ytbl[k];
 
  for (j = 0; j < me->NT; j++) {
    for (k = 0; k < me->NR; k++)
      F[j][k] = ftbl[j * me->NR + k];
  }
 
  if ((me->eosData.tableNum == 301 || me->eosData.tableNum == 303) && coldCurve) {
    for (k = 0; k < me->NR; k++)
      coldCurve[k] = CC[k];
  }
  else {
    coldCurve = NULL;
  }
 
}
 
#define MIN_TARGET_N 100
 
/***********************************************************************/
/*! 
 * \brief This helper function iteratively inverts the table with respect to the first
 *        independent variable as required by the specified dataType.
 *        This algorithm operates on each y-chore (i.e., isotherm) in turn.
 *        Forced-monotonicity is assumed to be already applied to the tables in me.
 * 
 * \param[in,out] *me        - eos_RecordType1 : data object pointer;
 *                             contents are populated with data
 * \param[in]     th         - EOS_INTEGER : table handle
 * \param[in]     dataType   - EOS_INTEGER : data type
 * \param[in]         refy   - EOS_REAL : value to find in ytbl[] to determine new first independent variable
 * \param[out]   errorCode   - EOS_INTEGER : error code
 * 
 * \return none
 *
 ***********************************************************************/
void _eos_InvertAtSetupRecordType1_CATEGORY1 (eos_RecordType1 *me, EOS_INTEGER th, EOS_INTEGER dataType, EOS_REAL refy, EOS_INTEGER *errorCode)
{
  EOS_REAL *xtbl, *ytbl, **ftbl, *coldCurve;
  EOS_REAL *xtbl_new = NULL, *ytbl_new = NULL, *ftbl_new = NULL;
  EOS_INTEGER *ftbl_invt_mask = NULL;
  EOS_INTEGER err, nxtbl, nytbl;
  EOS_INTEGER subTableNum = EOS_TYPE_TO_SUB_TAB_NUM (dataType);
  EOS_INTEGER target_N = MIN_TARGET_N;
 
  *errorCode = EOS_OK;
 
  nxtbl = me->NR;
  nytbl = me->NT;
 
  target_N = MAX(nxtbl, target_N);

  /* allocate temporary arrays */
  xtbl_new = (EOS_REAL *) malloc (target_N * sizeof (EOS_REAL));
  ytbl_new = (EOS_REAL *) malloc (nytbl * sizeof (EOS_REAL));
  ftbl_new = (EOS_REAL *) malloc (target_N * nytbl * sizeof (EOS_REAL));

  /* Fetch data array pointers */
  _eos_GetDataRecordType1 (me, &xtbl, &ytbl, &ftbl, &coldCurve, subTableNum);
 
  /* create inverted table */
  _eos_GetInvertedTable (th, dataType,
			 NULL, NULL, NULL, NULL,
			 &xtbl, &ytbl, ftbl, coldCurve,
			 &nxtbl, &nytbl,
			 &xtbl_new, &ytbl_new, &ftbl_new,
			 &ftbl_invt_mask, &err, target_N);
  assert(target_N == nxtbl);
  assert(nytbl == me->NT);

  /* reallocate memory in me and store new data in me */
  _eos_importTableRecordType1 (me, subTableNum, nxtbl, nytbl, xtbl_new, ytbl_new, ftbl_new, NULL);

  assert(nxtbl == me->NR);
  assert(nytbl == me->NT);
    
  EOS_FREE(xtbl_new);
  EOS_FREE(ytbl_new);
  EOS_FREE(ftbl_new);
  EOS_FREE(ftbl_invt_mask);
}

/***********************************************************************/
/*!
 * \brief This helper function iteratively inverts the table with respect to the second
 *        independent variable as required by the specified dataType.
 *        This algorithm operates on each x-chore (i.e., isochore) in turn.
 *        Forced-monotonicity is assumed to be already applied to the tables in me.
 *
 * \param[in,out] *me        - eos_RecordType1 : data object pointer;
 *                             contents are populated with data
 * \param[in]     th         - EOS_INTEGER : table handle
 * \param[in]     dataType   - EOS_INTEGER : data type
 * \param[in]         refx   - EOS_REAL : value to find in xtbl[] to determine new second independent variable
 * \param[out]   errorCode   - EOS_INTEGER : error code
 *
 * \return none
 *
 ***********************************************************************/
void _eos_InvertAtSetupRecordType1_CATEGORY2 (eos_RecordType1 *me, EOS_INTEGER th, EOS_INTEGER dataType, EOS_REAL refx, EOS_INTEGER *errorCode)
{
  EOS_REAL *xtbl, *ytbl, **ftbl, *coldCurve;
  EOS_REAL *xtbl_new = NULL, *ytbl_new = NULL, *ftbl_new = NULL;
  EOS_INTEGER *ftbl_invt_mask = NULL;
  EOS_INTEGER err, nxtbl, nytbl;
  EOS_INTEGER subTableNum = EOS_TYPE_TO_SUB_TAB_NUM (dataType);
  EOS_INTEGER target_N = MIN_TARGET_N;

  *errorCode = EOS_OK;

  nxtbl = me->NR;
  nytbl = me->NT;

  target_N = MAX(nytbl, target_N);

  /* allocate temporary arrays */
  xtbl_new = (EOS_REAL *) malloc (nxtbl * sizeof (EOS_REAL));
  ytbl_new = (EOS_REAL *) malloc (target_N * sizeof (EOS_REAL));
  ftbl_new = (EOS_REAL *) malloc (target_N * nxtbl * sizeof (EOS_REAL));

  /* Fetch data array pointers */
  _eos_GetDataRecordType1 (me, &xtbl, &ytbl, &ftbl, &coldCurve, subTableNum);

  /* create inverted table */
  _eos_GetInvertedTable (th, dataType,
			 NULL, NULL, NULL, NULL,
			 &xtbl, &ytbl, ftbl, coldCurve,
			 &nxtbl, &nytbl,
			 &xtbl_new, &ytbl_new, &ftbl_new,
			 &ftbl_invt_mask, &err, target_N);
  assert(nxtbl == me->NR);
  assert(target_N == nytbl);

  /* reallocate memory in me and store new data in me */
  _eos_importTableRecordType1 (me, subTableNum, nxtbl, nytbl, xtbl_new, ytbl_new, ftbl_new, NULL);

  assert(nxtbl == me->NR);
  assert(nytbl == me->NT);

  EOS_FREE(xtbl_new);
  EOS_FREE(ytbl_new);
  EOS_FREE(ftbl_new);
  EOS_FREE(ftbl_invt_mask);
}

/***********************************************************************/
/*!
 * \brief This helper function merges data of a previously inverted CATEGORY1 table
 *        as required by the specified CATEGORY3 dataType.
 *        This algorithm operates on each x-chore (i.e., isochore) in turn.
 *        Forced-monotonicity is assumed to be already applied to the tables in me.
 *
 * \param[in,out] *me        - eos_RecordType1 : data object pointer;
 *                             contents are populated with data
 * \param[in]     th         - EOS_INTEGER : table handle
 * \param[in]     dataType   - EOS_INTEGER : data type
 * \param[out]   errorCode   - EOS_INTEGER : error code
 *
 * \return none
 *
 ***********************************************************************/
void _eos_InvertAtSetupRecordType1_CATEGORY3 (eos_RecordType1 *me, EOS_INTEGER th, EOS_INTEGER dataType, EOS_INTEGER *errorCode)
{
  EOS_REAL *xtbl, *ytbl, **ftbl, *coldCurve;
  EOS_REAL *_xtbl2, *_ytbl2, **_ftbl2, *_coldCurve2;
  EOS_REAL *xtbl_new = NULL, *ytbl_new = NULL, *ftbl_new = NULL;
  EOS_INTEGER *ftbl_invt_mask = NULL;
  EOS_INTEGER err, nxtbl, nytbl;
  EOS_INTEGER tableRef2 = EOS_EOS_TABLE_TYPE_REF2(dataType);
  EOS_INTEGER subTableNum2 = EOS_TYPE_TO_SUB_TAB_NUM(tableRef2);
  EOS_INTEGER subTableNum_new = 1;
  EOS_INTEGER target_N = MIN_TARGET_N;

  *errorCode = EOS_OK;

  nxtbl = me->NR;
  nytbl = me->NT;

  target_N = MAX(nxtbl, target_N);

  /* allocate temporary arrays */
  xtbl_new = (EOS_REAL *) malloc (target_N * sizeof (EOS_REAL));
  ytbl_new = (EOS_REAL *) malloc (nytbl * sizeof (EOS_REAL));
  ftbl_new = (EOS_REAL *) malloc (target_N * nytbl * sizeof (EOS_REAL));

  /* Fetch data array pointers to contain intermediate CATEGORY1 inverted data */
  _eos_GetDataRecordType1 (me, &_xtbl2, &_ytbl2, &_ftbl2, &_coldCurve2, subTableNum2);

  /* Fetch data array pointers to contain inverted CATEGORY3 data */
  _eos_GetDataRecordType1 (me, &xtbl, &ytbl, &ftbl, &coldCurve, subTableNum_new);

  /* create inverted table */
  _eos_GetInvertedTable (th, dataType,
			 _xtbl2, _ytbl2, *_ftbl2, _coldCurve2,
			 &xtbl, &ytbl, ftbl, coldCurve,
			 &nxtbl, &nytbl,
			 &xtbl_new, &ytbl_new, &ftbl_new,
			 &ftbl_invt_mask, &err, target_N);
  assert(target_N == nxtbl);
  assert(nytbl == me->NT);
    
  /* reallocate memory in me and store new data in me */
  _eos_importTableRecordType1 (me, subTableNum_new, nxtbl, nytbl, xtbl_new, ytbl_new, ftbl_new, NULL);

  assert(nxtbl == me->NR);
  assert(nytbl == me->NT);

  EOS_FREE(xtbl_new);
  EOS_FREE(ytbl_new);
  EOS_FREE(ftbl_new);
  EOS_FREE(ftbl_invt_mask);
}

/***********************************************************************/
/*!
 * \brief This helper function merges data of a previously inverted CATEGORY2 table
 *        as required by the specified CATEGORY4 dataType.
 *        This algorithm operates on each x-chore (i.e., isochore) in turn.
 *        Forced-monotonicity is assumed to be already applied to the tables in me.
 *
 * \param[in,out] *me        - eos_RecordType1 : data object pointer;
 *                             contents are populated with data
 * \param[in]     th         - EOS_INTEGER : table handle
 * \param[in]     dataType   - EOS_INTEGER : data type
 * \param[out]   errorCode   - EOS_INTEGER : error code
 *
 * \return none
 *
 ***********************************************************************/
void _eos_InvertAtSetupRecordType1_CATEGORY4 (eos_RecordType1 *me, EOS_INTEGER th, EOS_INTEGER dataType, EOS_INTEGER *errorCode)
{
  EOS_REAL *xtbl, *ytbl, **ftbl, *coldCurve;
  EOS_REAL *_xtbl2, *_ytbl2, **_ftbl2, *_coldCurve2;
  EOS_REAL *xtbl_new = NULL, *ytbl_new = NULL, *ftbl_new = NULL;
  EOS_INTEGER *ftbl_invt_mask = NULL;
  EOS_INTEGER err, nxtbl, nytbl;
  EOS_INTEGER tableRef2 = EOS_EOS_TABLE_TYPE_REF2(dataType);
  EOS_INTEGER subTableNum2 = EOS_TYPE_TO_SUB_TAB_NUM(tableRef2);
  EOS_INTEGER subTableNum_new = 1;
  EOS_INTEGER target_N = MIN_TARGET_N;

  *errorCode = EOS_OK;

  nxtbl = me->NR;
  nytbl = me->NT;

  target_N = MAX(nytbl, target_N);

  /* allocate temporary arrays */
  xtbl_new = (EOS_REAL *) malloc (nxtbl * sizeof (EOS_REAL));
  ytbl_new = (EOS_REAL *) malloc (target_N * sizeof (EOS_REAL));
  ftbl_new = (EOS_REAL *) malloc (target_N * nxtbl * sizeof (EOS_REAL));

  /* Fetch data array pointers to contain intermediate CATEGORY2 inverted data */
  _eos_GetDataRecordType1 (me, &_xtbl2, &_ytbl2, &_ftbl2, &_coldCurve2, subTableNum2);

  /* Fetch data array pointers to contain inverted CATEGORY4 data */
  _eos_GetDataRecordType1 (me, &xtbl, &ytbl, &ftbl, &coldCurve, subTableNum_new);

  /* create inverted table */
  _eos_GetInvertedTable (th, dataType,
			 _xtbl2, _ytbl2, *_ftbl2, _coldCurve2,
			 &xtbl, &ytbl, ftbl, coldCurve,
			 &nxtbl, &nytbl,
			 &xtbl_new, &ytbl_new, &ftbl_new,
			 &ftbl_invt_mask, &err, target_N);
  assert(nxtbl == me->NR);
  assert(target_N == nytbl);
    
  /* reallocate memory in me and store new data in me */
  _eos_importTableRecordType1 (me, subTableNum_new, nxtbl, nytbl, xtbl_new, ytbl_new, ftbl_new, NULL);

  assert(nxtbl == me->NR);
  assert(nytbl == me->NT);

  EOS_FREE(xtbl_new);
  EOS_FREE(ytbl_new);
  EOS_FREE(ftbl_new);
  EOS_FREE(ftbl_invt_mask);
}

/***********************************************************************/
/*! 
 * \brief This function deallocates and resets the memory within the 
 *        eos_RecordType1::extrapolationBounds container.
 * 
 * \param[in,out] *me        - eos_RecordType1 : data object pointer;
 *                                               contents of object are initialized
 * 
 * \return none
 *
 ***********************************************************************/
void _eos_DestroyExtrapolationBounds(eos_RecordType1 *me)
{
  me->extrapolationBounds.stored = EOS_FALSE;
  me->extrapolationBounds.nx = 0;
  me->extrapolationBounds.ny = 0;
  EOS_FREE(me->extrapolationBounds.x);
  EOS_FREE(me->extrapolationBounds.xLo);
  EOS_FREE(me->extrapolationBounds.yLo);
  EOS_FREE(me->extrapolationBounds.xHi);
  EOS_FREE(me->extrapolationBounds.yHi);
}

/***********************************************************************/
/*!
 * \brief RecordType1 class constructor
 *
 * \param[in,out] *me        - eos_RecordType1 : data object pointer;
 *                                               contents of object are initialized
 * \param[in]     dataType   - EOS_INTEGER : data type
 *
 * \return none
 * 
 ***********************************************************************/
void eos_SetExtrapolationBoundsRecordType1(void *ptr, EOS_INTEGER dataType)
{
  eos_RecordType1 *me;
  int i, j;
  EOS_REAL *x, *y, **F, *cc;
  EOS_INTEGER cat = EOS_CATEGORY (dataType);
  EOS_INTEGER subTableNum = EOS_TYPE_TO_SUB_TAB_NUM (dataType);
  eos_OptionValue *optVal = NULL;

  me = (eos_RecordType1 *) ptr;
  if (me->extrapolationBounds.stored) return; /* already done for me */

  if (! me->eosData.isLoaded) return; /* no data is loaded */

  optVal = _eos_getOptionEosData (&(me->eosData), EOS_USE_TAYLOR_FIT);
  if (optVal->bval) return; /* no tablulated data is loaded to use for bounds */

  /* Fetch data array pointers */
  _eos_GetDataRecordType1 (me, &x, &y, &F, &cc, subTableNum);

  /* store boundaries */
  switch (cat) {
    EOS_INTEGER tabInd2;
    EOS_INTEGER subTableNum2;

  case EOS_CATEGORY0:          /* indicates the table is not inverted */
    {
      me->extrapolationBounds.nx  = 1;
      me->extrapolationBounds.ny  = 1;
      me->extrapolationBounds.x   = NULL;
      me->extrapolationBounds.xLo = (EOS_REAL*)malloc(me->extrapolationBounds.nx * sizeof(EOS_REAL));
      me->extrapolationBounds.yLo = (EOS_REAL*)malloc(me->extrapolationBounds.ny * sizeof(EOS_REAL));
      me->extrapolationBounds.xHi = (EOS_REAL*)malloc(me->extrapolationBounds.nx * sizeof(EOS_REAL));
      me->extrapolationBounds.yHi = (EOS_REAL*)malloc(me->extrapolationBounds.ny * sizeof(EOS_REAL));

      me->extrapolationBounds.yLo[0] = me->T[0];
      me->extrapolationBounds.yHi[0] = me->T[me->NT - 1];
      me->extrapolationBounds.xLo[0] = me->R[0];
      me->extrapolationBounds.xHi[0] = me->R[(me->NR - 1)];

      me->extrapolationBounds.stored = EOS_TRUE;
      break;
    }
  case EOS_CATEGORY3:          /* indicates the table is a merging of a CATEGORY1 table and a CATEGORY0 table */
    {
      /* get the data pointers and types for these variables */
      tabInd2 = EOS_EOS_TABLE_TYPE_REF2 (dataType);

      /* Fetch alternative data array pointers */
      subTableNum2 = EOS_TYPE_TO_SUB_TAB_NUM (tabInd2);
      _eos_GetDataRecordType1 (me, &x, &y, &F, &cc, subTableNum2);

      /* intentionaly continue to following EOS_CATEGORY1 block; DO NOT BREAK! */
    }
  case EOS_CATEGORY1:          /* indicates the table is inverted with respect to 1st independent variable */
    {
      me->extrapolationBounds.nx  = me->NT;
      me->extrapolationBounds.ny  = 1;
      me->extrapolationBounds.x   = (EOS_REAL*)malloc(me->NT                     * sizeof(EOS_REAL));
      me->extrapolationBounds.xLo = (EOS_REAL*)malloc(me->extrapolationBounds.nx * sizeof(EOS_REAL));
      me->extrapolationBounds.yLo = (EOS_REAL*)malloc(me->extrapolationBounds.ny * sizeof(EOS_REAL));
      me->extrapolationBounds.xHi = (EOS_REAL*)malloc(me->extrapolationBounds.nx * sizeof(EOS_REAL));
      me->extrapolationBounds.yHi = (EOS_REAL*)malloc(me->extrapolationBounds.ny * sizeof(EOS_REAL));

      me->extrapolationBounds.yLo[0] = me->T[0];
      me->extrapolationBounds.yHi[0] = me->T[me->NT - 1];
      for (j=0; j<me->NT; j++) {
	me->extrapolationBounds.x[j]   = me->T[j];
	me->extrapolationBounds.xLo[j] = F[j][0           ];
	me->extrapolationBounds.xHi[j] = F[j][(me->NR - 1)];
      }

      me->extrapolationBounds.stored = EOS_TRUE;
      break;
    }
  case EOS_CATEGORY4:          /* indicates the table is a merging of a CATEGORY2 table and a CATEGORY0 table */
    {
      /* get the data pointers and types for these variables */
      tabInd2 = EOS_EOS_TABLE_TYPE_REF2 (dataType);

      /* Fetch alternative data array pointers */
      subTableNum2 = EOS_TYPE_TO_SUB_TAB_NUM (tabInd2);
      _eos_GetDataRecordType1 (me, &x, &y, &F, &cc, subTableNum2);

      /* intentionaly continue to following EOS_CATEGORY2 block; DO NOT BREAK! */
    }
  case EOS_CATEGORY2:          /* indicates the table is inverted with respect to 2nd independent variable */
    {
      me->extrapolationBounds.nx = 1;
      me->extrapolationBounds.ny = me->NR;
      me->extrapolationBounds.x   = (EOS_REAL*)malloc(me->NR                     * sizeof(EOS_REAL));
      me->extrapolationBounds.xLo = (EOS_REAL*)malloc(me->extrapolationBounds.nx * sizeof(EOS_REAL));
      me->extrapolationBounds.yLo = (EOS_REAL*)malloc(me->extrapolationBounds.ny * sizeof(EOS_REAL));
      me->extrapolationBounds.xHi = (EOS_REAL*)malloc(me->extrapolationBounds.nx * sizeof(EOS_REAL));
      me->extrapolationBounds.yHi = (EOS_REAL*)malloc(me->extrapolationBounds.ny * sizeof(EOS_REAL));

      me->extrapolationBounds.xLo[0] = me->R[0];
      me->extrapolationBounds.xHi[0] = me->R[me->NR - 1];
      for (i=0; i<me->NR; i++) {
	me->extrapolationBounds.x[i]   = me->R[i];
	me->extrapolationBounds.yLo[i] = F[0         ][i];
	me->extrapolationBounds.yHi[i] = F[me->NT - 1][i];
      }

      me->extrapolationBounds.stored = EOS_TRUE;
      break;
    }
  }

  assert(me->extrapolationBounds.stored == EOS_TRUE);
}

/***********************************************************************/
/*! 
 * \brief This function inverts the tables as required by the specified dataType.
 *        All tables in me must be inverted since the independent variable changes.
 *        Forced-monotonicity is assumed to be already applied to the tables in me.
 * 
 * \param[in,out] *ptr       - void : data object pointer;
 *                             internally recast to eos_RecordType1*
 *                             contents are populated with data
 * \param[in]     th         - EOS_INTEGER : table handle
 * \param[in]     dataType   - EOS_INTEGER : data type
 * \param[out]   errorCode   - EOS_INTEGER : error code
 * 
 * \return none
 *
 ***********************************************************************/
void eos_InvertAtSetupRecordType1 (void *ptr, EOS_INTEGER th, EOS_INTEGER dataType, EOS_INTEGER *errorCode)
{
  eos_RecordType1 *me;
  EOS_INTEGER subTableNum = EOS_TYPE_TO_SUB_TAB_NUM (dataType);
  EOS_INTEGER cat = EOS_CATEGORY (dataType);

  me = (eos_RecordType1 *) ptr;
  *errorCode = EOS_OK;

  /* store boundaries if necessary */
  eos_SetExtrapolationBoundsRecordType1(ptr, dataType);

  /* invert table if necessary */
  switch (cat) {
  case EOS_CATEGORY0:          /* indicates the table is not inverted */
    {
      /* invalid option for this datatype */
      *errorCode = EOS_INVALID_OPTION_FLAG;
      return;
    }
  case EOS_CATEGORY1:          /* indicates the table is inverted with respect to 1st independent variable */
    {
      /* for each isotherm, solve D(f,T) -- where f = P, U, A or S -- given f values selected at or near the ??? */
      _eos_InvertAtSetupRecordType1_CATEGORY1 (me, th, dataType, (EOS_REAL)0, errorCode);

      break;
    }
  case EOS_CATEGORY2:          /* indicates the table is inverted with respect to 2nd independent variable */
    {
      /* for each isochore, solve T(D,f) -- where f = P, U, A or S -- given f values selected at or near the normal D */
      _eos_InvertAtSetupRecordType1_CATEGORY2 (me, th, dataType, me->refDensity, errorCode);

      if (subTableNum > 1) {   /* store new dependent values in table1 */

	EOS_REAL *xtbl, *ytbl, **ftbl, *coldCurve;
	EOS_REAL *xtbl_new, *ytbl_new, **ftbl_new;

	_eos_GetDataRecordType1 (me, &xtbl, &ytbl, &ftbl, &coldCurve, subTableNum);
	_eos_GetDataRecordType1 (me, &xtbl_new, &ytbl_new, &ftbl_new, &coldCurve, 1);

	memcpy (*ftbl_new, *ftbl, me->NT * me->NR * sizeof(EOS_REAL));
      }

      break;
    }
  case EOS_CATEGORY3:          /* indicates the table is a merging of a CATEGORY1 table and a CATEGORY0 table */
    {
      /* merge CATEGORY1 data with appropriate CATEGORY0 data */
      _eos_InvertAtSetupRecordType1_CATEGORY3 (ptr, th, dataType, errorCode);

      break;
    }
  case EOS_CATEGORY4:          /* indicates the table is a merging of a CATEGORY2 table and a CATEGORY0 table */
    {
      /* merge CATEGORY2 data with appropriate CATEGORY0 data */
      _eos_InvertAtSetupRecordType1_CATEGORY4 (me, th, dataType, errorCode);

      break;
    }
  }

  me->eosData.numSubtablesLoaded = 1; /* keep only first array */
  _eos_FreeUnusedArraysRecordType1(me);
  me->isInvertedAtSetup = EOS_TRUE;
}

/***********************************************************************/
/*! 
 * \brief This function allocates enough memory in class eos_RecordType1
 *  to hold R and T arrays of specified size.
 * 
 * \param[in,out] *me - eos_RecordType1 : data object pointer;
 *                      contents of *me are allocated
 * \param[in]     NR  - EOS_INTEGER : size of R array
 * \param[in]     NT  - EOS_INTEGER : size of T array
 * 
 * \return none
 *
 ***********************************************************************/
void eos_SetSizeRecordType1 (eos_RecordType1 *me, EOS_INTEGER NR,
                             EOS_INTEGER NT, EOS_INTEGER tableNum)
{
  int i;
  EOS_REAL *ptr, *ptr1, *ptr2, *ptr3, *ptr4;
  EOS_BOOLEAN useColdCurve = EOS_FALSE;

  // assume that it's the first time!
  me->NR = NR;
  me->NT = NT;
  if (me->R)
    EOS_FREE (me->R);           // This deallocates all continuous memory
  /* memory is allocated continuously in one chunk for R, T and all tables */
  if (me->table1)
    EOS_FREE (me->table1);
  if (me->table2)
    EOS_FREE (me->table2);
  if (me->table3)
    EOS_FREE (me->table3);
  if (me->table4)
    EOS_FREE (me->table4);

  if (me->coldCurve1 && me->coldCurve2 && me->coldCurve3 && me->coldCurve4
      && (tableNum == 301 || tableNum == 303))
    useColdCurve = EOS_TRUE; /* remember if CC arrays were already allocated */

  if (me->coldCurve1)
    EOS_FREE (me->coldCurve1);
  if (me->coldCurve2)
    EOS_FREE (me->coldCurve2);
  if (me->coldCurve3)
    EOS_FREE (me->coldCurve3);
  if (me->coldCurve4)
    EOS_FREE (me->coldCurve4);

  if (NR == 0 || NT == 0)
    return;                     // just deallocate memory this time

  me->table1 = (EOS_REAL **) malloc (NT * sizeof (EOS_REAL *));
  me->table2 = (EOS_REAL **) malloc (NT * sizeof (EOS_REAL *));
  me->table3 = (EOS_REAL **) malloc (NT * sizeof (EOS_REAL *));
  me->table4 = (EOS_REAL **) malloc (NT * sizeof (EOS_REAL *));

  /* memory is allocated continuously in one chunk for R, T and all tables */
  ptr = (EOS_REAL *) malloc (sizeof (EOS_REAL) * (NT + NR + MAX_TABLES_RECORDTYPE1 * NT * NR));
  me->R = ptr;
  me->T = ptr + NR;
  ptr1 = ptr + NT + NR;
  ptr2 = ptr1 + NT * NR;
  ptr3 = ptr2 + NT * NR;
  ptr4 = ptr3 + NT * NR;

  for (i = 0; i < NT; i++) {
    me->table1[i] = ptr1 + i * NR;
    me->table2[i] = ptr2 + i * NR;
    me->table3[i] = ptr3 + i * NR;
    me->table4[i] = ptr4 + i * NR;
  }

  if (useColdCurve) {
    me->coldCurve1 = (EOS_REAL *) malloc (NR * sizeof (EOS_REAL));
    me->coldCurve2 = (EOS_REAL *) malloc (NR * sizeof (EOS_REAL));
    me->coldCurve3 = (EOS_REAL *) malloc (NR * sizeof (EOS_REAL));
    me->coldCurve4 = (EOS_REAL *) malloc (NR * sizeof (EOS_REAL));
  }
  else {
    me->coldCurve1 = NULL;
    me->coldCurve2 = NULL;
    me->coldCurve3 = NULL;
    me->coldCurve4 = NULL;
  }
}

/***********************************************************************/
/*! 
 * \brief This function returns the dimensions of the specified table.
 * 
 * \param[out]    NR  - EOS_INTEGER : size of R array
 * \param[out]    NT  - EOS_INTEGER : size of T array
 * \param[in]     *me - eos_RecordType1 : data object pointer;
 * 
 * \return none
 *
 ***********************************************************************/
void eos_GetSizeRecordType1 (eos_RecordType1 *me, EOS_INTEGER *NR,
                             EOS_INTEGER *NT)
/* returns the size of the table */
{
  *NR = me->NR;
  *NT = me->NT;
}

/***********************************************************************/
/*! 
 * \brief This function returns pointers to data of class eos_RecordType1
 * 
 * \param[in]     *me         - eos_RecordType1 : data object pointer;
 * \param[out]    **R         - EOS_REAL : holds R-pointer
 * \param[out]    **T         - EOS_REAL : holds T-pointer
 * \param[out]    ***F        - EOS_REAL : holds F-pointer (F is 2d array allocated as 1 d array NR * NT)
 * \param[out]    **coldCurve - EOS_REAL : holds cold curve-pointer
 * \param[in]     subtableNum - EOS_INTEGER : number of subtable to take the data from.
 * 
 * \return none
 *
 ***********************************************************************/
void _eos_GetDataRecordType1 (eos_RecordType1 *me, EOS_REAL **R, EOS_REAL **T,
                              EOS_REAL ***F, EOS_REAL **coldCurve,
                              EOS_INTEGER subTableNum)
{
  *R = me->R;
  *T = me->T;
  *F = me->table1;
  *coldCurve = me->coldCurve1;

  if (*R && *T && *F) {
    if (me->isInvertedAtSetup) {
      /* only table1 is used in this object; therefore, subTableNum=1 is implied */
      return;
    }
  }

  if (me->eosData.numSubtablesLoaded < subTableNum) {
    *R = *T = NULL;
    *F = NULL;
    return;
  }

  switch (subTableNum) {
  case 1:
    *F = me->table1;
    *coldCurve = me->coldCurve1;
    break;
  case 2:
    *F = me->table2;
    *coldCurve = me->coldCurve2;
    break;
  case 3:
    *F = me->table3;
    *coldCurve = me->coldCurve3;
    break;
  case 4:
    *F = me->table4;
    *coldCurve = me->coldCurve4;
    break;
  default:
    break;
  }
}

/***********************************************************************/
/*! 
 * \brief This function prints the data of class eos_RecordType1
 * 
 * \param[out]    *err - EOS_INTEGER : error code
 * \param[in]     *ptr   - void : data object pointer;
 *                         internally recast to eos_RecordType1*
 * \param[in]     *fname - EOS_CHAR : file name
 * \param[in]     append - EOS_INTEGER : whether or not to append to file
 * \param[in]     th     - EOS_INTEGER : table Handle
 *
 * \return none
 *
 ***********************************************************************/
void eos_PrintRecordType1 (void *ptr, EOS_INTEGER th, EOS_CHAR *fname,
                           EOS_INTEGER append, EOS_INTEGER *err)
{
  EOS_INTEGER nxtbl, nytbl, subTableNum;
  EOS_INTEGER jl, nline, ipage, iy, jf0, jx, jy, jf;
  EOS_INTEGER dataType;
  char buffer[_MIN_FIELD_WIDTH+1];
  EOS_CHAR *sesame_fname;
  EOS_REAL xprnt, yprnt, fprnt, *coldCurve = NULL, *xtbls = NULL, *ytbls =
    NULL, *ftbls = NULL, xconv, yconv, fconv;
  EOS_REAL *_xtbls = NULL, *_ytbls = NULL, **_ftbls = NULL, *_coldCurve = NULL;
  EOS_REAL *_xtbls2 = NULL, *_ytbls2 = NULL, **_ftbls2 = NULL, *_coldCurve2 =
    NULL, *f_refData = NULL;
  EOS_REAL *xtbls_sv = NULL, *ytbls_sv = NULL, *ftbls_sv = NULL;
  EOS_INTEGER *ftbls_invt_mask = NULL;
  FILE *tableFile;
  eos_RecordType1 *me;
  eos_OptionValue *optVal = NULL;
  EOS_BOOLEAN use_taylor_fit = EOS_FALSE;
  EOS_REAL *xtbl_new = NULL, *ytbl_new = NULL, *ftbl_new = NULL;

  //#define DEBUG_EOS_PRINTRECORDTYPE1
#ifdef DEBUG_EOS_PRINTRECORDTYPE1
  EOS_INTEGER k, l;
#endif

  EOS_BOOLEAN logAxes = EOS_TYPE_TO_LOG_AXES (gEosDataMap.tableTypes[th]);

  me = (eos_RecordType1 *) ptr;

  *err = EOS_OK;
  dataType = eos_GetDataTypeFromTableHandle (th, err);
  if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) != EOS_OK)
    return;

  /* custom output is required for Taylor polynomial data */
  optVal = _eos_getOptionEosData (&(me->eosData), EOS_USE_TAYLOR_FIT);
  use_taylor_fit = optVal->bval;

  if (me->isInvertedAtSetup)
    subTableNum = 1;
  else
    subTableNum = EOS_TYPE_TO_SUB_TAB_NUM (dataType);

  if (! use_taylor_fit && subTableNum > me->eosData.numSubtablesLoaded) {
    *err = EOS_DATA_TYPE_NOT_FOUND;
    return;
  }

#ifdef DEBUG_EOS_PRINTRECORDTYPE1
  if (!use_taylor_fit) {
    printf
      ("------ eos_PrintRecordType1\nmat=%i th=%i subtab=%i type=%s table1, table2, table3 and table4\n",
       me->eosData.materialID, th, subTableNum, EOS_TYPE_TO_STRING (dataType));
    for (k = 0; k < me->NR; k++)
      for (l = 0; l < me->NT; l++)
	printf ("%23.15e %23.15e %23.15e %23.15e %23.15e %23.15e\n",
		me->R[k], me->T[l], me->table1[l][k], me->table2[l][k],
		me->table3[l][k], me->table4[l][k]);
  }
#endif

  tableFile = (append == 0) ? fopen (fname, "w") : fopen (fname, "a");
  if (!tableFile) {
    *err = EOS_OPEN_OUTPUT_FILE_FAILED;
    return;
  }

  /* process x-conversion option */
  xconv = eos_getRealOptionFromTableHandle (th, EOS_X_CONVERT, err);
  if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) != EOS_OK)
    return;

  /* process y-conversion option */
  yconv = eos_getRealOptionFromTableHandle (th, EOS_Y_CONVERT, err);
  if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) != EOS_OK)
    return;

  /* process f-conversion option */
  fconv = eos_getRealOptionFromTableHandle (th, EOS_F_CONVERT, err);
  if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) != EOS_OK)
    return;

  /* figure out the name of the sesame file */
  sesame_fname = _eos_GetSesameFileName (&(me->eosData));
  if (sesame_fname == NULL) {
    sesame_fname = "";
    *err = EOS_OPEN_SESAME_FILE_FAILED;
    fclose (tableFile);
    return;
  }

  if (use_taylor_fit) {
    /* Print Taylor polynomial fit data if requested; then return */

    int i;
    EOS_BOOLEAN freeEnergyIsDefined = (me->Taylor_objects[2]) ? EOS_TRUE : EOS_FALSE;
    EOS_BOOLEAN isOneDimDatatype = EOS_IS_ONE_DIM_TYPE (dataType);

    i = subTableNum-1;
    /* for (i=0; i<4; i++) { */
    if (me->Taylor_objects[i]) {
      fprintf (tableFile,
	       "%sTableHandle=%i matid =%6d source = %-20s\n%s%s\n%s%s\nTaylor polynomial %s fit parameters\n",
	       ((append) ? "\n" : ""), th, me->eosData.materialID,
	       sesame_fname, "Data Type = ", EOS_TYPE_TO_STRING (dataType),
	       "Description = ", EOS_TYPE_TO_TAB_NAME (dataType),
	       ((isOneDimDatatype)?"curve":"surface"));

      _eos_dbl2String (fconv, _MIN_FIELD_WIDTH, buffer);
      fprintf (tableFile,
	       "  fconv =%*s         ", _MIN_FIELD_WIDTH + 1, buffer);
      _eos_dbl2String (xconv, _MIN_FIELD_WIDTH, buffer);
      fprintf (tableFile, "xconv =%*s         ", _MIN_FIELD_WIDTH + 1, buffer);
      _eos_dbl2String (yconv, _MIN_FIELD_WIDTH, buffer);
      fprintf (tableFile, "yconv =%*s\n\n", _MIN_FIELD_WIDTH + 1, buffer);

      /* Print the coefficients of all of the Taylor objects to stdout */
#define USE_PRINTTAYLORCOEFFICIENTS
#ifdef USE_PRINTTAYLORCOEFFICIENTS
      eos_PrintTaylorCoefficients(me->Taylor_objects[i], me->M, me->N, dataType, tableFile);
#else
      {
	int j;

	for (j=0; j<(me->M * me->N); j++) {
	  eos_Taylor *p;
	  p = (me->Taylor_objects[i])[j];
	  p->Print(p, dataType, tableFile);
	}
      }
#endif
    }
    else if (freeEnergyIsDefined) {
      fprintf (tableFile,
	       "%sTableHandle=%i matid =%6d source = %-20s\n%s%s\n%s%s\nDependent upon the Helmholtz Free Energy Taylor polynomial %s fit parameters\n",
	       ((append) ? "\n" : ""), th, me->eosData.materialID,
	       sesame_fname, "Data Type = ", EOS_TYPE_TO_STRING (dataType),
	       "Description = ", EOS_TYPE_TO_TAB_NAME (dataType),
	       ((isOneDimDatatype)?"curve":"surface"));
    }
    /* } */

    fclose (tableFile);

    /* all done */
    return;
  }

  // set numbers of and pointers to eos data table x,y,f values.
  nxtbl = me->NR;
  nytbl = me->NT;
  _eos_GetDataRecordType1 (me, &_xtbls, &_ytbls, &_ftbls, &_coldCurve, EOS_TYPE_TO_SUB_TAB_NUM(dataType));
  xtbls = _xtbls;
  ytbls = _ytbls;
  ftbls = *_ftbls;

  /* save pointer addresses for future comparison */
  xtbls_sv = xtbls;
  ytbls_sv = ytbls;
  ftbls_sv = ftbls;

  /* EOS_CATEGORY1 -- table is inverted with respect to 1st independent variable */
  /* EOS_CATEGORY2 -- table is inverted with respect to 2nd independent variable */
  /* EOS_CATEGORY3 -- table is merged with another function to change 1st independent variable */
  /* EOS_CATEGORY4 -- table is merged with another function to change 2nd independent variable */

  if (EOS_CATEGORY (dataType) != EOS_CATEGORY0 && ! me->isInvertedAtSetup) {
    if (EOS_CATEGORY (dataType) == EOS_CATEGORY3 ||
        EOS_CATEGORY (dataType) == EOS_CATEGORY4) {
      if (EOS_TYPE_TO_RECORD_TYPE (EOS_EOS_TABLE_TYPE_REF2 (dataType)) ==
          EOS_RECORD_TYPE1) {
        /* get data for reference2 data type */
        _eos_GetDataRecordType1 (me, &_xtbls2, &_ytbls2, &_ftbls2,
                                 &_coldCurve2,
                                 EOS_TYPE_TO_SUB_TAB_NUM
                                 (EOS_EOS_TABLE_TYPE_REF2 (dataType)));
      }
      else {
        *err = EOS_UNDEFINED;
        *err = eos_SetCustomErrorMsg(th, *err,
				     "eos_RecordType1::eos_PrintRecordType1 ERROR, invalid data RecordType, %i",
				     EOS_TYPE_TO_RECORD_TYPE (EOS_EOS_TABLE_TYPE_REF2 (dataType)));
        return;
      }

      if (!(_xtbls2 || _ytbls2 || _ftbls2 || _coldCurve2)) {
        *err = eos_SetCustomErrorMsg(th, *err,
				     "eos_RecordType1::eos_PrintRecordType1 ERROR, temporary array(s) not allocated");
        return;
      }

      f_refData = (_ftbls2) ? &(_ftbls2[0][0]) : NULL;
    }

    { /* now generate inverted table */

      EOS_INTEGER target_N = MIN_TARGET_N;

      switch (EOS_CATEGORY (dataType)) {
      case EOS_CATEGORY1:          /* x(F,y) */
      case EOS_CATEGORY3:          /* F(G,y) */
	target_N = MAX(nxtbl, target_N);
	/* allocate temporary arrays */
	xtbl_new = (EOS_REAL *) malloc (target_N * sizeof (EOS_REAL));
	ytbl_new = (EOS_REAL *) malloc (nytbl * sizeof (EOS_REAL));
	ftbl_new = (EOS_REAL *) malloc (target_N * nytbl * sizeof (EOS_REAL));
	break;
      case EOS_CATEGORY2:          /* y(x,F) */
      case EOS_CATEGORY4:          /* F(x,G) */
	target_N = MAX(nytbl, target_N);
	/* allocate temporary arrays */
	xtbl_new = (EOS_REAL *) malloc (nxtbl * sizeof (EOS_REAL));
	ytbl_new = (EOS_REAL *) malloc (target_N * sizeof (EOS_REAL));
	ftbl_new = (EOS_REAL *) malloc (target_N * nxtbl * sizeof (EOS_REAL));
	break;
      default:
	*err = EOS_UNDEFINED;
	*err = eos_SetCustomErrorMsg(th, *err,
				     "eos_RecordType1::eos_PrintRecordType1 ERROR, invalid data category, %i",
				     EOS_CATEGORY (dataType));
	return;
      }

      _eos_GetInvertedTable (th, dataType,
			     _xtbls2, _ytbls2, f_refData, _coldCurve2,
			     &xtbls, &ytbls, &ftbls, coldCurve,
			     &nxtbl, &nytbl,
			     &xtbl_new, &ytbl_new, &ftbl_new,
			     &ftbls_invt_mask, err, target_N /* -1 */);
      if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) != EOS_OK) {
	*err = eos_SetCustomErrorMsg(th, *err,
				     "eos_RecordType1::eos_PrintRecordType1 ERROR, _eos_GetInvertedTable failed to invert table");
	return;
      }

      /* use new pointer addresses */
      xtbls = xtbl_new;
      ytbls = ytbl_new;
      ftbls = ftbl_new;

    }

    /* NULLify coldCurve[] since it is not needed for dumping after interpolation of inverted table */
    coldCurve = NULL;

  }
  else if (me->isInvertedAtSetup) {

    /* do nothing here */

  }
  else {

    xtbls = (EOS_REAL*) malloc (nxtbl         * sizeof (EOS_REAL));
    ytbls = (EOS_REAL*) malloc (nytbl         * sizeof (EOS_REAL));
    ftbls = (EOS_REAL*) malloc (nxtbl * nytbl * sizeof (EOS_REAL));

    /* copy data into temporary arrays */
    memcpy (xtbls, xtbls_sv, nxtbl * sizeof(EOS_REAL));
    memcpy (ytbls, ytbls_sv, nytbl * sizeof(EOS_REAL));
    memcpy (ftbls, ftbls_sv, nxtbl * nytbl * sizeof(EOS_REAL));

  }

  { /* fill ftbls_invt_mask[] with extrapolation error codes */

    int i, j;
    EOS_INTEGER nXYPairs = nxtbl * nytbl;
    EOS_REAL    *xVals = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
    EOS_REAL    *yVals = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));

    EOS_FREE (ftbls_invt_mask);
    ftbls_invt_mask = (EOS_INTEGER *) malloc (nXYPairs * sizeof (EOS_INTEGER));

    /* distribute (x,y) values */
    for (i = 0; i < nxtbl; i++) {
      for (j = 0; j < nytbl; j++) {
	xVals[i + j * nxtbl] = xtbls[i];
	yVals[i + j * nxtbl] = ytbls[j];
      }
    }

    me->eosData.CheckExtrap(ptr, th, dataType, nXYPairs, xVals, yVals, ftbls_invt_mask, err);

    EOS_FREE(xVals);
    EOS_FREE(yVals);

  }

  /* print eos data table x,y,f values.
     x values run down page and y values run across page.
     take anti-logs if table contains common logs. */

  ipage = 0;
  for (iy = 1; iy <= nytbl; iy += 10) {
    int i, opt_count = 0;;
    EOS_CHAR label[20] = "Loading Options: ";

    ipage = ipage + 1;

    fprintf (tableFile, "%sTableHandle=%i matid =%6d source = %-20s page = %2u\n",
             ((append || ipage > 1) ? "\n" : ""), th, me->eosData.materialID, sesame_fname, ipage);
    fprintf (tableFile, "%s%s\n",
	     "Data Type = ", EOS_TYPE_TO_STRING (dataType));

    /* display non-default loading options */
    for (i=0; i<EOS_TOTAL_TABLE_OPTIONS; i++) {
      eos_Data *obj1 = &(me->eosData);
      EOS_INTEGER opt;
      opt = eos_OptionFlags[i];

      if (! EOS_IS_LOADING_OPTION(opt) ||                /* consider only loading options -- duh! */
	  opt == EOS_DUMP_DATA || opt == EOS_APPEND_DATA /* this should be obvious if the tableFile exists! */
	  ) continue;

      if ( ! _isOptionDefaultEosData (obj1, opt, dataType) ) {
	fprintf (tableFile, "%s%s", label, get_OptionFlagStr(opt));
	sprintf(label, ", ");
	opt_count++;
      }

    }
    if (opt_count) fprintf (tableFile, "\n");

    fprintf (tableFile, "%s%s\n",
	     "Description = ", EOS_TYPE_TO_TAB_NAME (dataType));

    _eos_dbl2String (fconv, _MIN_FIELD_WIDTH, buffer);
    fprintf (tableFile,
             "  fconv =%*s         ", _MIN_FIELD_WIDTH + 1, buffer);
    _eos_dbl2String (xconv, _MIN_FIELD_WIDTH, buffer);
    fprintf (tableFile, "xconv =%*s         ", _MIN_FIELD_WIDTH + 1, buffer);
    _eos_dbl2String (yconv, _MIN_FIELD_WIDTH, buffer);
    fprintf (tableFile, "yconv =%*s\n\n", _MIN_FIELD_WIDTH + 1, buffer);

    nline = (nytbl - iy + 1 < 10) ? nytbl - iy + 1 : 10;
    jy = iy - 1;
    fprintf (tableFile, "%*s", _MIN_FIELD_WIDTH + 1, "y =");
    for (jl = 1; jl <= nline; jl++) {
      jy = jy + 1;
      yprnt = ytbls[jy - 1] * yconv;
      if (logAxes)
        yprnt = pow (10.0, yprnt);

      _eos_dbl2String (yprnt, _MIN_FIELD_WIDTH, buffer);
      fprintf (tableFile, "%*s", _MIN_FIELD_WIDTH + 1, buffer);
    }
    fprintf (tableFile, "\n");

    fprintf (tableFile, " x =\n");

    jf0 = nxtbl * (iy - 2);
    for (jx = 1; jx <= nxtbl; jx++) {
      xprnt = xtbls[jx - 1] * xconv;
      if (logAxes)
        xprnt = pow (10.0, xprnt);
      _eos_dbl2String (xprnt, _MIN_FIELD_WIDTH, buffer);
      fprintf (tableFile, "%*s", _MIN_FIELD_WIDTH + 1, buffer);

      jf = jf0 + jx;
      for (jl = 1; jl <= nline; jl++) {
        jf = jf + nxtbl;
        if (!ftbls_invt_mask || !ftbls_invt_mask[jf - 1]) {
          if (coldCurve) {
            fprnt = (ftbls[jf - 1] + coldCurve[jx - 1]) * fconv;
          }
          else {
            fprnt = ftbls[jf - 1] * fconv;
          }
          if (logAxes)
            fprnt = pow (10.0, fprnt);
          _eos_dbl2String (fprnt, _MIN_FIELD_WIDTH, buffer);
          fprintf (tableFile, "%*s", _MIN_FIELD_WIDTH + 1, buffer);
        }
        else {
	  sprintf(buffer, "---");
          fprintf (tableFile, "%*s", _MIN_FIELD_WIDTH + 1, buffer);
        }
      }
      fprintf (tableFile, "\n");
    }
  }
  fclose (tableFile);

  if (ftbls_invt_mask)
    EOS_FREE (ftbls_invt_mask);
  EOS_FREE (xtbl_new);
  EOS_FREE (ytbl_new);
  EOS_FREE (ftbl_new);
}

/***********************************************************************/
/*! 
 * \brief This packs the data of class eos_RecordType1 into provided char array
 * 
 * \param[out]    *err         - EOS_INTEGER : error code
 * \param[out]    *packedTable - EOS_CHAR    : allocated by user char array
 *                                             large enough to store packed data
 * \param[in]     *ptr         - void        : data object pointer;
 *                                             internally recast to eos_RecordType1*
 * \param[in]     th           - EOS_INTEGER : table handle
 * 
 * \return none
 *
 ***********************************************************************/
void eos_GetPackedTableRecordType1 (void *ptr, EOS_INTEGER th,
                                    EOS_CHAR *packedTable, EOS_INTEGER *err)
{
  eos_RecordType1 *me;
  eos_RecordType2 *tbl401 = NULL;
  EOS_INTEGER t, size401, byteCount = 0, tmpINT;
  EOS_REAL *F, *coldCurve;
  *err = EOS_OK;

  me = (eos_RecordType1 *) ptr;

  memcpy (packedTable + byteCount, &(me->avgAtomicNumber), sizeof (EOS_REAL));
  byteCount += sizeof (EOS_REAL);
  memcpy (packedTable + byteCount, &(me->avgAtomicWgt), sizeof (EOS_REAL));
  byteCount += sizeof (EOS_REAL);
  memcpy (packedTable + byteCount, &(me->refDensity), sizeof (EOS_REAL));
  byteCount += sizeof (EOS_REAL);
  memcpy (packedTable + byteCount, &(me->solidBulkModulus), sizeof (EOS_REAL));
  byteCount += sizeof (EOS_REAL);
  memcpy (packedTable + byteCount, &(me->exchangeCoefficient), sizeof (EOS_REAL));
  byteCount += sizeof (EOS_REAL);

  memcpy (packedTable + byteCount, &(me->isMonotonicX1),
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (packedTable + byteCount, &(me->isMonotonicX2),
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (packedTable + byteCount, &(me->isMonotonicX3),
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (packedTable + byteCount, &(me->isMonotonicX4),
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (packedTable + byteCount, &(me->shouldBeMonotonicX1),
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (packedTable + byteCount, &(me->shouldBeMonotonicX2),
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (packedTable + byteCount, &(me->shouldBeMonotonicX3),
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (packedTable + byteCount, &(me->shouldBeMonotonicX4),
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (packedTable + byteCount, &(me->isMonotonicY1),
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (packedTable + byteCount, &(me->isMonotonicY2),
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (packedTable + byteCount, &(me->isMonotonicY3),
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (packedTable + byteCount, &(me->isMonotonicY4),
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (packedTable + byteCount, &(me->shouldBeMonotonicY1),
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (packedTable + byteCount, &(me->shouldBeMonotonicY2),
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (packedTable + byteCount, &(me->shouldBeMonotonicY3),
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (packedTable + byteCount, &(me->shouldBeMonotonicY4),
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (packedTable + byteCount, &(me->shouldBeSmooth1),
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (packedTable + byteCount, &(me->shouldBeSmooth2),
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (packedTable + byteCount, &(me->shouldBeSmooth3),
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (packedTable + byteCount, &(me->shouldBeSmooth4),
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (packedTable + byteCount, &(me->shouldBePtSmooth1),
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (packedTable + byteCount, &(me->shouldBePtSmooth2),
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (packedTable + byteCount, &(me->shouldBePtSmooth3),
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (packedTable + byteCount, &(me->shouldBePtSmooth4),
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);

  memcpy (packedTable + byteCount, &(me->NR), sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (packedTable + byteCount, &(me->NT), sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);

  memcpy (packedTable + byteCount, me->R, sizeof (EOS_REAL) * me->NR);
  byteCount += sizeof (EOS_REAL) * me->NR;
  memcpy (packedTable + byteCount, me->T, sizeof (EOS_REAL) * me->NT);
  byteCount += sizeof (EOS_REAL) * me->NT;
  memcpy (packedTable + byteCount, &(me->eosData.numSubtablesLoaded),
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (packedTable + byteCount, &(me->eosData.coldCurveIsLoaded),
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);

  memcpy (packedTable + byteCount, &(me->isInvertedAtSetup),
          sizeof (EOS_BOOLEAN));
  byteCount += sizeof (EOS_BOOLEAN);

  /* pack extrapolation bounds for RT1 */
  memcpy (packedTable + byteCount, &(me->extrapolationBounds.stored), sizeof (EOS_BOOLEAN));
  byteCount += sizeof (EOS_BOOLEAN);

  if (me->extrapolationBounds.stored) {
    memcpy (packedTable + byteCount, &(me->extrapolationBounds.nx), sizeof (EOS_INTEGER));
    byteCount += sizeof (EOS_INTEGER);
    memcpy (packedTable + byteCount, &(me->extrapolationBounds.ny), sizeof (EOS_INTEGER));
    byteCount += sizeof (EOS_INTEGER);

    tmpINT = MAX(me->extrapolationBounds.nx, me->extrapolationBounds.ny);
    if (tmpINT > 1) { /* not CATEGORY0 */
      memcpy (packedTable + byteCount, me->extrapolationBounds.x, tmpINT * sizeof (EOS_REAL));
      byteCount += tmpINT * sizeof (EOS_REAL);
    }

    memcpy (packedTable + byteCount, me->extrapolationBounds.xLo, me->extrapolationBounds.nx * sizeof (EOS_REAL));
    byteCount += me->extrapolationBounds.nx * sizeof (EOS_REAL);

    memcpy (packedTable + byteCount, me->extrapolationBounds.yLo, me->extrapolationBounds.ny * sizeof (EOS_REAL));
    byteCount += me->extrapolationBounds.ny * sizeof (EOS_REAL);

    memcpy (packedTable + byteCount, me->extrapolationBounds.xHi, me->extrapolationBounds.nx * sizeof (EOS_REAL));
    byteCount += me->extrapolationBounds.nx * sizeof (EOS_REAL);

    memcpy (packedTable + byteCount, me->extrapolationBounds.yHi, me->extrapolationBounds.ny * sizeof (EOS_REAL));
    byteCount += me->extrapolationBounds.ny * sizeof (EOS_REAL);
  }

  /* pack ALL subtables, to be consistent with Load() for RT1 */
  for (t = 1; t <= MAX_TABLES_RECORDTYPE1; t++) {
    if (t > me->eosData.numSubtablesLoaded)
      break;

    switch (t) {
    case 1:
      F = &(me->table1[0][0]);
      coldCurve = me->coldCurve1;
      break;
    case 2:
      F = &(me->table2[0][0]);
      coldCurve = me->coldCurve2;
      break;
    case 3:
      F = &(me->table3[0][0]);
      coldCurve = me->coldCurve3;
      break;
    case 4:
      F = &(me->table4[0][0]);
      coldCurve = me->coldCurve4;
      break;
    }

    /* first pack the cold curves */
    if (me->eosData.coldCurveIsLoaded) {
      memcpy (packedTable + byteCount, coldCurve, me->NR * sizeof (EOS_REAL));
      byteCount += me->NR * sizeof (EOS_REAL);
    }

    /* now pack the subtable */
    memcpy (packedTable + byteCount, F, me->NT * me->NR * sizeof (EOS_REAL));
    byteCount += me->NT * me->NR * sizeof (EOS_REAL);

  }                             /* subtable loop */

  memcpy (packedTable + byteCount, &(me->eosData.dataFileIndex),
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (packedTable + byteCount, &(me->eosData.dataFileOffset),
          sizeof (long));
  byteCount += sizeof (long);
  memcpy (packedTable + byteCount, &(me->eosData.dataSize),
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);

  // pack the 401 table info here!
  memcpy (packedTable + byteCount, &(me->rt2_handle), sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  if (me->found_401) {
    tbl401 =
      (eos_RecordType2 *) gEosDataMap.dataObjects[gEosDataMap.
                                                  tableHandlesMap[me->
                                                                  rt2_handle]];

    /* now pack the table */
    eos_GetPackedTableSizeRecordType2 (tbl401, me->rt2_handle, &size401, err);
    eos_GetPackedTableRecordType2 (tbl401, me->rt2_handle,
                                   packedTable + byteCount, err);
    byteCount += size401;
  }
}


/***********************************************************************/
/*! 
 * \brief This sets the data of class eos_RecordType1 from the packed char array provided by caller
 * 
 * \param[out]   *err            - EOS_INTEGER : error code
 * \param[in]    *ptr            - void : data object pointer;
 *                                        internally recast to eos_RecordType1*
 * \param[in]    th              - EOS_INTEGER : table handle
 * \param[in]    packedTableSize - EOS_INTEGER : size in chars of packed data array
 * \param[in]    *packedTable    - EOS_CHAR : allocated by user char array large enough to store packed data
 * 
 * \return none
 *
 ***********************************************************************/
void eos_SetPackedTableRecordType1 (void *ptr, EOS_INTEGER th,
                                    EOS_CHAR *packedTable, EOS_INTEGER *err)
{
  eos_RecordType1 *me;
  EOS_INTEGER dataType, nt, nr, byteCount =
    0, tableNum, t, j, dt, tmpINT;
  EOS_REAL *F, *coldCurve = NULL;
  eos_RecordType2 *tbl401 = NULL;
  EOS_INTEGER size401;
  *err = EOS_OK;

  me = (eos_RecordType1 *) ptr;

  dataType = eos_GetDataTypeFromTableHandle (th, err);
  if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) != EOS_OK)
    return;

  tableNum = EOS_TYPE_TO_TAB_NUM (dataType);

  memcpy (&(me->avgAtomicNumber), packedTable + byteCount, sizeof (EOS_REAL));
  byteCount += sizeof (EOS_REAL);
  memcpy (&(me->avgAtomicWgt), packedTable + byteCount, sizeof (EOS_REAL));
  byteCount += sizeof (EOS_REAL);
  memcpy (&(me->refDensity), packedTable + byteCount, sizeof (EOS_REAL));
  byteCount += sizeof (EOS_REAL);
  memcpy (&(me->solidBulkModulus), packedTable + byteCount, sizeof (EOS_REAL));
  byteCount += sizeof (EOS_REAL);
  memcpy (&(me->exchangeCoefficient), packedTable + byteCount, sizeof (EOS_REAL));
  byteCount += sizeof (EOS_REAL);

  memcpy (&(me->isMonotonicX1), packedTable + byteCount,
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (&(me->isMonotonicX2), packedTable + byteCount,
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (&(me->isMonotonicX3), packedTable + byteCount,
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (&(me->isMonotonicX4), packedTable + byteCount,
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (&(me->shouldBeMonotonicX1), packedTable + byteCount,
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (&(me->shouldBeMonotonicX2), packedTable + byteCount,
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (&(me->shouldBeMonotonicX3), packedTable + byteCount,
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (&(me->shouldBeMonotonicX4), packedTable + byteCount,
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (&(me->isMonotonicY1), packedTable + byteCount,
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (&(me->isMonotonicY2), packedTable + byteCount,
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (&(me->isMonotonicY3), packedTable + byteCount,
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (&(me->isMonotonicY4), packedTable + byteCount,
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (&(me->shouldBeMonotonicY1), packedTable + byteCount,
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (&(me->shouldBeMonotonicY2), packedTable + byteCount,
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (&(me->shouldBeMonotonicY3), packedTable + byteCount,
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (&(me->shouldBeMonotonicY4), packedTable + byteCount,
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (&(me->shouldBeSmooth1), packedTable + byteCount,
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (&(me->shouldBeSmooth2), packedTable + byteCount,
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (&(me->shouldBeSmooth3), packedTable + byteCount,
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (&(me->shouldBeSmooth4), packedTable + byteCount,
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (&(me->shouldBePtSmooth1), packedTable + byteCount,
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (&(me->shouldBePtSmooth2), packedTable + byteCount,
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (&(me->shouldBePtSmooth3), packedTable + byteCount,
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (&(me->shouldBePtSmooth4), packedTable + byteCount,
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);

  memcpy (&nr, packedTable + byteCount, sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (&nt, packedTable + byteCount, sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);

  /* allocate memory if more memory is needed */
  if (nr > me->NR || nt > me->NT)
    eos_SetSizeRecordType1 (me, nr, nt, tableNum);
  me->NR = nr;
  me->NT = nt;

  memcpy (me->R, packedTable + byteCount, sizeof (EOS_REAL) * me->NR);
  byteCount += sizeof (EOS_REAL) * me->NR;
  memcpy (me->T, packedTable + byteCount, sizeof (EOS_REAL) * me->NT);
  byteCount += sizeof (EOS_REAL) * me->NT;
  memcpy (&(me->eosData.numSubtablesLoaded), packedTable + byteCount,
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (&(tmpINT), packedTable + byteCount, sizeof (EOS_INTEGER));
  me->eosData.coldCurveIsLoaded = (tmpINT != 0) ? EOS_TRUE : EOS_FALSE;
  byteCount += sizeof (EOS_INTEGER);

  memcpy (&(tmpINT), packedTable + byteCount, sizeof (EOS_INTEGER));
  me->isInvertedAtSetup = (tmpINT != 0) ? EOS_TRUE : EOS_FALSE;
  byteCount += sizeof (EOS_BOOLEAN);

  /* unpack extrapolation bounds for RT1 */
  memcpy (&(me->extrapolationBounds.stored), packedTable + byteCount, sizeof (EOS_BOOLEAN));
  byteCount += sizeof (EOS_BOOLEAN);

  if (me->extrapolationBounds.stored) {
    memcpy (&(me->extrapolationBounds.nx), packedTable + byteCount, sizeof (EOS_INTEGER));
    byteCount += sizeof (EOS_INTEGER);
    memcpy (&(me->extrapolationBounds.ny), packedTable + byteCount, sizeof (EOS_INTEGER));
    byteCount += sizeof (EOS_INTEGER);

    tmpINT = MAX(me->extrapolationBounds.nx, me->extrapolationBounds.ny);

    if (tmpINT > 1) { /* not CATEGORY0 */
      me->extrapolationBounds.x = (EOS_REAL*)malloc(tmpINT * sizeof(EOS_REAL));
      memcpy (me->extrapolationBounds.x, packedTable + byteCount, tmpINT * sizeof (EOS_REAL));
      byteCount += tmpINT * sizeof (EOS_REAL);
    }
    else {
      me->extrapolationBounds.x = NULL; /* CATEGORY0 */
    }

    me->extrapolationBounds.xLo = (EOS_REAL*)malloc(me->extrapolationBounds.nx * sizeof(EOS_REAL));
    memcpy (me->extrapolationBounds.xLo, packedTable + byteCount, me->extrapolationBounds.nx * sizeof (EOS_REAL));
    byteCount += me->extrapolationBounds.nx * sizeof (EOS_REAL);

    me->extrapolationBounds.yLo = (EOS_REAL*)malloc(me->extrapolationBounds.ny * sizeof(EOS_REAL));
    memcpy (me->extrapolationBounds.yLo, packedTable + byteCount, me->extrapolationBounds.ny * sizeof (EOS_REAL));
    byteCount += me->extrapolationBounds.ny * sizeof (EOS_REAL);

    me->extrapolationBounds.xHi = (EOS_REAL*)malloc(me->extrapolationBounds.nx * sizeof(EOS_REAL));
    memcpy (me->extrapolationBounds.xHi, packedTable + byteCount, me->extrapolationBounds.nx * sizeof (EOS_REAL));
    byteCount += me->extrapolationBounds.nx * sizeof (EOS_REAL);

    me->extrapolationBounds.yHi = (EOS_REAL*)malloc(me->extrapolationBounds.ny * sizeof(EOS_REAL));
    memcpy (me->extrapolationBounds.yHi, packedTable + byteCount, me->extrapolationBounds.ny * sizeof (EOS_REAL));
    byteCount += me->extrapolationBounds.ny * sizeof (EOS_REAL);
  }

  /* unpack ALL subtables, to be consistent with Load() for RT1 */
  for (t = 1; t <= me->eosData.numSubtablesLoaded; t++) {
    if (t > me->eosData.numSubtablesLoaded)
      break;

    switch (t) {
    case 1:
      F = &(me->table1[0][0]);
      coldCurve = (me->eosData.coldCurveIsLoaded) ? me->coldCurve1 : NULL;
      if (! me->eosData.coldCurveIsLoaded) /* Cold curve is not stored separately */
	EOS_FREE(me->coldCurve1);
      break;
    case 2:
      F = &(me->table2[0][0]);
      coldCurve = (me->eosData.coldCurveIsLoaded) ? me->coldCurve2 : NULL;
      if (! me->eosData.coldCurveIsLoaded) /* Cold curve is not stored separately */
	EOS_FREE(me->coldCurve2);
      break;
    case 3:
      F = &(me->table3[0][0]);
      coldCurve = (me->eosData.coldCurveIsLoaded) ? me->coldCurve3 : NULL;
      if (! me->eosData.coldCurveIsLoaded) /* Cold curve is not stored separately */
	EOS_FREE(me->coldCurve3);
      break;
    case 4:
      F = &(me->table4[0][0]);
      coldCurve = (me->eosData.coldCurveIsLoaded) ? me->coldCurve4 : NULL;
      if (! me->eosData.coldCurveIsLoaded) /* Cold curve is not stored separately */
	EOS_FREE(me->coldCurve4);
      break;
    }

    /* first unpack the cold curves */
    if (me->eosData.coldCurveIsLoaded) {
      memcpy (coldCurve, packedTable + byteCount, me->NR * sizeof (EOS_REAL));
      byteCount += me->NR * sizeof (EOS_REAL);
    }
    else if (coldCurve)
      for (j = 0; j < me->NR; j++)
        coldCurve[j] = ZERO;

    /* now unpack the subtable */
    memcpy (F, packedTable + byteCount, me->NT * me->NR * sizeof (EOS_REAL));
    byteCount += me->NT * me->NR * sizeof (EOS_REAL);

  }                             /* subtables loop */

  memcpy (&(me->eosData.dataFileIndex), packedTable + byteCount,
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (&(me->eosData.dataFileOffset), packedTable + byteCount,
          sizeof (long));
  byteCount += sizeof (long);
  memcpy (&(me->eosData.dataSize), packedTable + byteCount,
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  me->eosData.isLoaded = (me->eosData.numSubtablesLoaded > 0);
  // unpack the 401 table info here!
  // unpack handle if you can, does the handle stay the same?
  memcpy (&(me->rt2_handle), packedTable + byteCount, sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  me->found_401 = (me->rt2_handle >= 0) ? EOS_TRUE : EOS_FALSE;
  if (me->found_401) {
    /* create empty 401 table, fill it with unpacked data */
    dt = EOS_401_DATA;
    /* first construct an empty tbl w/o creating it (in case the table can't be created, we still want an empty RT2 */
    eos_CreateTablesEosDataMap (&gEosDataMap, 1, &dt, &me->eosData.materialID,
                                &(me->rt2_handle), EOS_FALSE, 0, EOS_FALSE, -1, err);
    if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) == EOS_OK && me->rt2_handle >= 0
        && gEosDataMap.tableHandlesMap[me->rt2_handle]) {
      tbl401 =
        (eos_RecordType2 *) gEosDataMap.dataObjects[gEosDataMap.
                                                    tableHandlesMap[me->
                                                                    rt2_handle]];

      /* now fill the table */
      eos_SetPackedTableRecordType2 (tbl401, me->rt2_handle,
                                     packedTable + byteCount, err);
      eos_GetPackedTableSizeRecordType2 (tbl401, me->rt2_handle, &size401,
                                         err);
      byteCount += size401;
    }
    else {                      /* couldn't create a new handle */

      me->rt2_handle = -1;
      me->found_401 = EOS_FALSE;
      *err = EOS_UNDEFINED;
      *err = eos_SetCustomErrorMsg (th, *err,
				    "eos_RecordType1::SetPackedTable ERROR couldn't create a handle for 401 table");
    }
  }
}

/***********************************************************************/
/*! 
 * \brief This returns the size of packed char array needed to store the data of class eos_RecordType1
 * 
 * \param[out]   *err             - EOS_INTEGER : error code
 * \param[in]    *ptr             - void : data object pointer;
 *                                         internally recast to eos_RecordType1*
 * \param[in]    *packedTableSize - EOS_INTEGER : size in chars of packed data array
 * \param[in]    th               - EOS_INTEGER : table handle
 * 
 * \return none
 *
 ***********************************************************************/
void eos_GetPackedTableSizeRecordType1 (void *ptr, EOS_INTEGER th,
                                        EOS_INTEGER *packedTableSize,
                                        EOS_INTEGER *err)
{
  eos_RecordType1 *me;
  eos_RecordType2 *tbl401 = NULL;
  EOS_INTEGER size401, byteCount = 0, tmpINT;
  *err = EOS_OK;

  me = (eos_RecordType1 *) ptr;

  byteCount += sizeof (EOS_REAL) * 5;
  byteCount += sizeof (EOS_INTEGER) * 26;
  byteCount += sizeof (EOS_REAL) * me->NR;
  byteCount += sizeof (EOS_REAL) * me->NT;
  byteCount += sizeof (EOS_INTEGER);    /* number of subtable loaded */
  byteCount += sizeof (EOS_INTEGER);    /* if cold curve is loaded */

  byteCount += sizeof (EOS_BOOLEAN);    /* isInvertedAtSetup */
  byteCount += sizeof (EOS_BOOLEAN);    /* extrapolationBounds.stored */
  if (me->extrapolationBounds.stored) {
    byteCount += sizeof (EOS_INTEGER) * 2; /* extrapolationBounds.nx and extrapolationBounds.ny */
    tmpINT = MAX(me->extrapolationBounds.nx, me->extrapolationBounds.ny);
    if (tmpINT > 1) { /* not CATEGORY0 */
      byteCount += tmpINT * sizeof (EOS_REAL); /* extrapolationBounds.x[] array */
    }
    byteCount += me->extrapolationBounds.nx * sizeof (EOS_REAL); /* extrapolationBounds.xLo[] array */
    byteCount += me->extrapolationBounds.ny * sizeof (EOS_REAL); /* extrapolationBounds.xLo[] array */
    byteCount += me->extrapolationBounds.nx * sizeof (EOS_REAL); /* extrapolationBounds.xHi[] array */
    byteCount += me->extrapolationBounds.ny * sizeof (EOS_REAL); /* extrapolationBounds.yHi[] array */
  }

  byteCount += sizeof (EOS_INTEGER);    /* 401 handle */

  byteCount += me->eosData.numSubtablesLoaded * me->NT * me->NR * sizeof (EOS_REAL);

  if (me->eosData.coldCurveIsLoaded)
    byteCount += me->eosData.numSubtablesLoaded * me->NR * sizeof (EOS_REAL);   /* cold curves for each subtable */
  byteCount += (sizeof (EOS_INTEGER) * 2 + sizeof (long));

  if (me->found_401) {
    tbl401 =
      (eos_RecordType2 *) gEosDataMap.dataObjects[gEosDataMap.
                                                  tableHandlesMap[me->
                                                                  rt2_handle]];
    eos_GetPackedTableSizeRecordType2 (tbl401, me->rt2_handle, &size401, err);
    byteCount += size401;
  }

  *packedTableSize = byteCount;
}

/***********************************************************************/
/*!
 * \brief Function _eos_InterpolateRecordType1 (helping function for
 *  eos_InterpolateRecordType1().
 *
 * \param[out]   fVals[nXYPairs] - EOS_REAL : array of the interpolated data corresponding
 *                                            to x and y.
 * \param[out]   dFx[nXYPairs]   - EOS_REAL : array of the interpolated partial derivatives
 *                                            of fVals with respect to x.
 * \param[out]   dFy[nXYPairs]   - EOS_REAL : array of the interpolated partial derivatives
 *                                            of fVals with respect to y.
 * \param[out]   dFCx[nXYPairs]  - EOS_REAL : optional array of the interpolated partial
 *                                            derivatives of cold curve with respect to x.
 * \param[out]   dFx0[nXYPairs]  - EOS_REAL : optional array of the interpolated partial
 *                                            derivatives of cat 0 F wrt x (for inverse
 *                                            functions mostly).
 * \param[out]   dFy0[nXYPairs]  - EOS_REAL : optional array of the interpolated partial
 *                                            derivatives of cat 0 F wrt y (for inverse
 *                                            functions mostly).
 * \param[out]   *xyBounds       - EOS_INTEGER : interpolation errors per xy-pair
 * \param[out]   errorCode       - EOS_INTEGER : error code of the interpolation:
 *                                               EOS_INTERP_EXTRAPOLATED or EOS_OK
 * \param[in]    *ptr            - void : data object pointer;
 *                                        internally recast to eos_RecordType1*
 * \param[in]    th              - EOS_INTEGER : table Handle
 * \param[in]    varOrder        - EOS_INTEGER : order of variables
 * \param[in]    dataType        - EOS_INTEGER : dataType
 * \param[in]    nXYPairs        - EOS_INTEGER : total number of pairs of independent variable values provided for interpolation.
 * \param[in]    srchX[nXYPairs] - EOS_REAL : array of the primary independent variable values to use during interpolation.
 * \param[in]    srchY[nXYPairs] - EOS_REAL : array of the secondary independent variable values to use during interpolation.
 *
 * \return none
 *
 ***********************************************************************/
void _eos_InterpolateRecordType1 (void *ptr, EOS_INTEGER th,
				  EOS_INTEGER varOrder, EOS_INTEGER dataType,
                                  EOS_INTEGER nXYPairs, EOS_REAL *srchX,
                                  EOS_REAL *srchY, EOS_REAL *fVals,
                                  EOS_REAL *dFx, EOS_REAL *dFy,
                                  EOS_REAL *dFCx, EOS_REAL *dFx0,
                                  EOS_REAL *dFy0, EOS_INTEGER *xyBounds,
                                  EOS_INTEGER *errorCode)
{
  /* macros to enable debugging output in this function;
     used to make algorithm logic more easily read */
  //#define _EOS_DUMP_INDEX_DATA_INTERPOLATE_RECORDTYPE1

  EOS_INTEGER i, ind, doRational = 0,
    err, tabInd1, tabInd2, nX, nY, cat, subTableNum, subTableNum2, tableNum, *xyBounds2;
  EOS_REAL *X, *Y, **F, **F2, *uVals, *dUy, *dUx, *dFCx1, *dUCx, *dYx, *dXy,
    *coldCurve, *newColdCurve = NULL, *newColdCurve_dFdx =
    NULL, *newFVals, *xVals, *yVals, *null_val = NULL;
  eos_Data *eosData;
  eos_RecordType1 *me;
  EOS_BOOLEAN isPtSmooth = EOS_FALSE, useCustomInterp = EOS_FALSE;
  EOS_INTEGER nGhostData=0;

  EOS_REAL *xtbls, *ytbls, **ftbls; /* temporary arrays to include ghost node data */
  EOS_BOOLEAN optVal = EOS_FALSE;
  EOS_INTEGER nxtbl, nytbl;
  EOS_CHAR *errMsg = NULL;

#ifdef _EOS_DUMP_INDEX_DATA_INTERPOLATE_RECORDTYPE1
  char fn[50] = "eos_RationalInterpolate_RecordType1.indices";
  char *appendMode = "a";
  static char *mode = "w";
#endif

  *errorCode = EOS_OK;

  if (nXYPairs <= 0) {
    *errorCode = EOS_FAILED;
    return;
  }

  err = EOS_OK;
  eosData = (eos_Data *) ptr;
  me = (eos_RecordType1 *) eosData;

  if (me->isInvertedAtSetup) {
    cat = EOS_INVERTED_AT_SETUP;
    subTableNum = 1;
  }
  else {
    cat = EOS_CATEGORY (dataType);
    subTableNum = EOS_TYPE_TO_SUB_TAB_NUM (dataType);
  }
  /* get the size of the data */
  eos_GetSizeRecordType1 (me, &nX, &nY);
  /* note, when the data is log 10 - cold curve, we need to take the indep. var into this form when inverting */
  tableNum = EOS_TYPE_TO_TAB_NUM (dataType);

  if (me->eosData.numSubtablesLoaded < subTableNum) {
    *errorCode = EOS_DATA_TYPE_NOT_FOUND;
    ((eos_ErrorHandler *) me)->HandleError (me, th, *errorCode);
    return;
  }

  /* Fetch data array pointers */
  _eos_GetDataRecordType1 (me, &X, &Y, &F, &coldCurve, subTableNum);

  /* initialize F, dFx, dFy to zero */
  for (i = 0; i < nXYPairs; i++) {
    fVals[i] = ZERO;
    dFx[i] = ZERO;
    dFy[i] = ZERO;
    if (dFCx)
      dFCx[i] = ZERO;
    if (dFx0)
      dFx0[i] = ZERO;
    if (dFy0)
      dFy0[i] = ZERO;
  }

  /* Use custom interpolator if EOS_PT_SMOOTHING is set for th */
  isPtSmooth =
    eos_getBoolOptionFromTableHandle (th, EOS_PT_SMOOTHING, errorCode);
  if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) != EOS_OK) {
    ((eos_ErrorHandler *) me)->HandleError (me, th, *errorCode);
    return;
  }
  useCustomInterp =
    eos_getBoolOptionFromTableHandle (th, EOS_USE_CUSTOM_INTERP, errorCode);
  if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) != EOS_OK) {
    ((eos_ErrorHandler *) me)->HandleError (me, th, *errorCode);
    return;
  }
  if (isPtSmooth && useCustomInterp &&
      (dataType == EOS_V_PtT || dataType == EOS_Ut_PtT)) {
    _eos_SesameInvTRecordType1 (me, dataType, nXYPairs, srchX, srchY,
                                fVals, dFx, dFy, xyBounds, errorCode, &errMsg);
    if (errMsg) err = eos_SetCustomErrorMsg(th, err, "%s", errMsg);
    EOS_FREE(errMsg);
    if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) != EOS_OK)
      ((eos_ErrorHandler *) me)->HandleError (me, th, *errorCode);
    return;
  }

  /* get EOS_DISABLE_GHOST_NODES setting for this handle */
  eos_GetOptionEosInterpolation (&gEosInterpolation, th, EOS_DISABLE_GHOST_NODES, &optVal, errorCode);
  if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) != EOS_OK)  {
    ((eos_ErrorHandler *) me)->HandleError (me, th, *errorCode);
    return;
  }

  doRational = eos_getBoolOptionFromTableHandle (th, EOS_RATIONAL, &err);

  if (! optVal && doRational && (nX > 1) && (nY > 1)/*  && (cat != EOS_CATEGORY0) */) {
    /* add "ghost node" data prior to interpolation */
    nGhostData = 1;
    if (EOS_TYPE_TO_INDEP_VAR1 (dataType) != EOS_NullTable
	&& EOS_TYPE_TO_INDEP_VAR2 (dataType) == EOS_NullTable) {
      /* use a NULL pointer for Y input array if operating on a 1-D table */
      _eos_CreateGhostData (nGhostData, nX, nY, X, NULL, F, NULL,
			    &nxtbl, &nytbl, &xtbls, &ytbls, &ftbls, NULL, &err, &errMsg);
      if (errMsg) err = eos_SetCustomErrorMsg(th, err, "%s", errMsg);
      EOS_FREE(errMsg);
    }
    else {
      /* operating on a 2-D table */
      _eos_CreateGhostData (nGhostData, nX, nY, X, Y, F, NULL,
			    &nxtbl, &nytbl, &xtbls, &ytbls, &ftbls, NULL, &err, &errMsg);
      if (errMsg) err = eos_SetCustomErrorMsg(th, err, "%s", errMsg);
      EOS_FREE(errMsg);
    }
    if (eos_GetStandardErrorCodeFromCustomErrorCode(err) != EOS_OK)  {
      nGhostData = 0;
      ((eos_ErrorHandler *) me)->HandleError (me, th, err);
      return;
    }
    nX = nxtbl;
    nY = nytbl;
    X = xtbls;
    Y = ytbls;
    F = ftbls;
  }


  /* Check if 1-D interpolation is required by dataType */
  if (EOS_TYPE_TO_INDEP_VAR1 (dataType) != EOS_NullTable
      && EOS_TYPE_TO_INDEP_VAR2 (dataType) == EOS_NullTable) {

    /* fetch cross-reference data if category 3 interpolation */
    if (cat == EOS_CATEGORY3) {
      ind = EOS_EOS_TABLE_TYPE_REF2(dataType);
      subTableNum2 = EOS_TYPE_TO_SUB_TAB_NUM (ind);
      _eos_GetDataRecordType1 (me, &X, &Y, &F2, &coldCurve, subTableNum2);
    }
    else
      F2 = &null_val;

    /* interpolate */
    _eos_InterpolateRecordType_1D (nX, X, *F, *F2, th, dataType,
                                   nXYPairs, srchX, fVals, dFx, xyBounds,
                                   errorCode, &errMsg);
    if (errMsg) *errorCode = eos_SetCustomErrorMsg(th, *errorCode, "%s", errMsg);
    EOS_FREE(errMsg);
    if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) != EOS_OK)
      ((eos_ErrorHandler *) me)->HandleError (me, th, *errorCode);
    return;
  }
  else if (EOS_TYPE_TO_INDEP_VAR1 (dataType) == EOS_NullTable
           && EOS_TYPE_TO_INDEP_VAR2 (dataType) != EOS_NullTable) {

    /* fetch cross-reference data if category 3 interpolation */
    if (cat == EOS_CATEGORY3) {
      ind = EOS_EOS_TABLE_TYPE_REF2(dataType);
      subTableNum2 = EOS_TYPE_TO_SUB_TAB_NUM (ind);
      _eos_GetDataRecordType1 (me, &X, &Y, &F2, &coldCurve, subTableNum2);
      _eos_GetDataRecordType1 (me, &X, &Y, &F2, &coldCurve, subTableNum);
    }
    else
      F2 = &null_val;

    /* interpolate */
    _eos_InterpolateRecordType_1D (nY, Y, *F, *F2, th, dataType,
                                   nXYPairs, srchX, fVals, dFx, xyBounds,
                                   errorCode, &errMsg);
    if (errMsg) *errorCode = eos_SetCustomErrorMsg(th, *errorCode, "%s", errMsg);
    EOS_FREE(errMsg);
    if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) != EOS_OK)
      ((eos_ErrorHandler *) me)->HandleError (me, th, *errorCode);
    return;
  }


  /* Perform 2-D interpolation as required by dataType */
  xVals = srchX;
  yVals = srchY;

  switch (cat) {
  case EOS_CATEGORY0:          /* indicates the table is not inverted */
  case EOS_INVERTED_AT_SETUP:  /* indicates the table is already inverted during setup */
    {

      if (cat != EOS_INVERTED_AT_SETUP) {

	/* send in the first row = cold curve */
	if ((tableNum == 301 || tableNum == 303) && coldCurve) {
	  /* interpolate cold curve at the given density */
	  newColdCurve = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
	  newColdCurve_dFdx =
	    (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
	  _eos_InterpolateRecordType_1D (nX, X, coldCurve, NULL, th, dataType,
					 nXYPairs, xVals, newColdCurve, newColdCurve_dFdx,
					 xyBounds, &err, &errMsg);
	  if (errMsg) err = eos_SetCustomErrorMsg(th, err, "%s", errMsg);
	  EOS_FREE(errMsg);
	  for (i = 0; i < nXYPairs; i++) {
	    if (xyBounds[i] == EOS_UNDEFINED) {
	      newColdCurve[i] = coldCurve[0];
	      newColdCurve_dFdx[i] = ZERO;
	      err = EOS_OK;
	    }
	  }
	}

      }

      if (doRational) {

	eos_RationalInterpolateXY (nXYPairs, xVals, yVals, dFx, dFy, fVals,
				   nX, nY, X, Y, F, coldCurve, nGhostData, xyBounds, &err);

      }
      else if (eos_getBoolOptionFromTableHandle (th, EOS_LINEAR, &err)) { /* interpolate linearly instead */
#ifdef _EOS_DUMP_INDEX_DATA_INTERPOLATE_RECORDTYPE1
	sprintf(fn, "eos_LinearInterpolate_RecordType1.indices");
	iyv = (EOS_INTEGER *) malloc (nXYPairs * sizeof (EOS_INTEGER));       /* indexes of Y near which Y points are */
	ixv = (EOS_INTEGER *) malloc (nXYPairs * sizeof (EOS_INTEGER));       /* indexes of X near which X points are */
	xyBounds2 = (EOS_INTEGER *) malloc (nXYPairs * sizeof (EOS_INTEGER)); /* xy-bounds for y */

	_eos_srchdf (nXYPairs, yVals, 1, nY - 1, Y, 1, iyv,
		     xyBounds, errorCode);
	_eos_srchdf (nXYPairs, xVals, 1, nX - 1, X, 1, ixv,
		     xyBounds2, errorCode);
	_eos_DumpIndicesToFile (fn, mode, nXYPairs, X, Y, F,
				ixv, iyv, NULL, NULL);
	EOS_FREE (xyBounds2);
	EOS_FREE (ixv);
	EOS_FREE (iyv);
#endif
        eos_BiLineInterpolate (eos_getBoolOptionFromTableHandle (th, EOS_DISCONTINUOUS_DERIVATIVES, &err),
			       nXYPairs, nX, nY, X, Y, F, xVals, yVals, fVals,
                               dFx, dFy, xyBounds, &err);
      }
      if (eos_GetStandardErrorCodeFromCustomErrorCode(err) != EOS_OK)
        *errorCode = err;
      if (eos_GetStandardErrorCodeFromCustomErrorCode(err) == EOS_UNDEFINED) {
        if ((tableNum == 301 || tableNum == 303) && newColdCurve) {
          EOS_FREE (newColdCurve);
          EOS_FREE (newColdCurve_dFdx);
        }
        break;
      }

      if (cat != EOS_INVERTED_AT_SETUP) {

	for (i = 0; i < nXYPairs; i++) {

	  /* the interpolated values are given as F-coldCurve, make them into F */
	  if ((tableNum == 301 || tableNum == 303) && newColdCurve) {
	    //OLgaLog10             fVals[i] = (tableNum == 301 || tableNum == 303)?  pow(10.0, fVals[i]) + newColdCurve[i] : pow(10.0, fVals[i]) ;
	    fVals[i] = fVals[i] + newColdCurve[i];
	    // DAP -- Add dFcold/dx to dF/dx since interpolator was only using thermal data
	    dFx[i] = dFx[i] + newColdCurve_dFdx[i];
	    if (dFCx)
	      dFCx[i] = newColdCurve_dFdx[i];
	  }
	  else if (dFCx)
	    dFCx[i] = ZERO;

	  if (dFx0)
	    dFx0[i] = dFx[i];
	  if (dFy0)
	    dFy0[i] = dFy[i];

	  if (xyBounds[i])
	    *errorCode = EOS_INTERP_EXTRAPOLATED;

	}

	if ((tableNum == 301 || tableNum == 303) && newColdCurve) {
	  EOS_FREE (newColdCurve);
	  EOS_FREE (newColdCurve_dFdx);
	}

      }
#ifdef _EOS_DUMP_INDEX_DATA_INTERPOLATE_RECORDTYPE1
      mode = appendMode; /* reset mode for indices output file */
#endif

      break;
    }
  case EOS_CATEGORY1:          /* indicates the table is inverted with respect to 1st independent variable */
    {
      /* means that the user provides y and F of a function that is defined as x( F,y)
         so effectively the x and y user has given us are F and Y */

      /* do inverse interpolation with respect to first var, x
         NOTE: dFx and dFx are dF/dx and dF/dy for category 0 table,
	 so they must be transformed below. */
      if (doRational) {
        err = EOS_OK;
        eos_InverseRationalInterpolateFY (nXYPairs, yVals, xVals, dFx, dFy,
                                          fVals, nX, nY, X, Y, F, coldCurve, nGhostData,
                                          xyBounds, &err, &errMsg);
	if (errMsg) err = eos_SetCustomErrorMsg(th, err, "%s", errMsg);
	EOS_FREE(errMsg);
      }
      else if (eos_getBoolOptionFromTableHandle (th, EOS_LINEAR, &err)) /* interpolate linearly instead */
        eos_InverseBilinearInterpolateFY (nXYPairs, yVals, xVals, dFx, dFy,
                                          fVals, nX, nY, X, Y, F, coldCurve,
                                          xyBounds, &err);
      if (eos_GetStandardErrorCodeFromCustomErrorCode(err) != EOS_OK)
        *errorCode = err;
      if (eos_GetStandardErrorCodeFromCustomErrorCode(err) == EOS_UNDEFINED)
        break;

      for (i = 0; i < nXYPairs; i++) {

	/* if EOS_CANT_INVERT_DATA, then potential div-by-zero */
	if (xyBounds[i] == EOS_CANT_INVERT_DATA) {
	  dFx[i] = dFy[i] = fVals[i] = 1.0e99;
	  continue;
	}

        /* optionally return dF/dx and dF/dy  for later use to
	   transform partial derivatives */
        if (dFx0)
          dFx0[i] = dFx[i];
        if (dFy0)
          dFy0[i] = dFy[i];

	/* DAP -- Test multiplication with EOS_CHECK_PRODUCT prior to performing it.
	   This is a temporary kludge to prevent SIGFPE. */
	/* dFy in this case needs to be transformed to dx/dy */
	if (EOS_CHECK_PRODUCT(dFy[i], -ONE/dFx[i]))
	  dFy[i] =(-ONE/dFx[i])*dFy[i];
	else
	  dFy[i] = SIGN(dFy[i]) * SIGN(-ONE/dFx[i]) *
	    (EOS_IS_PRODUCT_GT_MAX(dFy[i], -ONE/dFx[i]) ? DBL_MAX : DBL_MIN);

	/*calculate dx/dF*/
	dFx[i] = ONE / dFx[i];

        /* compute EOS_V_PtT differently: v = 1/r, dv/dP = -(1/r^2) * dr/dP = -1/xVals[i]^2 * dFx[i] */
        if (dataType == EOS_V_PtT) {
          fVals[i] = ONE / MAX (TINY_D, fVals[i]);

	  /* DAP -- Test multiplication with EOS_CHECK_PRODUCT prior to performing it.
	     This is a temporary kludge to prevent SIGFPE. */
	  if (EOS_CHECK_PRODUCT(fVals[i], fVals[i]) &&
	      EOS_CHECK_PRODUCT(fVals[i]*fVals[i],dFx[i]))
	    dFx[i] = -ONE * pow (fVals[i], 2.0) * dFx[i];
	  else
	    dFx[i] = -ONE * SIGN(dFx[i]) *
	      (EOS_IS_PRODUCT_GT_MAX(fVals[i], fVals[i]) ||
	       EOS_IS_PRODUCT_GT_MAX(fVals[i]*fVals[i],dFx[i]) ? DBL_MAX : DBL_MIN);
        }
      }
      break;
    }
  case EOS_CATEGORY2:          /* indicates the table is inverted with respect to 2nd independent variable */
    {
      /* do inverse interpolation with respect to second var */
      /* means that the user provides x and F of a function that is defined as y( x,F)
         so effectively the x and y user has given us are X and F */

      /* send in the first row = cold curve */
      if ((tableNum == 301 || tableNum == 303) && coldCurve) {
        /* interpolate cold curve given density */
        newColdCurve = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
        newColdCurve_dFdx =
          (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));

	if (doRational)
	  eos_RationalInterpolate (nXYPairs, nX, 1, 0, X, coldCurve, xVals,
				   newColdCurve, newColdCurve_dFdx, 'y',
				   xyBounds, &err);
	else if (eos_getBoolOptionFromTableHandle (th, EOS_LINEAR, &err)) { /* interpolate linearly instead */
	  eos_LineInterpolate (eos_getBoolOptionFromTableHandle (th, EOS_DISCONTINUOUS_DERIVATIVES, &err),
			       nXYPairs, nX, 1, 0, X, &coldCurve,
			       xVals, newColdCurve, newColdCurve_dFdx, 'y',
			       xyBounds, &err);
	  for (i = 0; i < nXYPairs; i++) {
	    if (xyBounds[i] == EOS_UNDEFINED) {
	      newColdCurve[i] = coldCurve[0];
	      newColdCurve_dFdx[i] = ZERO;
	      err = EOS_OK;
	    }
	  }
        }
      }

      /* need to take yVals which are in fact F-vals into the same form as stored in the table */
      newFVals = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
      for (i = 0; i < nXYPairs; i++)
        // OlgaLog10    newFVals[i] = (tableNum == 301 || tableNum == 303)?  log10(yVals[i] - newColdCurve[i]) : log10(yVals[i]);
        newFVals[i] = ((tableNum == 301 || tableNum == 303)
                       && newColdCurve) ? yVals[i] -
          newColdCurve[i] : yVals[i];

      /* do inverse interpolation with respect to second var */
      if (doRational) {
        err = EOS_OK;
        eos_InverseRationalInterpolateXF (nXYPairs, xVals, newFVals, dFx, dFy,
                                          fVals, nX, nY, X, Y, F, nGhostData, xyBounds,
                                          &err, &errMsg);
	if (errMsg) err = eos_SetCustomErrorMsg(th, err, "%s", errMsg);
	EOS_FREE(errMsg);
      }
      else if (eos_getBoolOptionFromTableHandle (th, EOS_LINEAR, &err)) /* interpolate linearly instead */
        eos_InverseBilinearInterpolateXF (nXYPairs, xVals, newFVals, dFx, dFy,
                                          fVals, nX, nY, X, Y, F, xyBounds,
                                          &err);
      if (eos_GetStandardErrorCodeFromCustomErrorCode(err) != EOS_OK)
        *errorCode = err;
      if (eos_GetStandardErrorCodeFromCustomErrorCode(err) == EOS_UNDEFINED) {
        if ((tableNum == 301 || tableNum == 303) && newColdCurve) {
          EOS_FREE (newColdCurve);
          EOS_FREE (newColdCurve_dFdx);
        }
        EOS_FREE (newFVals);
        break;
      }

      for (i = 0; i < nXYPairs; i++) {

	/* if EOS_CANT_INVERT_DATA, then potential div-by-zero */
	if (xyBounds[i] == EOS_CANT_INVERT_DATA) {
	  dFx[i] = dFy[i] = fVals[i] = 1.0e99;
	  continue;
	}

        /* optionally return dF/dx and dF/dy  for later use to
	   transform partial derivatives */
        if (dFy0)
          dFy0[i] = dFy[i];

	/* transform dFy from dF/dy to dy/dF */
	dFy[i] = ONE/dFy[i];

        if ((tableNum == 301 || tableNum == 303) && newColdCurve) {
          if (dFx0)
            dFx0[i] = dFx[i] + newColdCurve_dFdx[i];

	  /* DAP -- Test multiplication with EOS_CHECK_PRODUCT prior to performing it.
	     This is a temporary kludge to prevent SIGFPE. */
	  if (EOS_CHECK_PRODUCT(-(dFx[i] + newColdCurve_dFdx[i]), dFy[i]))
	    dFx[i] = -(dFx[i] + newColdCurve_dFdx[i]) * dFy[i];
	  else
	    dFx[i] = -(SIGN(dFx[i] + newColdCurve_dFdx[i])) * SIGN(dFy[i]) *
	      (EOS_IS_PRODUCT_GT_MAX(-(dFx[i] + newColdCurve_dFdx[i]), dFy[i]) ? DBL_MAX : DBL_MIN);

          if (dFCx)
            dFCx[i] = newColdCurve_dFdx[i];
        }
        else {
          if (dFx0)
            dFx0[i] = dFx[i];

	  /* DAP -- Test multiplication with EOS_CHECK_PRODUCT prior to performing it.
	     This is a temporary kludge to prevent SIGFPE. */
	  if (EOS_CHECK_PRODUCT(dFx[i], dFy[i]))
	    dFx[i] = -dFx[i] * dFy[i];
	  else
	    dFx[i] = -(SIGN(dFx[i])) * SIGN(dFy[i]) *
	      (EOS_IS_PRODUCT_GT_MAX(dFx[i], dFy[i]) ? DBL_MAX : DBL_MIN);

          if (dFCx)
            dFCx[i] = ZERO;
        }

      }

      if ((tableNum == 301 || tableNum == 303) && newColdCurve) {
        EOS_FREE (newColdCurve);
        EOS_FREE (newColdCurve_dFdx);
      }
      EOS_FREE (newFVals);
      break;
    }
  case EOS_CATEGORY4:          /* indicates the table is merged with another function */
    {
      /* get the data pointers and types for these variables */
      tabInd1 = EOS_EOS_TABLE_TYPE_REF1 (dataType);
      tabInd2 = EOS_EOS_TABLE_TYPE_REF2 (dataType);

      /* We need F(X,Y) given F(X, U) and U(X, Y) */
      /* 1. find U using second table, 2. find F using 1st table. */
      uVals = (EOS_REAL *) malloc (sizeof (EOS_REAL) * nXYPairs);
      dUy = (EOS_REAL *) malloc (sizeof (EOS_REAL) * nXYPairs);
      dYx = (EOS_REAL *) malloc (sizeof (EOS_REAL) * nXYPairs);
      dUCx = (EOS_REAL *) malloc (sizeof (EOS_REAL) * nXYPairs);
      dFCx1 = (EOS_REAL *) malloc (sizeof (EOS_REAL) * nXYPairs);
      xyBounds2 = (EOS_INTEGER *) malloc (nXYPairs * sizeof (EOS_INTEGER));

      _eos_InterpolateRecordType1 (eosData, th, X_U_F, tabInd2, nXYPairs,
                                   xVals, yVals, uVals, dFx /*dUx */ , dUy,
                                   dUCx, dYx, NULL, xyBounds, &err);
      if (eos_GetStandardErrorCodeFromCustomErrorCode(err) != EOS_OK)
        *errorCode = err;
      if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) == EOS_UNDEFINED) {
        EOS_FREE (dUy);
        EOS_FREE (uVals);
        EOS_FREE (dUCx);
        EOS_FREE (dFCx1);
        EOS_FREE (dYx);
        EOS_FREE (xyBounds2);
        break;
      }
      _eos_InterpolateRecordType1 (eosData, th, X_Y_U, tabInd1, nXYPairs,
                                   xVals, uVals, fVals, dFx, dFy /*dFu */ ,
                                   dFCx1, NULL, NULL, xyBounds2, &err);
      if (eos_GetStandardErrorCodeFromCustomErrorCode(err) != EOS_OK)
        *errorCode = err;
      if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) == EOS_UNDEFINED) {
        EOS_FREE (dUy);
        EOS_FREE (uVals);
        EOS_FREE (dUCx);
        EOS_FREE (dFCx1);
        EOS_FREE (dYx);
        EOS_FREE (xyBounds2);
        break;
      }

      for (i = 0; i < nXYPairs; i++) {
        /* derivative dFy = dFu * dUy; dFx = dFx - dFy * dUCx */

	/* DAP -- Test multiplication with EOS_CHECK_PRODUCT prior to performing it.
	   This is a temporary kludge to prevent SIGFPE. */
	if (EOS_CHECK_PRODUCT(dUy[i], dFy[i]))
	  dFy[i] = dUy[i] * dFy[i] /*dFu */ ;
	else
	  dFy[i] = SIGN(dUy[i]) * SIGN(dFy[i]) *
	    (EOS_IS_PRODUCT_GT_MAX(dUy[i], dFy[i]) ? DBL_MAX : DBL_MIN);

	/* DAP -- Test multiplication with EOS_CHECK_PRODUCT prior to performing it.
	   This is a temporary kludge to prevent SIGFPE. */
	if (EOS_CHECK_PRODUCT(dYx[i], dFy[i]))
	  dFx[i] = dFx[i] - dFy[i] * dYx[i];
	else
	  dFx[i] = SIGN(dYx[i]) * SIGN(dFy[i]) *
	    (EOS_IS_PRODUCT_GT_MAX(dYx[i], dFy[i]) ? DBL_MAX : DBL_MIN);

	/* combine extrapolation codes */
	xyBounds[i] = _eos_CombineExtrapErrors (xyBounds[i], xyBounds2[i]);
      }
      EOS_FREE (dUy);
      EOS_FREE (uVals);
      EOS_FREE (dUCx);
      EOS_FREE (dFCx1);
      EOS_FREE (dYx);
      EOS_FREE (xyBounds2);
      break;
    }
  case EOS_CATEGORY3:          /* indicates the table is merged with another function */
    {
      /* get the data pointers and types for these variables */
      tabInd1 = EOS_EOS_TABLE_TYPE_REF1 (dataType);
      tabInd2 = EOS_EOS_TABLE_TYPE_REF2 (dataType);

      /* We need F(X,Y) given F(U, Y) and U(X, Y) */
      /* 1. find U using second table, 2. find F using 1st table. */
      uVals = (EOS_REAL *) malloc (sizeof (EOS_REAL) * nXYPairs);
      dUx = (EOS_REAL *) malloc (sizeof (EOS_REAL) * nXYPairs);
      dXy = (EOS_REAL *) malloc (sizeof (EOS_REAL) * nXYPairs);
      xyBounds2 = (EOS_INTEGER *) malloc (nXYPairs * sizeof (EOS_INTEGER));
      _eos_InterpolateRecordType1 (eosData, th, U_Y_F, tabInd2, nXYPairs,
                                   xVals, yVals, uVals, dUx, dFy /*dUy */ ,
                                   NULL, NULL, dXy, xyBounds, &err);
      if (eos_GetStandardErrorCodeFromCustomErrorCode(err) != EOS_OK)
        *errorCode = err;
      if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) == EOS_UNDEFINED) {
        EOS_FREE (dUx);
        EOS_FREE (uVals);
        EOS_FREE (dXy);
        EOS_FREE (xyBounds2);
        break;
      }
      _eos_InterpolateRecordType1 (eosData, th, X_Y_U, tabInd1, nXYPairs,
                                   uVals, yVals, fVals, dFx /*dFu */ , dFy,
                                   NULL, NULL, NULL, xyBounds2, &err);
      if (eos_GetStandardErrorCodeFromCustomErrorCode(err) != EOS_OK)
        *errorCode = err;
      if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) == EOS_UNDEFINED) {
        EOS_FREE (dUx);
        EOS_FREE (uVals);
        EOS_FREE (dXy);
        EOS_FREE (xyBounds2);
        break;
      }

      for (i = 0; i < nXYPairs; i++) {
        /* derivative dFx = dFu * dUx */

	/* DAP -- Test multiplication with EOS_CHECK_PRODUCT prior to performing it.
	   This is a temporary kludge to prevent SIGFPE. */
	if (EOS_CHECK_PRODUCT(dUx[i], dFx[i]))
	  dFx[i] = dUx[i] * dFx[i] /*dFu */ ;
	else
	  dFx[i] = SIGN(dUx[i]) * SIGN(dFx[i]) *
	    (EOS_IS_PRODUCT_GT_MAX(dUx[i], dFx[i]) ? DBL_MAX : DBL_MIN);

	/* DAP -- Test multiplication with EOS_CHECK_PRODUCT prior to performing it.
	   This is a temporary kludge to prevent SIGFPE. */
	if (EOS_CHECK_PRODUCT(dXy[i], dFx[i]))
	  dFy[i] = dFy[i] - dFx[i] * dXy[i];
	else
	  dFy[i] = SIGN(dXy[i]) * SIGN(dFx[i]) *
	    (EOS_IS_PRODUCT_GT_MAX(dXy[i], dFx[i]) ? DBL_MAX : DBL_MIN);

	/* combine extrapolation codes */
	xyBounds[i] = _eos_CombineExtrapErrors (xyBounds[i], xyBounds2[i]);
      }
      EOS_FREE (dUx);
      EOS_FREE (uVals);
      EOS_FREE (dXy);
      EOS_FREE (xyBounds2);
      break;
    }
  default:
    {
      *errorCode = EOS_INVALID_TABLE_HANDLE;
      ((eos_ErrorHandler *) me)->HandleError (me, th, *errorCode);
      break;
    }
  }

  if (srchX != xVals)
    EOS_FREE (xVals);
  if (srchY != yVals)
    EOS_FREE (yVals);

  /* free arrays, which were allocated within _eos_CreateGhostData above */
  *errorCode = _eos_DestroyGhostData (&nGhostData, &xtbls, &ytbls, &ftbls, &coldCurve);

}

EOS_INTEGER _eos_EvaluateTransformedTaylorFor_Pt_DT (eos_Taylor *T, EOS_REAL x, EOS_REAL y, EOS_REAL *f, EOS_REAL *dFx, EOS_REAL *dFy)
{
  /* initialize */
  *f = *dFx = *dFy = 0;

  /* function */
  *f = - T->DerivativeScalar(T, 1, 0, x, y);

  /* partial derivatives */
  *dFx = (x * x) *T->DerivativeScalar(T, 2, 0, x, y);
  *dFy = - T->DerivativeScalar(T, 1, 1, x, y);

  return EOS_OK;
}

EOS_INTEGER _eos_EvaluateTransformedTaylorFor_Ut_DT (eos_Taylor *T, EOS_REAL x, EOS_REAL y, EOS_REAL *f, EOS_REAL *dFx, EOS_REAL *dFy)
{
  /* initialize */
  *f = *dFx = *dFy = 0;

  /* function */
  *f = T->EvaluateScalar(T, x, y) - y * T->DerivativeScalar(T, 0, 1, x, y);

  /* partial derivatives */
  *dFx = - (x * x) *(T->DerivativeScalar(T, 1, 0, x, y) - y * T->DerivativeScalar(T, 1, 1, x, y));
  *dFy = - y * T->DerivativeScalar(T, 0, 2, x, y);

  return EOS_OK;
}

EOS_INTEGER _eos_EvaluateTaylorFor_dataType_DT (eos_Taylor *T, EOS_REAL x, EOS_REAL y, EOS_REAL *f, EOS_REAL *dFx, EOS_REAL *dFy)
{
  /* initialize */
  *f = *dFx = *dFy = 0;

  /* function */
  *f = T->EvaluateScalar(T, x, y);

  /* partial derivatives */
  *dFx = -(x * x) *T->DerivativeScalar(T, 1, 0, x, y);
  *dFy = T->DerivativeScalar(T, 0, 1, x, y);

  return EOS_OK;
}

EOS_INTEGER _eos_EvaluateTransformedTaylorFor_St_DT (eos_Taylor *T, EOS_REAL x, EOS_REAL y, EOS_REAL *f, EOS_REAL *dFx, EOS_REAL *dFy)
{
  /* initialize */
  *f = *dFx = *dFy = 0;

  /* function */
  *f = - T->DerivativeScalar(T, 0, 1, x, y);

  /* partial derivatives */
  *dFx = (x * x) * T->DerivativeScalar(T, 1, 1, x, y);
  *dFy = - T->DerivativeScalar(T, 0, 2, x, y);

  return EOS_OK;
}

/***********************************************************************/
/*!
 * \brief Function _eos_EvaluateTaylorRecordType1 (helping function for
 *  eos_InterpolateRecordType1().
 *
 * \param[out]   fVals[nXYPairs] - EOS_REAL : array of the interpolated data corresponding
 *                                            to x and y.
 * \param[out]   dFx[nXYPairs]   - EOS_REAL : array of the interpolated partial derivatives
 *                                            of fVals with respect to x.
 * \param[out]   dFy[nXYPairs]   - EOS_REAL : array of the interpolated partial derivatives
 *                                            of fVals with respect to y.
 * \param[out]   dFCx[nXYPairs]  - EOS_REAL : optional array of the interpolated partial
 *                                            derivatives of cold curve with respect to x.
 * \param[out]   dFx0[nXYPairs]  - EOS_REAL : optional array of the interpolated partial
 *                                            derivatives of cat 0 F wrt x (for inverse
 *                                            functions mostly).
 * \param[out]   dFy0[nXYPairs]  - EOS_REAL : optional array of the interpolated partial
 *                                            derivatives of cat 0 F wrt y (for inverse
 *                                            functions mostly).
 * \param[out]   *xyBounds       - EOS_INTEGER : interpolation errors per xy-pair
 * \param[out]   errorCode       - EOS_INTEGER : error code of the interpolation:
 *                                               EOS_INTERP_EXTRAPOLATED or EOS_OK
 * \param[in]    *ptr            - void : data object pointer;
 *                                        internally recast to eos_RecordType1*
 * \param[in]    th              - EOS_INTEGER : table Handle
 * \param[in]    dataType        - EOS_INTEGER : dataType
 * \param[in]    nXYPairs        - EOS_INTEGER : total number of pairs of independent variable values provided for interpolation.
 * \param[in]    srchX[nXYPairs] - EOS_REAL : array of the primary independent variable values to use during interpolation.
 * \param[in]    srchY[nXYPairs] - EOS_REAL : array of the secondary independent variable values to use during interpolation.
 *
 * \return none
 *
 ***********************************************************************/
void _eos_EvaluateTaylorRecordType1 (void *ptr, EOS_INTEGER th, EOS_INTEGER dataType,
				     EOS_INTEGER nXYPairs, EOS_REAL *srchX,
				     EOS_REAL *srchY, EOS_REAL *fVals,
				     EOS_REAL *dFx, EOS_REAL *dFy,
				     EOS_REAL *dFCx, EOS_REAL *dFx0,
				     EOS_REAL *dFy0, EOS_INTEGER *xyBounds,
				     EOS_INTEGER *errorCode)
{
  eos_RecordType1 *me = (eos_RecordType1 *) ptr;

  EOS_INTEGER subTableNum = -1, my_subTableNum = -1;
  EOS_INTEGER k;

  my_subTableNum = EOS_TYPE_TO_SUB_TAB_NUM(dataType) - 1;
  if (me->Taylor_objects[my_subTableNum])
    subTableNum = my_subTableNum;
  else
    subTableNum = 2; /* transform Helmholtz free energy according to dataType */

  if (my_subTableNum >= 0 && *(me->_eos_EvaluateTaylor)[my_subTableNum]) {

    /* Evaluate Taylor polynomial (currently only for EOS_Pt, EOS_Ut, EOS_At, or EOS_St functions) */
    EOS_INTEGER i, *ix_low = NULL, *iy_low = NULL;
    EOS_INTEGER err = EOS_OK;
    eos_Taylor *T = NULL;
    EOS_REAL *inverse_srchX = NULL;
    EOS_REAL xMin, yMin, xMax, yMax;

    /* locate indexes of X, Y */
    ix_low = (EOS_INTEGER *) malloc (nXYPairs * sizeof (EOS_INTEGER));
    assert(ix_low != NULL);
    iy_low = (EOS_INTEGER *) malloc (nXYPairs * sizeof (EOS_INTEGER));
    assert(iy_low != NULL);

    inverse_srchX = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
    assert(inverse_srchX != NULL);
    for (i=0;i<nXYPairs;i++) { /* invert X values for search; impose TINY_D as minimum */
      inverse_srchX[i] = 1.0 / MAX(srchX[i], TINY_D);
    }

    _eos_srchdf (nXYPairs, inverse_srchX, 1, me->M, me->TX, 1, ix_low, xyBounds, &err);
    _eos_srchdf (nXYPairs, srchY, 1, me->N, me->TY, 1, iy_low, xyBounds, &err);

    /* evaluate */
    for (i=0;i<nXYPairs;i++) {
      k = ix_low[i] + iy_low[i] * me->M;
      T = (me->Taylor_objects[subTableNum])[k];

      //printf("ix_low=%d\tiy_low=%d\tk=%d\ta=%e\tb=%e\tx=%e\ty=%e\n",ix_low[i],iy_low[i],k,T->a,T->b,inverse_srchX[i],srchY[i]);

      /* evaluate function and partial derivatives based upon dataType */
      err = (*(me->_eos_EvaluateTaylor)[my_subTableNum])(T, inverse_srchX[i], srchY[i], &fVals[i], &dFx[i], &dFy[i]);
    }

    if (! err) {
      /* fix xyBounds[] array content since ranges in me->TX and/or me->TY do not represent to original data ranges */
      T = (me->Taylor_objects[subTableNum])[0];
      xMin = (me->tabulated_rhozero_exists) ? 0.0 : T->tx_interval[0];
      yMin = T->ty_interval[0];
      T = (me->Taylor_objects[subTableNum])[(me->M - 1) + (me->N - 1) * me->M];
      xMax = T->tx_interval[1];
      yMax = T->ty_interval[1];
      *errorCode = _eos_CheckExtrapEosInterpolationGeneric(xMin, xMax, yMin, yMax, nXYPairs, srchX, srchY, xyBounds);
    }
    else {
      *errorCode = err;
    }

    EOS_FREE (iy_low);
    EOS_FREE (ix_low);
    EOS_FREE (inverse_srchX);

  }
  else {
    *errorCode = EOS_INVALID_TABLE_HANDLE;
  }

  ((eos_ErrorHandler *) me)->HandleError (me, th, *errorCode);
}

/***********************************************************************/
/*!
 * \brief Function eos_InterpolateRecordType1 (helping function for
 *  eos_InterpolateEosInterpolation().
 *  The eos_InterpolateEosInterpolation() routine provides interpolated values
 *  for a single material using a table handle associated with Data stored
 *  within an data table.
 *
 * \param[out]   fVals[nXYPairs] - EOS_REAL : array of the interpolated data corresponding
 *                                 to x and y. 
 * \param[out]   dFx[nXYPairs]   - EOS_REAL : array of the interpolated partial derivatives
 *                                 of fVals with respect to x.
 * \param[out]   dFy[nXYPairs]   - EOS_REAL : array of the interpolated partial derivatives
 *                                 of fVals with respect to y.
 * \param[out]   *xyBounds       - EOS_INTEGER : interpolation errors per xy-pair
 * \param[out]   errorCode       - EOS_INTEGER : error code of the interpolation:
 *                                               EOS_INTERP_EXTRAPOLATED or EOS_OK
 *
 * The input arguments are:
 * void *ptr                 a pointer to a eos_RecordType1 instance from which to extract the data. 
 * EOS_INTEGER th            table Handle
 * EOS_INTEGER dataType      dataType
 * EOS_INTEGER nXYPairs      total number of pairs of independent variable values provided for interpolation.
 * EOS_REAL srchX[nXYPairs]  array of the primary independent variable values to use during interpolation. 
 * EOS_REAL srchY[nXYPairs]  array of the secondary independent variable values to use during interpolation. 
 *
 * \return none
 *
 ***********************************************************************/
void eos_InterpolateRecordType1 (void *ptr, EOS_INTEGER th, EOS_INTEGER dataType,
                                 EOS_INTEGER nXYPairs, EOS_REAL *srchX,
                                 EOS_REAL *srchY, EOS_REAL *fVals,
                                 EOS_REAL *dFx, EOS_REAL *dFy,
                                 EOS_INTEGER *xyBounds,
                                 EOS_INTEGER *errorCode)
{
  eos_OptionValue *optVal = NULL;
  eos_RecordType1 *me;
  me = (eos_RecordType1 *) ptr;

  /* make sure the data is record type 1 */
  if (EOS_TYPE_TO_RECORD_TYPE (dataType) != EOS_RECORD_TYPE1) {
    *errorCode = EOS_INVALID_TABLE_HANDLE;
    ((eos_ErrorHandler *) me)->HandleError (me, th, *errorCode);
    return;
  }

  optVal = _eos_getOptionEosData (&(me->eosData), EOS_USE_TAYLOR_FIT);

  if (optVal->bval) {

    _eos_EvaluateTaylorRecordType1 (ptr, th, dataType, nXYPairs, srchX,
				    srchY, fVals, dFx, dFy, NULL, NULL, NULL,
				    xyBounds, errorCode);

  }
  else {

    if (me->isInvertedAtSetup)
      eos_SetOptionEosInterpolation (&gEosInterpolation, th, EOS_DISABLE_GHOST_NODES, EOS_TRUE, errorCode);

    /* Interpolate tabular data */
    _eos_InterpolateRecordType1 (ptr, th, me->eosData.varOrder, dataType, nXYPairs, srchX,
				 srchY, fVals, dFx, dFy, NULL, NULL, NULL,
				 xyBounds, errorCode);

  }
}

/***********************************************************************/
/*!
 * \brief If the EOS_INTERP_EXTRAPOLATED error code is returned by either
 *  eos_Interp or eos_Mix, this routine allows the user to determine which
 *  (x,y) pairs caused extrapolation and in which direction (high or low),
 *  it occurred.
 *
 * The input arguments are
 * xVals	This is an array of the primary independent variable values to use during interpolation. There are nXYPairs elements in xVals.
 * yVals	This is an array of the secondary independent variable values to use during interpolation. There are nXYPairs elements in yVals.
 *
 * \param[out]   xyBounds  - EOS_INTEGER : This is an array of size nXYPairs elements that
 *                                         returns EOS_OK if extrapolation did not occur.
 *                                         If extrapolation occurred the variable and direction
 *                                         are determined from Table 2.
 * \param[out]   errorCode - EOS_INTEGER : This is a scalar EOS_INTEGER to contain an error code.
 *                                         In the case that eos_Mix returned
 *                                         EOS_INTERP_EXTRAPOLATED as an error code,
 *                                         an additional series of steps must be performed
 *                                         to determine which tableHandle(s) correspond to
 *                                         the extrapolation error:
 *
 *                                         -# For each tableHandle sent to eos_Mix, call
 *                                            eos_GetErrorCode and, optionally, eos_GetErrorMessage.
 *                                         -# For each of these tableHandles, call eos_CheckExtrap
 *                                            to determine one of codes listed in Table 2.
 *\code
 *               Table 2. Extrapolation return codes.
 *               Code Definition
 *               EOS_OK	No extrapolation occurred.
 *               EOS_xHi_yHi	Both the x and y arguments were high.
 *               EOS_xHi_yOk	The x argument was high, the y argument was OK.
 *               EOS_xHi_yLo	The x argument was high, the y argument was low.
 *               EOS_xOk_yLo	The x argument is OK and the y argument is low.
 *               EOS_xLo_yLo	Both the x and y arguments were low.
 *               EOS_xLo_yOk	The x argument was low, the y argument was OK.
 *               EOS_xLo_yHi	The x argument was low, the y argument was OK.
 *               EOS_xOk_yHi	The x argument is OK and the y argument is high.
 \endcode
 *
 * \return none
 *
 ***********************************************************************/
void eos_CheckExtrapRecordType1 (void *ptr, EOS_INTEGER th, EOS_INTEGER dataType,
                                 EOS_INTEGER nXYPairs, EOS_REAL *srchX,
                                 EOS_REAL *srchY, EOS_INTEGER *xyBounds,
                                 EOS_INTEGER *errorCode)
{
  eos_Data *eosData;
  EOS_INTEGER i, tabInd2, nX, nY, cat, subTableNum;
  EOS_REAL *X, *Y, **F, *coldCurve;
  EOS_INTEGER err = EOS_OK;
  EOS_REAL *xVals, *yVals;
  eos_RecordType1 *me;
  EOS_BOOLEAN isOneDimDatatype = EOS_FALSE, doRational;

  *errorCode = EOS_OK;

  eosData = (eos_Data *) ptr;
  me = (eos_RecordType1 *) eosData;
  cat = EOS_CATEGORY (dataType);
  subTableNum = EOS_TYPE_TO_SUB_TAB_NUM (dataType);
  /* get the size of the data */
  eos_GetSizeRecordType1 (me, &nX, &nY);

  /* make sure the data is record type 1 */
  if (EOS_TYPE_TO_RECORD_TYPE (dataType) != EOS_RECORD_TYPE1) {
    *errorCode = EOS_INVALID_TABLE_HANDLE;
    ((eos_ErrorHandler *) me)->HandleError (me, th, *errorCode);
    return;
  }

  if (me->eosData.numSubtablesLoaded < subTableNum) {
    *errorCode = EOS_DATA_TYPE_NOT_FOUND;
    ((eos_ErrorHandler *) me)->HandleError (me, th, *errorCode);
    return;
  }

  /* is the datatpe a function with one independent variable? */
  isOneDimDatatype = EOS_IS_ONE_DIM_TYPE (dataType);

  _eos_GetDataRecordType1 (me, &X, &Y, &F, &coldCurve, subTableNum);

  xVals = srchX;
  yVals = (! isOneDimDatatype) ? srchY : NULL;

  switch (cat) {
  case EOS_CATEGORY0:          /* indicates the table is not inverted */
    {
      if (! isOneDimDatatype) {

	_eos_CheckExtrapCategory0(0, me->NR, me->NT, X, Y,
				  nXYPairs, xVals, yVals, xyBounds, &err);

      }
      else {                  /* y is either OK or not considered */

	for (i = 0; i < nXYPairs; i++) {
	  if (xVals[i] < X[0])
	    xyBounds[i] = EOS_xLo_yOk;
	  else if (xVals[i] > X[nX - 1])
	    xyBounds[i] = EOS_xHi_yOk;
	  else
	    xyBounds[i] = EOS_OK;
	}

      }
      break;
    }
  case EOS_CATEGORY1:          /* indicates the table is inverted with respect to 1st independent variable */
    {
      /* means that the user provides y and F of a function that is defined as x( F,y)
         so effectively the x and y user has given us are F and Y */
      if (! isOneDimDatatype) {

	EOS_INTEGER *iy_low = NULL;

	iy_low = (EOS_INTEGER *) malloc (nXYPairs * sizeof (EOS_INTEGER));

	_eos_srchdf (nXYPairs, yVals, 1, nY - 1, Y, 1, iy_low, xyBounds, &err);

	doRational = eos_getBoolOptionFromTableHandle (th, EOS_RATIONAL, &err);

	if (doRational) {
	  for (i = 0; i < nXYPairs; i++)
	    iy_low[i] = MAX(MIN(iy_low[i], me->NT-3), 1);
	}
	_eos_CheckExtrapCategory1(eos_getBoolOptionFromTableHandle (th, EOS_DISCONTINUOUS_DERIVATIVES, &err),
				  0, iy_low, doRational,
				  me->NR, me->NT, X, Y, F,
				  nXYPairs, xVals, yVals, xyBounds, &err);
	EOS_FREE(iy_low);
      }
      else {                  /* y is either OK or not considered */

	for (i = 0; i < nXYPairs; i++) {
	  if (xVals[i] < F[0][0])
	    xyBounds[i] = EOS_xLo_yOk;
	  else if (xVals[i] > F[0][nX - 1])
	    xyBounds[i] = EOS_xHi_yOk;
	  else
	    xyBounds[i] = EOS_OK;
	}

      }
      break;
    }
  case EOS_CATEGORY2:          /* indicates the table is inverted with respect to 2nd independent variable */
    {
      /* means that the user provides x and F of a function that is defined as y( x,F)
         so effectively the x and y user has given us are X and F */

      if (! isOneDimDatatype) {

	EOS_INTEGER *ix_low = NULL;

	ix_low = (EOS_INTEGER *) malloc (nXYPairs * sizeof (EOS_INTEGER));

	_eos_srchdf (nXYPairs, xVals, 1, nX - 1, X, 1, ix_low, xyBounds, &err);

	doRational = eos_getBoolOptionFromTableHandle (th, EOS_RATIONAL, &err);

	if (doRational) {
	  for (i = 0; i < nXYPairs; i++)
	    ix_low[i] = MAX(MIN(ix_low[i], me->NR-3), 1);
	}
	_eos_CheckExtrapCategory2(eos_getBoolOptionFromTableHandle (th, EOS_DISCONTINUOUS_DERIVATIVES, &err),
				  0, ix_low, doRational,
				  me->NR, me->NT, X, Y, F,
				  nXYPairs, xVals, yVals, xyBounds, &err);
	EOS_FREE(ix_low);

      }
      else {                  /* y is either OK or not considered */

	for (i = 0; i < nXYPairs; i++) {
	  if (xVals[i] < F[0][0])
	    xyBounds[i] = EOS_xLo_yOk;
	  else if (xVals[i] > F[0][nX - 1])
	    xyBounds[i] = EOS_xHi_yOk;
	  else
	    xyBounds[i] = EOS_OK;
	}

      }

      break;
    }
  case EOS_CATEGORY3:          /* indicates the table is merged with another function */
  case EOS_CATEGORY4:
    {
      /* get the data pointers and types for these valuables */
      tabInd2 = EOS_EOS_TABLE_TYPE_REF2 (dataType);

      /* We need G(X,Y) given G(X, U) and U(X, Y) (cat3) or G(X,Y) given G(U, Y) and U(X, Y) */
      /* call checkExtrap recursively, allow for passing of only X or only Y into 
         eos_CheckExtrapEosInterpolation (), checking the second table is enough */
      me->eosData.tmpVarOrder = X_Y_U;
      eos_CheckExtrapRecordType1 (me, th, tabInd2, nXYPairs, xVals,
                                  yVals, xyBounds, errorCode);
      break;
    }
  default:
    {
      err = EOS_INVALID_TABLE_HANDLE;
      break;
    }
  }                             /* switch statement */

  if (srchX != xVals)
    EOS_FREE (xVals);
  if (! isOneDimDatatype && srchY != yVals)
    EOS_FREE (yVals);

  if (eos_GetStandardErrorCodeFromCustomErrorCode(err) != EOS_OK) {
    ((eos_ErrorHandler *) me)->HandleError (me, th, err);
    *errorCode = err;
    return;
  }
}

/***********************************************************************/
/*!
 * \brief If the EOS_INTERP_EXTRAPOLATED error code is returned by either
 *  eos_Interp or eos_Mix, this routine allows the user to determine which
 *  (x,y) pairs caused extrapolation and in which direction (high or low),
 *  it occurred.
 *
 * The input arguments are
 * xVals	This is an array of the primary independent variable values to use during interpolation. There are nXYPairs elements in xVals.
 * yVals	This is an array of the secondary independent variable values to use during interpolation. There are nXYPairs elements in yVals.
 *
 * \param[out]   xyBounds  - EOS_INTEGER : This is an array of size nXYPairs elements that
 *                                         returns EOS_OK if extrapolation did not occur.
 *                                         If extrapolation occurred the variable and direction
 *                                         are determined from Table 2.
 * \param[out]   errorCode - EOS_INTEGER : This is a scalar EOS_INTEGER to contain an error code.
 *                                         In the case that eos_Mix returned
 *                                         EOS_INTERP_EXTRAPOLATED as an error code,
 *                                         an additional series of steps must be performed
 *                                         to determine which tableHandle(s) correspond to
 *                                         the extrapolation error:
 *
 *                                         -# For each tableHandle sent to eos_Mix, call
 *                                            eos_GetErrorCode and, optionally, eos_GetErrorMessage.
 *                                         -# For each of these tableHandles, call eos_CheckExtrap
 *                                            to determine one of codes listed in Table 2.
 *\code
 *               Table 2. Extrapolation return codes.
 *               Code Definition
 *               EOS_OK	No extrapolation occurred.
 *               EOS_xHi_yHi	Both the x and y arguments were high.
 *               EOS_xHi_yOk	The x argument was high, the y argument was OK.
 *               EOS_xHi_yLo	The x argument was high, the y argument was low.
 *               EOS_xOk_yLo	The x argument is OK and the y argument is low.
 *               EOS_xLo_yLo	Both the x and y arguments were low.
 *               EOS_xLo_yOk	The x argument was low, the y argument was OK.
 *               EOS_xLo_yHi	The x argument was low, the y argument was OK.
 *               EOS_xOk_yHi	The x argument is OK and the y argument is high.
 \endcode
 *
 * \return none
 *
 ***********************************************************************/
void eos_CheckExtrapRecordType1_using_extrapolationBounds (
							   void *ptr, EOS_INTEGER th, EOS_INTEGER dataType, EOS_INTEGER nXYPairs, EOS_REAL *srchX,
							   EOS_REAL *srchY, EOS_INTEGER *xyBounds, EOS_INTEGER *errorCode)
{
  eos_Data *eosData;
  EOS_INTEGER i, nX, nY, cat, subTableNum;
  EOS_REAL *X, *Y, **F, *coldCurve;
  EOS_INTEGER err = EOS_OK;
  EOS_REAL *xVals, *yVals;
  eos_RecordType1 *me;
  EOS_BOOLEAN isOneDimDatatype = EOS_FALSE, doRational;

  *errorCode = EOS_OK;

  eosData = (eos_Data *) ptr;
  me = (eos_RecordType1 *) eosData;

  assert (me->extrapolationBounds.stored);

  if (me->isInvertedAtSetup)
    subTableNum = 1;
  else
    subTableNum = EOS_TYPE_TO_SUB_TAB_NUM (dataType);

  cat = EOS_CATEGORY (dataType);

  /* get the size of the data */
  eos_GetSizeRecordType1 (me, &nX, &nY);

  /* make sure the data is record type 1 */
  if (EOS_TYPE_TO_RECORD_TYPE (dataType) != EOS_RECORD_TYPE1) {
    *errorCode = EOS_INVALID_TABLE_HANDLE;
    ((eos_ErrorHandler *) me)->HandleError (me, th, *errorCode);
    return;
  }

  if (me->eosData.numSubtablesLoaded < subTableNum) {
    *errorCode = EOS_DATA_TYPE_NOT_FOUND;
    ((eos_ErrorHandler *) me)->HandleError (me, th, *errorCode);
    return;
  }

  /* is the datatpe a function with one independent variable? */
  isOneDimDatatype = EOS_IS_ONE_DIM_TYPE (dataType);

  _eos_GetDataRecordType1 (me, &X, &Y, &F, &coldCurve, subTableNum);

  xVals = srchX;
  yVals = (! isOneDimDatatype) ? srchY : NULL;

  switch (cat) {
  case EOS_CATEGORY0:          /* indicates the table is not inverted */
    {
      if (! isOneDimDatatype) {

	EOS_INTEGER xFlag, yFlag;
	xFlag = yFlag = EOS_OK;
	for (i = 0; i < nXYPairs; i++) {
	  if (xVals[i] < me->extrapolationBounds.xLo[0])
	    xFlag = EOS_xLo_yOk;
	  else if (xVals[i] > me->extrapolationBounds.xHi[0])
	    xFlag = EOS_xHi_yOk;
	  if (yVals[i] < me->extrapolationBounds.yLo[0])
	    yFlag = EOS_xLo_yOk;
	  else if (yVals[i] > me->extrapolationBounds.yHi[0])
	    yFlag = EOS_xHi_yOk;
	  xyBounds[i] = _eos_CombineExtrapErrors (xFlag, yFlag);
	}

      }
      else {                  /* y is either OK or not considered */

	for (i = 0; i < nXYPairs; i++) {
	  if (xVals[i] < me->extrapolationBounds.xLo[0])
	    xyBounds[i] = EOS_xLo_yOk;
	  else if (xVals[i] > me->extrapolationBounds.xHi[0])
	    xyBounds[i] = EOS_xHi_yOk;
	  else
	    xyBounds[i] = EOS_OK;
	}

      }
      break;
    }
  case EOS_CATEGORY1:          /* indicates the table is inverted with respect to 1st independent variable */
  case EOS_CATEGORY3:          /* indicates the table is a merging of a CATEGORY1 table and a CATEGORY0 table */
    {
      /* means that the user provides y and F of a function that is defined as x( F,y)
         so effectively the x and y user has given us are F and Y */
      if (! isOneDimDatatype) {

	EOS_REAL *fValsLo = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
	EOS_REAL *fValsHi = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
	EOS_INTEGER xFlag, yFlag;

	doRational = eos_getBoolOptionFromTableHandle (th, EOS_RATIONAL, &err);

	if (doRational) {
	  eos_RationalInterpolate (nXYPairs, me->extrapolationBounds.nx, 1, 0,
				   me->extrapolationBounds.x, me->extrapolationBounds.xLo,
				   yVals, fValsLo, NULL, 'y', xyBounds, &err);
	  eos_RationalInterpolate (nXYPairs, me->extrapolationBounds.nx, 1, 0,
				   me->extrapolationBounds.x, me->extrapolationBounds.xHi,
				   yVals, fValsHi, NULL, 'y', xyBounds, &err);
	}
	else {
	  eos_LineInterpolate (EOS_TRUE, nXYPairs, me->extrapolationBounds.nx, 1, 0,
			       me->extrapolationBounds.x, &(me->extrapolationBounds.xLo),
			       yVals, fValsLo, NULL, 'y', xyBounds, &err);
	  eos_LineInterpolate (EOS_TRUE, nXYPairs, me->extrapolationBounds.nx, 1, 0,
			       me->extrapolationBounds.x, &(me->extrapolationBounds.xHi),
			       yVals, fValsHi, NULL, 'y', xyBounds, &err);
	}

	for (i = 0; i < nXYPairs; i++) {
	  xFlag = yFlag = EOS_OK;
	  if (xVals[i] < fValsLo[i])
	    xFlag = EOS_xLo_yOk;
	  else if (xVals[i] > fValsHi[i])
	    xFlag = EOS_xHi_yOk;
	  if (yVals[i] < me->extrapolationBounds.yLo[0])
	    yFlag = EOS_xOk_yLo;
	  else if (yVals[i] > me->extrapolationBounds.yHi[0])
	    yFlag = EOS_xOk_yHi;
	  xyBounds[i] = _eos_CombineExtrapErrors (xFlag, yFlag);
	}

	EOS_FREE(fValsLo);
	EOS_FREE(fValsHi);

      }
      else {                  /* y is either OK or not considered */

	if (me->extrapolationBounds.stored) {

	  for (i = 0; i < nXYPairs; i++) {
	    if (xVals[i] < me->extrapolationBounds.xLo[0])
	      xyBounds[i] = EOS_xLo_yOk;
	    else if (xVals[i] > me->extrapolationBounds.xHi[0])
	      xyBounds[i] = EOS_xHi_yOk;
	    else
	      xyBounds[i] = EOS_OK;
	  }

	}
	else {

	  for (i = 0; i < nXYPairs; i++) {
	    if (xVals[i] < F[0][0])
	      xyBounds[i] = EOS_xLo_yOk;
	    else if (xVals[i] > F[0][nX - 1])
	      xyBounds[i] = EOS_xHi_yOk;
	    else
	      xyBounds[i] = EOS_OK;
	  }

	}

      }
      break;
    }
  case EOS_CATEGORY2:          /* indicates the table is inverted with respect to 2nd independent variable */
  case EOS_CATEGORY4:          /* indicates the table is a merging of a CATEGORY2 table and a CATEGORY0 table */
    {
      /* means that the user provides x and F of a function that is defined as y( x,F)
         so effectively the x and y user has given us are X and F */

      if (! isOneDimDatatype) {

	EOS_REAL *fValsLo = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
	EOS_REAL *fValsHi = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
	EOS_INTEGER xFlag, yFlag;

	doRational = eos_getBoolOptionFromTableHandle (th, EOS_RATIONAL, &err);

	if (doRational) {
	  eos_RationalInterpolate (nXYPairs, me->extrapolationBounds.ny, 1, 0,
				   me->extrapolationBounds.x, me->extrapolationBounds.yLo,
				   xVals, fValsLo, NULL, 'y', xyBounds, &err);
	  eos_RationalInterpolate (nXYPairs, me->extrapolationBounds.ny, 1, 0,
				   me->extrapolationBounds.x, me->extrapolationBounds.yHi,
				   xVals, fValsHi, NULL, 'y', xyBounds, &err);
	}
	else {
	  eos_LineInterpolate (EOS_TRUE, nXYPairs, me->extrapolationBounds.ny, 1, 0,
			       me->extrapolationBounds.x, &(me->extrapolationBounds.yLo),
			       xVals, fValsLo, NULL, 'y', xyBounds, &err);
	  eos_LineInterpolate (EOS_TRUE, nXYPairs, me->extrapolationBounds.ny, 1, 0,
			       me->extrapolationBounds.x, &(me->extrapolationBounds.yHi),
			       xVals, fValsHi, NULL, 'y', xyBounds, &err);
	}

	for (i = 0; i < nXYPairs; i++) {
	  xFlag = yFlag = EOS_OK;
	  if (xVals[i] < me->extrapolationBounds.xLo[0])
	    xFlag = EOS_xLo_yOk;
	  else if (xVals[i] > me->extrapolationBounds.xHi[0])
	    xFlag = EOS_xHi_yOk;
	  if (yVals[i] < fValsLo[i])
	    yFlag = EOS_xOk_yLo;
	  else if (yVals[i] > fValsHi[i])
	    yFlag = EOS_xOk_yHi;
	  xyBounds[i] = _eos_CombineExtrapErrors (xFlag, yFlag);
	}

	EOS_FREE(fValsLo);
	EOS_FREE(fValsHi);

      }
      else {                  /* y is either OK or not considered */

	if (me->extrapolationBounds.stored) {

	  for (i = 0; i < nXYPairs; i++) {
	    if (xVals[i] < me->extrapolationBounds.xLo[0])
	      xyBounds[i] = EOS_xLo_yOk;
	    else if (xVals[i] > me->extrapolationBounds.xHi[0])
	      xyBounds[i] = EOS_xHi_yOk;
	    else
	      xyBounds[i] = EOS_OK;
	  }

	}
	else {

	  for (i = 0; i < nXYPairs; i++) {
	    if (xVals[i] < F[0][0])
	      xyBounds[i] = EOS_xLo_yOk;
	    else if (xVals[i] > F[0][nX - 1])
	      xyBounds[i] = EOS_xHi_yOk;
	    else
	      xyBounds[i] = EOS_OK;
	  }

	}

      }

      break;
    }
  default:
    {
      err = EOS_INVALID_TABLE_HANDLE;
      break;
    }
  }                             /* switch statement */

  if (srchX != xVals)
    EOS_FREE (xVals);
  if (! isOneDimDatatype && srchY != yVals)
    EOS_FREE (yVals);

  if (eos_GetStandardErrorCodeFromCustomErrorCode(err) != EOS_OK) {
    ((eos_ErrorHandler *) me)->HandleError (me, th, err);
    *errorCode = err;
    return;
  }
}

/***********************************************************************/
/*! 
 * \brief * Makes data of class eos_RecordType1 monotonic
 * 
 * \param[out]    *err     - EOS_INTEGER : error code
 * \param[in]     *ptr     - void : data object pointer;
 *                                  internally recast to eos_RecordType1*
 * \param[in]     th       - EOS_INTEGER : table handle for error handling
 * \param[in]     dataType - EOS_INTEGER : data type of subtable to be made monotonic
 * \param[in]     inX      - EOS_BOOLEAN : make monotonic in x?
 * \param[in]     inY      - EOS_BOOLEAN : make monotonic in y?
 * 
 * \return none
 *
 ***********************************************************************/
void eos_MakeMonotonicRecordType1 (void *ptr, EOS_INTEGER th,
                                   EOS_INTEGER dataType, EOS_BOOLEAN inX,
                                   EOS_BOOLEAN inY, EOS_INTEGER *err)
{
  eos_RecordType1 *me;
  EOS_INTEGER subTableNum, *isMonotonicX, *isMonotonicY, indep = 0;
  EOS_REAL *table, *coldCurve;
  me = (eos_RecordType1 *) ptr;
  *err = EOS_OK;

  subTableNum = EOS_TYPE_TO_SUB_TAB_NUM (dataType);

  me = (eos_RecordType1 *) ptr;
  *err = EOS_OK;

  switch (subTableNum) {
  case 1:
    table = &(me->table1[0][0]);
    coldCurve = me->coldCurve1;
    isMonotonicX = &me->isMonotonicX1;
    isMonotonicY = &me->isMonotonicY1;
    break;
  case 2:
    table = &(me->table2[0][0]);
    coldCurve = me->coldCurve2;
    isMonotonicX = &me->isMonotonicX2;
    isMonotonicY = &me->isMonotonicY2;
    break;
  case 3:
    table = &(me->table3[0][0]);
    coldCurve = me->coldCurve3;
    isMonotonicX = &me->isMonotonicX3;
    isMonotonicY = &me->isMonotonicY3;
    break;
  case 4:
    table = &(me->table4[0][0]);
    coldCurve = me->coldCurve4;
    isMonotonicX = &me->isMonotonicX4;
    isMonotonicY = &me->isMonotonicY4;
    break;
  }

  if (inY && inX) {
    if (*isMonotonicY && *isMonotonicX)
      return;
    indep = 2;
  }
  else if (inY) {
    if (*isMonotonicY)
      return;
    indep = 3;
  }
  else if (inX) {
    if (*isMonotonicX)
      return;
    indep = 1;
  }
  else
    return;

  if (indep == 1 && me->eosData.coldCurveIsLoaded) {    /* make coldCurve monotonic */

    if (me->eosData.numSubtablesLoaded >= subTableNum)
      *err = _eos_MakeMonotonic (me->NR, 1, me->R, me->T, coldCurve, indep, NULL, EOS_FALSE);
    if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) != EOS_OK) {
      ((eos_ErrorHandler *) me)->HandleError (me, th, *err);
      return;
    }
  }

  if (me->eosData.numSubtablesLoaded >= subTableNum)
    *err = _eos_MakeMonotonic (me->NR, me->NT, me->R, me->T, table, indep, coldCurve, EOS_FALSE);

  if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) != EOS_OK) {
    ((eos_ErrorHandler *) me)->HandleError (me, th, *err);
    return;
  }

  if (inY && inX) {
    *isMonotonicY = 1;
    *isMonotonicX = 1;
  }
  else if (inY)
    *isMonotonicY = 1;
  else if (inX)
    *isMonotonicX = 1;
  else
    return;
}

/***********************************************************************/
/*! 
 * \brief checks if the data of class eos_RecordType1 is monotonic. Also returns EOS_FALSE
 * if the data is not increasing or decreasing EXCEPT the case where the data is ALL tTHE SAME!
 * 
 * \param[out]   *isMonotonic - EOS_BOOLEAN : is the data monotonic?
 * \param[out]   *err         - EOS_INTEGER : error code
 * \param[in]    *ptr         - void : internally recast to eos_RecordType1*
 * \param[in]    dataType     - EOS_INTEGER : dataType
 * \param[in]    inX          - EOS_BOOLEAN : make monotonic in x?
 * \param[in]    inY          - EOS_BOOLEAN : make monotonic in y?
 * 
 * \return none
 *
 ***********************************************************************/
void eos_IsMonotonicRecordType1 (void *ptr, EOS_INTEGER dataType,
                                 EOS_BOOLEAN *isMonotonic, EOS_BOOLEAN inX,
                                 EOS_BOOLEAN inY, EOS_INTEGER *err)
{
  eos_RecordType1 *me;
  EOS_INTEGER subTableNum = EOS_TYPE_TO_SUB_TAB_NUM (dataType), i, j, sign =
    0, oldSign, *isMonotonicX, *isMonotonicY;
  EOS_REAL diff, *table, *coldCurve;

  me = (eos_RecordType1 *) ptr;
  *err = EOS_OK;

  switch (subTableNum)
  {
  case 1:
    table = &(me->table1[0][0]);
    coldCurve = me->coldCurve1;
    isMonotonicX = &me->isMonotonicX1;
    isMonotonicY = &me->isMonotonicY1;
    break;
  case 2:
    table = &(me->table2[0][0]);
    coldCurve = me->coldCurve2;
    isMonotonicX = &me->isMonotonicX2;
    isMonotonicY = &me->isMonotonicY2;
    break;
  case 3:
    table = &(me->table3[0][0]);
    coldCurve = me->coldCurve3;
    isMonotonicX = &me->isMonotonicX3;
    isMonotonicY = &me->isMonotonicY3;
    break;
  case 4:
    table = &(me->table4[0][0]);
    coldCurve = me->coldCurve4;
    isMonotonicX = &me->isMonotonicX4;
    isMonotonicY = &me->isMonotonicY4;
    break;
  }

  if (inX && !inY && *isMonotonicX != -1 /* if set */ ) {
    *isMonotonic = (*isMonotonicX != 0) ? EOS_TRUE : EOS_FALSE;
    return;
  }
  else if (inY && !inX && *isMonotonicY != -1 /* if set */ ) {
    *isMonotonic = (*isMonotonicY != 0) ? EOS_TRUE : EOS_FALSE;
    return;
  }
  else if (*isMonotonicX != -1  /* if set */
           && *isMonotonicY != -1 /* if set */ ) {
    *isMonotonic = (*isMonotonicY != 0 && *isMonotonicX != 0) ? EOS_TRUE : EOS_FALSE;
    return;
  }

  /* otherwise check the data to determine monotonicity */
  //   *isMonotonicY = 1;
  //   *isMonotonicX = 1;

  if (me->eosData.coldCurveIsLoaded
      && me->eosData.numSubtablesLoaded >= subTableNum) {
    diff = coldCurve[1] - coldCurve[0];
    oldSign = (diff > ZERO) ? 1 : ((diff == ZERO) ? 0 : -1);
  }

  /* make sure coldCurve is monotonic SEPARATELY */
  for (i = 2;
       *isMonotonicX && inX && me->eosData.coldCurveIsLoaded && i < me->NR;
       i++) {
    if (me->eosData.numSubtablesLoaded >= subTableNum) {
      diff = coldCurve[i] - coldCurve[i - 1];
      sign = (diff > ZERO) ? 1 : ((diff == ZERO) ? 0 : -1);
    }


    if (me->eosData.numSubtablesLoaded >= subTableNum &&
	(abs (sign - oldSign) > 0 ||
	 (sign == 0 && oldSign == 0))) {
      *isMonotonicX = 0;
      break;
    }
    oldSign = sign;
  }

  for (j = 0; *isMonotonicX && inX && j < me->NT; j++) {
    if (me->eosData.numSubtablesLoaded >= subTableNum) {
      diff = table[j * me->NT + 1] - table[j * me->NT];
      oldSign = (diff > ZERO) ? 1 : ((diff == ZERO) ? 0 : -1);
    }

    for (i = 2; i < me->NR; i++) {
      if (me->eosData.numSubtablesLoaded >= subTableNum) {
        diff = table[j * me->NT + i] - table[j * me->NT + i - 1];
        sign = (diff > ZERO) ? 1 : ((diff == ZERO) ? 0 : -1);
      }

      if (me->eosData.numSubtablesLoaded >= subTableNum &&
	  (abs (sign - oldSign) > 0 ||
	   (sign == 0 && oldSign == 0))) {
        *isMonotonicX = 0;
        break;
      }
      oldSign = sign;
    }
  }

  for (i = 0; *isMonotonicY && inY && i < me->NR; i++) {
    if (me->eosData.numSubtablesLoaded >= subTableNum) {
      diff = table[me->NR + i] - table[0 + i];
      oldSign = (diff > ZERO) ? 1 : ((diff == ZERO) ? 0 : -1);
    }

    for (j = 2; j < me->NT; j++) {
      if (me->eosData.numSubtablesLoaded >= subTableNum) {
        diff = table[j * me->NR + i] - table[(j - 1) * me->NR + i];
        sign = (diff > ZERO) ? 1 : ((diff == ZERO) ? 0 : -1);
      }

      if (me->eosData.numSubtablesLoaded >= subTableNum &&
	  (abs (sign - oldSign) > 0 ||
	   (sign == 0 && oldSign == 0))) {
        *isMonotonicY = 0;
        break;
      }
      oldSign = sign;
    }
  }

  if (inX && !inY)
    *isMonotonic = (*isMonotonicX != 0) ? EOS_TRUE : EOS_FALSE;
  else if (inY && !inX)
    *isMonotonic = (*isMonotonicY != 0) ? EOS_TRUE : EOS_FALSE;
  else
    *isMonotonic = (*isMonotonicY != 0 && *isMonotonicX != 0) ? EOS_TRUE : EOS_FALSE;
}

/***********************************************************************/
/*! 
 * \brief Write object's data to output file in subdirectory, "dump_eos_data"
 * 
 * \param[in]     call_index - EOS_INTEGER : integer used as part of output file name
 * \param[in]     *ptr       - void : data object pointer;
 *                                    internally recast to eos_RecordType1*
 * \param[in]     *s         - EOS_CHAR : string used as part of output file name
 *
 * \return none
 *
 ***********************************************************************/
void _eos_DumpData (EOS_INTEGER call_index, void *ptr, EOS_CHAR *s)
{
  FILE *fh;
  EOS_BOOLEAN dirExists;
  EOS_CHAR *dir = "dump_eos_data";
  struct stat dir_statbuf;
  EOS_CHAR fn[50];
  eos_RecordType1 *me = (eos_RecordType1 *) ptr;

  eos_RecordType2 *altEosData = NULL;
  EOS_INTEGER i, j, nT401 = 0;
  EOS_REAL *P401 = NULL;        /*[nT401] */
  EOS_REAL *T401 = NULL;        /*[nT401] */
  EOS_REAL *RG401 = NULL;       /*[nT401] */
  EOS_REAL *RL401 = NULL;       /*[nT401] */
  EOS_REAL *EG401 = NULL;       /*[nT401] */
  EOS_REAL *EL401 = NULL;       /*[nT401] */

  if (me->rt2_handle >= 0) {
    altEosData =
      (eos_RecordType2 *) gEosDataMap.dataObjects[gEosDataMap.
                                                  tableHandlesMap[me->
                                                                  rt2_handle]];
    nT401 = altEosData->NT;
    P401 = altEosData->P;
    T401 = altEosData->T;
    RG401 = altEosData->RG;
    RL401 = altEosData->RL;
    EG401 = altEosData->EG;
    EL401 = altEosData->EL;
  }

  dirExists = (!((EOS_BOOLEAN) stat (dir, &dir_statbuf))) ? EOS_TRUE : EOS_FALSE;

#ifndef _MKDIR_NOT_AVAIL
  /* try to create dir with user rwx permissions; do nothing if fail to mkdir */
  if (!dirExists && !mkdir (dir, S_IRUSR | S_IWUSR | S_IXUSR))
    return;
#else
  /* return if dir does not already exist */
  if (!dirExists)
    return;
#endif

  sprintf (fn, "%s/%i.%i.%s.txt", dir, me->eosData.materialID, call_index, s);

  fh = fopen (fn, "w");
  _eos_DEBUG_PRINT ("%s%2i%s%6i%s%s\n", "dump data no. ", call_index,
                    " for matid ", me->eosData.materialID, " to file ", fn);

  fprintf (fh, "%23s%23s%23s%23s\n", "r301", "t301", "p301", "e301");
  for (i = 0; i < me->NR; i++)
    for (j = 0; j < me->NT; j++)
      fprintf (fh, "%23.14e%23.14e%23.14e%23.14e\n", me->R[i], me->T[j],
               me->table1[j][i], me->table2[j][i]);

  if (me->found_401) {
    fprintf (fh, "\n%23s%23s%23s%23s%23s%23s\n",
             "t401", "p401", "rg401", "rl401", "eg401", "el401");
    for (j = 0; j < nT401; j++)
      fprintf (fh, "%23.14e%23.14e%23.14e%23.14e%23.14e%23.14e\n",
               T401[j], P401[j], RG401[j], RL401[j], EG401[j], EL401[j]);
  }
  fclose (fh);
}

/***********************************************************************/
/*! 
 * \brief Adjust data tables: do not allow zero density and patch to shift
 *  temperatures (used in conjunction with EOS_PT_SMOOTHING option)
 * 
 * \param[out]   *T                  - EOS_REAL : array of temperature data to be shifted
 * \param[out]   **P                 - EOS_REAL : array of pressure data to be shifted
 * \param[out]   *err                - EOS_INTEGER : error code
 * \param[out]   *errMsg             - EOS_CHAR** : custom error message
 * \param[in]    userDefinedDataFile - EOS_BOOLEAN : is the associated sesame file specified by the user?
 * \param[in]    dataFileIndex       - EOS_INTEGER : index of the sesame file for reading the data
 * \param[in]    nT                  - EOS_INTEGER : number of temperatures
 * \param[in]    *R                  - EOS_REAL : density data array
 * \param[in]    mat                 - EOS_INTEGER : material ID
 *
 * \return none
 *
 ***********************************************************************/
void _eos_AdjustDataTables (EOS_BOOLEAN userDefinedDataFile, EOS_INTEGER dataFileIndex,
			    EOS_INTEGER nT, EOS_REAL *R,
			    EOS_REAL *T, EOS_REAL **P, EOS_INTEGER mat, EOS_INTEGER *err,
			    EOS_CHAR **errMsg)
{
  EOS_INTEGER it;
  EOS_REAL avgAtomicNumber, avgAtomicWgt, refDensity, solidBulkModulus,
    exchangeCoeff, dpdr;

  *err = EOS_OK;

  /******************************************************/
  /* THIS IS IMPORTED FROM THE ORIGINAL LOCATION OF SESAME_READ_TABLE() */
  /* .....           do not allow zero density */

  if (R[0] <= ZERO && RSMALL > ZERO) {
    for (it = 0; it < nT; it++) {
      dpdr = (P[it][1] - P[it][0]) / (R[1] - R[0]);
      P[it][0] = P[it][0] + dpdr * (RSMALL - R[0]);
    }
    R[0] = RSMALL;
  }

  /* DAP: Find out out how this Temperature shift needs to be implemented.
     Using solidBulkModulus to shift Temperature makes NO SENSE,
     but it was interpreted from the SAGE User Manual description
     of matdef(3,mat). We may need to provide ANOTHER option like
     EOS_ADJUST_VAP_PRES so a Temperature shift value maybe provided
     from the host code.
  */
  return;

  /* .....           patch to shift temperatures */
  /* get solid bulk modulus */
  *err =
    eos_SesGetBulkData (mat, userDefinedDataFile, dataFileIndex,
			&avgAtomicNumber, &avgAtomicWgt, &refDensity,
			&solidBulkModulus, &exchangeCoeff, errMsg);
  if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) != EOS_OK) {
    // set custom message here "Could not read solid bulk modulus from 201 table!|);
    eos_SetCustomMsg_str(errMsg, "%s", "eos_RecordType1::eos_GetBulkData ERROR, bulk data not available");
    return;
  }

  if (solidBulkModulus > (EOS_REAL) ZERO) {
    _eos_DEBUG_PRINT
      ("\n Adjusting temperatures: mid, matdef(3) = %8i %12.4e\n", mat,
       solidBulkModulus);
    for (it = 0; it < nT; it++)
      T[it] = T[it] + solidBulkModulus;
  }
}

/***********************************************************************/
/*! 
 * \brief Makes data of class eos_RecordType1 pt smooth
 * 
 * \param[out]   *err - EOS_INTEGER : error code
 * \param[in]    *ptr - void : data object pointer;
 *                             internally recast to eos_RecordType1*
 * \param[in]    th   - EOS_INTEGER : table Handle
 * 
 * \return none
 *
 ***********************************************************************/
void eos_FixTableRecordType1 (void *ptr, EOS_INTEGER th, EOS_INTEGER *err)
{
  eos_RecordType1 *me;
  eos_RecordType2 *altEosData = NULL;
  EOS_INTEGER t, i, j, dt = EOS_401_DATA, objectIndex = 0;
  EOS_INTEGER alt_handle = -1;
  EOS_INTEGER nT401, vaporArrayOffset, table_type;
  // bulk data from 201 table
  EOS_REAL avgAtomicNumber401, adjustVapPres;   // mean atomic number
  EOS_REAL avgAtomicWgt401;     // mean atomic mass
  EOS_REAL refDensity401;       // normal solid density
  // Vapor Pressure
  EOS_REAL *P401;               /*[nT401] */
  // Temperature
  EOS_REAL *T401;               /*[nT401] */
  // Vapor Density on Coexistence Line
  EOS_REAL *RG401;              /*[nT401] */
  // Density of Liquid or Solid on Coexistence Line
  EOS_REAL *RL401;              /*[nT401] */
  // Internal Energy of Vapor on Coexistence Line
  EOS_REAL *EG401;              /*[nT401] */
  // Internal Energy of Liquid on Coexistence Line
  EOS_REAL *EL401;              /*[nT401] */
  // Free Energy of Vapor on Coexistence Line
  EOS_REAL *AG401;              /*[nT401] */
  // Free Energy of Liquid on Coexistence Line
  EOS_REAL *AL401;              /*[nT401] */
  EOS_BOOLEAN table_good, gen401;
  eos_OptionValue *optVal = NULL;
  EOS_CHAR *errMsg = NULL;
  extern EOS_BOOLEAN enable_DEBUG_PRINT;

  me = (eos_RecordType1 *) ptr;
  *err = EOS_OK;

  /* load 401 table */
  /* first construct an empty tbl w/o creating it (in case the table can't be created, we still want an empty RT2 */
  eos_CreateTablesEosDataMap (&gEosDataMap, 1, &dt, &me->eosData.materialID,
                              &alt_handle, EOS_FALSE, 0, EOS_FALSE, -1, err);

  if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) == EOS_OK && alt_handle >= 0
      && gEosDataMap.tableHandlesMap[alt_handle]) {
    altEosData =
      (eos_RecordType2 *) gEosDataMap.dataObjects[gEosDataMap.
                                                  tableHandlesMap
                                                  [alt_handle]];
    me->rt2_handle = alt_handle;
    /* save object index, because it might be thrown out if cant load the table */
    objectIndex = gEosDataMap.tableHandlesMap[alt_handle];

    /* now create the table */
    altEosData->eosData.Create (altEosData, alt_handle);
    /* inherit EOS_DEBUG_PRINT option setting */
    gEosDataMap.
      generalOptions[EOS_GENERAL_OPTION_FLAG_TO_INDEX (EOS_DEBUG_PRINT)]
      [alt_handle].bval = enable_DEBUG_PRINT;

    /* check for errors */
    *err = eos_GetErrorCodeEosDataMap (&gEosDataMap, alt_handle);
    if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) != EOS_OK) {
      /* dont invalidate the handle, we will fill the object with generated data */
      //eos_InvalidateHandle (alt_handle);
      // gEosDataMap.dataObjects[alt_handle] = NULL;
      nT401 = 0;
      P401 = T401 = RG401 = RL401 = EG401 = EL401 = AG401 = AL401 = NULL;
      avgAtomicNumber401 = avgAtomicWgt401 = refDensity401 = ZERO;
      me->found_401 = EOS_FALSE;        /* but can be reset to true by build_dome called by _eos_FixTable() */
    }
    else {                      /* created OK, load it! */

      /* now load the table */
      eos_LoadTablesEosDataMap (&gEosDataMap, 1, &alt_handle, err);
      if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) != EOS_OK) {
        nT401 = 0;
        P401 = T401 = RG401 = RL401 = EG401 = EL401 = AG401 = AL401 = NULL;
        me->found_401 = EOS_FALSE;      /* but can be reset to true by build_dome called by fix_table() */
      }
      else {
        /* assign pointers to 401 data */
        nT401 = altEosData->NT;
        avgAtomicNumber401 = altEosData->avgAtomicNumber;
        avgAtomicWgt401 = altEosData->avgAtomicWgt;
        refDensity401 = altEosData->refDensity;
        P401 = altEosData->P;
        T401 = altEosData->T;
        RG401 = altEosData->RG;
        RL401 = altEosData->RL;
        EG401 = altEosData->EG;
        EL401 = altEosData->EL;
        AG401 = altEosData->AG;
        AL401 = altEosData->AL;
        me->found_401 = EOS_TRUE;
      }
    }                           /* created OK, load load */
  }
  else {                        /* couldn't allocate space for a table in the data map! */

    nT401 = 0;
    P401 = T401 = RG401 = RL401 = EG401 = EL401 = AG401 = AL401 = NULL;
    me->found_401 = EOS_FALSE;
    return;                     /* this should never happen! */
  }

  /* temporarily add back the cold curve data if nessesary */
  for (j = 0; j < me->NT; j++) {
    for (i = 0; i < me->NR; i++) {
      if (me->table1 && me->coldCurve1)
        me->table1[j][i] = me->table1[j][i] + me->coldCurve1[i];
      if (me->table2 && me->coldCurve2)
        me->table2[j][i] = me->table2[j][i] + me->coldCurve2[i];
      if (me->table3 && me->coldCurve3)
        me->table3[j][i] = me->table3[j][i] + me->coldCurve3[i];
      if (me->table4 && me->coldCurve4)
        me->table4[j][i] = me->table4[j][i] + me->coldCurve4[i];
    }
  }

  /* get EOS_ADJUST_VAP_PRES setting for this eosData object */
  optVal = _eos_getOptionEosData (&(me->eosData), EOS_ADJUST_VAP_PRES);
  adjustVapPres = (optVal) ? optVal->rval : ZERO;

  /* temporarily convert units of Sesame data into CGS units */
  _eos_ConvertUnits (_EOS_SESAME_TO_CGS, me->NR, me->NT, me->R, me->T,
                     me->table1, me->table2, nT401, P401, T401, RG401, RL401,
                     EG401, EL401, AG401, AL401, &avgAtomicNumber401,
                     &avgAtomicWgt401, &refDensity401, &adjustVapPres, err, &errMsg);
  if (errMsg) *err = eos_SetCustomErrorMsg(th, *err, "%s", errMsg);
  EOS_FREE(errMsg);
  if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) != EOS_OK)
    return;

  _eos_CheckTable (me->NR, me->NT, me->R, me->T, me->table1, me->table2,
                   me->eosData.materialID, nT401, &table_type, &table_good);

  /* Adjust data tables: do not allow zero density and patch to shift temperatures */
  _eos_AdjustDataTables (me->eosData.userDefinedDataFile, me->eosData.dataFileIndex,
			 me->NT, me->R, me->T, me->table1, me->eosData.materialID, err, &errMsg);
  if (errMsg) *err = eos_SetCustomErrorMsg(th, *err, "%s", errMsg);
  EOS_FREE(errMsg);

  if (enable_DEBUG_PRINT)
    _eos_DumpData (0, me, "eospac_fix_table");  /* dump data arrays to file */

  /* Only the first 2 subtables are needed. */
  gen401 = me->found_401;       /* if not found, it will be generated and set to EOS_TRUE */
  _eos_FixTable (me->NR, me->NT, me->R, me->T, me->table1, me->table2,
                 me->eosData.materialID, me->coldCurve1, me->coldCurve2,
                 &nT401, &vaporArrayOffset, &avgAtomicNumber401,
                 &avgAtomicWgt401, &refDensity401, &P401, &T401, &RG401,
                 &RL401, &EG401, &EL401, &AG401, &AL401, table_type, &gen401,
                 adjustVapPres, err, &errMsg);
  if (errMsg) *err = eos_SetCustomErrorMsg(th, *err, "%s", errMsg);
  EOS_FREE(errMsg);
  if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) != EOS_OK)
    return;

  if (nT401 > 0 && !me->found_401) {    /* 401 was not found, but generated */
    me->found_401 = EOS_TRUE;
    // reverse the invalidation of me->rt2_handle, assign generated data.
    altEosData->eosData.isLoaded = 1;
    me->rt2_handle = alt_handle;
    gEosDataMap.tableHandlesMap[me->rt2_handle] = objectIndex;  /* not nessesary, it's never invalidated
                                                                   even if Create() fails, see above. Olga */
    altEosData->avgAtomicNumber = avgAtomicNumber401;
    altEosData->avgAtomicWgt = avgAtomicWgt401;
    altEosData->refDensity = refDensity401;
    altEosData->NT = nT401;
    altEosData->vaporArrayOffset = vaporArrayOffset;
    altEosData->P = P401;
    altEosData->T = T401;
    altEosData->RG = RG401;
    altEosData->RL = RL401;
    altEosData->EG = EG401;
    altEosData->EL = EL401;
    /* AG, AL aren't being set by Rage code, allocate and zero them out for consistency */
    altEosData->AG = (EOS_REAL *) malloc (sizeof (EOS_REAL) * altEosData->NT);
    altEosData->AL = (EOS_REAL *) malloc (sizeof (EOS_REAL) * altEosData->NT);
    for (t = 0; t < altEosData->NT; t++)
      altEosData->AG[t] = altEosData->AL[t] = ZERO;
    altEosData->eosData.isLoaded = 1;
    altEosData->eosData.numSubtablesLoaded = 7;
  }

  if (nT401 == 0)
    me->rt2_handle = -1;        /* reset handle */
  else
    altEosData->vaporArrayOffset = vaporArrayOffset;    /* set vaporArrayOffset since handle is valid */

  if (enable_DEBUG_PRINT)
    _eos_DumpData (1, me, "eospac_fix_table");  /* dump data arrays to file */

  /* now restore the original data:
     convert back to Sesame units and subtract cold curve */

  _eos_ConvertUnits (_EOS_CGS_TO_SESAME, me->NR, me->NT, me->R, me->T,
                     me->table1, me->table2, nT401, P401, T401, RG401, RL401,
                     EG401, EL401, AG401, AL401, &avgAtomicNumber401,
                     &avgAtomicWgt401, &refDensity401, &adjustVapPres, err, &errMsg);
  if (errMsg) *err = eos_SetCustomErrorMsg(th, *err, "%s", errMsg);
  EOS_FREE(errMsg);
  if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) != EOS_OK)
    return;

  for (j = 0; j < me->NT; j++) {
    for (i = 0; i < me->NR; i++) {
      if (me->table1 && me->coldCurve1)
        me->table1[j][i] = me->table1[j][i] - me->coldCurve1[i];
      if (me->table2 && me->coldCurve2)
        me->table2[j][i] = me->table2[j][i] - me->coldCurve2[i];
      if (me->table3 && me->coldCurve3)
        me->table3[j][i] = me->table3[j][i] - me->coldCurve3[i];
      if (me->table4 && me->coldCurve4)
        me->table4[j][i] = me->table4[j][i] - me->coldCurve4[i];
    }
  }                             /* restore cold curve */
}

/***********************************************************************/
/*! 
 * \brief Makes data of class eos_RecordType1 smooth
 * 
 * \param[out]   *err     - EOS_INTEGER : error code
 * \param[in]     *ptr    - void : data object pointer;
 *                                 internally recast to eos_RecordType1*
 * \param[in]    th       - EOS_INTEGER : table Handle
 * \param[in]    dataType - EOS_INTEGER : data type of *ptr eos_RecordType1 object
 * \param[in]    ptSmooth - EOS_BOOLEAN : is data  of *ptr eos_RecordType1 object to
 *                                        be made smooth according to EOS_PT_SMOOTHING
 *                                        option?
 * 
 * \return none
 *
 ***********************************************************************/
void eos_MakeSmoothRecordType1 (void *ptr, EOS_INTEGER th,
                                EOS_INTEGER dataType, EOS_BOOLEAN ptSmooth,
                                EOS_INTEGER *err)
{
  eos_RecordType1 *me;
  EOS_INTEGER subTableNum = EOS_TYPE_TO_SUB_TAB_NUM (dataType);
  EOS_REAL *table;

  me = (eos_RecordType1 *) ptr;
  *err = EOS_OK;

  switch (subTableNum) {
  case 1:
    table = &(me->table1[0][0]);
    break;
  case 2:
    table = &(me->table2[0][0]);
    break;
  case 3:
    table = &(me->table3[0][0]);
    break;
  case 4:
    table = &(me->table4[0][0]);
    break;
  }

  /* if PT_SMOOTH, load the 401 table */
  if (ptSmooth) {
    eos_FixTableRecordType1 (ptr, th, err);
    if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) != EOS_OK)
      return;
  }
  else {
    if (me->eosData.numSubtablesLoaded >= subTableNum)
      *err = _eos_MakeSmooth (me->NR, me->NT, me->R, me->T, table);
    if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) != EOS_OK)
      return;
  }
}

/***********************************************************************/
/*!
 * \brief determines splitting options for me:
 *    1. the cowan-nuclear model (la-7313-ms, and T-1's Grizzly code)
 *    2. the ideal gas model
 *    3. the Number-proportional model
 *
 * \param[out]   *imodel        - EOS_INTEGER : desired model option flag
 * \param[out]   **optValForced - eos_OptionValue : is the model to be forced?
 * \param[in]    *me            - eos_RecordType1 : data object pointer;
 *
 * \return eos_GetAnalyticalEOSFlag - EOS_INTEGER : error code
 *
 ***********************************************************************/
EOS_INTEGER eos_GetAnalyticalEOSFlags (eos_RecordType1 *me,
                                       EOS_INTEGER *imodel,
                                       eos_OptionValue **optValForced)
{
  eos_OptionValue *optVal = NULL;
  EOS_INTEGER ierr = EOS_OK;

  *optValForced = _eos_getOptionEosData (&(me->eosData), EOS_SPLIT_FORCED);
  optVal = _eos_getOptionEosData (&(me->eosData), EOS_SPLIT_COWAN);
  *imodel = (optVal && optVal->bval == EOS_TRUE) ? EOS_SPLIT_COWAN : *imodel;
  optVal->bval = EOS_FALSE;
  optVal = _eos_getOptionEosData (&(me->eosData), EOS_SPLIT_IDEAL_GAS);
  *imodel = (optVal
             && optVal->bval == EOS_TRUE) ? EOS_SPLIT_IDEAL_GAS : *imodel;
  optVal->bval = EOS_FALSE;
  optVal = _eos_getOptionEosData (&(me->eosData), EOS_SPLIT_NUM_PROP);
  *imodel = (optVal
             && optVal->bval == EOS_TRUE) ? EOS_SPLIT_NUM_PROP : *imodel;
  optVal->bval = EOS_FALSE;

  return (ierr);
}

/***********************************************************************/
/*!
 * \brief Calculate appropriate data for requested table (303, 304 or 305)
 *  given the data calculated by an ionic model and the total EOS data.
 *
 * \param[out]   *Fion    - EOS_REAL : Appropriately adjusted datum
 * \param[in]    *me      - eos_RecordType1 : pointer to the instance of type eos_RecordType1
 * \param[in]    *Ftot    - EOS_REAL : total EOS datum
 * \param[in]    *Fc      - EOS_REAL : Cold curve datum (pointer maybe NULL)
 * \param[in]    ccLoaded - EOS_INTEGER : Is cold curve stored separately from Ftot?
 *
 * \return eos_SetAdjustedData - EOS_INTEGER : error code
 *
 ***********************************************************************/
EOS_INTEGER eos_SetAdjustedData (eos_RecordType1 *me,
                                 EOS_REAL *Fion, EOS_REAL *Ftot, EOS_REAL *Fc,
                                 EOS_INTEGER ccLoaded)
{
  EOS_REAL Fion_local, Fcold;
  Fcold = (Fc) ? *Fc : ZERO;
  if (ccLoaded)
    Fion_local = MIN (*Ftot + Fcold, (Fcold + *Fion));
  else
    Fion_local = MIN (*Ftot, (Fcold + *Fion));
  if (me->eosData.tableNum == 303) {    /* add cold curve data to Ion data */
    *Fion = Fion_local;
  }
  if (me->eosData.tableNum == 304) {    /* subtract Ion and cold curve from total */
    *Fion = (ccLoaded) ? *Ftot + Fcold - Fion_local : *Ftot - Fion_local;
  }
  if (me->eosData.tableNum == 305) { /* constrain consistently with 303 and 304 tableNum */
    *Fion = Fion_local - Fcold;
  }
  return 0;
}

/***********************************************************************/
/*!
 * \brief Compute the ideal-gas pressure, internal energy, entropy, and Helmholtz
 *        free energy (monatomic gas assumed)
 *
 * \param[out]   p   - EOS_REAL : ion pressure (Gj/m3); me->NR x me->NT elements
 * \param[out]   e   - EOS_REAL : internal energy/mass (Gj/Mg); me->NR x me->NT elements
 * \param[out]   a   - EOS_REAL : free energy/mass (Gj/Mg); me->NR x me->NT elements
 * \param[out]   s   - EOS_REAL : entropy/mass (Gj/Mg/K); me->NR x me->NT elements
 *
 * \param[in]    *me - eos_RecordType1 : pointer to the instance of type eos_RecordType1
 * \param[in]    *altEosData - eos_RecordType1 : pointer to the instance of type
 *                                               eos_RecordType1, which contains 301 data
 *
 * \return eos_computeIdealGasData - EOS_INTEGER : error code
 *
 ***********************************************************************/
EOS_INTEGER eos_computeIdealGasData (eos_RecordType1 *me, eos_RecordType1 *altEosData,
				     EOS_REAL *p, EOS_REAL *e, EOS_REAL *a, EOS_REAL *s)
{
  int i, j, ccLoaded;
  EOS_INTEGER ierr = EOS_OK;
  EOS_REAL *prtot, *entot, *prtot_cc, *entot_cc, *antot, *antot_cc;

  ccLoaded = altEosData->eosData.coldCurveIsLoaded;
  prtot = &(altEosData->table1[0][0]);
  prtot_cc = (ccLoaded) ? altEosData->coldCurve1 : NULL;
  entot = &(altEosData->table2[0][0]);
  entot_cc = (ccLoaded) ? altEosData->coldCurve2 : NULL;
  antot = NULL;
  antot_cc = NULL;
  if (altEosData->eosData.numSubtablesLoaded >= 3) {
    /* Total Helmholtz free energy data exists, subtable #3 */
    antot = &(altEosData->table3[0][0]);
    antot_cc = (ccLoaded) ? altEosData->coldCurve3 : NULL;
  }

  // compute ideal-gas pressure, internal energy, entropy, and Helmholtz free energy (monatomic gas assumed)
  for (i = 0; i < me->NR; i++) {
    for (j = 0; j < me->NT; j++) {

      p[i + j * me->NR] = UNIVERSAL_GAS_CONST / me->avgAtomicWgt * altEosData->T[j] * altEosData->R[i];
      e[i + j * me->NR] =
	((EOS_REAL) 1.5) * UNIVERSAL_GAS_CONST / me->avgAtomicWgt * altEosData->T[j];

      if (altEosData->R[i] > ZERO && altEosData->T[j] > ZERO) {
	/* A and S are prescribed after switch statement for altEosData->R=0 */

	/*! - Helmholtz free energy equation taken from T-1's opensesame (was Grizzly). */
	a[i + j * me->NR] = -UNIVERSAL_GAS_CONST * altEosData->T[j] / me->avgAtomicWgt *
	  ((EOS_REAL) (-7.072343) +
	   ((EOS_REAL) (1.5)) * log (me->avgAtomicWgt * altEosData->T[j]) +
	   log (me->avgAtomicWgt / altEosData->R[i]));

	/* calculate entropy */
	s[i + j * me->NR] = (altEosData->T[j] <= ZERO) ? ZERO
	  : (e[i + j * me->NR] -
	     a[i + j * me->NR]) / UNIVERSAL_GAS_CONST / altEosData->T[j] * me->avgAtomicWgt;
      }
      else {
	a[i + j * me->NR] = ZERO;
	s[i + j * me->NR] = ZERO;
      }

      /* Using total 301 table data, calculate appropriate data for requested table: 303, 304 or 305 */
      ierr =
	eos_SetAdjustedData (me, &p[i + j * me->NR],
			     &prtot[i + j * me->NR],
			     ((prtot_cc) ? &prtot_cc[i] : &prtot[i]),
			     ccLoaded);
      ierr =
	eos_SetAdjustedData (me, &e[i + j * me->NR],
			     &entot[i + j * me->NR],
			     ((entot_cc) ? &entot_cc[i] : &entot[i]),
			     ccLoaded);
      if (antot) {
	ierr =
	  eos_SetAdjustedData (me, &a[i + j * me->NR],
			       &antot[i + j * me->NR],
			       ((antot_cc) ? &antot_cc[i] : &antot[i]),
			       ccLoaded);
      }
      else {                  /* A = U - TS; therefore, A = U at T=0 */
	ierr =
	  eos_SetAdjustedData (me, &a[i + j * me->NR],
			       &entot[i + j * me->NR],
			       ((entot_cc) ? &entot_cc[i] : &entot[i]),
			       ccLoaded);
      }
      /* s[i+j*me->NR] need not be adjusted since S=0 at T=0 */
    }
  }

  return ierr;
}

/***********************************************************************/
/*!
 * \brief Compute the Cowan pressure, internal energy, entropy, and Helmholtz
 *        free energy
 *
 * \param[out]   p   - EOS_REAL : ion pressure (Gj/m3); me->NR x me->NT elements
 * \param[out]   e   - EOS_REAL : internal energy/mass (Gj/Mg); me->NR x me->NT elements
 * \param[out]   a   - EOS_REAL : free energy/mass (Gj/Mg); me->NR x me->NT elements
 * \param[out]   s   - EOS_REAL : entropy/mass (Gj/Mg/K); me->NR x me->NT elements
 *
 * \param[in]    *me - eos_RecordType1 : pointer to the instance of type eos_RecordType1
 * \param[in]    *altEosData - eos_RecordType1 : pointer to the instance of type
 *                                               eos_RecordType1, which contains 301 data
 *
 * \return eos_computeCowanData - EOS_INTEGER : error code
 *
 ***********************************************************************/
EOS_INTEGER eos_computeCowanData (eos_RecordType1 *me, eos_RecordType1 *altEosData,
				  EOS_REAL *p, EOS_REAL *e, EOS_REAL *a, EOS_REAL *s)
{
  int i, j, ccLoaded;
  EOS_INTEGER ierr = EOS_OK;
  EOS_REAL *prtot, *entot, *prtot_cc, *entot_cc, *antot, *antot_cc;
  EOS_REAL xi, rxi, rxi2, beta, entropy_factor, tcowan;
  EOS_REAL tmelt, phif3, phif, gamf, gams, gdens;
  EOS_REAL tdebye, phis, phis2, rphis, xphis, eterm;

  static const EOS_REAL third = (EOS_REAL) 1 / (EOS_REAL) 3;
  static const EOS_REAL third2 = (EOS_REAL) 1 / (EOS_REAL) 9;

  ccLoaded = altEosData->eosData.coldCurveIsLoaded;
  prtot = &(altEosData->table1[0][0]);
  prtot_cc = (ccLoaded) ? altEosData->coldCurve1 : NULL;
  entot = &(altEosData->table2[0][0]);
  entot_cc = (ccLoaded) ? altEosData->coldCurve2 : NULL;
  antot = NULL;
  antot_cc = NULL;
  if (altEosData->eosData.numSubtablesLoaded >= 3) {
    /* Total Helmholtz free energy data exists, subtable #3 */
    antot = &(altEosData->table3[0][0]);
    antot_cc = (ccLoaded) ? altEosData->coldCurve3 : NULL;
  }

  for (i = 0; i < me->NR; i++) {
    for (j = 0; j < me->NT; j++) {
      /*  initialize a */

      /*! - use ideal gas model to initialize values of P and U */
      p[i + j * me->NR] = UNIVERSAL_GAS_CONST / me->avgAtomicWgt * altEosData->T[j] * altEosData->R[i];
      e[i + j * me->NR] =
	((EOS_REAL) 1.5) * UNIVERSAL_GAS_CONST / me->avgAtomicWgt * altEosData->T[j];

      /*! - initialize values of A=U and S=0 for T=0 or S=(A-U)/T for T!=0 */
      a[i + j * me->NR] = e[i + j * me->NR];
      if (altEosData->T[i] <= ZERO)
	s[i + j * me->NR] = ZERO;
      else
	s[i + j * me->NR] = - (a[i + j * me->NR] - e[i + j * me->NR]) / altEosData->T[j];

      if (altEosData->R[i] <= ZERO)
	continue;

      //     compute useful constants.
      xi = altEosData->R[i] / (third2 * me->avgAtomicWgt *
			       (EOS_REAL) pow (me->avgAtomicNumber, (EOS_REAL) (-0.3)));
      rxi = ONE / (ONE + xi);
      rxi2 = rxi * rxi;
      beta = (EOS_REAL) 0.6 *(EOS_REAL) pow (me->avgAtomicNumber, third2);
      tmelt = EV_TO_KELVIN * (EOS_REAL) 0.32 *rxi2 * rxi2 *
	(EOS_REAL) pow (xi, (THREE + third + beta + beta));

      if (altEosData->T[j] > tmelt) {
	// compute fluid eos.
	phif3 = tmelt / altEosData->T[j];
	if (phif3 > (EOS_REAL) 1.e-18) {
	  // compute scaled eos.
	  phif = (EOS_REAL) pow (phif3, third);
	  gamf = THREE * beta - ONE + SIX * rxi;
	  p[i + j * me->NR] =
	    UNIVERSAL_GAS_CONST / me->avgAtomicWgt * altEosData->T[j] * altEosData->R[i] * (ONE +
											    gamf * phif);
	  e[i + j * me->NR] =
	    UNIVERSAL_GAS_CONST / me->avgAtomicWgt * altEosData->T[j] * ((EOS_REAL) 1.5) * (ONE +
											    phif);

	  /*! - Entropy and Helmholtz free energy equations taken from T-1's opensesame (was Grizzly). */
	  tcowan = (EOS_REAL) 0.42 / ((EOS_REAL) 22.0 + me->avgAtomicNumber);
	  entropy_factor = SEVEN - (THREE * phif + ((EOS_REAL) 1.5) *
				    log (altEosData->T[j] / EV_TO_KELVIN *
					 ((EOS_REAL) 0.02) / (tcowan * tcowan))) -
	    log (xi);
	  s[i + j * me->NR] = entropy_factor * UNIVERSAL_GAS_CONST / me->avgAtomicWgt; /* MJ/kg/K */
	  a[i + j * me->NR] = e[i + j * me->NR] - altEosData->T[j] * s[i + j * me->NR];
	}
      }
      else {
	// compute debye-solid eos.
	gams = beta + (TWO) * rxi;
	gdens = gams * altEosData->R[i];
	tdebye =
	  EV_TO_KELVIN * (EOS_REAL) 1.68 *rxi2 * (EOS_REAL) pow (xi,
								 (TWO +
								  beta)) /
	  (me->avgAtomicNumber + (EOS_REAL) 22);
	if (altEosData->T[j] > (tdebye * third)) {
	  //  compute classical debye eos.
	  phis = tdebye / altEosData->T[j];
	  phis2 = phis * phis;
	  e[i + j * me->NR] =
	    THREE * UNIVERSAL_GAS_CONST / me->avgAtomicWgt * altEosData->T[j] * (ONE +
										 phis2 *
										 ((EOS_REAL) 0.05 -
										  phis2 * ONE /
										  (EOS_REAL)
										  1680));
	  p[i + j * me->NR] = e[i + j * me->NR] * gdens;

	  /* Entropy equation taken from T-1's opensesmame (was Grizzly): */
	  entropy_factor = FOUR + THREE * (-log (phis) +
					   phis2 * ((EOS_REAL) 0.025 -
						    phis2 / (EOS_REAL) 2240));
	  s[i + j * me->NR] = entropy_factor * UNIVERSAL_GAS_CONST / me->avgAtomicWgt; /* MJ/kg/K */
	  a[i + j * me->NR] = e[i + j * me->NR] - altEosData->T[j] * s[i + j * me->NR];
	}
	else {
	  // compute quantum debye eos.
	  rphis = altEosData->T[j] / tdebye;
	  phis = ZERO;
	  xphis = ZERO;
	  if (rphis > ZERO)
	    phis = ONE / rphis;
	  if (rphis > (((EOS_REAL) 0.01) * third))
	    xphis = (EOS_REAL) exp (-phis);
	  eterm =
	    ((EOS_REAL) pow (pi, (EOS_REAL) 3) / FIVE * THREE) * rphis *
	    rphis * rphis - (NINE +
			     (EOS_REAL) 27 * rphis * (ONE +
						      rphis * (TWO + rphis +
							       rphis))) *
	    xphis;
	  e[i + j * me->NR] =
	    ((EOS_REAL) 1.125) * UNIVERSAL_GAS_CONST / me->avgAtomicWgt * tdebye +
	    UNIVERSAL_GAS_CONST / me->avgAtomicWgt * altEosData->T[j] * eterm;
	  p[i + j * me->NR] = e[i + j * me->NR] * gdens;

	  /* Entropy equation taken from T-1's opensesmame (was Grizzly): */
	  if (phis > ZERO) {
	    entropy_factor = FOUR *
	      ((EOS_REAL) pow (pi, (EOS_REAL) 4) / FIVE * rphis * rphis * rphis -
	       (NINE / FOUR + NINE * rphis + NINE * TWO * rphis * rphis +
		NINE * TWO * rphis * rphis * rphis) * xphis);
	  }
	  else {
	    entropy_factor = ZERO;
	  }

	  s[i + j * me->NR] = entropy_factor * UNIVERSAL_GAS_CONST / me->avgAtomicWgt; /* MJ/kg/K */
	  a[i + j * me->NR] = e[i + j * me->NR] - altEosData->T[j] * s[i + j * me->NR];
	}
      }

      /* Using total 301 table data, calculate appropriate data for requested table: 303, 304 or 305 */
      ierr =
	eos_SetAdjustedData (me, &p[i + j * me->NR],
			     &prtot[i + j * me->NR],
			     ((prtot_cc) ? &prtot_cc[i] : &prtot[i]),
			     ccLoaded);
      ierr =
	eos_SetAdjustedData (me, &e[i + j * me->NR],
			     &entot[i + j * me->NR],
			     ((entot_cc) ? &entot_cc[i] : &entot[i]),
			     ccLoaded);
      if (antot) {
	ierr =
	  eos_SetAdjustedData (me, &a[i + j * me->NR],
			       &antot[i + j * me->NR],
			       ((antot_cc) ? &antot_cc[i] : &antot[i]),
			       ccLoaded);
      }
      else {                  /* A = U - TS; therefore, A = U at T=0 */
	ierr =
	  eos_SetAdjustedData (me, &a[i + j * me->NR],
			       &entot[i + j * me->NR],
			       ((entot_cc) ? &entot_cc[i] : &entot[i]),
			       ccLoaded);
      }
      /* s[i+j*me->NR] need not be adjusted since S=0 at T=0 */
    }
  }

  return ierr;
}
 
/***********************************************************************/
/*!
 * \brief Compute the number-proportional pressure, internal energy, entropy, and Helmholtz
 *        free energy
 *
 * \param[out]   p   - EOS_REAL : ion pressure (Gj/m3); me->NR x me->NT elements
 * \param[out]   e   - EOS_REAL : internal energy/mass (Gj/Mg); me->NR x me->NT elements
 * \param[out]   a   - EOS_REAL : free energy/mass (Gj/Mg); me->NR x me->NT elements
 * \param[out]   s   - EOS_REAL : entropy/mass (Gj/Mg/K); me->NR x me->NT elements
 *
 * \param[in]    *me - eos_RecordType1 : pointer to the instance of type eos_RecordType1
 * \param[in]    *altEosData - eos_RecordType1 : pointer to the instance of type
 *                                               eos_RecordType1, which contains 301 data
 *
 * \return eos_computeNumPropData - EOS_INTEGER : error code
 *
 ***********************************************************************/
EOS_INTEGER eos_computeNumPropData (eos_RecordType1 *me, eos_RecordType1 *altEosData,
				    EOS_REAL *p, EOS_REAL *e, EOS_REAL *a, EOS_REAL *s)
{
  int i, j, ccLoaded;
  EOS_INTEGER ierr = EOS_OK, ierr2 = EOS_OK;
  EOS_REAL *prtot, *entot, *prtot_cc, *entot_cc, *antot, *antot_cc;
  EOS_REAL mult;
  EOS_REAL pcold, ecold, acold;
  EOS_REAL z0, z1, zf, zl, zh, zfmin, fz, *zf_arr;
  EOS_INTEGER it;
  EOS_REAL    _tolerance = 1.0e-6;
  EOS_INTEGER _maxiter   = 10000;

  /* fermi-gas constant (2.*kboltz*(2.*pi*me*kboltz/hplanck**2)**1.5
     in Gj/m**3/kelvin**2.5). */
  static const EOS_REAL cfermi = (EOS_REAL) 6.6677e-11;

  ccLoaded = altEosData->eosData.coldCurveIsLoaded;
  prtot = &(altEosData->table1[0][0]);
  prtot_cc = (ccLoaded) ? altEosData->coldCurve1 : NULL;
  entot = &(altEosData->table2[0][0]);
  entot_cc = (ccLoaded) ? altEosData->coldCurve2 : NULL;
  antot = NULL;
  antot_cc = NULL;
  if (altEosData->eosData.numSubtablesLoaded >= 3) {
    /* Total Helmholtz free energy data exists, subtable #3 */
    antot = &(altEosData->table3[0][0]);
    antot_cc = (ccLoaded) ? altEosData->coldCurve3 : NULL;
  }

  // compute ideal-gas pressure, internal energy, entropy, and Helmholtz free energy
  /* allocate temporary array to store zf values */
  zf_arr = (EOS_REAL *) malloc (sizeof (EOS_REAL) * me->NR * me->NT);

  for (i = 0; i < me->NR; i++) {

    if (ccLoaded) {
      pcold = prtot_cc[i];
      ecold = entot_cc[i];
      mult = ZERO;
    }
    else {
      pcold = prtot[i];
      ecold = entot[i];
      mult = ONE;
    }

    zfmin = ZERO;
    for (j = 0; j < me->NT; j++) {
      /* use ideal gas model as initial values of P and U */
      p[i + j * me->NR] = UNIVERSAL_GAS_CONST / me->avgAtomicWgt * altEosData->T[j] * altEosData->R[i];
      e[i + j * me->NR] =
	((EOS_REAL) 1.5) * UNIVERSAL_GAS_CONST / me->avgAtomicWgt * altEosData->T[j];

      /* initialize zf value to zero */
      zf_arr[i + j * me->NR] = ZERO;

      if (p[i + j * me->NR] <= ZERO)
	continue;

      z0 =
	(EOS_REAL) MAX (ONE,
			(prtot[i + j * me->NR] -
			 pcold * mult) / (UNIVERSAL_GAS_CONST / me->avgAtomicWgt *
					  altEosData->T[j] * altEosData->R[i]));
      /*! - Impose an upperbound on z0, which is physically-consistent
       *    with the tabulated average atomic number.
       */
      z0 = MIN(z0, me->avgAtomicNumber);

      z1 =
	cfermi * altEosData->T[j] * altEosData->T[j] * pow (altEosData->T[j],
							    ONE / TWO) / (UNIVERSAL_GAS_CONST /
									  me->avgAtomicWgt * altEosData->T[j] *
									  altEosData->R[i]);
      it = 0;

      zl = zfmin;
      zh = z0-1;
      /*! - old, imprecise initialization of zh (changed for artf7471):
       *  zh = me->avgAtomicNumber;
       *  This was removed to ensure algorithm convergence in spite of noisy
       *  Sesame data. The original assumption was that Z was monotonically-
       *  increasing with respect to temperature, but some Sesame data
       *  fails this physically-correct assumption.
       */
      // iterate to convergence using successive bisection.
      do {
	it++;
	zf = ONE / TWO * (zl + zh);
	fz = zf / z1;
	fz =
	  ONE + fz * ((EOS_REAL) 0.88388 +
		      fz * ((EOS_REAL) 0.37208 +
			    (EOS_REAL) 0.02645 * (pow (fz, FOUR / THREE))));
	fz = (EOS_REAL) (z0 * (pow (fz, (EOS_REAL) (-ONE / FIVE))) - ONE);
	if (zf > fz) {
	  zh = zf;
	}
	else {
	  zl = zf;
	}
      } while ((ABS (zf - fz) > (_tolerance * zf)) && (it < _maxiter));

      /* set alternative ierr2 code if convergence failed */
      if ((ABS (zf - fz) > (_tolerance * zf)) || (it >= _maxiter)) {

	ierr2 =EOS_SPLIT_FAILED;
      }

      /* store zf value for calculating Helmholtz Energy below */
      zf_arr[i + j * me->NR] = zf;

      // calculate ion data
      p[i + j * me->NR] =
	(prtot[i + j * me->NR] - pcold * mult) / (ONE + zf);
      e[i + j * me->NR] =
	(entot[i + j * me->NR] - ecold * mult) / (ONE + zf);
      //changes for artf7471
      //zfmin = zf;
    }
  }

  /* Now, calculate the ion Helmholtz free energy and entropy data */
  if (altEosData->eosData.numSubtablesLoaded <= 2) {
    /* Free energy table not in Sesame file */

    /* calculate entropy */
    ierr = eos_Entropy (me, me->NR, me->NT, e, EOS_NullPtr, altEosData->T, altEosData->R, s);
    if (eos_GetStandardErrorCodeFromCustomErrorCode(ierr) != EOS_OK) {
      if (ierr == -2 || ierr == -3) {
	/* ignore the error, just set the dataSize and numTables loaded */
	me->eosData.dataSize = 2 + me->NR + me->NT + 2 * me->NR * me->NT;     /* only TWO subtables */
	me->eosData.numSubtablesLoaded = 2;
	ierr = EOS_OK;
      }
      else
	return ierr;
    }
    else {
      /* calculate Helmholtz free energy */
      for (i = 0; i < me->NR; i++) {
	for (j = 0; j < me->NT; j++) {
	  a[i + j * me->NR] = e[i + j * me->NR] - altEosData->T[j] * s[i + j * me->NR];
	}
      }
    }
  }
  else {

    /* calculate Helmholtz free energy */
    for (i = 0; i < me->NR; i++) {
      acold = antot[i];
      for (j = 0; j < me->NT; j++) {
	a[i + j * me->NR] =
	  (antot[i + j * me->NR] - mult * acold) /
	  (ONE + zf_arr[i + j * me->NR]);
      }
    }

    /* calculate entropy */
    ierr = eos_Entropy (me, me->NR, me->NT, e, a, altEosData->T, altEosData->R, s);
    if (eos_GetStandardErrorCodeFromCustomErrorCode(ierr) != EOS_OK) {
      if (ierr == -2 || ierr == -3) {
	/* ignore the error, just set the dataSize and numTables loaded */
	me->eosData.dataSize = 2 + me->NR + me->NT + 2 * me->NR * me->NT;     /* only TWO subtables */
	me->eosData.numSubtablesLoaded = 3;
	ierr = EOS_OK;
      }
      else
	return ierr;
    }
  }

  /* Using total 301 table data, calculate appropriate data for requested table: 303, 304 or 305 */
  for (i = 0; i < me->NR; i++) {
    for (j = 0; j < me->NT; j++) {
      pcold = (prtot_cc) ? prtot_cc[i] : ((prtot) ? prtot[i] : ZERO);
      ecold = (entot_cc) ? entot_cc[i] : ((entot) ? entot[i] : ZERO);
      acold = (antot_cc) ? antot_cc[i] : ((antot) ? antot[i] : ZERO);

      ierr =
	eos_SetAdjustedData (me, &p[i + j * me->NR],
			     &prtot[i + j * me->NR], &pcold, ccLoaded);
      ierr =
	eos_SetAdjustedData (me, &e[i + j * me->NR],
			     &entot[i + j * me->NR], &ecold, ccLoaded);
      if (antot) {
	ierr =
	  eos_SetAdjustedData (me, &a[i + j * me->NR],
			       &antot[i + j * me->NR],
			       ((antot_cc) ? &antot_cc[i] : &antot[i]),
			       ccLoaded);
      }
      else {                  /* A = U - TS; therefore, A = U at T=0 */
	ierr =
	  eos_SetAdjustedData (me, &a[i + j * me->NR],
			       &entot[i + j * me->NR],
			       ((entot_cc) ? &entot_cc[i] : &entot[i]),
			       ccLoaded);
      }
      /* s[i+j*me->NR] need not be adjusted since S=0 at T=0 */
    }
  }

  /* deallocate temporary array */
  EOS_FREE(zf_arr);

  return (ierr) ? ierr : ierr2;
}

/***********************************************************************/
/*!
 * \brief Determines ion pressure, internal energy, and free energy using one of the following:
 *    - the cowan-nuclear model (
 *      <a href="http://lib-www.lanl.gov/cgi-bin/getfile?00309855.pdf" target=_blank>la-7313-ms</a>,
 *      and T-1's Grizzly code)
 *    - the ideal gas model
 *    - the Number-proportional model
 *
 * \param[out]   read_data - EOS_REAL : locally allocated array containing
 *                                         density (Mg/m3), temperature (kelvin),
 *                                         ion pressure (Gj/m3),
 *                                         internal energy/mass (Gj/Mg), and
 *                                         free energy/mass (Gj/Mg)
 * \param[out]   **errMsg  - EOS_CHAR** : custom error message
 * \param[in]    *me       - eos_RecordType1 : pointer to the instance of type eos_RecordType1
 * \param[in]    imodel    - EOS_INTEGER : desired model option flag
 *
 * \return eos_AnalyticalEOS - EOS_INTEGER : error code
 *
 ***********************************************************************/
EOS_INTEGER eos_AnalyticalEOS (eos_RecordType1 *me, const EOS_INTEGER imodel,
                               EOS_REAL **read_data, EOS_CHAR **errMsg)
{
  EOS_REAL *dens, *temp, *prion, *enion, *anion, *s_ion;
  EOS_INTEGER i, j, datatype;
  EOS_INTEGER alt_handle;
  EOS_INTEGER ierr = EOS_OK, ierr2 = EOS_OK;
  eos_RecordType1 *altEosData = NULL;
  static EOS_CHAR *eos_IdealGasStr = "Analytical Ideal Gas Model";
  static EOS_CHAR *eos_CowanStr = "Analytical Cowan Model";
  static EOS_CHAR *eos_NumPropStr = "Analytical Number-Proportional Model";

  //#define WRITE_FD2 fprintf
#ifdef WRITE_FD2
  FILE *fd2;
#endif

  // create the internal data objects for 301 table
  alt_handle = -1;
  datatype = EOS_301_DATA;
  // load density and temperature data from 301 table
  eos_CreateTablesEosDataMap (&gEosDataMap, 1, &datatype,
                              &me->eosData.materialID, &alt_handle, EOS_TRUE,
                              0, EOS_FALSE, -1, &ierr);
  if (ierr)
    return (ierr);
  altEosData =
    (eos_RecordType1 *) gEosDataMap.dataObjects[gEosDataMap.
                                                tableHandlesMap[alt_handle]];

  // Cannot assume NR & NT are same for 301, 303, 304, 305 ... ; thus, reallocation
  // of 'me' may be needed.
  if (me->NR != altEosData->NR || me->NT != altEosData->NT)
    eos_SetSizeRecordType1 (me, altEosData->NR, altEosData->NT, me->eosData.tableNum);
  me->NR = altEosData->NR;
  me->NT = altEosData->NT;
  // store the total size of the data to be stored for all subtables:
  // NR, NT, R[], T[], table1[], table2[], table3[], and table4[]
  me->eosData.dataSize = 2 + me->NR + me->NT + MAX_TABLES_RECORDTYPE1 * me->NR * me->NT;

  // load the total pressure, the total internal energy,
  // and (optionally) the total Helmholtz free energy data from 301 table
  eos_LoadTablesEosDataMap (&gEosDataMap, 1, &alt_handle, &ierr);
  if (ierr)
    return (ierr);

  /* return error if insufficient data */
  if (altEosData->T[0] > 0.0 && altEosData->eosData.numSubtablesLoaded < 3) {
    ierr = EOS_SPLIT_FAILED;
    eos_SetCustomMsg_str(errMsg,
			 "EOS_SPLIT_FAILED: The data splitting algorithm failed because T[0]>0 and no free energy table exists.");
    return (ierr);
  }

  // reallocate read_data to store four subtables: prion, enion, anion, s_ion
  *read_data =
    realloc (*read_data, (me->eosData.dataSize - 2) * sizeof (EOS_REAL));
  dens = &((*read_data)[0]);
  temp = &((*read_data)[me->NR]);
  for (i = 0; i < me->NR; i++)
    dens[i] = altEosData->R[i];
  for (i = 0; i < me->NT; i++)
    temp[i] = altEosData->T[i];
  prion = &((*read_data)[me->NR + me->NT]);
  enion = &((*read_data)[me->NR + me->NT + me->NR * me->NT]);
  anion = &((*read_data)[me->NR + me->NT + 2 * me->NR * me->NT]);
  s_ion = &((*read_data)[me->NR + me->NT + 3 * me->NR * me->NT]);

  // allocate memory for me (i.e., eos_RecordType1) if it has not yet been done
  if (!
      (me->R && me->T && me->table1 && me->table2 && me->table3
       && me->table4))
    eos_SetSizeRecordType1 (me, me->NR, me->NT, me->eosData.tableNum);

  switch (imodel) {
  case EOS_SPLIT_IDEAL_GAS:

    /*! case EOS_SPLIT_IDEAL_GAS: Ideal-Gas data splitting model details */

    ierr = eos_computeIdealGasData (me, altEosData, prion, enion, anion, s_ion);

    /* store the description of this alternative data source */
    me->eosData.altDataSource = eos_IdealGasStr;
    break;

  case EOS_SPLIT_COWAN:

    /*! case EOS_SPLIT_COWAN: Cowan-Nuclear data splitting model details */

    ierr = eos_computeCowanData (me, altEosData, prion, enion, anion, s_ion);

    /* store the description of this alternative data source */
    me->eosData.altDataSource = eos_CowanStr;
    break;

  case EOS_SPLIT_NUM_PROP:

    /*! case EOS_SPLIT_NUM_PROP: Number Proportional data splitting model details */

    ierr = eos_computeNumPropData (me, altEosData, prion, enion, anion, s_ion);

    /* store the description of this alternative data source */
    me->eosData.altDataSource = eos_NumPropStr;
    break;

  default:
    return (EOS_INVALID_SPLIT_FLAG);
    break;
  }

  if (dens[0] <= 0) {
    /* Reset values of U, A and S at dens=0  to be consistent with
     * how corresponding Sesame data is created
     */
    for (j = 0; j < me->NT; j++) {
      enion[0 + j * me->NR] = enion[1 + j * me->NR];
      anion[0 + j * me->NR] = anion[1 + j * me->NR];
      s_ion[0 + j * me->NR] = s_ion[1 + j * me->NR];
    }
  }

  me->eosData.numSubtablesLoaded = (EOS_INTEGER)
    MIN(MAX_TABLES_RECORDTYPE1,
	((me->eosData.dataSize - (2 + me->NR + me->NT)) / (me->NR * me->NT)));


  if (eos_GetStandardErrorCodeFromCustomErrorCode(ierr2) != EOS_OK &&
      eos_GetStandardErrorCodeFromCustomErrorCode(ierr) == EOS_OK)
    ierr = ierr2; /* use alternate stored error code */

  return (ierr);
}

/***********************************************************************/
/*! 
 * \brief adds points in between grid points of class eos_RecordType1 if the option is set
 * 
 * \param[out]   *err - EOS_INTEGER : error code
 * \param[in]    *ptr - void : data object pointer;
 *                             internally recast to eos_RecordType1*
 * 
 * \return none
 *
 ***********************************************************************/
void eos_ExpandGridRecordType1 (void *ptr, EOS_INTEGER *err)
{
  EOS_INTEGER nAdd, oldNT, oldNR, i, j, k, *xyBounds = NULL;
  EOS_REAL *oldR, *oldT, **oldF[MAX_TABLES_RECORDTYPE1], *oldColdCurve[MAX_TABLES_RECORDTYPE1];
  EOS_REAL *R, *T, **F, *coldCurve;
  EOS_REAL *ptr1;
  eos_OptionValue *optionVal;
  eos_RecordType1 *me;
  me = (eos_RecordType1 *) ptr;
  *err = EOS_OK;

  optionVal = _eos_getOptionEosData (&(me->eosData), EOS_INSERT_DATA);
  nAdd = optionVal->ival;
  if (nAdd == 0)
    return;                     /* nothing to do */

  oldNT = me->NT;
  oldNR = me->NR;
  oldR = (EOS_REAL *) malloc (oldNR * sizeof (EOS_REAL));
  for (k = 0; k < oldNR; k++)
    oldR[k] = me->R[k];
  oldT = (EOS_REAL *) malloc (oldNT * sizeof (EOS_REAL));
  for (k = 0; k < oldNT; k++)
    oldT[k] = me->T[k];

  /* first copy current data into temp array for all tables */
  for (i = 0; i < MAX_TABLES_RECORDTYPE1; i++) {
    if (me->eosData.numSubtablesLoaded < i + 1)
      break;

    _eos_GetDataRecordType1 (me, &R, &T, &F, &coldCurve, i + 1);

    oldF[i] = (EOS_REAL **) malloc (oldNT * sizeof (EOS_REAL *));

    /* allocate memory continuously */
    ptr1 = (EOS_REAL *) malloc (sizeof (EOS_REAL) * oldNT * oldNR);

    /* get data of i-th subtable */

    for (j = 0; j < oldNT; j++) {
      oldF[i][j] = ptr1 + j * oldNR;
      for (k = 0; k < oldNR; k++)
        oldF[i][j][k] = F[j][k];
    }

    if ((me->eosData.tableNum == 301 || me->eosData.tableNum == 303)
        && coldCurve) {
      oldColdCurve[i] = (EOS_REAL *) malloc (oldNR * sizeof (EOS_REAL));
      for (k = 0; k < oldNR; k++)
        oldColdCurve[i][k] = coldCurve[k];
    }
    else {
      oldColdCurve[i] = NULL;
    }
  }

  /* now reallocate our memory */
  eos_SetSizeRecordType1 (me, (oldNR - 1) * nAdd + oldNR,
                          (oldNT - 1) * nAdd + oldNT, me->eosData.tableNum);

  /* now fill in new values into the memory for all tables */
  for (i = 0; i < MAX_TABLES_RECORDTYPE1; i++) {
    if (me->eosData.numSubtablesLoaded < i + 1)
      break;

    /* get pointers to new data */
    _eos_GetDataRecordType1 (me, &R, &T, &F, &coldCurve, i + 1);
    eos_ExpandGridInterpolate (nAdd, oldNR, oldNT, oldR, oldT, oldF[i], R, T,
                               F, err);

    /* also interpolate new coldCurve */
    if ((me->eosData.tableNum == 301 || me->eosData.tableNum == 303)
        && coldCurve) {
      xyBounds =
        (EOS_INTEGER *) malloc (sizeof (EOS_INTEGER) *
                                ((oldNR - 1) * nAdd + oldNR));
      eos_RationalInterpolate ((oldNR - 1) * nAdd + oldNR, oldNR, 1, 0, oldR,
                               oldColdCurve[i], R, coldCurve, NULL, 'y',
                               xyBounds, err);
      EOS_FREE (xyBounds);
    }
  }
}

/***********************************************************************/
/*!
 * \brief returns information items for the table.
 * 
 * \param[out]   *err         - EOS_INTEGER : error code
 * \param[in]    *ptr         - void : data object pointer;
 *                                     internally recast to eos_RecordType1*
 * \param[in]    th           - EOS_INTEGER : table handle
 * \param[in]    numInfoItems - EOS_INTEGER : # of requested items
 * \param[in]    *infoItems   - EOS_INTEGER : array of requested items
 * \param[in]    *infoVals    - EOS_REAL : return item values
 * 
 * \return none
 *
 ***********************************************************************/
void eos_GetTableInfoRecordType1 (void *ptr, EOS_INTEGER th,
                                  EOS_INTEGER numInfoItems,
                                  EOS_INTEGER *infoItems, EOS_REAL *infoVals,
                                  EOS_INTEGER *err)
{
  EOS_INTEGER i, j, k, subTable, ind;
  EOS_REAL **F, *CC, CCval, xconv = ONE, yconv = ONE, fconv = ONE;
  eos_Data *tbl401 = NULL;
  eos_RecordType1 *me;
  EOS_CHAR s[50];
  EOS_BOOLEAN allow_all_info_items = EOS_FALSE;

  me = (eos_RecordType1 *) ptr;
  *err = EOS_OK;
  
  /* if either EOS_ALLOW_ALL_INFO_ITEMS option or me->isInvertedAtSetup is set, then lift data type restrictions */
  allow_all_info_items = me->isInvertedAtSetup ||
    gEosDataMap.generalOptions[EOS_GENERAL_OPTION_FLAG_TO_INDEX (EOS_ALLOW_ALL_INFO_ITEMS)][th].bval;

  if (EOS_ALLOW_CAT0_ONLY_INFO_ITEM (infoItems[0]) || allow_all_info_items) {
    /* determine if infoItems[0] is restricted to category 0 datatypes */
    if (EOS_CATEGORY (eos_GetDataTypeFromTableHandle (th, err)) != 0
	&& ! allow_all_info_items) {
      /* return error */
      *err = EOS_INVALID_INFO_FLAG;
      *err = eos_SetCustomErrorMsg(th, *err,
				   "Invalid info flag. This info flag is only valid for simple, non-inverted data types.");
      return;
    }
    else {
      /* process x-conversion option */
      xconv = eos_getRealOptionFromTableHandle (th, EOS_X_CONVERT, err);
      if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) != EOS_OK)
        return;

      /* process y-conversion option */
      yconv = eos_getRealOptionFromTableHandle (th, EOS_Y_CONVERT, err);
      if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) != EOS_OK)
        return;

      /* process f-conversion option */
      fconv = eos_getRealOptionFromTableHandle (th, EOS_F_CONVERT, err);
      if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) != EOS_OK)
        return;

      if (me->rt2_handle >= 0) {
        /* process x-conversion factor for rt2_handle */
        ind = EOS_GENERAL_OPTION_FLAG_TO_INDEX (EOS_X_CONVERT);
        gEosDataMap.generalOptions[ind][me->rt2_handle].rval = xconv;

        /* process f-conversion factor for rt2_handle */
        ind = EOS_GENERAL_OPTION_FLAG_TO_INDEX (EOS_F_CONVERT);
        gEosDataMap.generalOptions[ind][me->rt2_handle].rval = fconv;
      }
    }
  }

  switch (infoItems[0]) {
  case EOS_NT401:
  case EOS_P401:
  case EOS_T401:
  case EOS_RG401:
  case EOS_RL401:
  case EOS_EG401:
  case EOS_EL401:
    if (me->rt2_handle >= 0) {
      tbl401 =
        gEosDataMap.dataObjects[gEosDataMap.tableHandlesMap[me->rt2_handle]];
      tbl401->GetTableInfo (tbl401, me->rt2_handle, numInfoItems, infoItems,
                            infoVals, err);
      return;
    }
    else {
      *err = EOS_INVALID_TABLE_HANDLE;
      *err = eos_SetCustomErrorMsg(th, *err,
				   "Invalid table handle. 401 table data not available to supplied table handle.");
      return;
    }
    break;
  default:
    break;
  }

  if (infoItems[0] == EOS_R_Array) {
    if (numInfoItems < me->NR) {
      *err = EOS_FAILED;
      *err = eos_SetCustomErrorMsg(th, *err,
				   "Operation failed. Insufficient memory allocation assumed because numInfoItems<NR.");
      return;
    }
    for (i = 0; i < MIN (numInfoItems, me->NR); i++)
      infoVals[i] = me->R[i] * xconv;
    return;
  }

  if (infoItems[0] == EOS_T_Array) {
    if (numInfoItems < me->NT) {
      *err = EOS_FAILED;
      *err = eos_SetCustomErrorMsg(th, *err,
				   "Operation failed. Insufficient memory allocation assumed because numInfoItems<NT.");
      return;
    }
    for (i = 0; i < MIN (numInfoItems, me->NT); i++)
      infoVals[i] = me->T[i] * yconv;
    return;
  }

  if (infoItems[0] == EOS_X_Species_Data ||
      infoItems[0] == EOS_Y_Species_Data ||
      infoItems[0] == EOS_F_Species_Data ||
      infoItems[0] == EOS_dFx_Species_Data ||
      infoItems[0] == EOS_dFy_Species_Data
      ) {
    EOS_REAL *p = NULL;
    EOS_INTEGER nXYPairs = gEosInterpolation.interpolationDataList[th]->nXYPairs;
    if (infoItems[0] == EOS_X_Species_Data) {
      strcpy(s, "EOS_X_Species_Data");
      p = gEosInterpolation.interpolationDataList[th]->xSpecies;
    }
    if (infoItems[0] == EOS_Y_Species_Data) {
      strcpy(s, "EOS_Y_Species_Data");
      p = gEosInterpolation.interpolationDataList[th]->ySpecies;
    }
    if (infoItems[0] == EOS_F_Species_Data) {
      strcpy(s, "EOS_F_Species_Data");
      p = gEosInterpolation.interpolationDataList[th]->FSpecies;
    }
    if (infoItems[0] == EOS_dFx_Species_Data) {
      strcpy(s, "EOS_dFx_Species_Data");
      p = gEosInterpolation.interpolationDataList[th]->dFxSpecies;
    }
    if (infoItems[0] == EOS_dFy_Species_Data) {
      strcpy(s, "EOS_dFy_Species_Data");
      p = gEosInterpolation.interpolationDataList[th]->dFySpecies;
    }
    if (! p) {
      *err = EOS_WARNING;
      *err = eos_SetCustomErrorMsg(th, *err,
				   "Operation failed. The requested data (%s) is unavailable for table handle %d.",
				   s, th);
      return;
    }
    if (numInfoItems < nXYPairs) {
      *err = EOS_FAILED;
      *err = eos_SetCustomErrorMsg(th, *err,
				   "Operation failed. Insufficient memory allocation assumed because numInfoItems<nTables.");
      return;
    }
    for (i = 0; i < MIN (numInfoItems, nXYPairs); i++)
      infoVals[i] = p[i];
    return;
  }

  if (infoItems[0] == EOS_F_Array) {
    if (numInfoItems < me->NR * me->NT) {
      *err = EOS_FAILED;
      *err = eos_SetCustomErrorMsg(th, *err,
				   "Operation failed. Insufficient memory allocation assumed because numInfoItems<NR*NT.");
      return;
    }
    subTable =
      EOS_TYPE_TO_SUB_TAB_NUM (eos_GetDataTypeFromTableHandle (th, err));
    if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) != EOS_OK)
      return;
    switch (subTable) {
    case 1:
      F = me->table1;
      CC = me->coldCurve1;
      break;
    case 2:
      F = me->table2;
      CC = me->coldCurve2;
      break;
    case 3:
      F = me->table3;
      CC = me->coldCurve3;
      break;
    case 4:
      F = me->table4;
      CC = me->coldCurve4;
      break;
    }
    i = 0;
    for (k = 0; k < me->NT; k++)
      for (j = 0; j < me->NR; j++) {
        CCval = (CC) ? CC[j] : ZERO;
        infoVals[i++] = (F[k][j] + CCval) * fconv;
      }
    return;
  }

  for (i = 0; i < numInfoItems; i++) {
    if (EOS_ALLOW_CAT0_ONLY_INFO_ITEM (infoItems[0]) &&
        EOS_CATEGORY (eos_GetDataTypeFromTableHandle (th, err)) != 0 &&
	! allow_all_info_items) {
      /* return error */
      *err = EOS_INVALID_INFO_FLAG;
      *err = eos_SetCustomErrorMsg(th, *err,
				   "Invalid info flag. This info flag is only valid for simple, non-inverted data types.");
      return;
    }

    switch (infoItems[i]) {
    case EOS_X_Convert_Factor:
      /* process x-conversion option */
      infoVals[i] = eos_getRealOptionFromTableHandle (th, EOS_X_CONVERT, err);
      break;

    case EOS_Y_Convert_Factor:
      /* process y-conversion option */
      infoVals[i] = eos_getRealOptionFromTableHandle (th, EOS_Y_CONVERT, err);
      break;

    case EOS_F_Convert_Factor:
      /* process f-conversion option */
      infoVals[i] = eos_getRealOptionFromTableHandle (th, EOS_F_CONVERT, err);
      break;

    case EOS_Log_Val:
      infoVals[i] = (EOS_REAL)
        EOS_TYPE_TO_LOG_AXES (eos_GetDataTypeFromTableHandle (th, err));
      break;

    case EOS_Material_ID:
      infoVals[i] = (EOS_REAL) me->eosData.materialID;
      break;

    case EOS_Table_Type:
      infoVals[i] = (EOS_REAL) eos_GetDataTypeFromTableHandle (th, err);
      break;

    case EOS_Fmin:
      subTable =
        EOS_TYPE_TO_SUB_TAB_NUM (eos_GetDataTypeFromTableHandle (th, err));
      if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) != EOS_OK)
        return;
      switch (subTable) {
      case 1:
        F = me->table1;
        break;
      case 2:
        F = me->table2;
        break;
      case 3:
        F = me->table3;
        break;
      case 4:
        F = me->table4;
        break;
      }
      infoVals[i] = F[0][0];
      for (j = 0; j < me->NR; j++) {
        for (k = 0; k < me->NT; k++)
          if (F[k][j] < infoVals[i])
            infoVals[i] = F[k][j];
      }
      break;

    case EOS_Fmax:
      subTable =
        EOS_TYPE_TO_SUB_TAB_NUM (eos_GetDataTypeFromTableHandle (th, err));
      if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) != EOS_OK)
        return;
      switch (subTable) {
      case 1:
        F = me->table1;
        break;
      case 2:
        F = me->table2;
        break;
      case 3:
        F = me->table3;
        break;
      case 4:
        F = me->table4;
        break;
      }
      infoVals[i] = F[0][0];
      for (j = 0; j < me->NR; j++) {
        for (k = 0; k < me->NT; k++)
          if (F[k][j] > infoVals[i])
            infoVals[i] = F[k][j];
      }
      break;

    case EOS_Rmin:
      infoVals[i] = me->R[0];
      break;

    case EOS_Rmax:
      infoVals[i] = me->R[me->NR - 1];
      break;

    case EOS_Tmin:
      infoVals[i] = me->T[0];
      break;

    case EOS_Tmax:
      infoVals[i] = me->T[me->NT - 1];
      break;

    case EOS_NR:
      infoVals[i] = (EOS_REAL) me->NR;
      break;

    case EOS_NT:
      infoVals[i] = (EOS_REAL) me->NT;
      break;

    case EOS_Mean_Atomic_Num:
      infoVals[i] = (EOS_REAL) me->avgAtomicNumber;
      break;

    case EOS_Mean_Atomic_Mass:
      infoVals[i] = (EOS_REAL) me->avgAtomicWgt;
      break;

    case EOS_Normal_Density:
      infoVals[i] = (EOS_REAL) me->refDensity;
      break;

    case EOS_Modulus:
      infoVals[i] = (EOS_REAL) me->solidBulkModulus;
      break;

    case EOS_Exchange_Coeff:
      infoVals[i] = (EOS_REAL) me->exchangeCoefficient;
      break;

    case EOS_nXYPairs:
      infoVals[i] = (EOS_REAL) gEosInterpolation.interpolationDataList[th]->nXYPairs;
      break;

    case EOS_R_Array:
      strcpy(s, "EOS_R_Array");

    case EOS_T_Array:
      strcpy(s, "EOS_T_Array");

    case EOS_F_Array:
      strcpy(s, "EOS_F_Array");

    case EOS_X_Species_Data:
      strcpy(s, "EOS_X_Species_Data");

    case EOS_Y_Species_Data:
      strcpy(s, "EOS_Y_Species_Data");

    case EOS_F_Species_Data:
      strcpy(s, "EOS_F_Species_Data");

    case EOS_dFx_Species_Data:
      strcpy(s, "EOS_dFx_Species_Data");

    case EOS_dFy_Species_Data:
      strcpy(s, "EOS_dFy_Species_Data");

      *err = EOS_INVALID_INFO_FLAG;
      *err = eos_SetCustomErrorMsg(th, *err,
				   "Invalid info flag because %s may only be used without any other info flags.", s);
      break;

    default:
      *err = EOS_INVALID_INFO_FLAG;
    }
  }
}

/************************************************************************
 * 
 * returns meta data information items for the table.
 * 
 * Returned Values:
 * EOS_INTEGER *err    - output error code
 * EOS_CHAR *infoStr   - allocated string to contain all comments
 *
 * Input Value:
 * void *ptr           - this pointer (pointer to the instance of type eos_RecordType1
 * EOS_CHAR *infoItem  - flag specifying what meta data item to fetch
 * 
 ************************************************************************/
void eos_GetTableMetaDataRecordType1 (void *ptr, EOS_INTEGER infoItem,
				      EOS_CHAR *infoStr, EOS_INTEGER *err)
{
  //eos_RecordType1 *me;

  //me = (eos_RecordType1 *) ptr;
  *err = EOS_INVALID_DATA_TYPE; /* 101 table not found */
}

/***********************************************************************/
/*!
 * \section calculate_entropy Calculate Entropy
 * \brief Determines entropy using existing internal energy, temperature and, 
 * if available, free energy data.
 *
 * \param[out]   S[NR*NT]  - EOS_REAL : locally allocated array containing
 *                                      entropy/mass (Gj/Mg/K)
 * \param[in]    me        - eos_RecordType1* : optional pointer to object.
 *                           NOTE -- only used for debug output; otherwise NULL
 * \param[in]    NR        - EOS_INTEGER : number of density dependent values in arrays
 * \param[in]    NT        - EOS_INTEGER : number of temperature dependent values in arrays
 * \param[in]    U[NR*NT]  - EOS_REAL : array of internal energy data (Gj/Mg);
 *                                      it may include the cold curve component
 * \param[in]    F[NR*NT]  - EOS_REAL : array of free energy data (Gj/Mg)
 * \param[in]    T[NT]     - EOS_REAL : array of temperature data (K)
 *
 * \return eos_Entropy - EOS_INTEGER : error code
 *                            EOS_NOT_ALLOCATED indicates array(s) not allocated for data
 *                            -2                indicates T=zero isotherm not in data
 *                            -3                indicates EOS_PT_SMOOTHING option is set
 *
 ***********************************************************************/
EOS_INTEGER eos_Entropy (eos_RecordType1 *me,
                         EOS_INTEGER NR, EOS_INTEGER NT,
                         EOS_REAL *U, EOS_REAL *F, EOS_REAL *T, EOS_REAL *R, EOS_REAL *S)
{
  int i, j;
  EOS_INTEGER ierr = EOS_OK;
  EOS_REAL integralValue, *integralValues, *integrandValues;
  eos_OptionValue *optVal = NULL;

  /* if EOS_PT_SMOOTHING option is set, then do not calculate entropy */
  optVal = _eos_getOptionEosData (&(me->eosData), EOS_PT_SMOOTHING);
  if (optVal && optVal->bval)
    return -3;

  if (!U || !T || !S)
    return (EOS_NOT_ALLOCATED); // array(s) not allocated for data
  if (!F && T[0] > ZERO) {      // T=zero isotherm and F not in data
    /*! \note Need to figure out a reasonable method of calculating U(T=0)
     *        if free energy is unavailable; otherwise the current results of this
     *        function are incorrect when no cold curve data is tabulated. */
    return -2;
  }

  /*! The definition of free energy is \f$ A=U-TS \f$.
   */

  if (F) {
    /*! When free energy data is available, use \f$ S=\frac{(U-A)}{T} \f$ to calculate entropy.
     *  Calculate entropy using the definition of free energy, and force \f$ S=0 \f$ at \f$ T=0 \f$.
     */
    for (i = 0; i < NR; i++) {
      for (j = 0; j < NT; j++) {
        S[i + j * NR] = (T[j] <= ZERO) ? ZERO
          : (U[i + j * NR] - F[i + j * NR]) / T[j];
      }
    }
  }
  else {
    /*! When free energy data is not available, use \f$ S = \int_{0}^{T} \frac{1}{T} \frac{dU}{dT}dT \f$
     *  to calculate entropy, where U is the total internal energy. Calculate entropy using the
     *  definition of free energy, and force \f$ S=0 \f$ at \f$ T=0 \f$.
     *
     *  Integration by parts yields the equation,
     *  \f$ S = \frac{U}{T} + \int_{0}^{T} \frac{U}{{T}^{2}}dT \f$,
     *  which is calculated using a simple integration method using the
     *  \ref eos_TrapezoidIntegrate "trapezoid rule".
     */

    /* allocate temporary arrays */
    integrandValues = (EOS_REAL *) malloc (NT * sizeof (EOS_REAL));
    integralValues  = (EOS_REAL *) malloc (NT * sizeof (EOS_REAL));

    for (i = 0; i < NR; i++) {

      /*! Calculate values of integrand data, \f$ \frac{U}{{T}^{2}} \f$, and force to zero
       *  when \f$ T \leq 0 \f$.
       */
      for (j = 0; j < NT; j++) {
        integrandValues[j] =
          (T[j] <= ZERO) ? ZERO : (U[i + j * NR] - U[i]) / (T[j] * T[j]);
	integralValues[j] = ZERO;
      }

#if 0
      /* original, inefficient logic */
      for (j = 0; j < NT; j++) {
        ierr =
          eos_TrapezoidIntegrate (0, NT-1, T[j], NT, integrandValues, T, 99, &integralValue,
				  (!(i==(NR-1) && j==(NT-1)))?EOS_TRUE:EOS_FALSE);
        S[i + j * NR] =
          (T[j] <= ZERO) ? ZERO : (U[i + j * NR] - U[i]) / T[j] + integralValue;
      }
#else
      /* new, optimized logic */
      j = 0;
      if (T[j] <= ZERO) S[i + j * NR] = ZERO;
      for (j = 1; j < NT; j++) {
	if (T[j] <= ZERO) {
	  S[i + j * NR] = ZERO;
	}
	else {
	  ierr =
	    eos_TrapezoidIntegrate (j-1, j, T[j], NT, integrandValues, T, 99, &integralValue,
				    (!(i==(NR-1) && j==(NT-1)))?EOS_TRUE:EOS_FALSE);
	  integralValues[j] = integralValues[j-1] + integralValue;
	  S[i + j * NR] = (U[i + j * NR] - U[i]) / T[j] + integralValues[j];
	}
      }
#endif
    }

    EOS_FREE (integrandValues);
    EOS_FREE (integralValues);
  }

  return (ierr);
}

/***********************************************************************/
/*!
 * \brief This function gets data from 201 table. These values are needed
 *  for entropy and free energy data calculations.
 * 
 * \param[out]   errorCode - EOS_INTEGER : output error code
 * \param[out]   *zbar     - EOS_REAL : mean atomic number
 * \param[out]   *abar     - EOS_REAL : mean atomic mass
 * \param[out]   *dens0    - EOS_REAL : normal solid density
 * \param[in]    *ptr      - void : internally recast to eos_RecordType1*
 *
 * \return none
 *
 ***********************************************************************/
void eos_GetLoadedBulkDataRecordType1 (void *ptr, EOS_REAL *zbar,
                                       EOS_REAL *abar, EOS_REAL *dens0,
                                       EOS_INTEGER *errorCode)
{
  eos_RecordType1 *me;
  me = (eos_RecordType1 *) ptr;
  *errorCode = EOS_OK;

  *zbar = me->avgAtomicNumber;
  *abar = me->avgAtomicWgt;
  *dens0 = me->refDensity;
}

/***********************************************************************/
/*!
 * \brief This function assumes that tableNum 303 or 301 were loaded for a
 *  requested table Number 306 (cold Curve) We need to delete all data
 *  other than the cold Curve data and reset the table num to 306.
 * 
 * \param[out]   err        - EOS_INTEGER : error code
 * \param[out]   *zbar      - EOS_REAL : mean atomic number
 * \param[out]   *abar      - EOS_REAL : mean atomic mass
 * \param[out]   *dens0     - EOS_REAL : normal solid density
 * \param[in]    *ptr       - void : data object pointer;
 *                                   internally recast to eos_RecordType1*
 *
 * \return none
 *
 ***********************************************************************/
void eos_CleanUpColdCurveRecordType1 (void *ptr, EOS_INTEGER *err)
{
  eos_RecordType1 *me;
  EOS_INTEGER numSubTables, j, nr;
  EOS_REAL temp, *newR = NULL, *tableRow1 = NULL, *tableRow2 =
    NULL, *tableRow3 = NULL, *tableRow4 = NULL;
  me = (eos_RecordType1 *) ptr;
  *err = EOS_OK;

  /* first copy the available cold curve data into newly allocated arrays */
  tableRow1 = (EOS_REAL *) malloc (me->NR * sizeof (EOS_REAL));
  tableRow2 = (EOS_REAL *) malloc (me->NR * sizeof (EOS_REAL));
  tableRow3 = (EOS_REAL *) malloc (me->NR * sizeof (EOS_REAL));
  tableRow4 = (EOS_REAL *) malloc (me->NR * sizeof (EOS_REAL));
  newR = (EOS_REAL *) malloc (me->NR * sizeof (EOS_REAL));

  if (tableRow1) {
    if (me->table1 && me->coldCurve1) {
      /* cold curve was separated in eos_LoadRecordType1 */
      for (j = 0; j < me->NR; j++)
	tableRow1[j] = me->table1[0][j] + me->coldCurve1[j];
    }
    else if (me->table1 && ! me->coldCurve1) {
      /* cold curve was not separated in eos_LoadRecordType1 */
      for (j = 0; j < me->NR; j++)
	tableRow1[j] = me->table1[0][j];
    }
    else if (! me->table1 && me->coldCurve1) {
      /* cold curve was not separated in eos_LoadRecordType1 */
      for (j = 0; j < me->NR; j++)
	tableRow1[j] = me->coldCurve1[j];
    }
    else {
      /* something was messed up in eos_LoadRecordType1 */
      for (j = 0; j < me->NR; j++)
	tableRow1[j] = ZERO;
    }
  }
  if (tableRow2) {
    if (me->table2 && me->coldCurve2) {
      /* cold curve was separated in eos_LoadRecordType1 */
      for (j = 0; j < me->NR; j++)
	tableRow2[j] = me->table2[0][j] + me->coldCurve2[j];
    }
    else if (me->table2 && ! me->coldCurve2) {
      /* cold curve was not separated in eos_LoadRecordType1 */
      for (j = 0; j < me->NR; j++)
	tableRow2[j] = me->table2[0][j];
    }
    else if (! me->table2 && me->coldCurve2) {
      /* cold curve was not separated in eos_LoadRecordType1 */
      for (j = 0; j < me->NR; j++)
	tableRow2[j] = me->coldCurve2[j];
    }
    else {
      /* something was messed up in eos_LoadRecordType1 */
      for (j = 0; j < me->NR; j++)
	tableRow2[j] = ZERO;
    }
  }
  if (tableRow3) {
    if (me->table3 && me->coldCurve3) {
      /* cold curve was separated in eos_LoadRecordType1 */
      for (j = 0; j < me->NR; j++)
	tableRow3[j] = me->table3[0][j] + me->coldCurve3[j];
    }
    else if (me->table3 && ! me->coldCurve3) {
      /* cold curve was not separated in eos_LoadRecordType1 */
      for (j = 0; j < me->NR; j++)
	tableRow3[j] = me->table3[0][j];
    }
    else if (! me->table3 && me->coldCurve3) {
      /* cold curve was not separated in eos_LoadRecordType1 */
      for (j = 0; j < me->NR; j++)
	tableRow3[j] = me->coldCurve3[j];
    }
    else {
      /* something was messed up in eos_LoadRecordType1 */
      for (j = 0; j < me->NR; j++)
	tableRow3[j] = ZERO;
    }
  }
  if (tableRow4) {
    if (me->table4 && me->coldCurve4) {
      /* cold curve was separated in eos_LoadRecordType1 */
      for (j = 0; j < me->NR; j++)
	tableRow4[j] = me->table4[0][j] + me->coldCurve4[j];
    }
    else if (me->table4 && ! me->coldCurve4) {
      /* cold curve was not separated in eos_LoadRecordType1 */
      for (j = 0; j < me->NR; j++)
	tableRow4[j] = me->table4[0][j];
    }
    else if (! me->table4 && me->coldCurve4) {
      /* cold curve was not separated in eos_LoadRecordType1 */
      for (j = 0; j < me->NR; j++)
	tableRow4[j] = me->coldCurve4[j];
    }
    else {
      /* something was messed up in eos_LoadRecordType1 */
      for (j = 0; j < me->NR; j++)
	tableRow4[j] = ZERO;
    }
  }
  if (newR) {
    if (me->R) {
      for (j = 0; j < me->NR; j++)
	newR[j] = me->R[j];
    }
    else {
      /* something was messed up in eos_LoadRecordType1 */
      for (j = 0; j < me->NR; j++)
	newR[j] = ZERO;
    }
  }

  /* save nr */
  nr = me->NR;
  temp = me->T[0];
  numSubTables = me->eosData.numSubtablesLoaded;
  me->eosData.tableNum = 306;

  eos_SetSizeRecordType1 (me, 0, 0, 0);

  /* reset NT and dataSize */
  me->NT = 1;                   // reset NT
  me->NR = nr;

  eos_SetSizeRecordType1 (me, me->NR, me->NT, me->eosData.tableNum);

  me->eosData.numSubtablesLoaded = numSubTables;
  me->T[0] = temp;

  for (j = 0; j < me->NR; j++) {
    if (numSubTables >= 1)
      me->table1[0][j] = tableRow1[j];
    if (numSubTables >= 2)
      me->table2[0][j] = tableRow2[j];
    if (numSubTables >= 3)
      me->table3[0][j] = tableRow3[j];
    if (numSubTables >= 4)
      me->table4[0][j] = tableRow4[j];
    me->R[j] = newR[j];
  }

  EOS_FREE (newR);
  if (tableRow1) EOS_FREE (tableRow1);
  if (tableRow2) EOS_FREE (tableRow2);
  if (tableRow3) EOS_FREE (tableRow3);
  if (tableRow4) EOS_FREE (tableRow4);
  me->eosData.coldCurveIsLoaded = EOS_FALSE;    /* indicates that cold curves arrays are null! */
  me->eosData.isLoaded = 1;     /* indicate that data is loaded into object */
}


/***********************************************************************/
/*!
 * \brief Get the smoothing option flags.
 * 
 * \param[in]    dataType     - EOS_INTEGER : data type of *ptr eos_RecordType1 object
 * \param[out]   *isSmooth    - EOS_INTEGER : is data  of *ptr eos_RecordType1 object smooth
 *                                            according to EOS_SMOOTH option?
 * \param[out]   *isPtSmooth  - EOS_INTEGER : is data  of *ptr eos_RecordType1 object smooth
 *                                            according to EOS_PT_SMOOTHING option?
 * \param[in]    *ptr         - void : data object pointer;
 *                                     internally recast to eos_RecordType1*
 *
 * \return none
 *
 ***********************************************************************/
void eos_GetSmoothingRecordType1 (void *ptr, EOS_INTEGER dataType,
                                  EOS_INTEGER *isSmooth,
                                  EOS_INTEGER *isPtSmooth)
{
  eos_RecordType1 *me;
  EOS_INTEGER subTableNum;

  /* initialize values */
  *isSmooth = *isPtSmooth = 0;

  me = (eos_RecordType1 *) ptr;
  subTableNum = EOS_TYPE_TO_SUB_TAB_NUM (dataType);
  switch (subTableNum) {
  case 1:
    *isSmooth = me->shouldBeSmooth1;
    *isPtSmooth = me->shouldBePtSmooth1;
    break;
  case 2:
    *isSmooth = me->shouldBeSmooth2;
    *isPtSmooth = me->shouldBePtSmooth2;
    break;
  case 3:
    *isSmooth = me->shouldBeSmooth3;
    *isPtSmooth = me->shouldBePtSmooth4;
    break;
  case 4:
    *isSmooth = me->shouldBeSmooth4;
    *isPtSmooth = me->shouldBePtSmooth4;
    break;
  }
}

/***********************************************************************/
/*!
 * \brief Get the monotonicity flags
 *
 * \param[in]    dataType     - EOS_INTEGER : data type of *ptr eos_RecordType1 object
 * \param[out]   *inX         - EOS_INTEGER : is data  of *ptr eos_RecordType1 object monotonic
 *                                            with respect to first independent variable?
 * \param[out]   *inY         - EOS_INTEGER : is data  of *ptr eos_RecordType1 object monotonic
 *                                            with respect to second independent variable?
 * \param[in]     *ptr   - void : data object pointer;
 *                         internally recast to eos_RecordType1*
 *
 * \return none
 *
 ***********************************************************************/
void eos_GetMonotonicityRecordType1 (void *ptr, EOS_INTEGER dataType,
                                     EOS_INTEGER *inX, EOS_INTEGER *inY)
{
  EOS_INTEGER subTableNum = 1;
  eos_RecordType1 *me;


  me = (eos_RecordType1 *) ptr;
  subTableNum = EOS_TYPE_TO_SUB_TAB_NUM (dataType);

  switch (subTableNum) {
  case 1:
    *inX = me->shouldBeMonotonicX1;
    *inY = me->shouldBeMonotonicY1;
    break;
  case 2:
    *inX = me->shouldBeMonotonicX2;
    *inY = me->shouldBeMonotonicY2;
    break;
  case 3:
    *inX = me->shouldBeMonotonicX3;
    *inY = me->shouldBeMonotonicY3;
    break;
  case 4:
    *inX = me->shouldBeMonotonicX4;
    *inY = me->shouldBeMonotonicY4;
    break;
  }
}

/***********************************************************************/
/*!
 * \brief Set the flags so that appropriate data will be made monotonic upon loading
 *
 * \param[in]     *ptr    - void : data object pointer;
 *                                 internally recast to eos_RecordType1*
 * \param[in]    dataType - EOS_INTEGER : data type of *ptr eos_RecordType1 object
 * \param[in]    inX      - EOS_INTEGER : is data  of *ptr eos_RecordType1 object monotonic
 *                                        with respect to first independent variable?
 * \param[in]    inY      - EOS_INTEGER : is data  of *ptr eos_RecordType1 object monotonic
 *                                        with respect to second independent variable?
 *
 * \return none
 *
 ***********************************************************************/
void eos_SetMonotonicityRecordType1 (void *ptr, EOS_INTEGER dataType,
                                     EOS_INTEGER inX, EOS_INTEGER inY)
{
  EOS_INTEGER subTableNum = 1;
  eos_RecordType1 *me;


  me = (eos_RecordType1 *) ptr;
  subTableNum = EOS_TYPE_TO_SUB_TAB_NUM (dataType);

  switch (subTableNum) {
  case 1:
    if (me->shouldBePtSmooth1)
      return;
    if (inX >= 0 && !me->shouldBeMonotonicX1)
      me->shouldBeMonotonicX1 = inX;
    if (inY >= 0 && !me->shouldBeMonotonicY1)
      me->shouldBeMonotonicY1 = inY;
    break;
  case 2:
    if (me->shouldBePtSmooth2)
      return;
    if (inX >= 0 && !me->shouldBeMonotonicX2)
      me->shouldBeMonotonicX2 = inX;
    if (inY >= 0 && !me->shouldBeMonotonicY2)
      me->shouldBeMonotonicY2 = inY;
    break;
  case 3:
    if (me->shouldBePtSmooth3)
      return;
    if (inX >= 0 && !me->shouldBeMonotonicX3)
      me->shouldBeMonotonicX3 = inX;
    if (inY >= 0 && !me->shouldBeMonotonicY3)
      me->shouldBeMonotonicY3 = inY;
    break;
  case 4:
    if (me->shouldBePtSmooth4)
      return;
    if (inX >= 0 && !me->shouldBeMonotonicX4)
      me->shouldBeMonotonicX4 = inX;
    if (inY >= 0 && !me->shouldBeMonotonicY4)
      me->shouldBeMonotonicY4 = inY;
    break;
  }
}

/***********************************************************************/
/*!
 * \brief Find out if the requested type, monotonicity combo can share our table
 *  object
 * 
 * \param[out]   *compatible  - EOS_BOOLEAN : are the monotonic options compatible?
 * \param[in]    *ptr         - void : data object pointer;
 *                                     internally recast to eos_RecordType1*
 * \param[in]    dataType     - EOS_INTEGER : data type of *ptr eos_RecordType1 object
 * \param[in]    inX          - EOS_INTEGER : is data  of *ptr eos_RecordType1 object monotonic
 *                                            with respect to first independent variable?
 *                                            (ignore if negative)
 * \param[in]    inY          - EOS_INTEGER : is data  of *ptr eos_RecordType1 object monotonic
 *                                            with respect to second independent variable?
 *                                            (ignore if negative)
 *
 * \return none
 *
 ***********************************************************************/
void eos_AreMonotonicRequirementsCompatibleRecordType1 (void *ptr,
                                                        EOS_INTEGER dataType,
                                                        EOS_INTEGER inX,
                                                        EOS_INTEGER inY,
                                                        EOS_BOOLEAN
                                                        *compatible)
{
  EOS_INTEGER subTableNum = 1, subTableNumStart, subTableNumEnd;
  eos_RecordType1 *me;
  EOS_INTEGER *shouldBeMonotonicX=0, *shouldBeMonotonicY=0, *shouldBePtSmooth=0;

  me = (eos_RecordType1 *) ptr;
  if (dataType > 0)
    subTableNumStart = subTableNumEnd = EOS_TYPE_TO_SUB_TAB_NUM (dataType);
  else {                        /* do all 4 substables */

    subTableNumStart = 1;
    subTableNumEnd = 4;
  }
  *compatible = EOS_TRUE;

  for (subTableNum = subTableNumStart; subTableNum <= subTableNumEnd;
       subTableNum++) {
    switch (subTableNum) {
    case 1:
      shouldBeMonotonicX = &(me->shouldBeMonotonicX1);
      shouldBeMonotonicY = &(me->shouldBeMonotonicY1);
      shouldBePtSmooth = &(me->shouldBePtSmooth1);
      break;
    case 2:
      shouldBeMonotonicX = &(me->shouldBeMonotonicX2);
      shouldBeMonotonicY = &(me->shouldBeMonotonicY2);
      shouldBePtSmooth = &(me->shouldBePtSmooth2);
      break;
    case 3:
      shouldBeMonotonicX = &(me->shouldBeMonotonicX3);
      shouldBeMonotonicY = &(me->shouldBeMonotonicY3);
      shouldBePtSmooth = &(me->shouldBePtSmooth3);
      break;
    case 4:
      shouldBeMonotonicX = &(me->shouldBeMonotonicX4);
      shouldBeMonotonicY = &(me->shouldBeMonotonicY4);
      shouldBePtSmooth = &(me->shouldBePtSmooth4);
      break;
    }

    if (inX >= 0 && *shouldBeMonotonicX != inX) *compatible = EOS_FALSE;
    if (inY >= 0 && *shouldBeMonotonicY != inY) *compatible = EOS_FALSE;
    if (inX != 0 || (inY != 0 && *shouldBePtSmooth > 0)) *compatible = EOS_FALSE;

    if (*compatible == EOS_FALSE)
      break;
  }
}

/***********************************************************************/
/*!
 * \brief Set the flags so that appropriate data will be made smooth upon loading
 * 
 * \param[in]    *ptr      - void : data object pointer;
 *                                  internally recast to eos_RecordType1*
 * \param[in]    dataType  - EOS_INTEGER : data type of *ptr eos_RecordType1 object
 * \param[in]    pt_smooth - EOS_INTEGER : is data  of *ptr eos_RecordType1 object smooth
 *                                         according to EOS_PT_SMOOTHING option?
 * \param[in]    smooth    - EOS_INTEGER : is data  of *ptr eos_RecordType1 object smooth
 *                                         according to EOS_SMOOTH option?
 *
 * \return none
 *
 ***********************************************************************/
void eos_SetSmoothingRecordType1 (void *ptr, EOS_INTEGER dataType,
                                  EOS_INTEGER smooth, EOS_INTEGER pt_smooth)
{
  eos_RecordType1 *me;
  EOS_INTEGER subTableNum;
  me = (eos_RecordType1 *) ptr;
  subTableNum = EOS_TYPE_TO_SUB_TAB_NUM (dataType);

  switch (subTableNum) {
  case 1:
    if (!me->shouldBePtSmooth1)
      me->shouldBeSmooth1 = (smooth != 0);
    break;
  case 2:
    if (!me->shouldBePtSmooth2)
      me->shouldBeSmooth2 = (smooth != 0);
    break;
  case 3:
    if (!me->shouldBePtSmooth3)
      me->shouldBeSmooth3 = (smooth != 0);
    break;
  case 4:
    if (!me->shouldBePtSmooth4)
      me->shouldBeSmooth4 = (smooth != 0);
    break;
  }

  /* set all shouldBePtSmooth flags */
  me->shouldBePtSmooth1 =
    me->shouldBePtSmooth2 =
    me->shouldBePtSmooth3 = me->shouldBePtSmooth4 = (pt_smooth != 0);

  if (pt_smooth != 0) {
    /* override ALL shouldBeMonotonic flags */
    me->shouldBeMonotonicX1 = me->shouldBeMonotonicX2 =
      me->shouldBeMonotonicX3 = me->shouldBeMonotonicX4 =
      me->shouldBeMonotonicY1 = me->shouldBeMonotonicY2 =
      me->shouldBeMonotonicY3 = me->shouldBeMonotonicY4 = EOS_FALSE;
  }
}

/***********************************************************************/
/*!
 * \brief Find out if the requested type, smoothing combo can share our table object
 *
 * \param[out]   *compatible  - EOS_BOOLEAN : are the monotonic options compatible?
 * \param[in]    *ptr         - void : data object pointer;
 *                                     internally recast to eos_RecordType1*
 * \param[in]    dataType     - EOS_INTEGER : data type of *ptr eos_RecordType1 object
 * \param[in]    makeSmooth   - EOS_INTEGER : is data  of *ptr eos_RecordType1 object smooth
 *                                            with respect to EOS_SMOOTH option?
 * \param[in]    makePtSmooth - EOS_INTEGER : is data  of *ptr eos_RecordType1 object smooth
 *                                            with respect to EOS_PT_SMOOTHING option?
 *
 * \return none
 *
 ***********************************************************************/
void eos_AreSmoothingRequirementsCompatibleRecordType1 (void *ptr,
                                                        EOS_INTEGER dataType,
                                                        EOS_INTEGER
                                                        makeSmooth,
                                                        EOS_INTEGER
                                                        makePtSmooth,
                                                        EOS_BOOLEAN
                                                        *compatible)
{
  eos_RecordType1 *me;
  EOS_INTEGER subTableNum;
  me = (eos_RecordType1 *) ptr;
  subTableNum = EOS_TYPE_TO_SUB_TAB_NUM (dataType);
  *compatible = EOS_TRUE;

  switch (subTableNum) {
  case 1:
    if (me->shouldBeSmooth1 != makeSmooth)
      *compatible = EOS_FALSE;
    if (me->shouldBePtSmooth1 != makePtSmooth)
      *compatible = EOS_FALSE;
    if (makeSmooth != 0 && me->shouldBePtSmooth1 != 0)
      *compatible = EOS_FALSE;
    break;
  case 2:
    if (me->shouldBeSmooth2 != makeSmooth)
      *compatible = EOS_FALSE;
    if (me->shouldBePtSmooth2 != makePtSmooth)
      *compatible = EOS_FALSE;
    if (makeSmooth != 0 && me->shouldBePtSmooth2 != 0)
      *compatible = EOS_FALSE;
    break;
  case 3:
    if (me->shouldBeSmooth3 != makeSmooth)
      *compatible = EOS_FALSE;
    if (me->shouldBePtSmooth3 != makePtSmooth)
      *compatible = EOS_FALSE;
    if (makeSmooth != 0 && me->shouldBePtSmooth3 != 0)
      *compatible = EOS_FALSE;
    break;
  case 4:
    if (me->shouldBeSmooth4 != makeSmooth)
      *compatible = EOS_FALSE;
    if (me->shouldBePtSmooth4 != makePtSmooth)
      *compatible = EOS_FALSE;
    if (makeSmooth != 0 && me->shouldBePtSmooth4 != 0)
      *compatible = EOS_FALSE;
    break;
  }
}

/***********************************************************************/
/*!
 * \brief This is used to perform inverse interpolation at a given T when
 *  EOS_PT_SMOOTHING is set for the EOS_Ut_PtT or EOS_V_PtT data type.
 * 
 * \param[out]   *fVals     - EOS_REAL : nXYPairs of specific volume or specific internal energy
 * \param[out]   *dFx       - EOS_REAL : nXYPairs of partial derivatives w.r.t. pressure
 * \param[out]   **dFy      - EOS_REAL : nXYPairs of partial derivatives w.r.t. temperature
 * \param[out]   *errorCode - EOS_INTEGER : error code
 * \param[out]   *xyBounds  - EOS_INTEGER : interpolation errors per xy-pair
 * \param[in]    *me        - eos_RecordType1 : data object pointer
 * \param[in]    dataType   - EOS_INTEGER : data type
 * \param[in]    nXYPairs   - EOS_INTEGER : number of temperature-pressure datum pairs
 * \param[in]    *pres      - EOS_REAL : nXYPairs of pressure values
 * \param[in]    *temp      - EOS_REAL : nXYPairs of temperature values
 *
 * \return none
 *
 ***********************************************************************/
void _eos_SesameInvTRecordType1 (eos_RecordType1 *me, EOS_INTEGER dataType,
                                 EOS_INTEGER nXYPairs,
                                 EOS_REAL *pres, EOS_REAL *temp,
                                 EOS_REAL *fVals, EOS_REAL *dFx,
                                 EOS_REAL *dFy, EOS_INTEGER *xyBounds,
				 EOS_INTEGER *errorCode, EOS_CHAR **errMsg)
{
  EOS_INTEGER irlo, irhi, ir, j;
  EOS_REAL sr, sv, vol;
  EOS_INTEGER isotherm_nr, err;
  EOS_REAL *isotherm_p = NULL, *isotherm_e = NULL, *isotherm_r =
    NULL, *isotherm_v = NULL;

  _eos_DEBUG_PRINT ("\n*** ENTERING SESAME_INV_T\n");

  *errorCode = err = EOS_OK;

  if (dataType != EOS_V_PtT && dataType != EOS_Ut_PtT) {
    *errorCode = EOS_INVALID_DATA_TYPE;
    return;
  }

  for (j = 0; j < nXYPairs; j++) {

    if (j == 0 || temp[j] != temp[j - 1]) {
      _eos_sesame_isotherm_buildRecordType1
        (me, temp[j], &isotherm_nr, &isotherm_p, &isotherm_e, &isotherm_r,
         &isotherm_v, errorCode, errMsg);
      if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) != EOS_OK)
        return;
    }

    if (dataType == EOS_V_PtT) {

      /*
	Calculate the specific volume values for all pres[j] and temp[j] values
      */
      if (pres[j] < isotherm_p[0]) {

	/* _eos_sesame_isotherm_buildRecordType1 can return ZERO in isotherm_p and isotherm_r arrays,
	   and pres array can contain ZERO -- appropriate checks must be made */

	/* isotherm_p may contain zero which is minimum possible -- see _eos_sesame_isotherm_buildRecordType1 */
	/* isotherm_r may contain zero which is minimum possible -- see SESAME data */
	/* pres may contain negative, zero and/or positive values -- input */
	/* ORIGINAL CODE:
	   fVals[j] = ONE / (isotherm_r[0] * (pres[j] / isotherm_p[0]));
	*/
	fVals[j] = isotherm_r[0] * (pres[j] / FLOOR(isotherm_p[0]));
	if (fabs(fVals[j]) <= TINY_D || fabs(isotherm_p[0]) <= TINY_D) {
	  xyBounds[j] = EOS_CANT_INVERT_DATA;
	  if (fabs(isotherm_p[0]) <= TINY_D)
	    xyBounds[j] = EOS_UNDEFINED; /* density scaling is undefined */
	  err = EOS_INTERP_EXTRAPOLATED; /* store err until j-loop is finished */
	}
	fVals[j] = ONE / FLOOR(fVals[j]);
	/* 	dFx[j] = ????;  THIS IS CURRENTLY UNDEFINED AS DOCUMENTED IN THE USER MANUAL -- DAP */
	/* 	dFy[j] = ????;  THIS IS CURRENTLY UNDEFINED AS DOCUMENTED IN THE USER MANUAL -- DAP */

      }
      else if (pres[j] >= isotherm_p[isotherm_nr]) {

	/* isotherm_p may contain zero which is minimum possible -- see _eos_sesame_isotherm_buildRecordType1 */
	/* isotherm_r may contain zero which is minimum possible -- see SESAME data */
	/* pres may contain negative, zero and/or positive values -- input */
	/* ORIGINAL CODE:
	   fVals[j] = ONE / (isotherm_r[isotherm_nr] * (pres[j] / isotherm_p[isotherm_nr]));
	*/
        fVals[j] = isotherm_r[isotherm_nr] * (pres[j] / FLOOR(isotherm_p[isotherm_nr]));
	if (fabs(fVals[j]) < TINY_D || fabs(isotherm_p[isotherm_nr]) <= TINY_D) {
	  xyBounds[j] = EOS_CANT_INVERT_DATA;
	  if (fabs(isotherm_p[isotherm_nr]) <= TINY_D)
	    xyBounds[j] = EOS_UNDEFINED; /* density scaling is undefined */
	  err = EOS_INTERP_EXTRAPOLATED; /* store err until j-loop is finished */
	}
	fVals[j] = ONE / FLOOR(fVals[j]);
	/* 	dFx[j] = ????;  THIS IS CURRENTLY UNDEFINED AS DOCUMENTED IN THE USER MANUAL -- DAP */
	/* 	dFy[j] = ????;  THIS IS CURRENTLY UNDEFINED AS DOCUMENTED IN THE USER MANUAL -- DAP */

      }
      else {

        irlo = 0;
        irhi = isotherm_nr;

        while (irhi - irlo > 1) {

          ir = (irlo + irhi) / 2;
          if (pres[j] >= isotherm_p[ir])
            irlo = ir;
          else
            irhi = ir;

        }                       /* while */

        irhi = irlo + 1;

        sr =
          (pres[j] - isotherm_p[irlo]) / FLOOR(isotherm_p[irhi] -
					       isotherm_p[irlo]);
        fVals[j] =
          ONE / FLOOR(isotherm_r[irlo] +
		      sr * (isotherm_r[irhi] - isotherm_r[irlo]));

	if (fabs(isotherm_r[irlo] + sr * (isotherm_r[irhi] - isotherm_r[irlo])) <= TINY_D) {
	  xyBounds[j] = EOS_CANT_INVERT_DATA;
	  err = EOS_INTERP_EXTRAPOLATED; /* store err until j-loop is finished */
	}
	/* 	dFx[j] = ????;  THIS IS CURRENTLY UNDEFINED AS DOCUMENTED IN THE USER MANUAL -- DAP */
	/* 	dFy[j] = ????;  THIS IS CURRENTLY UNDEFINED AS DOCUMENTED IN THE USER MANUAL -- DAP */

      }

    }
    else if (dataType == EOS_Ut_PtT) {

      /*
	Calculate the specific internal energy values for all pres[j] and temp[j] values
      */
      if (pres[j] < isotherm_p[0]) {

        fVals[j] = isotherm_e[0];
	/* 	dFx[j] = ????;  THIS IS CURRENTLY UNDEFINED AS DOCUMENTED IN THE USER MANUAL -- DAP */
	/* 	dFy[j] = ????;  THIS IS CURRENTLY UNDEFINED AS DOCUMENTED IN THE USER MANUAL -- DAP */

      }
      else if (pres[j] >= isotherm_p[isotherm_nr]) {

        fVals[j] = isotherm_e[isotherm_nr];
	/* 	dFx[j] = ????;  THIS IS CURRENTLY UNDEFINED AS DOCUMENTED IN THE USER MANUAL -- DAP */
	/* 	dFy[j] = ????;  THIS IS CURRENTLY UNDEFINED AS DOCUMENTED IN THE USER MANUAL -- DAP */

      }
      else {

        irlo = 0;
        irhi = isotherm_nr;

        while (irhi - irlo > 1) {

          ir = (irlo + irhi) / 2;
          if (pres[j] >= isotherm_p[ir])
            irlo = ir;
          else
            irhi = ir;

        }                       /* while */

        irhi = irlo + 1;

        sr =
          (pres[j] - isotherm_p[irlo]) / (isotherm_p[irhi] -
                                          isotherm_p[irlo]);
        vol =
          ONE / (isotherm_r[irlo] +
                 sr * (isotherm_r[irhi] - isotherm_r[irlo]));

        sv = (vol - isotherm_v[irlo]) / (isotherm_v[irhi] - isotherm_v[irlo]);
        fVals[j] =
          isotherm_e[irlo] + sv * (isotherm_e[irhi] - isotherm_e[irlo]);
	/* 	dFx[j] = ????;  THIS IS CURRENTLY UNDEFINED AS DOCUMENTED IN THE USER MANUAL -- DAP */
	/* 	dFy[j] = ????;  THIS IS CURRENTLY UNDEFINED AS DOCUMENTED IN THE USER MANUAL -- DAP */

      }

    }

  }                             /* j loop */

  if (eos_GetStandardErrorCodeFromCustomErrorCode(err) != EOS_OK)
    *errorCode = err;

  /*  free temporary storage */
  EOS_FREE (isotherm_p);
  EOS_FREE (isotherm_e);
  EOS_FREE (isotherm_r);
  EOS_FREE (isotherm_v);

  _eos_DEBUG_PRINT ("*** LEAVING SESAME_INV_T\n");
}

/***********************************************************************/
/*!
 * \brief This is used to calculate an isotherm for at least one pressure at a
 *  given temperature.
 *
 * \param[out]   *nr         - EOS_INTEGER : number of isotherm data values returned
 * \param[out]   *isotherm_p - EOS_REAL : pressure values
 * \param[out]   *isotherm_e - EOS_REAL : specific internal energy isotherm
 * \param[out]   *isotherm_r - EOS_REAL : density isotherm
 * \param[out]   *isotherm_v - EOS_REAL : specific volume isotherm
 * \param[out]   *errorCode  - EOS_INTEGER : error code
 * \param[in]    *me         - eos_RecordType1 : data object pointer
 * \param[in]    temp        - EOS_REAL : temperature value
 *
 * \return none
 *
 ***********************************************************************/
void _eos_sesame_isotherm_buildRecordType1 (eos_RecordType1 *me,
                                            EOS_REAL temp,
                                            EOS_INTEGER *isotherm_nr,
                                            EOS_REAL **isotherm_p,
                                            EOS_REAL **isotherm_e,
                                            EOS_REAL **isotherm_r,
                                            EOS_REAL **isotherm_v,
                                            EOS_INTEGER *errorCode,
					    EOS_CHAR **errMsg)
{

  EOS_BOOLEAN bad;
  EOS_INTEGER use_flag, isotherm_dim;
  EOS_INTEGER it, ir, nr, itlo, ithi, which, scr_num;
  EOS_INTEGER irlo, irhi, iglo, ighi, illo, ilhi, i;
  EOS_REAL s, pmin;
  EOS_REAL plg, p, r, e, st, pmax;
  EOS_REAL tcrit, pcrit, rcrit, vcrit, ecrit;
  EOS_REAL rg, rl, vg, vl, eg, el, dedvl;
  EOS_REAL dedv, sp;
  EOS_REAL *scr_p = NULL, *scr_r = NULL, *scr_e = NULL;
  eos_RecordType2 *altEosData = NULL;
  EOS_INTEGER itadd, vapor_num;
  EOS_REAL *vapor_t = NULL, *vapor_p = NULL, *vapor_rl = NULL, *vapor_rg =
    NULL, *vapor_vl = NULL, *vapor_vg = NULL, *vapor_el = NULL, *vapor_eg =
    NULL;

  _eos_DEBUG_PRINT ("\n*** ENTERING SESAME_ISOTHERM_BUILD\n");

  if (temp < ZERO) {
    *errorCode = EOS_UNDEFINED;
    eos_SetCustomMsg_str (errMsg,
			  "_eos_sesame_isotherm_buildRecordType1 ERROR: Invalid temperature=%g",
			  temp);
    return;
  }

  if (eos_IsHandleValidEosDataMap (&gEosDataMap, me->rt2_handle)) {

    altEosData =
      (eos_RecordType2 *) gEosDataMap.dataObjects[gEosDataMap.
                                                  tableHandlesMap[me->
                                                                  rt2_handle]];

    /* find vapor data array offset */
    itadd = altEosData->vaporArrayOffset;
    vapor_num = altEosData->NT - itadd;
    if (vapor_num < 0) {
      *errorCode = EOS_UNDEFINED;
      eos_SetCustomMsg_str (errMsg,
			    "_eos_sesame_isotherm_buildRecordType1 ERROR vapor_num (%i) < 0",
			    vapor_num);
      return;
    }

    /* set vapor arrays based upon itadd */
    vapor_t = &(altEosData->T[itadd]);
    vapor_p = &(altEosData->P[itadd]);
    vapor_el = &(altEosData->EL[itadd]);
    vapor_eg = &(altEosData->EG[itadd]);
    vapor_rl = &(altEosData->RL[itadd]);
    vapor_rg = &(altEosData->RG[itadd]);
    vapor_vl = (EOS_REAL *) malloc (vapor_num * sizeof (EOS_REAL));
    vapor_vg = (EOS_REAL *) malloc (vapor_num * sizeof (EOS_REAL));

    for (i = 0; i < vapor_num; i++) {
      vapor_vl[i] = ONE / MAX (RSMALL, vapor_rl[i]);
      vapor_vg[i] = ONE / MAX (RSMALL, vapor_rg[i]);
    }
  }
  else {
    /* just use the RecordType1 data "as-is" for interpolation */
    itadd = 0;
    vapor_num = -1;             /* this negative value is critical for
                                   _eos_sesame_vapor_crit to function as designed */
  }

  scr_p = (EOS_REAL *) malloc ((me->NR + 5) * sizeof (EOS_REAL));
  scr_r = (EOS_REAL *) malloc ((me->NR + 5) * sizeof (EOS_REAL));
  scr_e = (EOS_REAL *) malloc ((me->NR + 5) * sizeof (EOS_REAL));

  EOS_FREE (*isotherm_r);
  EOS_FREE (*isotherm_p);
  EOS_FREE (*isotherm_e);
  EOS_FREE (*isotherm_v);

  isotherm_dim = me->NR + 15;
  *isotherm_r = (EOS_REAL *) malloc ((isotherm_dim) * sizeof (EOS_REAL));
  *isotherm_p = (EOS_REAL *) malloc ((isotherm_dim) * sizeof (EOS_REAL));
  *isotherm_e = (EOS_REAL *) malloc ((isotherm_dim) * sizeof (EOS_REAL));
  *isotherm_v = (EOS_REAL *) malloc ((isotherm_dim) * sizeof (EOS_REAL));

  for (i = 0; i < isotherm_dim; i++) {
    (*isotherm_r)[i] = ZERO;
    (*isotherm_p)[i] = ZERO;
    (*isotherm_e)[i] = ZERO;
    (*isotherm_v)[i] = ZERO;
  }

  _eos_sesame_vapor_crit (&tcrit, &pcrit, &rcrit, &vcrit, &ecrit, vapor_num,
                          vapor_t, vapor_p, vapor_vl, vapor_el);

  /* find bounding temperatures */

  if (temp < me->T[0]) {
    which = -1;
    st = temp / me->T[0];       /* previous temp check ensured temp >= zero; thus, me->T[0] > zero here */
    itlo = 1;
    ithi = 2;
  }
  else if (temp >= me->T[me->NT - 1]) {
    which = 1;
    st = temp / me->T[me->NT - 1];
    itlo = me->NT - 1 - 1;
    ithi = me->NT - 1;
  }
  else {
    which = 0;
    itlo = 0;
    ithi = me->NT - 1;
    while (ithi - itlo > 1) {
      it = (itlo + ithi) / 2;
      if (temp >= me->T[it])
        itlo = it;
      else
        ithi = it;
    }                           /* end while */
    ithi = itlo + 1;
    st = (temp - me->T[itlo]) / (me->T[ithi] - me->T[itlo]);
  }

  _eos_sesame_vapor_t (temp, vapor_num, vapor_p, vapor_t, vapor_vl, vapor_vg,
                       vapor_rl, vapor_rg, vapor_el, vapor_eg, &plg, &rg, &rl,
                       &vg, &vl, &eg, &el, &dedvl);

  scr_num = -1;
  if (me->R[0] > ZERO) {
    scr_num = scr_num + 1;
    scr_p[scr_num] = ZERO;
    scr_e[scr_num] = me->table2[0][0];
    scr_r[scr_num] = ZERO;
  }

  for (ir = 0; ir < me->NR; ir++) {
    r = me->R[ir];

    switch (which) {
    case -1:
      p = me->table1[0][ir];
      e = me->table2[0][ir];
      if (p > ZERO)
        p = p * st;
      break;
    case 1:
      p = me->table1[me->NT - 1][ir] * st;
      e = me->table2[me->NT - 1][ir] * st;
      break;
    default:
      p =
        me->table1[itlo][ir] + st * (me->table1[ithi][ir] -
                                     me->table1[itlo][ir]);
      e =
        me->table2[itlo][ir] + st * (me->table2[ithi][ir] -
                                     me->table2[itlo][ir]);
      break;
    }

    if (scr_num > -1) {
      if (p * scr_p[scr_num] < ZERO) {
        sp = scr_p[scr_num] / (scr_p[scr_num] - p);
        scr_p[scr_num + 1] = ZERO;
        scr_r[scr_num + 1] = scr_r[scr_num] + sp * (r - scr_r[scr_num]);
        scr_e[scr_num + 1] = scr_e[scr_num] + sp * (e - scr_e[scr_num]);
        scr_num = scr_num + 1;
      }
    }

    scr_num = scr_num + 1;
    scr_p[scr_num] = p;
    scr_e[scr_num] = e;
    scr_r[scr_num] = r;
  }                             /* ir loop */


  /* find the index of the first maximum (if any) */

  irlo = 0;
  for (ir = 1; ir <= scr_num; ir++) {
    if (scr_p[ir] < scr_p[ir - 1]) {
      irlo = ir - 1;
      break;
    }
  }                             /* ir loop */

  /* find the index of the last minimum (if any) */

  irhi = 0;
  for (ir = scr_num - 1; ir >= 0; ir--) {
    if (scr_p[ir] > scr_p[ir + 1]) {
      irhi = ir + 1;
      break;
    }
  }                             /* ir loop */


  /* determine what method to use */

  if (temp >= tcrit)
    use_flag = 1;

  else if (irhi <= 0) {
    if (irlo > 0) {
      /* call global_error('SESAME_ISOTHERM_BUILD: irhi.eq.0 .and. irlo.gt.0') */
      *errorCode = EOS_FAILED;
      eos_SetCustomMsg_str (errMsg,
			    "_eos_sesame_isotherm_buildRecordType1: irhi == 0 && irlo > 0");
      return;
    }
    use_flag = 1;
  }

  else {
    /* if (irlo.le.0)    call global_error('SESAME_ISOTHERM_BUILD: irhi.gt.0 .and. irlo.le.0') */
    if (irlo < 0) {
      *errorCode = EOS_FAILED;
      eos_SetCustomMsg_str (errMsg,
			    "_eos_sesame_isotherm_buildRecordType1: irhi > 0 && irlo < 0");
      return;
    }

    /* if (irlo.ge.irhi) call global_error('SESAME_ISOTHERM_BUILD: irlo.ge.irhi') */
    if (irlo >= irhi) {
      *errorCode = EOS_FAILED;
      eos_SetCustomMsg_str (errMsg,
			    "_eos_sesame_isotherm_buildRecordType1: irlo >= irhi");
      return;
    }

    if (irlo == 0)
      use_flag = -1;
    else if (scr_p[irlo] <= plg)
      use_flag = 1;
    else if (plg <= MAX (ZERO, scr_p[irhi]))
      use_flag = -1;
    else
      use_flag = 0;
  }
  if (scr_num + 2 > isotherm_dim) {
    /* write(*,*)'$$$ scr_num+2, isotherm_dim = ',scr_num+2, isotherm_dim */
    /* call global_error('SESAME_ISOTHERM_BUILD: scr_num+2.gt.isotherm_dim') */
    *errorCode = EOS_FAILED;
    eos_SetCustomMsg_str (errMsg,
			  "_eos_sesame_isotherm_buildRecordType1: scr_num+2>isotherm_dim");
    return;
  }

  /* do it */

  switch (use_flag) {
  case 1:

    /* isotherm is monotonic increasing */

    pmax = ZERO;
    for (ir = 0; ir <= scr_num; ir++) {
      pmax = MAX (pmax, scr_p[ir]);
      (*isotherm_p)[ir] = pmax;
      (*isotherm_r)[ir] = scr_r[ir];
      (*isotherm_e)[ir] = scr_e[ir];
    }                           /* ir */
    *isotherm_nr = scr_num;
    break;

  case -1:

    /* vapor pressure is less than zero */

    pmin = scr_p[scr_num];
    for (ir = scr_num; ir >= 0; ir--) {
      pmin = MAX (ZERO, MIN (pmin, scr_p[ir]));
      (*isotherm_p)[ir] = pmin;
      (*isotherm_r)[ir] = scr_r[ir];
      (*isotherm_e)[ir] = scr_e[ir];
    }                           /* ir */
    *isotherm_nr = scr_num;
    break;

  case 0:

    /* scr_p(irlo) > plg > max(0, scr_p(irhi)) */

    /* find dome vapor states for plg */

    iglo = 0;
    ighi = irlo;
    while (ighi - iglo > 1) {
      i = (iglo + ighi) / 2;
      if (plg >= scr_p[i])
        iglo = i;
      else
        ighi = i;
    }                           /* while */
    ighi = iglo + 1;
    s =
      MAX (ZERO,
           MIN (ONE, (plg - scr_p[iglo]) / (scr_p[ighi] - scr_p[iglo])));
    rg = scr_r[iglo] + s * (scr_r[ighi] - scr_r[iglo]);
    eg = scr_e[iglo] + s * (scr_e[ighi] - scr_e[iglo]);
    vg = ONE / rg;

    /* find dome liquid states for plg */

    illo = irhi;
    ilhi = scr_num - 1;
    while (ilhi - illo > 1) {
      i = (illo + ilhi) / 2;
      if (plg >= scr_p[i])
        illo = i;
      else
        ilhi = i;
    }                           /* while */
    ilhi = illo + 1;
    s =
      MAX (ZERO,
           MIN (ONE, (plg - scr_p[illo]) / (scr_p[ilhi] - scr_p[illo])));
    rl = scr_r[illo] + s * (scr_r[ilhi] - scr_r[illo]);
    el = scr_e[illo] + s * (scr_e[ilhi] - scr_e[illo]);
    vl = ONE / rl;

    dedv = ZERO;
    if (vg > vl)
      dedv = (eg - el) / (vg - vl);

    nr = -1;
    for (ir = 0; ir <= scr_num; ir++) {

      if (ir == ighi && rg < scr_r[ir]) {

        /* add the vapor point */

        nr = nr + 1;
        (*isotherm_r)[nr] = rg;
        (*isotherm_p)[nr] = plg;
        (*isotherm_e)[nr] = eg;
      }

      if (ir == ilhi && rl < scr_r[ir]) {

        /* add the liquid point */

        nr = nr + 1;
        (*isotherm_r)[nr] = rl;
        (*isotherm_p)[nr] = plg;
        (*isotherm_e)[nr] = el;
      }

      /* add the other points */

      nr = nr + 1;
      (*isotherm_r)[nr] = scr_r[ir];

      if (ir >= ighi && ir <= illo) {
        (*isotherm_p)[nr] = plg;
        (*isotherm_e)[nr] = el + dedv * (ONE / (*isotherm_r)[nr] - vl);
      }
      else {
        (*isotherm_p)[nr] = scr_p[ir];
        (*isotherm_e)[nr] = scr_e[ir];
      }
    }                           /* ir */
    *isotherm_nr = nr;
    break;

  default:
    /* call global_error('SESAME_ISOTHERM_BUILD: unknown use_flag') */
    *errorCode = EOS_FAILED;
    eos_SetCustomMsg_str (errMsg,
			  "_eos_sesame_isotherm_buildRecordType1: unknown use_flag");
    break;
  }

  for (ir = 0; ir <= *isotherm_nr; ir++) {
    if ((*isotherm_r)[ir] > ZERO)
      (*isotherm_v)[ir] = ONE / (*isotherm_r)[ir];
    else
      (*isotherm_v)[ir] = DBL_MAX;      //huge(ONE)
  }                             /* ir */

  /* do some testing */

  _eos_DEBUG_PRINT ("%2s %12s %12s %12s %12s\n",
                    "ir", "temp", "isotherm_p", "isotherm_e", "isotherm_r");
  for (ir = 0; ir <= *isotherm_nr; ir++)
    _eos_DEBUG_PRINT ("%2i %12.4e %12.4e %12.4e %12.4e %12.4e\n",
                      ir, temp, (*isotherm_p)[ir], (*isotherm_e)[ir],
                      (*isotherm_r)[ir]);

  bad = EOS_FALSE;
  for (ir = 1; ir <= *isotherm_nr; ir++) {

    if ((*isotherm_p)[ir] < (*isotherm_p)[ir - 1]) {
      bad = EOS_TRUE;
      /*               if (mype.eq.iope) then */
      /*                 write(*,"(a,i6,1p,e12.4,2e20.12)") & */
      /*                      ' SESAME_ISOTHERM_BUILD: ir, t, p1, p2 = ', & */
      /*                      ir,temp,isotherm_p(ir-1),isotherm_p(ir) */
      /*               endif */
      _eos_DEBUG_PRINT
        ("_eos_sesame_isotherm_buildRecordType1: ir, t, p1, p2 = %6i%12.4e%20.12e%20.12e\n",
         ir, temp, (*isotherm_p)[ir - 1], (*isotherm_p)[ir]);
    }

    if ((*isotherm_r)[ir] < (*isotherm_r)[ir - 1]) {
      bad = EOS_TRUE;
      /*               if (mype.eq.iope) then */
      /*                 write(*,"(a,i6,1p,e12.4,2e20.12)") & */
      /*                      ' SESAME_ISOTHERM_BUILD: ir, t, r1, r2 = ', & */
      /*                      ir,temp,isotherm_r(ir-1),isotherm_r(ir) */
      /*               endif */
      _eos_DEBUG_PRINT
        ("_eos_sesame_isotherm_buildRecordType1: ir, t, r1, r2 = %6i%12.4e%20.12e%20.12e\n",
         ir, temp, (*isotherm_r)[ir - 1], (*isotherm_r)[ir]);
    }

  }                             /* ir */

  if (bad) {
    for (ir = 0; ir <= scr_num; ir++) {
      /*               write(*,"(a,i6,1p,6e12.4)")'$$$ ir, scr_r, scr_p, scr_e = ', & */
      /*                    ir, scr_r(ir), scr_p(ir), scr_e(ir) */
      _eos_DEBUG_PRINT
        ("_eos_sesame_isotherm_buildRecordType1: $$$ ir, scr_r, scr_p, scr_e = %6i%12.4e%12.4e%12.4e%12.4e%12.4e%12.4e\n",
         ir, temp, scr_r[ir], scr_p[ir], scr_e[ir]);
    }                           /* ir */
    for (ir = 0; ir <= *isotherm_nr; ir++) {
      /*               write(*,"(a,i6,1p,6e12.4)")'$$$ ir, isotherm_r, isotherm_p, isotherm_e = ', & */
      /*                    ir, isotherm_r(ir), isotherm_p(ir), isotherm_e(ir) */
      _eos_DEBUG_PRINT
        ("_eos_sesame_isotherm_buildRecordType1: $$$ ir, isotherm_r, isotherm_p, isotherm_e = %6i%12.4e%12.4e%12.4e%12.4e%12.4e%12.4e\n",
         ir, temp, (*isotherm_r)[ir], (*isotherm_p)[ir], (*isotherm_e)[ir]);
    }                           /* ir */
    /*             call global_error('SESAME_ISOTHERM_BUILD: bad build') */
    *errorCode = EOS_FAILED;
    eos_SetCustomMsg_str (errMsg,
			  "_eos_sesame_isotherm_buildRecordType1: bad build");
  }

  /*  free temporary storage */
  EOS_FREE (scr_p);
  EOS_FREE (scr_r);
  EOS_FREE (scr_e);
  EOS_FREE (vapor_vl);
  EOS_FREE (vapor_vg);

  _eos_DEBUG_PRINT ("*** LEAVING SESAME_ISOTHERM_BUILD\n");
}
