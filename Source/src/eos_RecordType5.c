/*********************************************************************
 * Class Name : eos_RecordType5
 * ---------------------------------------------------------
 * Filetype: (SOURCE)
 * 
 * Copyright -- see file named COPYRIGHTNOTICE
 * 
 *********************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define _EOS_RECORDTYPE5_INTERNAL_PROTOTYPES
#include "eos_types_internal.h"
#include "eos_RecordType5.h"

#include "eos_Utils.h"

/************************************************************************/
/*!
 * \brief RecordType5 class constructor
 *
 * \param[in,out] *me        - eos_RecordType5 : data object pointer;
 *                                               contents of object are initialized
 * \param[in]     materialID - EOS_INTEGER     : id of material to load
 * \param[in]     th         - EOS_INTEGER     : table handle
 *
 * \return none
 *
 ************************************************************************/
void eos_ConstructRecordType5 (eos_RecordType5 *me, EOS_INTEGER th,
                               EOS_INTEGER materialID)
{
  /* SESAME table 201 data */
  me->avgAtomicNumber = (EOS_REAL) 0;
  me->avgAtomicWgt = (EOS_REAL) 0;
  me->refDensity = (EOS_REAL) 0;
  me->solidBulkModulus = (EOS_REAL) 0;
  me->exchangeCoefficient = (EOS_REAL) 0;

  /* Miscellaneous metadata */
  me->eosData.varOrder = -1;
  me->eosData.tmpVarOrder = -1;
  me->eosData.dataFileOffset = -1;
  me->eosData.dataFileIndex = -1;
  me->eosData.dataSize = 0;

  /* Create eos_DataMap */
  eos_ConstructEosData ((eos_Data *) me, th, materialID);

  /* Define class-specific virtual functions */
  me->eosData.Load = eos_LoadRecordType5;
  me->eosData.Create = eos_CreateRecordType5;
  me->eosData.Destroy = eos_DestroyRecordType5;
  me->eosData.SetFileIndexes = eos_SetFileIndexesRecordType5;
  me->eosData.Print = eos_PrintRecordType5;
  me->eosData.GetPackedTable = eos_GetPackedTableRecordType5;
  me->eosData.SetPackedTable = eos_SetPackedTableRecordType5;
  me->eosData.GetPackedTableSize = eos_GetPackedTableSizeRecordType5;
  me->eosData.IsMonotonic = eos_IsMonotonicRecordType5;
  me->eosData.MakeMonotonic = eos_MakeMonotonicRecordType5;
  me->eosData.MakeSmooth = eos_MakeSmoothRecordType5;
  me->eosData.GetTableInfo = eos_GetTableInfoRecordType5;
  me->eosData.GetTableMetaData = eos_GetTableMetaDataRecordType5;
  me->eosData.GetLoadedBulkData = eos_GetLoadedBulkDataRecordType5;
  me->eosData.SetMonotonicity = eos_SetMonotonicityRecordType5;
  me->eosData.GetMonotonicity = eos_GetMonotonicityRecordType5;
  me->eosData.GetSmoothing = eos_GetSmoothingRecordType5;
  me->eosData.AreMonotonicRequirementsCompatible = eos_AreMonotonicRequirementsCompatibleRecordType5;
  me->eosData.SetSmoothing = eos_SetSmoothingRecordType5;
  me->eosData.AreSmoothingRequirementsCompatible = eos_AreSmoothingRequirementsCompatibleRecordType5;
  me->eosData.Interpolate = NULL; /* no interpolation allowed */
  me->eosData.CheckExtrap = NULL; /* no interpolation allowed */
  me->eosData.InvertAtSetup = NULL; /* no inversion at setup allowed */
  me->eosData.SetExtrapolationBounds = NULL; /* no extrapolation bounds stored */
  me->eosData.AreGhostDataRequired = NULL; /* no ghost node data required*/
  me->eosData.AddGhostData = NULL; /* no ghost node data required*/
}

/************************************************************************
 * 
 * This function allocates RecordType5 class's
 * data structures for future loading.
 * 
 * Returned Values: none
 *
 * Input Value:
 * eos_RecordType5 *me  - this pointer (pointer to the instance of type eos_RecordType5
 * 
 ************************************************************************/
void eos_CreateRecordType5 (void *ptr, EOS_INTEGER th)
{
  eos_RecordType5 *me;

  me = (eos_RecordType5 *) ptr;

  gEosDataMap.errorCodes[th] = EOS_OK; /* reset previous error */
 
  /* set the sesame file indexes and offsets for RecordType5 */
  eos_SetFileIndexesRecordType5 (me, th);

  if (gEosDataMap.errorCodes[th]) /* return previous error to parent */
    return;

  /* check the size of data */
  if (me->eosData.dataSize != 5) {
    ((eos_ErrorHandler *) me)->HandleError (me, th, EOS_READ_DATA_FAILED);
    return;                     /* wrong amount of data read! */
  }

  me->eosData.isAllocated = 1;
}

/***********************************************************************/
/*! 
 * \brief This function sets the sesame file indexes and offsets for RecordType5
 * 
 * \param[in,out] *ptr       - void : data object pointer;
 *                             internally recast to eos_RecordType5*
 *                             contents are allocated
 * \param[in]     th         - EOS_INTEGER : table handle
 * 
 * \return none
 *
 ***********************************************************************/
void eos_SetFileIndexesRecordType5 (void *ptr, EOS_INTEGER th)
{
  eos_RecordType5 *me;
  EOS_INTEGER matid;
  EOS_INTEGER ierr = EOS_OK, count;
  EOS_REAL *read_data;
  EOS_CHAR *errMsg = NULL;

  me = (eos_RecordType5 *) ptr;

  matid = me->eosData.materialID;
  read_data = NULL;
  /* seek to the table */
  /* get the file offset for reading data and 3 reals date1, date2 and vers */
  {
    ses_material_id mid = (ses_material_id)me->eosData.materialID;
    ses_table_id    tid = (ses_table_id)me->eosData.tableNum;
    ierr = eos_SesSeekToDataTable (matid, me->eosData.tableNum, &read_data, 3,
				   &(me->eosData.dataFileIndex),
				   &mid, &tid, &(me->eosData.dataSize),
				   me->eosData.userDefinedDataFile, &errMsg);
    if (errMsg) ierr = eos_SetCustomErrorMsg(th, ierr, "%s", errMsg);
    EOS_FREE(errMsg);
  }
  if (ierr) {
    /* deallocate read_data[], which was allocated by eos_SesSeekToDataTable() above */
    EOS_FREE (read_data);
    ((eos_ErrorHandler *) me)->HandleError (me, th, ierr);
    return;
  }

  count = 0;
  /* read creation date */
  me->eosData.creationDate = (EOS_INTEGER) read_data[count++];     /* 1 */
  /* read modification date */
  me->eosData.modificationDate = (EOS_INTEGER) read_data[count++]; /* 2 */
  if (me->eosData.modificationDate == 0)
    me->eosData.modificationDate = me->eosData.creationDate;
  /* read version number */
  me->eosData.latestVersion = (EOS_INTEGER) read_data[count++];    /* 3 */

  /* deallocate read_data[], which was allocated by eos_SesSeekToDataTable() above */
  EOS_FREE (read_data);

  /* check the size of data */
  if (me->eosData.dataSize != 5) {
    ((eos_ErrorHandler *) me)->HandleError (me, th, EOS_READ_DATA_FAILED);
    return;                     /* wrong amount of data read! */
  }
}

/************************************************************************
 * 
 * This function loads data of RecordType5 and stores the data in the class's
 * data structures.
 * 
 * Returned Values: none
 *
 * Input Value:
 * eos_RecordType5 *me  - this pointer (pointer to the instance of type eos_RecordType5
 * 
 ************************************************************************/
void eos_LoadRecordType5 (void *ptr, EOS_INTEGER th)
{
  EOS_INTEGER count, ierr = EOS_OK;
  EOS_REAL *read_data;
  eos_RecordType5 *me;

  me = (eos_RecordType5 *) ptr;

  /* check handle's error code; this was moved from Create upon the introduction
   * of the eos_SetDataFileName function */
  if (eos_GetStandardErrorCodeFromCustomErrorCode(gEosDataMap.errorCodes[th]) == EOS_MATERIAL_NOT_FOUND) {
    ierr = gEosDataMap.errorCodes[th];
    ((eos_ErrorHandler *) me)->HandleError (me, th, ierr);
    return;
  }

#ifdef DEBUG
  printf ("loading data for type 5...\n");
#endif

  read_data = NULL;
  /* Load the table */
  ierr =
    eos_SesLoadSesameFiles (me->eosData.materialID, me->eosData.tableNum,
			    me->eosData.dataFileIndex, &read_data,
			    me->eosData.dataSize);
  if (ierr) {
#ifdef DEBUG
    printf ("error loading table! ERROR %d\n", ierr);
#endif
    ((eos_ErrorHandler *) me)->HandleError (me, th, ierr);
    return;
  }

  count = 0;

  me->avgAtomicNumber = read_data[count++];    /* 0 */
  me->avgAtomicWgt = read_data[count++];      /* 0 */
  me->refDensity = read_data[count++];       /* 0 */
  me->solidBulkModulus = read_data[count++];    /* 0 */
  me->exchangeCoefficient = read_data[count++]; /* 0 */
  EOS_FREE (read_data);

  /* store the number of subTables stored in memory */
  me->eosData.numSubtablesLoaded = 5;

  me->eosData.isLoaded = 1;
}

/************************************************************************
 * 
 * RecordType5 class destructor
 * 
 * Returned Values: none
 *
 * Input Value:
 * eos_RecordType5 *me  - this pointer (pointer to the instance of type eos_RecordType5
 * 
 ************************************************************************/
void eos_DestroyRecordType5 (void* ptr)
{
  eos_RecordType5 *me = (eos_RecordType5*) ptr;

  if (!me->eosData.destructing) /* to prevent circular calls to destructor */
    eos_DestroyEosData (&(me->eosData));
}

/************************************************************************
 * 
 * This function reurns prints the data of class eos_RecordType5
 * 
 * Returned Values: EOS_INTEGER *err - output error code
 *
 * Input Value:
 * void *ptr       - this pointer (pointer to the instance of type eos_RecordType5
 * EOS_CHAR * fname;
 * EOS_INTEGER append   - whether or not to append to file
 * EOS_INTEGER th       - table handle
 *
 ************************************************************************/
void eos_PrintRecordType5 (void *ptr, EOS_INTEGER th, EOS_CHAR *fname,
                           EOS_INTEGER append, EOS_INTEGER *err)
{
  EOS_CHAR *sesame_fname, buf[_MIN_FIELD_WIDTH+1];
  FILE *tableFile;
  eos_RecordType5 *me;
  EOS_INTEGER dataType;

  me = (eos_RecordType5 *) ptr;
  *err = EOS_OK;

  dataType = eos_GetDataTypeFromTableHandle (th, err);
  if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) != EOS_OK)
    return;

  tableFile = (append == 0) ? fopen (fname, "w") : fopen (fname, "a");
  if (!tableFile) {
    *err = EOS_OPEN_OUTPUT_FILE_FAILED;
    return;
  }

  /* figure out the name of the sesame file */
  sesame_fname = _eos_GetSesameFileName (&(me->eosData));
  if (sesame_fname == NULL) {
    sesame_fname = "";
    *err = EOS_OPEN_SESAME_FILE_FAILED;
    fclose (tableFile);
    return;
  }

  fprintf (tableFile,
           "%sTableHandle=%i matid =%6d source = %-20s\n%s%s\n%s%s\n",
           ((append) ? "\n" : ""), th, me->eosData.materialID, sesame_fname,
           "Data Type = ", EOS_TYPE_TO_STRING (dataType),
           "Description = ", EOS_TYPE_TO_TAB_NAME (dataType));

  _eos_dbl2String (me->avgAtomicNumber, _MIN_FIELD_WIDTH, buf);
  fprintf (tableFile, "ZBAR: %*f\n", _MIN_FIELD_WIDTH + 1,
           me->avgAtomicNumber);
  _eos_dbl2String (me->avgAtomicWgt, _MIN_FIELD_WIDTH, buf);
  fprintf (tableFile, "ABAR: %*f\n", _MIN_FIELD_WIDTH + 1,
           me->avgAtomicWgt);
  _eos_dbl2String (me->refDensity, _MIN_FIELD_WIDTH, buf);
  fprintf (tableFile, "RHO0: %*f\n", _MIN_FIELD_WIDTH + 1, me->refDensity);
  _eos_dbl2String (me->solidBulkModulus, _MIN_FIELD_WIDTH, buf);
  fprintf (tableFile, "BO:   %*f\n", _MIN_FIELD_WIDTH + 1,
           me->solidBulkModulus);
  _eos_dbl2String (me->exchangeCoefficient, _MIN_FIELD_WIDTH, buf);
  fprintf (tableFile, "CEX:  %*f\n", _MIN_FIELD_WIDTH + 1,
           me->exchangeCoefficient);

  fclose (tableFile);
}

/************************************************************************
 * 
 * This packs the data of class eos_RecordType5 into provided char array
 * 
 * Returned Values: EOS_INTEGER *err - output error code
 *
 * Input Value:
 * EOS_INTEGER th  - table handle
 * void *ptr       - this pointer (pointer to the instance of type eos_RecordType5
 * EOS_CHAR *packedTable - allocated by user char array large enough to store packed data
 * 
 ************************************************************************/
void eos_GetPackedTableRecordType5 (void *ptr, EOS_INTEGER th,
                                    EOS_CHAR *packedTable, EOS_INTEGER *err)
{
  eos_RecordType5 *me;
  EOS_INTEGER byteCount = 0;

  me = (eos_RecordType5 *) ptr;
  *err = EOS_OK;

  memcpy (packedTable + byteCount, &(me->avgAtomicNumber),
          sizeof (EOS_REAL));
  byteCount += sizeof (EOS_REAL);
  memcpy (packedTable + byteCount, &(me->avgAtomicWgt), sizeof (EOS_REAL));
  byteCount += sizeof (EOS_REAL);
  memcpy (packedTable + byteCount, &(me->refDensity), sizeof (EOS_REAL));
  byteCount += sizeof (EOS_REAL);
  memcpy (packedTable + byteCount, &(me->solidBulkModulus),
          sizeof (EOS_REAL));
  byteCount += sizeof (EOS_REAL);
  memcpy (packedTable + byteCount, &(me->exchangeCoefficient),
          sizeof (EOS_REAL));
  byteCount += sizeof (EOS_REAL);
  memcpy (packedTable + byteCount, &(me->eosData.numSubtablesLoaded),
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (packedTable + byteCount, &(me->eosData.isLoaded),
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);

  memcpy (packedTable + byteCount, &(me->eosData.dataFileIndex),
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (packedTable + byteCount, &(me->eosData.dataFileOffset),
          sizeof (long));
  byteCount += sizeof (long);
  memcpy (packedTable + byteCount, &(me->eosData.dataSize),
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
}


/************************************************************************
 * 
 * This sets the data of class eos_RecordType5 from the packed char array provided by caller
 * 
 * Returned Values: EOS_INTEGER *err - output error code
 *
 * Input Value:
 * void *ptr       - this pointer (pointer to the instance of type eos_RecordType5
 * EOS_CHAR *packedTable - allocated by user char array large enough to store packed data
 * EOS_INTEGER packedTableSize - size in chars of packed data array
 * EOS_INTEGER th - table handle
 * 
 ************************************************************************/
void eos_SetPackedTableRecordType5 (void *ptr, EOS_INTEGER th,
                                    EOS_CHAR *packedTable, EOS_INTEGER *err)
{
  eos_RecordType5 *me;
  EOS_INTEGER byteCount = 0, tmpINT;

  me = (eos_RecordType5 *) ptr;
  *err = EOS_OK;

  memcpy (&(me->avgAtomicNumber), packedTable + byteCount,
          sizeof (EOS_REAL));
  byteCount += sizeof (EOS_REAL);
  memcpy (&(me->avgAtomicWgt), packedTable + byteCount, sizeof (EOS_REAL));
  byteCount += sizeof (EOS_REAL);
  memcpy (&(me->refDensity), packedTable + byteCount, sizeof (EOS_REAL));
  byteCount += sizeof (EOS_REAL);
  memcpy (&(me->solidBulkModulus), packedTable + byteCount,
          sizeof (EOS_REAL));
  byteCount += sizeof (EOS_REAL);
  memcpy (&(me->exchangeCoefficient), packedTable + byteCount,
          sizeof (EOS_REAL));
  byteCount += sizeof (EOS_REAL);
  memcpy (&(me->eosData.numSubtablesLoaded), packedTable + byteCount,
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (&(tmpINT), packedTable + byteCount, sizeof (EOS_INTEGER));
  me->eosData.isLoaded = (tmpINT != 0) ? 1 : 0;
  byteCount += sizeof (EOS_INTEGER);

  memcpy (&(me->eosData.dataFileIndex), packedTable + byteCount,
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (&(me->eosData.dataFileOffset), packedTable + byteCount,
          sizeof (long));
  byteCount += sizeof (long);
  memcpy (&(me->eosData.dataSize), packedTable + byteCount,
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
}

/************************************************************************
 * 
 * This returns the size of packed char array needed to store the data of class eos_RecordType5
 * 
 * Returned Values: EOS_INTEGER *err - output error code
 *
 * Input Value:
 * void *ptr       - this pointer (pointer to the instance of type eos_RecordType5
 * EOS_INTEGER *packedTableSize - size in chars of packed data array
 * EOS_INTEGER th  - table handle
 * 
 ************************************************************************/
void eos_GetPackedTableSizeRecordType5 (void *ptr, EOS_INTEGER th,
                                        EOS_INTEGER *packedTableSize,
                                        EOS_INTEGER *err)
{
  eos_RecordType5 *me;

  me = (eos_RecordType5 *) ptr;
  *err = EOS_OK;

  *packedTableSize = 5 * sizeof (EOS_REAL);
  *packedTableSize += 2 * sizeof (EOS_INTEGER); /* me->eosData.numSubtablesLoaded and
                                                   me->eosData.isLoaded */
  *packedTableSize += (sizeof (EOS_INTEGER) * 2 + sizeof (long));

  if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) != EOS_OK) {
    ((eos_ErrorHandler *) me)->HandleError (me, th, *err);
    return;
  }
}

/************************************************************************
 * 
 * Makes data of class eos_RecordType5 monotonic
 * 
 * Returned Values: EOS_INTEGER *err - output error code
 *
 * Input Value:
 * void *ptr       - this pointer (pointer to the instance of type eos_RecordType5
 * EOS_INTEGER th  - table handle for error handling
 * EOS_INTEGER dataType - data type of subtable to be made monotonic
 * EOS_BOOLEAN inX, inY - to make monotonic in x, in y, or both
 * EOS_INTEGER *err   - error flag
 * 
 ************************************************************************/
void eos_MakeMonotonicRecordType5 (void *ptr, EOS_INTEGER th,
                                   EOS_INTEGER dataType, EOS_BOOLEAN inX,
                                   EOS_BOOLEAN inY, EOS_INTEGER *err)
{
  eos_RecordType5 *me;

  me = (eos_RecordType5 *) ptr;
  *err = EOS_OK;

  if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) != EOS_OK) {
    ((eos_ErrorHandler *) me)->HandleError (me, th, *err);
    return;
  }
}


/************************************************************************
 * 
 * checks if the data of class eos_RecordType5 is monotonic
 * 
 * Returned Values: EOS_INTEGER *err - output error code
 *
 * Input Value:
 * void *ptr       - this pointer (pointer to the instance of type eos_RecordType5
 * EOS_BOOLEAN *isMonotonic - result
 * 
 ************************************************************************/
void eos_IsMonotonicRecordType5 (void *ptr, EOS_INTEGER dataType,
                                 EOS_BOOLEAN *isMonotonic, EOS_BOOLEAN inX,
                                 EOS_BOOLEAN inY, EOS_INTEGER *err)
{
  *err = EOS_OK;
}


/************************************************************************
 * 
 * Makes data of class eos_RecordType5 smooth
 * 
 * Returned Values: EOS_INTEGER *err - output error code
 *
 * Input Value:
 * void *ptr       - this pointer (pointer to the instance of type eos_RecordType5
 * EOS_INTEGER *err   - error flag
 * 
 ************************************************************************/
void eos_MakeSmoothRecordType5 (void *ptr, EOS_INTEGER th,
                                EOS_INTEGER dataType, EOS_BOOLEAN ptSmooth,
                                EOS_INTEGER *err)
{
  eos_RecordType5 *me;
  me = (eos_RecordType5 *) ptr;
  *err = EOS_OK;

  if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) != EOS_OK) {
    ((eos_ErrorHandler *) me)->HandleError (me, th, *err);
    return;
  }
}

/************************************************************************
 * 
 * returns information items for the table.
 * 
 * Returned Values: EOS_INTEGER *err - output error code
 *
 * Input Value:
 * void *ptr                - this pointer (pointer to the instanse of type eos_RecordType4
 * EOS_INTEGER numInfoItems - # of requested items
 * EOS_INTEGER *infoItems   - array of requested items
 * EOS_REAL *infoVals       - return item values
 * EOS_INTEGER th           - table handle
 * EOS_INTEGER *err         - error flag
 * 
 ************************************************************************/
void eos_GetTableInfoRecordType5 (void *ptr, EOS_INTEGER th,
                                  EOS_INTEGER numInfoItems,
                                  EOS_INTEGER *infoItems, EOS_REAL *infoVals,
                                  EOS_INTEGER *err)
{
  EOS_INTEGER i;
  eos_RecordType5 *me;
  me = (eos_RecordType5 *) ptr;
  *err = EOS_OK;

  for (i = 0; i < numInfoItems; i++) {
    switch (infoItems[i]) {
    case EOS_Log_Val:
      infoVals[i] =
        EOS_TYPE_TO_LOG_AXES (eos_GetDataTypeFromTableHandle (th, err));
      break;

    case EOS_Material_ID:
      infoVals[i] = me->eosData.materialID;
      break;

    case EOS_Table_Type:
      infoVals[i] = eos_GetDataTypeFromTableHandle (th, err);
      break;

    case EOS_Mean_Atomic_Num:
      infoVals[i] = me->avgAtomicNumber;
      break;

    case EOS_Mean_Atomic_Mass:
      infoVals[i] = me->avgAtomicWgt;
      break;

    case EOS_Modulus:
      infoVals[i] = me->solidBulkModulus;
      break;

    case EOS_Normal_Density:
      infoVals[i] = me->refDensity;
      break;

    case EOS_Exchange_Coeff:
      infoVals[i] = me->exchangeCoefficient;
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
 * void *ptr           - this pointer (pointer to the instance of type eos_RecordType5
 * EOS_CHAR *infoItem  - flag specifying what meta data item to fetch
 * 
 ************************************************************************/
void eos_GetTableMetaDataRecordType5 (void *ptr, EOS_INTEGER infoItem,
				      EOS_CHAR *infoStr, EOS_INTEGER *err)
{
  //eos_RecordType5 *me;

  //me = (eos_RecordType5 *) ptr;
  *err = EOS_INVALID_DATA_TYPE; /* 101 table not found */
}

 /*************************************************************************
 *
 * Function eos_GetLoadedBulkDataRecordType5
 * Description:
 * 
 * This function gets data from 201 table. These values are needed
 * for entropy and free energy data calculations.
 * 
 * Returned Values:
 * EOS_INTEGER errorCode - output error code
 * EOS_REAL    *zbar      - mean atomic number
 * EOS_REAL    *abar      - mean atomic mass
 * EOS_REAL    *dens0     - normal solid density
 *
 * Input Value:
 * eos_RecordType5 *me
 *************************************************************************/
void eos_GetLoadedBulkDataRecordType5 (void *ptr, EOS_REAL *zbar,
                                       EOS_REAL *abar, EOS_REAL *dens0,
                                       EOS_INTEGER *errorCode)
{
  *errorCode = EOS_INVALID_DATA_TYPE;
}

 /*************************************************************************
 *
 * Function eos_SetMonotonicityRecordType5
 * Description:
 * 
 * set the flags so that appropriate data will be made monotonic upon loading
 * 
 * Returned Values:
 *
 * Input Value:
 * eos_RecordType5 *me
 * EOS_INTEGER dataType
 * EOS_INTEGER inX
 * EOS_INTEGER inY
 *************************************************************************/
void eos_SetMonotonicityRecordType5 (void *me, EOS_INTEGER dataType,
                                     EOS_INTEGER inX, EOS_INTEGER inY)
{
}

/*************************************************************************
 *
 * Function eos_GetMonotonicityRecordType5
 * Description:
 * 
 * set the flags so that appropriate data will be made monotonic upon loading
 * 
 * Returned Values:
 *
 * Input Value:
 * eos_RecordType5 *me
 * EOS_INTEGER dataType
 * EOS_INTEGER *inX
 * EOS_INTEGER *inY
 *************************************************************************/
void eos_GetMonotonicityRecordType5 (void *me, EOS_INTEGER dataType,
                                     EOS_INTEGER *inX, EOS_INTEGER *inY)
{
  *inX = *inY = 0;
}

/*************************************************************************
 *
 * Function eos_GetSmoothingRecordType5
 * Description:
 * 
 * set the flags so that appropriate data will be made monotonic upon loading
 * 
 * Returned Values:
 *
 * Input Value:
 * eos_RecordType5 *me
 * EOS_INTEGER dataType
 * EOS_INTEGER *isSmooth
 * EOS_INTEGER *isPtSmooth
 *************************************************************************/
void eos_GetSmoothingRecordType5 (void *me, EOS_INTEGER dataType,
                                  EOS_INTEGER *isSmooth,
                                  EOS_INTEGER *isPtSmooth)
{
  *isSmooth = 0;
  *isPtSmooth = 0;
}

/*************************************************************************
 *
 * Function eos_AreMonotonicRequirementsCompatibleRecordType5
 * Description:
 * 
 * find out if the requested type, monotonicity combo can share our table object
 * 
 * Returned Values:
 * EOS_BOOLEAN *compatible
 *
 * Input Value:
 * eos_RecordType5 *me
 * EOS_INTEGER dataType
 * EOS_INTEGER inX
 * EOS_INTEGER inY
 *************************************************************************/
void eos_AreMonotonicRequirementsCompatibleRecordType5 (void *me,
                                                        EOS_INTEGER dataType,
                                                        EOS_INTEGER inX,
                                                        EOS_INTEGER inY,
                                                        EOS_BOOLEAN
                                                        *compatible)
{
  *compatible = EOS_TRUE;
}

 /*************************************************************************
 *
 * Function eos_SetSmoothingDataRecordType5
 * Description:
 * 
 * set the flags so that appropriate data will be made smooth upon loading
 * 
 * Returned Values:
 *
 * Input Value:
 * eos_RecordType5 *me
 * EOS_INTEGER dataType
 * EOS_INTEGER makeSmooth
 * EOS_INTEGER makePtSmooth
 *************************************************************************/
void eos_SetSmoothingRecordType5 (void *me, EOS_INTEGER dataType,
                                  EOS_INTEGER makeSmooth,
                                  EOS_INTEGER makePtSmooth)
{
}

/*************************************************************************
 *
 * Function eos_AreSmoothingRequirementsCompatibleRecordType5
 * Description:
 * 
 * find out if the requested type, monotonicity combo can share our table object
 * 
 * Returned Values:
 * EOS_BOOLEAN *compatible
 *
 * Input Value:
 * eos_RecordType5 *me
 * EOS_INTEGER dataType
 * EOS_INTEGER makeSmooth
 * EOS_INTEGER makePtSmooth
 *************************************************************************/
void eos_AreSmoothingRequirementsCompatibleRecordType5 (void *me,
                                                        EOS_INTEGER dataType,
                                                        EOS_INTEGER
                                                        makeSmooth,
                                                        EOS_INTEGER
                                                        makePtSmooth,
                                                        EOS_BOOLEAN
                                                        *compatible)
{
  *compatible = EOS_TRUE;
}
