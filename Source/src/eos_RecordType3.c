/*********************************************************************
 * Class Name : eos_RecordType3
 * ---------------------------------------------------------
 * Filetype: (SOURCE)
 * 
 * Copyright -- see file named COPYRIGHTNOTICE
 * 
 *********************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define _EOS_RECORDTYPE3_INTERNAL_PROTOTYPES
#include "eos_types_internal.h"
#include "eos_RecordType3.h"

#include "eos_Utils.h"

/************************************************************************/
/*!
 * \brief RecordType3 class constructor
 *
 * \param[in,out] *me        - eos_RecordType3 : data object pointer;
 *                                               contents of object are initialized
 * \param[in]     materialID - EOS_INTEGER     : id of material to load
 * \param[in]     th         - EOS_INTEGER     : table handle
 *
 * \return none
 *
 ************************************************************************/
void eos_ConstructRecordType3 (eos_RecordType3 *me, EOS_INTEGER th,
                               EOS_INTEGER materialID)
{
  /* SESAME table 201 data */
  me->avgAtomicNumber = (EOS_REAL) 0;
  me->avgAtomicWgt = (EOS_REAL) 0;
  me->refDensity = (EOS_REAL) 0;
  me->solidBulkModulus = (EOS_REAL) 0;
  me->exchangeCoefficient = (EOS_REAL) 0;

  /* SESAME table data */
  me->N = 0;
  me->T = NULL;
  me->R = NULL;

  /* Miscellaneous metadata */
  me->eosData.varOrder = -1;
  me->eosData.tmpVarOrder = -1;
  me->eosData.dataFileOffset = -1;
  me->eosData.dataFileIndex = -1;
  me->eosData.dataSize = 0;

  /* Create eos_DataMap */
  eos_ConstructEosData ((eos_Data *) me, th, materialID);

  /* Define class-specific virtual functions */
  me->eosData.Load = eos_LoadRecordType3;
  me->eosData.Create = eos_CreateRecordType3;
  me->eosData.Destroy = eos_DestroyRecordType3;
  me->eosData.SetFileIndexes = eos_SetFileIndexesRecordType3;
  me->eosData.Print = eos_PrintRecordType3;
  me->eosData.GetPackedTable = eos_GetPackedTableRecordType3;
  me->eosData.SetPackedTable = eos_SetPackedTableRecordType3;
  me->eosData.GetPackedTableSize = eos_GetPackedTableSizeRecordType3;
  me->eosData.GetTableInfo = eos_GetTableInfoRecordType3;
  me->eosData.GetTableMetaData = eos_GetTableMetaDataRecordType3;
  me->eosData.IsMonotonic = eos_IsMonotonicRecordType3;
  me->eosData.MakeMonotonic = eos_MakeMonotonicRecordType3;
  me->eosData.MakeSmooth = eos_MakeSmoothRecordType3;
  me->eosData.GetLoadedBulkData = eos_GetLoadedBulkDataRecordType3;
  me->eosData.SetMonotonicity = eos_SetMonotonicityRecordType3;
  me->eosData.GetMonotonicity = eos_GetMonotonicityRecordType3;
  me->eosData.GetSmoothing = eos_GetSmoothingRecordType3;
  me->eosData.AreMonotonicRequirementsCompatible = eos_AreMonotonicRequirementsCompatibleRecordType3;
  me->eosData.SetSmoothing = eos_SetSmoothingRecordType3;
  me->eosData.AreSmoothingRequirementsCompatible = eos_AreSmoothingRequirementsCompatibleRecordType3;
  me->eosData.Interpolate = NULL; /* no interpolation allowed */
  me->eosData.CheckExtrap = NULL; /* no interpolation allowed */
  me->eosData.InvertAtSetup = NULL; /* no inversion at setup allowed */
  me->eosData.SetExtrapolationBounds = NULL; /* no extrapolation bounds stored */
  me->eosData.AreGhostDataRequired = NULL; /* no ghost node data required*/
  me->eosData.AddGhostData = NULL; /* no ghost node data required*/
}

/************************************************************************
 * 
 * This function allocates data of RecordType3 for loading
 * 
 * Returned Values: none
 *
 * Input Value:
 * eos_RecordType3 *me  - this pointer (pointer to the instance of type eos_RecordType3
 * 
 ************************************************************************/
void eos_CreateRecordType3 (void *ptr, EOS_INTEGER th)
{
  eos_RecordType3 *me;

  me = (eos_RecordType3 *) ptr;

  gEosDataMap.errorCodes[th] = EOS_OK; /* reset previous error */
 
  /* set the sesame file indexes and offsets for RecordType3 */
  eos_SetFileIndexesRecordType3 (me, th);

  if (gEosDataMap.errorCodes[th]) /* return previous error to parent */
    return;

  if (me->eosData.dataSize != 1 + 2 * me->N) {
    ((eos_ErrorHandler *) me)->HandleError (me, th, EOS_READ_DATA_FAILED);
    return;                     /* wrong amount of data read! */
  }

  /* allocate enough memory */
  eos_SetSizeRecordType3 (me, me->N);
}

/***********************************************************************/
/*! 
 * \brief This function sets the sesame file indexes and offsets for RecordType3
 * 
 * \param[in,out] *ptr       - void : data object pointer;
 *                             internally recast to eos_RecordType3*
 *                             contents are allocated
 * \param[in]     th         - EOS_INTEGER : table handle
 * 
 * \return none
 *
 ***********************************************************************/
void eos_SetFileIndexesRecordType3 (void *ptr, EOS_INTEGER th)
{
  eos_RecordType3 *me;
  me = (eos_RecordType3 *) ptr;
  EOS_INTEGER matid, count;
  EOS_INTEGER ierr = EOS_OK;
  EOS_REAL *read_data;
  EOS_CHAR *errMsg = NULL;

  matid = me->eosData.materialID;
  read_data = NULL;

  /* get the file offset for reading data and get back 4 reals: NT, date1, date2 and vers */
  {
    ses_material_id mid = (ses_material_id)me->eosData.materialID;
    ses_table_id    tid = (ses_table_id)me->eosData.tableNum;
    ierr = eos_SesSeekToDataTable (matid, me->eosData.tableNum, &read_data, 4,
				   &(me->eosData.dataFileIndex),
				   &mid, &tid, &(me->eosData.dataSize),
				   me->eosData.userDefinedDataFile, &errMsg);
    if (errMsg) ierr = eos_SetCustomErrorMsg(th, ierr, "%s", errMsg);
    EOS_FREE(errMsg);
  }
  if (ierr) {
    ((eos_ErrorHandler *) me)->HandleError (me, th, ierr);
    return;
  }

  /* return error code if requested amount of data is not available */
  count = 0;
  me->N = (EOS_INTEGER) read_data[count++];     /* 0 */
  /* read creation date */
  me->eosData.creationDate = (EOS_INTEGER) read_data[count++];     /* 1 */
  /* read modification date */
  me->eosData.modificationDate = (EOS_INTEGER) read_data[count++]; /* 2 */
  if (me->eosData.modificationDate == 0)
    me->eosData.modificationDate = me->eosData.creationDate;
  /* read version number */
  me->eosData.latestVersion = (EOS_INTEGER) read_data[count++];    /* 3 */

  /* check the size of data */
  if (me->eosData.dataSize != 1 + 2 * me->N) {
    EOS_FREE (read_data);
    ((eos_ErrorHandler *) me)->HandleError (me, th, EOS_READ_DATA_FAILED);
    return;                     /* wrong amount of data read! */
  }

  /* deallocate read_data[], which was allocated by eos_SesSeekToDataTable() above */
  EOS_FREE (read_data);
}

/************************************************************************
 * 
 * This function loads data of RecordType3 and stores the data in the class's
 * data structures.
 * 
 * Returned Values: none
 *
 * Input Value:
 * eos_RecordType3 *me  - this pointer (pointer to the instance of type eos_RecordType3
 * 
 ************************************************************************/
void eos_LoadRecordType3 (void *ptr, EOS_INTEGER th)
{
  EOS_INTEGER i, count;
  EOS_INTEGER ierr = EOS_OK;
  EOS_REAL *read_data;
  eos_RecordType3 *me;
  EOS_CHAR *errMsg = NULL;

  me = (eos_RecordType3 *) ptr;

  /* check handle's error code; this was moved from Create upon the introduction
   * of the eos_SetDataFileName function */
  if (eos_GetStandardErrorCodeFromCustomErrorCode(gEosDataMap.errorCodes[th]) == EOS_MATERIAL_NOT_FOUND) {
    ierr = gEosDataMap.errorCodes[th];
    ((eos_ErrorHandler *) me)->HandleError (me, th, ierr);
    return;
  }

  /* Load the table */
#ifdef DEBUG
  printf ("loading recordtype 3\n");
#endif
  read_data = NULL;

  ierr = eos_SesLoadSesameFiles (me->eosData.materialID, me->eosData.tableNum, me->eosData.dataFileIndex, &read_data, me->eosData.dataSize - 1);  
  if (ierr) {
#ifdef DEBUG
    printf ("error loading table! ERROR %d\n", ierr);
#endif
    ((eos_ErrorHandler *) me)->HandleError (me, th, ierr);
    return;
  }

  count = 0;
  /* read temperature density pairs */
  for (i = 0; i < me->N; i++) {
    me->T[i] = read_data[count++];
    me->R[i] = read_data[count++];
/*		printf("%lf %lf\n", me->T[i], me->R[i]); */
  }

  /* store the number of subTables stored in memory */
  me->eosData.numSubtablesLoaded = MAX_TABLES_RECORDTYPE3;

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
				 "eos_RecordType3::eos_GetBulkData ERROR, bulk data not available for table handle, %i", th);
    return;
  }

#ifdef DEBUG
  printf
    ("DEBUG >>> eos_LoadRecordType1: avgAtomicNumber=%E, avgAtomicWgt=%E, refDensity=%E\n",
     me->avgAtomicNumber, me->avgAtomicWgt, me->refDensity);
#endif

  EOS_FREE (read_data);
  me->eosData.isLoaded = 1;
}

/************************************************************************
 * 
 * RecordType3 class destructor
 * 
 * Returned Values: none
 *
 * Input Value:
 * eos_RecordType3 *me  - this pointer (pointer to the instance of type eos_RecordType3
 * 
 ************************************************************************/
void eos_DestroyRecordType3 (void* ptr)
{
  eos_RecordType3 *me = (eos_RecordType3*) ptr;

  if (me->T)
    EOS_FREE (me->T);
  me->T = NULL;
  if (me->R)
    EOS_FREE (me->R);
  me->R = NULL;
  if (!me->eosData.destructing) /* to prevent circular calls to destructor */
    eos_DestroyEosData (&(me->eosData));
}

/************************************************************************
 * 
 * This function allocates enough memory in class eos_RecordType3
 * to hold R and T raaays of specified size.
 * 
 * Returned Values: none
 *
 * Input Value:
 * eos_RecordType4 *me  - this pointer (pointer to the instance of type eos_RecordType3
 * EOS_INTEGER      size of T and R arrays.
 * 
 ************************************************************************/
void eos_SetSizeRecordType3 (eos_RecordType3 *me, EOS_INTEGER N)
{
  if (me->T)
    EOS_FREE (me->T);
  if (me->R)
    EOS_FREE (me->R);
  me->N = N;
  me->T = (EOS_REAL *) malloc (N * sizeof (EOS_REAL));
  me->R = (EOS_REAL *) malloc (N * sizeof (EOS_REAL));

  if (me->T && me->R)
    me->eosData.isAllocated = 1;
}

/***********************************************************************/
/*!
 * \brief This function returns the dimensions of the specified table.
 *
 * \param[out]    N  - EOS_INTEGER : size of R array
 * \param[in]     *me - eos_RecordType3 : data object pointer;
 *
 * \return none
 *
 ***********************************************************************/
void eos_GetSizeRecordType3 (eos_RecordType3 *me, EOS_INTEGER *N)
{
  *N = me->N;
}

/***********************************************************************/
/*!
 * \brief This function returns pointers to data of class eos_RecordType3
 *
 * \param[in]     *me         - eos_RecordType3 : data object pointer;
 * \param[out]    **R         - EOS_REAL : holds R-pointer
 * \param[out]    **T         - EOS_REAL : holds T-pointer
 *
 * \return none
 *
 ***********************************************************************/
void _eos_GetDataRecordType3 (eos_RecordType3 *me, EOS_REAL **R, EOS_REAL **T)
{
  *R = me->R;
  *T = me->T;
}

/************************************************************************
 * 
 * This function prints the data of class eos_RecordType3
 * 
 * Returned Values: EOS_INTEGER *err - output error code
 *
 * Input Value:
 * void *ptr       - this pointer (pointer to the instance of type eos_RecordType3
 * EOS_CHAR * fname;
 * EOS_INTEGER append   - whether or not to append to file
 * EOS_INTEGER th       - table handle
 * 
 ************************************************************************/
void eos_PrintRecordType3 (void *ptr, EOS_INTEGER th, EOS_CHAR *fname,
                           EOS_INTEGER append, EOS_INTEGER *err)
{
  EOS_CHAR *sesame_fname, buf1[_MIN_FIELD_WIDTH+1], buf2[_MIN_FIELD_WIDTH+1];
  FILE *tableFile;
  eos_RecordType3 *me;
  int i, dataType;

  me = (eos_RecordType3 *) ptr;
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
  for (i = 0; i < me->N; i++) {
    _eos_dbl2String (me->T[i], _MIN_FIELD_WIDTH, buf1);
    _eos_dbl2String (me->R[i], _MIN_FIELD_WIDTH, buf2);
    fprintf (tableFile, "%*s %*s\n", _MIN_FIELD_WIDTH + 1, buf1,
             _MIN_FIELD_WIDTH + 1, buf2);
  }

  fclose (tableFile);
}

/************************************************************************
 * 
 * This packs the data of class eos_RecordType3 into provided char array
 * 
 * Returned Values: EOS_INTEGER *err - output error code
 *
 * Input Value:
 * EOS_INTEGER th  - table handle
 * void *ptr       - this pointer (pointer to the instance of type eos_RecordType3
 * EOS_CHAR *packedTable - allocated by user char array large enough to store packed data
 * 
 ************************************************************************/
void eos_GetPackedTableRecordType3 (void *ptr, EOS_INTEGER dataType,
                                    EOS_CHAR *packedTable, EOS_INTEGER *err)
{
  eos_RecordType3 *me;
  EOS_INTEGER byteCount = 0;

  me = (eos_RecordType3 *) ptr;
  *err = EOS_OK;

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

  memcpy (packedTable + byteCount, &(me->N), sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (packedTable + byteCount, me->T, me->N * sizeof (EOS_REAL));
  byteCount += me->N * sizeof (EOS_REAL);
  memcpy (packedTable + byteCount, me->R, me->N * sizeof (EOS_REAL));
  byteCount += me->N * sizeof (EOS_REAL);
  memcpy (packedTable + byteCount, &(me->eosData.numSubtablesLoaded), sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (packedTable + byteCount, &(me->eosData.isLoaded), sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);

  memcpy (packedTable + byteCount, &(me->eosData.dataFileIndex), sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (packedTable + byteCount, &(me->eosData.dataFileOffset), sizeof (long));
  byteCount += sizeof (long);
  memcpy (packedTable + byteCount, &(me->eosData.dataSize), sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
}

/************************************************************************
 * 
 * This sets the data of class eos_RecordType3 from the packed char array provided by caller
 * 
 * Returned Values: EOS_INTEGER *err - output error code
 *
 * Input Value:
 * void *ptr       - this pointer (pointer to the instance of type eos_RecordType3
 * EOS_CHAR *packedTable - allocated by user char array large enough to store packed data
 * EOS_INTEGER packedTableSize - size in chars of packed data array
 * EOS_INTEGER th  - table handle
 * 
 ************************************************************************/
void eos_SetPackedTableRecordType3 (void *ptr, EOS_INTEGER th,
                                    EOS_CHAR *packedTable, EOS_INTEGER *err)
{
  eos_RecordType3 *me;
  EOS_INTEGER byteCount = 0, n, tmpINT;

  me = (eos_RecordType3 *) ptr;
  *err = EOS_OK;

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

  memcpy (&n, packedTable + byteCount, sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);

  if (n > me->N) eos_SetSizeRecordType3 (me, n);
  /* me->N = n; */
  /* memcpy (&(me->N), packedTable + byteCount, sizeof (EOS_INTEGER)); */
  /* byteCount += sizeof (EOS_INTEGER); */
  memcpy (me->T, packedTable + byteCount, me->N * sizeof (EOS_REAL));
  byteCount += me->N * sizeof (EOS_REAL);
  memcpy (me->R, packedTable + byteCount, me->N * sizeof (EOS_REAL));
  byteCount += me->N * sizeof (EOS_REAL);
  memcpy (&(me->eosData.numSubtablesLoaded), packedTable + byteCount, sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (&(tmpINT), packedTable + byteCount, sizeof (EOS_INTEGER));
  me->eosData.isLoaded = (tmpINT != 0) ? 1 : 0;
  byteCount += sizeof (EOS_INTEGER);

  memcpy (&(me->eosData.dataFileIndex), packedTable + byteCount, sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (&(me->eosData.dataFileOffset), packedTable + byteCount, sizeof (long));
  byteCount += sizeof (long);
  memcpy (&(me->eosData.dataSize), packedTable + byteCount, sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
}

/************************************************************************
 * 
 * This returns the size of packed char array needed to store the data of class eos_RecordType3
 * 
 * Returned Values: EOS_INTEGER *err - output error code
 *
 * Input Value:
 * void *ptr       - this pointer (pointer to the instance of type eos_RecordType3
 * EOS_INTEGER *packedTableSize - size in chars of packed data array
 * EOS_INTEGER th - table handle
 * 
 ************************************************************************/
void eos_GetPackedTableSizeRecordType3 (void *ptr, EOS_INTEGER th,
                                        EOS_INTEGER *packedTableSize,
                                        EOS_INTEGER *err)
{
  eos_RecordType3 *me;
  EOS_INTEGER byteCount = 0;

  me = (eos_RecordType3 *) ptr;
  *err = EOS_OK;

  byteCount += sizeof (EOS_REAL);         /* me->avgAtomicNumber */    
  byteCount += sizeof (EOS_REAL);         /* me->avgAtomicWgt */       
  byteCount += sizeof (EOS_REAL);         /* me->refDensity */         
  byteCount += sizeof (EOS_REAL);         /* me->solidBulkModulus */   
  byteCount += sizeof (EOS_REAL);         /* me->exchangeCoefficient */

  byteCount += sizeof (EOS_INTEGER);      /* me->N */
  byteCount += me->N * sizeof (EOS_REAL); /* me->T[] */
  byteCount += me->N * sizeof (EOS_REAL); /* me->R[] */
  byteCount += sizeof (EOS_INTEGER);      /* me->eosData.numSubtablesLoaded */
  byteCount += sizeof (EOS_INTEGER);      /* me->eosData.isLoaded */
  byteCount += sizeof (EOS_INTEGER);      /* me->eosData.dataFileIndex */
  byteCount += sizeof (long);             /* me->eosData.dataFileOffset */
  byteCount += sizeof (EOS_INTEGER);      /* me->eosData.dataSize */
  *packedTableSize = byteCount;
}

/************************************************************************
 * 
 * Makes data of class eos_RecordType3 monotonic
 * 
 * Returned Values: EOS_INTEGER *err - output error code
 *
 * Input Value:
 * void *ptr       - this pointer (pointer to the instance of type eos_RecordType3
 * EOS_INTEGER th  - table handle for error handling
 * EOS_INTEGER dataType - data type of subtable to be made monotonic
 * EOS_BOOLEAN inX, inY - to make monotonic in x, in y, or both
 * EOS_INTEGER *err   - error flag
 * 
 ************************************************************************/
void eos_MakeMonotonicRecordType3 (void *ptr, EOS_INTEGER th,
                                   EOS_INTEGER dataType, EOS_BOOLEAN inX,
                                   EOS_BOOLEAN inY, EOS_INTEGER *err)
{
  eos_RecordType3 *me;

  me = (eos_RecordType3 *) ptr;
  *err = EOS_OK;

  if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) != EOS_OK) {
    ((eos_ErrorHandler *) me)->HandleError (me, th, *err);
    return;
  }
}


/************************************************************************
 * 
 * checks if the data of class eos_RecordType3 is monotonic
 * 
 * Returned Values: EOS_INTEGER *err - output error code
 *
 * Input Value:
 * void *ptr       - this pointer (pointer to the instance of type eos_RecordType3
 * EOS_BOOLEAN *isMonotonic - result
 * 
 ************************************************************************/
void eos_IsMonotonicRecordType3 (void *ptr, EOS_INTEGER dataType,
                                 EOS_BOOLEAN *isMonotonic, EOS_BOOLEAN inX,
                                 EOS_BOOLEAN inY, EOS_INTEGER *err)
{
  *err = EOS_OK;
}


/************************************************************************
 * 
 * Makes data of class eos_RecordType3 smooth
 * 
 * Returned Values: EOS_INTEGER *err - output error code
 *
 * Input Value:
 * void *ptr       - this pointer (pointer to the instance of type eos_RecordType3
 * EOS_INTEGER *err   - error flag
 * 
 ************************************************************************/
void eos_MakeSmoothRecordType3 (void *ptr, EOS_INTEGER th,
                                EOS_INTEGER dataType, EOS_BOOLEAN ptSmooth,
                                EOS_INTEGER *err)
{
  eos_RecordType3 *me;

  me = (eos_RecordType3 *) ptr;
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
 * void *ptr                - this pointer (pointer to the instanse of type eos_RecordType3
 * EOS_INTEGER numInfoItems - # of requested items
 * EOS_INTEGER *infoItems   - array of requested items
 * EOS_REAL *infoVals       - return item values
 * EOS_INTEGER th           - table handle 
 * EOS_INTEGER *err         - error flag
 * 
 ************************************************************************/
void eos_GetTableInfoRecordType3 (void *ptr, EOS_INTEGER th,
                                  EOS_INTEGER numInfoItems,
                                  EOS_INTEGER *infoItems, EOS_REAL *infoVals,
                                  EOS_INTEGER *err)
{
  EOS_INTEGER i, j;
  EOS_REAL xconv = 1.0, yconv = 1.0;
  eos_RecordType3 *me;
  me = (eos_RecordType3 *) ptr;
  *err = EOS_OK;

  for (i = 0; i < numInfoItems; i++) {
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
      infoVals[i] =
        EOS_TYPE_TO_LOG_AXES (eos_GetDataTypeFromTableHandle (th, err));
      break;

    case EOS_Material_ID:
      infoVals[i] = me->eosData.materialID;
      break;

    case EOS_Table_Type:
      infoVals[i] = eos_GetDataTypeFromTableHandle (th, err);
      break;

    case EOS_Rmin:
      infoVals[i] = me->R[0];
      break;

    case EOS_Rmax:
      infoVals[i] = me->R[me->N - 1];
      break;

    case EOS_Tmin:
      infoVals[i] = me->T[0];
      break;

    case EOS_Tmax:
      infoVals[i] = me->T[me->N - 1];
      break;

    case EOS_NR:
    case EOS_NT:
      infoVals[i] = (EOS_REAL) me->N;
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

    case EOS_R_Array:
      if (numInfoItems < me->N) {
	*err = EOS_FAILED;
	*err = eos_SetCustomErrorMsg(th, *err,
				     "Operation failed. Insufficient memory allocation assumed because numInfoItems<N.");
	return;
      }
      /* process x-conversion option */
      xconv = eos_getRealOptionFromTableHandle (th, EOS_X_CONVERT, err);
      if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) != EOS_OK)
        return;
      for (j = 0; j < MIN (numInfoItems, me->N); j++)
	infoVals[j] = me->R[j] * xconv;
      return; /* all data is fetched for this option ... time to leave */
      break;

    case EOS_T_Array:
      if (numInfoItems < me->N) {
	*err = EOS_FAILED;
	*err = eos_SetCustomErrorMsg(th, *err,
				     "Operation failed. Insufficient memory allocation assumed because numInfoItems<N.");
	return;
      }
      /* process y-conversion option */
      yconv = eos_getRealOptionFromTableHandle (th, EOS_Y_CONVERT, err);
      if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) != EOS_OK)
        return;
      for (j = 0; j < MIN (numInfoItems, me->N); j++)
	infoVals[j] = me->T[j] * yconv;
      return; /* all data is fetched for this option ... time to leave */
      break;

    default:
      *err = EOS_INVALID_INFO_FLAG;
      break;
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
 * void *ptr           - this pointer (pointer to the instance of type eos_RecordType3
 * EOS_CHAR *infoItem  - flag specifying what meta data item to fetch
 * 
 ************************************************************************/
void eos_GetTableMetaDataRecordType3 (void *ptr, EOS_INTEGER infoItem,
				      EOS_CHAR *infoStr, EOS_INTEGER *err)
{
  //eos_RecordType3 *me;

  //me = (eos_RecordType3 *) ptr;
  *err = EOS_INVALID_DATA_TYPE; /* 101 table not found */
}

 /*************************************************************************
 *
 * Function eos_GetLoadedBulkDataRecordType3
 * Description:
 * 
 * This function lgets data from 201 table. These values are needed
 * for entropy and free energy data calculations.
 * 
 * Returned Values:
 * EOS_INTEGER errorCode - output error code
 * EOS_REAL    *zbar      - mean atomic number
 * EOS_REAL    *abar      - mean atomic mass
 * EOS_REAL    *dens0     - normal solid density
 *
 * Input Value:
 * eos_RecordType3 *me
 *************************************************************************/
void eos_GetLoadedBulkDataRecordType3 (void *ptr, EOS_REAL *zbar,
                                       EOS_REAL *abar, EOS_REAL *dens0,
                                       EOS_INTEGER *errorCode)
{
  eos_RecordType3 *me;
  me = (eos_RecordType3 *) ptr;
  *errorCode = EOS_OK;

  *zbar = me->avgAtomicNumber;
  *abar = me->avgAtomicWgt;
  *dens0 = me->refDensity;
}

 /*************************************************************************
 *
 * Function eos_SetMonotonicityRecordType3
 * Description:
 * 
 * set the flags so that appropriate data will be made monotonic upon loading
 * 
 * Returned Values:
 *
 * Input Value:
 * eos_RecordType3 *me
 * EOS_INTEGER dataType
 * EOS_INTEGER inX
 * EOS_INTEGER inY
 *************************************************************************/
void eos_SetMonotonicityRecordType3 (void *me, EOS_INTEGER dataType,
                                     EOS_INTEGER inX, EOS_INTEGER inY)
{
}

/*************************************************************************
 *
 * Function eos_GetMonotonicityRecordType3
 * Description:
 * 
 * set the flags so that appropriate data will be made monotonic upon loading
 * 
 * Returned Values:
 *
 * Input Value:
 * eos_RecordType3 *me
 * EOS_INTEGER dataType
 * EOS_INTEGER *inX
 * EOS_INTEGER *inY
 *************************************************************************/
void eos_GetMonotonicityRecordType3 (void *me, EOS_INTEGER dataType,
                                     EOS_INTEGER *inX, EOS_INTEGER *inY)
{
  *inX = *inY = 0;
}

/*************************************************************************
 *
 * Function eos_GetSmoothingRecordType3
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
void eos_GetSmoothingRecordType3 (void *me, EOS_INTEGER dataType,
                                  EOS_INTEGER *isSmooth,
                                  EOS_INTEGER *isPtSmooth)
{
  *isSmooth = 0;
  *isPtSmooth = 0;
}

/*************************************************************************
 *
 * Function eos_AreMonotonicRequirementsCompatibleRecordType3
 * Description:
 * 
 * find out if the requested type, monotonicity combo can share our table object
 * 
 * Returned Values:
 * EOS_BOOLEAN *compatible
 *
 * Input Value:
 * eos_RecordType3 *me
 * EOS_INTEGER dataType
 * EOS_INTEGER inX
 * EOS_INTEGER inY
 *************************************************************************/
void eos_AreMonotonicRequirementsCompatibleRecordType3 (void *me,
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
 * Function eos_SetSmoothingDataRecordType3
 * Description:
 * 
 * set the flags so that appropriate data will be made smooth upon loading
 * 
 * Returned Values:
 *
 * Input Value:
 * eos_RecordType3 *me
 * EOS_INTEGER dataType
 * EOS_INTEGER makeSmooth
 * EOS_INTEGER makePtSmooth
 *************************************************************************/
void eos_SetSmoothingRecordType3 (void *me, EOS_INTEGER dataType,
                                  EOS_INTEGER makeSmooth,
                                  EOS_INTEGER makePtSmooth)
{
}

/*************************************************************************
 *
 * Function eos_AreSmoothingRequirementsCompatibleRecordType3
 * Description:
 * 
 * find out if the requested type, monotonicity combo can share our table object
 * 
 * Returned Values:
 * EOS_BOOLEAN *compatible
 *
 * Input Value:
 * eos_RecordType3 *me
 * EOS_INTEGER dataType
 * EOS_INTEGER makeSmooth
 * EOS_INTEGER makePtSmooth
 *************************************************************************/
void eos_AreSmoothingRequirementsCompatibleRecordType3 (void *me,
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
