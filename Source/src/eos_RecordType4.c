/*********************************************************************
 * Class Name : eos_RecordType4
 * ---------------------------------------------------------
 * Filetype: (SOURCE)
 * 
 * Copyright -- see file named COPYRIGHTNOTICE
 * 
 *********************************************************************/

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#define _EOS_RECORDTYPE4_INTERNAL_PROTOTYPES
#include "eos_types_internal.h"
#include "eos_RecordType4.h"

#include "eos_Utils.h"

#define ALLOC_INCR 50

/************************************************************************
 * 
 * RecordType4 class constructor
 * 
 * Returned Values: none
 *
 * Input Value:
 * eos_RecordType4 *me         - this pointer (pointer to the instance of type eos_RecordType4
 * EOS_INTEGER      materialID - id of material to load.
 * EOS_INTEGER      th         - table handle
 * 
 ************************************************************************/

void eos_ConstructRecordType4 (eos_RecordType4 *me, EOS_INTEGER th,
                               EOS_INTEGER materialID)
{
  me->comment = NULL;
  me->numChars = NULL;
  me->tableNum = NULL;
  me->dataFileOffsets = NULL;

  me->numberTables = 0;
  me->numberAllocatedTables = 0;
  eos_ConstructEosData ((eos_Data *) me, th, materialID);
  me->eosData.Create = eos_CreateRecordType4;
  me->eosData.Destroy = eos_DestroyRecordType4;
  me->eosData.SetFileIndexes = eos_SetFileIndexesRecordType4;
  me->eosData.dataFileOffset = -1;
  me->eosData.dataFileIndex = -1;
  me->eosData.dataSize = 0;
  me->eosData.Load = eos_LoadRecordType4;
  me->eosData.Print = eos_PrintRecordType4;
  me->eosData.GetPackedTable = eos_GetPackedTableRecordType4;
  me->eosData.SetPackedTable = eos_SetPackedTableRecordType4;
  me->eosData.GetPackedTableSize = eos_GetPackedTableSizeRecordType4;
  me->eosData.IsMonotonic = eos_IsMonotonicRecordType4;
  me->eosData.MakeMonotonic = eos_MakeMonotonicRecordType4;
  me->eosData.MakeSmooth = eos_MakeSmoothRecordType4;
  me->eosData.GetTableInfo = eos_GetTableInfoRecordType4;
  me->eosData.GetTableCmnts = eos_GetTableCmntsRecordType4;
  me->eosData.GetTableMetaData = eos_GetTableMetaDataRecordType4;
  me->eosData.GetLoadedBulkData = eos_GetLoadedBulkDataRecordType4;
  me->eosData.SetMonotonicity = eos_SetMonotonicityRecordType4;
  me->eosData.GetMonotonicity = eos_GetMonotonicityRecordType4;
  me->eosData.GetSmoothing = eos_GetSmoothingRecordType4;
  me->eosData.AreMonotonicRequirementsCompatible =
    eos_AreMonotonicRequirementsCompatibleRecordType4;
  me->eosData.SetSmoothing = eos_SetSmoothingRecordType4;
  me->eosData.AreSmoothingRequirementsCompatible =
    eos_AreSmoothingRequirementsCompatibleRecordType4;
  me->eosData.Interpolate = NULL;
  me->eosData.CheckExtrap = NULL;
  me->eosData.InvertAtSetup = NULL; /* no inversion at setup allowed */
  me->eosData.SetExtrapolationBounds = NULL; /* no extrapolation bounds stored */
  me->eosData.varOrder = -1;
  me->eosData.tmpVarOrder = -1;
}

/************************************************************************
 * 
 * This function allocates RecordType4 class' data structures for future loading.
 * 
 * Returned Values: none
 *
 * Input Value:
 * eos_RecordType4 *me  - this pointer (pointer to the instance of type eos_RecordType4
 * 
 ************************************************************************/
void eos_CreateRecordType4 (void *ptr, EOS_INTEGER th)
{
  eos_RecordType4 *me;

  EOS_INTEGER i;

  me = (eos_RecordType4 *) ptr;

  /* set the sesame file indexes and offsets for RecordType4 */
  eos_SetFileIndexesRecordType4 (me, th);

  if (gEosDataMap.errorCodes[th]) /* return previous error to parent */
    return;

  /* return error message if no comment data were found */
  if (! me->numberTables) {
    ((eos_ErrorHandler *) me)->HandleError (me, th, EOS_NO_COMMENTS);
    return;
  }

  /* allocate enough space for each table */
  for (i = 0; i < me->numberTables; i++)
    me->comment[i] = (EOS_CHAR *) calloc (me->numChars[i]+1, sizeof(EOS_CHAR));

}

/***********************************************************************/
/*! 
 * \brief This function sets the sesame file indexes and offsets for RecordType4
 * 
 * \param[in,out] *ptr       - void : data object pointer;
 *                             internally recast to eos_RecordType4*
 *                             contents are allocated
 * \param[in]     th         - EOS_INTEGER : table handle
 * 
 * \return none
 *
 ***********************************************************************/
void eos_SetFileIndexesRecordType4 (void *ptr, EOS_INTEGER th)
{
  eos_RecordType4 *me;
  EOS_INTEGER matid, ndata, tableNum, totData = 0;
  EOS_INTEGER ierr = EOS_OK, count;
  EOS_REAL *read_data = NULL;
  long fileOffset = 0;
  EOS_CHAR *errMsg = NULL;

  me = (eos_RecordType4 *) ptr;
  matid = me->eosData.materialID;

  eos_ReallocateTablesRecordType4 (me, ALLOC_INCR);
  me->numberTables = 0;

  /* try to read 101 - 199 tables */
  for (tableNum = 101; tableNum <= 199; tableNum++) {
    /* get the file offset for reading data and 3 reals date1, date2 and vers */
    ses_material_id mid = matid;
    ses_table_id    tid = tableNum;
    ierr = eos_SesSeekToDataTable (matid, tableNum, &read_data, 3,
				   &(me->eosData.dataFileIndex), &mid, &tid,
				   &ndata, me->eosData.userDefinedDataFile, &errMsg);
    if (errMsg) ierr = eos_SetCustomErrorMsg(th, ierr, "%s", errMsg);
    EOS_FREE(errMsg);
    if (ierr) {
      /* deallocate read_data[], which was allocated by eos_SeekToDataTable() above */
      EOS_FREE (read_data);
      if (eos_GetStandardErrorCodeFromCustomErrorCode(ierr) == EOS_MATERIAL_NOT_FOUND) break;
      continue;
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

    //if (! me->eosData.userDefinedDataFile)
    //  me->eosData.dataFileIndex = fileIndex;      /* assume all tables share the same sesame file */

    totData += ndata;

    /* reallocate enough space for this table's indexes */
    eos_ReallocateTablesRecordType4 (me, ++me->numberTables);
    me->numChars[me->numberTables - 1] = sizeof (EOS_REAL) * ndata + 1;
    me->tableNum[me->numberTables - 1] = tableNum;
    me->dataFileOffsets[me->numberTables - 1] = fileOffset;

    /* deallocate read_data[], which was allocated by eos_SeekToDataTable() above */
    EOS_FREE (read_data);
  }

  /* return error message if no comment data were found */
  if (!totData) {
    ((eos_ErrorHandler *) me)->HandleError (me, th, ierr);
    return;
  }
}

/************************************************************************
 * 
 * This function loads data of RecordType4 and stores the data in the class's
 * data structures.
 * 
 * Returned Values: none
 *
 * Input Value:
 * eos_RecordType4 *me  - this pointer (pointer to the instance of type eos_RecordType4
 * 
 ************************************************************************/
void eos_LoadRecordType4 (void *ptr, EOS_INTEGER th)
{
  EOS_INTEGER tableNum, nreals;
  EOS_INTEGER ierr = EOS_OK;
  eos_RecordType4 *me;
  EOS_REAL *read_data;
  EOS_CHAR *emptyStr = "";

  me = (eos_RecordType4 *) ptr;

  /* check handle's error code; this was moved from Create upon the introduction
   * of the eos_SetDataFileName function */
  if (eos_GetStandardErrorCodeFromCustomErrorCode(gEosDataMap.errorCodes[th]) == EOS_MATERIAL_NOT_FOUND) {
    ierr = gEosDataMap.errorCodes[th];
    ((eos_ErrorHandler *) me)->HandleError (me, th, ierr);
    return;
  }

#ifdef DEBUG
  printf ("loading data for type 4...\n");
#endif
  read_data = NULL;

  /* try to read 101 - 199 tables */
  for (tableNum = 0; tableNum < me->numberTables; tableNum++) {
    nreals = (me->numChars[tableNum] - 1) / sizeof (EOS_REAL);  /* amount of reals to read */

    //ierr = eos_SesLoadSesameFiles (me->eosData.mid, me->eosData.tid, me->eosData.dataFileIndex, &read_data, nreals); 
    ierr = eos_SesLoadSesameFiles (me->eosData.materialID, me->eosData.tableNum+tableNum,
				   me->eosData.dataFileIndex, &read_data, nreals); 

    if (read_data)
      memcpy (me->comment[tableNum], read_data, sizeof (EOS_REAL) * nreals);
    else
      memcpy (me->comment[tableNum], emptyStr, sizeof (EOS_CHAR)); /* allow for missing table in SESAME file */
    /* add the null char in the end */
    me->comment[tableNum][sizeof (EOS_REAL) * nreals] = '\0';
    EOS_FREE (read_data);
  }

  /* store the number of Comment Tables stored in memory */
  me->eosData.numSubtablesLoaded = me->numberTables;

  me->eosData.isLoaded = 1;
}

/************************************************************************
 * 
 * RecordType4 class destructor
 * 
 * Returned Values: none
 *
 * Input Value:
 * eos_RecordType4 *me  - this pointer (pointer to the instance of type eos_RecordType4
 * 
 ************************************************************************/
void eos_DestroyRecordType4 (void* ptr)
{
  eos_RecordType4 *me = (eos_RecordType4*) ptr;

  EOS_INTEGER i;
  if (me->comment) {
    for (i = 0; i < me->numberTables; i++)
      EOS_FREE (me->comment[i]);
    EOS_FREE (me->comment);
    EOS_FREE (me->numChars);
    EOS_FREE (me->tableNum);
    EOS_FREE (me->dataFileOffsets);
  }

  if (!me->eosData.destructing) /* to prevent circular calls to destructor */
    eos_DestroyEosData (&(me->eosData));
}

/************************************************************************
 * 
 * This function allocates enough memory in class eos_RecordType4
 * to hold N tables.
 * 
 * Returned Values: none
 *
 * Input Value:
 * eos_RecordType4 *me  - this pointer (pointer to the instance of type eos_RecordType4
 * EOS_INTEGER      N   - number of tables to hold
 * 
 ************************************************************************/


void eos_ReallocateTablesRecordType4 (eos_RecordType4 *me, EOS_INTEGER N)
{
  if (N <= me->numberAllocatedTables)
    return;

  if (0 == me->numberAllocatedTables) {
    me->comment =
      (EOS_CHAR **) malloc ((me->numberAllocatedTables + ALLOC_INCR) *
                            sizeof (EOS_CHAR *));
    me->numChars =
      (EOS_INTEGER *) malloc ((me->numberAllocatedTables + ALLOC_INCR) *
                              sizeof (EOS_INTEGER));
    me->tableNum =
      (EOS_INTEGER *) malloc ((me->numberAllocatedTables + ALLOC_INCR) *
                              sizeof (EOS_INTEGER));
    me->dataFileOffsets =
      (EOS_INTEGER *) malloc ((me->numberAllocatedTables + ALLOC_INCR) *
                              sizeof (long));
  }
  else {
    me->comment =
      (EOS_CHAR **) realloc (me->comment,
                             (me->numberAllocatedTables +
                              ALLOC_INCR) * sizeof (EOS_CHAR *));
    me->numChars =
      (EOS_INTEGER *) realloc (me->numChars,
                               (me->numberAllocatedTables +
                                ALLOC_INCR) * sizeof (EOS_INTEGER));
    me->tableNum =
      (EOS_INTEGER *) realloc (me->tableNum,
                               (me->numberAllocatedTables +
                                ALLOC_INCR) * sizeof (EOS_INTEGER));
    me->dataFileOffsets =
      (EOS_INTEGER *) realloc (me->dataFileOffsets,
                               (me->numberAllocatedTables +
                                ALLOC_INCR) * sizeof (EOS_INTEGER));
  }
  me->numberAllocatedTables = (me->numberAllocatedTables + ALLOC_INCR);
}

/************************************************************************
 * 
 * This function prints the data of class eos_RecordType4
 * 
 * Returned Values: EOS_INTEGER *err - output error code
 *
 * Input Value:
 * void *ptr       - this pointer (pointer to the instance of type eos_RecordType4
 * EOS_CHAR * fname;
 * EOS_INTEGER append   - whether or not to append to file
 * EOS_INTEGER th       - table handle
 * 
 ************************************************************************/
void eos_PrintRecordType4 (void *ptr, EOS_INTEGER th, EOS_CHAR *fname,
                           EOS_INTEGER append, EOS_INTEGER *err)
{
  EOS_CHAR *sesame_fname;
  FILE *tableFile;
  eos_RecordType4 *me;
  int i, dataType;

  me = (eos_RecordType4 *) ptr;
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
  for (i = 0; i < me->numberTables; i++) {
    fprintf (tableFile, "table %d nchars %d comment: <%s>\n", me->tableNum[i],
             me->numChars[i], me->comment[i]);
  }

  fclose (tableFile);
}

/************************************************************************
 * 
 * This packs the data of class eos_RecordType4 into provided char array
 * 
 * Returned Values: EOS_INTEGER *err - output error code
 *
 * Input Value:
 * EOS_INTEGER th  - table handle
 * void *ptr       - this pointer (pointer to the instance of type eos_RecordType4
 * EOS_CHAR *packedTable - allocated by user char array large enough to store packed data
 * 
 ************************************************************************/
void eos_GetPackedTableRecordType4 (void *ptr, EOS_INTEGER th,
                                    EOS_CHAR *packedTable, EOS_INTEGER *err)
{
  eos_RecordType4 *me;
  EOS_INTEGER byteCount = 0, i;

  me = (eos_RecordType4 *) ptr;
  *err = EOS_OK;

  memcpy (packedTable + byteCount, &(me->numberTables), sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);

  for (i = 0; i < me->numberTables; i++) {
    memcpy (packedTable + byteCount, &(me->numChars[i]), sizeof (EOS_INTEGER));
    byteCount += sizeof (EOS_INTEGER);
    memcpy (packedTable + byteCount, me->comment[i], me->numChars[i] * sizeof (EOS_CHAR));
    byteCount += me->numChars[i] * sizeof (EOS_CHAR);
    memcpy (packedTable + byteCount, &(me->tableNum[i]), sizeof (EOS_INTEGER));
    byteCount += sizeof (EOS_INTEGER);
    memcpy (packedTable + byteCount, &(me->dataFileOffsets[i]), sizeof (EOS_INTEGER));
    byteCount += sizeof (EOS_INTEGER);
  }
  memcpy (packedTable + byteCount, &(me->eosData.numSubtablesLoaded), sizeof (EOS_INTEGER));
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
 * This sets the data of class eos_RecordType4 from the packed char array provided by caller
 * 
 * Returned Values: EOS_INTEGER *err - output error code
 *
 * Input Value:
 * void *ptr       - this pointer (pointer to the instance of type eos_RecordType4
 * EOS_CHAR *packedTable - allocated by user char array large enough to store packed data
 * EOS_INTEGER packedTableSize - size in chars of packed data array
 * EOS_INTEGER th - table handle
 * 
 ************************************************************************/
void eos_SetPackedTableRecordType4 (void *ptr, EOS_INTEGER th,
                                    EOS_CHAR *packedTable, EOS_INTEGER *err)
{
  eos_RecordType4 *me;
  EOS_INTEGER byteCount = 0, nt, i;

  me = (eos_RecordType4 *) ptr;
  *err = EOS_OK;

  memcpy (&nt, packedTable + byteCount, sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);

  if (nt > me->numberTables)
    eos_ReallocateTablesRecordType4 (me, nt);
  me->numberTables = nt;

  for (i = 0; i < me->numberTables; i++) {
    memcpy (&(me->numChars[i]), packedTable + byteCount, sizeof (EOS_INTEGER));
    byteCount += sizeof (EOS_INTEGER);

    me->comment[i] = (EOS_CHAR*) malloc((me->numChars[i]+1) * sizeof (EOS_CHAR));

    memcpy (me->comment[i], packedTable + byteCount, me->numChars[i] * sizeof (EOS_CHAR));
    byteCount += me->numChars[i] * sizeof (EOS_CHAR);
    memcpy (&(me->tableNum[i]), packedTable + byteCount, sizeof (EOS_INTEGER));
    byteCount += sizeof (EOS_INTEGER);
    memcpy (&(me->dataFileOffsets[i]), packedTable + byteCount, sizeof (EOS_INTEGER));
    byteCount += sizeof (EOS_INTEGER);
  }

  /* store the number of Comment Tables stored in memory */
  me->eosData.numSubtablesLoaded = me->numberTables;
  me->eosData.isLoaded = (me->eosData.numSubtablesLoaded > 0);

  memcpy (&(me->eosData.dataFileIndex), packedTable + byteCount, sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (&(me->eosData.dataFileOffset), packedTable + byteCount, sizeof (long));
  byteCount += sizeof (long);
  memcpy (&(me->eosData.dataSize), packedTable + byteCount, sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
}

/************************************************************************
 * 
 * This returns the size of packed char array needed to store the data of class eos_RecordType4
 * 
 * Returned Values: EOS_INTEGER *err - output error code
 *
 * Input Value:
 * void *ptr       - this pointer (pointer to the instance of type eos_RecordType4
 * EOS_INTEGER *packedTableSize - size in chars of packed data array
 * EOS_INTEGER th  - table handle
 * 
 ************************************************************************/
void eos_GetPackedTableSizeRecordType4 (void *ptr, EOS_INTEGER th,
                                        EOS_INTEGER *packedTableSize,
                                        EOS_INTEGER *err)
{
  eos_RecordType4 *me;
  EOS_INTEGER byteCount = 0, i;

  me = (eos_RecordType4 *) ptr;
  *err = EOS_OK;

  byteCount += sizeof (EOS_INTEGER);                  /* me->numberTables */
  for (i = 0; i < me->numberTables; i++) {
    byteCount += sizeof (EOS_INTEGER);                /* me->numChars[i] */
    byteCount += me->numChars[i] * sizeof (EOS_CHAR); /* me->comment[i] */
    byteCount += sizeof (EOS_INTEGER);                /* me->tableNum[i] */
    byteCount += sizeof (EOS_INTEGER);                /* me->dataFileOffsets[i] */
  }

  byteCount += sizeof (EOS_INTEGER);                  /* me->eosData.numSubtablesLoaded */
  byteCount += sizeof (EOS_INTEGER);                  /* me->eosData.dataFileIndex */
  byteCount += sizeof (long);                         /* me->eosData.dataFileOffset */
  byteCount += sizeof (EOS_INTEGER);                  /* me->eosData.dataSize */
  *packedTableSize = byteCount;
}

/************************************************************************
 * 
 * Makes data of class eos_RecordType4 monotonic
 * 
 * Returned Values: EOS_INTEGER *err - output error code
 *
 * Input Value:
 * void *ptr       - this pointer (pointer to the instance of type eos_RecordType4
 * EOS_INTEGER th  - table handle for error handling
 * EOS_INTEGER dataType - data type of subtable to be made monotonic
 * EOS_BOOLEAN inX, inY - to make monotonic in x, in y, or both
 * EOS_INTEGER *err   - error flag
 * 
 ************************************************************************/
void eos_MakeMonotonicRecordType4 (void *ptr, EOS_INTEGER th,
                                   EOS_INTEGER dataType, EOS_BOOLEAN inX,
                                   EOS_BOOLEAN inY, EOS_INTEGER *err)
{
  eos_RecordType4 *me;

  me = (eos_RecordType4 *) ptr;
  *err = EOS_OK;

  if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) != EOS_OK) {
    ((eos_ErrorHandler *) me)->HandleError (me, th, *err);
    return;
  }
}


/************************************************************************
 * 
 * checks if the data of class eos_RecordType4 is monotonic
 * 
 * Returned Values: EOS_INTEGER *err - output error code
 *
 * Input Value:
 * void *ptr       - this pointer (pointer to the instance of type eos_RecordType4
 * EOS_BOOLEAN *isMonotonic - result
 * 
 ************************************************************************/
void eos_IsMonotonicRecordType4 (void *ptr, EOS_INTEGER dataType,
                                 EOS_BOOLEAN *isMonotonic, EOS_BOOLEAN inX,
                                 EOS_BOOLEAN inY, EOS_INTEGER *err)
{
  *err = EOS_OK;
}


/************************************************************************
 * 
 * Makes data of class eos_RecordType4 smooth
 * 
 * Returned Values: EOS_INTEGER *err - output error code
 *
 * Input Value:
 * void *ptr       - this pointer (pointer to the instance of type eos_RecordType4
 * EOS_INTEGER *err   - error flag
 * 
 ************************************************************************/
void eos_MakeSmoothRecordType4 (void *ptr, EOS_INTEGER th,
                                EOS_INTEGER dataType, EOS_BOOLEAN ptSmooth,
                                EOS_INTEGER *err)
{
  eos_RecordType4 *me;

  me = (eos_RecordType4 *) ptr;
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
void eos_GetTableInfoRecordType4 (void *ptr, EOS_INTEGER th,
                                  EOS_INTEGER numInfoItems,
                                  EOS_INTEGER *infoItems, EOS_REAL *infoVals,
                                  EOS_INTEGER *err)
{
  EOS_INTEGER i, j, cmtLen = 0;
  eos_RecordType4 *me;
  me = (eos_RecordType4 *) ptr;
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
      infoVals[i] = eos_GetDataTypeFromTableHandle (th, err);;
      break;

    case EOS_Cmnt_Len:
      for (j = 0; j < me->numberTables; j++) {
        cmtLen += 5;            /* 3 digit table number, space and ':' */
        cmtLen += me->numChars[j] - 1;  /* ignore the zero char */
        cmtLen += 1;            /* char. return or zero char in the end */
      }
      infoVals[i] = cmtLen;
      break;

    default:
      *err = EOS_INVALID_INFO_FLAG;

    }
  }
}

/*************************************************************************
 *
 * Fetch the substring from the comment string that is indicated by the
 * supplied keyword.
 *
 *************************************************************************/
int _eos_get_field_value(EOS_CHAR *str, EOS_CHAR *keyword, EOS_CHAR *oStr)
{
  EOS_INTEGER i, L, rVal = 0;
  EOS_CHAR *s = NULL;
  EOS_CHAR *end = NULL;
  EOS_CHAR *str_lc = NULL;

  L = strlen(str);
  str_lc = (EOS_CHAR*) malloc(L * sizeof(EOS_CHAR));

  for(i=0; i < L; i++) str_lc[i] = tolower(str[i]);

  s  = strstr(str_lc, keyword);

  if (s) {
    s += strlen(keyword);
    i = s - str_lc;
    s = str + i;

    /* Trim leading space */
    while(isspace(*s)) s++;

    /* Find length of substring */
    i = strchr(s, '/') - s;
    end = s + i - 1;

    /* Trim trailing space */
    while(end > s && isspace(*end)) end--;

    /* Write new null terminator */
    *(end+1) = '\0';

    /* copy substring to oStr */
    strncpy ( oStr, s, MIN(strlen(s), EOS_META_DATA_STRLEN-1) );
    oStr[MIN(strlen(s), EOS_META_DATA_STRLEN-1)] = '\0';

    rVal = strlen(oStr);

  }

  EOS_FREE(str_lc);

  return(rVal);
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
 * void *ptr           - this pointer (pointer to the instance of type eos_RecordType4
 * EOS_CHAR *infoItem  - flag specifying what meta data item to fetch
 * 
 ************************************************************************/
void eos_GetTableMetaDataRecordType4 (void *ptr, EOS_INTEGER infoItem,
				      EOS_CHAR *infoStr, EOS_INTEGER *err)
{
  EOS_INTEGER j, L = -1;
  eos_RecordType4 *me;
  EOS_CHAR *cmntStr = NULL;

  me = (eos_RecordType4 *) ptr;
  *err = EOS_OK;

  for (j = 0; j < me->numberTables; j++) {
    if (me->tableNum[j] == 101) {

      /* create temporary buffer for 101 comment table */
      cmntStr = (EOS_CHAR*) malloc((strlen(me->comment[j]) + 1) * sizeof(EOS_CHAR));
      strcpy (cmntStr, me->comment[j]);

      switch (infoItem) {

      case EOS_Material_Name:
	L = _eos_get_field_value(cmntStr, "material.", infoStr);
	break;

      case EOS_Material_Source:
	L = _eos_get_field_value(cmntStr, "source.", infoStr);
	break;

      case EOS_Material_Date:
	L = _eos_get_field_value(cmntStr, "date.", infoStr);
	break;

      case EOS_Material_Ref:
	L = _eos_get_field_value(cmntStr, "refs.", infoStr);
	if (L <= 0)
	  L = _eos_get_field_value(cmntStr, "ref.", infoStr);
	break;

      case EOS_Material_Composition:
	L = _eos_get_field_value(cmntStr, "comp.", infoStr);
	break;

      case EOS_Material_Codes:
	L = _eos_get_field_value(cmntStr, "codes.", infoStr);
	break;

      case EOS_Material_Phases:
	L = _eos_get_field_value(cmntStr, "phases.", infoStr);
	break;

      }

      if ( L < 0 ) {
	*err = EOS_INVALID_INFO_FLAG; /* RTFM for possible infoItem values! */
      }

      EOS_FREE(cmntStr);

      break; /* all done; exit loop */

    }
    else {
      *err = EOS_INVALID_DATA_TYPE; /* 101 table not found */
    }
  }
}

/************************************************************************
 * 
 * returns comment items for the table.
 * 
 * Returned Values:
 * EOS_INTEGER *err   - output error code
 * EOS_CHAR *cmntStr  - allocated string to contain all comments
 *
 * Input Value:
 * void *ptr          - this pointer (pointer to the instanse of type eos_RecordType4
 * 
 ************************************************************************/
void eos_GetTableCmntsRecordType4 (void *ptr, EOS_CHAR *cmntStr,
                                   EOS_INTEGER *err)
{
  EOS_INTEGER j;
  eos_RecordType4 *me;
  me = (eos_RecordType4 *) ptr;
  *err = EOS_OK;

  strcpy (cmntStr, "");
  for (j = 0; j < me->numberTables; j++) {
    EOS_CHAR tid[10];
    sprintf (tid, "%3d: ", me->tableNum[j]);
    strcat (cmntStr, tid);
    strcat (cmntStr, me->comment[j]);
    if (j != me->numberTables - 1) strcat (cmntStr, "\n");
  }
}

 /*************************************************************************
 *
 * Function eos_GetLoadedBulkDataRecordType4
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
 * eos_RecordType4 *me
 *************************************************************************/
void eos_GetLoadedBulkDataRecordType4 (void *ptr, EOS_REAL *zbar,
                                       EOS_REAL *abar, EOS_REAL *dens0,
                                       EOS_INTEGER *errorCode)
{
  *errorCode = EOS_INVALID_DATA_TYPE;
}

 /*************************************************************************
 *
 * Function eos_SetMonotonicityRecordType4
 * Description:
 * 
 * set the flags so that appropriate data will be made monotonic upon loading
 * 
 * Returned Values:
 *
 * Input Value:
 * eos_RecordType4 *me
 * EOS_INTEGER dataType
 * EOS_INTEGER inX
 * EOS_INTEGER inY
 *************************************************************************/
void eos_SetMonotonicityRecordType4 (void *me, EOS_INTEGER dataType,
                                     EOS_INTEGER inX, EOS_INTEGER inY)
{
}

/*************************************************************************
 *
 * Function eos_GetMonotonicityRecordType4
 * Description:
 * 
 * set the flags so that appropriate data will be made monotonic upon loading
 * 
 * Returned Values:
 *
 * Input Value:
 * eos_RecordType4 *me
 * EOS_INTEGER dataType
 * EOS_INTEGER *inX
 * EOS_INTEGER *inY
 *************************************************************************/
void eos_GetMonotonicityRecordType4 (void *me, EOS_INTEGER dataType,
                                     EOS_INTEGER *inX, EOS_INTEGER *inY)
{
  *inX = *inY = 0;
}

/*************************************************************************
 *
 * Function eos_GetSmoothingRecordType4
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
void eos_GetSmoothingRecordType4 (void *me, EOS_INTEGER dataType,
                                  EOS_INTEGER *isSmooth,
                                  EOS_INTEGER *isPtSmooth)
{
  *isSmooth = 0;
  *isPtSmooth = 0;
}

/*************************************************************************
 *
 * Function eos_AreMonotonicRequirementsCompatibleRecordType4
 * Description:
 * 
 * find out if the requested type, monotonicity combo can share our table object
 * 
 * Returned Values:
 * EOS_BOOLEAN *compatible
 *
 * Input Value:
 * eos_RecordType4 *me
 * EOS_INTEGER dataType
 * EOS_INTEGER inX
 * EOS_INTEGER inY
 *************************************************************************/
void eos_AreMonotonicRequirementsCompatibleRecordType4 (void *me,
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
 * Function eos_SetSmoothingDataRecordType4
 * Description:
 * 
 * set the flags so that appropriate data will be made smooth upon loading
 * 
 * Returned Values:
 *
 * Input Value:
 * eos_RecordType4 *me
 * EOS_INTEGER dataType
 * EOS_INTEGER makeSmooth
 * EOS_INTEGER makePtSmooth
 *************************************************************************/
void eos_SetSmoothingRecordType4 (void *me, EOS_INTEGER dataType,
                                  EOS_INTEGER makeSmooth,
                                  EOS_INTEGER makePtSmooth)
{
}

/*************************************************************************
 *
 * Function eos_AreSmoothingRequirementsCompatibleRecordType4
 * Description:
 * 
 * find out if the requested type, monotonicity combo can share our table object
 * 
 * Returned Values:
 * EOS_BOOLEAN *compatible
 *
 * Input Value:
 * eos_RecordType4 *me
 * EOS_INTEGER dataType
 * EOS_INTEGER makeSmooth
 * EOS_INTEGER makePtSmooth
 *************************************************************************/
void eos_AreSmoothingRequirementsCompatibleRecordType4 (void *me,
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
