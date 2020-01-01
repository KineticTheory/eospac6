/*********************************************************************
 * Class Name : eos_RecordType2
 * ---------------------------------------------------------
 * Filetype: (SOURCE)
 * 
 * Copyright -- see file named COPYRIGHTNOTICE
 * 
 *********************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "eos_types_internal.h"

#define _EOS_RECORDTYPE2_INTERNAL_PROTOTYPES
#include "eos_RecordType2.h"

#define _EOS_RECORDTYPE2INTERP_INTERNAL_PROTOTYPES
#include "eos_RecordType2Interp.proto.h"

#include "eos_Utils.h"

#include "eos_Interpolation.h"

//#define DEBUG

static const EOS_REAL ZERO = (EOS_REAL) 0;

/************************************************************************
 * 
 * RecordType2 class constructor
 * 
 * Returned Values: none
 *
 * Input Value:
 * eos_RecordType2 *me         - this pointer (pointer to the instance of type eos_RecordType2
 * EOS_INTEGER      materialID - id of material to load.
 * EOS_INTEGER      th         - table handle.
 * 
 ************************************************************************/
void eos_ConstructRecordType2 (eos_RecordType2 *me, EOS_INTEGER th,
                               EOS_INTEGER materialID)
{
#ifdef DEBUG
  printf ("rcreating record type 2... \n");
#endif
  me->P = NULL;
  me->NT = 0;
  me->vaporArrayOffset = 0;
  me->avgAtomicNumber = (EOS_REAL) 0;
  me->avgAtomicWgt = (EOS_REAL) 0;
  me->refDensity = (EOS_REAL) 0;
  me->solidBulkModulus = (EOS_REAL) 0;
  me->exchangeCoefficient = (EOS_REAL) 0;
  me->T = NULL;
  me->RG = NULL;                /*[NT] */
  me->RL = NULL;                /*[NT] */
  me->EG = NULL;                /*[NT] */
  me->EL = NULL;                /*[NT] */
  me->AG = NULL;                /*[NT] */
  me->AL = NULL;                /*[NT] */
  me->shouldBeMonotonicX1 = 0;
  me->shouldBeMonotonicX2 = 0;
  me->shouldBeMonotonicX3 = 0;
  me->shouldBeMonotonicX4 = 0;
  me->shouldBeMonotonicX5 = 0;
  me->shouldBeMonotonicX6 = 0;
  me->shouldBeMonotonicX7 = 0;
  eos_ConstructEosData ((eos_Data *) me, th, materialID);
  me->eosData.Load = eos_LoadRecordType2;
  me->eosData.Create = eos_CreateRecordType2;
  me->eosData.Destroy = eos_DestroyRecordType2;
  me->eosData.SetFileIndexes = eos_SetFileIndexesRecordType2;
  me->eosData.Print = eos_PrintRecordType2;
  me->eosData.GetPackedTable = eos_GetPackedTableRecordType2;
  me->eosData.SetPackedTable = eos_SetPackedTableRecordType2;
  me->eosData.GetPackedTableSize = eos_GetPackedTableSizeRecordType2;
  me->eosData.IsMonotonic = eos_IsMonotonicRecordType2;
  me->eosData.MakeMonotonic = eos_MakeMonotonicRecordType2;
  me->eosData.MakeSmooth = eos_MakeSmoothRecordType2;
  me->eosData.GetTableInfo = eos_GetTableInfoRecordType2;
  me->eosData.GetTableMetaData = eos_GetTableMetaDataRecordType2;
  me->eosData.GetLoadedBulkData = eos_GetLoadedBulkDataRecordType2;
  me->eosData.SetMonotonicity = eos_SetMonotonicityRecordType2;
  me->eosData.AreMonotonicRequirementsCompatible =
    eos_AreMonotonicRequirementsCompatibleRecordType2;
  me->eosData.SetSmoothing = eos_SetSmoothingRecordType2;
  me->eosData.GetMonotonicity = eos_GetMonotonicityRecordType2;
  me->eosData.GetSmoothing = eos_GetSmoothingRecordType2;
  me->eosData.AreSmoothingRequirementsCompatible =
    eos_AreSmoothingRequirementsCompatibleRecordType2;
  me->eosData.Interpolate = eos_InterpolateRecordType2;
  me->eosData.CheckExtrap = eos_CheckExtrapRecordType2;
  me->eosData.InvertAtSetup = NULL; /* no inversion at setup allowed */
  me->eosData.SetExtrapolationBounds = NULL; /* no extrapolation bounds stored */
  me->eosData.varOrder = X_F;
  me->eosData.tmpVarOrder = -1;
  me->eosData.dataFileOffset = -1;
  me->eosData.dataFileIndex = -1;
  me->eosData.dataSize = 0;
}

/************************************************************************
 * 
 * RecordType2 class destructor
 * 
 * Returned Values: none
 *
 * Input Value:
 * eos_RecordType2 *me  - this pointer (pointer to the instance of type eos_RecordType2
 * 
 ************************************************************************/
void eos_DestroyRecordType2 (void* ptr)
{
  eos_RecordType2 *me = (eos_RecordType2*) ptr;

  if (me->P)
    EOS_FREE (me->P);
  if (me->T)
    EOS_FREE (me->T);
  if (me->RG)
    EOS_FREE (me->RG);
  if (me->RL)
    EOS_FREE (me->RL);
  if (me->EG)
    EOS_FREE (me->EG);
  if (me->EL)
    EOS_FREE (me->EL);
  if (me->AG)
    EOS_FREE (me->AG);
  if (me->AL)
    EOS_FREE (me->AL);
  if (!me->eosData.destructing) /* to prevent circular calls to destructor */
    eos_DestroyEosData (&(me->eosData));
}

/************************************************************************
 * 
 * This function createss data of RecordType2 and allocatesn the class's
 * data structures for loading.
 * 
 * Returned Values: none
 *
 * Input Value:
 * eos_RecordType2 *me  - this pointer (pointer to the instance of type eos_RecordType2
 * 
 ************************************************************************/
void eos_CreateRecordType2 (void *ptr, EOS_INTEGER th)
{

  eos_RecordType2 *me;

  me = (eos_RecordType2 *) ptr;

  gEosDataMap.errorCodes[th] = EOS_OK; /* reset previous error */
 
  /* set the sesame file indexes and offsets for RecordType2 */
  eos_SetFileIndexesRecordType2 (me, th);

  if (gEosDataMap.errorCodes[th]) /* return previous error to parent */
    return;

  /* check the size of data */
  if (me->eosData.dataSize < 1 + 2 * me->NT) {  /* at least 1 subtable! */
    ((eos_ErrorHandler *) me)->HandleError (me, th, EOS_READ_DATA_FAILED);
    return;
  }

  /* allocate enough memory */
  eos_SetSizeRecordType2 (me, me->NT);

}

/***********************************************************************/
/*! 
 * \brief This function sets the sesame file indexes and offsets for RecordType2
 * 
 * \param[in,out] *ptr       - void : data object pointer;
 *                             internally recast to eos_RecordType2*
 *                             contents are allocated
 * \param[in]     th         - EOS_INTEGER : table handle
 * 
 * \return none
 *
 ***********************************************************************/
void eos_SetFileIndexesRecordType2 (void *ptr, EOS_INTEGER th)
{
  eos_RecordType2 *me;
  EOS_INTEGER matid, count;
  EOS_INTEGER ierr = EOS_OK;
  EOS_REAL *read_data;
  EOS_CHAR *errMsg = NULL;

  me = (eos_RecordType2 *) ptr;
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

  count = 0;
  /* read number of temperatures */
  me->NT = (EOS_INTEGER) read_data[count++];    /* 0 */
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
}

/************************************************************************
 * 
 * This function loads data of RecordType2 and stores the data in the class's
 * data structures.
 * 
 * Returned Values: none
 *
 * Input Value:
 * eos_RecordType2 *me  - this pointer (pointer to the instance of type eos_RecordType2
 * 
 ************************************************************************/
void eos_LoadRecordType2 (void *ptr, EOS_INTEGER th)
{
  EOS_INTEGER i, count;
  EOS_INTEGER ierr = EOS_OK;
  EOS_REAL *read_data;
  eos_RecordType2 *me;
  EOS_CHAR *errMsg = NULL;

  me = (eos_RecordType2 *) ptr;

  /* check handle's error code; this was moved from Create upon the introduction
   * of the eos_SetDataFileName function */
  if (eos_GetStandardErrorCodeFromCustomErrorCode(gEosDataMap.errorCodes[th]) == EOS_MATERIAL_NOT_FOUND) {
    ierr = gEosDataMap.errorCodes[th];
    ((eos_ErrorHandler *) me)->HandleError (me, th, ierr);
    return;
  }

  /* Load the table */
#ifdef DEBUG
  printf ("loading data for type 2...\n");
#endif
  read_data = NULL;
  ierr = eos_SesLoadSesameFiles (me->eosData.materialID, me->eosData.tableNum, me->eosData.dataFileIndex, &read_data, me->eosData.dataSize - 2); /* DATA SIZE INCLUDES DATA ALREADY READ */
  if (ierr) {
    ((eos_ErrorHandler *) me)->HandleError (me, th, ierr);
    return;
  }

  count = 0;

  /* Vapor Pressure */
  for (i = 0; i < me->NT; i++)
    me->P[i] = (count < me->eosData.dataSize - 1) ? read_data[count++] : 0.0;   /* 1 to NT */

  /* temperature */
  for (i = 0; i < me->NT; i++)
    me->T[i] = (count < me->eosData.dataSize - 1) ? read_data[count++] : 0.0;   /* 1+NT to 1+2NT */

  /* Vapor Density on Coexistence Line */
  for (i = 0; i < me->NT; i++)
    me->RG[i] = (count < me->eosData.dataSize - 1) ? read_data[count++] : 0.0;  /* 1+2NT to 1+3NT */

  /* Density of Liquid or Solid on Coexistence Line */
  for (i = 0; i < me->NT; i++)
    me->RL[i] = (count < me->eosData.dataSize - 1) ? read_data[count++] : 0.0;  /* 1+3NT to 1+4NT */

  /* Internal Energy of Vapor on Coexistence Line */
  for (i = 0; i < me->NT; i++)
    me->EG[i] = (count < me->eosData.dataSize - 1) ? read_data[count++] : 0.0;  /* 1+4NT to 1+5NT */

  /* Internal Energy of Liquid on Coexistence Line */
  for (i = 0; i < me->NT; i++)
    me->EL[i] = (count < me->eosData.dataSize - 1) ? read_data[count++] : 0.0;  /* 1+5NT to 1+6NT */

  /* Free Energy of Vapor on Coexistence Line */
  for (i = 0; i < me->NT; i++)
    me->AG[i] = (count < me->eosData.dataSize - 1) ? read_data[count++] : 0.0;  /* 1+6NT to 1+7NT */

  /* Free Energy of Liquid on Coexistence Line */
  for (i = 0; i < me->NT; i++)
    me->AL[i] = (count < me->eosData.dataSize - 1) ? read_data[count++] : 0.0;  /* 1+7NT to 1+8NT */

  /* store the number of subTables stored in memory */
  me->eosData.numSubtablesLoaded = MAX_TABLES_RECORDTYPE2;

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
				 "eos_RecordType2::eos_GetBulkData ERROR, bulk data not available for table handle, %i", th);
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
 * This function allocates enough memory in class eos_RecordType2
 * to hold P,T,RG,RL,RG,EG, EL, AG, AL arrays of specified size.
 * 
 * Returned Values: none
 *
 * Input Value:
 * eos_RecordType2 *me  - this pointer (pointer to the instance of type eos_RecordType2
 * EOS_INTEGER      NT  - size of P,T,RG,RL,RG,EG, EL, AG, AL arrays
 * 
 ************************************************************************/

void eos_SetSizeRecordType2 (eos_RecordType2 *me, EOS_INTEGER NT)
{
  if (me->P)
    EOS_FREE (me->P);
  if (me->T)
    EOS_FREE (me->T);
  if (me->RG)
    EOS_FREE (me->RG);
  if (me->RL)
    EOS_FREE (me->RL);
  if (me->EG)
    EOS_FREE (me->EG);
  if (me->EL)
    EOS_FREE (me->EL);
  if (me->AG)
    EOS_FREE (me->AG);
  if (me->AL)
    EOS_FREE (me->AL);
  me->NT = NT;
  me->P = (EOS_REAL *) malloc (NT * sizeof (EOS_REAL));
  me->T = (EOS_REAL *) malloc (NT * sizeof (EOS_REAL));
  me->RG = (EOS_REAL *) malloc (NT * sizeof (EOS_REAL));
  me->RL = (EOS_REAL *) malloc (NT * sizeof (EOS_REAL));
  me->EG = (EOS_REAL *) malloc (NT * sizeof (EOS_REAL));
  me->EL = (EOS_REAL *) malloc (NT * sizeof (EOS_REAL));
  me->AG = (EOS_REAL *) malloc (NT * sizeof (EOS_REAL));
  me->AL = (EOS_REAL *) malloc (NT * sizeof (EOS_REAL));

  if (me->P && me->T && me->RG && me->RL && me->EG && me->EL && me->AG && me->AL)
    me->eosData.isAllocated = 1;
}

/************************************************************************
 * 
 * This function prints the data of class eos_RecordType2
 * 
 * Returned Values: EOS_INTEGER *err - output error code
 *
 * Input Value:
 * void *ptr       - this pointer (pointer to the instance of type eos_RecordType2
 * EOS_CHAR * fname;
 * EOS_INTEGER append   - whether or not to append to file
 * EOS_INTEGER th       - table handle
 * 
 ************************************************************************/
void eos_PrintRecordType2 (void *ptr, EOS_INTEGER th, EOS_CHAR *fname,
                           EOS_INTEGER append, EOS_INTEGER *err)
{
//#define STORED_MIN_FIELD_WIDTH _MIN_FIELD_WIDTH
#ifdef STORED_MIN_FIELD_WIDTH
#undef _MIN_FIELD_WIDTH
#define _MIN_FIELD_WIDTH 23
#endif
  EOS_CHAR *sesame_fname, buf1[_MIN_FIELD_WIDTH+1], buf2[_MIN_FIELD_WIDTH+1],
    buf3[_MIN_FIELD_WIDTH+1];
  FILE *tableFile;
  eos_RecordType2 *me;
  EOS_INTEGER dataType, subTableNum;
  int i;
  EOS_REAL xconv, fconv, *xtbls = NULL, *ftbls = NULL;
  EOS_REAL *_xtbls2 = NULL, *_ftbls2 = NULL, *f_refData = NULL;
  EOS_INTEGER *ftbls_invt_mask = NULL, nxtbl, nytbl;
  EOS_REAL *xtbl_new = NULL, *ftbl_new = NULL;

  me = (eos_RecordType2 *) ptr;
  *err = EOS_OK;
  dataType = eos_GetDataTypeFromTableHandle (th, err);
  if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) != EOS_OK)
    return;

  subTableNum = EOS_TYPE_TO_SUB_TAB_NUM (dataType);

  if (subTableNum > me->eosData.numSubtablesLoaded) {
    *err = EOS_DATA_TYPE_NOT_FOUND;
    return;
  }

  tableFile = (append == 0) ? fopen (fname, "w") : fopen (fname, "a");
  if (!tableFile) {
    *err = EOS_OPEN_OUTPUT_FILE_FAILED;
    return;
  }

  /* process x-conversion option */
  xconv = eos_getRealOptionFromTableHandle (th, EOS_X_CONVERT, err);
  if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) != EOS_OK)
    return;

  /* process f-conversion option */
  fconv = eos_getRealOptionFromTableHandle (th, EOS_F_CONVERT, err);
  if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) != EOS_OK)
    return;

  // set numbers of and indices to eos data table x,y,f values.

  nxtbl = me->NT;
  xtbls = me->T;

  switch (subTableNum) {
  case 1:
    ftbls = &(me->P[0]);
    break;
  case 2:
    ftbls = &(me->RG[0]);
    break;
  case 3:
    ftbls = &(me->RL[0]);
    break;
  case 4:
    ftbls = &(me->EG[0]);
    break;
  case 5:
    ftbls = &(me->EL[0]);
    break;
  case 6:
    ftbls = &(me->AG[0]);
    break;
  case 7:
    ftbls = &(me->AL[0]);
    break;
  }

  /* EOS_CATEGORY1 -- table is inverted with respect to 1st independent variable */
  /* EOS_CATEGORY3 -- table is merged with another function to change 1st independent variable */

  if (EOS_CATEGORY (dataType) != EOS_CATEGORY0) {
    if (EOS_CATEGORY (dataType) == EOS_CATEGORY3) {
      if (EOS_TYPE_TO_RECORD_TYPE (EOS_EOS_TABLE_TYPE_REF2 (dataType)) ==
          EOS_RECORD_TYPE2) {

        /* get data for reference2 data type */
        _eos_GetDataRecordType2 (me, &_xtbls2, &_ftbls2,
                                 EOS_TYPE_TO_SUB_TAB_NUM
                                 (EOS_EOS_TABLE_TYPE_REF2 (dataType)));
      }
      else {
        *err = EOS_UNDEFINED;
        *err = eos_SetCustomErrorMsg(th, *err,
				     "eos_RecordType2::eos_PrintRecordType2 ERROR, invalid data RecordType, %i",
				     EOS_TYPE_TO_RECORD_TYPE (EOS_EOS_TABLE_TYPE_REF2 (dataType)));
        return;
      }

      if (!(_xtbls2 || _ftbls2)) {
        *err = eos_SetCustomErrorMsg(th, *err,
				     "eos_RecordType2::eos_PrintRecordType2 ERROR, temporary array(s) not allocated");
        return;
      }

      f_refData = (_ftbls2) ? &(_ftbls2[0]) : NULL;
    }

    /* allocate temporary arrays */
    xtbl_new = (EOS_REAL *) malloc (nxtbl * sizeof (EOS_REAL));
    ftbl_new = (EOS_REAL *) malloc (nxtbl * sizeof (EOS_REAL));

    _eos_GetInvertedTable (th, dataType,
                           _xtbls2, NULL, f_refData, NULL,
                           &xtbls, NULL, &ftbls, NULL,
                           &nxtbl, &nytbl,
			   &xtbl_new, NULL, &ftbl_new,
			   &ftbls_invt_mask, err, -1);
    if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) != EOS_OK) {
      *err = eos_SetCustomErrorMsg(th, *err,
				   "eos_RecordType2::eos_PrintRecordType2 ERROR, _eos_GetInvertedTable failed to invert table");
      return;
    }

    /* use new pointer addresses */
    xtbls = xtbl_new;
    ftbls = ftbl_new;

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

  _eos_dbl2String (fconv, _MIN_FIELD_WIDTH, buf1);
  fprintf (tableFile, "  fconv =%*s         ", _MIN_FIELD_WIDTH + 1, buf1);
  _eos_dbl2String (xconv, _MIN_FIELD_WIDTH, buf1);
  fprintf (tableFile, "xconv =%*s\n\n", _MIN_FIELD_WIDTH + 1, buf1);
  fprintf (tableFile, "%*s%*s\n", _MIN_FIELD_WIDTH + 1, "x",
           _MIN_FIELD_WIDTH + 1, "f");

  for (i = 0; i < me->NT; i++) {
    _eos_dbl2String (xtbls[i], _MIN_FIELD_WIDTH, buf2);
    _eos_dbl2String (ftbls[i], _MIN_FIELD_WIDTH, buf3);
    fprintf (tableFile, "%*s%*s\n", _MIN_FIELD_WIDTH + 1, buf2,
             _MIN_FIELD_WIDTH + 1, buf3);
  }

  fclose (tableFile);

  if (ftbls_invt_mask)
    EOS_FREE (ftbls_invt_mask);
  EOS_FREE (xtbl_new);
  EOS_FREE (ftbl_new);

#ifdef STORED_MIN_FIELD_WIDTH
#undef _MIN_FIELD_WIDTH
#define _MIN_FIELD_WIDTH STORED_MIN_FIELD_WIDTH
#undef STORED_MIN_FIELD_WIDTH
#endif
}

/************************************************************************
 * 
 * This packs the data of class eos_RecordType2 into provided char array
 * 
 * Returned Values: EOS_INTEGER *err - output error code
 *
 * Input Value:
 * EOS_INTEGER th - table handle
 * void *ptr       - this pointer (pointer to the instance of type eos_RecordType2
 * EOS_CHAR *packedTable - allocated by user char array large enough to store packed data
 * 
 ************************************************************************/
void eos_GetPackedTableRecordType2 (void *ptr, EOS_INTEGER th,
                                    EOS_CHAR *packedTable, EOS_INTEGER *err)
{
  eos_RecordType2 *me;
  EOS_INTEGER byteCount = 0;

  me = (eos_RecordType2 *) ptr;
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

  memcpy (packedTable + byteCount, &(me->NT), sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (packedTable + byteCount, &(me->vaporArrayOffset),
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (packedTable + byteCount, me->P, me->NT * sizeof (EOS_REAL));
  byteCount += me->NT * sizeof (EOS_REAL);
  memcpy (packedTable + byteCount, me->T, me->NT * sizeof (EOS_REAL));
  byteCount += me->NT * sizeof (EOS_REAL);
  memcpy (packedTable + byteCount, me->RG, me->NT * sizeof (EOS_REAL));
  byteCount += me->NT * sizeof (EOS_REAL);
  memcpy (packedTable + byteCount, me->RL, me->NT * sizeof (EOS_REAL));
  byteCount += me->NT * sizeof (EOS_REAL);
  memcpy (packedTable + byteCount, me->EG, me->NT * sizeof (EOS_REAL));
  byteCount += me->NT * sizeof (EOS_REAL);
  memcpy (packedTable + byteCount, me->EL, me->NT * sizeof (EOS_REAL));
  byteCount += me->NT * sizeof (EOS_REAL);
  memcpy (packedTable + byteCount, me->AG, me->NT * sizeof (EOS_REAL));
  byteCount += me->NT * sizeof (EOS_REAL);
  memcpy (packedTable + byteCount, me->AL, me->NT * sizeof (EOS_REAL));
  byteCount += me->NT * sizeof (EOS_REAL);
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
  memcpy (packedTable + byteCount, &(me->shouldBeMonotonicX5),
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (packedTable + byteCount, &(me->shouldBeMonotonicX6),
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (packedTable + byteCount, &(me->shouldBeMonotonicX7),
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
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
 * This sets the data of class eos_RecordType2 from the packed char array provided by caller
 * 
 * Returned Values: EOS_INTEGER *err - output error code
 *
 * Input Value:
 * void *ptr       - this pointer (pointer to the instance of type eos_RecordType2
 * EOS_CHAR *packedTable - allocated by user char array large enough to store packed data
 * EOS_INTEGER packedTableSize - size in chars of packed data array
 * EOS_INTEGER th - table handle
 * 
 ************************************************************************/
void eos_SetPackedTableRecordType2 (void *ptr, EOS_INTEGER th,
                                    EOS_CHAR *packedTable, EOS_INTEGER *err)
{
  eos_RecordType2 *me;
  EOS_INTEGER byteCount = 0, nt, tmpINT;

  me = (eos_RecordType2 *) ptr;
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

  memcpy (&nt, packedTable + byteCount, sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);

  if (nt > me->NT)
    eos_SetSizeRecordType2 (me, nt);
  me->NT = nt;

  memcpy (&(me->vaporArrayOffset), packedTable + byteCount,
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);

  memcpy (me->P, packedTable + byteCount, me->NT * sizeof (EOS_REAL));
  byteCount += me->NT * sizeof (EOS_REAL);
  memcpy (me->T, packedTable + byteCount, me->NT * sizeof (EOS_REAL));
  byteCount += me->NT * sizeof (EOS_REAL);
  memcpy (me->RG, packedTable + byteCount, me->NT * sizeof (EOS_REAL));
  byteCount += me->NT * sizeof (EOS_REAL);
  memcpy (me->RL, packedTable + byteCount, me->NT * sizeof (EOS_REAL));
  byteCount += me->NT * sizeof (EOS_REAL);
  memcpy (me->EG, packedTable + byteCount, me->NT * sizeof (EOS_REAL));
  byteCount += me->NT * sizeof (EOS_REAL);
  memcpy (me->EL, packedTable + byteCount, me->NT * sizeof (EOS_REAL));
  byteCount += me->NT * sizeof (EOS_REAL);
  memcpy (me->AG, packedTable + byteCount, me->NT * sizeof (EOS_REAL));
  byteCount += me->NT * sizeof (EOS_REAL);
  memcpy (me->AL, packedTable + byteCount, me->NT * sizeof (EOS_REAL));
  byteCount += me->NT * sizeof (EOS_REAL);
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
  memcpy (&(me->shouldBeMonotonicX5), packedTable + byteCount,
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (&(me->shouldBeMonotonicX6), packedTable + byteCount,
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (&(me->shouldBeMonotonicX7), packedTable + byteCount,
          sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
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
 * This returns the size of packed char array needed to store the data of class eos_RecordType2
 * 
 * Returned Values: EOS_INTEGER *err - output error code
 *
 * Input Value:
 * void *ptr       - this pointer (pointer to the instance of type eos_RecordType2
 * EOS_INTEGER *packedTableSize - size in chars of packed data array
 * EOS_INTEGER th - table handle
 * 
 ************************************************************************/
void eos_GetPackedTableSizeRecordType2 (void *ptr, EOS_INTEGER th,
                                        EOS_INTEGER *packedTableSize,
                                        EOS_INTEGER *err)
{
  eos_RecordType2 *me;
  EOS_INTEGER byteCount = 0;

  me = (eos_RecordType2 *) ptr;
  *err = EOS_OK;

  byteCount += sizeof (EOS_REAL);
  byteCount += sizeof (EOS_REAL);
  byteCount += sizeof (EOS_REAL);
  byteCount += sizeof (EOS_REAL);
  byteCount += sizeof (EOS_REAL);

  byteCount += sizeof (EOS_INTEGER);
  byteCount += sizeof (EOS_INTEGER);
  byteCount += me->NT * sizeof (EOS_REAL);
  byteCount += me->NT * sizeof (EOS_REAL);
  byteCount += me->NT * sizeof (EOS_REAL);
  byteCount += me->NT * sizeof (EOS_REAL);
  byteCount += me->NT * sizeof (EOS_REAL);
  byteCount += me->NT * sizeof (EOS_REAL);
  byteCount += me->NT * sizeof (EOS_REAL);
  byteCount += me->NT * sizeof (EOS_REAL);
  byteCount += 7 * sizeof (EOS_INTEGER);        /* shouldBeMonotonicX1 - X7 flags */
  byteCount += 2 * sizeof (EOS_INTEGER);        /* me->eosData.numSubtablesLoaded and
                                                   me->eosData.isLoaded */
  byteCount += (sizeof (EOS_INTEGER) * 2 + sizeof (long));
  *packedTableSize = byteCount;
}

/************************************************************************
 * 
 * This function returns pointers to data of class _eos_RecordType2
 * 
 * Returned Values: none
 *
 * Input Value:
 * eos_RecordType2 *me  - this pointer (pointer to the instance of type eos_RecordType2
 * EOS_REAL			**T  - output: holds T-pointer
 * EOS_REAL			**F - output: holds F-pointer 
 * EOS_INTEGER      subtableNum  - number of subtable to take the data from.
 * 
 ************************************************************************/
void _eos_GetDataRecordType2 (eos_RecordType2 *me, EOS_REAL **T, EOS_REAL **F,
                              EOS_INTEGER subTableNum)
{
  *T = me->T;

  switch (subTableNum) {
  case 1:
    *F = me->P;
    break;
  case 2:
    *F = me->RG;
    break;
  case 3:
    *F = me->RL;
    break;
  case 4:
    *F = me->EG;
    break;
  case 5:
    *F = me->EL;
    break;
  case 6:
    *F = me->AG;
    break;
  case 7:
    *F = me->AL;
    break;
  default:
    break;
  }
}

/************************************************************************
 * 
 * Makes data of class eos_RecordType2 monotonic
 * 
 * Returned Values: EOS_INTEGER *err - output error code
 *
 * Input Value:
 * void *ptr       - this pointer (pointer to the instance of type eos_RecordType2
 * EOS_INTEGER th  - table handle for error handling
 * EOS_INTEGER dataType - data type of subtable to be made monotonic
 * EOS_BOOLEAN inX, inY - to make monotonic in x, in y, or both
 * EOS_INTEGER *err   - error flag
 * 
 ************************************************************************/
void eos_MakeMonotonicRecordType2 (void *ptr, EOS_INTEGER th,
                                   EOS_INTEGER dataType, EOS_BOOLEAN inX,
                                   EOS_BOOLEAN inY, EOS_INTEGER *err)
{
  eos_RecordType2 *me;

  me = (eos_RecordType2 *) ptr;
  *err = EOS_OK;

  if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) != EOS_OK) {
    ((eos_ErrorHandler *) me)->HandleError (me, th, *err);
    return;
  }
}


/************************************************************************
 * 
 * checks if the data of class eos_RecordType2 is monotonic-increasing or
 * monotonic-decreasing
 * 
 * Returned Values: EOS_INTEGER *err - output error code
 *
 * Input Value:
 * void *ptr       - this pointer (pointer to the instance of type eos_RecordType2
 * EOS_BOOLEAN *isMonotonic - result
 * EOS_INTEGER *err
 * 
 ************************************************************************/
void eos_IsMonotonicRecordType2 (void *ptr, EOS_INTEGER dataType,
                                 EOS_BOOLEAN *isMonotonic, EOS_BOOLEAN inX,
                                 EOS_BOOLEAN inY, EOS_INTEGER *err)
{
  eos_RecordType2 *me;
  EOS_INTEGER subTableNum = EOS_TYPE_TO_SUB_TAB_NUM (dataType), j, sign =
    0, oldSign;
  EOS_REAL diff, *table;

  me = (eos_RecordType2 *) ptr;
  *err = EOS_OK;

  *isMonotonic = EOS_FALSE;
  if (!inX)
    return;

  switch (subTableNum) {
  case 1:
    table = me->P;
    break;
  case 2:
    table = me->RG;
    break;
  case 3:
    table = me->RL;
    break;
  case 4:
    table = me->EG;
    break;
  case 5:
    table = me->EL;
    break;
  case 6:
    table = me->AG;
    break;
  case 7:
    table = me->AL;
    break;
  }

  /* check the data to determine monotonicity */
  *isMonotonic = EOS_TRUE;
  for (j = 0; j < me->NT - 1; j++) {
    diff = table[j + 1] - table[j];
/*     sign = SIGN(diff); */
    sign = (diff > ZERO) ? 1 : ((diff == ZERO) ? 0 : -1);
    if (j == 0)
      oldSign = sign;

    if (abs (sign - oldSign) > 0 ||
	(sign == 0 && oldSign == 0)) {
      *isMonotonic = EOS_FALSE;
      break;
    }
    oldSign = sign;
  }
}


/************************************************************************
 * 
 * Makes data of class eos_RecordType2 smooth
 * 
 * Returned Values: EOS_INTEGER *err - output error code
 *
 * Input Value:
 * void *ptr       - this pointer (pointer to the instance of type eos_RecordType2
 * EOS_INTEGER *err   - error flag
 * 
 ************************************************************************/
void eos_MakeSmoothRecordType2 (void *ptr, EOS_INTEGER th,
                                EOS_INTEGER dataType, EOS_BOOLEAN ptSmooth,
                                EOS_INTEGER *err)
{
  eos_RecordType2 *me;

  me = (eos_RecordType2 *) ptr;
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
 * void *ptr                - this pointer (pointer to the instanse of type eos_RecordType2
 * EOS_INTEGER numInfoItems - # of requested items
 * EOS_INTEGER *infoItems   - array of requested items
 * EOS_REAL *infoVals       - return item values
 * EOS_INTEGER th           - table handle
 * EOS_INTEGER *err         - error flag
 * 
 ************************************************************************/
void eos_GetTableInfoRecordType2 (void *ptr, EOS_INTEGER th,
                                  EOS_INTEGER numInfoItems,
                                  EOS_INTEGER *infoItems, EOS_REAL *infoVals,
                                  EOS_INTEGER *err)
{
  EOS_INTEGER i, j, subTable;
  EOS_REAL *F = NULL, xconv = 1.0, fconv = 1.0;
  eos_RecordType2 *me;
  me = (eos_RecordType2 *) ptr;
  *err = EOS_OK;

  if (EOS_ALLOW_CAT0_ONLY_INFO_ITEM (infoItems[0])) {   /* determine if infoItems[0] is restricted to category 0 datatypes */
    if (EOS_CATEGORY (eos_GetDataTypeFromTableHandle (th, err)) != 0) {
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

      /* process f-conversion option */
      fconv = eos_getRealOptionFromTableHandle (th, EOS_F_CONVERT, err);
      if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) != EOS_OK)
        return;
    }
  }

  switch (infoItems[0]) {
  case EOS_NT401: infoVals[0] = (EOS_REAL) me->NT; return;
  case EOS_P401:  F = me->P;  break;
  case EOS_T401:  F = me->T;  fconv = xconv; break;
  case EOS_RG401: F = me->RG; break;
  case EOS_RL401: F = me->RL; break;
  case EOS_EG401: F = me->EG; break;
  case EOS_EL401: F = me->EL; break;
  case EOS_AG401: F = me->EG; break;
  case EOS_AL401: F = me->EL; break;
  }
  if (F) {
    if (numInfoItems < me->NT) {
      *err = EOS_FAILED;
      *err = eos_SetCustomErrorMsg(th, *err,
				   "Operation failed. Insufficient memory allocation assumed because numInfoItems<NT.");
      return;
    }
    for (i = 0; i < MIN (numInfoItems, me->NT); i++)
      infoVals[i] = F[i] * fconv;
    return;
  }
  else {
    /* figure out F based upon subTable */
    subTable =
      EOS_TYPE_TO_SUB_TAB_NUM (eos_GetDataTypeFromTableHandle (th, err));
    if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) != EOS_OK)
      return;
    switch (subTable) {
    case 1: F = me->P; break;
    case 2: F = me->RG; break;
    case 3: F = me->RL; break;
    case 4: F = me->EG; break;
    case 5: F = me->EL; break;
    case 6: F = me->AG; break;
    case 7: F = me->AL; break;
    }
  }

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
      infoVals[i] = (EOS_REAL) me->eosData.materialID;
      break;

    case EOS_Table_Type:
      infoVals[i] = (EOS_REAL) eos_GetDataTypeFromTableHandle (th, err);
      break;

    case EOS_Fmin:
      infoVals[i] = F[0];
      for (j = 0; j < me->NT; j++) {
	if (F[j] < infoVals[i])
	  infoVals[i] = F[j];
      }
      break;

    case EOS_Fmax:
      infoVals[i] = F[0];
      for (j = 0; j < me->NT; j++) {
	if (F[j] > infoVals[i])
	  infoVals[i] = F[j];
      }
      break;

    case EOS_Tmin:
      infoVals[i] = me->T[0];
      break;

    case EOS_Tmax:
      infoVals[i] = me->T[me->NT - 1];
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

    case EOS_T_Array:
      if (numInfoItems < me->NT) {
	*err = EOS_FAILED;
	*err = eos_SetCustomErrorMsg(th, *err,
				     "Operation failed. Insufficient memory allocation assumed because numInfoItems<NT.");
	return;
      }
      for (j = 0; j < MIN (numInfoItems, me->NT); j++)
	infoVals[j] = me->T[j] * xconv;
      return; /* all data is fetched for this option ... time to leave */
      break;

    case EOS_F_Array:
      if (numInfoItems < me->NT) {
	*err = EOS_FAILED;
	*err = eos_SetCustomErrorMsg(th, *err,
				     "Operation failed. Insufficient memory allocation assumed because numInfoItems<NT.");
	return;
      }
      for (j = 0; j < MIN (numInfoItems, me->NT); j++)
	infoVals[j] = F[j] * fconv;
      return; /* all data is fetched for this option ... time to leave */
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
 * void *ptr           - this pointer (pointer to the instance of type eos_RecordType2
 * EOS_CHAR *infoItem  - flag specifying what meta data item to fetch
 * 
 ************************************************************************/
void eos_GetTableMetaDataRecordType2 (void *ptr, EOS_INTEGER infoItem,
				      EOS_CHAR *infoStr, EOS_INTEGER *err)
{
  //eos_RecordType2 *me;

  //me = (eos_RecordType2 *) ptr;
  *err = EOS_INVALID_DATA_TYPE; /* 101 table not found */
}

 /*************************************************************************
 *
 * Function eos_GetLoadedBulkDataRecordType2
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
 * eos_RecordType2 *me
 *************************************************************************/
void eos_GetLoadedBulkDataRecordType2 (void *ptr, EOS_REAL *zbar,
                                       EOS_REAL *abar, EOS_REAL *dens0,
                                       EOS_INTEGER *errorCode)
{
  eos_RecordType2 *me;
  me = (eos_RecordType2 *) ptr;
  *errorCode = EOS_OK;

  *zbar = me->avgAtomicNumber;
  *abar = me->avgAtomicWgt;
  *dens0 = me->refDensity;
}

 /*************************************************************************
 *
 * Function eos_SetMonotonicityRecordType2
 * Description:
 * 
 * set the flags so that appropriate data will be made monotonic upon loading
 * 
 * Returned Values:
 *
 * Input Value:
 * eos_RecordType2 *me
 * EOS_INTEGER dataType
 * EOS_INTEGER inX
 * EOS_INTEGER inY
 *************************************************************************/
void eos_SetMonotonicityRecordType2 (void *ptr, EOS_INTEGER dataType,
                                     EOS_INTEGER inX, EOS_INTEGER inY)
{
  EOS_INTEGER subTableNum = 1;
  eos_RecordType2 *me;

  me = (eos_RecordType2 *) ptr;
  subTableNum = EOS_TYPE_TO_SUB_TAB_NUM (dataType);

  switch (subTableNum) {
  case 1:
    if (inX >= 0 && !me->shouldBeMonotonicX1)
      me->shouldBeMonotonicX1 = inX;
    break;
  case 2:
    if (inX >= 0 && !me->shouldBeMonotonicX2)
      me->shouldBeMonotonicX2 = inX;
    break;
  case 3:
    if (inX >= 0 && !me->shouldBeMonotonicX3)
      me->shouldBeMonotonicX3 = inX;
    break;
  case 4:
    if (inX >= 0 && !me->shouldBeMonotonicX4)
      me->shouldBeMonotonicX4 = inX;
    break;
  case 5:
    if (inX >= 0 && !me->shouldBeMonotonicX5)
      me->shouldBeMonotonicX5 = inX;
    break;
  case 6:
    if (inX >= 0 && !me->shouldBeMonotonicX6)
      me->shouldBeMonotonicX6 = inX;
    break;
  case 7:
    if (inX >= 0 && !me->shouldBeMonotonicX7)
      me->shouldBeMonotonicX7 = inX;
    break;
  }
}

/*************************************************************************
 *
 * Function eos_GetMonotonicityRecordType2
 * Description:
 * 
 * set the flags so that appropriate data will be made monotonic upon loading
 * 
 * Returned Values:
 *
 * Input Value:
 * eos_RecordType2 *me
 * EOS_INTEGER dataType
 * EOS_INTEGER *inX
 * EOS_INTEGER *inY
 *************************************************************************/
void eos_GetMonotonicityRecordType2 (void *ptr, EOS_INTEGER dataType,
                                     EOS_INTEGER *inX, EOS_INTEGER *inY)
{
  EOS_INTEGER subTableNum = 1;
  eos_RecordType2 *me;


  me = (eos_RecordType2 *) ptr;
  subTableNum = EOS_TYPE_TO_SUB_TAB_NUM (dataType);

  switch (subTableNum) {
  case 1:
    *inX = me->shouldBeMonotonicX1;
    break;
  case 2:
    *inX = me->shouldBeMonotonicX2;
    break;
  case 3:
    *inX = me->shouldBeMonotonicX3;
    break;
  case 4:
    *inX = me->shouldBeMonotonicX4;
    break;
  case 5:
    *inX = me->shouldBeMonotonicX5;
    break;
  case 6:
    *inX = me->shouldBeMonotonicX6;
    break;
  case 7:
    *inX = me->shouldBeMonotonicX7;
    break;
  }
}

/*************************************************************************
 *
 * Function eos_GetSmoothingRecordType2
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
void eos_GetSmoothingRecordType2 (void *me, EOS_INTEGER dataType,
                                  EOS_INTEGER *isSmooth,
                                  EOS_INTEGER *isPtSmooth)
{
  *isSmooth = 0;
  *isPtSmooth = 0;
}

/*************************************************************************
 *
 * Function eos_AreMonotonicRequirementsCompatibleRecordType2
 * Description:
 * 
 * find out if the requested type, monotonicity combo can share our table object
 * 
 * Returned Values:
 * EOS_BOOLEAN *compatible
 *
 * Input Value:
 * eos_RecordType2 *me
 * EOS_INTEGER dataType
 * EOS_INTEGER inX
 * EOS_INTEGER inY
 *************************************************************************/
void eos_AreMonotonicRequirementsCompatibleRecordType2 (void *ptr,
                                                        EOS_INTEGER dataType,
                                                        EOS_INTEGER inX,
                                                        EOS_INTEGER inY,
                                                        EOS_BOOLEAN
                                                        *compatible)
{
  EOS_INTEGER subTableNum = 1, subTableNumStart, subTableNumEnd;
  eos_RecordType2 *me;

  me = (eos_RecordType2 *) ptr;
  if (dataType > 0)
    subTableNumStart = subTableNumEnd = EOS_TYPE_TO_SUB_TAB_NUM (dataType);
  else {                        /* do all 7 substables */

    subTableNumStart = 1;
    subTableNumEnd = 7;
  }
  *compatible = EOS_TRUE;

  for (subTableNum = subTableNumStart; subTableNum <= subTableNumEnd;
       subTableNum++) {
    switch (subTableNum) {
    case 1:
      if (inX >= 0 && me->shouldBeMonotonicX1 != inX)
        *compatible = EOS_FALSE;
      break;
    case 2:
      if (inX >= 0 && me->shouldBeMonotonicX2 != inX)
        *compatible = EOS_FALSE;
      break;
    case 3:
      if (inX >= 0 && me->shouldBeMonotonicX3 != inX)
        *compatible = EOS_FALSE;
      break;
    case 4:
      if (inX >= 0 && me->shouldBeMonotonicX4 != inX)
        *compatible = EOS_FALSE;
      break;
    case 5:
      if (inX >= 0 && me->shouldBeMonotonicX5 != inX)
        *compatible = EOS_FALSE;
      break;
    case 6:
      if (inX >= 0 && me->shouldBeMonotonicX6 != inX)
        *compatible = EOS_FALSE;
      break;
    case 7:
      if (inX >= 0 && me->shouldBeMonotonicX7 != inX)
        *compatible = EOS_FALSE;
      break;
    }
    if (*compatible == EOS_FALSE)
      break;
  }
}

 /*************************************************************************
 *
 * Function eos_SetSmoothingDataRecordType2
 * Description:
 * 
 * set the flags so that appropriate data will be made smooth upon loading
 * 
 * Returned Values:
 *
 * Input Value:
 * eos_RecordType2 *me
 * EOS_INTEGER dataType
 * EOS_INTEGER makeSmooth
 * EOS_INTEGER makePtSmooth
 *************************************************************************/
void eos_SetSmoothingRecordType2 (void *me, EOS_INTEGER dataType,
                                  EOS_INTEGER makeSmooth,
                                  EOS_INTEGER makePtSmooth)
{
}

/*************************************************************************
 *
 * Function eos_AreSmoothingRequirementsCompatibleRecordType2
 * Description:
 * 
 * find out if the requested type, monotonicity combo can share our table object
 * 
 * Returned Values:
 * EOS_BOOLEAN *compatible
 *
 * Input Value:
 * eos_RecordType2 *me
 * EOS_INTEGER dataType
 * EOS_INTEGER makeSmooth
 * EOS_INTEGER makePtSmooth
 *************************************************************************/
void eos_AreSmoothingRequirementsCompatibleRecordType2 (void *me,
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
