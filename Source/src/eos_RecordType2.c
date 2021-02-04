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

/************************************************************************/
/*!
 * \brief RecordType2 class constructor
 *
 * \param[in,out] *me        - eos_RecordType2 : data object pointer;
 *                                               contents of object are initialized
 * \param[in]     materialID - EOS_INTEGER     : id of material to load
 * \param[in]     th         - EOS_INTEGER     : table handle
 *
 * \return none
 *
 ************************************************************************/
void eos_ConstructRecordType2 (eos_RecordType2 *me, EOS_INTEGER th,
                               EOS_INTEGER materialID)
{
  int i;

  /* SESAME table 201 data */
  me->avgAtomicNumber = (EOS_REAL) 0;
  me->avgAtomicWgt = (EOS_REAL) 0;
  me->refDensity = (EOS_REAL) 0;
  me->solidBulkModulus = (EOS_REAL) 0;
  me->exchangeCoefficient = (EOS_REAL) 0;

  /* SESAME table data */
  me->NT = 0;
  me->T = NULL;
  me->P = NULL;
  me->RG = NULL;                /*[NT] */
  me->RL = NULL;                /*[NT] */
  me->EG = NULL;                /*[NT] */
  me->EL = NULL;                /*[NT] */
  me->AG = NULL;                /*[NT] */
  me->AL = NULL;                /*[NT] */

  /* Miscellaneous metadata */
  me->vaporArrayOffset = 0;

  me->eosData.varOrder = X_F;
  me->eosData.tmpVarOrder = -1;
  me->eosData.dataFileOffset = -1;
  me->eosData.dataFileIndex = -1;
  me->eosData.dataSize = 0;

  for(i=0; i<MAX_TABLES_RECORDTYPE2; i++) {
    me->shouldBeMonotonicX[i] = 0;
  }

  /* Create eos_DataMap */
  eos_ConstructEosData ((eos_Data *) me, th, materialID);

  /* Define class-specific virtual functions */
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
  me->eosData.AreMonotonicRequirementsCompatible = eos_AreMonotonicRequirementsCompatibleRecordType2;
  me->eosData.SetSmoothing = eos_SetSmoothingRecordType2;
  me->eosData.GetMonotonicity = eos_GetMonotonicityRecordType2;
  me->eosData.GetSmoothing = eos_GetSmoothingRecordType2;
  me->eosData.AreSmoothingRequirementsCompatible = eos_AreSmoothingRequirementsCompatibleRecordType2;
  me->eosData.Interpolate = eos_InterpolateRecordType2;
  me->eosData.CheckExtrap = eos_CheckExtrapRecordType2;
  me->eosData.InvertAtSetup = NULL; /* no inversion at setup allowed */
  me->eosData.SetExtrapolationBounds = NULL; /* no extrapolation bounds stored */
  me->eosData.eos_IsRequiredDataLoaded = eos_isRequiredDataLoadedRecordType2;
  me->eosData.AreGhostDataRequired = NULL; /* no ghost node data required*/
  me->eosData.AddGhostData = NULL; /* no ghost node data required*/
  me->eosData.GenerateHashTables = eos_GenerateHashTablesRecordType2;
}

/***********************************************************************/
/*!
 * \brief RecordType2 class destructor
 *
 * \param[in,out] *me        - eos_RecordType2 : data object pointer;
 *                             contents of *me are destroyed
 *
 * \return none
 *
 ***********************************************************************/
void eos_DestroyRecordType2 (void* ptr)
{
  eos_RecordType2 *me = (eos_RecordType2*) ptr;

#ifdef DO_OFFLOAD
  {
    EOS_INTEGER i;
    int t_ = omp_get_default_device();
    for(i=0; i<MAX_TABLES_RECORDTYPE2; i++) {
      omp_target_free(me->gpu_xtbls[i], t_);
      omp_target_free(me->gpu_ftbls[i], t_);
      me->gpu_xtbls[i]     = NULL;
      me->gpu_ftbls[i]     = NULL;
    }
  }
#endif /* DO_OFFLOAD */

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
  
  /* Free hashtables */
  if (me->P_ht) eos_HashTable1D_free(me->P_ht);
  if (me->T_ht) eos_HashTable1D_free(me->T_ht);
  if (me->RG_ht) eos_HashTable1D_free(me->RG_ht);
  if (me->RL_ht) eos_HashTable1D_free(me->RL_ht);
  if (me->EG_ht) eos_HashTable1D_free(me->EG_ht);
  if (me->EL_ht) eos_HashTable1D_free(me->EL_ht);
  if (me->AG_ht) eos_HashTable1D_free(me->AG_ht);
  if (me->AL_ht) eos_HashTable1D_free(me->AL_ht);

  if (!me->eosData.destructing) /* to prevent circular calls to destructor */
    eos_DestroyEosData (&(me->eosData));
}

/***********************************************************************/
/*!
 * \brief This function allocates data of RecordType2 for loading
 *
 * \param[in,out] *ptr       - void : data object pointer;
 *                             internally recast to eos_RecordType2*
 *                             contents are allocated
 * \param[in]     th         - EOS_INTEGER : table handle
 *
 * \return none
 *
 ***********************************************************************/
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

/***********************************************************************/
/*!
 * \brief This function loads data of RecordType2 and stores the data in the class's
 *  data structures.
 *
 * \param[in,out] *ptr       - void : data object pointer;
 *                             internally recast to eos_RecordType2*
 *                             contents are populated with data
 * \param[in]     th         - table handle
 *
 * \return none
 *
 ***********************************************************************/
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

/***********************************************************************/
/*!
 * \brief This function allocates enough memory in class eos_RecordType2
 * to hold P,T,RG,RL,RG,EG, EL, AG, AL arrays of specified size.
 *
 * \param[in,out] *me - eos_RecordType2 : data object pointer;
 *                      contents of *me are allocated herein
 * \param[in]     NT  - EOS_INTEGER : size of P,T,RG,RL,RG,EG, EL, AG, AL arrays
 *
 * \return none
 *
 ***********************************************************************/
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

/***********************************************************************/
/*!
 * \brief This function prints the data of class eos_RecordType2
 *
 * \param[out]    *err - EOS_INTEGER : error code
 * \param[in]     *ptr   - void : data object pointer;
 *                         internally recast to eos_RecordType2*
 * \param[in]     *fname - EOS_CHAR : file name
 * \param[in]     append - EOS_INTEGER : whether or not to append to file
 * \param[in]     th     - EOS_INTEGER : table Handle
 *
 * \return none
 *
 ***********************************************************************/
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

  if (me->eosData.eos_IsRequiredDataLoaded && ! me->eosData.eos_IsRequiredDataLoaded(me, dataType)) {
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
        _eos_GetDataRecordType2 (me, &_xtbls2, &_ftbls2, NULL,
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
                           &nxtbl, &nytbl, 0,
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

/***********************************************************************/
/*!
 * \brief This packs the data of class eos_RecordType2 into provided char array
 *
 * \param[out]    *err         - EOS_INTEGER : error code
 * \param[out]    *packedTable - EOS_CHAR    : allocated by user char array
 *                                             large enough to store packed data
 * \param[in]     *ptr         - void        : data object pointer;
 *                                             internally recast to eos_RecordType2*
 * \param[in]     th           - EOS_INTEGER : table handle
 *
 * \return none
 *
 ***********************************************************************/
void eos_GetPackedTableRecordType2 (void *ptr, EOS_INTEGER th,
                                    EOS_CHAR *packedTable, EOS_INTEGER *err)
{
  eos_RecordType2 *me;
  EOS_INTEGER i, byteCount = 0;

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
  for(i=0; i<MAX_TABLES_RECORDTYPE2; i++) {
    memcpy (packedTable + byteCount, &(me->shouldBeMonotonicX[i]), sizeof (EOS_INTEGER));
    byteCount += sizeof (EOS_INTEGER);
  }
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

  /* Pack Hashtables */
  if (me->eosData.isLoaded) {
    eos_HashTable1D* htables[8] = {
      me->T_ht, me->P_ht, me->RG_ht, me->RL_ht, me->EG_ht, me->EL_ht, me->AG_ht, me->AL_ht};
    for (i = 0; i < 8; i++) {
      EOS_CHAR exists = (htables[i] != NULL);
      memcpy(packedTable + byteCount, &exists, sizeof(EOS_CHAR));
      byteCount += sizeof(EOS_CHAR);
      if (exists) {
        byteCount += eos_HashTable1D_pack(htables[i], packedTable + byteCount);
      }
    }
  }
}

/***********************************************************************/
/*!
 * \brief This sets the data of class eos_RecordType2 from the packed char array provided by caller
 *
 * \param[out]   *err            - EOS_INTEGER : error code
 * \param[in]    *ptr            - void : data object pointer;
 *                                        internally recast to eos_RecordType2*
 * \param[in]    th              - EOS_INTEGER : table handle
 * \param[in]    packedTableSize - EOS_INTEGER : size in chars of packed data array
 * \param[in]    *packedTable    - EOS_CHAR : allocated by user char array large enough to store packed data
 *
 * \return none
 *
 ***********************************************************************/
void eos_SetPackedTableRecordType2 (void *ptr, EOS_INTEGER th,
                                    EOS_CHAR *packedTable, EOS_INTEGER *err)
{
  eos_RecordType2 *me;
  EOS_INTEGER i, byteCount = 0, nt, tmpINT;

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
  for(i=0; i<MAX_TABLES_RECORDTYPE2; i++) {
    memcpy (&(me->shouldBeMonotonicX[i]), packedTable + byteCount, sizeof (EOS_INTEGER));
    byteCount += sizeof (EOS_INTEGER);
  }
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

  /* Unpack Hashtables */
  if (me->eosData.isLoaded) {
    eos_HashTable1D** htables[8] = {
      &me->T_ht, &me->P_ht, &me->RG_ht, &me->RL_ht, &me->EG_ht, &me->EL_ht, &me->AG_ht, &me->AL_ht};
    for (i = 0; i < 8; i++) {
      EOS_CHAR exists;
      memcpy(&exists, packedTable + byteCount, sizeof(EOS_CHAR));
      byteCount += sizeof(EOS_CHAR);
      if (exists) {
        *htables[i] = (eos_HashTable1D*)malloc(sizeof(eos_HashTable1D));
        byteCount += eos_HashTable1D_unpack(*htables[i], packedTable + byteCount);
      }
    }
  }
}

/***********************************************************************/
/*!
 * \brief This returns the size of packed char array needed to store the data of class eos_RecordType2
 *
 * \param[out]   *err             - EOS_INTEGER : error code
 * \param[in]    *ptr             - void : data object pointer;
 *                                         internally recast to eos_RecordType2*
 * \param[in]    *packedTableSize - EOS_INTEGER : size in chars of packed data array
 * \param[in]    th               - EOS_INTEGER : table handle
 *
 * \return none
 *
 ***********************************************************************/
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
  byteCount += MAX_TABLES_RECORDTYPE2 * sizeof (EOS_INTEGER); /* shouldBeMonotonicX[i] flags */
  byteCount += 2 * sizeof (EOS_INTEGER);                      /* me->eosData.numSubtablesLoaded and
                                                                 me->eosData.isLoaded */
  byteCount += (sizeof (EOS_INTEGER) * 2 + sizeof (long));

  /* Hashtable bytes */
  if (me->eosData.isLoaded) {
    int i;
    eos_HashTable1D* htables[8] = {
      me->T_ht, me->P_ht, me->RG_ht, me->RL_ht, me->EG_ht, me->EL_ht, me->AG_ht, me->AL_ht};
    for (i = 0; i < 8; i++) {
      byteCount += sizeof(EOS_CHAR);
      if (htables[i] != NULL) {
        byteCount += eos_HashTable1D_byteSize(htables[i]);
      }
    }
  }

  *packedTableSize = byteCount;
}

/***********************************************************************/
/*!
 * \brief This function returns the dimensions of the specified table.
 *
 * \param[out]    NT  - EOS_INTEGER : size of various arrays
 * \param[in]     *me - eos_RecordType2 : data object pointer;
 *
 * \return none
 *
 ***********************************************************************/
void eos_GetSizeRecordType2 (eos_RecordType2 *me, EOS_INTEGER *NT)
{
  *NT = me->NT;
}

/***********************************************************************/
/*!
 * \brief This function returns EOS_TRUE or EOS_FALSE depending upon the existence
 *        of loaded data table(s).
 *
 * \param[in]     *ptr     - void : data object pointer;
 * \param[in]     dataType - EOS_INTEGER : data type
 *
 * \return EOS_BOOLEAN
 *
 ***********************************************************************/
EOS_BOOLEAN eos_isRequiredDataLoadedRecordType2 (void *ptr, EOS_INTEGER dataType)
{
  eos_RecordType2 *me = (eos_RecordType2*) ptr;
  EOS_BOOLEAN bval = EOS_TRUE;

  EOS_INTEGER subTableNum = EOS_TYPE_TO_SUB_TAB_NUM (dataType);
  bval = (subTableNum > me->eosData.numSubtablesLoaded) ? EOS_FALSE : EOS_TRUE;

  return bval;
}

/***********************************************************************/
/*!
 * \brief This function returns pointers to data of class eos_RecordType2
 *
 * \param[in]     *me         - eos_RecordType2 : data object pointer;
 * \param[out]    **T         - EOS_REAL : holds T-pointer
 * \param[out]    **F         - EOS_REAL : holds F-pointer (F is 1d array)
 * \param[out]    **F_ht      - eos_HashTable1D : holds pointer to hashtable for F (pass NULL to ignore)
 * \param[in]     subtableNum - EOS_INTEGER : number of subtable to take the data from.
 *
 * \return none
 *
 ***********************************************************************/
void _eos_GetDataRecordType2 (eos_RecordType2 *me, EOS_REAL **T, EOS_REAL **F, eos_HashTable1D** F_ht,
                              EOS_INTEGER subTableNum)
{
  *T = me->T;

#ifdef DO_OFFLOAD
  if (gEosDataMap.useGpuData) {
    switch (subTableNum) {
    case 1:
      *F = (me->P == NULL) ? NULL  : me->gpu_ftbls[0];
      break;
    case 2:
      *F = (me->RG == NULL) ? NULL  : me->gpu_ftbls[1];
      break;
    case 3:
      *F = (me->RL == NULL) ? NULL  : me->gpu_ftbls[2];
      break;
    case 4:
      *F = (me->EG == NULL) ? NULL  : me->gpu_ftbls[3];
      break;
    case 5:
      *F = (me->EL == NULL) ? NULL  : me->gpu_ftbls[4];
      break;
    case 6:
      *F = (me->AG == NULL) ? NULL  : me->gpu_ftbls[5];
      break;
    case 7:
      *F = (me->AL == NULL) ? NULL  : me->gpu_ftbls[6];
      break;
    default:
      break;
    }
  }
  else
#endif /* DO_OFFLOAD */
  {
    switch (subTableNum) {
    case 1:
      *F = me->P;
      if (F_ht != NULL) *F_ht = me->P_ht;
      break;
    case 2:
      *F = me->RG;
      if (F_ht != NULL) *F_ht = me->RG_ht;
      break;
    case 3:
      *F = me->RL;
      if (F_ht != NULL) *F_ht = me->RL_ht;
      break;
    case 4:
      *F = me->EG;
      if (F_ht != NULL) *F_ht = me->EG_ht;
      break;
    case 5:
      *F = me->EL;
      if (F_ht != NULL) *F_ht = me->EL_ht;
      break;
    case 6:
      *F = me->AG;
      if (F_ht != NULL) *F_ht = me->AG_ht;
      break;
    case 7:
      *F = me->AL;
      if (F_ht != NULL) *F_ht = me->AL_ht;
      break;
    default:
      break;
    }
  }
}

/***********************************************************************/
/*!
 * \brief * Makes data of class eos_RecordType2 monotonic
 *
 * \param[out]    *err     - EOS_INTEGER : error code
 * \param[in]     *ptr     - void : data object pointer;
 *                                  internally recast to eos_RecordType2*
 * \param[in]     th       - EOS_INTEGER : table handle for error handling
 * \param[in]     dataType - EOS_INTEGER : data type of subtable to be made monotonic
 * \param[in]     inX      - EOS_BOOLEAN : make monotonic in x?
 * \param[in]     inY      - EOS_BOOLEAN : make monotonic in y?
 *
 * \return none
 *
 ***********************************************************************/
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

/***********************************************************************/
/*!
 * \brief checks if the data of class eos_RecordType2 is monotonic.
 *
 * \param[out]   *isMonotonic - EOS_BOOLEAN : is the data monotonic?
 * \param[out]   *err         - EOS_INTEGER : error code
 * \param[in]    *ptr         - void : internally recast to eos_RecordType2*
 * \param[in]    dataType     - EOS_INTEGER : dataType
 * \param[in]    inX          - EOS_BOOLEAN : make monotonic in x?
 * \param[in]    inY          - EOS_BOOLEAN : make monotonic in y?
 *
 * \return none
 *
 ***********************************************************************/
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

/***********************************************************************/
/*!
 * \brief Makes data of class eos_RecordType2 smooth
 *
 * \param[out]   *err     - EOS_INTEGER : error code
 * \param[in]     *ptr    - void : data object pointer;
 *                                 internally recast to eos_RecordType2*
 * \param[in]    th       - EOS_INTEGER : table Handle
 * \param[in]    dataType - EOS_INTEGER : data type of *ptr eos_RecordType2 object
 * \param[in]    ptSmooth - EOS_BOOLEAN : not used here
 *
 * \return none
 *
 ***********************************************************************/
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

/***********************************************************************/
/*!
 * \brief returns information items for the table.
 *
 * \param[out]   *err         - EOS_INTEGER : error code
 * \param[in]    *ptr         - void : data object pointer;
 *                                     internally recast to eos_RecordType2*
 * \param[in]    th           - EOS_INTEGER : table handle
 * \param[in]    numInfoItems - EOS_INTEGER : # of requested items
 * \param[in]    *infoItems   - EOS_INTEGER : array of requested items
 * \param[in]    *infoVals    - EOS_REAL : return item values
 *
 * \return none
 *
 ***********************************************************************/
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

/***********************************************************************/
/*!
 * \brief returns meta data information items for the table.
 *
 * \param[out]   *err         - EOS_INTEGER : error code
 * \param[in]    *ptr         - void : data object pointer;
 *                                     internally recast to eos_RecordType2*
 * \param[in]    *infoItem    - EOS_INTEGER : flag specifying what meta data item to fetch
 * \param[out]   *infoStr     - EOS_CHAR* : allocated string to contain all comments
 *
 * \return none
 *
 ***********************************************************************/
void eos_GetTableMetaDataRecordType2 (void *ptr, EOS_INTEGER infoItem,
				      EOS_CHAR *infoStr, EOS_INTEGER *err)
{
  //eos_RecordType2 *me;

  //me = (eos_RecordType2 *) ptr;
  *err = EOS_INVALID_DATA_TYPE; /* 101 table not found */
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
 * \param[in]    *ptr      - void : internally recast to eos_RecordType2*
 *
 * \return none
 *
 ***********************************************************************/
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

 /***********************************************************************/
/*!
 * \brief Set the flags so that appropriate data will be made monotonic upon loading
 *
 * \param[in]     *ptr    - void : data object pointer;
 *                                 internally recast to eos_RecordType2*
 * \param[in]    dataType - EOS_INTEGER : data type of *ptr eos_RecordType2 object
 * \param[in]    inX      - EOS_INTEGER : is data  of *ptr eos_RecordType2 object monotonic
 *                                        with respect to first independent variable?
 * \param[in]    inY      - EOS_INTEGER : not used here
 *
 * \return none
 *
 ***********************************************************************/
void eos_SetMonotonicityRecordType2 (void *ptr, EOS_INTEGER dataType,
                                     EOS_INTEGER inX, EOS_INTEGER inY)
{
  EOS_INTEGER i, subTableNum = EOS_TYPE_TO_SUB_TAB_NUM (dataType);
  eos_RecordType2 *me;

  me = (eos_RecordType2 *) ptr;

  assert(subTableNum>=1);
  assert(subTableNum<=MAX_TABLES_RECORDTYPE2);
  i = subTableNum - 1;

  if (inX >= 0 && !me->shouldBeMonotonicX[i]) me->shouldBeMonotonicX[i] = inX;
}

/***********************************************************************/
/*!
 * \brief Get the monotonicity flags
 *
 * \param[in]    dataType     - EOS_INTEGER : data type of *ptr eos_RecordType2 object
 * \param[out]   *inX         - EOS_INTEGER : is data  of *ptr eos_RecordType2 object monotonic
 *                                            with respect to first independent variable?
 * \param[out]   *inY         - EOS_INTEGER : not used here
 * \param[in]     *ptr   - void : data object pointer;
 *                         internally recast to eos_RecordType2*
 *
 * \return none
 *
 ***********************************************************************/
void eos_GetMonotonicityRecordType2 (void *ptr, EOS_INTEGER dataType,
                                     EOS_INTEGER *inX, EOS_INTEGER *inY)
{
  EOS_INTEGER i, subTableNum = EOS_TYPE_TO_SUB_TAB_NUM (dataType);
  eos_RecordType2 *me;

  me = (eos_RecordType2 *) ptr;

  assert(subTableNum>=1);
  assert(subTableNum<=MAX_TABLES_RECORDTYPE2);
  i = subTableNum - 1;

  *inX = me->shouldBeMonotonicX[i];
}

/***********************************************************************/
/*!
 * \brief Get the smoothing option flags.
 *
 * \param[in]    dataType     - EOS_INTEGER : data type of *ptr eos_RecordType2 object
 * \param[out]   *isSmooth    - EOS_INTEGER : is data  of *ptr eos_RecordType2 object smooth
 *                                            according to EOS_SMOOTH option?
 * \param[out]   *isPtSmooth  - EOS_INTEGER : is data  of *ptr eos_RecordType2 object smooth
 *                                            according to EOS_PT_SMOOTHING option?
 * \param[in]    *ptr         - void : data object pointer;
 *                                     internally recast to eos_RecordType2*
 *
 * \return none
 *
 ***********************************************************************/
void eos_GetSmoothingRecordType2 (void *me, EOS_INTEGER dataType,
                                  EOS_INTEGER *isSmooth,
                                  EOS_INTEGER *isPtSmooth)
{
  *isSmooth = 0;
  *isPtSmooth = 0;
}

/***********************************************************************/
/*!
 * \brief Find out if the requested type, monotonicity combo can share our table
 *  object
 *
 * \param[out]   *compatible  - EOS_BOOLEAN : are the monotonic options compatible?
 * \param[in]    *ptr         - void : data object pointer;
 *                                     internally recast to eos_RecordType2*
 * \param[in]    dataType     - EOS_INTEGER : data type of *ptr eos_RecordType2 object
 * \param[in]    inX          - EOS_INTEGER : is data  of *ptr eos_RecordType2 object monotonic
 *                                            with respect to first independent variable?
 *                                            (ignore if negative unless shouldBePtSmooth)
 * \param[in]    inY          - EOS_INTEGER : not used here
 *
 * \return none
 *
 ***********************************************************************/
void eos_AreMonotonicRequirementsCompatibleRecordType2 (void *ptr,
                                                        EOS_INTEGER dataType,
                                                        EOS_INTEGER inX,
                                                        EOS_INTEGER inY,
                                                        EOS_BOOLEAN
                                                        *compatible)
{
  EOS_INTEGER i, subTableNum = 1, subTableNumStart, subTableNumEnd;
  eos_RecordType2 *me;

  me = (eos_RecordType2 *) ptr;
  if (dataType > 0)
    subTableNumStart = subTableNumEnd = EOS_TYPE_TO_SUB_TAB_NUM (dataType);
  else {                        /* do all subtables */
    subTableNumStart = 1;
    subTableNumEnd = MAX_TABLES_RECORDTYPE2;
  }
  *compatible = EOS_TRUE;

  for (subTableNum = subTableNumStart; subTableNum <= subTableNumEnd; subTableNum++) {
    i = subTableNum - 1;

    if (inX >= 0 && me->shouldBeMonotonicX[i] != inX) *compatible = EOS_FALSE;

    if (*compatible == EOS_FALSE)
      break;
  }
}

/***********************************************************************/
/*!
 * \brief Set the flags so that appropriate data will be made smooth upon loading
 *
 * \param[in]    *ptr      - void : data object pointer;
 *                                  not used here
 * \param[in]    dataType  - EOS_INTEGER : not used here
 * \param[in]    pt_smooth - EOS_INTEGER : not used here
 * \param[in]    smooth    - EOS_INTEGER : not used here
 *
 * \return none
 *
 ***********************************************************************/
void eos_SetSmoothingRecordType2 (void *me, EOS_INTEGER dataType,
                                  EOS_INTEGER makeSmooth,
                                  EOS_INTEGER makePtSmooth)
{
  /* THIS FUNCTION IS PURPOSELY EMPTY */
}

/***********************************************************************/
/*!
 * \brief Find out if the requested type, smoothing combo can share our table object
 *
 * \param[out]   *compatible  - EOS_BOOLEAN : are the monotonic options compatible?
 * \param[in]    *ptr         - void : data object pointer;
 *                                     not used here
 * \param[in]    dataType     - EOS_INTEGER : not used here
 * \param[in]    makeSmooth   - EOS_INTEGER : not used here
 * \param[in]    makePtSmooth - EOS_INTEGER : not used here
 *
 * \return none
 *
 ***********************************************************************/
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

/**
 * \brief This function generates search hash tables for record type 2
 * 
 * \param[in,out] *ptr  - void : data object pointer
 */
void eos_GenerateHashTablesRecordType2(void *ptr)
{
  eos_RecordType2 *me = (eos_RecordType2*)ptr;
  if (me->T != NULL) { me->T_ht = eos_HashTable1D_gen(me->T, me->NT); }
                else { me->T_ht = NULL; }
  if (me->P != NULL) { me->P_ht = eos_HashTable1D_gen(me->P, me->NT); }
                else { me->P_ht = NULL; }
  if (me->RG != NULL) { me->RG_ht = eos_HashTable1D_gen(me->RG, me->NT); }
                else { me->RG_ht = NULL; }
  if (me->RL != NULL) { me->RL_ht = eos_HashTable1D_gen(me->RL, me->NT); }
                else { me->RL_ht = NULL; }
  if (me->EG != NULL) { me->EG_ht = eos_HashTable1D_gen(me->EG, me->NT); }
                else { me->EG_ht = NULL; }
  if (me->EL != NULL) { me->EL_ht = eos_HashTable1D_gen(me->EL, me->NT); }
                else { me->EL_ht = NULL; }
  if (me->AG != NULL) { me->AG_ht = eos_HashTable1D_gen(me->AG, me->NT); }
                else { me->AG_ht = NULL; }
  if (me->AL != NULL) { me->AL_ht = eos_HashTable1D_gen(me->AL, me->NT); }
                else { me->AL_ht = NULL; }
}

#ifdef DO_OFFLOAD

/***********************************************************************/
/*! 
 * \brief This function offloads object's data to the target GPU device.
 * 
 * \param[in,out] *ptr           - void : data object pointer
 * \param[in]     th             - EOS_INTEGER : table Handle
 * \param[out]    *errorCode     - EOS_INTEGER : error code
 * 
 * \return errorCode             - EOS_INTEGER : error code
 *
 ***********************************************************************/
EOS_INTEGER eos_GpuOffloadDataRecordType2(void *ptr, EOS_INTEGER th)
{
  eos_RecordType2 *me = (eos_RecordType2*) ptr;
  EOS_REAL *xtbls = NULL, *ftbls = NULL;
  EOS_INTEGER dataType, subTableNum;
  EOS_INTEGER i=0;
  EOS_INTEGER err = EOS_OK;
  EOS_REAL *_gpu_xtbls = NULL, *_gpu_ftbls = NULL;
  int h_ = omp_get_initial_device();
  int t_ = omp_get_default_device();
  assert(EOS_FALSE && "This setup is not ported to GPU");

  dataType = eos_GetDataTypeFromTableHandle (th, &err);
  if (eos_GetStandardErrorCodeFromCustomErrorCode(err) != EOS_OK)
    return(err);
  subTableNum = EOS_TYPE_TO_SUB_TAB_NUM(dataType);

  // set numbers of and pointers to eos data table x,y,f values.
  _eos_GetDataRecordType2 (me, &xtbls, &ftbls, NULL, EOS_TYPE_TO_SUB_TAB_NUM(dataType));

  /* allocate necessary device memory */
  if (xtbls)     _gpu_xtbls     = (EOS_REAL*) omp_target_alloc(sizeof(EOS_REAL)*me->NT, t_);
  if (ftbls)     _gpu_ftbls     = (EOS_REAL*) omp_target_alloc(sizeof(EOS_REAL)*me->NT, t_);

  /* store new pointers in me for later use */
  assert(EOS_TYPE_TO_SUB_TAB_NUM(dataType)>=1);
  assert(EOS_TYPE_TO_SUB_TAB_NUM(dataType)<=MAX_TABLES_RECORDTYPE2);
  i = EOS_TYPE_TO_SUB_TAB_NUM(dataType) - 1;
  me->gpu_xtbls[i]      = _gpu_xtbls;
  me->gpu_ftbls[i]      = _gpu_ftbls;

  /* copy necessary data to device */
  if (xtbls)     omp_target_memcpy(_gpu_xtbls    , xtbls,     sizeof(EOS_REAL)*me->NT, 0, 0, t_, h_);
  if (ftbls)     omp_target_memcpy(_gpu_ftbls    , ftbls,     sizeof(EOS_REAL)*me->NT, 0, 0, t_, h_);

  return(err);
}

#endif /* DO_OFFLOAD */
