/*********************************************************************
 * Class Name : eos_RecordType6
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

#define _EOS_RECORDTYPE6_INTERNAL_PROTOTYPES
#include "eos_types_internal.h"
#include "eos_RecordType6.h"

#include "eos_Utils.h"

#include "eos_Interpolation.h"

static const EOS_REAL ONE = (EOS_REAL) 1;
static const EOS_REAL ZERO = (EOS_REAL) 0;
static const EOS_REAL EOS_MIN_MASS_FRACTION = (EOS_REAL) 1.0e-8;

/************************************************************************
 * 
 * RecordType6 class constructor
 * 
 * Returned Values: none
 *
 * Input Value:
 * eos_RecordType6 *me         - this pointer (pointer to the instance of type eos_RecordType6
 * EOS_INTEGER      materialID - id of material to load.
 * EOS_INTEGER      th         - table handle.
 * 
 ************************************************************************/
void eos_ConstructRecordType6 (eos_RecordType6 *me, EOS_INTEGER th,
                               EOS_INTEGER materialID)
{
  me->NR = 0;
  me->NT = 0;
  me->NP = 0;
  me->avgAtomicNumber = (EOS_REAL) 0;
  me->avgAtomicWgt = (EOS_REAL) 0;
  me->refDensity = (EOS_REAL) 0;
  me->solidBulkModulus = (EOS_REAL) 0;
  me->exchangeCoefficient = (EOS_REAL) 0;
  me->R = NULL;
  me->T = NULL;
  me->table = NULL;
  me->eosData.dataFileOffset = -1;
  me->eosData.dataFileIndex = -1;
  me->eosData.dataSize = 0;
  eos_ConstructEosData ((eos_Data *) me, th, materialID);
  me->eosData.Load = eos_LoadRecordType6;
  me->eosData.Create = eos_CreateRecordType6;
  me->eosData.Destroy = eos_DestroyRecordType6;
  me->eosData.SetFileIndexes = eos_SetFileIndexesRecordType6;
  me->eosData.Print = eos_PrintRecordType6;
  me->eosData.GetPackedTable = eos_GetPackedTableRecordType6;
  me->eosData.SetPackedTable = eos_SetPackedTableRecordType6;
  me->eosData.GetPackedTableSize = eos_GetPackedTableSizeRecordType6;
  me->eosData.IsMonotonic = eos_IsMonotonicRecordType6;
  me->eosData.MakeMonotonic = eos_MakeMonotonicRecordType6;
  me->eosData.MakeSmooth = eos_MakeSmoothRecordType6;
  me->eosData.GetTableInfo = eos_GetTableInfoRecordType6;
  me->eosData.GetTableMetaData = eos_GetTableMetaDataRecordType6;
  me->eosData.GetLoadedBulkData = eos_GetLoadedBulkDataRecordType6;
  me->eosData.SetMonotonicity = eos_SetMonotonicityRecordType6;
  me->eosData.AreMonotonicRequirementsCompatible =
    eos_AreMonotonicRequirementsCompatibleRecordType6;
  me->eosData.SetSmoothing = eos_SetSmoothingRecordType6;
  me->eosData.GetMonotonicity = eos_GetMonotonicityRecordType6;
  me->eosData.GetSmoothing = eos_GetSmoothingRecordType6;
  me->eosData.AreSmoothingRequirementsCompatible =
    eos_AreSmoothingRequirementsCompatibleRecordType6;
  me->eosData.Interpolate = eos_InterpolateRecordType6;
  me->eosData.CheckExtrap = eos_CheckExtrapRecordType6;
  me->eosData.InvertAtSetup = NULL; /* no inversion at setup allowed */
  me->eosData.SetExtrapolationBounds = NULL; /* no extrapolation bounds stored */
  me->eosData.varOrder = X_Y_F;
  me->eosData.tmpVarOrder = -1;
}

/************************************************************************
 * 
 * RecordType6 class destructor
 * 
 * Returned Values: none
 *
 * Input Value:
 * eos_RecordType6 *me  - this pointer (pointer to the instance of type eos_RecordType6
 * 
 ************************************************************************/
void eos_DestroyRecordType6 (void* ptr)
{
  eos_RecordType6 *me = (eos_RecordType6*) ptr;

  if (!me->table) {
    if (!me->eosData.destructing)       /* to prevent circular calls to destructor */
      eos_DestroyEosData (&(me->eosData));
    return;
  }

  eos_SetSizeRecordType6 (me, 0, 0, 0, 0);

  if (!me->eosData.destructing) /* avoid circular destructor calls */
    eos_DestroyEosData (&(me->eosData));
}

/************************************************************************
 * 
 * This function allocates data of RecordType6 for loading
 * 
 * Returned Values: none
 *
 * Input Value:
 * eos_RecordType6 *me  - this pointer (pointer to the instance of type eos_RecordType6
 * 
 ************************************************************************/
void eos_CreateRecordType6 (void *ptr, EOS_INTEGER th)
{
  eos_RecordType6 *me;

  me = (eos_RecordType6 *) ptr;

  gEosDataMap.errorCodes[th] = EOS_OK; /* reset previous error */
 
  /* set the sesame file indexes and offsets for RecordType6 */
  eos_SetFileIndexesRecordType6 (me, th);

  if (gEosDataMap.errorCodes[th]) /* return previous error to parent */
    return;

  /* check if there is enough data -- at least 1 subtable is required! */
  if (me->eosData.dataSize < 2 + me->NR + me->NT + me->NR * me->NT) {
    ((eos_ErrorHandler *) me)->HandleError (me, th, EOS_READ_DATA_FAILED);
    return;
  }

  // allocate enough memory
  eos_SetSizeRecordType6 (me, me->NR, me->NT, me->NP, me->eosData.tableNum);
}

/***********************************************************************/
/*! 
 * \brief This function sets the sesame file indexes and offsets for RecordType6
 * 
 * \param[in,out] *ptr       - void : data object pointer;
 *                             internally recast to eos_RecordType6*
 *                             contents are allocated
 * \param[in]     th         - EOS_INTEGER : table handle
 * 
 * \return none
 *
 ***********************************************************************/
void eos_SetFileIndexesRecordType6 (void *ptr, EOS_INTEGER th)
{
  eos_RecordType6 *me;
  EOS_INTEGER matid, count;
  EOS_INTEGER ierr = EOS_OK;
  EOS_REAL *read_data;
  EOS_CHAR *errMsg = NULL;

  me = (eos_RecordType6 *) ptr;
  matid = me->eosData.materialID;
  read_data = NULL;

  /* check handle's error code; this was moved from Create upon the introduction
   * of the eos_SetDataFileName function */
  if (eos_GetStandardErrorCodeFromCustomErrorCode(gEosDataMap.errorCodes[th]) == EOS_MATERIAL_NOT_FOUND) {
    ierr = gEosDataMap.errorCodes[th];
    ((eos_ErrorHandler *) me)->HandleError (me, th, ierr);
    return;
  }

  /* get the file offset for reading data and get back 5 reals: NT, NR, date1, date2 and vers  */
  {
    ses_material_id mid = (ses_material_id)me->eosData.materialID;
    ses_table_id    tid = (ses_table_id)me->eosData.tableNum;
    ierr = eos_SesSeekToDataTable (matid, me->eosData.tableNum, &read_data, 5,
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

  // determine how many subtables (a.k.a. material phases) are available;
  // return error code if requested is not available
  count = 0;
  me->NR = (EOS_INTEGER) read_data[count++];    /* count = 0 */
  me->NT = (EOS_INTEGER) read_data[count++];    /* count = 1 */
  /* read creation date */
  me->eosData.creationDate = (EOS_INTEGER) read_data[count++];     /* 2 */
  /* read modification date */
  me->eosData.modificationDate = (EOS_INTEGER) read_data[count++]; /* 3 */
  if (me->eosData.modificationDate == 0)
    me->eosData.modificationDate = me->eosData.creationDate;
  /* read version number */
  me->eosData.latestVersion = (EOS_INTEGER) read_data[count++];    /* 4 */
  /* NW = 1 + 1 + NR + NT + NR*NT*NP
   * NP = ( NW - 2 - NR - NT ) / ( NR*NT )
   *  where NW = me->eosData.dataSize
   */
  me->NP = ( me->eosData.dataSize - 2 - me->NR - me->NT ) / ( me->NR * me->NT );

  /* check if there is enough data -- at least 1 subtable is required! */
  if (me->eosData.dataSize < 2 + me->NR + me->NT + me->NR * me->NT) {
    ((eos_ErrorHandler *) me)->HandleError (me, th, EOS_READ_DATA_FAILED);
    EOS_FREE (read_data);
    return;                     /* wrong amount of data read! */
  }

  /* deallocate read_data[], which was allocated by eos_SesSeekToDataTable() above */
  EOS_FREE (read_data);
}

/************************************************************************
 * 
 * This function loads data of RecordType6 and stores the data in the class'
 * data structures.
 * 
 * Returned Values: none
 *
 * Input Value:
 * eos_RecordType6 *me  - this pointer (pointer to the instance of type eos_RecordType6
 * 
 ************************************************************************/
void eos_LoadRecordType6 (void *ptr, EOS_INTEGER th)
{
  eos_RecordType6 *me;
  EOS_INTEGER i, j, k, count;
  EOS_INTEGER ierr = EOS_OK;
  EOS_REAL *read_data;
  EOS_CHAR *errMsg = NULL;

#ifdef DEBUG
  printf ("loading data for type 1...\n");
#endif
  me = (eos_RecordType6 *) ptr;
  read_data = NULL;

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
				 "eos_RecordType6::eos_GetBulkData ERROR, bulk data not available for table handle, %i", th);
    return;
  }

#ifdef DEBUG
  printf
    ("DEBUG >>> eos_LoadRecordType6: avgAtomicNumber=%E, avgAtomicWgt=%E, refDensity=%E\n",
     me->avgAtomicNumber, me->avgAtomicWgt, me->refDensity);
#endif

  /* Load the read_data array with all of the available data. */

  if (me->eosData.dataSize < 2) { /* eos_CreateRecordType6 failed to load data */

    /* tableNum doesn't exist in Sesame for current matID and no splitting requested */
    ierr = EOS_DATA_TYPE_NOT_FOUND;

  }
  else {

    /* DATA SIZE INCLUDES DATA ALREADY READ */
    ierr = eos_SesLoadSesameFiles (me->eosData.materialID, me->eosData.tableNum,
				   me->eosData.dataFileIndex,
				   &read_data, me->eosData.dataSize - 2);
    if (ierr) {
#ifdef DEBUG
      printf ("error loading table! ERROR %d\n", ierr);
#endif
      ((eos_ErrorHandler *) me)->HandleError (me, th, ierr);
      return;
    }

    /* Store the number of subTables stored in memory.
       This should be the same value as me->NP. */
    me->eosData.numSubtablesLoaded =
      ( me->eosData.dataSize - 2 - me->NR - me->NT ) / ( me->NR * me->NT );
    if (me->eosData.numSubtablesLoaded != me->NP) {
      ierr = EOS_READ_DATA_FAILED;
      ierr = eos_SetCustomErrorMsg(th, ierr,
				   "eos_RecordType6::eos_LoadRecordType6 ERROR, Could not load data table; eosData.numSubtablesLoaded != NP");
    }
  }

  if (eos_GetStandardErrorCodeFromCustomErrorCode(ierr) != EOS_OK) {
#ifdef DEBUG
    printf ("error loading table! ERROR %d\n", ierr);
#endif
    ((eos_ErrorHandler *) me)->HandleError (me, th, ierr);
    return;
  }

  count = 0;

  for (i = 0; i < me->NR; i++) {
#ifdef DEBUG
    printf("%3d. %23.15e\n",i,read_data[count]);
#endif
    me->R[i] = read_data[count++];      /* count = 0 to NR-1 */
  }

  for (i = 0; i < me->NT; i++) {
#ifdef DEBUG
    printf("%3d. %23.15e\n",i,read_data[count]);
#endif
    me->T[i] = read_data[count++];      /* count = NR  to  NR+NT-1 */
  }

  /*  table[NP][NT][NR] */
//#define DEBUG
  for (k = 0; k < me->NP; k++) {
#ifdef DEBUG
    printf ("Material %d, Table 321, Subtable %d\n", me->eosData.materialID, k+1);
#endif
    for (j = 0; j < me->NT; j++) {
      for (i = 0; i < me->NR; i++) {
	me->table[k][j][i] = read_data[count++];      /* count = NR+NT  to  NR+NT+(NT*NR*NP)-1 */
#ifdef DEBUG
        printf ("%23.15e %23.15e %23.15e\n",
                me->R[i], me->table[k][j][i], me->T[j]);
#endif
      }
#ifdef DEBUG
      printf ("\n");
#endif
    }
  }
#undef DEBUG

  EOS_FREE (read_data);
  me->eosData.isLoaded = 1;

}

/************************************************************************
 * 
 * This function allocates enough memory in class eos_RecordType6
 * to hold R and T arrays of specified size, and to hold NP arrays of NR*NT size.
 * 
 * Returned Values: none
 *
 * Input Value:
 * eos_RecordType6 *me  - this pointer (pointer to the instance of type eos_RecordType6
 * EOS_INTEGER      NR  - size of R array
 * EOS_INTEGER      NT  - size of T array
 * EOS_INTEGER      NP  - number of subtables
 * 
 ************************************************************************/

void eos_SetSizeRecordType6 (eos_RecordType6 *me, EOS_INTEGER NR,
                             EOS_INTEGER NT, EOS_INTEGER NP, EOS_INTEGER tableNum)
{
  int i, j, k, count, offset1, offset2;
  int d1_size, d2_size, d3_size, pad1, pad2;
  unsigned char *ptr;

  // assume that it's the first time!
  me->NR = NR;
  me->NT = NT;
  me->NP = NP;
  if (me->R)
    EOS_FREE (me->R);           // This deallocates all continuous memory
  /* memory is allocated continuously in one chunk for R, T and all subtables */
  if (me->table)
    EOS_FREE (me->table);

  if (NR == 0 || NT == 0 || NP == 0)
    return;                     // just deallocate memory this time

  /*
    Emulate the following arrays in a continuous block of
    memory with a single malloc():
    
    // table[i:0..(NP-1)][j:0..(NT-1)][k:0..(NR-1)]
    table = (EOS_REAL ***) malloc( NP * sizeof(EOS_REAL **) );
    for ( i = 0 ; i < NP ; ++i ) {
    table[i] = (EOS_REAL **) malloc( NT * sizeof(EOS_REAL *) );
    for ( j = 0 ; j < NT ; ++j ) {
    table[i][j] = (EOS_REAL *) malloc( NR * sizeof(EOS_REAL) );
    }
    }
  */

  /* allocate memory continuously for R[NR] and T[NT] */
  me->R = (EOS_REAL *) malloc( (NR + NT) * sizeof(EOS_REAL) );
  me->T = me->R + NR;

  /* calculate memory padding to align EOS_REAL data on 8-byte boundaries */
  pad1 = (sizeof (EOS_REAL **) < sizeof (EOS_REAL)) ? NP % 2 : 0;
  pad2 = (sizeof (EOS_REAL * ) < sizeof (EOS_REAL)) ? (NP * NT) % 2 : 0;

  d3_size = sizeof (EOS_REAL **) * (NP + pad1);
  d2_size = sizeof (EOS_REAL *)  * (NP + pad1) * (NT + pad2);
  d1_size = sizeof (EOS_REAL  )  * NP * NR * NT;

  /* allocate memory continuously for NP subtables with NR*NT dimension */
  ptr = (unsigned char *) malloc (d3_size /* pointers to subtables */
                                  + d2_size /* pointers to subtable rows */
                                  + d1_size /* data values */
                                  );
  if (!ptr) {
    me->eosData.isAllocated = 0;
    EOS_FREE(me->R);
    me->T = NULL;
    me->NR = 0;
    me->NT = 0;
    me->NP = 0;
    return;
  }

  me->table = (EOS_REAL ***) ptr;
  for ( i = 0  ;  i < NP  ;  i++ ) {
    offset1 = d3_size //((NP + pad1) * sizeof(EOS_REAL **))
      + i * ((NT + pad2) * sizeof(EOS_REAL *))
      + i * (NT * NR * sizeof(EOS_REAL));

    me->table[i] = (EOS_REAL **)(ptr + offset1);
    for ( j = 0  ;  j < NT  ;  j++ ) {
      offset2 =  ((NT + pad2) * sizeof(EOS_REAL *)) + j * (NR * sizeof(EOS_REAL));
      me->table[i][j] = (EOS_REAL *)(ptr + offset1 + offset2);
    }
  }

  /* DAP -- test table array allocation */
  count = 0;
  /*  table[NP][NT][NR] */
  for (k = 0; k < me->NP; k++) {
    for (j = 0; j < me->NT; j++) {
      for (i = 0; i < me->NR; i++) {
        me->table[k][j][i] = (EOS_REAL) count++;
      }
    }
  }

  me->eosData.isAllocated = 1;
}

void eos_GetSizeRecordType6 (eos_RecordType6 *me, EOS_INTEGER *NR,
                             EOS_INTEGER *NT)
/* returns the size of a subtable */
{
  *NR = me->NR;
  *NT = me->NT;
}

/************************************************************************
 * 
 * This function returns pointers to data of class eos_RecordType6
 * 
 * Returned Values: none
 *
 * Input Value:
 * eos_RecordType6 *me  - this pointer (pointer to the instance of type eos_RecordType6
 * EOS_REAL			**R  - output: holds R-pointer
 * EOS_REAL			**T  - output: holds T-pointer
 * EOS_REAL			***F - output: holds F-pointer (F is 2d array allocated as 1 d array NR * NT)
 * EOS_REAL			**coldCurve  - output: holds cold curve-pointer
 * EOS_INTEGER      subtableNum  - number of subtable to take the data from.
 * 
 ************************************************************************/
void _eos_GetDataRecordType6 (eos_RecordType6 *me, EOS_REAL **R, EOS_REAL **T,
                              EOS_REAL ***F, EOS_INTEGER subTableNum)
{
  *R = me->R;
  *T = me->T;

  if (me->eosData.numSubtablesLoaded < subTableNum) {
    *R = *T = NULL;
    *F = NULL;
    return;
  }

  *F = me->table[MAX(subTableNum-1,0)];
}

/************************************************************************
 * 
 * This function prints the data of class eos_RecordType6
 * 
 * Returned Values: EOS_INTEGER *err - output error code
 *
 * Input Value:
 * void *ptr       - this pointer (pointer to the instance of type eos_RecordType6
 * EOS_CHAR * fname;
 * EOS_INTEGER append   - whether or not to append to file
 * EOS_INTEGER th   - table Handle
 *
 ************************************************************************/
void eos_PrintRecordType6 (void *ptr, EOS_INTEGER th, EOS_CHAR *fname,
                           EOS_INTEGER append, EOS_INTEGER *err)
{
  EOS_INTEGER nxtbl, nytbl, subTableNum;
  EOS_INTEGER jl, nline, ipage, iy, jf0, jx, jy, jf;
  EOS_INTEGER dataType;
  char buffer[_MIN_FIELD_WIDTH+1];
  EOS_CHAR *sesame_fname;
  EOS_REAL xprnt, yprnt, fprnt, *xtbls = NULL, *ytbls =
    NULL, xconv, yconv, fconv;
  FILE *tableFile;
  eos_RecordType6 *me;

//#define DEBUG_EOS_PRINTRECORDTYPE6
#ifdef DEBUG_EOS_PRINTRECORDTYPE6
  EOS_INTEGER k, l;
#endif

  EOS_BOOLEAN logAxes = EOS_TYPE_TO_LOG_AXES (gEosDataMap.tableTypes[th]);

  me = (eos_RecordType6 *) ptr;

  *err = EOS_OK;
  dataType = eos_GetDataTypeFromTableHandle (th, err);
  if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) != EOS_OK)
    return;

  if (me->eosData.numSubtablesLoaded < 1) {
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

  /* process y-conversion option */
  yconv = eos_getRealOptionFromTableHandle (th, EOS_Y_CONVERT, err);
  if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) != EOS_OK)
    return;

  /* process f-conversion option */
  fconv = eos_getRealOptionFromTableHandle (th, EOS_F_CONVERT, err);
  if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) != EOS_OK)
    return;

  // set numbers of and indices to eos data table x,y,f values.

  nxtbl = me->NR;
  nytbl = me->NT;
  xtbls = me->R;
  ytbls = me->T;

  for (subTableNum=1; subTableNum<=me->NP; subTableNum++) {

    if (EOS_CATEGORY (dataType) != EOS_CATEGORY0) {

      *err = EOS_CANT_INVERT_DATA;
      *err = eos_SetCustomErrorMsg(th, *err,
				   "eos_RecordType6::eos_PrintRecordType6 ERROR, cannot invert table");
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

    /* print eos data table x,y,f values.
       x values run down page and y values run across page. */

    ipage = 0;
    for (iy = 1; iy <= nytbl; iy += 10) {
      ipage = ipage + 1;

      fprintf (tableFile,
	       "%sTableHandle=%i matid =%6d source = %-20s page = %2u\n%s%s\n%s%s%s%d\n",
	       ((append
		 || ipage > 1) ? "\n" : ""), th, me->eosData.materialID,
	       sesame_fname, ipage, "Data Type = ",
	       EOS_TYPE_TO_STRING (dataType), "Description = ",
	       EOS_TYPE_TO_TAB_NAME (dataType),"   Subtable #",subTableNum);

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
	  fprnt = me->table[subTableNum-1][iy-1+jl-1][jx-1] * fconv;
	  if (logAxes)
	    fprnt = pow (10.0, fprnt);
	  _eos_dbl2String (fprnt, _MIN_FIELD_WIDTH, buffer);
	  fprintf (tableFile, "%*s", _MIN_FIELD_WIDTH + 1, buffer);
	}
	fprintf (tableFile, "\n");
      }
    }

    fprintf (tableFile, "\n");

  } // end for (subTableNum=1; subTableNum<=me->NP; subTableNum++)

  fclose (tableFile);

}

/************************************************************************
 * 
 * This packs the data of class eos_RecordType6 into provided char array
 * 
 * Returned Values: EOS_INTEGER *err - output error code
 *
 * Input Value:
 * EOS_INTEGER th - table handle
 * void *ptr       - this pointer (pointer to the instance of type eos_RecordType6
 * EOS_CHAR *packedTable - allocated by user char array large enough to store packed data
 * 
 ************************************************************************/
void eos_GetPackedTableRecordType6 (void *ptr, EOS_INTEGER th,
                                    EOS_CHAR *packedTable, EOS_INTEGER *err)
{
  eos_RecordType6 *me;
  EOS_INTEGER t, byteCount = 0;
  EOS_REAL *F;

  *err = EOS_OK;

  me = (eos_RecordType6 *) ptr;

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

  memcpy (packedTable + byteCount, &(me->NR), sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (packedTable + byteCount, &(me->NT), sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (packedTable + byteCount, &(me->NP), sizeof (EOS_INTEGER));
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

  /* pack ALL subtables, to be consistent with Load() for RT6 */
  for (t = 1; t <= me->NP; t++) {
    if (t > me->NP)
      break;

    F = &(me->table[t-1][0][0]);

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

}


/************************************************************************
 * 
 * This sets the data of class eos_RecordType6 from the packed char array provided by caller
 * 
 * Returned Values: EOS_INTEGER *err - output error code
 *
 * Input Value:
 * void *ptr       - this pointer (pointer to the instance of type eos_RecordType6
 * EOS_CHAR *packedTable - allocated by user char array large enough to store packed data
 * EOS_INTEGER packedTableSize - size in chars of packed data array
 * EOS_INTEGER th - table handle
 * 
 ************************************************************************/
void eos_SetPackedTableRecordType6 (void *ptr, EOS_INTEGER th,
                                    EOS_CHAR *packedTable, EOS_INTEGER *err)
{
  eos_RecordType6 *me;
  EOS_INTEGER dataType, nt, nr, np, byteCount =
    0, tableNum, t, tmpINT;
  EOS_REAL *F;

  *err = EOS_OK;

  me = (eos_RecordType6 *) ptr;

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

  memcpy (&nr, packedTable + byteCount, sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (&nt, packedTable + byteCount, sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);
  memcpy (&np, packedTable + byteCount, sizeof (EOS_INTEGER));
  byteCount += sizeof (EOS_INTEGER);

  /* allocate memory if more memory is needed */
  if (nr > me->NR || nt > me->NT || np > me->NP)
    eos_SetSizeRecordType6 (me, nr, nt, np, tableNum);
  me->NR = nr;
  me->NT = nt;
  me->NP = np;

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

  /* unpack ALL subtables, to be consistent with Load() for RT6 */
  for (t = 1; t <= me->NP; t++) {
    if (t > me->NP)
      break;

    F = &(me->table[t-1][0][0]);

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
  me->eosData.isLoaded = (me->eosData.numSubtablesLoaded > 0) ? 1 : 0;

}

/************************************************************************
 * 
 * This returns the size of packed char array needed to store the data of class eos_RecordType6
 * 
 * Returned Values: EOS_INTEGER *err - output error code
 *
 * Input Value:
 * void *ptr       - this pointer (pointer to the instance of type eos_RecordType6
 * EOS_INTEGER *packedTableSize - size in chars of packed data array
 * EOS_INTEGER th   - table handle
 * 
 ************************************************************************/
void eos_GetPackedTableSizeRecordType6 (void *ptr, EOS_INTEGER th,
                                        EOS_INTEGER *packedTableSize,
                                        EOS_INTEGER *err)
{
  eos_RecordType6 *me;
  EOS_INTEGER byteCount = 0;
  *err = EOS_OK;

  me = (eos_RecordType6 *) ptr;

  byteCount += sizeof (EOS_REAL);
  byteCount += sizeof (EOS_REAL);
  byteCount += sizeof (EOS_REAL);
  byteCount += sizeof (EOS_REAL);
  byteCount += sizeof (EOS_REAL);
  byteCount += sizeof (EOS_INTEGER);
  byteCount += sizeof (EOS_INTEGER);
  byteCount += sizeof (EOS_INTEGER);
  byteCount += sizeof (EOS_REAL) * me->NR;
  byteCount += sizeof (EOS_REAL) * me->NT;
  byteCount += sizeof (EOS_INTEGER);    /* number of subtable loaded */
  byteCount += sizeof (EOS_INTEGER);    /* if cold curve is loaded */

  byteCount +=
    me->eosData.numSubtablesLoaded * me->NT * me->NR * me->NP * sizeof (EOS_REAL);
  if (me->eosData.coldCurveIsLoaded)
    byteCount += me->eosData.numSubtablesLoaded * me->NR * sizeof (EOS_REAL);   /* cold curves for each subtable */
  byteCount += (sizeof (EOS_INTEGER) * 2 + sizeof (long));

  *packedTableSize = byteCount;
}

/*	function _eos_InterpolateRecordType6 (helping function for eos_InterpolateEosInterpolation() 
	The eos_InterpolateEosInterpolation() routine provides interpolated values for a single material using a table handle associated with
	Data stored within an data table.

	The input arguments are:
		void *ptr                 a pointer to a eos_RecordType6 instance from which to extract the data. 
		EOS_INTEGER th            table Handle
		EOS_INTEGER subTableNum   which subtable/phase to use for interpolation
		EOS_INTEGER varOrder      order of variables
		EOS_INTEGER dataType      dataType
		EOS_INTEGER nXYPairs	  total number of pairs of independent variable values provided for interpolation.
		EOS_REAL srchX[nXYPairs]  array of the primary independent variable values to use during interpolation.
		EOS_REAL srchY[nXYPairs]  array of the secondary independent variable values to use during interpolation.

	The output arguments are:
		EOS_REAL fVals[nXYPairs]  array of the interpolated data corresponding to x and y. 
		EOS_REAL dFx[nXYPairs]	  array of the interpolated partial derivatives of fVals with respect to x. 
		EOS_REAL dFy[nXYPairs]	  array of the interpolated partial derivatives of fVals with respect to y. 
		EOS_REAL dFCx[nXYPairs]	  optional array of the interpolated partial derivatives of cold curve with respect to x. 
		EOS_REAL dFx0[nXYPairs]	  optional array of the interpolated partial derivatives of cat 0 F wrt x (for inverse functions mostly). 
		EOS_REAL dFy0[nXYPairs]	  optional array of the interpolated partial derivatives of cat 0 F wrt y (for inverse functions mostly). 
		EOS_INTEGER *xyBounds     interpolation errors per xy-pair
		EOS_INTEGER errorCode	  error code of the interpolation: EOS_INTERP_EXTRAPOLATED or EOS_OK
*/

void _eos_InterpolateRecordType6 (void *ptr, EOS_INTEGER th, EOS_INTEGER subTableNum,
                                  EOS_INTEGER varOrder, EOS_INTEGER dataType,
                                  EOS_INTEGER nXYPairs, EOS_REAL *srchX,
                                  EOS_REAL *srchY, EOS_REAL *fVals,
                                  EOS_REAL *dFx, EOS_REAL *dFy,
                                  EOS_INTEGER *xyBounds,
                                  EOS_INTEGER *errorCode)
{
  EOS_INTEGER i, doRational =
    0, err, nX, nY, cat, *ixv, *iyv, *xyBounds2;
  EOS_REAL *X, *Y, **F, *xVals, *yVals;
  eos_Data *eosData;
  eos_RecordType6 *me;

  *errorCode = EOS_OK;

  if (nXYPairs <= 0) {
    *errorCode = EOS_FAILED;
    return;
  }

  err = EOS_OK;
  eosData = (eos_Data *) ptr;
  me = (eos_RecordType6 *) eosData;

  /* make sure the data is record type 6 */
  if (EOS_TYPE_TO_RECORD_TYPE (dataType) != EOS_RECORD_TYPE6) {
    *errorCode = EOS_INVALID_TABLE_HANDLE;
    ((eos_ErrorHandler *) me)->HandleError (me, th, *errorCode);
    return;
  }

  cat = EOS_CATEGORY (dataType);

  /* get the size of the data */
  eos_GetSizeRecordType6 (me, &nX, &nY);

  if (me->eosData.numSubtablesLoaded < subTableNum) {
    *errorCode = EOS_DATA_TYPE_NOT_FOUND;
    ((eos_ErrorHandler *) me)->HandleError (me, th, *errorCode);
    return;
  }

  /* initialize F, dFx, dFy to zero */
  for (i = 0; i < nXYPairs; i++) {
    fVals[i] = ZERO;
    dFx[i] = ZERO;
    dFy[i] = ZERO;
  }

  _eos_GetDataRecordType6 (me, &X, &Y, &F, subTableNum);

  xVals = srchX;
  yVals = srchY;

  switch (cat) {
  case EOS_CATEGORY0:          /* indicates the table is not inverted */
    {

      /* force LINEAR interpolation */
      gEosInterpolation.interpolationDataList[th]->interpolationType = EOS_LINEAR;

      doRational = eos_getBoolOptionFromTableHandle (th, EOS_RATIONAL, &err);
      if (doRational) {

	/* .... perform table searches to load indices and spacings of nearest
	   .... data table x,y values to x,searchYVals. */

	iyv = (EOS_INTEGER *) malloc (nXYPairs * sizeof (EOS_INTEGER));       /* indexes of Y near which Y points are */
	ixv = (EOS_INTEGER *) malloc (nXYPairs * sizeof (EOS_INTEGER));       /* indexes of X near which X points are */
	xyBounds2 = (EOS_INTEGER *) malloc (nXYPairs * sizeof (EOS_INTEGER)); /* xy-bounds for y */

	_eos_srchdf (nXYPairs, yVals, 1, nY - 1, Y, 1, iyv,
		     xyBounds, errorCode);
	_eos_srchdf (nXYPairs, xVals, 1, nX - 1, X, 1, ixv,
		     xyBounds2, errorCode);

	for (i = 0; i < nXYPairs; i++)
	  xyBounds[i] = _eos_CombineExtrapErrors (xyBounds2[i], xyBounds[i]);

        eos_BiRationalInterpolate (nXYPairs, nX, nY, X, Y, F, ixv, iyv, xVals, yVals,
                                   fVals, dFx, dFy, xyBounds, &err);

	EOS_FREE (xyBounds2);
	EOS_FREE (ixv);
	EOS_FREE (iyv);

      }
      else if (eos_getBoolOptionFromTableHandle (th, EOS_LINEAR, &err)) /* interpolate linearly instead */
        eos_BiLineInterpolate (eos_getBoolOptionFromTableHandle (th, EOS_DISCONTINUOUS_DERIVATIVES, &err),
			       nXYPairs, nX, nY, X, Y, F, xVals, yVals, fVals,
                               dFx, dFy, xyBounds, &err);
      if (eos_GetStandardErrorCodeFromCustomErrorCode(err) != EOS_OK)
        *errorCode = err;

      break;
    }

  default:
    {
      *errorCode = EOS_INVALID_TABLE_HANDLE;
      ((eos_ErrorHandler *) me)->HandleError (me, th, *errorCode);
      break;
    }
  } /* end switch (cat) */

  if (srchX != xVals)
    EOS_FREE (xVals);
  if (srchY != yVals)
    EOS_FREE (yVals);

}

/*
  integer function _eos_GetNumberOfPhases
  This function is used to determine the total number of material phases that are tabulated
  within the Sesame mass fraction table.
  The input arguments are:
      void *ptr               a pointer to a eos_RecordType6 instance from which to extract the data.
      
  The output argument is:
      EOS_INTEGER errorCode   error code of the interpolation: EOS_INTERP_EXTRAPOLATED or EOS_OK

  The returned value is an EOS_INTEGER corresponding to the total number of material phases.
*/
EOS_INTEGER _eos_GetNumberOfPhases(void *ptr, EOS_INTEGER *errorCode, EOS_CHAR **errMsg) {

  eos_RecordType6 *me;

  me = (eos_RecordType6 *) ptr;

  *errorCode = EOS_OK;

  if (me->eosData.numSubtablesLoaded < 0) {
    *errorCode = EOS_UNDEFINED;
    eos_SetCustomMsg_str(errMsg,
			 "eos_RecordType6::_eos_GetNumberOfPhases ERROR, multiphase data unavailable");
    return 0;
  }

  return me->NP;
}

/*! function _eos_maxloc
    This function determines the index location of the maximum value in EOS_REAL
    array (A) of size N corresponding to EOS_TRUE elements of EOS_BOOLEAN mask
    array (M) of size N (if M=NULL, then ignore it). If more than one element has the same
    maximum value, the location of the first one found is returned.
*/
EOS_INTEGER _eos_maxloc(EOS_REAL *A, EOS_BOOLEAN *M, EOS_INTEGER N)
{
  EOS_INTEGER i, j=0;

  if (M == NULL) { /* M is not used */

    for (i = 0; i < N; i++) {
      if (A[j] < A[i])
	j = i;
    }

  }
  else { /* M is used */

    for (i = 0; i < N; i++) {
      if (M[i]) {
	j = i;
	break;
      }
    }

    for (i = 0; i < N; i++) {
      if (! M[i])
	continue;
      if (A[j] < A[i])
	j = i;
    }

  }

  return j;
}

/*	function eos_ApplyMassFracConstraintsRecordType6 (helping function for eos_InterpolateRecordType6() 
	The eos_ApplyMassFracConstraintsRecordType6() routine applies constraints to interpolated values.

	The input arguments are:
		void *ptr                 a pointer to a eos_RecordType6 instance from which to extract the data. 
		EOS_INTEGER th            table Handle
		EOS_INTEGER dataType      dataType
		EOS_INTEGER nXYPairs	  total number of pairs of independent variable values provided for interpolation.

	The output arguments are:
		EOS_REAL fVals[NP*nXYPairs]  array of the interpolated data corresponding to x and y. 
		EOS_INTEGER errorCode	  error code of the interpolation: EOS_INTERP_EXTRAPOLATED or EOS_OK
*/

void eos_ApplyMassFracConstraintsRecordType6 (void *ptr, EOS_INTEGER th,
					      EOS_INTEGER dataType, EOS_INTEGER nXYPairs,
					      EOS_REAL *fVals, EOS_INTEGER *errorCode)
{

  EOS_INTEGER subTableNum, i, i_max1, i_max2;
  EOS_BOOLEAN *mask=NULL;
  EOS_REAL *F;
  eos_RecordType6 *me;

  me = (eos_RecordType6 *) ptr;

  *errorCode = EOS_OK;

  if (dataType == EOS_M_DT && me->NP > 2) { /* no constraints necessary for one or two subTables */

    mask = (EOS_BOOLEAN *) malloc(me->NP * sizeof(EOS_BOOLEAN));
    F = (EOS_REAL *) malloc(me->NP * sizeof(EOS_REAL));

    /* apply data constraints to interpolated values. */
    for (i = 0; i < nXYPairs; i++) {

      /* initialize temporary arrays */
      for (subTableNum = 0; subTableNum < me->NP; subTableNum++) {
	mask[subTableNum] = EOS_TRUE;
	F[subTableNum] = fVals[subTableNum*nXYPairs + i];
      }

      /* find the largest fVals subTable index */
      i_max1 = _eos_maxloc(F, NULL, me->NP);

      mask[i_max1] = EOS_FALSE;

      /* find the second-largest fVals subTable index */
      i_max2 = _eos_maxloc(F, mask, me->NP);

      /* reset all other values to ZERO */
      for (subTableNum = 0; subTableNum < me->NP; subTableNum++) {
	if (subTableNum != i_max1 && subTableNum != i_max2)
	  F[subTableNum] = ZERO;
      }

      if (F[i_max2] <= EOS_MIN_MASS_FRACTION) /* impose a minumum value */
	F[i_max1] = ONE;

      F[i_max2] = ONE - F[i_max1]; /* impose a summation of ONE */

      /* store adjusted values */
      for (subTableNum = 0; subTableNum < me->NP; subTableNum++)
	fVals[subTableNum*nXYPairs + i] = F[subTableNum];

    }

    EOS_FREE(mask);
    EOS_FREE(F);

  }

  return;
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
void eos_InterpolateRecordType6 (void *ptr, EOS_INTEGER th, EOS_INTEGER dataType,
                                 EOS_INTEGER nXYPairs, EOS_REAL *srchX,
                                 EOS_REAL *srchY, EOS_REAL *fVals,
                                 EOS_REAL *dFx, EOS_REAL *dFy,
                                 EOS_INTEGER *xyBounds,
                                 EOS_INTEGER *errorCode)
{

  EOS_INTEGER numPhases = 1, subTableNum;
  EOS_REAL *F, *dFx_ = dFx, *dFy_ = dFy;
  eos_RecordType6 *me;
  EOS_CHAR *errMsg = NULL;

  *errorCode = EOS_OK;

  me = (eos_RecordType6 *) ptr;

  if (dataType == EOS_M_DT) {

    /* The sizes of srchX, srchY, fVals, dFx, and dFy are NOT equal so new logic is required. */

    /* fVals is allocated to contain nXYPairs*numPhases values; therefore,
       interpolate for each material phase prior to applying data constraints.
     */
    numPhases = _eos_GetNumberOfPhases(ptr, errorCode, &errMsg);
    if (errMsg) *errorCode = eos_SetCustomErrorMsg(th, *errorCode, "%s", errMsg);
    EOS_FREE(errMsg);

    /* no derivatives are returned to the host code for EOS_M_DT,
       so temporary arrays are allocated here to simplify integration with
       existing interpolation functions. */
    dFx_ = (EOS_REAL *) malloc(nXYPairs * sizeof(EOS_REAL));
    dFy_ = (EOS_REAL *) malloc(nXYPairs * sizeof(EOS_REAL));

    for (subTableNum = 1; subTableNum <= numPhases; subTableNum++) {

      F = &(fVals[(subTableNum-1)*nXYPairs]);
      _eos_InterpolateRecordType6 (ptr, th, subTableNum, me->eosData.varOrder, dataType, nXYPairs,
				   srchX, srchY, F, dFx_, dFy_, xyBounds, errorCode);

    }

    /* apply data constraints to interpolated values. */
    eos_ApplyMassFracConstraintsRecordType6 (ptr, th, dataType, nXYPairs,
					     fVals, errorCode);

    /* free temporary arrays */
    EOS_FREE(dFx_);
    EOS_FREE(dFy_);

  } /*end if (dataType == EOS_M_DT)*/

}

/* eos_CheckExtrap
If the EOS_INTERP_EXTRAPOLATED error code is returned by either eos_Interp or eos_Mix, this routine allows the user 
to determine which (x,y) pairs caused extrapolation and in which direction (high or low), it occurred.
The input arguments are
xVals	This is an array of the primary independent variable values to use during interpolation. There are nXYPairs elements in xVals.
yVals	This is an array of the secondary independent variable values to use during interpolation. There are nXYPairs elements in yVals.

The output arguments are 
xyBounds	This is an array of size nXYPairs elements that returns EOS_OK if extrapolation did not occur. If extrapolation occurred the variable and direction are determined from Table 2.
errorCode	This is a scalar EOS_INTEGER to contain an error code.

In the case that eos_Mix returned EOS_INTERP_EXTRAPOLATED as an error code, an additional series of steps must be performed to determine which tableHandle(s) correspond to the extrapolation error:
1.	For each tableHandle sent to eos_Mix, call eos_GetErrorCode and, optionally, eos_GetErrorMessage.
2.	For each of these tableHandles, call eos_CheckExtrap to determine one of codes listed in Table 2.
Table 2.	Extrapolation return codes.
Code	Definition
EOS_OK	No extrapolation occurred.
EOS_xHi_yHi	Both the x and y arguments were high.
EOS_xHi_yOk	The x argument was high, the y argument was OK.
EOS_xHi_yLo	The x argument was high, the y argument was low.
EOS_xOk_yLo	The x argument is OK and the y argument is low.
EOS_xLo_yLo	Both the x and y arguments were low.
EOS_xLo_yOk	The x argument was low, the y argument was OK.
EOS_xLo_yHi	The x argument was low, the y argument was OK.
   EOS_xOk_yHi	The x argument is OK and the y argument is high.
 */
void eos_CheckExtrapRecordType6 (void *ptr, EOS_INTEGER th, EOS_INTEGER dataType,
                                 EOS_INTEGER nXYPairs, EOS_REAL *srchX,
                                 EOS_REAL *srchY, EOS_INTEGER *xyBounds,
                                 EOS_INTEGER *errorCode)
{
  eos_Data *eosData;
  EOS_INTEGER i, nX, nY, cat, subTableNum;
  EOS_REAL *X, *Y, **F;
  EOS_INTEGER err = EOS_OK;
  EOS_REAL *xVals, *yVals;
  eos_RecordType6 *me;
  EOS_BOOLEAN isOneDimDatatype = EOS_FALSE;

  *errorCode = EOS_OK;

  eosData = (eos_Data *) ptr;
  me = (eos_RecordType6 *) eosData;
  cat = EOS_CATEGORY (dataType);
  subTableNum = EOS_TYPE_TO_SUB_TAB_NUM (dataType);
  /* get the size of the data */
  eos_GetSizeRecordType6 (me, &nX, &nY);

  /* make sure the data is record type 6 */
  if (EOS_TYPE_TO_RECORD_TYPE (dataType) != EOS_RECORD_TYPE6) {
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

  _eos_GetDataRecordType6 (me, &X, &Y, &F, subTableNum);

  xVals = srchX;
  yVals = srchY;


  switch (cat) {
  case EOS_CATEGORY0:          /* indicates the table is not inverted */
    {
      for (i = 0; i < nXYPairs; i++) {
        if (! isOneDimDatatype && yVals[i] < Y[0]) {
          if (xVals[i] < X[0])
            xyBounds[i] = EOS_xLo_yLo;
          else if (xVals[i] > X[nX - 1])
            xyBounds[i] = EOS_xHi_yLo;
          else
            xyBounds[i] = EOS_xOk_yLo;
        }
        else if (! isOneDimDatatype && yVals[i] > Y[nY - 1]) {
          if (xVals[i] < X[0])
            xyBounds[i] = EOS_xLo_yHi;
          else if (xVals[i] > X[nX - 1])
            xyBounds[i] = EOS_xHi_yHi;
          else
            xyBounds[i] = EOS_xOk_yHi;
        }
        else {                  /* y is either OK or not considered */

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
  default:
    {
      err = EOS_INVALID_TABLE_HANDLE;
      break;
    }
  }                             /* switch statement */


  if (srchX != xVals)
    EOS_FREE (xVals);
  if (srchY != yVals)
    EOS_FREE (yVals);

  if (eos_GetStandardErrorCodeFromCustomErrorCode(err) != EOS_OK) {
    ((eos_ErrorHandler *) me)->HandleError (me, th, err);
    *errorCode = err;
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
 * void *ptr                - this pointer (pointer to the instanse of type eos_RecordType6
 * EOS_INTEGER numInfoItems - # of requested items
 * EOS_INTEGER *infoItems   - array of requested items
 * EOS_REAL *infoVals       - return item values
 * EOS_INTEGER th           - table handle
 * EOS_INTEGER *err         - error flag
 * 
 ************************************************************************/
void eos_GetTableInfoRecordType6 (void *ptr, EOS_INTEGER th,
                                  EOS_INTEGER numInfoItems,
                                  EOS_INTEGER *infoItems, EOS_REAL *infoVals,
                                  EOS_INTEGER *err)
{
  EOS_INTEGER i, j, k, subTable;
  EOS_REAL **F, xconv = ONE, yconv = ONE, fconv = ONE;
  eos_RecordType6 *me;
  me = (eos_RecordType6 *) ptr;
  *err = EOS_OK;


  if (EOS_ALLOW_CAT0_ONLY_INFO_ITEM (infoItems[0]) &&
      EOS_CATEGORY (eos_GetDataTypeFromTableHandle (th, err)) != 0) {
    /* return error */
    *err = EOS_INVALID_INFO_FLAG;
    *err = eos_SetCustomErrorMsg(th, *err,
				 "Invalid info flag. The info flag, %d, is only valid for simple, non-inverted data types.",
				 infoItems[0]);
    return;
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

  if (infoItems[0] == EOS_F_Array) {
    if (numInfoItems < me->NR * me->NT * me->NP) {
      *err = EOS_FAILED;
      *err = eos_SetCustomErrorMsg(th, *err,
				   "Operation failed. Insufficient memory allocation assumed because numInfoItems<NR*NT*NP.");
      return;
    }
    i = 0;
    for (subTable = 1; subTable <= me->NP; subTable++) {
      //F = me->table[subTable-1];
      for (k = 0; k < me->NT; k++)
        for (j = 0; j < me->NR; j++) 
          infoVals[i++] = me->table[subTable-1][k][j] * fconv;
    }
    return;
  }

  for (i = 0; i < numInfoItems; i++) {

    if (EOS_ALLOW_CAT0_ONLY_INFO_ITEM (infoItems[i]) &&
        EOS_CATEGORY (eos_GetDataTypeFromTableHandle (th, err)) != 0) {
      /* return error */
      *err = EOS_INVALID_INFO_FLAG;
      *err = eos_SetCustomErrorMsg(th, *err,
				   "Invalid info flag. The info flag, %d, is only valid for simple, non-inverted data types.",
				   infoItems[i]);
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
      for (subTable = 1; subTable <= me->NP; subTable++) {
        F = me->table[subTable-1];
        infoVals[i] = F[0][0];
        for (j = 0; j < me->NR; j++) {
          for (k = 0; k < me->NT; k++)
            if (F[k][j] < infoVals[i])
              infoVals[i] = F[k][j];
        }
      }
      break;

    case EOS_Fmax:
      for (subTable = 1; subTable <= me->NP; subTable++) {
        F = me->table[subTable-1];
        infoVals[i] = F[0][0];
        for (j = 0; j < me->NR; j++) {
          for (k = 0; k < me->NT; k++)
            if (F[k][j] < infoVals[i])
              infoVals[i] = F[k][j];
        }
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

    case EOS_NUM_PHASES:
      infoVals[i] = (EOS_REAL) me->NP;
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
    case EOS_T_Array:
    case EOS_F_Array:
      *err = EOS_INVALID_INFO_FLAG;
      *err = eos_SetCustomErrorMsg(th, *err,
				   "Invalid info flag because EOS_R_Array and EOS_T_Array may only be used without any other info flags.");

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
 * void *ptr           - this pointer (pointer to the instance of type eos_RecordType6
 * EOS_CHAR *infoItem  - flag specifying what meta data item to fetch
 * 
 ************************************************************************/
void eos_GetTableMetaDataRecordType6 (void *ptr, EOS_INTEGER infoItem,
				      EOS_CHAR *infoStr, EOS_INTEGER *err)
{
  //eos_RecordType6 *me;

  //me = (eos_RecordType6 *) ptr;
  *err = EOS_INVALID_DATA_TYPE; /* 101 table not found */
}

 /*************************************************************************
 *
 * Function eos_GetLoadedBulkDataRecordType6
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
 * eos_RecordType6 *me
 *************************************************************************/
void eos_GetLoadedBulkDataRecordType6 (void *ptr, EOS_REAL *zbar,
                                       EOS_REAL *abar, EOS_REAL *dens0,
                                       EOS_INTEGER *errorCode)
{
  eos_RecordType6 *me;
  me = (eos_RecordType6 *) ptr;
  *errorCode = EOS_OK;

  *zbar = me->avgAtomicNumber;
  *abar = me->avgAtomicWgt;
  *dens0 = me->refDensity;
}


/************************************************************************
 * 
 * Makes data of class eos_RecordType6 monotonic
 * 
 * Returned Values: EOS_INTEGER *err - output error code
 *
 * Input Value:
 * void *ptr       - this pointer (pointer to the instance of type eos_RecordType6
 * EOS_INTEGER th  - table handle for error handling
 * EOS_INTEGER dataType - data type of subtable to be made monotonic
 * EOS_BOOLEAN inX, inY - to make monotonic in x, in y, or both
 * EOS_INTEGER *err   - error flag
 * 
 ************************************************************************/
void eos_MakeMonotonicRecordType6 (void *ptr, EOS_INTEGER th,
                                   EOS_INTEGER dataType, EOS_BOOLEAN inX,
                                   EOS_BOOLEAN inY, EOS_INTEGER *err)
{
  eos_RecordType6 *me;

  me = (eos_RecordType6 *) ptr;
  *err = EOS_OK;

  if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) != EOS_OK) {
    ((eos_ErrorHandler *) me)->HandleError (me, th, *err);
    return;
  }
}


/************************************************************************
 * 
 * checks if the data of class eos_RecordType6 is monotonic-increasing or
 * monotonic-decreasing
 * 
 * Returned Values: EOS_INTEGER *err - output error code
 *
 * Input Value:
 * void *ptr       - this pointer (pointer to the instance of type eos_RecordType6
 * EOS_BOOLEAN *isMonotonic - result
 * EOS_INTEGER *err
 * 
 ************************************************************************/
void eos_IsMonotonicRecordType6 (void *ptr, EOS_INTEGER dataType,
                                 EOS_BOOLEAN *isMonotonic, EOS_BOOLEAN inX,
                                 EOS_BOOLEAN inY, EOS_INTEGER *err)
{
  /* do nothing here since monotonicity is not required for this class of data */
  *err = EOS_OK;

  *isMonotonic = EOS_FALSE;

  return;
}


/************************************************************************
 * 
 * Makes data of class eos_RecordType6 smooth
 * 
 * Returned Values: EOS_INTEGER *err - output error code
 *
 * Input Value:
 * void *ptr       - this pointer (pointer to the instance of type eos_RecordType6
 * EOS_INTEGER *err   - error flag
 * 
 ************************************************************************/
void eos_MakeSmoothRecordType6 (void *ptr, EOS_INTEGER th,
                                EOS_INTEGER dataType, EOS_BOOLEAN ptSmooth,
                                EOS_INTEGER *err)
{
  /* do nothing here since smoothing is not required for this class of data */
  *err = EOS_OK;
}


/*************************************************************************
 *
 * Function eos_GetMonotonicityRecordType6
 * Description:
 * 
 * set the flags so that appropriate data will be made monotonic upon loading
 * 
 * Returned Values:
 *
 * Input Value:
 * eos_RecordType6 *me
 * EOS_INTEGER dataType
 * EOS_INTEGER *inX
 * EOS_INTEGER *inY
 *************************************************************************/
void eos_GetMonotonicityRecordType6 (void *ptr, EOS_INTEGER dataType,
                                     EOS_INTEGER *inX, EOS_INTEGER *inY)
{
  /* do nothing here since monotonicity is not required for this class of data */
  *inX = EOS_FALSE;
  *inY = EOS_FALSE;
}


 /*************************************************************************
 *
 * Function eos_SetMonotonicityRecordType6
 * Description:
 * 
 * set the flags so that appropriate data will be made monotonic upon loading
 * 
 * Returned Values:
 *
 * Input Value:
 * eos_RecordType6 *me
 * EOS_INTEGER dataType
 * EOS_INTEGER inX
 * EOS_INTEGER inY
 *************************************************************************/
void eos_SetMonotonicityRecordType6 (void *ptr, EOS_INTEGER dataType,
                                     EOS_INTEGER inX, EOS_INTEGER inY)
{
  /* do nothing here since monotonicity is not required for this class of data */
  return;
}


/*************************************************************************
 *
 * Function eos_GetSmoothingRecordType6
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
void eos_GetSmoothingRecordType6 (void *me, EOS_INTEGER dataType,
                                  EOS_INTEGER *isSmooth,
                                  EOS_INTEGER *isPtSmooth)
{
  /* do nothing here since smoothing is not required for this class of data */
  *isSmooth = 0;
  *isPtSmooth = 0;
}


/*************************************************************************
 *
 * Function eos_AreMonotonicRequirementsCompatibleRecordType6
 * Description:
 * 
 * find out if the requested type, monotonicity combo can share our table object
 * 
 * Returned Values:
 * EOS_BOOLEAN *compatible
 *
 * Input Value:
 * eos_RecordType6 *me
 * EOS_INTEGER dataType
 * EOS_INTEGER inX
 * EOS_INTEGER inY
 *************************************************************************/
void eos_AreMonotonicRequirementsCompatibleRecordType6 (void *ptr,
                                                        EOS_INTEGER dataType,
                                                        EOS_INTEGER inX,
                                                        EOS_INTEGER inY,
                                                        EOS_BOOLEAN
                                                        *compatible)
{
  /* do nothing here since monotonicity is not required for this class of data */
  *compatible = EOS_TRUE;
}


 /*************************************************************************
 *
 * Function eos_SetSmoothingDataRecordType6
 * Description:
 * 
 * set the flags so that appropriate data will be made smooth upon loading
 * 
 * Returned Values:
 *
 * Input Value:
 * eos_RecordType6 *me
 * EOS_INTEGER dataType
 * EOS_INTEGER makeSmooth
 * EOS_INTEGER makePtSmooth
 *************************************************************************/
void eos_SetSmoothingRecordType6 (void *me, EOS_INTEGER dataType,
                                  EOS_INTEGER makeSmooth,
                                  EOS_INTEGER makePtSmooth)
{
  /* do nothing here since smoothing is not required for this class of data */
}


/*************************************************************************
 *
 * Function eos_AreSmoothingRequirementsCompatibleRecordType6
 * Description:
 * 
 * find out if the requested type, monotonicity combo can share our table object
 * 
 * Returned Values:
 * EOS_BOOLEAN *compatible
 *
 * Input Value:
 * eos_RecordType6 *me
 * EOS_INTEGER dataType
 * EOS_INTEGER makeSmooth
 * EOS_INTEGER makePtSmooth
 *************************************************************************/
void eos_AreSmoothingRequirementsCompatibleRecordType6 (void *me,
                                                        EOS_INTEGER dataType,
                                                        EOS_INTEGER
                                                        makeSmooth,
                                                        EOS_INTEGER
                                                        makePtSmooth,
                                                        EOS_BOOLEAN
                                                        *compatible)
{
  /* do nothing here since smoothing is not required for this class of data */
  *compatible = EOS_TRUE;
}
