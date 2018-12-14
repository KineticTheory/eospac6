/*********************************************************************
 * Class Name : eos_DataMap
 * ---------------------------------------------------------
 * Filetype: (SOURCE)
 *
 * Copyright -- see file named COPYRIGHTNOTICE
 *
 *********************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define _EOS_DATAMAP_INTERNAL_PROTOTYPES
#include "eos_types_internal.h"
#include "eos_DataMap.h"

#include "eos_Utils.h"
#include "eos_SesUtils.h"

/************************************************************************
 * 
 * RecordType1 class constructor
 * 
 * Returned Values: none
 *
 * Input Value:
 * eos_DataMap *me         - this pointer (pointer to the instance of type eos_DataMap
 * 
 ************************************************************************/
void eos_ConstructEosDataMap (eos_DataMap *me)
{
  EOS_INTEGER i;

  me->nAlloc = 0;
  me->nTables = 0;
  me->nHandles = 0;
  me->dataObjects = NULL;
  me->tableHandlesMap = NULL;
  me->tableTypes = NULL;
  me->errorCodes = NULL;
  me->customErrorMsg = NULL;
  for (i = 0; i < EOS_NUM_GENERAL_OPTIONS; i++)
    me->generalOptions[i] = NULL;
  eos_ConstructEosErrorHandler ((eos_ErrorHandler *) me);
  me->eosErrorHandler.HandleError = eos_HandleErrorEosDataMap;  /* derived virtual function */
}

/************************************************************************
 * 
 * DataMap class destructor
 * 
 * Returned Values: none
 *
 * Input Value:
 * eos_DataMap *me  - this pointer (pointer to the instance of type eos_DataMap
 * 
 ************************************************************************/
void eos_DestroyEosDataMap (eos_DataMap *me)
{
  EOS_INTEGER i;

  for (i = 0; i < me->nAlloc; i++) {
    if (!me->dataObjects[i])
      continue;
    eos_DestroyEosData (me->dataObjects[i]);
    EOS_FREE(me->dataObjects[i]); /* perform unprotected deallocation,
				     which ignores (me->dataObjects[i])->refCounter */
  }

  eos_ResetAllCustomErrorMsg();

  for (i = 0; i < me->nAlloc; i++)
    EOS_FREE(gEosDataMap.customErrorMsg[i]);
  EOS_FREE(gEosDataMap.customErrorMsg);

  me->nAlloc = 0;
  me->nTables = 0;
  me->nHandles = 0;
  EOS_FREE (me->dataObjects);
  EOS_FREE (me->tableHandlesMap);
  EOS_FREE (me->tableTypes);
  EOS_FREE (me->errorCodes);
  EOS_FREE (me->isHandlePublic);
  for (i = 0; i < EOS_NUM_GENERAL_OPTIONS; i++)
    EOS_FREE (me->generalOptions[i]);

  eos_DestroyEosErrorHandler (&(me->eosErrorHandler));
}


/************************************************************************
 * 
 * Destroys specified table handles
 * 
 * Returned Values: none
 *
 * Input Value:
 * eos_DataMap *me  - this pointer (pointer to the instance of type eos_DataMap
 * 
 ************************************************************************/
void eos_DestroyTablesEosDataMap (eos_DataMap *me, EOS_INTEGER nTables,
                                  EOS_INTEGER tableHandles[],
                                  EOS_INTEGER *errorCode)
{
  EOS_INTEGER i, index;
  *errorCode = EOS_OK;

  for (i = 0; i < nTables; i++) {
    if (!eos_IsHandleValidEosDataMap (me, tableHandles[i])) {
      /*
         eos_HandleErrorEosDataMap(me, tableHandles[i], EOS_INVALID_TABLE_HANDLE);
         *errorCode = EOS_INVALID_TABLE_HANDLE;
       */
      continue;                 /* in case there are identical handles */
    }

    index = me->tableHandlesMap[tableHandles[i]];
    me->dataObjects[index]->refCounter--;
    me->nHandles--;
    /* check if there are any other handles pointing to this table, if there are, don't delete it */
    if (me->dataObjects[index]->refCounter > 0) {
      eos_InvalidateHandle (tableHandles[i]);
      me->errorCodes[tableHandles[i]] = EOS_INVALID_TABLE_HANDLE;       /* reflect status of handle */
      continue;
    }
    eos_DestroyEosData (me->dataObjects[index]);
    eos_FreeEosData (&(me->dataObjects[index]),
                     me->dataObjects[index]->recordType);

    eos_ResetCustomErrorMsg(tableHandles[i]);
    EOS_FREE(me->customErrorMsg[tableHandles[i]]);

    eos_InvalidateHandle (tableHandles[i]);
    me->errorCodes[tableHandles[i]] = EOS_INVALID_TABLE_HANDLE; /* reflect status of handle */
    me->nTables--;
  }
}


/************************************************************************
 * 
 * Cleans up duplicate tables which can be re-used
 * 
 * Returned Values: none
 *
 * Input Value:
 * eos_DataMap *me  - this pointer (pointer to the instance of type eos_DataMap
 * 
 ************************************************************************/
void _eos_CleanupTablesEosDataMap (eos_DataMap *me, EOS_INTEGER *errorCode)
{
  eos_Data *eosData, *newEosData;
  EOS_INTEGER c, i, j;
  *errorCode = EOS_OK;

  for (i = 0; i < me->nAlloc; i++) {
    /* if this handle already reuses another object , skip it */
    if (!me->dataObjects[i] || ! eos_IsHandleValid(i))
      continue;

    /* if this handle uses an unshareable object , skip it */
    if (!me->dataObjects[i]->IsaShareableObject(me->dataObjects[i]))
      continue;

    /* check if we can re-use the existing object */
    for (j = i + 1; j < me->nAlloc; j++) {

      EOS_INTEGER tabNum1, tabNum2;

      /* if this handle already reuses another object , skip it */
      if (!me->dataObjects[j] || ! eos_IsHandleValid(i) || j == i)
        continue;

      /* if this handle uses an unshareable object , skip it */
      if (!me->dataObjects[j]->IsaShareableObject(me->dataObjects[j]))
	continue;

      tabNum1 = me->dataObjects[j]->tableNum;
      tabNum2 = EOS_TYPE_TO_TAB_NUM (me->tableTypes[i]);

      /*
       * Check the following:
       *    1. the tableNumbers are identical
       *    2. the material IDs are identical
       *    3. the options are compatible
       *    4. neither material ID is in the global gMatidMap[]
       *    5. the dataFileIndex values are identical
       *    6. the dataFileIndex values are not user defined
       * If yes to all conditions, then re-use the table!
       */
      if (tabNum1 == tabNum2 &&
	  me->dataObjects[i]->dataFileIndex == me->dataObjects[j]->dataFileIndex &&
	  _eos_find_userdefined_fileindex_in_gEosDataMapDataObjects(me->dataObjects[i]->dataFileIndex) < 0 &&
	  _eos_find_matid_in_gMatidMap(me->dataObjects[i]->materialID) < 0 &&
	  _eos_find_matid_in_gMatidMap(me->dataObjects[j]->materialID) < 0) {
	if (! me->dataObjects[i]->destructing)
	  if (me->dataObjects[i]->materialID == me->dataObjects[j]->materialID)
	    if (_eos_AreOptionsCompatibleEosData (me->dataObjects[j],
						  me->dataObjects[i],
						  me->tableTypes[j], -1,
						  NULL, NULL, 0)) {
	      eosData = me->dataObjects[j];
	      newEosData = me->dataObjects[i];
	      /* re-assign table mappings for other table handles pointing to this data */
	      c = 0;
	      while (eosData->refCounter > 1) {
		if (c != j && me->tableHandlesMap[c] == j) {
		  me->tableHandlesMap[c] = i;
		  newEosData->refCounter++;   /* increase the counter for the new one */
		  eosData->refCounter--;      /* decrease the refcount of what it used to point to */
		}
		c++;
	      }

	      /* now only we point to j-th object */
	      eos_DestroyEosData (me->dataObjects[j]);
	      eos_FreeEosData (&(me->dataObjects[j]), me->dataObjects[j]->recordType);
	      me->nTables--;
	      newEosData->refCounter++;       /* increase the counter for the new one */
	      me->tableHandlesMap[j] = i;
	    }
      }
    }                           /* j-loop */
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
void eos_HandleErrorEosDataMap (void *ptr, EOS_INTEGER th,
                                EOS_INTEGER errorCode)
{
  eos_DataMap *me = (eos_DataMap *) ptr;
  EOS_CHAR* cErrMsg = NULL;
  EOS_INTEGER pErr;

  pErr = me->errorCodes[th];

  if (!eos_IsHandleValidEosDataMap (me, th)) {

    EOS_CHAR errorMsg0[EOS_MaxErrMsgLen]=" ";
    EOS_CHAR errorMsg1[EOS_MaxErrMsgLen]=" ";
    EOS_CHAR because[EOS_MaxErrMsgLen]=" because ";
    EOS_INTEGER len1, len2, err;

    /* fetch message of last error or use default message */
    if (eos_GetStandardErrorCodeFromCustomErrorCode(pErr) != EOS_OK)
      sprintf (errorMsg0, "%s", eos_GetErrorMsg (pErr));
    else
      sprintf (errorMsg0, "an unknown error has occurred");

    /* fetch message of last error */
    sprintf (errorMsg1, "%s", eos_GetErrorMsg (EOS_INVALID_TABLE_HANDLE));

    if (pErr == EOS_OK) {
      /* do nothing here, because the reason for invalid handle has been forgotten */
    }
    else {
      /* customize last error message*/
      len1 = strlen(errorMsg1);
      len2 = strlen(because);
      strncat(errorMsg1, because, EOS_MaxErrMsgLen-1-len2);
      len1 = strlen(errorMsg1);
      if (strstr(errorMsg0, errorMsg1)    /* errorMsg1 exists in errorMsg0  */
	  ) {
	strcpy(errorMsg1, errorMsg0);     /* replace errorMsg1 */
      }
      else { /* errorMsg1 not in errorMsg0; concatenate errorMsg1 and errorMsg0 */
	strncat(errorMsg1, errorMsg0, EOS_MaxErrMsgLen-len1);
      }
    }

    err = eos_SetCustomErrorMsg (th, EOS_INVALID_TABLE_HANDLE, errorMsg1);
    me->eosErrorHandler.errorCode = err;
  }
  else {
    //me->eosErrorHandler.errorCode = errorCode;
    me->eosErrorHandler.errorCode = eos_GetStandardErrorCodeFromCustomErrorCode(errorCode);
    me->eosErrorHandler.errorCode = eos_SetCustomErrorCode(th, me->eosErrorHandler.errorCode);
  }

  cErrMsg = eos_GetCustomErrorMsg(th, errorCode);

  if (! gEosDataMap.errorCodes) return;
  if (pErr >= 0 && strlen(cErrMsg) > 0) {
    gEosDataMap.errorCodes[th] = me->eosErrorHandler.errorCode;
  }
  else {
    gEosDataMap.errorCodes[th] = errorCode;
  }
}

/************************************************************************
 * 
 * eos_ShouldBeMonotonic(eos_Data *eosData, EOS_INTEGER dataType, EOS_INTEGER *inX, EOS_INTEGER *inY)
 * if the dataType implies inverted table, or is combined, and one of the references inverted,
 * return that the table should be made monotonic. Calls itself recurvively
 * EOS_INTEGER *dataType1, EOS_INTEGER *dataType2 - are output data types which need to be made monotonic
 * there are 2 of them to accomodate category 3 nd 4 types, where one or both subtypes have to be monotonic.
 ************************************************************************/
void eos_ShouldBeMonotonic (eos_Data *eosData, EOS_INTEGER dataType,
                            EOS_INTEGER *inX1, EOS_INTEGER *inY1,
                            EOS_INTEGER *inX2, EOS_INTEGER *inY2,
                            EOS_INTEGER *dataType1, EOS_INTEGER *dataType2)
{
  EOS_INTEGER cat, ind1, ind2, dt1, dt2;
  EOS_INTEGER makeSmooth, pt_smooth;

  cat = EOS_CATEGORY (dataType);
  *inX1 = *inY1 = *inX2 = *inY2 = EOS_FALSE;
  *dataType1 = *dataType2 = -1;

  eosData->GetSmoothing (eosData, dataType, &makeSmooth, &pt_smooth);
  if (pt_smooth)                /* don't eforce any monotonicity if pt smooth */
    return;

  if (cat == 0)
    return;
  else if (cat == 1) {
    *inX1 = EOS_TRUE;
    *dataType1 = dataType;
  }
  else if (cat == 2) {
    *inY1 = EOS_TRUE;
    *dataType1 = dataType;
  }
  else if (cat == 3) {
    /* get the data pointers and types for these valuables */
/*      F(G,t) = F(r(G, t) t), when does F need to be monotonic wrt any var? 
        dF/dG(t const) = dF/dr (G cost) * dr/dG (t const)
		check if G needs to be monotonic wrt to t */
    ind1 = EOS_EOS_TABLE_TYPE_REF1 (dataType);  /* F */
    ind2 = EOS_EOS_TABLE_TYPE_REF2 (dataType);  /* G */
    eos_ShouldBeMonotonic (eosData, ind1, inX1, inY1, inX1, inY1, &dt1, &dt2);
    eos_ShouldBeMonotonic (eosData, ind2, inX2, inY2, inX2, inY2, &dt1, &dt2);
    if (*inX1 || *inY1)
      *dataType1 = ind1;
    if (*inX2 || *inY2)
      *dataType2 = ind2;
  }
  else if (cat == 4) {
    /* get the data pointers and types for these valuables */
/*      F(r,G) = F(r, T(r, G)),  when does F need to be monotonic wrt any var? 
        dF/dG(t const) = dF/dr (G cost) * dr/dG (t const)
		check if T needs to be monotonic wrt to any var */
    ind1 = EOS_EOS_TABLE_TYPE_REF1 (dataType);
    ind2 = EOS_EOS_TABLE_TYPE_REF2 (dataType);
    eos_ShouldBeMonotonic (eosData, ind1, inX1, inY1, inX1, inY1, &dt1, &dt2);
    eos_ShouldBeMonotonic (eosData, ind2, inX2, inY2, inX2, inY2, &dt1, &dt2);
    if (*inX1 || *inY1)
      *dataType1 = ind1;
    if (*inX2 || *inY2)
      *dataType2 = ind2;
  }

}

/************************************************************************
 * 
 * eos_CanBeMadeMonotonicInX(EOS_INTEGER dataType, EOS_INTEGER *err)
 * Check if the tabe CAN be made monotonic wrt X. returns EOS_CANT_MAKE_MONOTONIC
 * or EOS_CANT_INVERT_DATA Calls itself recurvively
 * ASSUME that either the user has explicitly requested monotonicity wrt x,
 * or we have decided that the table needs to be monotonic because
 * it is (or one of its components is) inverted.
 ************************************************************************/
void eos_CanBeMadeMonotonicInX (eos_Data *eosData, EOS_INTEGER dataType,
                                EOS_INTEGER *err)
{
  EOS_INTEGER rt, cat, ind1, ind2;
  EOS_INTEGER firstVar, depVar;
  EOS_BOOLEAN compatible = EOS_TRUE;
  *err = EOS_OK;

  cat = EOS_CATEGORY (dataType);
  rt = EOS_TYPE_TO_RECORD_TYPE (dataType);
  firstVar = EOS_TYPE_TO_INDEP_VAR1 (dataType);
  depVar = EOS_TYPE_TO_DEP_VAR (dataType);

  if (rt != 1) {
    *err = EOS_CANT_MAKE_MONOTONIC;
    return;
  }

  /*
    EOS_Pc  -- Pressure Cold Curve (GPa)
    EOS_Pe  -- Electron Pressure (GPa)
    EOS_Pf  -- Freeze Pressure (GPa)
    EOS_Piz -- Ion Pressure Including Zero Point (GPa)
    EOS_Pic -- Ion Pressure plus Cold Curve Pressure (GPa)
    EOS_Pm  -- Melt Pressure (GPa)
    EOS_Pt  -- Total Pressure (GPa)
    EOS_Pv  -- Vapor Pressure (GPa)
  */
  if (cat == 0) {
    if (firstVar == EOS_D && ! EOS_IS_PRESSURE_VARIABLE(depVar)) {
/*         depVar != EOS_Pt && depVar != EOS_Pic && depVar != EOS_Pe */
/*         && depVar != EOS_Pc) { */
      *err = EOS_CANT_MAKE_MONOTONIC;
      return;
    }
  }
  else if (cat == 1)            // needs to be monotonic in X */
  {
    if (depVar == EOS_D && ! EOS_IS_PRESSURE_VARIABLE(firstVar)) {
/*         firstVar != EOS_Pt && firstVar != EOS_Pic && firstVar != EOS_Pe */
/*         && firstVar != EOS_Pc) { */
      *err = EOS_CANT_INVERT_DATA;
      return;
    }
  }
  else if (cat == 2)
    return;
  else if (cat == 3) {
/*      F(G,t) = F(r(G, t) t),  when if F monotonic wrt G? 
        dF/dG(t const) = dF/dr (G const) * dr/dG (t const)
		so F(G,t) is monotonic wrt to G when F if monotonic wrt r, and G is monotonic wrt to r */
    ind1 = EOS_EOS_TABLE_TYPE_REF1 (dataType);  /* F */
    /* call eosData to check if this type need to be monotonic in X for this table
       (eosData stores info which subtables need to be monotonic) */
    eosData->AreMonotonicRequirementsCompatible (eosData, ind1, 1, -1,
                                                 &compatible);
    if (compatible)
      eos_CanBeMadeMonotonicInX (eosData, ind1, err);
    if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) == EOS_OK) {
      ind2 = EOS_EOS_TABLE_TYPE_REF2 (dataType);        /* G */
      /* call eosData to check if this type need to be monotonic in X for this table
         (eosData stores info which subtables need to be monotonic) */
      eosData->AreMonotonicRequirementsCompatible (eosData, ind2, 1, -1,
                                                   &compatible);
      if (compatible)
        eos_CanBeMadeMonotonicInX (eosData, ind2, err);
    }
  }
  else if (cat == 4) {
    /* F(r,G) = F(r, T(r, G)), F is monotonic wrt to r indep of T, G  */
    /* check the component functions can be made monotonic in X, 
       if they should, recuse and check if they CAN be monotonic */
    ind1 = EOS_EOS_TABLE_TYPE_REF1 (dataType);  /* F */
    /* call eosData to check if this type need to be monotonic in X for this table
       (eosData stores info which subtables need to be monotonic) */
    eosData->AreMonotonicRequirementsCompatible (eosData, ind1, 1, -1,
                                                 &compatible);
    if (compatible)
      eos_CanBeMadeMonotonicInX (eosData, ind1, err);
  }
}

/*************************************************************************
 *
 * Function Name: _eos_LoadEosData
 *
 * Description:
 * helping function which actually loads the eosData, but first checks if
 * another table number should be loaded to get the data from.
 *
 * Parameters:
 * eos_Data *me  - this pointer (pointer to the instance of type eos_DataMap
 * returns error code.
 *
 *************************************************************************/
EOS_INTEGER _eos_LoadEosData (eos_Data *eosData, EOS_INTEGER th)
{
  EOS_INTEGER tableNums[3] = { 301, 303, 306 }, j, c;

  /* handle the cases where the 306 table is requested */
  if (eosData->tableNum == 306) {
    c = EOS_OK;
    /* first try 301 table, then 303, then 306 if the other ones don't work */
    for (j = 0; j < 3; j++) {
      eosData->tableNum = tableNums[j];
      eosData->forceCreate = EOS_TRUE;
      eosData->Create (eosData, th);
      eosData->forceCreate = EOS_FALSE;
      c = eos_GetErrorCodeEosDataMap (&gEosDataMap, th);
      if (eos_GetStandardErrorCodeFromCustomErrorCode(c) == EOS_OK) {
        eosData->Load (eosData, th);
        c = eos_GetErrorCodeEosDataMap (&gEosDataMap, th);
      }

      if (eos_GetStandardErrorCodeFromCustomErrorCode(c) == EOS_OK) /* data was found */
        break;
      else if (j < 2)  /* reset error code for th, and try next Sesame table */
        eos_HandleErrorEosDataMap (&gEosDataMap, th, EOS_OK);
    }

    /* if 301 or 303 is loaded, do a cleanup: delete all data other than the cold curve */
    if (eos_GetStandardErrorCodeFromCustomErrorCode(c) == EOS_OK && j < 2)
      eos_CleanUpColdCurveRecordType1 (eosData, &c);    /* resets eosData->tableNum to 306 */
  }
  else {                        /* the requested table number is not 306 */

    eosData->Load (eosData, th);
    c = eos_GetErrorCodeEosDataMap (&gEosDataMap, th);
  }

  return c;
}

/*************************************************************************
 *
 * Function Name: eos_LoadTablesEosDataMap
 *
 * Description:
 * The eos_LoadTables function creates a collection of data tables and
 * fills them with the requested data tables from SESAME.
 * Before calling this routine the host code may need to call eos_SetOption
 * so the desired set up options can be changed from the documented
 * defaults.
 *
 * Parameters:
 * eos_DataMap *me  - this pointer (pointer to the instance of type eos_DataMap
 * EOS_INTEGER nTables
 * EOS_INTEGER tableHandles[]
 * EOS_INTEGER errorCode[nAlloc] )
 *
 *************************************************************************/
void eos_LoadTablesEosDataMap (eos_DataMap *me, EOS_INTEGER nTables,
                               EOS_INTEGER tableHandles[],
                               EOS_INTEGER *errorCode)
{
  EOS_INTEGER i, c, combinedErr, makeMonotonicType1,
    makeMonotonicType2, dataType, reqNumSubTables;
  EOS_BOOLEAN isMonotonicX, isMonotonicY;
  EOS_INTEGER makeMonotonicInX, makeMonotonicInY, makeMonotonicInX1,
    makeMonotonicInY1, makeMonotonicInX2, makeMonotonicInY2;
  eos_Data *eosData;
  eos_OptionValue *optVal;
  extern EOS_BOOLEAN enable_DEBUG_PRINT;
  EOS_INTEGER dataObjectIndex;

  *errorCode = combinedErr = EOS_OK;

  /* first we want to clean up, make sure there are no duplicate tables
     the duplicates can appear  if tables have the same options which
     were set one by one */

  _eos_CleanupTablesEosDataMap (me, errorCode);

  for (i = 0; i < nTables; i++) {
    *errorCode = EOS_OK;
    if (!eos_IsHandleValidEosDataMap (me, tableHandles[i])) {
      combinedErr = EOS_INVALID_TABLE_HANDLE;
      eos_HandleErrorEosDataMap (me, tableHandles[i], combinedErr);
      continue;
    }
    if (eos_GetStandardErrorCodeFromCustomErrorCode(gEosDataMap.errorCodes[tableHandles[i]]) == EOS_MATERIAL_NOT_FOUND) {
      combinedErr = EOS_MATERIAL_NOT_FOUND;
      combinedErr = eos_SetCustomErrorMsg (tableHandles[i], combinedErr,
					   "EOS_MATERIAL_NOT_FOUND: Material ID specified for table handle %d is not in library",
					   tableHandles[i]);
      *errorCode = combinedErr;
      eos_HandleErrorEosDataMap (me, tableHandles[i], combinedErr);
      /* print it if requested */
      me->dataObjects[me->tableHandlesMap[tableHandles[i]]]->dumpNotLoadedMsg = EOS_TRUE;
      eos_PrintTableEosDataMap (me, tableHandles[i], "TablesLoaded.dat", &combinedErr);
      me->dataObjects[me->tableHandlesMap[tableHandles[i]]]->dumpNotLoadedMsg = EOS_FALSE;
      combinedErr = *errorCode;
      continue;
    }

    /* define eosData for tableHandles[i] */
    eosData = me->dataObjects[me->tableHandlesMap[tableHandles[i]]];

    /* fetch the dataType for tableHandles[i] */
    dataType = me->tableTypes[tableHandles[i]];

    /* fetch the reqNumSubTables for dataType */
    reqNumSubTables = eos_getRequiredNumSubtables (eosData, dataType);

    /* if EOS_USE_TAYLOR_FIT option is set, then reqNumSubTables=1 */
    eos_GetOptionEosDataMap (me, tableHandles[i], EOS_USE_TAYLOR_FIT, &optVal, errorCode);
    if (optVal && optVal->bval) reqNumSubTables = 1;

    /* get EOS_DEBUG_PRINT setting for this handle */
    eos_GetOptionEosDataMap (me, tableHandles[i], EOS_DEBUG_PRINT, &optVal, errorCode);
    enable_DEBUG_PRINT = (optVal) ? optVal->bval : EOS_FALSE;

    /* check if the monotonic option is set (this means either user has requested it, or
       we set it so that table can be inverted */
    optVal = _eos_getOptionEosData (eosData, EOS_MONOTONIC_IN_X);
    makeMonotonicInX = (optVal) ? optVal->bval : EOS_FALSE;
    optVal = _eos_getOptionEosData (eosData, EOS_MONOTONIC_IN_Y);
    makeMonotonicInY = (optVal) ? optVal->bval : EOS_FALSE;

    /* now check if it should be monotonic because the type, or one of it's components is inverted.
       get the subtypes that should be monotonic, and for each of them which vars need to be monotonic.
       NOTE: we had done this already when creating this tableHandle, because we needed to check
       which tables can be shared. We have stored which subtables need to be monotonic in eos_RT1 class,
       but we have lost the information regarding which data Types need to be monotonic */

    /* maybe do it just here, not when creating table? , then SetOption should be called here,
       and tables re-mapped. It's better to check if the types should be monotonic twice, since it's
       not expensive to check this. */

    eos_ShouldBeMonotonic (eosData, me->tableTypes[tableHandles[i]],
                           &makeMonotonicInX1, &makeMonotonicInY1,
                           &makeMonotonicInX2, &makeMonotonicInY2,
                           &makeMonotonicType1, &makeMonotonicType2);

    if (eosData->isLoaded > 0) {
      /* print it if requested */
      eos_PrintTableEosDataMap (me, tableHandles[i], "TablesLoaded.dat",
                                errorCode);
      if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) != EOS_OK) {
        combinedErr = *errorCode;
	((eos_ErrorHandler *) eosData)->HandleError (eosData, tableHandles[i], *errorCode);
        continue;
      }
      continue;
    }

    /* NOTE: if the table is inverted, the option is set at creation to make t monotonic  */

    /* check if there is EOS_MONOTONIC_IN_X option requested, and if there is, check if the
       data type CAN be made monotonic in X */

    /* if EOS_PT_SMOOTHING option is not set, then fail if datatype=EOS_Ut_PtT */
    eos_GetOptionEosDataMap (me, tableHandles[i], EOS_PT_SMOOTHING, &optVal, errorCode);
    if (optVal && ! optVal->bval && dataType==EOS_Ut_PtT) {
      c = *errorCode = combinedErr = EOS_INVALID_DATA_TYPE;
      c = eos_SetCustomErrorMsg(tableHandles[i], c,
				"EOS_INVALID_DATA_TYPE: Cannot load data for EOS_Ut_PtT unless EOS_PT_SMOOTHING option is set for this table handle, %d", tableHandles[i]);
      combinedErr = c;
      ((eos_ErrorHandler *) eosData)->HandleError (eosData, tableHandles[i], c);

      /* write error message to file if requested */
      me->dataObjects[me->tableHandlesMap[tableHandles[i]]]->dumpNotLoadedMsg = EOS_TRUE;
      eos_PrintTableEosDataMap (me, tableHandles[i], "TablesLoaded.dat", errorCode);
      me->dataObjects[me->tableHandlesMap[tableHandles[i]]]->dumpNotLoadedMsg = EOS_FALSE;

      /* invalidate the current table handle */
      dataObjectIndex = me->tableHandlesMap[tableHandles[i]];
      eos_InvalidateHandle (tableHandles[i]);

      eosData->refCounter--; /* current table handle no longer references data object */

      if (eosData->refCounter <= 0) {
	/* this data object is no longer referenced by any table handle */
	eos_DestroyEosData (eosData);
	eos_FreeEosData (&eosData, eosData->recordType);
	me->dataObjects[dataObjectIndex] = eosData;
	me->nTables--;
      }

      continue;
    }
    else {
      /* actually go ahead and load the data */
      c = _eos_LoadEosData (eosData, tableHandles[i]);

      /* store extrapolation boundaries if necessary */
      if (eosData->SetExtrapolationBounds)
	eosData->SetExtrapolationBounds(eosData, dataType);
    }

    /* return error if the requested subtable is not actually loaded */
    if (eos_GetStandardErrorCodeFromCustomErrorCode(c) != EOS_WARNING &&
	eos_GetStandardErrorCodeFromCustomErrorCode(c) != EOS_SPLIT_FAILED &&
	eos_GetStandardErrorCodeFromCustomErrorCode(c) != EOS_MATERIAL_NOT_FOUND &&
	(eos_GetStandardErrorCodeFromCustomErrorCode(c) != EOS_OK ||
	 reqNumSubTables > eosData->numSubtablesLoaded)) {
      if (eos_GetStandardErrorCodeFromCustomErrorCode(c) != EOS_INVALID_DATA_TYPE)
	*errorCode = combinedErr = EOS_DATA_TYPE_NOT_FOUND;

      /* set custom error message if required table is not loaded for the current subTableNum */
      if (reqNumSubTables > eosData->numSubtablesLoaded)
      {
	*errorCode =
	  eos_SetCustomErrorMsg (tableHandles[i], EOS_NO_DATA_TABLE,
				 "EOS_NO_DATA_TABLE: Data table, %s, is not in EOS table area and it could not be manufactured for table handle %d",
				 EOS_TYPE_TO_STRING(dataType), tableHandles[i]);
        combinedErr = *errorCode;
      }

      ((eos_ErrorHandler *) eosData)->HandleError (eosData, tableHandles[i], *errorCode);

      /* write error message to file if requested */
      me->dataObjects[me->tableHandlesMap[tableHandles[i]]]->dumpNotLoadedMsg = EOS_TRUE;
      eos_PrintTableEosDataMap (me, tableHandles[i], "TablesLoaded.dat", errorCode);
      me->dataObjects[me->tableHandlesMap[tableHandles[i]]]->dumpNotLoadedMsg = EOS_FALSE;

      /* invalidate the current table handle */
      dataObjectIndex = me->tableHandlesMap[tableHandles[i]];
      eos_InvalidateHandle (tableHandles[i]);

      eosData->refCounter--; /* current table handle no longer references data object */

      if (eosData->refCounter <= 0) {
	/* this data object is no longer referenced by any table handle */
	eos_DestroyEosData (eosData);
	eos_FreeEosData (&eosData, eosData->recordType);
	me->dataObjects[dataObjectIndex] = eosData;
	me->nTables--;
      }

      continue;
    }
    else if (eos_GetStandardErrorCodeFromCustomErrorCode(c) == EOS_SPLIT_FAILED  || c == EOS_MATERIAL_NOT_FOUND) {
      *errorCode = combinedErr = c;
      ((eos_ErrorHandler *) eosData)->HandleError (eosData, tableHandles[i],
						   *errorCode);
      /* write error message to file if requested */
      me->dataObjects[me->tableHandlesMap[tableHandles[i]]]->dumpNotLoadedMsg = EOS_TRUE;
      eos_PrintTableEosDataMap (me, tableHandles[i], "TablesLoaded.dat", errorCode);
      me->dataObjects[me->tableHandlesMap[tableHandles[i]]]->dumpNotLoadedMsg = EOS_FALSE;
      continue;
    }

    if (makeMonotonicInX) {
      eosData->IsMonotonic (eosData, me->tableTypes[tableHandles[i]],
                            &isMonotonicX, EOS_TRUE, EOS_FALSE, errorCode);
      if (!isMonotonicX) {
        eos_CanBeMadeMonotonicInX (eosData, me->tableTypes[tableHandles[i]],
                                   errorCode);
        if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) != EOS_OK) {
          combinedErr = *errorCode;

          ((eos_ErrorHandler *) eosData)->HandleError (eosData, tableHandles[i],
                                                       *errorCode);
	  /* write error message to file if requested */
	  me->dataObjects[me->tableHandlesMap[tableHandles[i]]]->dumpNotLoadedMsg = EOS_TRUE;
	  eos_PrintTableEosDataMap (me, tableHandles[i], "TablesLoaded.dat", errorCode);
	  me->dataObjects[me->tableHandlesMap[tableHandles[i]]]->dumpNotLoadedMsg = EOS_FALSE;

          /* invalidate the current table handle */
	  dataObjectIndex = me->tableHandlesMap[tableHandles[i]];
	  eos_InvalidateHandle (tableHandles[i]);
	  eosData->refCounter--; /* current table handle no longer references data object */

	  if (eosData->refCounter <= 0) {
	    /* this data object is no longer referenced by any table handle */
	    eos_DestroyEosData (eosData);
	    eos_FreeEosData (&eosData, eosData->recordType);
	    me->dataObjects[dataObjectIndex] = eosData;
	    me->nTables--;
	  }

          continue;
        }
        *errorCode = EOS_OK;
        eosData->MakeMonotonic (eosData, tableHandles[i],
                                me->tableTypes[tableHandles[i]],
                                (makeMonotonicInX)?EOS_TRUE:EOS_FALSE, (makeMonotonicInY)?EOS_TRUE:EOS_FALSE,
                                errorCode);
      }
      if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) != EOS_OK) {
        combinedErr = *errorCode;

        ((eos_ErrorHandler *) eosData)->HandleError (eosData, tableHandles[i],
                                                     *errorCode);
	/* write error message to file if requested */
	me->dataObjects[me->tableHandlesMap[tableHandles[i]]]->dumpNotLoadedMsg = EOS_TRUE;
	eos_PrintTableEosDataMap (me, tableHandles[i], "TablesLoaded.dat", errorCode);
	me->dataObjects[me->tableHandlesMap[tableHandles[i]]]->dumpNotLoadedMsg = EOS_FALSE;

        /* invalidate the current table handle */
        dataObjectIndex = me->tableHandlesMap[tableHandles[i]];
	eos_InvalidateHandle (tableHandles[i]);
	eosData->refCounter--; /* current table handle no longer references data object */

	if (eosData->refCounter <= 0) {
	  /* this data object is no longer referenced by any table handle */
	  eos_DestroyEosData (eosData);
	  eos_FreeEosData (&eosData, eosData->recordType);
	  me->dataObjects[dataObjectIndex] = eosData;
	  me->nTables--;
	}

        continue;
      }
    }
    if (makeMonotonicInY) {
      eosData->IsMonotonic (eosData, me->tableTypes[tableHandles[i]],
                            &isMonotonicY, EOS_FALSE, EOS_TRUE, errorCode);
      if (!isMonotonicY)
        eosData->MakeMonotonic (eosData, tableHandles[i],
                                me->tableTypes[tableHandles[i]],
                                (makeMonotonicInX)?EOS_TRUE:EOS_FALSE, (makeMonotonicInY)?EOS_TRUE:EOS_FALSE,
                                errorCode);
      if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) != EOS_OK) {
        combinedErr = *errorCode;

        ((eos_ErrorHandler *) eosData)->HandleError (eosData, tableHandles[i],
                                                     *errorCode);
	/* write error message to file if requested */
	me->dataObjects[me->tableHandlesMap[tableHandles[i]]]->dumpNotLoadedMsg = EOS_TRUE;
	eos_PrintTableEosDataMap (me, tableHandles[i], "TablesLoaded.dat", errorCode);
	me->dataObjects[me->tableHandlesMap[tableHandles[i]]]->dumpNotLoadedMsg = EOS_FALSE;

        /* invalidate the current table handle */
        dataObjectIndex = me->tableHandlesMap[tableHandles[i]];
	eos_InvalidateHandle (tableHandles[i]);
	eosData->refCounter--; /* current table handle no longer references data object */

	if (eosData->refCounter <= 0) {
	  /* this data object is no longer referenced by any table handle */
	  eos_DestroyEosData (eosData);
	  eos_FreeEosData (&eosData, eosData->recordType);
	  me->dataObjects[dataObjectIndex] = eosData;
	  me->nTables--;
	}

        continue;
      }
    }

    if (makeMonotonicInX1) {
      eosData->IsMonotonic (eosData, makeMonotonicType1, &isMonotonicX,
                            EOS_TRUE, EOS_FALSE, errorCode);
      if (!isMonotonicX) {
        eos_CanBeMadeMonotonicInX (eosData, makeMonotonicType1, errorCode);
        if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) != EOS_OK) {
          combinedErr = *errorCode;

          ((eos_ErrorHandler *) eosData)->HandleError (eosData, tableHandles[i],
                                                       *errorCode);
	  /* write error message to file if requested */
	  me->dataObjects[me->tableHandlesMap[tableHandles[i]]]->dumpNotLoadedMsg = EOS_TRUE;
	  eos_PrintTableEosDataMap (me, tableHandles[i], "TablesLoaded.dat", errorCode);
	  me->dataObjects[me->tableHandlesMap[tableHandles[i]]]->dumpNotLoadedMsg = EOS_FALSE;

          /* invalidate the current table handle */
          dataObjectIndex = me->tableHandlesMap[tableHandles[i]];
	  eos_InvalidateHandle (tableHandles[i]);
	  eosData->refCounter--; /* current table handle no longer references data object */

	  if (eosData->refCounter <= 0) {
	    /* this data object is no longer referenced by any table handle */
	    eos_DestroyEosData (eosData);
	    eos_FreeEosData (&eosData, eosData->recordType);
	    me->dataObjects[dataObjectIndex] = eosData;
	    me->nTables--;
	  }

          continue;
        }
        *errorCode = EOS_OK;
        eosData->MakeMonotonic (eosData, tableHandles[i], makeMonotonicType1,
                                (makeMonotonicInX1)?EOS_TRUE:EOS_FALSE, (makeMonotonicInY1)?EOS_TRUE:EOS_FALSE,
                                errorCode);
      }
      if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) != EOS_OK) {
        combinedErr = *errorCode;
        /* invalidate the current table handle */
        dataObjectIndex = me->tableHandlesMap[tableHandles[i]];
	eos_InvalidateHandle (tableHandles[i]);
	eosData->refCounter--; /* current table handle no longer references data object */

	if (eosData->refCounter <= 0) {
	  /* this data object is no longer referenced by any table handle */
	  eos_DestroyEosData (eosData);
	  eos_FreeEosData (&eosData, eosData->recordType);
	  me->dataObjects[dataObjectIndex] = eosData;
	  me->nTables--;
	}

        continue;
      }
    }
    if (makeMonotonicInY1) {
      eosData->IsMonotonic (eosData, makeMonotonicType1, &isMonotonicY,
                            EOS_FALSE, EOS_TRUE, errorCode);
      if (!isMonotonicY)
        eosData->MakeMonotonic (eosData, tableHandles[i], makeMonotonicType1,
                                (makeMonotonicInX1)?EOS_TRUE:EOS_FALSE, (makeMonotonicInY1)?EOS_TRUE:EOS_FALSE,
                                errorCode);
      if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) != EOS_OK) {
        combinedErr = *errorCode;

	((eos_ErrorHandler *) eosData)->HandleError (eosData, tableHandles[i],
						     *errorCode);
	/* write error message to file if requested */
	me->dataObjects[me->tableHandlesMap[tableHandles[i]]]->dumpNotLoadedMsg = EOS_TRUE;
	eos_PrintTableEosDataMap (me, tableHandles[i], "TablesLoaded.dat", errorCode);
	me->dataObjects[me->tableHandlesMap[tableHandles[i]]]->dumpNotLoadedMsg = EOS_FALSE;

        /* invalidate the current table handle */
        dataObjectIndex = me->tableHandlesMap[tableHandles[i]];
	eos_InvalidateHandle (tableHandles[i]);
	eosData->refCounter--; /* current table handle no longer references data object */

	if (eosData->refCounter <= 0) {
	  /* this data object is no longer referenced by any table handle */
	  eos_DestroyEosData (eosData);
	  eos_FreeEosData (&eosData, eosData->recordType);
	  me->dataObjects[dataObjectIndex] = eosData;
	  me->nTables--;
	}

        continue;
      }
    }

    if (makeMonotonicInX2) {
      eosData->IsMonotonic (eosData, makeMonotonicType2, &isMonotonicX,
                            EOS_TRUE, EOS_FALSE, errorCode);
      if (!isMonotonicX) {
        eos_CanBeMadeMonotonicInX (eosData, makeMonotonicType2, errorCode);
        if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) != EOS_OK) {
          combinedErr = *errorCode;

          ((eos_ErrorHandler *) eosData)->HandleError (eosData, tableHandles[i],
                                                       *errorCode);
	  /* write error message to file if requested */
	  me->dataObjects[me->tableHandlesMap[tableHandles[i]]]->dumpNotLoadedMsg = EOS_TRUE;
	  eos_PrintTableEosDataMap (me, tableHandles[i], "TablesLoaded.dat", errorCode);
	  me->dataObjects[me->tableHandlesMap[tableHandles[i]]]->dumpNotLoadedMsg = EOS_FALSE;

          /* invalidate the current table handle */
          dataObjectIndex = me->tableHandlesMap[tableHandles[i]];
	  eos_InvalidateHandle (tableHandles[i]);
	  eosData->refCounter--; /* current table handle no longer references data object */

	  if (eosData->refCounter <= 0) {
	    /* this data object is no longer referenced by any table handle */
	    eos_DestroyEosData (eosData);
	    eos_FreeEosData (&eosData, eosData->recordType);
	    me->dataObjects[dataObjectIndex] = eosData;
	    me->nTables--;
	  }

          continue;
        }
        *errorCode = EOS_OK;
        eosData->MakeMonotonic (eosData, tableHandles[i], makeMonotonicType2,
                                (makeMonotonicInX2)?EOS_TRUE:EOS_FALSE, (makeMonotonicInY2)?EOS_TRUE:EOS_FALSE,
                                errorCode);
      }
      if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) != EOS_OK) {
        combinedErr = *errorCode;

	((eos_ErrorHandler *) eosData)->HandleError (eosData, tableHandles[i],
						     *errorCode);
	/* write error message to file if requested */
	me->dataObjects[me->tableHandlesMap[tableHandles[i]]]->dumpNotLoadedMsg = EOS_TRUE;
	eos_PrintTableEosDataMap (me, tableHandles[i], "TablesLoaded.dat", errorCode);
	me->dataObjects[me->tableHandlesMap[tableHandles[i]]]->dumpNotLoadedMsg = EOS_FALSE;

        /* invalidate the current table handle */
        dataObjectIndex = me->tableHandlesMap[tableHandles[i]];
	eos_InvalidateHandle (tableHandles[i]);
	eosData->refCounter--; /* current table handle no longer references data object */

	if (eosData->refCounter <= 0) {
	  /* this data object is no longer referenced by any table handle */
	  eos_DestroyEosData (eosData);
	  eos_FreeEosData (&eosData, eosData->recordType);
	  me->dataObjects[dataObjectIndex] = eosData;
	  me->nTables--;
	}

        continue;
      }
    }
    if (makeMonotonicInY2) {
      eosData->IsMonotonic (eosData, makeMonotonicType2, &isMonotonicY,
                            EOS_FALSE, EOS_TRUE, errorCode);
      if (!isMonotonicY)
        eosData->MakeMonotonic (eosData, tableHandles[i], makeMonotonicType2,
                                (makeMonotonicInX2)?EOS_TRUE:EOS_FALSE, (makeMonotonicInY2)?EOS_TRUE:EOS_FALSE,
                                errorCode);
      if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) != EOS_OK) {
        combinedErr = *errorCode;

	((eos_ErrorHandler *) eosData)->HandleError (eosData, tableHandles[i],
						     *errorCode);
	/* write error message to file if requested */
	me->dataObjects[me->tableHandlesMap[tableHandles[i]]]->dumpNotLoadedMsg = EOS_TRUE;
	eos_PrintTableEosDataMap (me, tableHandles[i], "TablesLoaded.dat", errorCode);
	me->dataObjects[me->tableHandlesMap[tableHandles[i]]]->dumpNotLoadedMsg = EOS_FALSE;

        /* invalidate the current table handle */
        dataObjectIndex = me->tableHandlesMap[tableHandles[i]];
	eos_InvalidateHandle (tableHandles[i]);
	eosData->refCounter--; /* current table handle no longer references data object */

	if (eosData->refCounter <= 0) {
	  /* this data object is no longer referenced by any table handle */
	  eos_DestroyEosData (eosData);
	  eos_FreeEosData (&eosData, eosData->recordType);
	  me->dataObjects[dataObjectIndex] = eosData;
	  me->nTables--;
	}

        continue;
      }
    }

    /* check if we need to make it smooth */
    optVal = _eos_getOptionEosData (eosData, EOS_SMOOTH);
    if (optVal && optVal->bval) {
      eosData->MakeSmooth (me->dataObjects[tableHandles[i]], tableHandles[i],
                           me->tableTypes[tableHandles[i]], EOS_FALSE,
                           errorCode);
      if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) != EOS_OK) {
        combinedErr = *errorCode;

	((eos_ErrorHandler *) eosData)->HandleError (eosData, tableHandles[i],
						     *errorCode);
	/* write error message to file if requested */
	me->dataObjects[me->tableHandlesMap[tableHandles[i]]]->dumpNotLoadedMsg = EOS_TRUE;
	eos_PrintTableEosDataMap (me, tableHandles[i], "TablesLoaded.dat", errorCode);
	me->dataObjects[me->tableHandlesMap[tableHandles[i]]]->dumpNotLoadedMsg = EOS_FALSE;

        /* invalidate the current table handle */
        dataObjectIndex = me->tableHandlesMap[tableHandles[i]];
	eos_InvalidateHandle (tableHandles[i]);
	eosData->refCounter--; /* current table handle no longer references data object */

	if (eosData->refCounter <= 0) {
	  /* this data object is no longer referenced by any table handle */
	  eos_DestroyEosData (eosData);
	  eos_FreeEosData (&eosData, eosData->recordType);
	  me->dataObjects[dataObjectIndex] = eosData;
	  me->nTables--;
	}

        continue;
      }
    }

    /* check if we need to make it smooth */
    optVal = _eos_getOptionEosData (eosData, EOS_PT_SMOOTHING);
    if (optVal && optVal->bval) {
      eosData->MakeSmooth (me->dataObjects[tableHandles[i]], tableHandles[i],
                           me->tableTypes[tableHandles[i]], EOS_TRUE,
                           errorCode);
      if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) != EOS_OK && eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) != EOS_GEN401_AND_NOT_FOUND) {
        combinedErr = *errorCode;

	((eos_ErrorHandler *) eosData)->HandleError (eosData, tableHandles[i],
						     *errorCode);
	/* write error message to file if requested */
	me->dataObjects[me->tableHandlesMap[tableHandles[i]]]->dumpNotLoadedMsg = EOS_TRUE;
	eos_PrintTableEosDataMap (me, tableHandles[i], "TablesLoaded.dat", errorCode);
	me->dataObjects[me->tableHandlesMap[tableHandles[i]]]->dumpNotLoadedMsg = EOS_FALSE;

        /* invalidate the current table handle */
        dataObjectIndex = me->tableHandlesMap[tableHandles[i]];
	eos_InvalidateHandle (tableHandles[i]);
	eosData->refCounter--; /* current table handle no longer references data object */

	if (eosData->refCounter <= 0) {
	  /* this data object is no longer referenced by any table handle */
	  eos_DestroyEosData (eosData);
	  eos_FreeEosData (&eosData, eosData->recordType);
	  me->dataObjects[dataObjectIndex] = eosData;
	  me->nTables--;
	}

        continue;
      }
    }

    /* store extrapolation boundaries if necessary */
    if (eosData->SetExtrapolationBounds)
      eosData->SetExtrapolationBounds(eosData, dataType);

    /* check if inversion at setup is required */
    optVal = _eos_getOptionEosData (eosData, EOS_INVERT_AT_SETUP);
    if (optVal && optVal->bval && eosData->InvertAtSetup) {
      eosData->InvertAtSetup(eosData, tableHandles[i], me->tableTypes[tableHandles[i]], errorCode);

      if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) != EOS_OK) {
        combinedErr = *errorCode;
	((eos_ErrorHandler *) eosData)->HandleError (eosData, tableHandles[i], *errorCode);
      }
    }    

    /* print it if requested */
    eos_PrintTableEosDataMap (me, tableHandles[i], "TablesLoaded.dat",
                              errorCode);
    if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) != EOS_OK) {
      combinedErr = *errorCode;
      continue;
    }
  }                             /* table handle loop */
  *errorCode = combinedErr;

  /* Cleanup error code(s) that are not to be returned from this function */
  if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) == EOS_INTERP_EXTRAPOLATED)
    *errorCode = EOS_OK;
  for (i = 0; i < nTables; i++)
    if (eos_GetStandardErrorCodeFromCustomErrorCode(me->errorCodes[tableHandles[i]]) == EOS_INTERP_EXTRAPOLATED)
      me->errorCodes[tableHandles[i]] = EOS_OK;
}

/*************************************************************************
 * PACK and UNPACK debugging functions
 *************************************************************************/
//#define _DEBUG_PACKING_FUNCTIONS
#ifdef _DEBUG_PACKING_FUNCTIONS
static EOS_INTEGER *packedByteTotals = NULL;
static EOS_INTEGER packedByteTotalsSize = 0;
static EOS_INTEGER *packedBytes = NULL;
static EOS_INTEGER packedBytesSize = 0;
static EOS_INTEGER eos_GetPackedTablesIndex0 = 0;
static EOS_INTEGER eos_SetPackedTablesIndex0 = 0;
EOS_INTEGER packedBytesResetOverride = EOS_FALSE;
#endif

void _EOS_CHECK_PACKEDBYTES (EOS_CHAR *errortype, EOS_INTEGER *i,
                             EOS_INTEGER b, EOS_INTEGER tot, EOS_CHAR *s,
			     EOS_INTEGER th, EOS_BOOLEAN storeData)
{
#ifdef _DEBUG_PACKING_FUNCTIONS
  if (storeData) { /* store the packedBytes and return */
    printf (" %i. %i(%i) ", packedBytesSize, b, tot);
    packedBytes =
      (EOS_INTEGER *) realloc (packedBytes,
			       (++packedBytesSize) * sizeof (EOS_INTEGER));
    packedBytes[packedBytesSize - 1] = b;

    packedByteTotals =
      (EOS_INTEGER *) realloc (packedByteTotals,
			       (++packedByteTotalsSize) * sizeof (EOS_INTEGER));
    packedByteTotals[packedByteTotalsSize - 1] = tot;
    return;
  }
  printf (" %i. %i(%i) ", *i, b, tot);
  if (packedBytes[*i] != b) {
    printf
      ("\n%s INC-ERROR for TableHandle=%i: '%s' expected %i bytes, but %i bytes were packed\n",
       errortype, th, s, packedBytes[*i], b);
    assert (packedBytes[*i] == b);
  }
  if (packedByteTotals[*i] != tot) {
    printf
      ("\n%s TOT-ERROR for TableHandle=%i: '%s' expected %i bytes, but %i bytes were packed\n",
       errortype, th, s, packedByteTotals[*i], tot);
    assert (packedByteTotals[*i] == tot);
  }
  (*i)++;
#endif
}

/*! **********************************************************************
 *
 * \brief The eos_GetPackedTablesEosDataMap function is dual-purposed:
 *   -# It fills a character array with the specified data table's data.
 *   -# It returns the size requirements for the character array specified in 1.
 *
 * This is the function to extract the data tables from EOSPAC to allow multithreaded
 * codes to share the data.
 * Before calling this routine, the host code must call  eos_GetPackedTablesSize
 * to determine packedTablesSize, which is the total number of bytes required to contain
 * the data associated with the specified data tables; This provides the needed
 * information allowing the host code to allocate adequate storage.
 *
 * Input Values:
 * eos_DataMap *me  - this pointer (pointer to the instance of type eos_DataMap
 * EOS_INTEGER nTables
 * EOS_INTEGER tableHandles[nTables]
 *
 * Ordered List of Optional Input Value(s):
 * EOS_BOOLEAN getSizeOnly - if true, then no data is packed, just the packedTablesSize
 *                           is calculated and returned
 *
 * Returned Values:
 * EOS_CHAR    *packedTables[packedTablesSize]
 *                 - if NULL, then no data is packed, just the packedTablesSize
 *                   is calculated and returned
 * EOS_INTEGER *packedTablesSize = size packed
 * EOS_INTEGER *errorCode
 *
 *************************************************************************/
void eos_GetPackedTablesEosDataMap (eos_DataMap *me, EOS_INTEGER nTables,
                                    EOS_INTEGER tableHandles[],
                                    EOS_CHAR *packedTables,
                                    EOS_INTEGER *packedTablesSize,
                                    EOS_INTEGER *errorCode)
{

#ifdef DEBUG
  char fname_tmp[100];
  static int count1 = 0;
#endif
  EOS_INTEGER i, j, c, sizePacked = 0, tableOption, size, index = 0;
  eos_Data *eosData;
  extern EOS_BOOLEAN enable_DEBUG_PRINT;
  EOS_BOOLEAN getSizeOnly = EOS_FALSE;

  *errorCode = EOS_OK;

  /* determine if only packed tables' size is requested */
  getSizeOnly = (packedTables == NULL)?EOS_TRUE:EOS_FALSE;

#ifdef DEBUG
  count1++;
#endif

#ifdef _DEBUG_PACKING_FUNCTIONS
  if (getSizeOnly && ! packedBytesResetOverride) {
    printf ("\n\t\ti. Inc(Total)\t*** RESET BYTE OFFSETS STORAGE eos_GetPackedTablesEosDataMap ***");
    packedBytesSize = packedByteTotalsSize = 0;
    eos_GetPackedTablesIndex0 = eos_SetPackedTablesIndex0 = 0;
  }
  index = eos_GetPackedTablesIndex0;
  if (getSizeOnly)
     printf ("\n\t\ti. Inc(Total)\t*** STORING BYTE OFFSETS IN eos_GetPackedTablesEosDataMap ***");
  else
    printf ("\n\t\ti. Inc(Total)\t*** CHECKING BYTE OFFSETS IN eos_GetPackedTablesEosDataMap ***");
#endif

  /* pack sesameFilesL */
  if (!getSizeOnly)
    memcpy (packedTables + sizePacked, &sesameFilesL, sizeof (sesameFilesL));
  size = sizeof (sesameFilesL);
  sizePacked += size;
  _EOS_CHECK_PACKEDBYTES ("PACK", &index, size, sizePacked, "sesameFilesL", -1, getSizeOnly);

  /* pack sesameFiles list */
  for (i = 0; i < sesameFilesL; i++) {
    j = strlen(sesameFiles[i]) + 1;
    if (!getSizeOnly)
      memcpy (packedTables + sizePacked, sesameFiles[i], j);
    size = j;
    sizePacked += size;
    _EOS_CHECK_PACKEDBYTES ("PACK", &index, size, sizePacked, "sesameFiles[i]", -1, getSizeOnly);
  }

  /* pack sesameIndexLocationsL */
  if (!getSizeOnly)
    memcpy (packedTables + sizePacked, &sesameIndexLocationsL, sizeof (sesameIndexLocationsL));
  size = sizeof (sesameIndexLocationsL);
  sizePacked += size;
  _EOS_CHECK_PACKEDBYTES ("PACK", &index, size, sizePacked, "sesameIndexLocationsL", -1, getSizeOnly);

  /* pack sesameIndexLocations list */
  for (i = 0; i < sesameIndexLocationsL; i++) {
    j = strlen(sesameIndexLocations[i]) + 1;
    if (!getSizeOnly)
      memcpy (packedTables + sizePacked, sesameIndexLocations[i], j);
    size = j;
    sizePacked += size;
    _EOS_CHECK_PACKEDBYTES ("PACK", &index, size, sizePacked, "sesameIndexLocations[i]", -1, getSizeOnly);
  }

  /* pack sesameFileCache, but don't include file handles */
  for (i = 0; i < sesameFilesL; i++) {

    /* pack sesFile */
    size = sizeof (sesameFileCache[i].sesFile);
    if (!getSizeOnly)
      memcpy (packedTables + sizePacked, &sesameFileCache[i].sesFile, size);
    sizePacked += size;
    _EOS_CHECK_PACKEDBYTES ("PACK", &index, size, sizePacked, "sesFile", -1, getSizeOnly);

    /* pack nmats */
    size = sizeof (sesameFileCache[i].nmats);
    if (!getSizeOnly)
      memcpy (packedTables + sizePacked, &sesameFileCache[i].nmats, size);
    sizePacked += size;
    _EOS_CHECK_PACKEDBYTES ("PACK", &index, size, sizePacked, "nmats", -1, getSizeOnly);

    /* pack sesMats */
    size = sesameFileCache[i].nmats * sizeof (sesameFileCache[i].sesMats[0]);
    if (!getSizeOnly)
      memcpy (packedTables + sizePacked, sesameFileCache[i].sesMats, size);
    sizePacked += size;
    _EOS_CHECK_PACKEDBYTES ("PACK", &index, size, sizePacked, "sesMats", -1, getSizeOnly);

    /* pack materialListLoaded */
    size = sizeof (sesameFileCache[i].materialListLoaded);
    if (!getSizeOnly)
      memcpy (packedTables + sizePacked, &sesameFileCache[i].materialListLoaded, size);
    sizePacked += size;
    _EOS_CHECK_PACKEDBYTES ("PACK", &index, size, sizePacked, "materialListLoaded", -1, getSizeOnly);

  }

  /* pack all the tables */
  for (i = 0; i < nTables; i++) {

#ifdef _DEBUG_PACKING_FUNCTIONS
    printf ("\nTableHandle %3i: ", tableHandles[i]);
#endif
    /* pack tableHandles[i] */
    if (!getSizeOnly)
      memcpy (packedTables + sizePacked, &tableHandles[i], sizeof (EOS_INTEGER));
    size = sizeof (EOS_INTEGER);
    sizePacked += size;
    _EOS_CHECK_PACKEDBYTES ("PACK", &index, size, sizePacked, "me->tableHandlesMap",
                            tableHandles[i], getSizeOnly);

    /* pack me->tableHandlesMap[i] */
    if (!getSizeOnly)
      memcpy (packedTables + sizePacked, &me->tableHandlesMap[tableHandles[i]], sizeof (EOS_INTEGER));
    size = sizeof (EOS_INTEGER);
    sizePacked += size;
    _EOS_CHECK_PACKEDBYTES ("PACK", &index, size, sizePacked, "me->tableHandlesMap",
                            tableHandles[i], getSizeOnly);

    if (!eos_IsHandleValidEosDataMap (me, tableHandles[i])) {
#ifdef _DEBUG_PACKING_FUNCTIONS
      printf ("-->TH %i", me->tableHandlesMap[tableHandles[i]]);
#endif
      eos_HandleErrorEosDataMap (me, tableHandles[i],
                                 EOS_INVALID_TABLE_HANDLE);
      *errorCode = EOS_INVALID_TABLE_HANDLE;
      continue;
    }

    eosData = me->dataObjects[me->tableHandlesMap[tableHandles[i]]];

    /* pack the dataType and the material id in front of the data */
    if (!getSizeOnly)
      memcpy (packedTables + sizePacked, &(me->tableTypes[tableHandles[i]]),
	      sizeof (me->tableTypes[tableHandles[i]]));
    size = sizeof (me->tableTypes[tableHandles[i]]);
    sizePacked += size;
    _EOS_CHECK_PACKEDBYTES ("PACK", &index, size, sizePacked, "me->tableTypes",
                            tableHandles[i], getSizeOnly);
    if (!getSizeOnly)
      memcpy (packedTables + sizePacked, &(eosData->materialID),
	      sizeof (eosData->materialID));
    size = sizeof (eosData->materialID);
    sizePacked += size;
    _EOS_CHECK_PACKEDBYTES ("PACK", &index, size, sizePacked, "eosData->materialID",
                            tableHandles[i], getSizeOnly);

    /* pack the public/internal flag */
    if (!getSizeOnly)
      memcpy (packedTables + sizePacked, &(me->isHandlePublic[tableHandles[i]]),
	      sizeof (me->isHandlePublic[tableHandles[i]]));
    size = sizeof (me->isHandlePublic[tableHandles[i]]);
    sizePacked += size;
    _EOS_CHECK_PACKEDBYTES ("PACK", &index, size, sizePacked, "me->isHandlePublic",
                            tableHandles[i], getSizeOnly);

    /* pack the general options specific for this table handle */
    size = 0;
    for (j = 0; j < EOS_NUM_GENERAL_OPTIONS; j++) {
      tableOption = EOS_GENERAL_INDEX_TO_OPTION_FLAG (j);
      switch (tableOption) {
      case EOS_X_CONVERT:
      case EOS_Y_CONVERT:
      case EOS_F_CONVERT:
	if (!getSizeOnly)
	  memcpy (packedTables + sizePacked + size,
		  &(me->generalOptions[j][tableHandles[i]].rval),
		  sizeof (EOS_REAL));
        size += sizeof (EOS_REAL);
        break;

      case EOS_DEBUG_PRINT:
	if (!getSizeOnly)
	  memcpy (packedTables + sizePacked + size,
		  &(me->generalOptions[j][tableHandles[i]].bval),
		  sizeof (EOS_BOOLEAN));
        enable_DEBUG_PRINT = me->generalOptions[j][tableHandles[i]].bval;
        size += sizeof (EOS_BOOLEAN);
        break;

      default:
	if (!getSizeOnly)
	  memcpy (packedTables + sizePacked + size,
		  &(me->generalOptions[j][tableHandles[i]].bval),
		  sizeof (EOS_BOOLEAN));
        size += sizeof (EOS_BOOLEAN);
        break;
      }
    }
    sizePacked += size;
    _EOS_CHECK_PACKEDBYTES ("PACK", &index, size, sizePacked, "me->generalOptions",
                            tableHandles[i], getSizeOnly);

    /* pack the LOADING table options */
    if (!getSizeOnly)
      eos_PackOptionsEosData (eosData, packedTables + sizePacked, &size, errorCode);
    else
      eos_PackOptionsEosData (eosData, NULL, &size, errorCode);
    sizePacked += size;
    _EOS_CHECK_PACKEDBYTES ("PACK", &index, size, sizePacked, "loading options",
                            tableHandles[i], getSizeOnly);

    /* now actually pack the data */
    eosData->GetPackedTableSize (eosData, tableHandles[i], &size, &c);
    if (!getSizeOnly)
      eosData->GetPackedTable (eosData, tableHandles[i],
			       packedTables + sizePacked, &c);
    sizePacked += size;
    c = eos_GetErrorCodeEosDataMap (&gEosDataMap, tableHandles[i]);
    if (eos_GetStandardErrorCodeFromCustomErrorCode(c) != EOS_OK)
      *errorCode = c;
    _EOS_CHECK_PACKEDBYTES ("PACK", &index, size, sizePacked, "data", tableHandles[i], getSizeOnly);
  }

  *packedTablesSize = sizePacked;

#ifdef _DEBUG_PACKING_FUNCTIONS
  printf ("\n");
#endif

#ifdef DEBUG
  sprintf (fname_tmp, "TablesLoaded_Get_%i.dat", count1);

  /* overwrite output file */
  for (i = 0; i < nTables; i++)
    eosData->Print (me->dataObjects[me->tableHandlesMap[tableHandles[i]]],
                    tableHandles[i], fname_tmp, (i != 0), errorCode);
#endif

#ifdef _DEBUG_PACKING_FUNCTIONS
  eos_GetPackedTablesIndex0 = index;
#endif
}


/*! **********************************************************************
 *
 * \brief The eos_SetPackedTablesEosDataMap function fills the specified data
 * tables with data tables stored as a character array.
 *
 * Typically this is used to insert the data tables into EOSPAC after a
 * multithreaded code has shared the data tables extracted by eos_GetPackedTables.
 *
 * Input Values:
 * eos_DataMap *me  - this pointer (pointer to the instance of type eos_DataMap
 * EOS_INTEGER nTables
 * EOS_INTEGER tableHandles[nTables]
 * EOS_CHAR    packedTables[packedTablesSize]
 *
 * Returned Values:
 * EOS_INTEGER *unpackedTablesSize
 * EOS_INTEGER *errorCode
 *
 *************************************************************************/

/* The following flag is used for testing: */

EOS_BOOLEAN disable_SetPackedTablesPrint = EOS_TRUE;

void eos_SetPackedTablesEosDataMap (eos_DataMap *me, EOS_INTEGER nTables,
                                    EOS_INTEGER tableHandles[],
                                    EOS_CHAR *packedTables,
                                    EOS_INTEGER *unpackedTablesSize,
                                    EOS_INTEGER *errorCode)
{

#ifdef DEBUG
  char fname_tmp[100];
  static int count2 = 0;
#endif
  EOS_INTEGER tableType, isPublic, matID, i, j, c, size, tableOption, sizeUnpacked = 0,
    oldHandle, oldHandle_ref;
  EOS_REAL fval = 0.0;
  EOS_INTEGER ival = 0, index = 0;
  EOS_BOOLEAN bval = EOS_FALSE;
  eos_Data *eosData;
  extern EOS_BOOLEAN enable_DEBUG_PRINT;

  ses_file_handle sFH_tmp;
  ses_boolean sBool;

  *errorCode = EOS_OK;

#ifdef DEBUG
  count2++;
#endif

#ifdef _DEBUG_PACKING_FUNCTIONS
  index = eos_SetPackedTablesIndex0;
  printf
    ("\n\t\ti. Inc(Total)\t*** CHECKING BYTE OFFSETS IN eos_SetPackedTablesEosDataMap ***");
#endif

  /* make room for unpacking of sesameFilesL, sesameFiles,
     sesameIndexLocations, sesameIndexLocationsL and sesameFileCache */
  if ((sesameFilesL > 0 || sesameIndexLocationsL > 0) &&
      ( sesameFiles || sesameFileCache || sesameIndexLocations))
    eos_SesCleanFileCache();

  /* unpack sesameFilesL */
  memcpy (&sesameFilesL, packedTables + sizeUnpacked, sizeof (sesameFilesL));
  size = sizeof (sesameFilesL);
  sizeUnpacked += size;
  _EOS_CHECK_PACKEDBYTES ("UNPACK", &index, size, sizeUnpacked, "sesameFilesL", -1, EOS_FALSE);

  sesameFileCacheL = sesameFilesL;

  /* unpack sesameFiles list */
  sesameFiles = (EOS_CHAR **) malloc(sesameFilesL * sizeof(EOS_CHAR *));
  for (i = 0; i < sesameFilesL; i++) {
    j = strlen(packedTables + sizeUnpacked);
    sesameFiles[i] = (EOS_CHAR *) malloc((j+1) * sizeof(EOS_CHAR));
    sesameFiles[i] = strcpy(sesameFiles[i], packedTables + sizeUnpacked);
    size = j + 1;
    sizeUnpacked += size;
    _EOS_CHECK_PACKEDBYTES ("UNPACK", &index, size, sizeUnpacked, "sesameFiles[i]", -1, EOS_FALSE);
  }

  /* unpack sesameIndexLocationsL */
  memcpy (&sesameIndexLocationsL, packedTables + sizeUnpacked, sizeof (sesameIndexLocationsL));
  size = sizeof (sesameIndexLocationsL);
  sizeUnpacked += size;
  _EOS_CHECK_PACKEDBYTES ("UNPACK", &index, size, sizeUnpacked, "sesameIndexLocationsL", -1, EOS_FALSE);

  /* unpack sesameFiles list */
  sesameIndexLocations = (EOS_CHAR **) malloc(sesameIndexLocationsL * sizeof(EOS_CHAR *));
  for (i = 0; i < sesameIndexLocationsL; i++) {
    j = strlen(packedTables + sizeUnpacked);
    sesameIndexLocations[i] = (EOS_CHAR *) malloc((j+1) * sizeof(EOS_CHAR));
    sesameIndexLocations[i] = strcpy(sesameIndexLocations[i], packedTables + sizeUnpacked);
    size = j + 1;
    sizeUnpacked += size;
    _EOS_CHECK_PACKEDBYTES ("UNPACK", &index, size, sizeUnpacked, "sesameIndexLocations[i]", -1, EOS_FALSE);
  }

  /* unpack sesameFileCache, but don't open files */
  sesameFileCache = (SesameFileCache *) malloc (sesameFilesL * sizeof (SesameFileCache));
  for (i = 0; i < sesameFilesL; i++) {

    /* unpack sesFile */
    memcpy (&sFH_tmp, packedTables + sizeUnpacked, sizeof (sFH_tmp));
    sesameFileCache[i].sesFile = sFH_tmp;
    size = sizeof (sFH_tmp);
    sizeUnpacked += size;
    _EOS_CHECK_PACKEDBYTES ("UNPACK", &index, size, sizeUnpacked, "sesFile", -1, EOS_FALSE);

    /* unpack nmats */
    memcpy (&j, packedTables + sizeUnpacked, sizeof (j));
    sesameFileCache[i].nmats = j;
    size = sizeof (j);
    sizeUnpacked += size;
    _EOS_CHECK_PACKEDBYTES ("UNPACK", &index, size, sizeUnpacked, "nmats", -1, EOS_FALSE);

    /* unpack sesMats */
    size = sesameFileCache[i].nmats * sizeof (sesameFileCache[i].sesMats[0]);
    sesameFileCache[i].sesMats =
      (ses_material_id_reference) malloc (size);
    if (size > 0)
      memcpy (sesameFileCache[i].sesMats, packedTables + sizeUnpacked, size);
    else
      sesameFileCache[i].sesMats = (ses_material_id_reference)NULL;
    sizeUnpacked += size;
    _EOS_CHECK_PACKEDBYTES ("UNPACK", &index, size, sizeUnpacked, "sesMats", -1, EOS_FALSE);

    /* unpack materialListLoaded */
    memcpy (&sBool, packedTables + sizeUnpacked, sizeof (sBool));
    sesameFileCache[i].materialListLoaded = sBool;
    size = sizeof (sBool);
    sizeUnpacked += size;
    _EOS_CHECK_PACKEDBYTES ("UNPACK", &index, size, sizeUnpacked, "materialListLoaded", -1, EOS_FALSE);

  }

  /* unpack all the tables */
  for (i = 0; i < nTables; i++) {

#ifdef _DEBUG_PACKING_FUNCTIONS
    printf ("\nTableHandle %3i: ", tableHandles[i]);
#endif
    /* unpack tableHandles[i] */
    memcpy (&oldHandle, packedTables + sizeUnpacked, sizeof (oldHandle));
    size = sizeof (oldHandle);
    sizeUnpacked += size;
    _EOS_CHECK_PACKEDBYTES ("UNPACK", &index, size, sizeUnpacked, "tableHandles[i]",
                            oldHandle, EOS_FALSE);

    /* unpack me->tableHandlesMap[i] */
    memcpy (&oldHandle_ref, packedTables + sizeUnpacked, sizeof (oldHandle_ref));
    size = sizeof (oldHandle_ref);
    sizeUnpacked += size;
    _EOS_CHECK_PACKEDBYTES ("UNPACK", &index, size, sizeUnpacked, "me->tableHandlesMap[i]",
                            oldHandle_ref, EOS_FALSE);

    if (oldHandle_ref < 0) {
      me->nHandles++;
      // reallocate enough memory for the new handle
      eos_SetSizeEosDataMap (me, me->nAlloc + 1, errorCode);
      if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) != EOS_OK)
        return;

      tableHandles[i] = me->nHandles - 1;
      me->tableHandlesMap[tableHandles[i]] = oldHandle_ref;
      me->dataObjects[tableHandles[i]] = NULL;
      eos_InvalidateHandle (tableHandles[i]);

      // update the number of table handles in eos_Interpolation class
      eos_SetNumberOfHandles (&gEosInterpolation, 1, &c);

#ifdef _DEBUG_PACKING_FUNCTIONS
      printf ("-->TH %i", me->tableHandlesMap[tableHandles[i]]);
#endif
      continue;
    }

    /* unpack the dataType and the material id stored in front of the data */
    memcpy (&tableType, packedTables + sizeUnpacked, sizeof (tableType));
    size = sizeof (tableType);
    sizeUnpacked += size;
    _EOS_CHECK_PACKEDBYTES ("UNPACK", &index, size, sizeUnpacked, "tableType",
                            tableHandles[i], EOS_FALSE);

    memcpy (&matID, packedTables + sizeUnpacked, sizeof (matID));
    size = sizeof (matID);
    sizeUnpacked += size;
    _EOS_CHECK_PACKEDBYTES ("UNPACK", &index, size, sizeUnpacked, "matID",
                            tableHandles[i], EOS_FALSE);

    /* unpack the public/internal flag */
    memcpy (&isPublic, packedTables + sizeUnpacked, sizeof (isPublic));
    size = sizeof (isPublic);
    sizeUnpacked += size;
    _EOS_CHECK_PACKEDBYTES ("UNPACK", &index, size, sizeUnpacked, "isPublic",
                            tableHandles[i], EOS_FALSE);

    /* construct an empty table without creating it */
    eos_CreateTablesEosDataMap (me, 1, &tableType, &matID, &(tableHandles[i]),
                                EOS_FALSE, isPublic, EOS_FALSE, -1, errorCode);
    eosData = me->dataObjects[me->tableHandlesMap[tableHandles[i]]];
    c = eos_GetErrorCodeEosDataMap (&gEosDataMap, tableHandles[i]);
    if (eos_GetStandardErrorCodeFromCustomErrorCode(c) != EOS_OK) {
      *errorCode = c;
      return;
    }

    /* unpack the general options specific for this table handle */
    size = 0;
    for (j = 0; j < EOS_NUM_GENERAL_OPTIONS; j++) {
      tableOption = EOS_GENERAL_INDEX_TO_OPTION_FLAG (j);
      switch (tableOption) {
      case EOS_X_CONVERT:
      case EOS_Y_CONVERT:
      case EOS_F_CONVERT:
        memcpy (&(me->generalOptions[j][tableHandles[i]].rval),
                packedTables + sizeUnpacked + size, sizeof (EOS_REAL));
        size += sizeof (EOS_REAL);
        break;

      case EOS_DEBUG_PRINT:
        memcpy (&(me->generalOptions[j][tableHandles[i]].bval),
                packedTables + sizeUnpacked + size, sizeof (EOS_BOOLEAN));
        enable_DEBUG_PRINT = me->generalOptions[j][tableHandles[i]].bval;
        size += sizeof (EOS_BOOLEAN);
        break;

      default:
        memcpy (&(me->generalOptions[j][tableHandles[i]].bval),
                packedTables + sizeUnpacked + size, sizeof (EOS_BOOLEAN));
        size += sizeof (EOS_BOOLEAN);
        break;
      }
    }
    sizeUnpacked += size;
    _EOS_CHECK_PACKEDBYTES ("UNPACK", &index, size, sizeUnpacked, "me->generalOptions",
                            tableHandles[i], EOS_FALSE);

    /* now unpack the loading options, if refCount for our object is > 1, call _eos_SetOptionsEosDataMap () 
       for each unpacked option, just in case there are non-default options and we need to create another object. */
    size = 0;
    if (eosData->refCounter == 1) {     /* we do not share this object */
      eos_UnpackOptionsEosData (eosData, packedTables + sizeUnpacked,
                                errorCode);
      eos_GetPackedOptionsSizeEosData (eosData, &size, errorCode);
    }
    else {                      /* we share the data object, must do by hand */

      for (j = 0; j < EOS_NUM_LOADING_OPTIONS; j++) {
        tableOption = eosData->tableOptions[j].optionFlag;
	eosData->tableOptions[j].optionType =
	  eos_DefaultTableOptions[EOS_OPTION_FLAG_TO_INDEX(tableOption)].optionType;
        switch (tableOption) {
          /* only set non-default options! */
        case EOS_INSERT_DATA:
          memcpy (&ival, packedTables + sizeUnpacked + size,
                  sizeof (EOS_INTEGER));
          if (ival !=
              eos_DefaultTableOptions[EOS_OPTION_FLAG_TO_INDEX (tableOption)].
              optionValue.ival)
            eosData->tableOptions[j].optionValue.ival = ival;
          size += sizeof (EOS_INTEGER);
          break;

        default:
          memcpy (&bval, packedTables + sizeUnpacked + size,
                  sizeof (EOS_BOOLEAN));
          if (bval !=
              eos_DefaultTableOptions[EOS_OPTION_FLAG_TO_INDEX (tableOption)].
              optionValue.bval) {
            _eos_SetOptionsEosDataMap (me, tableHandles[i], &tableOption,
                                       &bval, &fval, 1, -1, errorCode);
            /* the eosData object might have changed! re-assign it again! */
            eosData = me->dataObjects[me->tableHandlesMap[tableHandles[i]]];
            if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) != EOS_OK)
              return;
          }
          size += sizeof (EOS_BOOLEAN);
          break;
        }
      }
    }                           /* end unpacking options */
    sizeUnpacked += size;
    _EOS_CHECK_PACKEDBYTES ("UNPACK", &index, size, sizeUnpacked, "loading options",
                            tableHandles[i], EOS_FALSE);

    /* now actually unpack the data if it's not already loaded */
    if (eosData->isLoaded) {
      eosData->GetPackedTableSize (eosData, tableHandles[i], &size, &c);
      sizeUnpacked += size;
      _EOS_CHECK_PACKEDBYTES ("UNPACK", &index, size, sizeUnpacked, "data",
                              tableHandles[i], EOS_FALSE);

#ifdef _DEBUG_PACKING_FUNCTIONS
      printf ("-->TH %i", me->tableHandlesMap[tableHandles[i]]);
#endif

      if (! disable_SetPackedTablesPrint) {
	/* print table if requested */
	eos_PrintTableEosDataMap (me, tableHandles[i], "TablesLoaded.dat", errorCode);
	if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) != EOS_OK)
	  return;
      }

      continue;
    }

    eosData->SetPackedTable (eosData, tableHandles[i],
                             packedTables + sizeUnpacked, &c);
    eosData->GetPackedTableSize (eosData, tableHandles[i], &size, &c);
    sizeUnpacked += size;
    c = eos_GetErrorCodeEosDataMap (&gEosDataMap, tableHandles[i]);
    if (eos_GetStandardErrorCodeFromCustomErrorCode(c) != EOS_OK)
      *errorCode = c;
    _EOS_CHECK_PACKEDBYTES ("UNPACK", &index, size, sizeUnpacked, "data", tableHandles[i], EOS_FALSE);

    if (! disable_SetPackedTablesPrint) {
      /* print table if requested */
      eos_PrintTableEosDataMap (me, tableHandles[i], "TablesLoaded.dat", errorCode);
      if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) != EOS_OK)
	return;
    }
  }

  *unpackedTablesSize = sizeUnpacked;

#ifdef _DEBUG_PACKING_FUNCTIONS
  printf ("\n");
#endif

#ifdef DEBUG
  sprintf (fname_tmp, "TablesLoaded_Set_%i.dat", count2);

  /* overwrite output file */
  for (i = 0; i < nTables; i++)
    eosData->Print (me->dataObjects[me->tableHandlesMap[tableHandles[i]]],
                    tableHandles[i], fname_tmp, (i != 0), errorCode);
#endif

#ifdef _DEBUG_PACKING_FUNCTIONS
  eos_SetPackedTablesIndex0 = index;
#endif
}


/*************************************************************************
 *
 * Function Name: eos_PrintTableEosDataMap
 *
 * Description:
 * The eos_PrintTableEosDataMap dumps the table data into the specified file.
 *
 * Parameters:
 * eos_DataMap *me  - this pointer (pointer to the instance of type eos_DataMap
 * EOS_INTEGER tableHandle
 * EOS_INTEGER append - whether or not to append to existing file
 * EOS_INTEGER *errorCode
 * EOS_CHAR *fname
 *
 *************************************************************************/
void eos_PrintTableEosDataMap (eos_DataMap *me, EOS_INTEGER tableHandle,
                               EOS_CHAR *fname, EOS_INTEGER *errorCode)
{
  eos_Data *eosData;
  EOS_INTEGER ind1, ind2;
  EOS_BOOLEAN overwrite = EOS_FALSE, append = EOS_FALSE;

  FILE *tableFile = NULL;

  *errorCode = EOS_OK;

  if (!eos_IsHandleValidEosDataMap (me, tableHandle)) {
    *errorCode = EOS_INVALID_TABLE_HANDLE;
    eos_HandleErrorEosDataMap (me, tableHandle, *errorCode);
    return;
  }

  eosData = me->dataObjects[me->tableHandlesMap[tableHandle]];

  ind1 = EOS_GENERAL_OPTION_FLAG_TO_INDEX (EOS_DUMP_DATA);
  ind2 = EOS_GENERAL_OPTION_FLAG_TO_INDEX (EOS_APPEND_DATA);

  if (me->generalOptions[ind1][tableHandle].bval) {
    overwrite = EOS_TRUE;
    append = EOS_FALSE;
  }
  else if (me->generalOptions[ind2][tableHandle].bval) {
    overwrite = EOS_FALSE;
    append = EOS_TRUE;
  }
  else {
    /* print nothing */
    return;
  }

  if (overwrite || ! fileListPrinted) {
    /* print the sesameFiles[] content at the top of the output file */
    int i;

    tableFile = fopen (fname, "w");
    if (tableFile) {
      fprintf (tableFile, "Ordered list of Sesame files searched:\n");
      for(i=0; i<sesameFilesL; i++)
	fprintf (tableFile, "    %s\n", sesameFiles[i]);
      fprintf (tableFile, "\n");
      fclose(tableFile);
    }
    else {
      *errorCode = EOS_OPEN_OUTPUT_FILE_FAILED;
      eos_HandleErrorEosDataMap (me, tableHandle, *errorCode);
    }

    fileListPrinted = EOS_TRUE;
  }

  if (eosData->dumpNotLoadedMsg) {

    EOS_INTEGER ierr = EOS_OK;
    EOS_INTEGER dataType = eos_GetDataTypeFromTableHandle (tableHandle, &ierr);

    tableFile = fopen (fname, "a");
    if (tableFile) {
      fprintf (tableFile,
	       "%sTableHandle=%i matid =%6d Data Type = %s\tNOT LOADED\n",
	       ((append) ? "\n" : ""), tableHandle, eosData->materialID,
	       EOS_TYPE_TO_STRING (dataType));
      fprintf (tableFile, "Description = %s\n",
	       EOS_TYPE_TO_TAB_NAME (dataType));
      fclose(tableFile);
    }
    else {
      *errorCode = EOS_OPEN_OUTPUT_FILE_FAILED;
      eos_HandleErrorEosDataMap (me, tableHandle, *errorCode);
    }

    return;
  }

  if (!eosData->isLoaded) {
    *errorCode = EOS_INVALID_TABLE_HANDLE;
    eos_HandleErrorEosDataMap (me, tableHandle, *errorCode);
    return;
  }

  if (! eosData->dumpNotLoadedMsg) {
    /* append data to output file */
    eosData->Print (eosData, tableHandle, fname, 1, errorCode);
  }

  if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) == EOS_DATA_TYPE_NOT_FOUND) {

    EOS_INTEGER ierr = EOS_OK;
    EOS_INTEGER dataType = eos_GetDataTypeFromTableHandle (tableHandle, &ierr);

    tableFile = fopen (fname, "a");
    if (tableFile) {
      fprintf (tableFile,
	       "%sTableHandle=%i matid =%6d Data Type = %s\tNOT LOADED\n",
	       ((append) ? "\n" : ""), tableHandle, eosData->materialID,
	       EOS_TYPE_TO_STRING (dataType));
      fprintf (tableFile, "Description = %s\n",
	       EOS_TYPE_TO_TAB_NAME (dataType));
      fclose(tableFile);
    }
    else {
      *errorCode = EOS_OPEN_OUTPUT_FILE_FAILED;
      eos_HandleErrorEosDataMap (me, tableHandle, *errorCode);
    }

    return;
  }
}

/*************************************************************************
 *
 * Function Name: eos_CreateTablesEosDataMap
 *
 * Description:
 * The eos_CreateTablesEosTableMap function allocates all memory to store the
 * specified data tables.
 *
 * Input Values:
 * EOS_INTEGER tableType[nAlloc]
 * EOS_INTEGER matID[nAlloc]
 * EOS_INTEGER nTables
 * EOS_INTEGER doCreate, a flag specifying whether or not to call Create() method on a new table
 * EOS_INTEGER updateOnly, typically this is EOS_FALSE, but EOS_TRUE allows the reuse of a handle number
 * EOS_INTEGER userDefinedDataFileIndex, typically this is -1, otherwise it indicates a user-specific sesame data file index
 * EOS_INTEGER isPublic, is a table public or only for internal use? 
 *
 * Returned Value:
 * EOS_INTEGER *tableHandles[nAlloc]
 * EOS_INTEGER *errorCode
 *
 *************************************************************************/

void eos_CreateTablesEosDataMap (eos_DataMap *me, EOS_INTEGER nTables,
                                 EOS_INTEGER tableType[], EOS_INTEGER matID[],
                                 EOS_INTEGER tableHandles[],
                                 EOS_INTEGER doCreate, EOS_INTEGER isPublic,
				 EOS_INTEGER updateOnly, EOS_INTEGER userDefinedDataFileIndex,
                                 EOS_INTEGER *errorCode)
{
  EOS_INTEGER oldAlloc, i, c, newIndex, j;
  EOS_INTEGER makeMonotonicX1, makeMonotonicY1, makeMonotonicX2,
    makeMonotonicY2, dataType1, dataType2;

  eos_Data *eosData = NULL;
  *errorCode = EOS_OK;

  oldAlloc = me->nAlloc;
  eos_SetSizeEosDataMap (me, me->nAlloc + nTables, errorCode);
  if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) != EOS_OK)
    return;

  /* construct tableMap later. Olga */
  for (i = 0; i < nTables; i++) {
    me->nHandles++;
    newIndex = i + oldAlloc;

    me->tableTypes[newIndex] = tableType[i];
#undef MY_DEBUG
#ifdef MY_DEBUG
   printf("tableType[%d] is %d\n", i, tableType[i]);
   printf("newIndex is %d\n", newIndex);
#endif

    if (! updateOnly) {
      me->tableHandlesMap[newIndex] = tableHandles[i] = newIndex;
      me->isHandlePublic[newIndex] = isPublic;

      /*
       * Now figure out if we need to create new dataObject or can re-use an
       * existing one.
       * Check if the table of this material id and record type already exist
       * so we can re-use it -- unless the material id is in the global gMatidMap[]
       * or the dataFileIndex is user defined or selected option(s) set.
       */
      for (j = 0; j < newIndex; j++) {    /* search among previously created tables */

	/* if object is undefined , skip it */
	if (!me->dataObjects[j])
	  continue;

	/* if this handle uses an unshareable object , skip it */
	if (!me->dataObjects[j]->IsaShareableObject(me->dataObjects[j]))
	  continue;

	if (_eos_find_matid_in_gMatidMap(matID[i]) >= 0 ||
	    (me->dataObjects[i] &&
	     _eos_find_userdefined_fileindex_in_gEosDataMapDataObjects(me->dataObjects[i]->dataFileIndex)))
	  continue;
	/* is tableNumber and material id the same and options haven't been changed, then reuse existing table */
#ifdef MY_DEBUG
        //printf("before EOS_TYPE_TO_TAB_NUM tableType %d is %d\n", i, tableType[i]);
#endif
	if (EOS_TYPE_TO_TAB_NUM (tableType[i]) == me->dataObjects[j]->tableNum
	    && matID[i] == me->dataObjects[j]->materialID
	    && _eos_AreOptionsDefaultEosData (me->dataObjects[j])) {
	  me->dataObjects[j]->refCounter++;
	  me->dataObjects[newIndex] = NULL;       /* set the object pointer at this index to NULL, we are using exising object at j */
	  me->tableHandlesMap[newIndex] = j;
	  break;
	}
      }

      if (j < newIndex) {          /* object to re-use was found! */
	/* Set error code if data type is deprecated */
	if (EOS_IS_TYPE_DEPRECATED(tableType[i]) && eos_IsHandleValid(newIndex)) {
	  c = EOS_WARNING;
	  if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) == EOS_OK) *errorCode = c;
	  c = eos_SetCustomErrorMsg(newIndex, c,
				    "EOS_WARNING: data type, %s, is deprecated due to undesirable results, and it will soon be deleted",
				    EOS_TYPE_TO_STRING(tableType[i]));
	  me->errorCodes[newIndex] = c;
	}
	continue;
      }
    }

    /* create new dataObject, assign tableHandle to be an index */
    if (updateOnly) {
      me->tableHandlesMap[tableHandles[i]] = newIndex; /* modify the original handle's map value */
      eos_InvalidateHandle (newIndex);                 /* invalidate the newly-created handle */
      me->errorCodes[tableHandles[i]] = EOS_OK;        /* reset error code */
      eosData = eos_AllocEosData (matID[i], tableHandles[i]);
      eosData->tableHandle = tableHandles[i];
    }
    else
      eosData = eos_AllocEosData (matID[i], newIndex);
    c = eos_GetErrorCodeEosDataMap (&gEosDataMap, tableHandles[i]);
    if (eos_GetStandardErrorCodeFromCustomErrorCode(c) != EOS_OK) {
      /* keep the handle, so the user can get to the error message, but invalidate it */
      *errorCode = c;
      /* invalidate the handle */
      eos_InvalidateHandle (newIndex);
      me->dataObjects[newIndex] = NULL;
      // update the number of table handles in eos_Interpolation class
      eos_SetNumberOfHandles (&gEosInterpolation, nTables, errorCode);
      return;
    }

    if (! updateOnly) {
      eosData->tableHandle = newIndex;
      me->tableHandlesMap[newIndex] = newIndex;
    }
    eosData->tableNum = EOS_TYPE_TO_TAB_NUM (tableType[i]);
    /* insert the pair into a tableMap */
    me->dataObjects[newIndex] = eosData;
    eosData->refCounter++;
    if (userDefinedDataFileIndex > -1) {
      eosData->userDefinedDataFile = EOS_TRUE;
      eosData->dataFileIndex = userDefinedDataFileIndex;
    }
    if (doCreate)
      eosData->Create (eosData, tableHandles[i]);
    /* check for errors */
    c = eos_GetErrorCodeEosDataMap (&gEosDataMap, tableHandles[i]);
    if (eos_GetStandardErrorCodeFromCustomErrorCode(c) != EOS_OK && c != EOS_WARNING) {
      if (eos_GetStandardErrorCodeFromCustomErrorCode(c) == EOS_MATERIAL_NOT_FOUND) {
	// DAP - do not return error code so that eos_LoadTables can decide what to do in the case
	//       that a Sesame material is missing.
	*errorCode = EOS_OK;
      }
      else {
	*errorCode = c;
	/* invalidate the handle */
	eos_InvalidateHandle (newIndex);
	me->dataObjects[newIndex] = NULL;
	eos_DestroyEosData (eosData);
	free (eosData);
      }
    }
    /* Set error code if data type is deprecated */
    if (EOS_IS_TYPE_DEPRECATED(tableType[i]) &&
	eos_IsHandleValid(newIndex) && me->tableHandlesMap[newIndex] < me->nAlloc) {
      c = EOS_WARNING;
      c = eos_SetCustomErrorMsg(newIndex, c,
				"EOS_WARNING: data type, %s, is deprecated due to undesirable results, and it will soon be deleted",
				EOS_TYPE_TO_STRING(tableType[i]));
      me->errorCodes[newIndex] = c;
    }
    me->nTables++;
  }

  /* check if the table should be monotonic  in X or Y */
  c = EOS_OK;
  for (i = 0; i < nTables; i++) {
    if (! eos_IsHandleValid(tableHandles[i]))
      continue;                 /* ignore invalid handles */

    eosData = me->dataObjects[me->tableHandlesMap[tableHandles[i]]];
    if (! eosData)
      continue;                 /* ignore invalid objects */

    eos_ShouldBeMonotonic (eosData, tableType[i], &makeMonotonicX1,
                           &makeMonotonicY1, &makeMonotonicX2,
                           &makeMonotonicY2, &dataType1, &dataType2);
    if (dataType1 > 0 && makeMonotonicX1)
      /* this can re-map data object */
      eos_SetOptionEosDataMap (me, tableHandles[i], EOS_MONOTONIC_IN_X,
                               EOS_TRUE, dataType1, &c);
    if (dataType2 > 0 && makeMonotonicX2)
      /* this can re-map data object */
      eos_SetOptionEosDataMap (me, tableHandles[i], EOS_MONOTONIC_IN_X,
                               EOS_TRUE, dataType2, &c);
    if (dataType1 > 0 && makeMonotonicY1)
      /* this can re-map data object */
      eos_SetOptionEosDataMap (me, tableHandles[i], EOS_MONOTONIC_IN_Y,
                               EOS_TRUE, dataType1, &c);
    if (dataType2 > 0 && makeMonotonicY2)
      /* this can re-map data object */
      eos_SetOptionEosDataMap (me, tableHandles[i], EOS_MONOTONIC_IN_Y,
                               EOS_TRUE, dataType2, &c);
  }
  if (eos_GetStandardErrorCodeFromCustomErrorCode(c) != EOS_OK)
    *errorCode = c;

  // update the number of table handles in eos_Interpolation class
  c = EOS_OK;
  eos_SetNumberOfHandles (&gEosInterpolation, nTables, &c);
  if (eos_GetStandardErrorCodeFromCustomErrorCode(c) != EOS_OK)
    *errorCode = c;
}

/************************************************************************
 * 
 * get-method for the error code stored in eos_DataMap
 * 
 * Returned Values: 
 * EOS_INTEGER error code
 *
 * Input Value:
 * eos_DataMap *me  - this pointer the instance of type eos_DataMap
 * EOS_INTEGER tableHandle
 * 
 ************************************************************************/
EOS_INTEGER eos_GetErrorCodeEosDataMap (eos_DataMap *me,
                                        EOS_INTEGER tableHandle)
{
  if (!eos_IsHandleValidEosDataMap (me, tableHandle)) {
    ((eos_ErrorHandler *) me)->HandleError (me, tableHandle,
					    eos_SetCustomErrorCode(tableHandle, EOS_INVALID_TABLE_HANDLE));
  }

  return me->errorCodes[tableHandle];
}

/************************************************************************
 * 
 * get-method for the eos_Data (one table)
 * 
 * Returned Values: 
 * eos_Data * (pointer to a table)
 * EOS_INTEGER *dataType
 * EOS_INTEGER *errorCode
 *
 * Input Value:
 * eos_DataMap *me  - this pointer the instance of type eos_DataMap
 * EOS_INTEGER tableHandle
 * 
 ************************************************************************/
eos_Data *eos_GetEosDataEosDataMap (eos_DataMap *me, EOS_INTEGER tableHandle,
                                    EOS_INTEGER *dataType,
                                    EOS_INTEGER *errorCode)
{
  if (!eos_IsHandleValidEosDataMap (me, tableHandle)) {
    *errorCode = EOS_INVALID_TABLE_HANDLE;
    eos_HandleErrorEosDataMap (me, tableHandle, EOS_INVALID_TABLE_HANDLE);
    return NULL;
  }
  *errorCode = EOS_OK;
  *dataType = me->tableTypes[tableHandle];
  return me->dataObjects[me->tableHandlesMap[tableHandle]];
}

/************************************************************************
 * 
 * helping internal function that .
 * 
 * Returned Values:
 * eos_Data*   data object pointer returned as function value
 * EOS_INTEGER error code returned via argument list
 *
 * Input Value:
 * eos_DataMap *me  - pointer to an instance of type eos_Data.
 * EOS_INTEGER tableHandle1
 * EOS_INTEGER tableHandle2
 * EOS_INTEGER optFlags[] array of options to set 
 * EOS_BOOLEAN bval[] - boolean option values
 * EOS_REAL    fval[] - real option values to set
 * EOS_INTEGER *numOptions
 * EOS_INTEGER alternativeDataType - this is used to set monotonicity for the subtypes of categories 3 and 4
 *                                   if > 0, the alternativeDataType is passed into _eos_AreOptionsCompatibleEosData()
 *                                   instead of the dataType for the givenDataHandle. It is also passed into eos_SetOptionEosData
 *                                   and ultimately into eosData->SetMonotonicity()
 * 
 ************************************************************************/
eos_Data* _eos_RemapEosDataMap (eos_DataMap *me, EOS_INTEGER tableHandle1, EOS_INTEGER tableHandle2,
				EOS_INTEGER *optFlags,
				EOS_BOOLEAN *bval,
				EOS_REAL *fval, EOS_INTEGER numOptions, EOS_INTEGER alternativeDataType, 
				EOS_INTEGER *errorCode) {

  EOS_INTEGER materialID, c, moveToIndex, i, myNewIndex = -1;
  eos_Data *eosData1 = NULL, *eosData2 = NULL, *newEosData = NULL;
  EOS_BOOLEAN areOptionsCompatible = EOS_FALSE;

  if (!eos_IsHandleValidEosDataMap (me, tableHandle1)) {
    *errorCode = EOS_INVALID_TABLE_HANDLE;
    eos_HandleErrorEosDataMap (me, tableHandle1, EOS_INVALID_TABLE_HANDLE);
    return NULL;
  }
  if (!eos_IsHandleValidEosDataMap (me, tableHandle2)) {
    *errorCode = EOS_INVALID_TABLE_HANDLE;
    eos_HandleErrorEosDataMap (me, tableHandle2, EOS_INVALID_TABLE_HANDLE);
    return NULL;
  }

  *errorCode = EOS_OK;

  if (me->tableHandlesMap[tableHandle1] == _EOS_NEWLY_INITIALIZED_TABLE_HANDLE_VALUE ||
      me->tableHandlesMap[tableHandle2] == _EOS_NEWLY_INITIALIZED_TABLE_HANDLE_VALUE)
    return NULL;

  eosData1 = me->dataObjects[me->tableHandlesMap[tableHandle1]];
  eosData2 = me->dataObjects[me->tableHandlesMap[tableHandle2]];

  areOptionsCompatible = _eos_AreOptionsCompatibleEosData (eosData1, eosData2,
							   me->tableTypes[tableHandle1],
							   alternativeDataType, optFlags, bval,
							   numOptions);

  if (eosData1->refCounter > 1 && (!areOptionsCompatible) ) {
    /* we share the eosData * object with another tableHandle, but we shouldn't */

    /* if the eosData object is in fact OURS, we have to MOVE it to another tableHandle that shares it 
       with us so we can create a new object for ourselves */
    if (me->dataObjects[tableHandle1] != NULL) { /* this is the same as me->tableHandlesMap[tableHandle1] = tableHandle1 */
      moveToIndex = 0;
      /* find the first tableHandle that points to our object, that's where 
         we want to move the object to */
      while (moveToIndex == tableHandle1
             || me->tableHandlesMap[moveToIndex] != tableHandle1)
        moveToIndex++;
      me->dataObjects[moveToIndex] = eosData1;
      me->dataObjects[tableHandle1] = NULL;
      /* now re-assign the mapping for ALL who share */
      for (i = moveToIndex; i < me->nAlloc; i++) {
        if (me->tableHandlesMap[i] == tableHandle1)
          me->tableHandlesMap[i] = moveToIndex;
      }
      /* find the first tableHandle that points to our object, that's where 
         we want to move the object to */
    }

    if (myNewIndex < 0) {       /* if existing matching table not found */
      materialID = eosData1->materialID;
      /* create new dataObject, assign tableHandle to be an index */
      newEosData = eos_AllocEosData (materialID, tableHandle1);
      me->dataObjects[tableHandle1] = newEosData;
      /* copy loading options */
      _eos_CopyOptionsEosData (newEosData, eosData1);
      /* don't need to copy general options, because they are stored per handle and we don't share them with anyone */

      myNewIndex = tableHandle1;
      newEosData->tableHandle = tableHandle1;
      newEosData->Create (newEosData, tableHandle1);
      me->nTables++;
    }

    /* assign re-used or newly created eosData object and re-map */
    eosData1->refCounter--;      /* decrease counter for the eosData we used to point to */
    newEosData->refCounter++;   /* increase the counter for the new one */
    me->tableHandlesMap[tableHandle1] = myNewIndex;

    /* check for errors */
    c = eos_GetErrorCodeEosDataMap (&gEosDataMap, tableHandle1);
    if (eos_GetStandardErrorCodeFromCustomErrorCode(c) != EOS_OK)
      *errorCode = c;

    eosData1 = newEosData;
  }                             /* create new object or re-use another existing one that matches, if this object is shared */

  return eosData1;
}

/************************************************************************
 * 
 * helping internal function that set table-specific LOADING options.
 * 
 * Returned Values:
 * EOS_INTEGER error code
 *
 * Input Value:
 * eos_DataMap *me  - pointer to an instance of type eos_Data.
 * EOS_INTEGER tableHandle
 * EOS_INTEGER optFlags[] array of options to set 
 * EOS_BOOLEAN bval[] - boolean option values
 * EOS_REAL    fval[] - real option values to set
 * EOS_INTEGER *numOptions
 * EOS_INTEGER alternativeDataType - this is used to set monotonicity for the subtypes of categories 3 and 4
 *                                   if > 0, the alternativeDataType is passed into _eos_AreOptionsCompatibleEosData()
 *                                   instead of the dataType for the givenDataHandle. It is also passed into eos_SetOptionEosData
 *                                   and ultimately into eosData->SetMonotonicity()
 * 
 ************************************************************************/
void _eos_SetOptionsEosDataMap (eos_DataMap *me, EOS_INTEGER tableHandle,
                                EOS_INTEGER *optFlags, EOS_BOOLEAN *bval,
                                EOS_REAL *fval, EOS_INTEGER numOptions,
                                EOS_INTEGER alternativeDataType,
                                EOS_INTEGER *errorCode)
{
  eos_Data *eosData = NULL;
  EOS_INTEGER j, i, dataType;
  eos_OptionValue *optVal1, *optVal2, *optVal3, *optVal4;
  *errorCode = EOS_OK;

  if (!eos_IsHandleValidEosDataMap (me, tableHandle)) {
    *errorCode = EOS_INVALID_TABLE_HANDLE;
    eos_HandleErrorEosDataMap (me, tableHandle, EOS_INVALID_TABLE_HANDLE);
    return;
  }

  *errorCode = EOS_OK;
  eosData = me->dataObjects[me->tableHandlesMap[tableHandle]];
  dataType = me->tableTypes[me->tableHandlesMap[tableHandle]];

  /* verify EOS_PT_SMOOTHING is not set concurrently with any of
     EOS_MONOTONIC_IN_X, EOS_MONOTONIC_IN_Y, EOS_SMOOTH and EOS_INVERT_AT_SETUP.
   */
  for (j = 0; j < numOptions; j++) {
    switch (optFlags[j]) {
    case EOS_ADJUST_VAP_PRES:
      optVal1 = _eos_getOptionEosData (eosData, EOS_PT_SMOOTHING);
      if (!optVal1->bval) {
        *errorCode = EOS_INVALID_OPTION_FLAG;
        eos_HandleErrorEosDataMap (me, tableHandle, *errorCode);
        *errorCode = eos_SetCustomErrorMsg (tableHandle, *errorCode,
					    "EOS_INVALID_OPTION_FLAG: The option flag passed into eos_SetOption() is invalid because EOS_ADJUST_VAP_PRES can be used only with conjunction to EOS_PT_SMOOTHING");
        return;
      }
      break;
    case EOS_PT_SMOOTHING:
      /* get monotonicity options' settings */
      optVal1 = _eos_getOptionEosData (eosData, EOS_MONOTONIC_IN_X);
      optVal2 = _eos_getOptionEosData (eosData, EOS_MONOTONIC_IN_Y);
      optVal3 = _eos_getOptionEosData (eosData, EOS_SMOOTH);
      optVal4 = _eos_getOptionEosData (eosData, EOS_INVERT_AT_SETUP);
      if (EOS_TYPE_TO_TAB_NUM (dataType) != 301 || EOS_TYPE_TO_SUB_TAB_NUM (dataType) > 2) {    /* check for valid types for this option! */
        *errorCode = EOS_INVALID_OPTION_FLAG;
        eos_HandleErrorEosDataMap (me, tableHandle, *errorCode);
        *errorCode = eos_SetCustomErrorMsg (tableHandle, *errorCode,
					    "EOS_INVALID_OPTION_FLAG: The option flag passed into eos_SetOption() is invalid because it is only applicable to total pressure and total internal energy data (i.e., Sesame 301 tables)");
        return;
      }
      if (optVal1->bval || optVal2->bval || optVal3->bval || optVal4->bval) {    /* not compatible! */
        *errorCode = EOS_INVALID_OPTION_FLAG;
        eos_HandleErrorEosDataMap (me, tableHandle, *errorCode);
        *errorCode = eos_SetCustomErrorMsg (tableHandle, *errorCode,
					    "EOS_INVALID_OPTION_FLAG: The option flag passed into eos_SetOption() is invalid because it conflicts a previously-set value of EOS_MONOTONIC_IN_X, EOS_MONOTONIC_IN_Y, EOS_SMOOTH or EOS_INVERT_AT_SETUP");
        return;
      }
      /* reset subtable-specific ptsmooth flags */
      eosData->SetSmoothing (eosData, dataType, 0, 1);
      break;
    case EOS_SMOOTH:
      if (EOS_TYPE_TO_TAB_NUM (dataType) != 301 &&
	  EOS_TYPE_TO_TAB_NUM (dataType) != 303 &&
	  EOS_TYPE_TO_TAB_NUM (dataType) != 304 &&
	  EOS_TYPE_TO_TAB_NUM (dataType) != 305) {    /* check for valid types for this option! */
        *errorCode = EOS_INVALID_OPTION_FLAG;
        /* do not set handle-specific errorCode using eos_HandleErrorEosDataMap */
        *errorCode = eos_SetCustomErrorMsg (tableHandle, *errorCode,
					    "EOS_INVALID_OPTION_FLAG: The option flag passed into eos_SetOption() is invalid and ignored because it is incompatible with the specified data table type");
        return;
      }
      /* the "break" statement is intentionally missing here */
    case EOS_MONOTONIC_IN_X:
    case EOS_MONOTONIC_IN_Y:
      optVal1 = _eos_getOptionEosData (eosData, EOS_PT_SMOOTHING);
      if (optVal1->bval) {      /* not compatible! */
        *errorCode = EOS_INVALID_OPTION_FLAG;
        eos_HandleErrorEosDataMap (me, tableHandle, *errorCode);
        *errorCode = eos_SetCustomErrorMsg (tableHandle, *errorCode,
					    "EOS_INVALID_OPTION_FLAG: The option flag passed into eos_SetOption() is invalid because it conflicts a previously-set value of EOS_PT_SMOOTHING");
        return;
      }
      break;
    case EOS_USE_MAXWELL_TABLE:
      if (EOS_SUBCATEGORY (dataType) != EOS_TOTAL) {    /* check for valid types for this option! */
        *errorCode = EOS_INVALID_OPTION_FLAG;
        eos_HandleErrorEosDataMap (me, tableHandle, *errorCode);
        *errorCode = eos_SetCustomErrorMsg (tableHandle, *errorCode,
					    "EOS_INVALID_OPTION_FLAG: The option flag, %s, passed into eos_SetOption() is invalid and ignored because it is incompatible with the specified data table type, %s",
					    get_OptionFlagStr(optFlags[j]), EOS_TYPE_TO_STRING (dataType));
        return;
      }
      break;
    case EOS_INVERT_AT_SETUP:
      if (EOS_CATEGORY (dataType) == EOS_CATEGORY0) {    /* check for valid types for this option! */
        *errorCode = EOS_INVALID_OPTION_FLAG;
        eos_HandleErrorEosDataMap (me, tableHandle, *errorCode);
        *errorCode = eos_SetCustomErrorMsg (tableHandle, *errorCode,
					    "EOS_INVALID_OPTION_FLAG: The option flag, %s, passed into eos_SetOption() is invalid and ignored because it is incompatible with the specified data table type, %s",
					    get_OptionFlagStr(optFlags[j]), EOS_TYPE_TO_STRING (dataType));
        return;
      }
      break;
    }
  }

  /* if changing the existing option, check if there are other tableHandles using our eosData *
     object, and if there are, create a new eosData * object and re-map. */
  /* check only boolean options because only boolean options affect loading */
  eosData = _eos_RemapEosDataMap (me, tableHandle, tableHandle,
				  optFlags, bval, fval, numOptions, alternativeDataType, 
				  errorCode);

  for (i = 0; i < numOptions; i++) {
    /* process only loading options */
    if (bval[i])
      eos_SetOptionEosData (eosData, me->tableTypes[tableHandle],
                            alternativeDataType, optFlags[i], fval[i],
                            errorCode);
    else                        /* set to default */
      eos_ResetOptionEosData (eosData, me->tableTypes[tableHandle],
                              alternativeDataType, optFlags[i], errorCode);
  }

}

/************************************************************************
 * 
 * virtual method function that set table-specific options.
 * 
 * Returned Values:
 * EOS_INTEGER error code
 *
 * Input Value:
 * eos_DataMap *me  - pointer to an instance of type eos_Data.
 * EOS_INTEGER tableHandle
 * EOS_INTEGER tableOption  - option to set
 * EOS_REAL    tableOptionValue - optional option value to set
 * EOS_INTEGER alternativeDataType - this is used to set monotonicity for the subtypes of categories 3 and 4
 *                                   if > 0, the alternativeDataType is passed into _eos_AreOptionsCompatibleEosData()
 *                                   instead of the dataType for the givenDataHandle. It is also passed into eos_SetOptionEosData
 *                                   and ultimately into eosData->SetMonotonicity()
 *                                   if alternative data type is the same as ours, set to -1.
 * 
 ************************************************************************/
void eos_SetOptionEosDataMap (eos_DataMap *me, EOS_INTEGER tableHandle,
                              EOS_INTEGER tableOption,
                              EOS_REAL tableOptionVal,
                              EOS_INTEGER alternativeDataType,
                              EOS_INTEGER *errorCode)
{
  EOS_BOOLEAN bval = EOS_TRUE;
  EOS_INTEGER ind, ind2, flag = tableOption;
  *errorCode = EOS_OK;

  if (!eos_IsHandleValidEosDataMap (me, tableHandle)) {
    *errorCode = EOS_INVALID_TABLE_HANDLE;
    eos_HandleErrorEosDataMap (me, tableHandle, EOS_INVALID_TABLE_HANDLE);
    return;
  }

  /* first see if it is general option */
  if (EOS_IS_GENERAL_OPTION (flag)) {
    ind = EOS_GENERAL_OPTION_FLAG_TO_INDEX (flag);
    switch (flag) {
    case EOS_X_CONVERT:
    case EOS_Y_CONVERT:
    case EOS_F_CONVERT:
      me->generalOptions[ind][tableHandle].rval = (EOS_REAL) tableOptionVal;
      break;
    case EOS_APPEND_DATA:
      me->generalOptions[ind][tableHandle].bval = (EOS_BOOLEAN) EOS_TRUE;
      ind2 = EOS_GENERAL_OPTION_FLAG_TO_INDEX (EOS_DUMP_DATA);
      me->generalOptions[ind2][tableHandle].bval = (EOS_BOOLEAN) EOS_FALSE;
      break;
    case EOS_DUMP_DATA:
      me->generalOptions[ind][tableHandle].bval = (EOS_BOOLEAN) EOS_TRUE;
      ind2 = EOS_GENERAL_OPTION_FLAG_TO_INDEX (EOS_APPEND_DATA);
      me->generalOptions[ind2][tableHandle].bval = (EOS_BOOLEAN) EOS_FALSE;
      break;
    default:
      me->generalOptions[ind][tableHandle].bval = (EOS_BOOLEAN) EOS_TRUE;
    }
  }
  else if (EOS_IS_LOADING_OPTION (flag)) {      /* loading option */
    _eos_SetOptionsEosDataMap (me, tableHandle, &flag, &bval, &tableOptionVal,
                               1, alternativeDataType, errorCode);
  }
  else                          /* not a valid option */
    *errorCode = EOS_INVALID_OPTION_FLAG;
}

/************************************************************************
 * 
 * virtual method function that reset table-specific options to default.
 * 
 * Returned Values:
 * EOS_INTEGER error code
 *
 * Input Value:
 * eos_DataMap *me  - pointer to an instance of type eos_Data.
 * EOS_INTEGER tableHandle
 * EOS_INTEGER alternativeDataType - this is used to set monotonicity for the subtypes of categories 3 and 4
 *                                   if > 0, the alternativeDataType is passed into _eos_AreOptionsCompatibleEosData()
 *                                   instead of the dataType for the givenDataHandle. It is also passed into eos_SetOptionEosData
 *                                   and ultimately into eosData->SetMonotonicity()
 * EOS_INTEGER tableOption  - option to set
 * 
 ************************************************************************/
void eos_ResetOptionEosDataMap (eos_DataMap *me, EOS_INTEGER tableHandle,
                                EOS_INTEGER tableOption,
                                EOS_INTEGER alternativeDataType,
                                EOS_INTEGER *errorCode)
{
  EOS_INTEGER ind, ind2, ind3, flag = tableOption;
  EOS_BOOLEAN bval = EOS_FALSE;
  EOS_REAL rval = 0.0;
  extern EOS_BOOLEAN enable_DEBUG_PRINT;

  *errorCode = EOS_OK;
  if (!eos_IsHandleValidEosDataMap (me, tableHandle)) {
    *errorCode = EOS_INVALID_TABLE_HANDLE;
    eos_HandleErrorEosDataMap (me, tableHandle, EOS_INVALID_TABLE_HANDLE);
    return;
  }
  if (alternativeDataType < 0)
    alternativeDataType = me->tableTypes[tableHandle];

  /* first see if it is general option */
  if (!EOS_IS_LOADING_OPTION (flag)) {
    ind = EOS_GENERAL_OPTION_FLAG_TO_INDEX (flag);
    ind2 = EOS_OPTION_FLAG_TO_INDEX (flag);
    /* get the default option */
    bval = eos_DefaultTableOptions[ind2].optionValue.bval;
    rval = eos_DefaultTableOptions[ind2].optionValue.rval;

    switch (flag) {
    case EOS_X_CONVERT:
    case EOS_Y_CONVERT:
    case EOS_F_CONVERT:
      me->generalOptions[ind][tableHandle].rval = rval;
      break;
    case EOS_APPEND_DATA:
      me->generalOptions[ind][tableHandle].bval = bval;
      ind2 = EOS_GENERAL_OPTION_FLAG_TO_INDEX (EOS_DUMP_DATA);
      ind3 = EOS_OPTION_FLAG_TO_INDEX (EOS_DUMP_DATA);
      me->generalOptions[ind2][tableHandle].bval =
        eos_DefaultTableOptions[ind3].optionValue.bval;
      break;
    case EOS_DUMP_DATA:
      me->generalOptions[ind][tableHandle].bval = bval;
      ind2 = EOS_GENERAL_OPTION_FLAG_TO_INDEX (EOS_APPEND_DATA);
      ind3 = EOS_OPTION_FLAG_TO_INDEX (EOS_APPEND_DATA);
      me->generalOptions[ind2][tableHandle].bval =
        eos_DefaultTableOptions[ind3].optionValue.bval;
      break;
    case EOS_DEBUG_PRINT:
      me->generalOptions[ind][tableHandle].bval = enable_DEBUG_PRINT = bval;
      ind2 = EOS_GENERAL_OPTION_FLAG_TO_INDEX (EOS_DEBUG_PRINT);
      ind3 = EOS_OPTION_FLAG_TO_INDEX (EOS_DEBUG_PRINT);
      me->generalOptions[ind2][tableHandle].bval =
        eos_DefaultTableOptions[ind3].optionValue.bval;
      break;
    default:
      me->generalOptions[ind][tableHandle].bval = bval;
    }
  }
  else                          /* loading option */
    _eos_SetOptionsEosDataMap (me, tableHandle, &flag, &bval, &rval, 1,
                               alternativeDataType, errorCode);
}

/*************************************************************************
 *
 * Function Name: eos_GetTableMetaData
 *
 * Description:
 * The eos_GetTableMetaData routine is returns a string representation of a specified
 * meta datum associated with a table handle. Information is requested by passing a
 * table handle and a requested info item flag to the routine. The corresponding meta
 * data is returned as a character string.
 *
 * Input Values:
 * eos_DataMap *me          - this pointer (pointer to the instance of type eos_DataMap)
 * EOS_INTEGER tableHandle
 * EOS_INTEGER infoItem
 *
 * Returned Values:
 * EOS_CHAR    infoStr[EOS_META_DATA_STRLEN]
 * EOS_INTEGER *errorCode
 *
 *************************************************************************/
void FUNC_INTER eos_GetTableMetaDataEosDataMap (eos_DataMap *me, EOS_INTEGER tableHandle,
						EOS_INTEGER infoItem, EOS_CHAR *infoStr,
						EOS_INTEGER *errorCode)
{
  EOS_INTEGER i;
  eos_Data *eosData = NULL;
  EOS_CHAR *s = NULL;

  if (!eos_IsHandleValidEosDataMap (me, tableHandle)) {
    *errorCode = EOS_INVALID_TABLE_HANDLE;
    eos_HandleErrorEosDataMap (me, tableHandle, EOS_INVALID_TABLE_HANDLE);
    return;
  }

  switch (infoItem) {

  case EOS_File_Name:

    /* get the name of the associated sesame file */
    eosData = me->dataObjects[me->tableHandlesMap[tableHandle]];
    s = _eos_GetSesameFileName(eosData);
    i = MIN(strlen(s), EOS_META_DATA_STRLEN-1);
    //printf("MIN(strlen(s)+1, EOS_META_DATA_STRLEN-1) = %d\n", i);
    strncpy ( infoStr, s, i );
    infoStr[i] = '\0';
    break;

  case EOS_Material_Name:
  case EOS_Material_Source:
  case EOS_Material_Date:
  case EOS_Material_Ref:
  case EOS_Material_Composition:
  case EOS_Material_Codes:
  case EOS_Material_Phases:

    eosData = me->dataObjects[me->tableHandlesMap[tableHandle]];
    eosData->GetTableMetaData(eosData, infoItem, infoStr, errorCode);
    break;

  default:

    *errorCode = EOS_INVALID_INFO_FLAG;
    eos_HandleErrorEosDataMap (me, tableHandle, EOS_INVALID_INFO_FLAG);
    break;

  }
}

/************************************************************************
 * 
 * returns information items for the table.
 * 
 * Returned Values: EOS_INTEGER *err - output error code
 *
 * Input Value:
 * eos_DataMap *me          - this pointer (pointer to the instance of type eos_DataMap)
 * EOS_INTEGER tableHandle
 * EOS_INTEGER *infoItems   - array of requested items
 * EOS_REAL *infoVals       - return item values
 * EOS_INTEGER dataType     - dataType for which items requested
 * EOS_INTEGER *err         - error flag
 * 
 ************************************************************************/
void FUNC_INTER eos_GetTableInfoEosDataMap (eos_DataMap * me, EOS_INTEGER tableHandle, EOS_INTEGER numInfoItems,
					    EOS_INTEGER * infoItems, EOS_REAL * infoVals, EOS_INTEGER * errorCode)
{
  eos_Data *eosData = NULL;

  if (!eos_IsHandleValidEosDataMap (me, tableHandle)) {
    *errorCode = eos_GetErrorCodeEosDataMap (me, tableHandle);
    return;
  }

  *errorCode = EOS_OK;
  eosData = me->dataObjects[me->tableHandlesMap[tableHandle]];
  eosData->GetTableInfo (eosData, tableHandle, numInfoItems, infoItems,
                         infoVals, errorCode);
}

/*************************************************************************
 *
 * Function eos_GetTableCmnts
 * Description:
 * The eos_GetTableCmnts routine returns the comments available about the
 * requested data table. This routine works on only a single data table.
 * Before calling this routine the host code must call eos_GetTableInfo to
 * find out the length of the comments, lenCmnts, allowing the host code
 * to allocate adequate storage.
 * Works on only a type 4 data table.
 *
 * Input Values:
 * eos_DataMap *me          - this pointer (pointer to the instance of type eos_DataMap)
 * EOS_INTEGER tableHandle
 * EOS_CHAR *cmntStr  - ALLOCATED string to hold table comments, the assumption
 *                      is made that the user has called eos_GettableInfo() for the
 *                      same table handle, and have allocated the comment string to
 *                      be the size returned from eos_GettableInfo().
 *
 * Returned Values:
 * EOS_INTEGER *errorCode
 *
 *************************************************************************/
void eos_GetTableCmntsEosDataMap (eos_DataMap *me, EOS_INTEGER tableHandle,
                                  EOS_CHAR *cmntStr, EOS_INTEGER *errorCode)
{
  eos_Data *eosData = NULL;

  if (!eos_IsHandleValidEosDataMap (me, tableHandle)) {
    eos_HandleErrorEosDataMap (me, tableHandle, EOS_INVALID_TABLE_HANDLE);
    *errorCode = eos_GetErrorCodeEosDataMap (me, tableHandle);
    return;
  }

  *errorCode = EOS_OK;
  eosData = me->dataObjects[me->tableHandlesMap[tableHandle]];
  if (eosData->GetTableCmnts)
    eosData->GetTableCmnts (eosData, cmntStr, errorCode);
  else
    *errorCode = EOS_BAD_DATA_TYPE;
}

 /*************************************************************************
 *
 * Function eos_GetLoadedBulkDataEosDataMap
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
 * eos_DataMap *me
 *************************************************************************/
void eos_GetLoadedBulkDataEosDataMap (eos_DataMap *me,
                                      EOS_INTEGER tableHandle, EOS_REAL *zbar,
                                      EOS_REAL *abar, EOS_REAL *dens0,
                                      EOS_INTEGER *errorCode)
{
  eos_Data *eosData = NULL;
  *errorCode = EOS_OK;

  if (!eos_IsHandleValidEosDataMap (me, tableHandle)) {
    *errorCode = EOS_INVALID_TABLE_HANDLE;
    eos_HandleErrorEosDataMap (me, tableHandle, EOS_INVALID_TABLE_HANDLE);
    return;
  }

  eosData = me->dataObjects[me->tableHandlesMap[tableHandle]];
  if (eosData && eosData->GetLoadedBulkData)
    eosData->GetLoadedBulkData (eosData, zbar, abar, dens0, errorCode);
  else
    *errorCode = EOS_BAD_DATA_TYPE;
}

 /*************************************************************************
 *
 * Function eos_GetOptionEosDataMap
 * Description:
 * 
 * Return an option Value for given handle and option flag.
 * 
 * Returned Values:
 * EOS_INTEGER errorCode - output error code
 * eos_OptionValue **optVal;
 *
 * Input Value:
 * eos_DataMap *me
 * EOS_INTEGER tableOption        - option flag
 *************************************************************************/
void eos_GetOptionEosDataMap (eos_DataMap *me, EOS_INTEGER tableHandle,
                              EOS_INTEGER tableOption,
                              eos_OptionValue **optVal,
                              EOS_INTEGER *errorCode)
{
  EOS_INTEGER ind;
  eos_Data *eosData = NULL;
  *errorCode = EOS_OK;

  if (!eos_IsHandleValidEosDataMap (me, tableHandle)) {
    *errorCode = EOS_INVALID_TABLE_HANDLE;
    eos_HandleErrorEosDataMap (me, tableHandle, EOS_INVALID_TABLE_HANDLE);
    return;
  }

  if (!EOS_IS_LOADING_OPTION (tableOption)) {
    ind = EOS_GENERAL_OPTION_FLAG_TO_INDEX (tableOption);       /* get total opt index from j */
    *optVal = &(me->generalOptions[ind][tableHandle]);
  }
  else {                        /* loading option */

    eosData = me->dataObjects[me->tableHandlesMap[tableHandle]];
    *optVal = _eos_getOptionEosData (eosData, tableOption);
  }
}

/*************************************************************************
 *
 * Function eos_SetSizeEosDataMap
 * Description:
 * Allocates or re-allocates enough space to store nhandles handles.
 * 
 * Returned Values:
 * EOS_INTEGER errorCode - output error code
 *
 * Input Value:
 * eos_DataMap *me
 * EOS_INTEGER nhandles; - number of handles to store
 *************************************************************************/
void eos_SetSizeEosDataMap (eos_DataMap *me, EOS_INTEGER nhandles,
                            EOS_INTEGER *errorCode)
{
  EOS_INTEGER oldAlloc, i, of, j, k, ind;

  oldAlloc = me->nAlloc;
  me->nAlloc = nhandles;
  if (!me->dataObjects) {
    me->dataObjects = (eos_Data **) malloc (sizeof (eos_Data *) * me->nAlloc);
    me->tableHandlesMap =
      (EOS_INTEGER *) malloc (sizeof (EOS_INTEGER) * me->nAlloc);
    me->tableTypes =
      (EOS_INTEGER *) malloc (sizeof (EOS_INTEGER) * me->nAlloc);
    me->errorCodes =
      (EOS_INTEGER *) malloc (sizeof (EOS_INTEGER) * me->nAlloc);
    me->customErrorMsg =
      (EOS_CHAR ***) malloc (sizeof (EOS_CHAR**) * me->nAlloc);
    me->isHandlePublic =
      (EOS_INTEGER *) malloc (sizeof (EOS_INTEGER) * me->nAlloc);
    for (i = 0; i < EOS_NUM_GENERAL_OPTIONS; i++)
      me->generalOptions[i] =
        (eos_OptionValue *) malloc (sizeof (eos_OptionValue) * me->nAlloc);
  }
  else {
    me->dataObjects =
      (eos_Data **) realloc (me->dataObjects,
                             sizeof (eos_Data *) * me->nAlloc);
    me->tableHandlesMap =
      (EOS_INTEGER *) realloc (me->tableHandlesMap,
                               sizeof (EOS_INTEGER) * me->nAlloc);
    me->tableTypes =
      (EOS_INTEGER *) realloc (me->tableTypes,
                               sizeof (EOS_INTEGER) * me->nAlloc);
    me->errorCodes =
      (EOS_INTEGER *) realloc (me->errorCodes,
                               sizeof (EOS_INTEGER) * me->nAlloc);
    me->customErrorMsg =
      (EOS_CHAR ***) realloc (me->customErrorMsg,
                              sizeof (EOS_CHAR**) * me->nAlloc);
    me->isHandlePublic =
      (EOS_INTEGER *) realloc (me->isHandlePublic,
                               sizeof (EOS_INTEGER) * me->nAlloc);
    for (i = 0; i < EOS_NUM_GENERAL_OPTIONS; i++)
      me->generalOptions[i] =
        (eos_OptionValue *) realloc (me->generalOptions[i],
                                     sizeof (eos_OptionValue) * me->nAlloc);
  }

  /* initialize all the new tables */
  for (i = oldAlloc; i < me->nAlloc; i++) {
    int nCustomErrorMsg = EOS_MAX_ERROR_CODE_VALUE - EOS_MIN_ERROR_CODE_VALUE + 1;
    me->dataObjects[i]     = NULL;
    me->errorCodes[i]      = EOS_OK;
    me->customErrorMsg[i] = (EOS_CHAR **) malloc (sizeof (EOS_CHAR*) * (nCustomErrorMsg));
    for (j = 0; j < nCustomErrorMsg; j++) me->customErrorMsg[i][j] = (EOS_CHAR*)NULL;
    me->tableHandlesMap[i] = _EOS_NEWLY_INITIALIZED_TABLE_HANDLE_VALUE;
    me->isHandlePublic[i]  = 0;
  }

  /* initialize all general options for the new tables */
  for (j = 0; j < EOS_NUM_GENERAL_OPTIONS; j++) {
    of = EOS_GENERAL_INDEX_TO_OPTION_FLAG (j);
    ind = EOS_OPTION_FLAG_TO_INDEX (of);        /* get total opt index from j */
    for (k = oldAlloc; k < me->nAlloc; k++) {
      me->generalOptions[j][k].rval =
	eos_DefaultTableOptions[ind].optionValue.rval;
      me->generalOptions[j][k].ival =
	eos_DefaultTableOptions[ind].optionValue.ival;
      me->generalOptions[j][k].bval =
	eos_DefaultTableOptions[ind].optionValue.bval;
    }
  }
}

/*************************************************************************
 *
 * Function eos_IsHandleValidEosDataMap
 * Description:
 * Checks if the user has given us valid table handle
 * 
 * Returned Values:
 * EOS_INTEGER 1 for valid, 0 for invalid
 *
 * Input Value:
 * eos_DataMap *me
 * EOS_INTEGER handle; - the handle user has provided
 *************************************************************************/
EOS_INTEGER eos_IsHandleValidEosDataMap (eos_DataMap *me,
                                         EOS_INTEGER tableHandle)
{

  if (! eos_IsHandleValid(tableHandle))
    return 0;
  else
    return 1;
}
