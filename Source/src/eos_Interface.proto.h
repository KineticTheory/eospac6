/*********************************************************************
 * Class Name : eos_Interface
 * ---------------------------------------------------------
 * Filetype: (HEADER)
 * 
 * Copyright -- see file named COPYRIGHTNOTICE
 * 
 ********************************************************************/

#ifndef EOSPAC6_EOS_INTERFACE_PROTOTYPES
#define EOSPAC6_EOS_INTERFACE_PROTOTYPES

/*
 * ---------------------------------------------------
 * PUBLIC FUNCTION PROTOTYPES FOR eos_FC_Interface.c
 * ---------------------------------------------------
 */
#ifdef __cplusplus
extern "C"
{
#endif

  void FUNC_INTER eos_CheckExtrap (EOS_INTEGER *tableHandle,
                                   EOS_INTEGER *nXYPairs,
                                   EOS_REAL *xVals,
                                   EOS_REAL *yVals,
                                   EOS_INTEGER *xyBounds,
                                   EOS_INTEGER *errorCode);
  void FUNC_INTER eos_CreateTables (EOS_INTEGER *nTables,
                                    EOS_INTEGER tableType[],
                                    EOS_INTEGER matID[],
                                    EOS_INTEGER tableHandles[],
                                    EOS_INTEGER *errorCode);
  void FUNC_INTER eos_DestroyAll (EOS_INTEGER *errorCode);
  void FUNC_INTER eos_DestroyTables (EOS_INTEGER *nTables,
                                     EOS_INTEGER tableHandles[],
                                     EOS_INTEGER *errorCode);
  void FUNC_INTER eos_GetErrorCode (EOS_INTEGER *tableHandle,
                                    EOS_INTEGER *errorCode);
  void FUNC_INTER eos_GetErrorMessage (EOS_INTEGER *errorCode,
                                       EOS_CHAR errorMsg[EOS_MaxErrMsgLen]);
  void FUNC_INTER eos_GetMaxDataFileNameLength (EOS_INTEGER *max_length);
  void FUNC_INTER eos_GetPackedTables (EOS_INTEGER *nTables,
                                       EOS_INTEGER tableHandles[],
                                       EOS_CHAR *packedTables,
                                       EOS_INTEGER *errorCode);
  void FUNC_INTER eos_GetPackedTablesSize (EOS_INTEGER *nTables,
                                           EOS_INTEGER tableHandles[],
                                           EOS_INTEGER *packedTablesSize,
                                           EOS_INTEGER *errorCode);
  void FUNC_INTER eos_Interpolate (EOS_INTEGER *tableHandle,
                                   EOS_INTEGER *nXYPairs,
                                   EOS_REAL *xVals,
                                   EOS_REAL *yVals,
                                   EOS_REAL *fVals,
                                   EOS_REAL *dFx,
                                   EOS_REAL *dFy, EOS_INTEGER *errorCode);
  void FUNC_INTER eos_LoadTables (EOS_INTEGER *nTables,
                                  EOS_INTEGER tableHandles[],
                                  EOS_INTEGER *errorCode);
  void FUNC_INTER eos_ResetOption (EOS_INTEGER *tableHandle,
                                   const EOS_INTEGER *tableOption,
                                   EOS_INTEGER *errorCode);
  void FUNC_INTER eos_SetDataFileName_Cwrapper (EOS_INTEGER *tableHandle,
						EOS_INTEGER *matID,
						EOS_INTEGER *tableType,
						EOS_CHAR *fileName,
						EOS_INTEGER *errorCode);
  void FUNC_INTER eos_ErrorCodesEqual (EOS_INTEGER *err1, EOS_INTEGER *err2, EOS_BOOLEAN *result);
  void FUNC_INTER eos_SetPackedTables (EOS_INTEGER *nTables,
                                       EOS_INTEGER *packedTablesSize,
                                       EOS_CHAR *packedTables,
                                       EOS_INTEGER tableHandles[],
                                       EOS_INTEGER *errorCode);
  void FUNC_INTER eos_SetOption (EOS_INTEGER *tableHandle,
                                 const EOS_INTEGER *tableOption,
                                 const EOS_REAL *tableOptionVal,
                                 EOS_INTEGER *errorCode);
  void FUNC_INTER eos_GetMetaData (EOS_INTEGER *infoItem, EOS_INTEGER *infoItemCategory,
				   EOS_CHAR *infoStr, EOS_INTEGER *errorCode);
  void FUNC_INTER eos_GetTableMetaData (EOS_INTEGER *tableHandle, EOS_INTEGER *infoItem,
					EOS_CHAR *infoStr, EOS_INTEGER *errorCode);
  void FUNC_INTER eos_GetTableInfo (EOS_INTEGER *tableHandle,
                                    EOS_INTEGER *numInfoItems,
                                    EOS_INTEGER *infoItems,
                                    EOS_REAL *infoVals,
                                    EOS_INTEGER *errorCode);
  void FUNC_INTER eos_GetTableCmnts (EOS_INTEGER *tableHandle,
                                     EOS_CHAR *cmntStr,
                                     EOS_INTEGER *errorCode);
  void FUNC_INTER eos_Mix (EOS_INTEGER *nTables, EOS_INTEGER *tableHandles,
                           EOS_INTEGER *nXYPairs, EOS_REAL *concInMix,
                           EOS_REAL *xVals, EOS_REAL *yVals, EOS_REAL *fVals,
                           EOS_REAL *dFx, EOS_REAL *dFy,
                           EOS_INTEGER *errorCode);
  void FUNC_INTER eos_GetVersionLength (EOS_INTEGER *length);
  void FUNC_INTER eos_GetVersion (EOS_CHAR *version);
  void FUNC_INTER eos_Time (EOS_BOOLEAN *reset, EOS_REAL *wctime,
                            EOS_REAL *cputime, EOS_REAL *cpucycles,
                            EOS_INTEGER *err);

#ifdef __cplusplus
}
#endif
#endif /* EOSPAC6_EOS_INTERFACE_PROTOTYPES */
