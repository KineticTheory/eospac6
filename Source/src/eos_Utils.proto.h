/*********************************************************************
 *
 * Filetype: (HEADER)
 *
 * Automatically generated by cflow.filter.pl, Thu Jun  1 12:38:47 2017
 *
 * Copyright -- see file named COPYRIGHTNOTICE
 * 
 *********************************************************************/
#ifndef EOS_UTILS_PROTOTYPES

#define EOS_UTILS_PROTOTYPES

/*
 * ----------------------------------------------
 * 37 PUBLIC FUNCTION PROTOTYPES FOR eos_Utils.c
 * ----------------------------------------------
 */
void _eos_CompressArray (EOS_REAL * a, EOS_INTEGER * n, EOS_INTEGER n_target, EOS_CHAR ** errMsg,
                         EOS_INTEGER * ia);
void _eos_CreateGhostData (EOS_INTEGER nGhostData, EOS_INTEGER nxtbl_in, EOS_INTEGER nytbl_in, EOS_REAL * xtbls_in,
                           EOS_REAL * ytbls_in, EOS_REAL ** ftbls_in, EOS_REAL * coldCurve_in,
                           EOS_INTEGER * nxtbl_out, EOS_INTEGER * nytbl_out, EOS_REAL ** xtbls_out,
                           EOS_REAL ** ytbls_out, EOS_REAL *** ftbls_out, EOS_REAL ** coldCurve_out,
                           EOS_INTEGER * err, EOS_CHAR ** errMsg);
void _eos_DEBUG_PRINT (const EOS_CHAR * fmt, ...);
EOS_INTEGER _eos_DestroyGhostData (EOS_INTEGER * nGhostData, EOS_REAL ** xtbls, EOS_REAL ** ytbls,
                                   EOS_REAL *** ftbls, EOS_REAL ** coldCurve);
void _eos_GetInvertedTable (EOS_INTEGER th, EOS_INTEGER dataType, EOS_REAL * _xtbls2, EOS_REAL * _ytbls2,
                            EOS_REAL * _ftbls2, EOS_REAL * _coldCurve2, EOS_REAL ** xtbls, EOS_REAL ** ytbls,
                            EOS_REAL ** ftbls, EOS_REAL * coldCurve, EOS_INTEGER * nxtbl, EOS_INTEGER * nytbl,
                            EOS_REAL ** xtbls_new, EOS_REAL ** ytbls_new, EOS_REAL ** ftbls_new,
                            EOS_INTEGER ** ftbls_invt_mask, EOS_INTEGER * err, EOS_INTEGER _target_N);
EOS_CHAR *_eos_GetSesameFileName (eos_Data * me);
void _eos_InterpolateRecordType_1D (EOS_INTEGER nX, EOS_REAL * X, EOS_REAL * F, EOS_REAL * F2, EOS_INTEGER th,
                                    EOS_INTEGER dataType, EOS_INTEGER nXYPairs, EOS_REAL * srchX, EOS_REAL * fVals,
                                    EOS_REAL * dFx, EOS_INTEGER * xyBounds, EOS_INTEGER * errorCode,
                                    EOS_CHAR ** errMsg);
EOS_INTEGER _eos_MakeMonotonic (EOS_INTEGER nx, EOS_INTEGER ny, EOS_REAL x[], EOS_REAL y[], EOS_REAL F[],
                                EOS_INTEGER indep, EOS_REAL F_shift[], EOS_BOOLEAN enableDebugDump);
EOS_INTEGER _eos_MakeSmooth (EOS_INTEGER nX, EOS_INTEGER nY, EOS_REAL X[], EOS_REAL Y[], EOS_REAL F[]);
EOS_INTEGER _eos_QuickSort (EOS_INTEGER N, EOS_REAL a[], EOS_INTEGER lvl, EOS_INTEGER * err, EOS_CHAR ** errMsg,
			    EOS_INTEGER * ia);
EOS_CHAR *_eos_ReplaceSubstring (const EOS_CHAR * original, const EOS_CHAR * pattern,
				 const EOS_CHAR * replacement);
void _eos_dbl2String (EOS_REAL x, EOS_INTEGER fw, char *buffer);
EOS_BOOLEAN _eos_fileExistsAndValid (EOS_CHAR * filename);
EOS_INTEGER _eos_find_matid_in_gMatidMap (EOS_INTEGER matid);
EOS_INTEGER _eos_find_userdefined_fileindex_in_gEosDataMapDataObjects (EOS_INTEGER fileIndex);
int _eos_hunt (EOS_REAL x[], EOS_INTEGER n, EOS_INTEGER ix, EOS_REAL y, EOS_INTEGER jlower, EOS_INTEGER * err);
EOS_BOOLEAN _eos_is_absoluteFileName (EOS_CHAR * f);
void eos_GetConversionFactorsFromTableHandle (EOS_INTEGER th, EOS_INTEGER * dataType, EOS_REAL * convX,
                                              EOS_REAL * convY, EOS_REAL * convF, EOS_INTEGER * err);
EOS_CHAR *eos_GetCustomErrorMsg (EOS_INTEGER th, const EOS_INTEGER err);
EOS_CHAR *eos_GetDataTypeDescriptionFromTableHandle (EOS_INTEGER th);
EOS_INTEGER eos_GetDataTypeFromTableHandle (EOS_INTEGER th, EOS_INTEGER * err);
EOS_INTEGER eos_GetHandleFromCustomErrorCode (const EOS_INTEGER err);
EOS_INTEGER eos_GetStandardErrorCodeFromCustomErrorCode (const EOS_INTEGER err);
void eos_InvalidateHandle (EOS_INTEGER th);
EOS_BOOLEAN eos_IsHandleValid (EOS_INTEGER th);
void eos_ResetAllCustomErrorMsg (void);
void eos_ResetCustomErrorMsg (EOS_INTEGER th);
EOS_INTEGER eos_SetCustomErrorCode (EOS_INTEGER th, const EOS_INTEGER err);
EOS_INTEGER eos_SetCustomErrorMsg (EOS_INTEGER th, const EOS_INTEGER err, const EOS_CHAR * fmt, ...);
void eos_SetCustomMsg_str (EOS_CHAR ** msg, const EOS_CHAR * fmt, ...);
EOS_BOOLEAN eos_getBoolOptionFromTableHandle (EOS_INTEGER th, EOS_INTEGER optFlag, EOS_INTEGER * err);
EOS_REAL eos_getRealOptionFromTableHandle (EOS_INTEGER th, EOS_INTEGER optFlag, EOS_INTEGER * err);
EOS_INTEGER eos_getRequiredNumSubtables (eos_Data * me, EOS_INTEGER dataType);
EOS_INTEGER eos_getSesameFileNames (EOS_CHAR *** files, EOS_INTEGER * filesL, EOS_CHAR ** errMsg);
EOS_CHAR *get_OptionFlagStr (EOS_INTEGER f);
EOS_INTEGER get_VarListIndex (EOS_INTEGER t);
EOS_CHAR *get_VarStr (EOS_INTEGER t, EOS_INTEGER flag);
void _eos_destroyMatidMap (void);

#ifdef _EOS_UTILS_INTERNAL_PROTOTYPES

/*
 * -----------------------------------------------
 * 27 PRIVATE FUNCTION PROTOTYPES FOR eos_Utils.c
 * -----------------------------------------------
 */
void _eos_GetInvertedTable_CATEGORY1 (EOS_INTEGER th, EOS_INTEGER dataType, EOS_REAL ** xtbls, EOS_REAL ** ytbls,
                                      EOS_REAL ** ftbls, EOS_REAL * coldCurve, EOS_INTEGER * nxtbl,
                                      EOS_INTEGER * nytbl, EOS_REAL ** xtbls_invt, EOS_REAL ** ytbls_invt,
                                      EOS_REAL ** ftbls_invt, EOS_INTEGER * err, EOS_INTEGER _target_N,
                                      EOS_INTEGER * nXYPairs);
void _eos_GetInvertedTable_CATEGORY2 (EOS_INTEGER th, EOS_INTEGER dataType, EOS_REAL ** xtbls, EOS_REAL ** ytbls,
				      EOS_REAL ** ftbls, EOS_REAL * coldCurve, EOS_INTEGER * nxtbl,
				      EOS_INTEGER * nytbl, EOS_REAL ** xtbls_invt, EOS_REAL ** ytbls_invt,
				      EOS_REAL ** ftbls_invt, EOS_INTEGER * err, EOS_INTEGER _target_N,
				      EOS_INTEGER * nXYPairs);
EOS_INTEGER _eos_NormalizeWithX (EOS_INTEGER th, EOS_INTEGER dataType, EOS_INTEGER * nxtbl, EOS_INTEGER * nytbl,
				 EOS_REAL ** ftbls, EOS_REAL * X);
EOS_INTEGER _eos_RemoveAndStoreColdCurve (EOS_INTEGER th, EOS_INTEGER dataType, EOS_INTEGER * nxtbl,
                                          EOS_INTEGER * nytbl, EOS_REAL ** ftbls, EOS_REAL * coldCurve);
EOS_INTEGER _eos_RemoveDuplicates (EOS_REAL * a, EOS_INTEGER * n, EOS_INTEGER * ia);
int _eos_RemoveLoops (EOS_INTEGER nx, EOS_INTEGER ny, EOS_REAL F[], EOS_INTEGER dim, _EXCEPTIONS_T * e);
EOS_INTEGER _eos_addDefaultFileNames (EOS_CHAR * srchPathName, EOS_CHAR *** fileNames, EOS_INTEGER * fileNamesL);
void _eos_addMatidMap (EOS_INTEGER matid, EOS_INTEGER index);
EOS_INTEGER _eos_byteSwap (EOS_CHAR * buf, EOS_INTEGER nwds, EOS_INTEGER wordSize, EOS_BOOLEAN bigEndianFile);
EOS_REAL _eos_calcFmod (EOS_REAL Fim1, EOS_REAL Fi, EOS_REAL x0, EOS_REAL xmax, EOS_REAL xi, EOS_REAL trend,
                        void *calcFmod_debug);
EOS_INTEGER _eos_compactFilesArray (EOS_CHAR *** files, EOS_INTEGER * filesL, EOS_BOOLEAN update_gMatidMap);
EOS_INTEGER _eos_deleteDuplicateNames (EOS_CHAR *** files, EOS_INTEGER * filesL, struct stat *stat_buf);
EOS_BOOLEAN _eos_in_gMatidMap (EOS_INTEGER idx);
EOS_BOOLEAN _eos_isBigEndianFile (FILE * esFile);
EOS_INTEGER _eos_littleEndianMachine (void);
int _eos_locate (EOS_REAL x[], EOS_INTEGER jlower, EOS_INTEGER jupper, EOS_INTEGER ix, EOS_REAL y, EOS_INTEGER * l,
                 EOS_INTEGER * u);
EOS_INTEGER _eos_mag (EOS_REAL g, EOS_INTEGER gamma);
EOS_INTEGER _eos_parseENV (const EOS_CHAR * envVar, EOS_CHAR *** paths, EOS_INTEGER * pathsL);
EOS_INTEGER _eos_parseIndexFile (FILE * fp, EOS_CHAR * fpPathName, EOS_CHAR *** fileNames,
                                 EOS_INTEGER * fileNamesL, EOS_CHAR ** errMsg);
EOS_INTEGER _eos_readLine (FILE * fp, EOS_CHAR ** line);
EOS_REAL _eos_round (EOS_REAL x, EOS_REAL p);
EOS_BOOLEAN eos_IsErrorCodeValid (EOS_INTEGER err);
void eos_ResetOneCustomErrorMsg (EOS_INTEGER th, const EOS_INTEGER err);
EOS_INTEGER eos_compareFiles (EOS_CHAR * file1, struct stat *stat_buf1, EOS_CHAR * file2, struct stat *stat_buf2);
EOS_INTEGER eos_getIntOptionFromTableHandle (EOS_INTEGER th, EOS_INTEGER optFlag, EOS_INTEGER * err);
EOS_INTEGER get_OptionFlagIndex (EOS_INTEGER f);
EOS_CHAR *strrstr (const EOS_CHAR * string, const EOS_CHAR * find);

#endif /* _EOS_UTILS_INTERNAL_PROTOTYPES */

#endif /* EOS_UTILS_PROTOTYPES */
