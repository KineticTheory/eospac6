/*********************************************************************
 *
 * Filetype: (HEADER)
 *
 * Automatically generated by cflow.filter.pl, Mon Aug 25 16:13:41 2014
 *
 * Copyright -- see file named COPYRIGHTNOTICE
 * 
 *********************************************************************/
#ifndef EOS_RECORDTYPE4_PROTOTYPES

#define EOS_RECORDTYPE4_PROTOTYPES

/*
 * ---------------------------------------------------
 * 2 PUBLIC FUNCTION PROTOTYPES FOR eos_RecordType4.c
 * ---------------------------------------------------
 */
void eos_ConstructRecordType4 (eos_RecordType4 * me, EOS_INTEGER th, EOS_INTEGER materialID);
int _eos_get_field_value(EOS_CHAR *str, EOS_CHAR *keyword, EOS_CHAR *oStr);

#ifdef _EOS_RECORDTYPE4_INTERNAL_PROTOTYPES

/*
 * -----------------------------------------------------
 * 20 PRIVATE FUNCTION PROTOTYPES FOR eos_RecordType4.c
 * -----------------------------------------------------
 */
void eos_AreMonotonicRequirementsCompatibleRecordType4 (void *me, EOS_INTEGER dataType, EOS_INTEGER inX,
							EOS_INTEGER inY, EOS_BOOLEAN * compatible);
void eos_AreSmoothingRequirementsCompatibleRecordType4 (void *me, EOS_INTEGER dataType, EOS_INTEGER makeSmooth,
							EOS_INTEGER makePtSmooth, EOS_BOOLEAN * compatible);
void eos_CreateRecordType4 (void *ptr, EOS_INTEGER th);
void eos_DestroyRecordType4 (void *ptr);
void eos_GetLoadedBulkDataRecordType4 (void *ptr, EOS_REAL * zbar, EOS_REAL * abar, EOS_REAL * dens0,
				       EOS_INTEGER * errorCode);
void eos_GetMonotonicityRecordType4 (void *me, EOS_INTEGER dataType, EOS_INTEGER * inX, EOS_INTEGER * inY);
void eos_GetPackedTableRecordType4 (void *ptr, EOS_INTEGER th, EOS_CHAR * packedTable, EOS_INTEGER * err);
void eos_GetPackedTableSizeRecordType4 (void *ptr, EOS_INTEGER th, EOS_INTEGER * packedTableSize,
					EOS_INTEGER * err);
void eos_GetSmoothingRecordType4 (void *me, EOS_INTEGER dataType, EOS_INTEGER * isSmooth,
				  EOS_INTEGER * isPtSmooth);
void eos_GetTableCmntsRecordType4 (void *ptr, EOS_CHAR * cmntStr, EOS_INTEGER * err);
void eos_GetTableMetaDataRecordType4 (void *ptr, EOS_INTEGER infoItem, EOS_CHAR *infoStr, EOS_INTEGER *err);
void eos_GetTableInfoRecordType4 (void *ptr, EOS_INTEGER th, EOS_INTEGER numInfoItems, EOS_INTEGER * infoItems,
				  EOS_REAL * infoVals, EOS_INTEGER * err);
void eos_IsMonotonicRecordType4 (void *ptr, EOS_INTEGER dataType, EOS_BOOLEAN * isMonotonic, EOS_BOOLEAN inX,
				 EOS_BOOLEAN inY, EOS_INTEGER * err);
void eos_LoadRecordType4 (void *ptr, EOS_INTEGER th);
void eos_MakeMonotonicRecordType4 (void *ptr, EOS_INTEGER th, EOS_INTEGER dataType, EOS_BOOLEAN inX,
				   EOS_BOOLEAN inY, EOS_INTEGER * err);
void eos_MakeSmoothRecordType4 (void *ptr, EOS_INTEGER th, EOS_INTEGER dataType, EOS_BOOLEAN ptSmooth,
				EOS_INTEGER * err);
void eos_PrintRecordType4 (void *ptr, EOS_INTEGER th, EOS_CHAR * fname, EOS_INTEGER append, EOS_INTEGER * err);
void eos_ReallocateTablesRecordType4 (eos_RecordType4 * me, EOS_INTEGER N);
void eos_SetFileIndexesRecordType4 (void *ptr, EOS_INTEGER th);
void eos_SetMonotonicityRecordType4 (void *me, EOS_INTEGER dataType, EOS_INTEGER inX, EOS_INTEGER inY);
void eos_SetPackedTableRecordType4 (void *ptr, EOS_INTEGER th, EOS_CHAR * packedTable, EOS_INTEGER * err);
void eos_SetSmoothingRecordType4 (void *me, EOS_INTEGER dataType, EOS_INTEGER makeSmooth,
				  EOS_INTEGER makePtSmooth);

#endif /* _EOS_RECORDTYPE4_INTERNAL_PROTOTYPES */

#endif /* EOS_RECORDTYPE4_PROTOTYPES */
