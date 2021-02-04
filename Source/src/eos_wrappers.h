/*********************************************************************
 * 
 * ---------------------------------------------------------
 * Filetype: (HEADER)
 * 
 * Copyright -- see file named COPYRIGHTNOTICE
 * 
 ********************************************************************/
/* Defines C function wrappers for machine portability */
#ifndef EOS_WRAPPERS_H
#define EOS_WRAPPERS_H

#if defined(HAVE_CONFIG_H)
#include <config.h>
#endif /* defined(HAVE_CONFIG_H) */

#if defined(HAVE_CONFIG_H)

/* Use the fortran name mangling macro defined in config.h */
#define eos_CheckExtrap FC_FUNC(eos_checkextrap,EOS_CHECKEXTRAP)
#define eos_CreateTables FC_FUNC(eos_createtables,EOS_CREATETABLES)
#define eos_DestroyAll FC_FUNC(eos_destroyall,EOS_DESTROYALL)
#define eos_DestroyTables FC_FUNC(eos_destroytables,EOS_DESTROYTABLES)
#define eos_GetErrorCode FC_FUNC(eos_geterrorcode,EOS_GETERRORCODE)
#define eos_GetErrorMessage FC_FUNC(eos_geterrormessage,EOS_GETERRORMESSAGE)
#define eos_GetPackedTables FC_FUNC(eos_getpackedtables,EOS_GETPACKEDTABLES)
#define eos_GetPackedTablesSize FC_FUNC(eos_getpackedtablessize,EOS_GETPACKEDTABLESSIZE)
#define eos_Interpolate FC_FUNC(eos_interpolate,EOS_INTERPOLATE)
#define eos_LoadTables FC_FUNC(eos_loadtables,EOS_LOADTABLES)
#define eos_ResetOption FC_FUNC(eos_resetoption,EOS_RESETOPTION)
#define eos_SetPackedTables FC_FUNC(eos_setpackedtables,EOS_SETPACKEDTABLES)
#define eos_SetOption FC_FUNC(eos_setoption,EOS_SETOPTION)
#define eos_GetMetaData FC_FUNC(eos_getmetadata,EOS_GETMETADATA)
#define eos_GetTableMetaData FC_FUNC(eos_gettablemetadata,EOS_GETTABLEMETADATA)
#define eos_GetTableInfo FC_FUNC(eos_gettableinfo,EOS_GETTABLEINFO)
#define eos_GetTableCmnts FC_FUNC(eos_gettablecmnts,EOS_GETTABLECMNTS)
#define eos_Mix FC_FUNC(eos_mix,EOS_MIX)
#define eos_Time FC_FUNC(eos_time,EOS_TIME)
#define eos_GetVersion FC_FUNC(eos_getversion,EOS_GETVERSION)
#define eos_GetVersionLength FC_FUNC(eos_getversionlength,EOS_GETVERSIONLENGTH)
#define eos_SetDataFileName FC_FUNC(eos_setdatafilename,EOS_SETDATAFILENAME)
#define eos_SetDataFileName_Cwrapper FC_FUNC(eos_setdatafilename_cwrapper,EOS_SETDATAFILENAME_CWRAPPER)
#define eos_GetMaxDataFileNameLength FC_FUNC(eos_getmaxdatafilenamelength,EOS_GETMAXDATAFILENAMELENGTH)
#define eos_ErrorCodesEqual FC_FUNC(eos_errorcodesequal,EOS_ERRORCODESEQUAL)
#ifdef DO_OFFLOAD
#define eos_GpuOffloadData FC_FUNC(eos_gpuoffloaddata,EOS_GPUOFFLOADDATA)
#endif /* DO_OFFLOAD */

#else /* !defined(HAVE_CONFIG_H) */

/*
 * This section is used by the build scripts distributed with EOSPAC6 to allow proper object name mangling.
 */

#if defined PC

#define FUNC_INTER __stdcall

#else

#define FUNC_INTER

#endif

#if !defined MixedCase

/* disable compilation of the following if MixedCase is not defined */
#define NO_EOS_SETDATAFILENAME_CWRAPPER

#endif

/* define macros for all public functions */
#if defined MixedCase

/* NO NAME MANGLING DONE FOR THIS CASE */

#elif defined UPPERCASE

#define eos_CheckExtrap EOS_CHECKEXTRAP
#define eos_CreateTables EOS_CREATETABLES
#define eos_DestroyAll EOS_DESTROYALL
#define eos_DestroyTables EOS_DESTROYTABLES
#define eos_GetErrorCode EOS_GETERRORCODE
#define eos_GetErrorMessage EOS_GETERRORMESSAGE
#define eos_GetPackedTables EOS_GETPACKEDTABLES
#define eos_GetPackedTablesSize EOS_GETPACKEDTABLESSIZE
#define eos_Interpolate EOS_INTERPOLATE
#define eos_LoadTables EOS_LOADTABLES
#define eos_ResetOption EOS_RESETOPTION
#define eos_SetPackedTables EOS_SETPACKEDTABLES
#define eos_SetOption EOS_SETOPTION
#define eos_GetMetaData EOS_GETMETADATA
#define eos_GetTableMetaData EOS_GETTABLEMETADATA
#define eos_GetTableInfo EOS_GETTABLEINFO
#define eos_GetTableCmnts EOS_GETTABLECMNTS
#define eos_Mix EOS_MIX
#define eos_Time EOS_TIME
#define eos_GetVersion EOS_GETVERSION
#define eos_GetVersionLength EOS_GETVERSIONLENGTH
#define eos_SetDataFileName EOS_SETDATAFILENAME
#define eos_GetMaxDataFileNameLength EOS_GETMAXDATAFILENAMELENGTH
#define eos_ErrorCodesEqual EOS_ERRORCODESEQUAL
#ifdef DO_OFFLOAD
#define eos_GpuOffloadData EOS_GPUOFFLOADDATA
#endif /* DO_OFFLOAD */

#elif defined lowercase

#define eos_CheckExtrap eos_checkextrap
#define eos_CreateTables eos_createtables
#define eos_DestroyAll eos_destroyall
#define eos_DestroyTables eos_destroytables
#define eos_GetErrorCode eos_geterrorcode
#define eos_GetErrorMessage eos_geterrormessage
#define eos_GetPackedTables eos_getpackedtables
#define eos_GetPackedTablesSize eos_getpackedtablessize
#define eos_Interpolate eos_interpolate
#define eos_LoadTables eos_loadtables
#define eos_ResetOption eos_resetoption
#define eos_SetPackedTables eos_setpackedtables
#define eos_SetOption eos_setoption
#define eos_GetMetaData eos_getmetadata
#define eos_GetTableMetaData eos_gettablemetadata
#define eos_GetTableInfo eos_gettableinfo
#define eos_GetTableCmnts eos_gettablecmnts
#define eos_Mix eos_mix
#define eos_Time eos_time
#define eos_GetVersion eos_getversion
#define eos_GetVersionLength eos_getversionlength
#define eos_SetDataFileName eos_setdatafilename
#define eos_GetMaxDataFileNameLength eos_getmaxdatafilenamelength
#define eos_ErrorCodesEqual eos_errorcodesequal
#ifdef DO_OFFLOAD
#define eos_GpuOffloadData eos_gpuoffloaddata
#endif /* DO_OFFLOAD */

#elif defined lowercase_

#define eos_CheckExtrap eos_checkextrap_
#define eos_CreateTables eos_createtables_
#define eos_DestroyAll eos_destroyall_
#define eos_DestroyTables eos_destroytables_
#define eos_GetErrorCode eos_geterrorcode_
#define eos_GetErrorMessage eos_geterrormessage_
#define eos_GetPackedTables eos_getpackedtables_
#define eos_GetPackedTablesSize eos_getpackedtablessize_
#define eos_Interpolate eos_interpolate_
#define eos_LoadTables eos_loadtables_
#define eos_ResetOption eos_resetoption_
#define eos_SetPackedTables eos_setpackedtables_
#define eos_SetOption eos_setoption_
#define eos_GetMetaData eos_getmetadata_
#define eos_GetTableMetaData eos_gettablemetadata_
#define eos_GetTableInfo eos_gettableinfo_
#define eos_GetTableCmnts eos_gettablecmnts_
#define eos_Mix eos_mix_
#define eos_Time eos_time_
#define eos_GetVersion eos_getversion_
#define eos_GetVersionLength eos_getversionlength_
#define eos_SetDataFileName eos_setdatafilename_
#define eos_GetMaxDataFileNameLength eos_getmaxdatafilenamelength_
#define eos_ErrorCodesEqual eos_errorcodesequal_
#ifdef DO_OFFLOAD
#define eos_GpuOffloadData eos_gpuoffloaddata_
#endif /* DO_OFFLOAD */

#elif defined lowercase__

#define eos_CheckExtrap eos_checkextrap__
#define eos_CreateTables eos_createtables__
#define eos_DestroyAll eos_destroyall__
#define eos_DestroyTables eos_destroytables__
#define eos_GetErrorCode eos_geterrorcode__
#define eos_GetErrorMessage eos_geterrormessage__
#define eos_GetPackedTables eos_getpackedtables__
#define eos_GetPackedTablesSize eos_getpackedtablessize__
#define eos_Interpolate eos_interpolate__
#define eos_LoadTables eos_loadtables__
#define eos_ResetOption eos_resetoption__
#define eos_SetPackedTables eos_setpackedtables__
#define eos_SetOption eos_setoption__
#define eos_GetMetaData eos_getmetadata__
#define eos_GetTableMetaData eos_gettablemetadata__
#define eos_GetTableInfo eos_gettableinfo__
#define eos_GetTableCmnts eos_gettablecmnts__
#define eos_Mix eos_mix__
#define eos_Time eos_time__
#define eos_GetVersion eos_getversion__
#define eos_GetVersionLength eos_getversionlength__
#define eos_SetDataFileName eos_setdatafilename__
#define eos_GetMaxDataFileNameLength eos_getmaxdatafilenamelength__
#define eos_ErrorCodesEqual eos_errorcodesequal__
#ifdef DO_OFFLOAD
#define eos_GpuOffloadData eos_gpuoffloaddata__
#endif /* DO_OFFLOAD */

#elif defined _lowercase

#define eos_CheckExtrap _eos_checkextrap
#define eos_CreateTables _eos_createtables
#define eos_DestroyAll _eos_destroyall
#define eos_DestroyTables _eos_destroytables
#define eos_GetErrorCode _eos_geterrorcode
#define eos_GetErrorMessage _eos_geterrormessage
#define eos_GetPackedTables _eos_getpackedtables
#define eos_GetPackedTablesSize _eos_getpackedtablessize
#define eos_Interpolate _eos_interpolate
#define eos_LoadTables _eos_loadtables
#define eos_ResetOption _eos_resetoption
#define eos_SetPackedTables _eos_setpackedtables
#define eos_SetOption _eos_setoption
#define eos_GetMetaData _eos_getmetadata
#define eos_GetTableMetaData _eos_gettablemetadata
#define eos_GetTableInfo _eos_gettableinfo
#define eos_GetTableCmnts _eos_gettablecmnts
#define eos_Mix _eos_mix
#define eos_Time _eos_time
#define eos_GetVersion _eos_getversion
#define eos_GetVersionLength _eos_getversionlength
#define eos_SetDataFileName _eos_setdatafilename
#define eos_GetMaxDataFileNameLength _eos_getmaxdatafilenamelength
#define eos_ErrorCodesEqual _eos_errorcodesequal
#ifdef DO_OFFLOAD
#define eos_GpuOffloadData _eos_gpuoffloaddata
#endif /* DO_OFFLOAD */

#elif defined __lowercase

#define eos_CheckExtrap __eos_checkextrap
#define eos_CreateTables __eos_createtables
#define eos_DestroyAll __eos_destroyall
#define eos_DestroyTables __eos_destroytables
#define eos_GetErrorCode __eos_geterrorcode
#define eos_GetErrorMessage __eos_geterrormessage
#define eos_GetPackedTables __eos_getpackedtables
#define eos_GetPackedTablesSize __eos_getpackedtablessize
#define eos_Interpolate __eos_interpolate
#define eos_LoadTables __eos_loadtables
#define eos_ResetOption __eos_resetoption
#define eos_SetPackedTables __eos_setpackedtables
#define eos_SetOption __eos_setoption
#define eos_GetMetaData __eos_getmetadata
#define eos_GetTableMetaData __eos_gettablemetadata
#define eos_GetTableInfo __eos_gettableinfo
#define eos_GetTableCmnts __eos_gettablecmnts
#define eos_Mix __eos_mix
#define eos_Time __eos_time
#define eos_GetVersion __eos_getversion
#define eos_GetVersionLength __eos_getversionlength
#define eos_SetDataFileName __eos_setdatafilename
#define eos_GetMaxDataFileNameLength __eos_getmaxdatafilenamelength
#define eos_ErrorCodesEqual __eos_errorcodesequal
#ifdef DO_OFFLOAD
#define eos_GpuOffloadData __eos_gpuoffloaddata
#endif /* DO_OFFLOAD */

#else /* use lowercase_ as default */

#define eos_CheckExtrap eos_checkextrap_
#define eos_CreateTables eos_createtables_
#define eos_DestroyAll eos_destroyall_
#define eos_DestroyTables eos_destroytables_
#define eos_GetErrorCode eos_geterrorcode_
#define eos_GetErrorMessage eos_geterrormessage_
#define eos_GetPackedTables eos_getpackedtables_
#define eos_GetPackedTablesSize eos_getpackedtablessize_
#define eos_Interpolate eos_interpolate_
#define eos_LoadTables eos_loadtables_
#define eos_ResetOption eos_resetoption_
#define eos_SetPackedTables eos_setpackedtables_
#define eos_SetOption eos_setoption_
#define eos_GetMetaData eos_getmetadata_
#define eos_GetTableMetaData eos_gettablemetadata_
#define eos_GetTableInfo eos_gettableinfo_
#define eos_GetTableCmnts eos_gettablecmnts_
#define eos_Mix eos_mix_
#define eos_Time eos_time_
#define eos_GetVersion eos_getversion_
#define eos_GetVersionLength eos_getversionlength_
#define eos_SetDataFileName eos_setdatafilename_
#define eos_GetMaxDataFileNameLength eos_getmaxdatafilenamelength_
#define eos_ErrorCodesEqual eos_errorcodesequal_
#ifdef DO_OFFLOAD
#define eos_GpuOffloadData eos_gpuoffloaddata_
#endif /* DO_OFFLOAD */

#endif

#endif /* defined(HAVE_CONFIG_H) */

#endif /* if not defined EOS_WRAPPERS_H */
