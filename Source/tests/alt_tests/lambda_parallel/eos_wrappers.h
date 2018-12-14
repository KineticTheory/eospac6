/*********************************************************************
 * 
 * ---------------------------------------------------------
 * Filetype: (HEADER)
 * 
 * Copyright -- see file named COPYRIGHTNOTICE
 * 
 ********************************************************************/
/* Defines C function wrappers for machine portability */

#if defined PC

#define FUNC_INTER __stdcall

#else

#define FUNC_INTER

#endif

/* define macros for all public functions */
#if defined __CYGWIN__ || defined WIN32
#define eos_DestroyAll eos_destroyall
#define eos_Interpolate eos_interpolate
#define eos_LoadTables eos_loadtables
#define eos_CreateTables eos_createtables
#define eos_DestroyTables eos_destroytables
#define eos_GetErrorCode eos_geterrorcode
#define eos_GetErrorMessage eos_geterrormessage
#define eos_GetPackedTables eos_getpackedtables
#define eos_SetPackedTables eos_setpackedtables
#define eos_GetPackedTablesSize eos_getpackedtablessize
#define eos_SetOption eos_setoption
#define eos_ResetOption eos_resetoption
#define eos_CheckExtrap eos_checkextrap

#else
#define eos_DestroyAll eos_destroyall_
#define eos_Interpolate eos_interpolate_
#define eos_LoadTables eos_loadtables_
#define eos_CreateTables eos_createtables_
#define eos_DestroyTables eos_destroytables_
#define eos_GetErrorCode eos_geterrorcode_
#define eos_GetErrorMessage eos_geterrormessage_
#define eos_GetPackedTables eos_getpackedtables_
#define eos_SetPackedTables eos_setpackedtables_
#define eos_GetPackedTablesSize eos_getpackedtablessize_
#define eos_SetOption eos_setoption_
#define eos_ResetOption eos_resetoption_
#define eos_CheckExtrap eos_checkextrap_

#endif
