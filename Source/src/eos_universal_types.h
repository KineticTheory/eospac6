/*********************************************************************
 * Header for C code to set the generic types used by EOSPAC
 * ---------------------------------------------------------
 * Filetype: (HEADER)
 * 
 * Copyright -- see file named COPYRIGHTNOTICE
 * 
 ********************************************************************/

#ifndef UNIVERSAL_TYPES_HEADER
#define UNIVERSAL_TYPES_HEADER

#if defined (PC)

#define FUNC_INTER __stdcall
typedef long EOS_INTEGER;
typedef unsigned long EOS_INTEGER_UNSIGNED;
typedef double EOS_REAL;
typedef char EOS_CHAR;

#else

#define FUNC_INTER
typedef int EOS_INTEGER;
typedef unsigned int EOS_INTEGER_UNSIGNED;
typedef double EOS_REAL;
typedef char EOS_CHAR;

#endif

/* define BOOLEAN type and values: */
enum EOS_BOOLEAN_enum
{ EOS_FALSE=0, EOS_TRUE=1, EOS_INVALID=-99999 };
typedef enum EOS_BOOLEAN_enum EOS_BOOLEAN;

/* define special table handle values */
#define _EOS_INVALID_TABLE_HANDLE_VALUE           -2
#define _EOS_NEWLY_INITIALIZED_TABLE_HANDLE_VALUE -1

/* define basic type indexes that are stored in an eos_Option struct and
   are related to eos_OptionValue union typedef:
   1: EOS_BOOLEAN, 2: EOS_INTEGER, 3: EOS_REAL, 4: EOS_CHAR* and 5: EOS_INTEGER_UNSIGNED
 */
#define _BOOLEAN_TYPE_INDEX  1
#define _INTEGER_TYPE_INDEX  2
#define _REAL_TYPE_INDEX     3
#define _CHAR_PTR_TYPE_INDEX 4
#define _INTEGER_UNSIGNED_TYPE_INDEX 5;

#endif
