/*********************************************************************
 * Unit Test API Functions
 * ---------------------------------------------------------
 * Filetype: (HEADER)
 * 
 * Copyright -- see file named COPYRIGHTNOTICE
 * 
 ********************************************************************/
#ifndef TEST_FUNCTIONS_HEADERFILE
#define TEST_FUNCTIONS_HEADERFILE
#include "eos_universal_types.h"
#include "stdlib.h"

#ifdef __cplusplus
# define _EXTERN_C_HEAD_ extern "C" {
# define _EXTERN_C_TAIL_ }
#else
# define _EXTERN_C_HEAD_
# define _EXTERN_C_TAIL_
#endif

#define ERROR_TO_TEXT(i) ((i==EOS_xHi_yHi) ? "EOS_xHi_yHi" : \
                          (i==EOS_xHi_yOk) ? "EOS_xHi_yOk" : \
                          (i==EOS_xHi_yLo) ? "EOS_xHi_yLo" : \
                          (i==EOS_xOk_yLo) ? "EOS_xOk_yLo" : \
                          (i==EOS_xLo_yLo) ? "EOS_xLo_yLo" : \
                          (i==EOS_xLo_yOk) ? "EOS_xLo_yOk" : \
                          (i==EOS_xLo_yHi) ? "EOS_xLo_yHi" : \
                          (i==EOS_xOk_yHi) ? "EOS_xOk_yHi" : \
                          (i==EOS_UNDEFINED) ? "EOS_UNDEFINED" : \
                          (i==EOS_CONVERGENCE_FAILED) ? "EOS_CONVERGENCE_FAILED" : \
                          (i==EOS_CANT_INVERT_DATA) ? "EOS_CANT_INVERT_DATA" : \
                          (i==EOS_OK     ) ? "EOS_OK" : \
                          "INVALID EXTRAP ERROR" \
                         )

_EXTERN_C_HEAD_

#include "TEST_FUNCTIONS.proto.h"
#include "eos_SaferMemoryAllocation.proto.h"

/* Define hidden option flags that are currently not defined in the public interface.
 *  NOTE: If you want to use these in a test code, copy these defines into your code. */
#define EOS_NUM_PRIVATE_OPTIONS 4
#define EOS_MIN_PRIVATE_OPTION_FLAG_VALUE   11000       /* Minimum private option flag value */
static const EOS_INTEGER DISABLE_FTBLS_INVT_MASK = 11000; /* Disable FTBLS_INVT_MASK function */
static const EOS_INTEGER EOS_DEBUG_PRINT = 11001; /* Enable DEBUG_PRINT function */
static const EOS_INTEGER EOS_DISABLE_GHOST_NODES = 11002;      /* Disable the forced usage of ghost node data during interpolation */
static const EOS_INTEGER EOS_ALLOW_ALL_INFO_ITEMS = 11003;  /* Override category restrictions related to selected table information parameters */

/* Repeat some internal function prototypes here */
EOS_INTEGER get_VarListIndex(EOS_INTEGER t);
EOS_CHAR* get_VarStr(EOS_INTEGER t, EOS_INTEGER flag);

_EXTERN_C_TAIL_

#endif
