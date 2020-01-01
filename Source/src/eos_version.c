/*
 *.......................................................................................
 *  ./eos_version.c
 *    Creation date: 02-25-2019 01:02:23 PM MST
 *    This file is automatically generated by eospac6.4.0/Source/scripts/eos_version.pl
 *.......................................................................................
 */
#include "eos_types.h"
#include "eos_wrappers.h"
#include <string.h>

#ifdef __cplusplus
extern "C"
{
#endif

#ifdef _EOS_INCLUDE_SPECIAL_FUNCTION
void eos_version_name_EOSPAC_v6_4_0_612ea8c9b8ffa6d9175d9118955571d9107f1e3c()
{
  return;
}
#endif

static EOS_CHAR *eos_version_info_msg = "EOSPAC v6.4.0 612ea8c9b8ffa6d9175d9118955571d9107f1e3c";
void FUNC_INTER eos_GetVersionLength(EOS_INTEGER *length)
{
  /* include space for the '\0' character */
  *length = strlen(eos_version_info_msg) + 1;
}

void FUNC_INTER eos_GetVersion(EOS_CHAR *version)
{
  strcpy(version, eos_version_info_msg);
  return;
}

#ifdef __cplusplus
}
#endif

