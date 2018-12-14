/*********************************************************************
 * Class Name : eos_Access
 * ---------------------------------------------------------
 * Filetype: (SOURCE)
 *
 * Copyright -- see file named COPYRIGHTNOTICE
 *
 *********************************************************************/

#include <stdlib.h>
#include <stdio.h>

#define _EOS_ACCESS_INTERNAL_PROTOTYPES

#include "eos_types_internal.h"
#include "eos_Data.h"
#include "eos_Access.h"
#include "eos_Utils.h"

/************************************************************************
 * 
 * eos_Data class destructor
 * 
 * Returned Values: none
 *
 * Input Value:
 * eos_Data **me  - this pointer (pointer to the instance of type eos_Data)
 * 
 ************************************************************************/
void eos_DestroyEosAccess (eos_Access *me)
{
  eos_DestroyEosErrorHandler (&(me->eosErrorHandler));
}

/************************************************************************
 * 
 * eosAccess class constructor
 * 
 * Returned Values: none
 *
 * Input Value:
 * eos_Access *me  - this pointer (pointer to the instance of type eos_eosAccess
 * 
 ************************************************************************/
void eos_ConstructEosAccess (eos_Access *me)
{
  eos_ConstructEosErrorHandler ((eos_ErrorHandler *) me);
  me->eosErrorHandler.HandleError = eos_HandleErrorEosAccess;   /* derived virtual function */
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
void eos_HandleErrorEosAccess (void *ptr, EOS_INTEGER th,
                               EOS_INTEGER errorCode)
{
  eos_ErrorHandler *me = (eos_ErrorHandler *) ptr;
  gEosDataMap.errorCodes[th] = errorCode;
  me->errorCode = errorCode;
#ifdef DEBUG
  printf ("eos_Access: error is: %s\n", eos_GetErrorMsg (errorCode));
#endif
}
