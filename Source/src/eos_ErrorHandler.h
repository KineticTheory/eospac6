/*********************************************************************
 * Class Name : eos_ErrorHandler
 * ---------------------------------------------------------
 * Filetype: (HEADER)
 * 
 * Copyright -- see file named COPYRIGHTNOTICE
 * 
 ********************************************************************/
#ifndef EOS_ERRORS_H
#define EOS_ERRORS_H
#include "eos_types.h"
#include "eos_universal_types.h"


typedef struct
{
  EOS_INTEGER errorCode;
  void (*HandleError) (void *me, EOS_INTEGER th, EOS_INTEGER error_code);       // virtual handler function
} eos_ErrorHandler;

#include "eos_ErrorHandler.proto.h"

#endif /* EOS_ERRORS_H */
