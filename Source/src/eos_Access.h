/*********************************************************************
 * Class Name : eos_Access
 * ---------------------------------------------------------
 * Filetype: (HEADER)
 * 
 * Copyright -- see file named COPYRIGHTNOTICE
 * 
 *********************************************************************/
#ifndef  EOS_ACCESS_H
#define  EOS_ACCESS_H
#include "eos_types.h"
#include "eos_ErrorHandler.h"
#include "eos_universal_types.h"

typedef struct
{
  eos_ErrorHandler eosErrorHandler;     // must be the FIRST, DO NOT MOVE!
} eos_Access;

#include "eos_Access.proto.h"

#endif
