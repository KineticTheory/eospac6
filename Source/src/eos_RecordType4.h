/*********************************************************************
 * Class Name : eos_RecordType4
 * ---------------------------------------------------------
 * Filetype: (HEADER)
 * 
 * Copyright -- see file named COPYRIGHTNOTICE
 * 
 *********************************************************************/
#ifndef  EOS_RECORDTYPE4_H
#define  EOS_RECORDTYPE4_H

#include "eos_Data.h"
#include "eos_SesUtils.h"

// This class is designed to manipulate Sesame data stored in format #4 -- this is specific to Sesame tables 101-199.

typedef struct
{
  eos_Data eosData;             /* must be the FIRST, DO NOT MOVE! */
  /* Number of 100-series Sesame tables available. */
  EOS_INTEGER numberTables;
  EOS_INTEGER *tableNum;        /* table numbers, example: 101 */
  EOS_INTEGER numberAllocatedTables;
  // Array of character strings to store comment tables.
  EOS_CHAR **comment;           /*[0..(numberTables-1)][0..(numChars] */
  EOS_INTEGER *numChars;        /*[0..(numberTables-1)] */
  EOS_INTEGER *dataFileOffsets; /*[0..(numberTables-1)] */

} eos_RecordType4;

#include "eos_RecordType4.proto.h"

#endif
