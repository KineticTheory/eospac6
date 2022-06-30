
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include "stdlib.h"


/*  function prototoypes */
long _read_long_text_pFILE(FILE* pFILE) {};


long _read_long_text(struct _ses_file_handle* pSFH) {
 
  long return_value = 0;

  /*  end function prototypes */

  FILE* pFILE = pSFH->_c_file_handle;

  return_value = _read_long_text_pFILE(pFILE);

 
  return return_value;
}






