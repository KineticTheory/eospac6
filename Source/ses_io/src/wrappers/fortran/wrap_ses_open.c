

#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_internals.h"
#include "ses_externs.h"

#include "stdio.h"
#include <stdlib.h>

#include <stdio.h>

#undef DEBUG_WRAP

#ifdef LC_UNDER
ses_file_handle ses_open_( ses_string filename, ses_open_type* p_open_flags){
#else

#ifdef UC_UNDER
ses_file_handle SES_OPEN_( ses_string filename, ses_open_type* p_open_flags){
#else

#ifdef LC_NOUNDER
ses_file_handle ses_open( ses_string filename, ses_open_type* p_open_flags) {
#else

#ifdef UC_NOUNDER
ses_file_handle SES_OPEN( ses_string filename, ses_open_type* p_open_flags) {
#endif

#endif
#endif
#endif

  

  ses_open_type open_flags = *p_open_flags;
#ifdef DEBUG_WRAP
  printf("wrap_ses_open.c:  entered with filename is %s\n", filename);
  printf("wrap_ses_open.c:  open_flags is %c\n", open_flags);
#endif

  ses_file_handle return_value = 0;

  return_value = ses_open(filename, open_flags);
#ifdef DEBUG_WRAP
  printf("wrap_ses_open.c:  returning %d\n", return_value);
#endif
  return return_value;

}

 
