/* The java SesClose.c file, which implements the native function */
#include <jni.h>      /* Java Native Interface headers */
#include "MySesIO_SesIO.h"   /* header that defines ses_io java class for wrapper */

#include "ses_defines.h"
#include "stdio.h"
#include <stdlib.h>

#undef DEBUG_WRAP

 
JNIEXPORT jint JNICALL Java_MySesIO_SesIO_SesClose(JNIEnv *env, jobject obj, jint jhandle)
{
  ses_error_flag return_value = SES_NO_ERROR;
  ses_file_handle the_handle = (ses_file_handle)jhandle;

#ifdef DEBUG_WRAP
  printf("SesClose.c:  the_handle is %d\n", the_handle);
#endif

  return_value = ses_close(the_handle);
#ifdef DEBUG_WRAP 
  printf("SesClose.c:  return_value is %d\n", return_value);
#endif
  return return_value;

}
