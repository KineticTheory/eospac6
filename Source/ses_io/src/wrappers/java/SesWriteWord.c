/* The java SesWriteWord.c file, which implements the native function */
#include <jni.h>      /* Java Native Interface headers */
#include "MySesIO_SesIO.h"   /* header that defines ses_io java class for wrapper */

#include "ses_defines.h"
#include "stdio.h"
#include <stdlib.h>

#undef DEBUG_WRAP

 
JNIEXPORT jint JNICALL Java_MySesIO_SesIO_SesWriteWord(JNIEnv *env, jobject obj, jint jhandle, jdouble jbuffer)
{

  ses_error_flag return_value = SES_NO_ERROR;

  ses_file_handle handle = (ses_file_handle)jhandle;
  double buffer = (double)jbuffer;

#ifdef DEBUG_WRAP
  printf("SesWriteWord.c:  the_handle is %d\n", handle);
#endif

  return_value = ses_write_word(handle, buffer);
#ifdef DEBUG_WRAP 
  printf("SesWriteWord.c:  return_value is %d\n", return_value);
#endif
  return return_value;

}

 
