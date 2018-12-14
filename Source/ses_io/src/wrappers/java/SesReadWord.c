/* The java SesReadWord.c file, which implements the native function */
#include <jni.h>      /* Java Native Interface headers */
#include "MySesIO_SesIO.h"   /* header that defines ses_io java class for wrapper */

#include "ses_defines.h"
#include "stdio.h"
#include <stdlib.h>

#undef DEBUG_WRAP

 
JNIEXPORT jdouble JNICALL Java_MySesIO_SesIO_SesReadWord(JNIEnv *env, jobject obj, jint jhandle)
{

  ses_error_flag return_value = SES_NO_ERROR;

  ses_file_handle handle = (ses_file_handle)jhandle;

  double buffer;

#ifdef DEBUG_WRAP
  printf("SesReadWord.c:  the_handle is %d\n", handle);
#endif

  return_value = ses_read_word(handle, &buffer);

  jdouble jbuffer = (jdouble)buffer;
#ifdef DEBUG_WRAP 
  printf("SesReadWord.c:  return_value is %d\n", return_value);
#endif
  return jbuffer;

}

 
