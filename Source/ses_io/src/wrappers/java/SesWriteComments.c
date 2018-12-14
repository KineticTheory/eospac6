/* The java SesWriteComments.c file, which implements the native function */
#include <jni.h>      /* Java Native Interface headers */
#include "MySesIO_SesIO.h"   /* header that defines ses_io java class for wrapper */

#include "ses_defines.h"
#include "stdio.h"
#include <stdlib.h>

#undef DEBUG_WRAP

 
JNIEXPORT jint JNICALL Java_MySesIO_SesIO_SesWriteComments(JNIEnv *env, jobject obj, jint jhandle, jstring jcomments, jlong jdim)
{

  ses_error_flag return_value = SES_NO_ERROR;

  ses_file_handle handle = (ses_file_handle)jhandle;
  const char* local_string = (*env)->GetStringUTFChars(env, jcomments,  0);
  char* comments = (char*)local_string;
  long dim = (long)jdim;

#ifdef DEBUG_WRAP
  printf("SesWriteComments.c:  the_handle is %d\n", handle);
#endif

  return_value = ses_write_comments(handle, comments, dim);
#ifdef DEBUG_WRAP 
  printf("SesWritecomments.c:  return_value is %d\n", return_value);
#endif
  return return_value;

}

 
