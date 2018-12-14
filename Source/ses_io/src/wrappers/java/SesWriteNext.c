/* The java SesWriteNext.c file, which implements the native function */
#include <jni.h>      /* Java Native Interface headers */
#include "MySesIO_SesIO.h"   /* header that defines ses_io java class for wrapper */

#include "ses_defines.h"
#include "stdio.h"
#include <stdlib.h>
#include <string.h>

#undef DEBUG_WRAP

 
JNIEXPORT jint JNICALL Java_MySesIO_SesIO_SesWriteNext(JNIEnv *env, jobject obj, jint jhandle, jdoubleArray jbuffer, jint jdim, jstring jlabel )
{

  ses_error_flag return_value = SES_NO_ERROR;

  ses_file_handle handle = (ses_file_handle)jhandle;
  jdouble* jdbuffer = 
           (*env)-> GetDoubleArrayElements(env, jbuffer, 0);
  double* buffer = (double*)jdbuffer;
  long dim = (long)jdim;

  const char* local_string = (*env)->GetStringUTFChars(env, jlabel,  0);
  char* the_label = (char*)local_string;

#ifdef DEBUG_WRAP
  printf("SesWriteNext.c:  the_handle is %d\n", handle);
  printf("SesWriteNext.c:  the_buffer[0] is %e\n", buffer[0]);
  printf("SesWriteNext.c:  dim is %ld\n", dim);
  printf("SesWriteNext.c:  label is %s\n", the_label);
#endif

  return_value = ses_write_next(handle, buffer, dim, the_label);
#ifdef DEBUG_WRAP 
  printf("SesWriteNext.c:  return_value is %d\n", return_value);
#endif
  return return_value;

}

 
