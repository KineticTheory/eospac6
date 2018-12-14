/* The java SesWritePairs.c file, which implements the native function */
#include <jni.h>      /* Java Native Interface headers */
#include "MySesIO_SesIO.h"   /* header that defines ses_io java class for wrapper */

#include "ses_defines.h"
#include "stdio.h"
#include <stdlib.h>

#undef DEBUG_WRAP

 
JNIEXPORT jint JNICALL Java_MySesIO_SesIO_SesWritePairs(JNIEnv *env, jobject obj, jint jhandle, jdoubleArray jbuf1, jdoubleArray jbuf2, jlong jdim)
{

  ses_error_flag return_value = SES_NO_ERROR;

  ses_file_handle handle = (ses_file_handle)jhandle;
  jdouble* jdbuffer1 = 
           (*env)-> GetDoubleArrayElements(env, jbuf1, 0);
  double* buf1 = (double*)jdbuffer1;
  jdouble* jdbuffer2 = 
           (*env)-> GetDoubleArrayElements(env, jbuf2, 0);
  double* buf2 = (double*)jdbuffer2;
  long dim = (long)jdim;

#ifdef DEBUG_WRAP
  printf("SesWritePairs.c:  the_handle is %d\n", handle);
#endif

  return_value = ses_write_pairs(handle, buf1, buf2, dim);
#ifdef DEBUG_WRAP 
  printf("SesWritePairs.c:  return_value is %d\n", return_value);
#endif
  return return_value;

}

 
