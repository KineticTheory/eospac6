/* The java SesChangeNext.c file, which implements the native function */
#include <jni.h>      /* Java Native Interface headers */
#include "MySesIO_SesIO.h"   /* header that defines ses_io java class for wrapper */

#include "ses_defines.h"
#include "stdio.h"
#include <stdlib.h>

#undef DEBUG_WRAP

 
JNIEXPORT jint JNICALL Java_MySesIO_SesIO_SesChangeNext(JNIEnv *env, jobject obj, jint jhandle, jdoubleArray jbuffer, jlong jdim)
{

  ses_error_flag return_value = SES_NO_ERROR;

  ses_file_handle the_handle = (ses_file_handle)jhandle;
  jdouble* jdbuffer = 
           (*env)-> GetDoubleArrayElements(env, jbuffer, 0);
  double* buffer = (double*)jdbuffer;
  long dim = (long)jdim;
 
#ifdef DEBUG_WRAP
  printf("SesChangeNext.c:  the_handle is %d\n", the_handle);
  printf("SesChangeNext.c:  what's in the_buffer is %e\n", buffer[0]);
  printf("SesChangeNext.c:  dim1 is %ld\n", dim);
#endif

  return_value = ses_change_next(the_handle, buffer, dim);
#ifdef DEBUG_WRAP 
  printf("SesChangeNext.c:  return_value is %d\n", return_value);
#endif

  return return_value;


}

 
