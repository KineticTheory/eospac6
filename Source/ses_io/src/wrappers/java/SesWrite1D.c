/* The java_array_size_next.c file, which implements the native function */
#include <jni.h>      /* Java Native Interface headers */
#include "MySesIO_SesIO.h"   /* header that defines ses_io java class for wrapper */

#include "ses_defines.h"
#include "stdio.h"
#include <stdlib.h>

#undef DEBUG_WRAP

 
JNIEXPORT jint JNICALL Java_MySesIO_SesIO_SesWrite1D(JNIEnv *env, jobject obj, jint jhandle, jdoubleArray jbuffer, jint jdim)
{

  ses_error_flag return_value;

  ses_file_handle handle = (ses_file_handle)jhandle;
  jdouble* jdbuffer = 
           (*env)-> GetDoubleArrayElements(env, jbuffer, 0);
  double* buffer = (double*)jdbuffer;
  long dim = (long)jdim;
 

#ifdef DEBUG_WRAP
  printf("SesWrite1D.c:  the_handle is %d\n", handle);
#endif

  return_value = ses_write_1D(handle, buffer, dim);
#ifdef DEBUG_WRAP 
  printf("SesWrite1D.c:  return_value is %d\n", return_value);
#endif
  return return_value;

}

 
