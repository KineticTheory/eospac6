/* The java SesWrite2D.c file, which implements the native function */
#include <jni.h>      /* Java Native Interface headers */
#include "MySesIO_SesIO.h"   /* header that defines ses_io java class for wrapper */

#include "ses_defines.h"
#include "stdio.h"
#include <stdlib.h>

#undef DEBUG_WRAP

 
JNIEXPORT jint JNICALL Java_MySesIO_SesIO_SesWrite2D(JNIEnv *env, jobject obj, jint jhandle, jdoubleArray jbuffer, jint jdim1, jint jdim2)
{

  ses_error_flag return_value = SES_NO_ERROR;

  ses_file_handle handle = (ses_file_handle)jhandle;
  jdouble* jdbuffer = 
           (*env)-> GetDoubleArrayElements(env, jbuffer, 0);
  double* buffer = (double*)jdbuffer;
  long dim1 = (long)jdim1;
  long dim2 = (long)jdim2;

#ifdef DEBUG_WRAP
  printf("SesWrite2D.c:  the_handle is %d\n", handle);
  printf("SesWrite2D.c:  dim1 is %ld\n", dim1);
  printf("SesWrite2D.c:  dim2 is %ld\n", dim2);
#endif

  return_value = ses_write_2D(handle, buffer, dim1, dim2);
#ifdef DEBUG_WRAP 
  printf("SesWrite2D.c:  return_value is %d\n", return_value);
#endif
  return return_value;

}

 
