/* The java SesWrite3D.c file, which implements the native function */
#include <jni.h>      /* Java Native Interface headers */
#include "MySesIO_SesIO.h"   /* header that defines ses_io java class for wrapper */

#include "ses_defines.h"
#include "stdio.h"
#include <stdlib.h>

#undef DEBUG_WRAP

 
JNIEXPORT jint JNICALL Java_MySesIO_SesIO_SesWrite3D(JNIEnv *env, jobject obj, jint jhandle, jdoubleArray jbuffer, jint jdim1, jint jdim2, jint jdim3)
{

  ses_error_flag return_value = SES_NO_ERROR;

  ses_file_handle handle = (ses_file_handle)jhandle;
  jdouble* jdbuffer = 
           (*env)-> GetDoubleArrayElements(env, jbuffer, 0);
  double* buffer = (double*)jdbuffer;
  long dim1 = (long)jdim1;
  long dim2 = (long)jdim2;
  long dim3 = (long)jdim3;

#ifdef DEBUG_WRAP
  printf("SesWrite3D.c:  the_handle is %d\n", handle);
#endif

  return_value = ses_write_3D(handle, buffer, dim1, dim2, dim3);
#ifdef DEBUG_WRAP 
  printf("SesWrite3D.c:  return_value is %d\n", return_value);
#endif
  return return_value;

}

 
