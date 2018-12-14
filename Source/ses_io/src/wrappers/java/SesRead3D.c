/* The java SesRead3D.c file, which implements the native function */
#include <jni.h>      /* Java Native Interface headers */
#include "MySesIO_SesIO.h"   /* header that defines ses_io java class for wrapper */

#include "ses_defines.h"
#include "stdio.h"
#include <stdlib.h>

#undef DEBUG_WRAP

 
JNIEXPORT jdoubleArray JNICALL Java_MySesIO_SesIO_SesRead3D(JNIEnv *env, jobject obj, jint jhandle, jint jdim1, jint jdim2, jint jdim3)
{
  ses_file_handle handle = (ses_file_handle)jhandle;
  long dim1 = (long)jdim1;
  long dim2 = (long)jdim2;
  long dim3 = (long)jdim3;

#ifdef DEBUG_WRAP
  printf("SesRead3D.c:  the_handle is %d\n", handle);
  printf("SesRead3D.c:  dim1 is %d\n", dim1);
  printf("SesRead3D.c:  dim2 is %d\n", dim2);
  printf("SesRead3D.c:  dim3 is %d\n", dim3);
#endif

  ses_word_reference buffer = malloc(sizeof(double)*dim1*dim2*dim3);
  ses_error_flag my_return_value = ses_read_3D(handle, buffer, dim1, dim2, dim3);
#ifdef DEBUG_WRAP 
  printf("SesRead3D.c:  buffer[3] is %e\n", buffer[3]);
  printf("SesRead3D.c:  my return value is %d\n", my_return_value);
#endif

   jdoubleArray return_value  = (*env)->NewDoubleArray(env, dim1*dim2*dim3);
  (*env)->SetDoubleArrayRegion(env, return_value, 0, dim1*dim2*dim3, (jdouble*)buffer);


  /*  free(buffer) */

  my_return_value = 0;

  return return_value;

}

 
