/* The java SesRead2D.c file, which implements the native function */
#include <jni.h>      /* Java Native Interface headers */
#include "MySesIO_SesIO.h"   /* header that defines ses_io java class for wrapper */

#include "ses_defines.h"
#include "stdio.h"
#include <stdlib.h>

#undef DEBUG_WRAP

 
JNIEXPORT jdoubleArray JNICALL Java_MySesIO_SesIO_SesRead2D(JNIEnv *env, jobject obj, jint jhandle, jint jdim1, jint jdim2)
{
  ses_file_handle handle = (ses_file_handle)jhandle;
  long dim1 = (long)jdim1;
  long dim2 = (long)jdim2;

#ifdef DEBUG_WRAP
  printf("SesRead2D.c:  the_handle is %d\n", handle);
  printf("SesRead2D.c:  dim1 is %d\n", dim1);
  printf("SesRead2D.c:  dim2 is %d\n", dim2);
#endif

  ses_word_reference buffer = malloc(sizeof(double)*dim1*dim2);
  ses_error_flag my_return_value = ses_read_2D(handle, buffer, dim1, dim2);

#ifdef DEBUG_WRAP 
  printf("SesRead2D.c:  buffer[3] is %e\n", buffer[3]);
#endif
  jdoubleArray return_value = (*env)->NewDoubleArray(env, dim1*dim2);
  (*env)->SetDoubleArrayRegion(env, return_value, 0, dim1*dim2, (jdouble*)buffer);

  /*  free(buffer); */

  my_return_value = 0;

  return return_value;

}

 
