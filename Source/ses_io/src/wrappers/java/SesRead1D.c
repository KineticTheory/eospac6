/* The java_array_size_next.c file, which implements the native function */
#include <jni.h>      /* Java Native Interface headers */
#include "MySesIO_SesIO.h"   /* header that defines ses_io java class for wrapper */

#include "ses_defines.h"
#include "stdio.h"
#include <stdlib.h>

#undef DEBUG_WRAP

 
JNIEXPORT jdoubleArray JNICALL Java_MySesIO_SesIO_SesRead1D(JNIEnv *env, jobject obj, jint jhandle, jint jdim)
{

  ses_file_handle handle = (ses_file_handle)jhandle;
  long dim = (long)jdim;


#ifdef DEBUG_WRAP
  printf("SesRead1D.c:  the_handle is %d\n", handle);
  printf("SesRead1D.c:  dim is %d\n", dim);
#endif

  ses_word_reference buffer = malloc(sizeof(double)*dim);
  ses_error_flag my_return_value = ses_read_1D(handle, buffer, dim);
#ifdef DEBUG_WRAP
  printf("SesRead1D.c:  buffer[3] is %e\n", buffer[3]);
#endif

  my_return_value = 0;
  jdoubleArray return_value = (*env)->NewDoubleArray(env, dim);
  (*env)->SetDoubleArrayRegion(env, return_value, 0, dim, (jdouble*)buffer);


  /*  free(buffer); */
 
  return return_value;

}

 
