/* The java SesReadNext.c file, which implements the native function */
#include <jni.h>      /* Java Native Interface headers */
#include "MySesIO_SesIO.h"   /* header that defines ses_io java class for wrapper */

#include "ses_defines.h"
#include "stdio.h"
#include <stdlib.h>

#undef DEBUG_WRAP

 
JNIEXPORT jdoubleArray JNICALL Java_MySesIO_SesIO_SesReadNext(JNIEnv *env, jobject obj, jint jhandle, jint jdim)
{


  ses_file_handle handle = (ses_file_handle)jhandle;
  int dim = jdim;

#ifdef DEBUG_WRAP
  printf("SesReadNext.c:  the_handle is %d\n", handle);
  printf("SesReadNext.c:  dim is %d\n", dim);
#endif

  ses_word_reference my_return_value = ses_read_next(handle);

  jdoubleArray return_value = (*env)->NewDoubleArray(env, dim);
  (*env)->SetDoubleArrayRegion(env, return_value, 0, dim, (jdouble*)my_return_value);

 #ifdef DEBUG_WRAP 
  printf("SesReadNext.c:  return_value[0] is %e\n", my_return_value[0]);
#endif
  return return_value;

}

 
