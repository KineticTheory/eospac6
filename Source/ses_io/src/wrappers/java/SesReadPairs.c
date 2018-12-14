/* The java_array_size_next.c file, which implements the native function */
#include <jni.h>      /* Java Native Interface headers */
#include "MySesIO_SesIO.h"   /* header that defines ses_io java class for wrapper */

#include "ses_defines.h"
#include "stdio.h"
#include <stdlib.h>

#undef DEBUG_WRAP

 
JNIEXPORT jint JNICALL Java_MySesIO_SesIO_SesReadPairs(JNIEnv *env, jobject obj, jint jhandle, jdoubleArray jbuf1, jdoubleArray jbuf2, jlong jdim)
{

  ses_error_flag my_return_value = SES_NO_ERROR;


#ifdef DEBUG_WRAP
  printf("SesReadPairs.c:  the_handle is %d\n", handle);
#endif

  printf("SesReadPairs.c:  Function not yet implemented in java \n"); 


#ifdef DEBUG_WRAP 
  printf("SesReadPairs.c:  return_value is %d\n", my_return_value);
#endif
  return my_return_value;

}

 
