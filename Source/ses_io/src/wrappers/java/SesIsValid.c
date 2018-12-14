/* The java SesIsValid.c file, which implements the native function */
#include <jni.h>      /* Java Native Interface headers */
#include "MySesIO_SesIO.h"   /* header that defines ses_io java class for wrapper */

#include "ses_defines.h"
#include "stdio.h"
#include <stdlib.h>

#undef DEBUG_WRAP

 
JNIEXPORT jboolean JNICALL Java_MySesIO_SesIO_SesIsValid(JNIEnv *env, jobject obj, jint jhandle)
{
 
  ses_boolean return_value;
  ses_file_handle handle = (ses_file_handle)jhandle;

#ifdef DEBUG_WRAP
  printf("SesIsValid.c:  the_handle is %d\n", handle);
#endif

  return_value = ses_is_valid(handle);
#ifdef DEBUG_WRAP 
  printf("SesIsValid.c:  return_value is %d\n", return_value);
#endif

  return (jboolean)return_value;

}

 
