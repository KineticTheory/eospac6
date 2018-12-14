/* The java SesGetVersion.c file, which implements the native function */
#include <jni.h>      /* Java Native Interface headers */
#include "MySesIO_SesIO.h"   /* header that defines ses_io java class for wrapper */

#include "ses_defines.h"
#include "stdio.h"
#include <stdlib.h>

#undef DEBUG_WRAP

 
JNIEXPORT jlong JNICALL Java_MySesIO_SesIO_SesGetVersion(JNIEnv *env, jobject obj, jint jhandle)
{

  jlong jreturn_value = (long)NULL;

  ses_file_handle handle = (ses_file_handle)jhandle;

#ifdef DEBUG_WRAP
  printf("SesGetVersion.c:  the_handle is %d\n", handle);
#endif

  jreturn_value = (jlong)ses_get_label(handle);
#ifdef DEBUG_WRAP 
  printf("SesGetVersion.c:  return_value is %s\n", (long)return_value);
#endif

  return jreturn_value;

}

 
