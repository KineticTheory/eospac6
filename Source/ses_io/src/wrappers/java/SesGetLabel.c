/* The java SesGetLabel.c file, which implements the native function */
#include <jni.h>      /* Java Native Interface headers */
#include "MySesIO_SesIO.h"   /* header that defines ses_io java class for wrapper */

#include "ses_defines.h"
#include "stdio.h"
#include <stdlib.h>

#undef DEBUG_WRAP

 
JNIEXPORT jstring JNICALL Java_MySesIO_SesIO_SesGetLabel(JNIEnv *env, jobject obj, jint jhandle)
{

  ses_label return_value = (ses_label)NULL;

  ses_file_handle handle = (ses_file_handle)jhandle;

#ifdef DEBUG_WRAP
  printf("SesGetLabel.c:  the_handle is %d\n", handle);
#endif

  return_value = ses_get_label(handle);
#ifdef DEBUG_WRAP 
  printf("SesGetLabel.c:  return_value is %s\n", return_value);
#endif

  jstring jreturn_value = (*env)->NewStringUTF(env, return_value);
  return jreturn_value;

}

 
