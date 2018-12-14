/* The java SesGetAvailableFormats.c file, which implements the native function */
#include <jni.h>      /* Java Native Interface headers */
#include "MySesIO_SesIO.h"   /* header that defines ses_io java class for wrapper */

#include "ses_defines.h"
#include "stdio.h"
#include <stdlib.h>
#include <string.h>

#undef DEBUG_WRAP

JNIEXPORT jstring JNICALL Java_MySesIO_SesIO_SesGetAvailableFormats(JNIEnv *env, jobject obj)
{

  ses_string return_value = (ses_string)NULL;

 return_value = ses_get_available_formats();
 
 
  jstring jreturn_value = (*env)->NewStringUTF(env, return_value);

  return jreturn_value;
}

