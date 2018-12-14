/* The java SesVersion.c file, which implements the native function */
#include <jni.h>      /* Java Native Interface headers */
#include "MySesIO_SesIO.h"   /* header that defines ses_io java class for wrapper */

#include "ses_defines.h"
#include "stdio.h"
#include <stdlib.h>
#include <string.h>

#undef DEBUG_WRAP

 
JNIEXPORT jstring JNICALL Java_MySesIO_SesIO_SesVersion(JNIEnv *env, jobject obj, jint jhandle)
{

  ses_string my_return_value = (ses_string)NULL;

  ses_file_handle handle = (ses_file_handle)jhandle;

#ifdef DEBUG_WRAP
  printf("SesVersion.c:  the_handle is %d\n", handle);
#endif

  my_return_value = ses_version(handle);
#ifdef DEBUG_WRAP 
  printf("SesVersion.c:  return_value is %s\n", my_return_value);
#endif

  jstring jreturn_value = (*env)->NewStringUTF(env, my_return_value);


  return jreturn_value;

}

 
