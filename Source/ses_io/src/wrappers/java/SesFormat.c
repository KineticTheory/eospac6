/* The java SesFormat.c file, which implements the native function */
#include <jni.h>      /* Java Native Interface headers */
#include "MySesIO_SesIO.h"   /* header that defines ses_io java class for wrapper */

#include "ses_defines.h"
#include "stdio.h"
#include <stdlib.h>
#include <string.h>

#undef DEBUG_WRAP

JNIEXPORT jstring JNICALL Java_MySesIO_SesIO_SesFormat(JNIEnv *env, jobject obj, jint jhandle)
{

  ses_string return_value = (ses_string)NULL;
  ses_file_handle the_handle = (ses_file_handle)jhandle;
#ifdef DEBUG_WRAP
  printf("SesFormat.c:  the_handle is %d\n", the_handle);
#endif

 return_value = ses_format(the_handle);
 
#ifdef DEBUG_WRAP
  printf("SesFormat.c:  returning string = %s\n", return_value);
#endif
 
  jstring jreturn_value = (*env)->NewStringUTF(env, return_value);

  return jreturn_value;
}

