/* The java SesPrintErrorCondtion.c file, which implements the native function */
#include <jni.h>      /* Java Native Interface headers */
#include "MySesIO_SesIO.h"   /* header that defines ses_io java class for wrapper */

#include "ses_defines.h"
#include "stdio.h"
#include <stdlib.h>

#undef DEBUG_WRAP

 
JNIEXPORT jstring JNICALL Java_MySesIO_SesIO_SesPrintErrorCondition(JNIEnv *env, jobject obj, jint jhandle)
{

  ses_string return_value = (ses_string)NULL;
  ses_file_handle handle = (ses_file_handle)jhandle;

#ifdef DEBUG_WRAP
  printf("SesPrintErrorCondition:  the_handle is %d\n", handle);
#endif

  return_value = ses_print_error_condition(handle);
#ifdef DEBUG_WRAP
  if (return_value == (ses_string)NULL) {
     printf("SesPrintErrorCondition.c:  return_value is null\n");
  }
  else {
     printf("SesPrintErrorCondition.c:  return_value is %s\n", return_value);
  }
#endif
  jstring jreturn = (*env)->NewStringUTF(env, return_value);
  return jreturn;

}

 
