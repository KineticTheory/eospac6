/* The java SesPrintErrorMessage.c file, which implements the native function */
#include <jni.h>      /* Java Native Interface headers */
#include "MySesIO_SesIO.h"   /* header that defines ses_io java class for wrapper */

#include "ses_defines.h"
#include "stdio.h"
#include <stdlib.h>

#undef DEBUG_WRAP

 
JNIEXPORT jstring JNICALL Java_MySesIO_SesIO_SesPrintErrorMessage(JNIEnv *env, jobject obj, jint the_error_message)
{

  ses_string return_value = (ses_string)NULL;

  return_value = ses_print_error_message(the_error_message);
#ifdef DEBUG_WRAP
  if (return_value == (ses_string)NULL) {
     printf("SesPrintErrorMessage.c:  return_value is null\n");
  }
  else {
     printf("SesPrintErrorMessage.c:  return_value is %s\n", return_value);
  }
#endif
  jstring jreturn = (*env)->NewStringUTF(env, return_value);
  return jreturn;

}

 
