/* The java SesGetLabel.c file, which implements the native function */
#include <jni.h>      /* Java Native Interface headers */
#include "MySesIO_SesIO.h"   /* header that defines ses_io java class for wrapper */

#include "ses_defines.h"
#include "stdio.h"
#include <stdlib.h>

#undef DEBUG_WRAP

 
JNIEXPORT jstring JNICALL Java_MySesIO_SesIO_SesGetComments(JNIEnv *env, jobject obj, jint jhandle)
{

  ses_string return_value = (ses_string)NULL;

  ses_file_handle handle = (ses_file_handle)jhandle;

#ifdef DEBUG_WRAP
  printf("SesGetComments.c:  the_handle is %d\n", handle);
#endif

  ses_string my_string = (ses_string)NULL;
  ses_error_flag my_return_value = ses_get_comments(handle, &my_string);
#ifdef DEBUG_WRAP 
  printf("SesGetComments.c:  return_value is %d\n", my_return_value);
  printf("SesGetComments.c : my_string is %s\n", my_string);
#endif

  my_return_value = 0;
  return_value = 0;

  jstring jreturn_value = (*env)->NewStringUTF(env, my_string);
  return jreturn_value;

}

 
