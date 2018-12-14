/* The java SesSetLabel.c file, which implements the native function */
#include <jni.h>      /* Java Native Interface headers */
#include "MySesIO_SesIO.h"   /* header that defines ses_io java class for wrapper */

#include "ses_defines.h"
#include "stdio.h"
#include <stdlib.h>

#undef DEBUG_WRAP

 
JNIEXPORT jint JNICALL Java_MySesIO_SesIO_SesSetLabel(JNIEnv *env, jobject obj, jint jhandle, jstring jlabel)
{

  ses_error_flag return_value = SES_NO_ERROR;

  ses_file_handle handle = (ses_file_handle)jhandle;
  const char* local_string = (*env)->GetStringUTFChars(env, jlabel,  0);
  char* the_label = (char*)local_string;


#ifdef DEBUG_WRAP
  printf("SesSetLabel.c:  the_handle is %d\n", handle);
#endif

  return_value = ses_set_label(handle, the_label);
#ifdef DEBUG_WRAP 
  printf("SesSetLabel.c:  return_value is %d\n", return_value);
#endif
  return return_value;

}

 
