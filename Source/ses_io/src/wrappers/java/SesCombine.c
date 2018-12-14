/* The SesCombine.c file, which implements the native function */
#include <jni.h>      /* Java Native Interface headers */
#include "MySesIO_SesIO.h"   /* header that defines ses_io java class for wrapper */

#include "ses_defines.h"
#include "stdio.h"
#include <stdlib.h>

#undef DEBUG_WRAP

 
JNIEXPORT jint JNICALL Java_MySesIO_SesIO_SesCombine(JNIEnv *env, jobject obj, jint jhandle1, jint jhandle2, jstring jnew_filename)
{
  ses_error_flag return_value = SES_NO_ERROR;

  ses_file_handle the_handle1 = (ses_file_handle)jhandle1;
  ses_file_handle the_handle2 = (ses_file_handle)jhandle2;
  const char* new_filename = (*env)->GetStringUTFChars(env, jnew_filename, 0);
  ses_string filename = (ses_string)new_filename;

 #ifdef DEBUG_WRAP
  printf("SesCombine.c:  the_handle1 is %d\n", the_handle1);
  printf("SesCombine.c:  the_handle2 is %d\n", the_handle2);
  printf("SesCombine.c:  new_filename is %s\n", filename);
#endif

  return_value = ses_combine(the_handle1, the_handle2, filename);
#ifdef DEBUG_WRAP 
  printf("SesCombine.c:  return_value is %d\n", return_value);
#endif
  return return_value;

}
