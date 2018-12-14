/* The java SesReadNumber.c file, which implements the native function */
#include <jni.h>      /* Java Native Interface headers */
#include "MySesIO_SesIO.h"   /* header that defines ses_io java class for wrapper */

#include "ses_defines.h"
#include "stdio.h"
#include <stdlib.h>

#undef DEBUG_WRAP

 
JNIEXPORT jlong JNICALL Java_MySesIO_SesIO_SesReadNumber(JNIEnv *env, jobject obj, jint jhandle)             
{

  struct my_object {
    jlong my_number;
    jint my_error_flag;
  }my_return_value;

  ses_error_flag return_value = SES_NO_ERROR;
  ses_number  my_buffer;

  ses_file_handle handle = (ses_file_handle)jhandle;

#ifdef DEBUG_WRAP
  printf("SesReadNumber.c:  the_handle is %d\n", handle);
#endif

  return_value = ses_read_number(handle, &my_buffer);

#ifdef DEBUG_WRAP 
  printf("SesReadNumber.c:  return_value is %d\n", return_value);
  printf("SesReadNumber.c:  buffer is %d\n", my_buffer);
#endif

  my_return_value.my_number = (jlong)my_buffer;
  my_return_value.my_error_flag = (jint)return_value;

  return my_return_value.my_number;

}

 
