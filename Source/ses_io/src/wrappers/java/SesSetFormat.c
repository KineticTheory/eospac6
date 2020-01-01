/* The java SesSetDate.c file, which implements the native function */
#include <jni.h>      /* Java Native Interface headers */
#include "MySesIO_SesIO.h"   /* header that defines ses_io java class for wrapper */

#include "ses_defines.h"
#include "stdio.h"
#include <stdlib.h>

#define TRY_OPTIONAL

#undef DEBUG_WRAP

JNIEXPORT jint JNICALL Java_MySesIO_SesIO_SesSetFormat(JNIEnv *env, jobject obj, jint jhandle, jchar jformat, jchar ascii_format)
{

  ses_error_flag return_value = SES_NO_ERROR;

  ses_file_handle handle = (ses_file_handle)jhandle;
  char format = (char)jformat;

#ifdef DEBUG_WRAP
  printf("SesSetFormat.c:  the_handle is %d\n", handle);
#endif

#ifdef TRY_OPTIONAL
  return_value = ses_set_format(3, handle, format, ascii_format);
#else
    return_value = ses_set_format(handle, format, ascii_format);
#endif
    
#ifdef DEBUG_WRAP 
  printf("SesSetFormat.c:  return_value is %d\n", return_value);
#endif
  return return_value;

}

 
