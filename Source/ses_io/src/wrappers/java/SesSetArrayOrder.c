/* The java SesSetArrayOrder.c file, which implements the native function */
#include <jni.h>      /* Java Native Interface headers */
#include "MySesIO_SesIO.h"   /* header that defines ses_io java class for wrapper */

#include "ses_defines.h"
#include "stdio.h"
#include <stdlib.h>

#undef DEBUG_WRAP

 
JNIEXPORT jint JNICALL Java_MySesIO_SesIO_SesSetArrayOrder(JNIEnv *env, jobject obj, jint jhandle, jchar jorder)
{

  ses_error_flag return_value = SES_NO_ERROR;

  ses_file_handle handle = (ses_file_handle)jhandle;

    /* 20 Apr 2016, I believe the line of code below is a typo
     
     char the_order = (char)the_order;
     
     and it should be: */
    char the_order = (char)jorder;

#ifdef DEBUG_WRAP
  printf("SesSetArrayOrder.c:  the_handle is %d\n", handle);
#endif

  return_value = ses_set_array_order(handle, the_order);
#ifdef DEBUG_WRAP 
  printf("SesSetArrayOrder.c:  return_value is %d\n", return_value);
#endif
  return return_value;

}

 
