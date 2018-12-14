/* The java SesDeleteTable.c file, which implements the native function */
#include <jni.h>      /* Java Native Interface headers */
#include "MySesIO_SesIO.h"   /* header that defines ses_io java class for wrapper */

#include "ses_defines.h"
#include "stdio.h"
#include <stdlib.h>

#undef DEBUG_WRAP

 
JNIEXPORT jint JNICALL Java_MySesIO_SesIO_SesDeleteTable(JNIEnv *env, jobject obj, jint jhandle)
{

  ses_error_flag return_value = 0;

  ses_file_handle handle = (ses_file_handle)jhandle;

#ifdef DEBUG_WRAP
  printf("SesDeleteTable.c:  the_handle is %d\n", handle);
#endif

   return_value = ses_delete_table(handle);
#ifdef DEBUG_WRAP 
  printf("SesDeleteTable.c:  return_value is %d\n", return_value);
#endif
  return return_value;

}

 
