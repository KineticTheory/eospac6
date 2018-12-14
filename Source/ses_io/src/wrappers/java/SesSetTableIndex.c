/* The java_array_size_next.c file, which implements the native function */
#include <jni.h>      /* Java Native Interface headers */
#include "MySesIO_SesIO.h"   /* header that defines ses_io java class for wrapper */

#include "ses_defines.h"
#include "stdio.h"
#include <stdlib.h>

#undef DEBUG_WRAP

 
JNIEXPORT jint JNICALL Java_MySesIO_SesIO_SesSetTableIndex(JNIEnv *env, jobject obj, jint jhandle, jlong date1, jlong date2, jlong version)
{

  ses_error_flag my_return_value = SES_NO_ERROR;


#ifdef DEBUG_WRAP
  printf("SesSetTableIndex.c:  the_handle is %d\n", handle);
#endif

//  ses_file_handle the_handle = (ses_file_handle)jhandle;
//  long my_date1 = (long)date1;
//  long my_date2 = (long)date2;
//  long my_version = (long)version;


  printf("SesSetTableIndex.c:  Function not yet implemented in java \n"); 


#ifdef DEBUG_WRAP 
  printf("SesSetTableIndex.c:  return_value is %d\n", my_return_value);
#endif
  return my_return_value;

}

 
