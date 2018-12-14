/* The java_array_size_next.c file, which implements the native function */
#include <jni.h>      /* Java Native Interface headers */
#include "MySesIO_SesIO.h"   /* header that defines ses_io java class for wrapper */

#include "ses_defines.h"
#include "stdio.h"
#include <stdlib.h>

#undef DEBUG_WRAP

 
JNIEXPORT jlong JNICALL Java_MySesIO_SesIO_SesAccessTableIndex(JNIEnv *env, jobject obj, jint jhandle, jlongArray matid, jlongArray nwds, jlongArray  iadr, jlong date1, jlong date2, jlong version)
{

  ses_error_flag my_return_value = SES_NO_ERROR;


#ifdef DEBUG_WRAP
  printf("SesAccessTableIndex.c:  the_handle is %d\n", handle);
#endif

  printf("SesAccessTableIndex.c:  Function not yet implemented in java \n"); 


#ifdef DEBUG_WRAP 
  printf("SesAccessTableIndex.c:  return_value is %d\n", my_return_value);
#endif
  return my_return_value;

}

 
