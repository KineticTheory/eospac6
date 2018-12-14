/* The java SesExit.c file, which implements the native function */
#include <jni.h>      /* Java Native Interface headers */
#include "MySesIO_SesIO.h"   /* header that defines ses_io java class for wrapper */

#include "ses_defines.h"
#include "stdio.h"
#include <stdlib.h>

#undef DEBUG_WRAP

 
JNIEXPORT jint JNICALL Java_MySesIO_SesIO_SesExit(JNIEnv *env, jobject obj)
{

  ses_boolean return_value = SES_FALSE;

  return_value = ses_exit();
#ifdef DEBUG_WRAP 
  printf("SesExit.c:  return_value is %d\n", return_value);
#endif
  return return_value;

}

 
