/* The java SesIndicatesError.c file, which implements the native function */
#include <jni.h>      /* Java Native Interface headers */
#include "MySesIO_SesIO.h"   /* header that defines ses_io java class for wrapper */

#include "ses_defines.h"
#include "stdio.h"
#include <stdlib.h>

#undef DEBUG_WRAP

 
JNIEXPORT jboolean JNICALL Java_MySesIO_SesIO_SesIndicatesError(JNIEnv *env, jobject obj, jint jerror_flag)
{

  ses_boolean return_value = SES_FALSE;
  ses_error_flag the_ses_error_flag = (ses_error_flag)jerror_flag;

#ifdef DEBUG_WRAP
  printf("SesIndicatesError:  the_error is %d\n", the_ses_error_flag);
#endif

  return_value = ses_indicates_error(the_ses_error_flag);
#ifdef DEBUG_WRAP 
  printf("SesIndicatesError.c:  return_value is %d\n", return_value);
#endif

  return (jboolean)return_value;
  

}

 
