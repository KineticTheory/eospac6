/* The java SesWriteSetup.c file, which implements the native function */
#include <jni.h>      /* Java Native Interface headers */
#include "MySesIO_SesIO.h"   /* header that defines ses_io java class for wrapper */

#include "ses_defines.h"
#include "stdio.h"
#include <stdlib.h>

#undef DEBUG_WRAP

 
JNIEXPORT jint JNICALL Java_MySesIO_SesIO_SesWriteSetup(JNIEnv *env, jobject obj, jint jhandle, jint jmid, jint jtid, jint nr, jint nt, jint ntab)
{

  ses_error_flag return_value = SES_NO_ERROR;

  ses_file_handle handle = (ses_file_handle)jhandle;

#ifdef DEBUG_WRAP
  printf("SesWriteSetup.c:  the_handle is %d\n", handle);
  printf("SesWriteSetup.c:  mid is %d\n", jmid);
  printf("SesWriteSetup.c:  tid is %d\n", jtid);
#endif

  return_value = ses_write_setup(handle, (long)jmid, (long)jtid, nr, nt, ntab);
#ifdef DEBUG_WRAP 
  printf("SesWriteSetup.c:  return_value is %d\n", return_value);
#endif
  return return_value;

}

 
