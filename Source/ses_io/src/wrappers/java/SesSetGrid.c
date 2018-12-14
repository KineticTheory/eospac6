/* The java SesSetGrid.c file, which implements the native function */
#include <jni.h>      /* Java Native Interface headers */
#include "MySesIO_SesIO.h"   /* header that defines ses_io java class for wrapper */

#include "ses_defines.h"
#include "stdio.h"
#include <stdlib.h>

#undef DEBUG_WRAP

 
JNIEXPORT jint JNICALL Java_MySesIO_SesIO_SesSetGrid(JNIEnv *env, jobject obj, jint jhandle, jint jnr, jint jnt, jint jntab)
{

  ses_error_flag return_value = SES_NO_ERROR;

  ses_file_handle handle = (ses_file_handle)jhandle;
  long nr = (long)jnr;
  long nt = (long)jnt;
  long ntab = (long)jntab;

#ifdef DEBUG_WRAP
  printf("SesSetGrid.c:  the_handle is %d\n", handle);
  printf("SesSetGrid.c:  nr is %ld\n", nr);
  printf("SesSetGrid.c:  nt is %ld\n", nt);
  printf("SesSetGrid.c:  ntag is %ld\n", ntab);
#endif

  return_value = ses_set_grid(handle, nr, nt, ntab);
#ifdef DEBUG_WRAP 
  printf("SesSetGrid.c:  return_value is %d\n", return_value);
#endif
  return (jint)return_value;

}

 
