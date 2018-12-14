/* The java SesGetGrid.c file, which implements the native function */
#include <jni.h>      /* Java Native Interface headers */
#include "MySesIO_SesIO.h"   /* header that defines ses_io java class for wrapper */

#include "ses_defines.h"
#include "stdio.h"
#include <stdlib.h>

#undef DEBUG_WRAP

 
JNIEXPORT jlongArray JNICALL Java_MySesIO_SesIO_SesGetGrid(JNIEnv *env, jobject obj, jint jhandle,  jlong the_mid, jlong the_tid)
{

  jlongArray jreturn_value;

  ses_file_handle handle = (ses_file_handle)jhandle;

#ifdef DEBUG_WRAP
  printf("SesGetGrid.c:  the_handle is %d\n", handle);
#endif

  long* ptnr = (long*)NULL;
  long* ptnt = (long*)NULL;
  long* ptntab = (long*)NULL;
  int error_flag = SES_NO_ERROR;
  error_flag = ses_get_grid(handle, the_mid, the_tid, ptnr, ptnt, ptntab);
#ifdef DEBUG_WRAP 
  printf("SesGetGrid.c:  return_value is %s\n", return_value);
  printf("SesGetGrid.c:  nr, nt is %d %d\n", *ptnr, *ptnt);
#endif

  int i = 0;
  int num = 3;
  long* tmp_return_value = (long*)NULL;  

  if (num > 0) {
     tmp_return_value = malloc(sizeof(long)*num);
#ifdef DEBUG_WRAP 

     printf("SesGetGrid.c:  num is %d\n", num);
#endif
   
     tmp_return_value[0] = *ptnr;
     tmp_return_value[1] = *ptnt;
     tmp_return_value[2] = *ptntab;
  
  }
  jreturn_value = (*env)->NewLongArray(env, num);
  jlong* pID = (*env)->GetLongArrayElements(env, jreturn_value, NULL);
  for (i = 0; i < num; i++) {
	pID[i] = tmp_return_value[i];
  }
  (*env)->ReleaseLongArrayElements(env, jreturn_value, pID, 0);


  return jreturn_value;

}

 
