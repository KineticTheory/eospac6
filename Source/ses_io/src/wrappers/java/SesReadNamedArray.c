/* The java SesReadNamedArray.c file, which implements the native function */
#include <jni.h>      /* Java Native Interface headers */
#include "MySesIO_SesIO.h"   /* header that defines ses_io java class for wrapper */

#include "ses_defines.h"
#include "stdio.h"
#include <stdlib.h>

#undef DEBUG_WRAP

 
JNIEXPORT jdoubleArray JNICALL Java_MySesIO_SesIO_SesReadNamedArray(JNIEnv *env, jobject obj, jint jhandle, jstring jlabel, jint jdim)
{



  ses_file_handle handle = (ses_file_handle)jhandle;

  const char* local_string = (*env)->GetStringUTFChars(env, jlabel,  0);
  char* the_label = (char*)local_string;

  int dim = (int)jdim;
 
#ifdef DEBUG_WRAP
  printf("SesReadNamedArray.c:  the_handle is %d\n", handle);
#endif

  ses_word_reference buffer = malloc(sizeof(ses_word)*dim);
  ses_error_flag my_return_value = ses_read_named_array(handle, the_label, buffer);


  jdoubleArray return_value = (*env)->NewDoubleArray(env, dim);
  (*env)->SetDoubleArrayRegion(env, return_value, 0, dim, (jdouble*)buffer);

  /*  free(buffer); */

  my_return_value = 0;

#ifdef DEBUG_WRAP 
  printf("SesReadNamedArray.c:  buffer(3) is %e\n", buffer[0]);
#endif
  return return_value;

}

 
