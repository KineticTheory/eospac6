/* The java SesOpen.c file, which implements the native function */
#include <jni.h>      /* Java Native Interface headers */
#include "MySesIO_SesIO.h"   /* header that defines ses_io java class for wrapper */

#include "ses_defines.h"
#include "stdio.h"
#include <stdlib.h>

#undef DEBUG_WRAP

 
JNIEXPORT jint JNICALL Java_MySesIO_SesIO_SesOpen(JNIEnv *env, jobject obj, jstring jfilename, jchar jopen_flags)
{

  ses_file_handle return_value = 0;

  const char* local_string = (*env)->GetStringUTFChars(env, jfilename, 0);
  char* filename = (char*)local_string;

  ses_open_type open_flags = (char)jopen_flags;

#ifdef DEBUG_WRAP
  printf("SesOpen.c:  the_filename is %s\n", filename);
  printf("SesOpen.c:  open flags is %c\n", (char)jopen_flags);
#endif

  return_value = ses_open(filename, open_flags);
  (*env)->ReleaseStringUTFChars(env, jfilename, local_string);
#ifdef DEBUG_WRAP 
  printf("SesOpen.c:  return_value is %d\n", return_value);
#endif
  return return_value;

}

 
