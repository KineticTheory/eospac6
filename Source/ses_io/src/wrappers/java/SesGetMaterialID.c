/* The java SesGetMaterialID.c file, which implements the native function */
#include <jni.h>      /* Java Native Interface headers */
#include "MySesIO_SesIO.h"   /* header that defines ses_io java class for wrapper */

#include "ses_defines.h"
#include "stdio.h"
#include <stdlib.h>

#undef DEBUG_WRAP

 
JNIEXPORT jint JNICALL Java_MySesIO_SesIO_SesGetMaterialID(JNIEnv *env, jobject obj, jstring jmatname)
{

  int return_value = 0;
/*  const char* local_string = (*env)->GetStringUTFChars(env, jmatname,  0);
  char* matname = (char*)local_string;


#ifdef DEBUG_WRAP
  printf("SesGetMaterialID.c:  the_string is %s\n", matname);
#endif

   return_value = ses_get_material_id(matname);
#ifdef DEBUG_WRAP 
  printf("SesGetMaterialID.c:  return_value is %d\n", return_value);
#endif
*/
  return return_value;

}

 
