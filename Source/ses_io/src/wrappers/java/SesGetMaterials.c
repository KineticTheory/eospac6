/* The java SesGetMaterials.c file, which implements the native function */
#include <jni.h>      /* Java Native Interface headers */
#include "MySesIO_SesIO.h"   /* header that defines ses_io java class for wrapper */

#include "ses_defines.h"
#include "stdio.h"
#include <stdlib.h>

#undef DEBUG_WRAP
 
JNIEXPORT jlongArray JNICALL Java_MySesIO_SesIO_SesGetMaterials(JNIEnv *env, jobject obj, jint jhandle)
{

  jlongArray return_value;

  ses_material_id_reference my_materials = (ses_material_id_reference)NULL;
  ses_file_handle handle = (ses_file_handle)jhandle;
  long* my_sizes = (long*)NULL;

#ifdef DEBUG_WRAP
  printf("SesGetMaterials.c:  the_handle is %d\n", handle);
#endif

  my_sizes = malloc(sizeof(long)*1);
  my_sizes[0] = 0;
  my_materials = ses_get_materials(handle, my_sizes);
#ifdef DEBUG_WRAP
  printf("SesGetMaterials.c:  returned from ses_get_materials\n");
  printf("SesGetMaterials.c:  my_sizes[0] is %d\n", my_sizes[0]);
#endif
  int i = 0;
  int num = my_sizes[0];
  if (num > 0) {
  
#ifdef DEBUG_WRAP 

     for (i = 0; i < num; i++) {
       printf("SesGetMaterials.c:  material %d is %d\n", i, my_materials[i]);
     }
#endif
   
   }


#ifdef DEBUG_WRAP
  printf("SesGetMaterials.c:  my_sizes[0] is %d\n", my_sizes[0]);
#endif
  
  return_value = (*env)->NewLongArray(env, num);
  jlong* pID = (*env)->GetLongArrayElements(env, return_value, NULL);
  for (i = 0; i < num; i++) {
	pID[i] = my_materials[i];
  }
  (*env)->ReleaseLongArrayElements(env, return_value, pID, 0);
#ifdef DEBUG_WRAP 
  printf("SesGetMaterials.c:  returning");
#endif
  free(my_sizes);
  
  return return_value;

}

 
