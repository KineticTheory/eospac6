/* The java SesGetTableIDS.c file, which implements the native function */
#include <jni.h>      /* Java Native Interface headers */
#include "MySesIO_SesIO.h"   /* header that defines ses_io java class for wrapper */

#include "ses_defines.h"
#include "stdio.h"
#include <stdlib.h>

#undef DEBUG_WRAP

 
JNIEXPORT jlongArray JNICALL Java_MySesIO_SesIO_SesGetTableSizes(JNIEnv *env, jobject obj, jint jhandle, jlong jmid)
{

  jlongArray return_value;

  ses_file_handle handle = (ses_file_handle)jhandle;
  ses_material_id mid = (ses_material_id)jmid;
  long* my_size = malloc(sizeof(long)*1);
  my_size[0] = 0;
  ses_table_sizes_reference my_table_sizes = (ses_table_sizes_reference)NULL;


#ifdef DEBUG_WRAP
  printf("SesGetTableSizes.c:  the_handle is %d\n", handle);
  printf("SesGetTableSizes.c:  mid is %d\n", mid);
#endif

  my_table_sizes = ses_get_table_sizes(handle, mid, my_size);
  

  int i = 0;
  int num = my_size[0];

  long* tmp_return_value = (long*)NULL;  
  if (num > 0) {
     tmp_return_value = malloc(sizeof(long)*num);
#ifdef DEBUG_WRAP 

     printf("SesGetTableSizes.c:  num is %d\n", num);
     for (i = 0; i < num; i++) {
       printf("SesGetTableSizes.c:  table %d is %d\n", i, my_table_sizes[i]);
     }
#endif
   
     for (i = 0; i < num; i++) {
       tmp_return_value[i] = (jlong)my_table_sizes[i];
     }
  
  }
#ifdef DEBUG_WRAP
  printf("SesGetTableSizes.c:  got past num = %d loop\n", num);
#endif
  return_value = (*env)->NewLongArray(env, num);
  jlong* pID = (*env)->GetLongArrayElements(env, return_value, NULL);
  for (i = 0; i < num; i++) {
	pID[i] = my_table_sizes[i];
  }
  (*env)->ReleaseLongArrayElements(env, return_value, pID, 0);
  //(*env)->SetLongArrayRegion(env, return_value, 0, num, (const jlong*)tmp_return_value);
 
  return return_value;

}

 
