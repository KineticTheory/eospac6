/* The java SesDefineTable.c file, which implements the native function */
#include <jni.h>      /* Java Native Interface headers */
#include "MySesIO_SesIO.h"   /* header that defines ses_io java class for wrapper */

#include "ses_defines.h"
#include "stdio.h"
#include <stdlib.h>
#include <string.h>

#undef DEBUG_WRAP

 
JNIEXPORT jint JNICALL Java_MySesIO_SesIO_SesDefineTable(JNIEnv *env, jobject jobj, jint jtid, jstring description, jlong jnr, jlong jnt, jlong jnum_independent, jlong jnum_arrays, jlongArray jsize_arrays, jobjectArray jlabels) { 


  ses_error_flag return_value = 0;

  int the_tid = jtid;

  const char** my_description = (const char**)(*env)->GetStringUTFChars(env, description, 0);
  ses_label local_description = (char*)my_description;

  long my_nr = jnr;
  long my_nt = jnt;
  long my_num_independent = jnum_independent;

  long my_num_arrays = jnum_arrays;

  jlong* srcArrayElems2 = 
           (*env)-> GetLongArrayElements(env, jsize_arrays, 0);
  long* my_size_arrays = (long*)srcArrayElems2;

#ifdef DEBUG_WRAP
  printf("SesDefineTable.c:  the_tid is %d\n", the_tid);
  printf("SesDefineTable.c:  description is %s\n", my_description);
  printf("SesDefineTable.c:  num_arrays is %d\n", my_num_arrays);
  int i = 0;
  for (i = 0; i < my_num_arrays; i++) {
    printf("SesDefineTable.c:  my_size_arrays[%d] is %d\n", i, my_size_arrays[i]);
  }
#endif

  char** my_labels = malloc(sizeof(char*) * my_num_arrays);
  
  int j = 0;
  jobject  my_object[my_num_arrays];
  jstring my_jstring[my_num_arrays];

  for(j = 0; j < my_num_arrays; j++)
  {
      my_object[j] = (*env)->GetObjectArrayElement(env, jlabels, j);
      my_jstring[j] = (jstring)my_object[j];
      const char* utf = (*env)->GetStringUTFChars(env, my_jstring[j], 0);
      my_labels[j] = (char*)utf;
      printf("SesDefineTable: my_labels[%d] is %s\n", j, my_labels[j]);
      
  }  
  printf("SesDefineTable.c:  out of loop\n");

#ifdef DEBUG_WRAP
  printf("SesDefineTable.c:  tid is %d\n", the_tid);
  printf("SesDefineTable.c:  local_description is %s\n", local_description);
  printf("SesDefineTable.c:  num_arrays is %d\n", my_num_arrays);
  printf("SesDefineTable.c:  size_arrays[0] is %d\n", my_size_arrays[0]);
  printf("SesDefineTable.c:  labels[0] is %s\n", my_labels[0]);
  
#endif

  return_value = ses_define_table(the_tid, local_description, my_nr, my_nt, my_num_independent, my_num_arrays, (char**)my_size_arrays, my_labels);
   
#ifdef DEBUG_WRAP 
  printf("SesDefineTable.c:  return_value is %d\n", return_value);
#endif

  (*env)->ReleaseStringUTFChars(env, description, (char*)my_description);
  (*env)->ReleaseLongArrayElements(env, jsize_arrays, srcArrayElems2, 0);
   for (j=0; j < my_num_arrays; j++) {
    free(my_labels[j]);
   }
   free(my_labels);

   return return_value;

}

 
